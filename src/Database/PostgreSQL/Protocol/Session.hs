-- | Module:    Database.PostgreSQL.Protocol.Frontend
-- Description: Generic asynchronous PostgreSQL frontend structure
-- Copyright:   © 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable
--
-- This module defines a primitive low-level structure of a generic
-- PostgreSQL frontend capable of asynchronous message transmission
-- and reception.
--
-- The frontend takes care of initial session setup and authentication,
-- decoding of received messages and all asynchronous thread safety issues.
--
-- However, messages sent to the backend are represented by raw chunks
-- of binary data, relying entirely on the user to keep all message

module Database.PostgreSQL.Protocol.Frontend (
  Connection,
  AuthenticationHandler (..),
  connectHandle,
  backendProcessID,
  backendKey,
  majorProtocolVersion,
  minorProtocolVersion,
  unsupportedProtocolOptions,
  sendMessage, receiveBackendMessage,
  throwProtocolError, throwErrorResponse,
  sendCancelRequest,
) where

import Control.Exception
import Data.ByteString (ByteString)
import Data.Data
import Data.Word

import Database.PostgreSQL.Protocol.Decoders
import Database.PostgreSQL.Protocol.Encoders
import Database.PostgreSQL.Protocol.Exception
import Database.PostgreSQL.Protocol.Handle
import Database.PostgreSQL.Protocol.Internal.Checked
import Database.PostgreSQL.Protocol.Types

data Connection = Connection {
  connectionHandle           :: Handle,
  backendProcessID           :: Maybe ProcessID,
  backendKey                 :: Maybe Word32,
  majorProtocolVersion       :: !Word16,
  minorProtocolVersion       :: !Word32,
  unsupportedProtocolOptions :: [ByteString]
}

newtype AuthenticationHandler = AuthenticationHandler {
  runAuthenticationHandler :: AuthenticationResponse -> Maybe (Connection -> IO AuthenticationHandler)
}

newConnection :: Handle -> Connection
newConnection h = Connection {
  connectionHandle           = h,
  backendProcessID           = Nothing,
  backendKey                 = Nothing,
  majorProtocolVersion       = CURRENT_MAJOR_VERSION,
  minorProtocolVersion       = fromIntegral CURRENT_MINOR_VERSION,
  unsupportedProtocolOptions = []
}

connectHandleWith :: AuthenticationHandler -> ConnectionParameters -> Handle -> IO (Connection, [AsynchronousMessage], TransactionStatus)
connectHandleWith auth0 params h = do
  sendBytes h (startupMessage CURRENT_MAJOR_VERSION CURRENT_MINOR_VERSION params)
  negotiate auth [] (newConnection h)
 where
  negotiate auth ams conn = do
    msg <- receiveBackendMessage conn
    case msg of
      AsynchronousMessage am -> negotiate auth (am:ams) conn
      AuthenticationResponse r -> do
        auth' <- case runAuthenticationHandler auth r of
                   Just authenticate -> authenticate conn
                   Nothing -> throwProtocolError ("Unsupported authentication method requested by server: " <> show (authenticationResponseTag r))
        negotiate auth' ams conn
      NegotiateProtocolVersion npv npo ->
        negotiate auth ams conn { minorProtocolVersion = npv, unsupportedProtocolOptions = npo }
      BackendKeyData pid k -> negotiate auth ams conn { backendProcessID = Just pid, backendKey = Just k }
      ReadyForQuery ts -> return (conn, reverse ams, ts)
      ErrorResponse fs -> throwErrorResponse fs
      _ -> throwProtocolError ("Unexpected message received from server: " <> show (toConstr msg))

connectHandleWithPassword :: IO ByteString -> ConnectionParameters -> Handle -> IO (Connection, [AsynchronousMessage], TransactionStatus)
connectHandleWithPassword = connectHandleWith . authenticateWithPassword

authenticateWithPassword :: IO ByteString -> AuthenticationHandler
authenticateWithPassword getPassword = AuthenticationHandler authenticate
 where
  auth (AuthenticationOK) = Just ()
  auth (AuthenticationCleartextPassword) = Just $ do
    passwd <- getPassword
    sendMessage conn $ passwordMessage $ LazyByteString.fromStrict passwd
    return authenticateAnonymous
  auth (AuthenticationMD5Password salt) = Just $ do
    passwd <- getPassword
    sendMessage conn $ passwordMessage $ "md5" <> md5hex (md5hex (LazyByteString.fromChunks [passwd, user] <> salt))
    return authenticateAnonymous


authenticateAnonymous :: AuthenticationHandler
authenticateAnonymous = AuthenticationHandler handle
 where
  handle AuthenticationOk -> Just $ const $ return authenticateAnonymous
  handle

sendMessage :: Connection -> LazyByteString -> IO ()
sendMessage conn = sendBytes (connectionHandle conn)

receiveBackendMessage :: Connection -> IO BackendMessage
receiveBackendMessage conn =
  runChecked throwProtocolError $
  backendMessage $
  unchecked . receiveBytes (connectionHandle conn) . fromIntegral

throwProtocolError :: String -> IO a
throwProtocolError = throwIO . DatabaseProtocolError

throwErrorResponse :: NoticeFields -> IO a
throwErrorResponse = throwIO . DatabaseErrorResponse

sendCancelRequest :: Connection -> IO ()
sendCancelRequest conn =
  maybe (return ()) (sendMsg) (cancelRequest <$> backendProcessID conn <*> backendKey conn)
 where
  sendMsg msg = bracket (duplicateHandle (connectionHandle conn)) (closeHandle) (flip sendBytes msg)
