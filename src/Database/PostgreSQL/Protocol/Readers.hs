-- | Module:    Database.PostgreSQL.Protocol.Readers
-- Description: Readers for PostgreSQL messages.
-- Copyright:   © 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  pat@jantar.org
-- Stability:   experimental
-- Portability: portable
--
-- This module defines low-level readers for all defined PostgreSQL messages,
-- abstracting over the actual source of data, which is represented simply
-- by a @read@ function that retrieves within a 'MonadPlus' a lazy bytestring
-- of a fixed size.

module Database.PostgreSQL.Protocol.Readers (
  readSessionMessage,
  readSSLResponse,
  readFrontendMessage,
  readBackendMessage
) where

import Database.PostgreSQL.Protocol.Types
import Database.PostgreSQL.Protocol.Version
import Database.PostgreSQL.Protocol.Internal.Parsers

import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Bits
import Data.ByteString (ByteString)
import Data.Functor
import Data.Int
import Data.Monoid
import Prelude hiding (fail)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString

maxMessageSize :: Int
maxMessageSize = 16 * 1024 * 1024

readSessionMessage :: MonadFail m => (Int -> m ByteString) -> m SessionMessage
readSessionMessage readData = do
  (n, v) <- readData 8 >>= parseByteString ((,) <$> num32 <*> int32)
  case v of
    80877102 -> do unless (n == 16) $
                     fail $ "Invalid CancelRequest message length: " <> show n <> " (expecting 16)"
                   readData 8 >>= parseByteString (CancelRequest <$> num32 <*> num32)
    80877103 -> do unless (n == 8) $
                     fail $ "Invalid SSLRequest message length: " <> show n <> " (expecting 8)"
                   return SSLRequest
    _        -> do unless (8 <= n && n <= maxMessageSize) $
                     fail $ "Invalid StartupMessage message length: " <> show n <>
                            " (expecting between 8 and " <> show maxMessageSize <> " bytes)"
                   let vm = fromIntegral (v `shiftR` 16)
                       vn = fromIntegral (v .&. 0xFFFF)
                   unless (vm == currentMajorVersion) $
                     fail $ "Invalid major protocol version in StartupMessage: " <> show vm <> " (expecting " <> show currentMajorVersion <> ")"
                   s <- readData (n - 8)
                   StartupMessage vm vn <$> parseSessionParameters (ByteString.split 0 s)
 where
  parseSessionParameters (pn:pv:ps')
    | ByteString.null pn = (unless (ByteString.null pv && null ps') $
                              fail $ "Null session parameter name supplied in StartupMessage") $> []
    | otherwise = ((pn, LazyByteString.fromStrict pv):) <$> parseSessionParameters ps'
  parseSessionParameters _ = fail $ "Missing session parameter list terminator"

readSSLResponse :: MonadFail m => (Int -> m ByteString) -> m SSLResponse
readSSLResponse readData = do
  t <- readData 1 >>= parseByteString char8
  case t of
    'S' -> return SSLRequestAccepted
    'N' -> return SSLRequestRejected
    'E' -> do n <- readData 4 >>= parseByteString num32
              readMessage readData n "ErrorResponse" (SSLRequestFailed <$> noticeFields)
    _  -> fail $ "Unrecognised SSL response tag: " <> show t

readFrontendMessage :: MonadFail m => (Int -> m ByteString) -> m FrontendMessage
readFrontendMessage readData = do
  (t, n) <- readData 5 >>= parseByteString ((,) <$> char8 <*> num32)
  case t of
    'p' -> readMessage readData n "AuthenticationResponse" (AuthenticationResponse <$> remainingLazyByteString)
    'B' -> readMessage readData n "Bind" (Bind <$> byteStringZ <*> byteStringZ <*> listL16 num16 <*> listL16 maybeLazyByteStringL32 <*> listL16 num16)
    'C' -> readMessage readData n "Close" (Close <$> num8 <*> byteStringZ)
    'd' -> readMessage readData n "CopyInData" (CopyInData <$> remainingLazyByteString)
    'c' -> readTrivialMessage   n "CopyInDone" CopyInDone
    'f' -> readMessage readData n "CopyInFail" (CopyInFail <$> lazyByteStringZ)
    'D' -> readMessage readData n "Describe" (Describe <$> num8 <*> byteStringZ)
    'E' -> readMessage readData n "Execute" (Execute <$> byteStringZ <*> num32)
    'H' -> readTrivialMessage   n "Flush" Flush
    'F' -> readMessage readData n "FunctionCall" (FunctionCall <$> num32 <*> listL16 num16 <*> listL16 maybeLazyByteStringL32 <*> num16)
    'P' -> readMessage readData n "Parse" (Parse <$> byteStringZ <*> lazyByteStringZ <*> listL16 num32)
    'Q' -> readMessage readData n "Query" (Query <$> lazyByteStringZ)
    'S' -> readTrivialMessage   n "Sync" Sync
    'X' -> readTrivialMessage   n "Terminate" Terminate
    _   -> fail $ "Unrecognized frontend message: " <> show t

readBackendMessage :: MonadFail m => (Int -> m ByteString) -> m BackendMessage
readBackendMessage readData = do
  (t, n) <- readData 5 >>= parseByteString ((,) <$> char8 <*> num32)
  case t of
    'R' -> readMessage readData n "AuthenticationRequest" (AuthenticationRequest <$> authenticationRequestMessage)
    'K' -> readMessage readData n "BackendKeyData" (BackendKeyData <$> num32 <*> num32)
    '2' -> readTrivialMessage   n "BindComplete" BindComplete
    '3' -> readTrivialMessage   n "CloseComplete" CloseComplete
    'C' -> readMessage readData n "CommandComplete" (CommandComplete <$> lazyByteStringZ)
    'd' -> readMessage readData n "CopyOutData" (CopyOutData <$> remainingLazyByteString)
    'c' -> readTrivialMessage   n "CopyOutDone" CopyOutDone
    'G' -> readMessage readData n "CopyInResponse" (CopyInResponse <$> num8 <*> listL16 num16)
    'H' -> readMessage readData n "CopyOutResponse" (CopyOutResponse <$> num8 <*> listL16 num16)
    'W' -> readMessage readData n "CopyBothResponse" (CopyBothResponse <$> num8 <*> listL16 num16)
    'D' -> readMessage readData n "DataRow" (DataRow <$> listL16 maybeLazyByteStringL32)
    'I' -> readTrivialMessage   n "EmptyQueryResponse" EmptyQueryResponse
    'E' -> readMessage readData n "ErrorResponse" (ErrorResponse <$> noticeFields)
    'V' -> readMessage readData n "FunctionCallResponse" (FunctionCallResponse <$> maybeLazyByteStringL32)
    'v' -> readMessage readData n "NegotiateProtocolVersion" (NegotiateProtocolVersion <$> num32 <*> listL32 byteStringZ)
    'n' -> readTrivialMessage   n "NoData" NoData
    'N' -> readMessage readData n "NoticeResponse" (NoticeResponse <$> noticeFields)
    'A' -> readMessage readData n "NotificationResponse" (NotificationResponse <$> num32 <*> byteStringZ <*> lazyByteStringZ)
    't' -> readMessage readData n "ParameterDescription" (ParameterDescription <$> listL16 num32)
    'S' -> readMessage readData n "ParameterStatus" (ParameterStatus <$> byteStringZ <*> lazyByteStringZ)
    '1' -> readTrivialMessage   n "ParseComplete" ParseComplete
    's' -> readTrivialMessage   n "PortalSuspended" PortalSuspended
    'Z' -> readMessage readData n "ReadyForQuery" (ReadyForQuery <$> num8)
    'T' -> readMessage readData n "RowDescription" (RowDescription <$> listL16 fieldDescription)
    _   -> fail $ "unrecognized backend message: " <> show t
 where
  authenticationRequestMessage = do
    t <- int32
    case t of
      0  -> pure AuthenticationOk
      2  -> pure AuthenticationKerberosV5
      3  -> pure AuthenticationCleartextPassword
      5  -> AuthenticationMD5Password <$> num32
      6  -> pure AuthenticationSCMCredential
      7  -> pure AuthenticationGSS
      8  -> AuthenticationGSSContinue <$> remainingLazyByteString
      9  -> pure AuthenticationSSPI
      10 -> AuthenticationSASL <$> saslAuthenticationMechanisms
      11 -> AuthenticationSASLContinue <$> remainingLazyByteString
      12 -> AuthenticationSASLFinal <$> remainingLazyByteString
      _  -> fail $ "Unsupported authentication method: " <> show t
  fieldDescription = FieldDescription <$> byteStringZ <*> num32 <*> num16 <*> num32 <*> num16 <*> num32 <*> num16
  saslAuthenticationMechanisms = parse =<< ByteString.split 0 <$> remainingByteString
   where
    parse (f:fs')
      | ByteString.null f = (unless (fs' == [ByteString.empty]) $ fail $ "Null SASL authentication method") $> []
      | otherwise = (f:) <$> parse fs'
    parse [] = fail $ "Missing notice field list terminator"

readMessage :: MonadFail m => (Int -> m ByteString) -> Int -> String -> Parser a -> m a
readMessage readData n k p = do
  unless (n >= 4 && n <= maxMessageSize) $ fail $ "Invalid " <> k <> " message size: " <> show n
  readData n >>= parseByteString (p <* end)

readTrivialMessage :: MonadFail m => Int -> String -> a -> m a
readTrivialMessage n k r = do
  unless (n == 4) $ fail $ "Invalid " <> k <> " message size: " <> show n <> " (expecting 4)"
  return r

noticeFields :: Parser [(NoticeFieldTag, LazyByteString)]
noticeFields = parse =<< ByteString.split 0 <$> remainingByteString
 where
  parse (f:fs') = case ByteString.uncons f of
                    Just (ft, fv) -> ((ft, LazyByteString.fromStrict fv):) <$> parse fs'
                    Nothing -> (unless (fs' == [ByteString.empty]) $ fail $ "Null notice field tag") $> []
  parse [] = fail $ "Missing notice field list terminator"
