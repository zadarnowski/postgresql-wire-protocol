-- | Module:    Database.PostgreSQL.Protocol.Session
-- Description: Low-level implementation of a PostgreSQL client session
-- Copyright:   © 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable
--
-- This module defines a primitive low-level utilities used to establish
-- PostgreSQL frontend (i.e., client) connections. It handles initial
-- session startup protocol, including generic client authentication
-- mechanism. Once a session has been established, further well-formed
-- exchange of communication messages is left up to higher levels of
-- abstraction.

{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Protocol.Session (
  Session,
  beginSession,
  beginSessionWithMD5Password,
  beginSessionWithPassword,
  beginSessionWith,
  sessionBackendPID,
  sessionBackendKey,
  sessionMajorProtocolVersion,
  sessionMinorProtocolVersion,
  sessionUnsupportedProtocolOptions,
  AuthenticationHandler (..),
  noAuthentication,
  md5PasswordAuthentication,
  passwordAuthentication,
  sendMessage,
  receiveBackendMessage,
  cancelSession,
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

import qualified Data.ByteString.Lazy as LazyByteString

-- | A low-level authenticated PostgreSQL client session,
-- as viewed from theSessionfrontend's (i.e., client's) perspective.
data Session = Session {
  -- | Underlying communication handle
  sessionHandle :: Handle,
  -- | Process ID of the connected backend, if provided by the server.
  sessionBackendPID :: Maybe ProcessID,
  -- | Backend key for use of query cancellation requests as described
  -- in 'CancelRequest' and 'cancelSession'.
  sessionBackendKey :: Maybe Word32,
  -- | Major protocol version requested in the 'StartupMessage' used to
  -- establish the connection; currently, this is always set to
  -- 'CURRENT_MAJOR_VERSION'.
  sessionMajorProtocolVersion :: !Word16,
  -- | Minor protocol version as requested in the 'StartupMessage' used to
  -- establish the connection (i.e., 'CURRENT_MINOR_VERSION') and potentially
  -- adjusted by the server using the 'NegotiateProtocolVersion' message.
  sessionMinorProtocolVersion :: !Word32,
  -- | List of protocol options requested in the 'StartupMessage' that are not
  -- recognised and/or supported by the server, as reported in the
  -- 'NegotiateProtocolVersion' message.
  sessionUnsupportedProtocolOptions :: [ByteString]
}

-- Initial null session using the specified communication handle.
newSession :: Handle -> Session
newSession h = Session {
  sessionHandle                     = h,
  sessionBackendPID                 = Nothing,
  sessionBackendKey                 = Nothing,
  sessionMajorProtocolVersion       = CURRENT_MAJOR_VERSION,
  sessionMinorProtocolVersion       = fromIntegral CURRENT_MINOR_VERSION,
  sessionUnsupportedProtocolOptions = []
}

-- | Establish a new session over the specified 'Handle' and using the specified
-- list of connection parameters @ps@ to a server that does not require a
-- protocol-level authentication mechanism. This should be used only with
-- servers configured to implicitly trust all connecting clients (e.g., by
-- specifying the “@trust@” authentication method in @pg_hba.conf@(, and
-- servers utilising transport-layer authentication method such as “@ident@”,
-- “@peer@” or “@cert@”. If the server requests any protocol-level
-- authentication method (e.g., password, SSPI, GSS or SASL), connection will
-- be rejected by throwing an 'UnsupportedAuthenticationMethod' exception.
--
-- @beginSession@ reports all errors using appropriate exceptions, usually
-- 'IOException' for I/O errors and 'DatabaseException' for protocol-level
-- errors. In particular, error conditions reported by the server using
-- 'ErrorResponse' message are converted into 'DatabaseErrorResponse'
-- exception.
--
-- On success, 'beginSession' returns an established 'Session' object, a list
-- of asynchronous messages issued by the server during session establishment,
-- and the initial transaction status (usually 'TRANSACTION_IDLE'.) The list of
-- asynchronous messages will usually include at least the 'ParameterStatus'
-- messages for all ”important” session parameters, but may also include
-- 'NoticeResponse' messages for any warnings issues by the servers.
beginSession :: Handle -> ConnectionParameters -> IO (Session, [AsynchronousMessage], TransactionStatus)
beginSession = beginSessionWith noAuthentication

-- | Same as 'beginSession', but with added support for MD5-protected password
-- authentication method, i.e., “@md5@” in @pg_hba.conf@. Unauthenticated
-- connections such as “@trust@” and transport-layer authentication methods
-- such as “@ident@”, “@peer@” or “@cert@” are also supported, but methods that
-- require transmission of cleartext passwords (including “@password@”,
-- “@ldap@” and “@radius@” are rejected.)
--
-- The password, if requested by the server, is obtained using an 'IO' action
-- supplied as the initial parameter to @connectHandleWithMD5@ in order to
-- allow clients to issue password prompt only when required by the server.
beginSessionWithMD5Password :: Handle -> ConnectionParameters -> IO ByteString -> IO (Session, [AsynchronousMessage], TransactionStatus)
beginSessionWithMD5Password h ps getPassword =
  beginSessionWith (md5PasswordAuthentication ps getPassword) h ps

-- | Same as 'beginSessionWithMD5Password', but with added support for unencrypted
-- password-based authentication methods, i.e., “@password@”, “@ldap@” and
-- “@radius@” in @pg_hba.conf@.  Encrypted password authentication (“@md5@”),
-- unauthenticated (“@trust@”) and transport-layer authentication methods such
-- as “@ident@”, “@peer@” and “@cert@” are also supported.
--
-- This method should only ever be used over local (i.e., @localhost@ and UNIX
-- domain sockets) and encrypted network connections, as the password is sent
-- to the supplied handle in clear text. In addition, SSL or TLS certificate
-- verification should be employed to ensure that the password is not leaked to
-- unauthorised parties through DNS spoofing and similar attacks.
beginSessionWithPassword :: Handle -> ConnectionParameters -> IO ByteString -> IO (Session, [AsynchronousMessage], TransactionStatus)
beginSessionWithPassword h ps getPassword =
  beginSessionWith (passwordAuthentication ps getPassword) h ps

-- | Establishes a connection to PostgreSQL server over the specified
-- communication 'Handle', and using the specified list of connection
-- parameters. In stock PostgreSQL servers, connection parameter list must
-- include at least the @user@ parameter used to identify the caller for
-- authentication purposes.
--
-- Any authentication method requested by the server is delegated to
-- the supplied t'AuthenticationHandler'.

-- @beginSessionWith@ reports all errors using appropriate exceptions, usually
-- 'IOException' for I/O errors and 'DatabaseException' for protocol-level
-- errors. In particular, error conditions reported by the server using
-- 'ErrorResponse' message are converted into 'DatabaseErrorResponse'
-- exception.
--
-- On success, @beginSessionWith@ returns an established 'Connection' object, a
-- list of asynchronous messages issued by the server during session
-- establishment, and the initial transaction status (usually
-- 'TRANSACTION_IDLE'.) The list of asynchronous messages will usually
-- include at least the 'ParameterStatus' messages for all ”important” session
-- parameters, but may also include 'NoticeReponse' messages for any warnings
-- issues by the servers.
beginSessionWith :: AuthenticationHandler -> Handle -> ConnectionParameters -> IO (Session, [AsynchronousMessage], TransactionStatus)
beginSessionWith auth h ps = do
  sendBytes h (startupMessage CURRENT_MAJOR_VERSION CURRENT_MINOR_VERSION ps)
  negotiate auth (newSession h) []

-- Handle server response to the 'StartupMessage' as described in Section
-- 48.2.1 of PostgreSQL manual.  In official PostgreSQL documentation, the
-- startup protocol is separated into two distinct phases: authentication and
-- setup, but, since each phase uses distinct set of message types,
-- @negotiate@ does not distinguish these and simply handles all
-- acceptable message types until arrival of the first 'ReadyForQuery' or
-- 'ErrorResponse' message. This means that @negotiate@ is able to
-- handle multiple authentication requests in a single sessions, although no
-- stock PostgreSQL server ever issues more than one.
negotiate :: AuthenticationHandler -> Session -> [AsynchronousMessage] -> IO (Session, [AsynchronousMessage], TransactionStatus)
negotiate auth session ams = do
  msg <- receiveBackendMessage session
  case msg of
    AsynchronousMessage am -> negotiate auth session (am:ams)
    AuthenticationResponse AuthenticationOk -> negotiate noAuthentication session ams
    AuthenticationResponse r -> do auth' <- runAuthenticationHandler auth (sessionHandle session) r
                                   negotiate auth' session ams
    NegotiateProtocolVersion npv npo -> negotiate auth session { sessionMinorProtocolVersion = npv, sessionUnsupportedProtocolOptions = npo } ams
    BackendKeyData pid k -> negotiate auth session { sessionBackendPID = Just pid, sessionBackendKey = Just k } ams
    ReadyForQuery ts -> return (session, reverse ams, ts)
    ErrorResponse fs -> throwErrorResponse fs
    _ -> throwProtocolError ("Unexpected message received from server: " <> show (toConstr msg))

-- | 'beginSessionWith' delegates any authentication method requested by the
-- server to a user-supplied 'AuthenticationHandler'. An authentication handler
-- is simply an 'IO' action that receives the t'AuthenticationResponse' message
-- issued by the server and a communication 'Handle', and is responsible for
-- sending any further authentication data data to the connection as required
-- by the authentication method. For most authentication methods, the handler
-- should simply use 'sendMessage' to issue an appropriately-formatted
-- 'PasswordMessage' to the backend, but, for methods such as Kerberos V5 and
-- SCM, authentication may take form of arbitrary (even bidirectional) I/O over
-- the underlying connection handle.
--
-- In order to support multi-step stateful authentication methods such as
-- GSSAPI, SSPI and SASL, authentication handlers can return a new
-- authentication handler value used to process all subsequent
-- t'AuthenticationResponse' messages encountered during session
-- establishment.  If no further t'AuthenticationResponse' messages are expected
-- for the requested method, the handler should return 'noAuthentication'.
--
-- If the handler does not recognise or support the requested authentication
-- method, it should reject the connection by raising the
-- 'UnsupportedAuthenticationMethod' exception.
newtype AuthenticationHandler = AuthenticationHandler {
  runAuthenticationHandler :: Handle -> AuthenticationResponse -> IO AuthenticationHandler
}

noAuthentication :: AuthenticationHandler
noAuthentication = AuthenticationHandler handler
 where
  handler _ (AuthenticationOk) = return noAuthentication
  handler _ r = throwUnsupportedAuthenticationMethod r

md5PasswordAuthentication :: ConnectionParameters -> IO ByteString -> AuthenticationHandler
md5PasswordAuthentication ps getPassword = AuthenticationHandler handler
 where
  handler h r@(AuthenticationMD5Password salt) =
    case [ v | ("user", v) <- ps ] of
      (user:_) -> do
        password <- getPassword
        sendBytes h $ md5PasswordMessage user password salt
        return noAuthentication
      [] -> throwUnsupportedAuthenticationMethod r -- MD5 authentication method requires a user name
  handler h r = runAuthenticationHandler noAuthentication h r

passwordAuthentication :: ConnectionParameters -> IO ByteString -> AuthenticationHandler
passwordAuthentication ps getPassword = AuthenticationHandler handler
 where
  handler h (AuthenticationCleartextPassword) = do
    password <- getPassword
    sendBytes h $ passwordMessage $ LazyByteString.fromStrict password
    return noAuthentication
  handler h r = runAuthenticationHandler (md5PasswordAuthentication ps getPassword) h r

sendMessage :: Session -> LazyByteString -> IO ()
sendMessage = sendBytes . sessionHandle

receiveBackendMessage :: Session -> IO BackendMessage
receiveBackendMessage session =
  runChecked throwProtocolError $
  backendMessage $
  unchecked . receiveBytes (sessionHandle session)

cancelSession :: Session -> IO ()
cancelSession session =
  maybe (return ()) (sendMsg) (cancelRequest <$> sessionBackendPID session <*> sessionBackendKey session)
 where
  sendMsg msg = bracket (duplicateHandle (sessionHandle session)) (closeHandle) (flip sendBytes msg)
