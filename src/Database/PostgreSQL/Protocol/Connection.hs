-- | Module:    Database.PostgreSQL.Protocol.Connection
-- Description: Low-level connection for exchange of PostgreSQL protocol messages
-- Copyright:   © 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable
--
-- This module defines a primitive low-level structure of a generic
-- PostgreSQL frontend capable of asynchronous message transmission
-- and reception.

module Database.PostgreSQL.Protocol.Connection (
  Connection,
  close, abort,
  send, receive

) where

import Database.PostgreSQL.Protocol.Builders
import Database.PostgreSQL.Protocol.Readers
import Database.PostgreSQL.Protocol.Types

import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString.Lazy as LazyByteString

data Connection = CONNECTION {
  startupContext :: StartupContext,
  close :: IO (),
  abort :: IO (),
  send :: LazyByteString -> IO (),
  readBytes :: Int -> IO LazyByteString
}

data StartupContext :: STARTUP_CONTEXT {
  startupAsynchronousMessages :: [(Word8, LazyByteString)],
  startupBackendKeyData :: Maybe (ProcessID, Word32),
  startupNegotiatedProtocolVersion :: Maybe (Word32, [ByteString])
}

instance Semigroup StartupContext where
  STARTUP_CONTEXT m1 k1 v1 <> STARTUP_CONTEXT m2 k2 v2 =
    STARTUP_CONTEXT (m1 <> m2) (k2 <|> k1) (v2 <|> v1)

instance Monoid StartupContext where
  mempty = STARTUP_CONTEXT [] Nothing Nothing

connectSocketWithPassword :: IO ByteString -> ConnectionParameters -> Socket -> IO Connection
connectSocketWithPassword getPassword params sock = do
  sendMessage (startupMessage params)
  (tag, mstr) <- receiveMessageBytes
  case tag of
    NEGOTIATE_PROTOCOL_VERSION -> do
      fail "TODO"
    ERROR_RESPONSE -> do
      readErrorResponse mstr >>= throw . ErrorResponse
    AUTHENTICATION_RESPONSE -> do
      msg <- readAuthenticationResponse mstr
      case msg of
        AUTHENTICATION_OK -> fail "TODO"
        AUTHENTICATION_KERBEROS_V5 -> fail "TODO"
        AUTHENTICATION_CLEARTEXT_PASSWORD -> fail "TODO"
      fail "TODO"
    _ -> do
      fail "TODO"

 where

  performAuthentication :: AuthenticationResponse -> IO ()
  performAuthentication (AUTHENTICATION_OK) = return ()
  performAuthentication (AUTHENTICATION_KERBEROS_V5) =
    unsupportedAuthenticationMethod "Kerberos V5"
  performAuthentication (AUTHENTICATION_CLEARTEXT_PASSWORD) = do
    password <- LazyByteString.fromStrict <$> getPassword
    sendMessage $ passwordMessage password
    completeAuthentication
  performAuthentication (AUTHENTICATION_MD5_PASSWORD salt) = do
    password <- LazyByteString.fromStrict <$> getPassword
    let md5 = "md5" <> md5hex (md5hex password <> toLazyByteString (word32be salt))
    sendMessage (passwordMessage md5)
    completeAuthentication
  performAuthentication (AUTHENTICATION_SCM_CREDENTIAL) = unsupportedAuthentication "SCM"
  performAuthentication (AUTHENTICATION_GSS) = unsupportedAuthentication "GSS"
  performAuthentication (AUTHENTICATION_SSPI) = unsupportedAuthentication "SSPI"
  performAuthentication (AUTHENTICATION_GSS_CONTINUE s) = unsupportedAuthentication "GSS"
  performAuthentication (AUTHENTICATION_SASL ms) = unsupportedAuthentication "SASL"
  performAuthentication (AUTHENTICATION_SASL_CONTINUE s) = unsupportedAuthentication "SASL"
  performAuthentication (AUTHENTICATION_SASL_FINAL s) = unsupportedAuthentication "SASL"
  performAuthentication (AUTHENTICATION_MISCELLANEOUS t s) = unsupportedAuthenticationMethod ("#" <> show t)

  unsupportedAuthenticationMethod :: String -> IO ()
  unsupportedAuthenticationMethod = throw . UnsupportedAuthenticationMethod

  sendMessage :: LazyByteString -> IO ()
  sendMessage = LazyByteString.sendAll sock . toLazyByteString

  receiveMessageBytes :: IO (Word8, LazyByteString)
  receiveMessageBytes = fail "TODO"

  receiveBytes :: Int -> IO LazyByteString
  receiveBytes n
    | (n <= 0) = return ""
    | otherwise = receiveBytes' (fromIntegral n)

  receiveBytes' :: Int64 -> IO LazyByteString
  receiveBytes' n = do
    s <- LazyByteString.recv sock n
    let m = LazyByteString.length s
    if (m < n) then do
      guard (m > 0) <|> fail "Premature end of input."
      (s <>) <$> receiveBytes' (n - fromIntegral m)
    else do
      return s

abortConnection :: Connection -> IO ()
abortConnection conn = maybe (return ()) (sendBuilder conn . uncurry cancelRequestMessage) (backendKeyData conn)

sendBuilder :: Connection -> Builder -> IO ()
sendBuilder conn = send conn . toLazyByteString

receive :: Connection -> IO (Word8, LazyByteString)
receive conn = do
  (tag, n) <- readBytes conn 5 >>= runGetM ((,) <$> getChar8 <*> getInt32be)
  case compare n 4 of
    LT -> fail $ "Message too short: " <> show tag <> " " <> show n
    EQ -> return (tag, "")
    GT -> (tag,) <$> readBytes conn (fromIntegral n - 4)

runGetM :: (Alternative m, MonadFail m) => Get a -> LazyByteString -> m a
runGetM g s = either reject accept (runGetOrFail g s)
 where
  reject (_, _, msg) = fail msg
  accept (xs, _, x) = x <$ guard (LazyByteString.null xs) <|>
                        fail "Unexpected data found at the end of message"

