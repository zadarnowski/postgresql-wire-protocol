-- | Module:    Database.PostgreSQL.Protocol.Decoders
-- Description: Decoders for PostgreSQL messages.
-- Copyright:   © 2015-2020 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable
--
-- This module defines low-level readers for all defined PostgreSQL messages,
-- abstracting over the actual source of data, which is represented simply by
-- a user-supplied @read@ function that retrieves, within a 'MonadFail', a lazy
-- byte string of a fixed size.
--
-- The @read@ function is always invoked with a strictly-positive integer, and
-- is expected to return a lazy byte string of precisely that number of input
-- bytes.  Any other result length will be interpreted as a message parse error
-- handled with a call to the monad's 'fail' method.
--
-- In order to protect against denial of service attacks, the message parsers
-- impose reasonable internal limits on the maximum size of messages they are
-- willing to receive (as appropriate to the specific communication context),
-- so that the @read@ functions should assume that the requested number of
-- bytes is always reasonable and warranted.

module Database.PostgreSQL.Protocol.Decoders (
  sessionMessage,
  frontendMessage,
  backendMessage
) where

import Control.Monad
import Data.Binary.Get
import Data.Bits
import Data.ByteString (ByteString)
import Data.Char (chr)
import Data.Int
import Prelude hiding (fail)

import Database.PostgreSQL.Protocol.Types

import qualified Data.ByteString.Lazy as LazyByteString

-- | Reads and parsers a 'SessionMessage' from a monadic input stream.
-- @sessionMessage r@ loads the message data using the supplied monadic method
-- @r@, which it invokes (possibly repeatedly) with the number of input bytes
-- required. The number is always a strictly-positive 32-bit integer. The
-- input method is expected to return precisely the requested number of bytes
-- as a lazy byte string, and any other result will be interpreted as an error.
-- If the supplied message cannot be decoded for any reason, @sessionMessage@
-- responds by invoking the monad's 'fail' method with a message which
-- attempts to provide a human-readable description of the error condition,
-- suitable for debugging purposes rather than end-user consumption.
--
-- In order to protect against denial of service attacks, the maximum size of a
-- 'StartupMessage' accepted by this method is 10000 bytes, same as the limit
-- imposed by the @libpq@ library.
sessionMessage :: MonadFail m => (Int -> m LazyByteString) -> m SessionMessage
sessionMessage readBytes = do
  (n, v) <- readBytes 8 >>= runGetM ((,) <$> getInt32be <*> getWord32be)
  case v of
    80877102 -> do when (n /= 16) $ invalidMessageLength n "CancelRequest"
                   readBytes 8 >>= runGetM (CancelRequest <$> getWord32be <*> getWord32be)
    80877103 -> do when (n /= 8) $ invalidMessageLength n "SSLRequest"
                   return SSLRequest
    80877104 -> do when (n /= 8) $ invalidMessageLength n "GSSENCRequest"
                   return GSSENCRequest
    _        -> do when (n < 8 || n > 10000) $ invalidMessageLength n "StartupMessage"
                   let vm = fromIntegral (v `shiftR` 16)
                       vn = fromIntegral (v .&. 0xFFFF)
                   when (vm /= CURRENT_MAJOR_VERSION || vn /= CURRENT_MINOR_VERSION) $
                     fail ("invalid protocol version in StartupMessage: " <> show vm <> "." <> show vn)
                   params <- readBytes (fromIntegral n - 8) >>= runGetM getRemainingLazyByteStrings
                   StartupMessage vm vn <$> makeParameters params

 where
  makeParameters (pn:pv:ps') = ((LazyByteString.toStrict pn, LazyByteString.toStrict pv):) <$> makeParameters ps'
  makeParameters [] = return []
  makeParameters _ = fail "missing session parameter list terminator"

-- | Reads and parsers a 'FrontendMessage' from a monadic input stream.
-- @frontendMessage r@ loads the message data using the supplied monadic method
-- @r@, which it invokes (possibly repeatedly) with the number of input bytes
-- required. The number is always a strictly-positive 32-bit integer. The
-- input method is expected to return precisely the requested number of bytes
-- as a lazy byte string, and any other result will be interpreted as an error.
-- If the supplied message cannot be decoded for any reason, @frontendMessage@
-- responds by invoking the monad's 'fail' method with a message which
-- attempts to provide a human-readable description of the error condition,
-- suitable for debugging purposes rather than end-user consumption.
frontendMessage :: MonadFail m => (Int -> m LazyByteString) -> m FrontendMessage
frontendMessage readBytes = do
  (t, n) <- readBytes 5 >>= runGetM ((,) <$> getChar8 <*> getInt32be)
  case t of
    'B' -> readSmallMessage n "Bind" readBytes $ Bind
             <$> getByteStringNul
             <*> getByteStringNul
             <*> getArray16be getWord16be
             <*> getArray16be getPgValue
             <*> getArray16be getWord16be
    'C' -> readSmallMessage n "Close" readBytes $ Close
             <$> getWord8
             <*> getRemainingByteStringNul
    'd' -> readLargeMessage n "CopyInData" readBytes $ CopyInData
             <$> getRemainingLazyByteString
    'c' -> readTrivialMessage n CopyInDone
    'f' -> readSmallMessage n "CopyFail" readBytes $ CopyFail
             <$> getRemainingLazyByteStringNul
    'D' -> readSmallMessage n "Describe" readBytes $ Describe
             <$> getWord8
             <*> getRemainingByteStringNul
    'E' -> readSmallMessage n "Execute" readBytes $ Execute
             <$> getByteStringNul
             <*> getInt32be
    'H' -> readTrivialMessage n Flush
    'F' -> readLargeMessage n "FunctionCall" readBytes $ FunctionCall
             <$> getWord32be
             <*> getArray16be getWord16be
             <*> getArray16be getPgValue
             <*> getWord16be
    'P' -> readLargeMessage n "Parse" readBytes $ Parse
             <$> getByteStringNul
             <*> getLazyByteStringNul
             <*> getArray16be getWord32be
    'p' -> readSmallMessage n "PasswordMessage" readBytes $ PasswordMessage
             <$> getRemainingLazyByteStringNul
    'Q' -> readLargeMessage n "Query" readBytes $ Query
             <$> getRemainingLazyByteStringNul
    'S' -> readTrivialMessage n Sync
    'X' -> readTrivialMessage n Terminate
    _   -> fail ("unrecognized message received from database frontend: " <> show t)

-- | Reads and parsers a 'BackendMessage' from a monadic input stream.
-- @backendMessage r@ loads the message data using the supplied monadic method
-- @r@, which it invokes (possibly repeatedly) with the number of input bytes
-- required. The number is always a strictly-positive 32-bit integer. The
-- input method is expected to return precisely the requested number of bytes
-- as a lazy byte string, and any other result will be interpreted as an error.
-- If the supplied message cannot be decoded for any reason, @backendMessage@
-- responds by invoking the monad's 'fail' method with a message which
-- attempts to provide a human-readable description of the error condition,
-- suitable for debugging purposes rather than end-user consumption.
backendMessage :: MonadFail m => (Int -> m LazyByteString) -> m BackendMessage
backendMessage readBytes = do
  (t, n) <- readBytes 5 >>= runGetM ((,) <$> getChar8 <*> getInt32be)
  case t of
    'R' -> readSmallMessage n "AuthenticationResponse" readBytes $ AuthenticationResponse
             <$> getAuthenticationResponse
    'K' -> readSmallMessage n "BackendKeyData" readBytes $ BackendKeyData
             <$> getWord32be
             <*> getWord32be
    '2' -> readTrivialMessage n BindComplete
    '3' -> readTrivialMessage n CloseComplete
    'C' -> readSmallMessage n "CommandComplete" readBytes $ CommandComplete
             <$> getRemainingLazyByteString
    'd' -> readLargeMessage n "CopyOutData" readBytes $ CopyOutData
             <$> getRemainingLazyByteString
    'c' -> readTrivialMessage n CopyOutDone
    'G' -> readSmallMessage n "CopyInResponse" readBytes $ CopyInResponse
             <$> getWord8
             <*> getArray16be getWord16be
    'H' -> readSmallMessage n "CopyOutResponse" readBytes $ CopyOutResponse
             <$> getWord8
             <*> getArray16be getWord16be
    'W' -> readSmallMessage n "CopyBothResponse" readBytes $ CopyBothResponse
             <$> getWord8
             <*> getArray16be getWord16be
    'D' -> readLargeMessage n "DataRow" readBytes $ DataRow
             <$> getArray16be getPgValue
    'I' -> readTrivialMessage n EmptyQueryResponse
    'E' -> readSmallMessage n "ErrorResponse" readBytes $ ErrorResponse
             <$> getNoticeFields
    'V' -> readLargeMessage n "FunctionCallResponse" readBytes $ FunctionCallResponse
             <$> getPgValue
    '?' -> readSmallMessage n "NegotiateProtocolVersion" readBytes $ NegotiateProtocolVersion
             <$> getWord32be
             <*> getArray16be getByteStringNul
    'n' -> readTrivialMessage n NoData
    'N' -> readSmallMessage n "NoticeResponse" readBytes $ fmap AsynchronousMessage $ NoticeResponse
             <$> getNoticeFields
    'A' -> readLargeMessage n "NotificationResponse" readBytes $ fmap AsynchronousMessage $ NotificationResponse
             <$> getWord32be
             <*> getByteStringNul
             <*> getRemainingLazyByteStringNul
    't' -> readSmallMessage n "ParameterDescription" readBytes $ ParameterDescription
             <$> getArray16be getWord32be
    'S' -> readSmallMessage n "ParameterStatus" readBytes $ fmap AsynchronousMessage $ ParameterStatus
             <$> getByteStringNul
             <*> getRemainingLazyByteStringNul
    '1' -> readTrivialMessage n ParseComplete
    's' -> readTrivialMessage n PortalSuspended
    'Z' -> readSmallMessage n "ReadyForQuery" readBytes $ ReadyForQuery
             <$> getWord8
    'T' -> readSmallMessage n "RowDescription" readBytes $ RowDescription
             <$> getArray16be getFieldDescription
    _   -> fail ("unrecognized message received from database backend: " <> show t)

readMessage :: MonadFail m => Int32 -> Int32 -> String -> (Int -> m LazyByteString) -> Get a -> m a
readMessage maxN n k readBytes g = do
  when (n < 4 || n > maxN) $ invalidMessageLength n k
  readBytes (fromIntegral n) >>= runGetM g

readLargeMessage :: MonadFail m => Int32 -> String -> (Int -> m LazyByteString) -> Get a -> m a
readLargeMessage = readMessage (64 * 1024 * 1024)

readSmallMessage :: MonadFail m => Int32 -> String -> (Int -> m LazyByteString) -> Get a -> m a
readSmallMessage = readMessage (1 * 1024 * 1024)

readTrivialMessage :: (MonadFail m, Show a) => Int32 -> a -> m a
readTrivialMessage n k = k <$ when (n /= 4) (invalidMessageLength n (show k))

invalidMessageLength :: MonadFail m => Int32 -> String -> m a
invalidMessageLength n k = fail ("Invalid " <> k <> " message length: " <> show n)

runGetM :: MonadFail m => Get a -> LazyByteString -> m a
runGetM g s = either reject accept (runGetOrFail g s)
 where
  accept (xs, _, x) = x <$ unless (LazyByteString.null xs) (fail "Unexpected data found at the end of message")
  reject (_, _, msg) = fail msg

getAuthenticationResponse :: Get AuthenticationResponse
getAuthenticationResponse = do
  t <- getWord32be
  case t of
    0  -> return AuthenticationOk
    2  -> return AuthenticationKerberosV5
    3  -> return AuthenticationCleartextPassword
    5  -> AuthenticationMD5Password <$> getWord32be
    6  -> return AuthenticationSCMCredential
    7  -> return AuthenticationGSS
    8  -> AuthenticationGSSContinue <$> getRemainingLazyByteString
    9  -> return AuthenticationSSPI
    10 -> AuthenticationSASL . fmap LazyByteString.toStrict <$> getRemainingLazyByteStrings
    11 -> AuthenticationSASLContinue <$> getRemainingLazyByteString
    12 -> AuthenticationSASLFinal <$> getRemainingLazyByteString
    _  -> AuthenticationOther t <$> getRemainingLazyByteString

getNoticeFields :: Get [NoticeField]
getNoticeFields =
  getRemainingLazyByteStrings >>=
  traverse (maybe (fail "Missing notice field tag") return . LazyByteString.uncons)

getFieldDescription :: Get FieldDescription
getFieldDescription = FIELD_DESCRIPTION
  <$> getByteStringNul
  <*> getWord32be
  <*> getInt16be
  <*> getWord32be
  <*> getInt16be
  <*> getWord32be
  <*> getWord16be

getChar8 :: Get Char
getChar8 = chr . fromIntegral <$> getWord8

getByteStringNul :: Get ByteString
getByteStringNul = LazyByteString.toStrict <$> getLazyByteStringNul

getRemainingByteStringNul :: Get ByteString
getRemainingByteStringNul = LazyByteString.toStrict <$> getRemainingLazyByteStringNul

getRemainingLazyByteStringNul :: Get LazyByteString
getRemainingLazyByteStringNul = getRemainingLazyByteString >>= stripNulTerminator

stripNulTerminator :: LazyByteString -> Get LazyByteString
stripNulTerminator s =
  case LazyByteString.unsnoc s of
    Just (s', 0) -> return s'
    _ -> fail "Missing NUL string terminator"

-- Parse the remainder of a message consisting of a list of NUL-terminated
-- lazy byte strings, that is itself terminated by an additional NUL terminator.
getRemainingLazyByteStrings :: Get [LazyByteString]
getRemainingLazyByteStrings = do
  s <- getRemainingLazyByteStringNul
  if LazyByteString.null s then
    return []
  else
    LazyByteString.split 0 <$> stripNulTerminator s

getArray16be :: Get a -> Get [a]
getArray16be g = do
  n <- getInt16be
  unless (n >= 0) $ fail ("Invalid array length: " <> show n)
  replicateM (fromIntegral n) g

getPgValue :: Get (Maybe LazyByteString)
getPgValue = do
  n <- getInt32be
  if (n == -1) then do
    return Nothing
  else do
    unless (n >= 0) $ fail ("Invalid value length: " <> show n)
    Just <$> getLazyByteString (fromIntegral n)
