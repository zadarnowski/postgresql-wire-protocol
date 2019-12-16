-- | Module:    Database.PostgreSQL.Protocol.Readers
-- Description: Readers for PostgreSQL messages.
-- Copyright:   © 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable
--
-- This module defines low-level readers for all defined PostgreSQL messages,
-- abstracting over the actual source of data, which is represented simply by a
-- @read@ function that retrieves, within a 'MonadFail', a lazy byte string of a
-- fixed size.

module Database.PostgreSQL.Protocol.Readers (
  readSessionMessage,
  readFrontendMessage,
  readBackendMessage
) where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Binary.Get
import Data.Bits
import Data.ByteString (ByteString)
import Data.Char (chr)
import Data.Int
import Prelude hiding (fail)

import Database.PostgreSQL.Protocol.Types

import qualified Data.ByteString.Lazy as LazyByteString

readSessionMessage :: (Alternative m, MonadFail m) => (Int32 -> m LazyByteString) -> m SessionMessage
readSessionMessage readChunk = do
  (n, v) <- readChunk 8 >>= runGetM ((,) <$> getInt32be <*> getWord32be)
  case v of
    80877102 -> do guard (n == 16) <|> invalidMessageLength n "CANCEL_REQUEST"
                   readChunk 8 >>= runGetM (CANCEL_REQUEST <$> getWord32be <*> getWord32be)
    80877103 -> do guard (n == 8) <|> invalidMessageLength n "SSL_REQUEST"
                   return SSL_REQUEST
    _        -> do guard (n >= 8) <|> invalidMessageLength n "STARTUP_MESSAGE"
                   let vm = fromIntegral (v `shiftR` 16)
                       vn = fromIntegral (v .&. 0xFFFF)
                   guard (vm == CURRENT_MAJOR_VERSION && vn == CURRENT_MINOR_VERSION) <|>
                     fail ("invalid protocol version in STARTUP_MESSAGE: " <> show vm <> "." <> show vn)
                   params <- readChunk (n - 8) >>= runGetM getRemainingLazyByteStrings
                   STARTUP_MESSAGE vm vn <$> makeParameters params

 where
  makeParameters (pn:pv:ps') = ((LazyByteString.toStrict pn, pv):) <$> makeParameters ps'
  makeParameters [] = return []
  makeParameters _ = fail "missing session parameter list terminator"

readFrontendMessage :: (Alternative m, MonadFail m) => (Int32 -> m LazyByteString) -> m FrontendMessage
readFrontendMessage readChunk = do
  (t, n) <- readChunk 5 >>= runGetM ((,) <$> getChar8 <*> getInt32be)
  case t of
    'B' -> readMessage n "BIND" readChunk $ BIND
             <$> getByteStringNul
             <*> getByteStringNul
             <*> getArray16be getWord16be
             <*> getArray16be getPgValue
             <*> getArray16be getWord16be
    'C' -> readMessage n "CLOSE" readChunk $ CLOSE
             <$> getWord8
             <*> getRemainingByteStringNul
    'd' -> readMessage n "COPY_IN_DATA" readChunk $ COPY_IN_DATA
             <$> getRemainingLazyByteString
    'c' -> readTrivialMessage n COPY_IN_DONE
    'f' -> readMessage n "COPY_FAIL" readChunk $ COPY_FAIL
             <$> getRemainingLazyByteStringNul
    'D' -> readMessage n "DESCRIBE" readChunk $ DESCRIBE
             <$> getWord8
             <*> getRemainingByteStringNul
    'E' -> readMessage n "EXECUTE" readChunk $ EXECUTE
             <$> getByteStringNul
             <*> getInt32be
    'H' -> readTrivialMessage n FLUSH
    'F' -> readMessage n "FUNCTION_CALL" readChunk $ FUNCTION_CALL
             <$> getWord32be
             <*> getArray16be getWord16be
             <*> getArray16be getPgValue
             <*> getWord16be
    'P' -> readMessage n "PARSE" readChunk $ PARSE
             <$> getByteStringNul
             <*> getLazyByteStringNul
             <*> getArray16be getWord32be
    'p' -> readMessage n "PASSWORD_MESSAGE" readChunk $ PASSWORD_MESSAGE
             <$> getRemainingLazyByteStringNul
    'Q' -> readMessage n "QUERY" readChunk $ QUERY
             <$> getRemainingLazyByteStringNul
    'S' -> readTrivialMessage n SYNC
    'X' -> readTrivialMessage n TERMINATE
    _   -> fail ("unrecognized message received from database frontend: " <> show t)

readBackendMessage :: (Alternative m, MonadFail m) => (Int32 -> m LazyByteString) -> m BackendMessage
readBackendMessage readChunk = do
  (t, n) <- readChunk 5 >>= runGetM ((,) <$> getChar8 <*> getInt32be)
  case t of
    'R' -> readMessage n "AUTHENTICATION_RESPONSE" readChunk $ AUTHENTICATION_RESPONSE
             <$> getAuthenticationResponse
    'K' -> readMessage n "BACKEND_KEY_DATA" readChunk $ BACKEND_KEY_DATA
             <$> getWord32be
             <*> getWord32be
    '2' -> readTrivialMessage n BIND_COMPLETE
    '3' -> readTrivialMessage n CLOSE_COMPLETE
    'C' -> readMessage n "COMMAND_COMPLETE" readChunk $ COMMAND_COMPLETE
             <$> getRemainingLazyByteString
    'd' -> readMessage n "COPY_OUT_DATA" readChunk $ COPY_OUT_DATA
             <$> getRemainingLazyByteString
    'c' -> readTrivialMessage n COPY_OUT_DONE
    'G' -> readMessage n "COPY_IN_RESPONSE" readChunk $ COPY_IN_RESPONSE
             <$> getWord8
             <*> getArray16be getWord16be
    'H' -> readMessage n "COPY_OUT_RESPONSE" readChunk $ COPY_OUT_RESPONSE
             <$> getWord8
             <*> getArray16be getWord16be
    'W' -> readMessage n "COPY_BOTH_RESPONSE" readChunk $ COPY_BOTH_RESPONSE
             <$> getWord8
             <*> getArray16be getWord16be
    'D' -> readMessage n "DATA_ROW" readChunk $ DATA_ROW
             <$> getArray16be getPgValue
    'I' -> readTrivialMessage n EMPTY_QUERY_RESPONSE
    'E' -> readMessage n "ERROR_RESPONSE" readChunk $ ERROR_RESPONSE
             <$> getNoticeFields
    'V' -> readMessage n "FUNCTION-CALL_RESPONSE" readChunk $ FUNCTION_CALL_RESPONSE
             <$> getPgValue
    'n' -> readTrivialMessage n NO_DATA
    'N' -> readMessage n "NOTICE_RESPONSE" readChunk $ NOTICE_RESPONSE
             <$> getNoticeFields
    'A' -> readMessage n "NOTIFICATION_RESPONSE" readChunk $ NOTIFICATION_RESPONSE
             <$> getWord32be
             <*> getByteStringNul
             <*> getRemainingLazyByteStringNul
    't' -> readMessage n "PARAMETER_DESCRIPTION" readChunk $ PARAMETER_DESCRIPTION
             <$> getArray16be getWord32be
    'S' -> readMessage n "PARAMETER_STATUS" readChunk $ PARAMETER_STATUS
             <$> getByteStringNul
             <*> getRemainingLazyByteStringNul
    '1' -> readTrivialMessage n PARSE_COMPLETE
    's' -> readTrivialMessage n PORTAL_SUSPENDED
    'Z' -> readMessage n "READY_FOR_QUERY" readChunk $ READY_FOR_QUERY
             <$> getWord8
    'T' -> readMessage n "ROW_DESCRIPTION" readChunk $ ROW_DESCRIPTION
             <$> getArray16be getFieldDescription
    _   -> fail ("unrecognized message received from database backend: " <> show t)

getAuthenticationResponse :: Get AuthenticationResponse
getAuthenticationResponse = do
  t <- getWord32be
  case t of
    0  -> return AUTHENTICATION_OK
    2  -> return AUTHENTICATION_KERBEROS_V5
    3  -> return AUTHENTICATION_CLEARTEXT_PASSWORD
    5  -> AUTHENTICATION_MD5_PASSWORD <$> getWord32be
    6  -> return AUTHENTICATION_SCM_CREDENTIAL
    7  -> return AUTHENTICATION_GSS
    8  -> AUTHENTICATION_GSS_CONTINUE <$> getRemainingLazyByteString
    9  -> return AUTHENTICATION_SSPI
    10 -> AUTHENTICATION_SASL . fmap LazyByteString.toStrict <$> getRemainingLazyByteStrings
    11 -> AUTHENTICATION_SASL_CONTINUE <$> getRemainingLazyByteString
    12 -> AUTHENTICATION_SASL_FINAL <$> getRemainingLazyByteString
    _  -> AUTHENTICATION_MISCELLANEOUS t <$> getRemainingLazyByteString

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

readMessage :: (Alternative m, MonadFail m) => Int32 -> String -> (Int32 -> m LazyByteString) -> Get a -> m a
readMessage n k readChunk g = do
  guard (n >= 4) <|> invalidMessageLength n k
  readChunk n >>= runGetM g

readTrivialMessage :: (Alternative m, MonadFail m, Show a) => Int32 -> a -> m a
readTrivialMessage n k = k <$ guard (n == 4) <|> invalidMessageLength n (show k)

invalidMessageLength :: MonadFail m => Int32 -> String -> m a
invalidMessageLength n k = fail ("Invalid " <> k <> " message length: " <> show n)

runGetM :: (Alternative m, MonadFail m) => Get a -> LazyByteString -> m a
runGetM g s = either reject accept (runGetOrFail g s)
 where
  reject (_, _, msg) = fail msg
  accept (xs, _, x) = x <$ guard (LazyByteString.null xs) <|>
                        fail "Unexpected data found at the end of message"

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
  guard (n >= 0) <|> fail ("Invalid array length: " <> show n)
  replicateM (fromIntegral n) g

getPgValue :: Get (Maybe LazyByteString)
getPgValue = do
  n <- getInt32be
  if (n == -1) then do
    return Nothing
  else do
    guard (n >= 0) <|> fail ("Invalid value length: " <> show n)
    Just <$> getLazyByteString (fromIntegral n)
