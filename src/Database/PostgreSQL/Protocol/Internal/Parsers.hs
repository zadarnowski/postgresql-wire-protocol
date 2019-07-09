{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

-- | Module:    Database.PostgreSQL.Protocol.Parsers.Utilities
-- Description: Parsing utilities
-- Copyright:   © 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  pat@jantar.org
-- Stability:   experimental
-- Portability: portable
--
-- This module defines a set of functions and types useful
-- for parsing of the PostgreSQL wire protocol data. It is
-- not exposed from the package as the following interfaces
-- are likely to change in the future; for now, they're simple
-- wrappers around the "Data.Serialize.Get" monad, but it's
-- likely to change in the future.

module Database.PostgreSQL.Protocol.Internal.Parsers (
  Parser, parseByteString, parseLazyByteString,
  int8, int16, int32, int64,
  num8, num16, num32, num64,
  char8, float, double,
  byteStringZ, lazyByteStringZ,
  byteStringL32, lazyByteStringL32,
  remainingByteString, remainingLazyByteString,
  maybeByteStringL32, maybeLazyByteStringL32,
  listL16, listL32,
  end
) where

import Database.PostgreSQL.Protocol.Types

import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Bool
import Data.ByteString (ByteString)
import Data.Char (chr)
import Data.Int
import Data.Word
import Prelude hiding (fail)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Serialize.Get as Cereal
import qualified Data.Serialize.IEEE754 as Cereal

newtype Parser a = P { fromP :: Cereal.Get a }
  deriving (Applicative, Functor, Monad, MonadFail)

parseByteString :: MonadFail m => Parser a -> ByteString -> m a
parseByteString p = either fail return . Cereal.runGet (fromP p)

parseLazyByteString :: MonadFail m => Parser a -> LazyByteString -> m a
parseLazyByteString p = either fail return . Cereal.runGetLazy (fromP p)

int8 :: Parser Int8
int8 = P Cereal.getInt8

int16 :: Parser Int16
int16 = P Cereal.getInt16be

int32 :: Parser Int32
int32 = P Cereal.getInt32be

int64 :: Parser Int64
int64 = P Cereal.getInt64be

num8 :: Num a => Parser a
num8 = fromIntegral <$> int8

num16 :: Num a => Parser a
num16 = fromIntegral <$> int16

num32 :: Num a => Parser a
num32 = fromIntegral <$> int32

num64 :: Num a => Parser a
num64 = fromIntegral <$> int64

float :: Parser Float
float = P Cereal.getFloat32be

double :: Parser Double
double = P Cereal.getFloat64be

char8 :: Parser Char
char8 = chr <$> num8

byteStringZ :: Parser ByteString
byteStringZ = P $ do
  (bs, n) <- Cereal.lookAhead $ do
    bs <- Cereal.remaining >>= Cereal.getBytes
    case ByteString.elemIndex 0 bs of
      Just ix -> return (ByteString.take ix bs, ix)
      Nothing -> fail "Missing NUL string terminator"
  Cereal.skip n
  return bs

lazyByteStringZ :: Parser LazyByteString
lazyByteStringZ = LazyByteString.fromStrict <$> byteStringZ

byteStringL32 :: Parser ByteString
byteStringL32 = P (Cereal.getInt32be >>= Cereal.getBytes . fromIntegral)

lazyByteStringL32 :: Parser LazyByteString
lazyByteStringL32 = LazyByteString.fromStrict <$> byteStringL32

remainingByteString :: Parser ByteString
remainingByteString = P (Cereal.remaining >>= Cereal.getBytes)

remainingLazyByteString :: Parser LazyByteString
remainingLazyByteString = LazyByteString.fromStrict <$> remainingByteString

maybeByteStringL32 :: Parser (Maybe ByteString)
maybeByteStringL32 = P $ do
  n <- fromIntegral <$> Cereal.getInt32be
  if n < 0 then do
    when (n /= (-1)) $ fail $ "Negative value length: " <> show n
    return Nothing
  else
    Just <$> Cereal.getBytes n

maybeLazyByteStringL32 :: Parser (Maybe LazyByteString)
maybeLazyByteStringL32 = P $ do
  n <- fromIntegral <$> Cereal.getInt32be
  if n < 0 then do
    when (n /= (-1)) $ fail $ "Negative value length: " <> show n
    return Nothing
  else
    Just . LazyByteString.fromStrict <$> Cereal.getBytes n

listL16 :: Parser a -> Parser [a]
listL16 p = do
  n::Word16 <- num16
  replicateM (fromIntegral n) p

listL32 :: Parser a -> Parser [a]
listL32 p = do
  n <- num32
  replicateM n p

end :: Parser ()
end = P Cereal.isEmpty >>= bool (fail "Excess data found at the end of message") (return ())
