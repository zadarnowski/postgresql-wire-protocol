-- | Module:    Database.PostgreSQL.Protocol.Internal.Builders
-- Description: Helper functions for message serialisation
-- Copyright:   © 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable
--
-- This module defines a number of private helper functions used to simplify
-- implementation of builders for PostgreSQL protocol message types in
-- "Database.PostgreSQL.Protocol.Builders".  These helper functions, which are
-- kept internal to the package, are distilled into a separate module in order
-- to eliminate the need for tediously-long export list in
-- "Database.PostgreSQL.Protocol.Builders".

module Database.PostgreSQL.Protocol.Internal.Builders where

import Control.Exception
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Word

import Database.PostgreSQL.Protocol.Tags
import Database.PostgreSQL.Protocol.Types

import qualified Data.ByteString.Lazy as LazyByteString

-- | Converts a builder into a compact lazy byte string consisting of only a
-- single chunk. This is intended to be used with parameterless messages, where
-- the compacted the lazy byte string needs to be assembled only once, but for
-- maximum benefit, the resulting function should be marked with the @NOINLINE@
-- pragma.
toCompactLazyByteString :: Builder -> LazyByteString
toCompactLazyByteString =
  LazyByteString.fromStrict . LazyByteString.toStrict . toLazyByteString

-- | Adds a full PostgreSQL message header to the front of a message,
-- consisting of the tag byte extracted from the least-significant 8 bits of
-- the specified character, and followed by a 32-bit big-endian integer
-- depicting the message size in bytes, inclusive of the size header but
-- excluding the tag byte. For maximum compatibility, messages larger than
-- @2^31-1@ bytes will be rejected with an 'Overflow' exception.
withMessageHeader :: Word8 -> Builder -> Builder
withMessageHeader tag = (word8 tag <>) . withMessageSizeHeader

-- | Adds an untagged size header to the front of a message builder, consisting
-- of the resulting message size in bytes, inclusive of the header itself,
-- encoded as a 32-bit big-endian integer. For maximum compatibility, messages
-- larger than @2^31-1@ bytes will be rejected with an 'Overflow' exception.
withMessageSizeHeader :: Builder -> Builder
withMessageSizeHeader b =
  (int32BE $ fromIntegral $ 4 + LazyByteString.length s) <> lazyByteString s
 where s = toLazyByteString b

-- | Adds an authentication response header, consisting of a normal message
-- header with the tag byte 'R', followed by a 32-bit big-endian integer
-- depicting the type of authentication response being sent.
withAuthenticationResponseHeader :: Word32 -> Builder -> Builder
withAuthenticationResponseHeader k b =
  withMessageHeader AUTHENTICATION_RESPONSE (word32BE k <> b)

-- | Constructs a builder for a NUL-terminated strict byte string, which is
-- assumed to include no embedded NUL bytes, but not checked for these.
byteStringNul :: ByteString -> Builder
byteStringNul s = byteString s <> char8 '\0'

-- | Constructs a builder for a NUL-terminated lazy byte string, which is
-- assumed to include no embedded NUL bytes, but not checked for these.
lazyByteStringNul :: LazyByteString -> Builder
lazyByteStringNul s = lazyByteString s <> char8 '\0'

-- | Constructs a builder for a lazy byte string prefixed with a 32-bit
-- big-endian integer representing the string's length in bytes. For maximum
-- compatibility, strings longer than @2^31-1@ bytes will be rejected with an
-- 'Overflow' exception.
lazyByteString32BE :: LazyByteString -> Builder
lazyByteString32BE s =
  int32BE (fromIntegralCheckMaxBound (LazyByteString.length s)) <>
  lazyByteString s

-- | Constructs a builder for PostgreSQL values, in which NULL values are
-- represented as 'Nothing' and encoded as the integer @-1@, and all other
-- values, represented by lazy byte strings, are encoded using
-- 'lazyByteString32BE'. For maximum compatibility, values larger than
-- @2^31-1@ bytes will be rejected with an 'Overflow' exception.
pgValue :: Maybe LazyByteString -> Builder
pgValue = maybe (int32BE (-1)) lazyByteString32BE

-- | Constructs a builder for a PostgreSQL array of fixed-size objects
-- serialised individually using the specified builder, and collectively
-- prefixed by a 16-bit integer depicting the number of elements in the array.
-- For maximum compatibility, arrays with more than @2^16-1@ elements be
-- rejected with an 'Overflow' exception.
array16BE :: Foldable t => (e -> Builder) -> t e -> Builder
array16BE element xs =
  int16BE (fromIntegralCheckMaxBound (length xs)) <>
  foldMap element xs

fromIntegralCheckMaxBound :: (Integral a, Integral b, Num b, Bounded b) => a -> b
fromIntegralCheckMaxBound x
  | (x <= fromIntegral (maxBound `asTypeOf` y)) = y
  | otherwise = throw Overflow
 where y = fromIntegral x
