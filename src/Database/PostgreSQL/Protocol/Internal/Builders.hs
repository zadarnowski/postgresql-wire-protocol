-- | Module:    Database.PostgreSQL.Protocol.Builders.Utilities
-- Description: Serialisation utilities
-- Copyright:   © 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  pat@jantar.org
-- Stability:   experimental
-- Portability: portable
--
-- This module defines a set of functions and types useful
-- for implementation of the PostgreSQL message builders.
-- It is not exposed from the package as the following interfaces
-- are likely to change in the future.

module Database.PostgreSQL.Protocol.Internal.Builders where

import Database.PostgreSQL.Protocol.Types
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Foldable
import Data.Int
import Data.Word

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Builder as Builder

-- | In the PostgreSQL wire protocol adopts the binary representation of an object
--   on the wire is often preceded by that representation's size, in bytes. In order
--   to implement builders for these objects efficiently, it's prudent to keep track
--   of the expected size of the object's binary representation alongside its
--   actual 'Builder', and the following 'Buffer' type does just that.

data Buffer = B {
  -- | exact length of the byte string represented by this buffer
  bufferSize :: {-# UNPACK #-} !Word32,
  -- | a builder for the buffer's content; must generate a byte string
  --   of the exact size specified by 'bufferSize'
  toBuilder :: Builder
}

instance Semigroup Buffer where
  B s1 b1 <> B s2 b2 = B (s1 + s2) (b1 <> b2)
  {-# INLINE (<>) #-}

instance Monoid Buffer where
  mempty = B 0 mempty
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}
  mconcat = foldl' (<>) mempty
  {-# INLINE mconcat #-}

-- | Converts a buffer to a lazy byte string.
toLazyByteString :: Buffer -> LazyByteString
toLazyByteString = Builder.toLazyByteString . toBuilder
{-# INLINE toLazyByteString #-}

-- | Compacts a 'Builder' by converting it into a strict 'ByteString'
--   and back into a 'Builder'. The result is a builder with the same
--   semantic meaning, but different performance characteristics.
--   In particular, if the builder is invariant and bound to a definition
--   marked @NOINLINE@, this ensures that its byte string is memoised
--   by the GHC runtime instead of being assembled at every invocation point,
--   which provides a non-trivial performance gains for many small PostgreSQL
--   message objects.
compact :: Builder -> Builder
compact = Builder.byteString . LazyByteString.toStrict . Builder.toLazyByteString
{-# INLINE compact #-}

-- | Serialises an ASCII character into a 'Buffer'.
char8 :: Char -> Buffer
char8 = B 1 . Builder.char8
{-# INLINE char8 #-}

-- | Serialises an 8-bit signed integer into a 1-byte Buffer.
int8 :: Int8 -> Buffer
int8 = B 1 . Builder.int8
{-# INLINE int8 #-}

-- | Serialises a 16-bit signed integer into a 2-byte Buffer in big-endian byte order.
int16 :: Int16 -> Buffer
int16 = B 2 . Builder.int16BE
{-# INLINE int16 #-}

-- | Serialises a 32-bit signed integer into a 4-byte Buffer in big-endian byte order.
int32 :: Int32 -> Buffer
int32 = B 4 . Builder.int32BE
{-# INLINE int32 #-}

-- | Serialises the least-significant 8 bits of the supplied number
--   into a 1-byte Buffer; same as @'int8' . 'fromIntegral'@.
num8 :: Integral a => a -> Buffer
num8 = B 1 . Builder.word8 . fromIntegral
{-# INLINE num8 #-}

-- | Serialises the least-significant 16 bits of the supplied number
--   into a 2-byte 'Buffer' in the big-endian order;
--   same as @'int16' . 'fromIntegral'@.
num16 :: Integral a => a -> Buffer
num16 = B 2 . Builder.word16BE . fromIntegral
{-# INLINE num16 #-}

-- | Serialises the least-significant 32 bits of the supplied number
--   into a 4-byte 'Buffer' in the big-endian order;
--   same as @'int32 . 'fromIntegral'@.
num32 :: Integral a => a -> Buffer
num32 = B 4 . Builder.word32BE . fromIntegral
{-# INLINE num32 #-}

-- | Serialises a raw strict 'ByteString' into a buffer.
byteString :: ByteString -> Buffer
byteString s = B (fromIntegral (ByteString.length s)) (Builder.byteString s)
{-# INLINE byteString #-}

-- | Serialises a raw 'LazyByteString' into a buffer.
lazyByteString :: LazyByteString -> Buffer
lazyByteString s = B (fromIntegral (LazyByteString.length s)) (Builder.lazyByteString s)
{-# INLINE lazyByteString #-}

-- | Serialises a 'ByteString' as a NUL-terminated byte sequence;
--   Note that the input is not checked for embedded NUL bytes.
byteStringZ :: ByteString -> Buffer
byteStringZ s = B (1 + fromIntegral (ByteString.length s)) (Builder.byteString s <> Builder.word8 0)
{-# INLINE byteStringZ #-}

-- | Serialises a 'LazyByteString' as a NUL-terminated byte sequence;
--   Note that the input is not checked for embedded NUL bytes.
lazyByteStringZ :: LazyByteString -> Buffer
lazyByteStringZ s = B (1 + fromIntegral (LazyByteString.length s)) (Builder.lazyByteString s <> Builder.word8 0)
{-# INLINE lazyByteStringZ #-}

-- | Serialises an optional 'LazyByteString' into a buffer prefixed with
--   its size as a 32-bit number in the big-endian byte order. 'Nothing'
--   values (which usually represent SQL @NULL@) is serialised as @'int32' (-1)@.
maybeLazyByteString :: Maybe LazyByteString -> Buffer
maybeLazyByteString = maybe (int32 (-1)) (withSize . lazyByteString)
{-# INLINE maybeLazyByteString #-}

-- | Prefixes the specified buffer with its size, serialised as a 32-bit number
--   in the big-endian byte order.
withSize :: Buffer -> Buffer
withSize (B s b) = B (s + 4) (Builder.word32BE s <> b)
{-# INLINE withSize #-}

-- | Prefixes the specified buffer, with a size header consisting of the
--   /resulting/ buffer's size, serialised as a 32-bit number in the
--   big-endian byte order. Similar to 'withSize', except that the emitted
--   size includes the four bytes of the size header itself.
withSizeHeader :: Buffer -> Buffer
withSizeHeader (B s b) = B s' (Builder.word32BE s' <> b)
 where s' = s + 4
{-# INLINE withSizeHeader #-}

-- | @'withHeader' c b@ prefixes the specified buffer @b@
--   with a message header consisting of the supplied byte @c@,
--   followed by the size header equal to @4 + bufferSize b@.
withHeader :: Char -> Buffer -> Buffer
withHeader c b = char8 c <> withSizeHeader b
{-# INLINE withHeader #-}

-- | @'foldMapL16' f bs@ serialises each entry in the list
--   @bs@ using the serialiser @f@, concatenates the results, and
--   prefixes the lot with the length of the list @bs@, expressed
--   as a 16-bit number in the big-endian byte order.
foldMapL16 :: Foldable t => (a -> Buffer) -> t a -> Buffer
foldMapL16 f bs = num16 (length bs) <> foldMap f bs
{-# INLINE foldMapL16 #-}

-- | @'foldMapL32' f bs@ serialises each entry in the list
--   @bs@ using the serialiser @f@, concatenates the results, and
--   prefixes the lot with the length of the list @bs@, expressed
--   as a 32-bit number in the big-endian byte order.
foldMapL32 :: Foldable t => (a -> Buffer) -> t a -> Buffer
foldMapL32 f bs = num32 (length bs) <> foldMap f bs
{-# INLINE foldMapL32 #-}
