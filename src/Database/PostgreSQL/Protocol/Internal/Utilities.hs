-- | Module:    Database.PostgreSQL.Protocol.Internal.Utilities
-- Description: Miscellaneous Functions
-- Copyright:   © 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable

module Database.PostgreSQL.Protocol.Internal.Utilities (
  md5hex
) where

import Data.Bits
import Data.Char
import Database.PostgreSQL.Protocol.Types

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString

md5hex :: LazyByteString -> LazyByteString
md5hex = LazyByteString.pack . ByteString.foldr' toHex [] . MD5.hashlazy
 where
  toHex x xs = toHexChar (x `shiftR` 4) : toHexChar (x .&. 15) : xs
  toHexChar b
    | (b < 10)  = b + fromIntegral (ord '0')
    | otherwise = b - 10 + fromIntegral (ord 'a')
