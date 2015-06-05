> -- | Module:    Database.PostgreSQL.Protocol.Internal.Builders
> -- Description: Helper functions for message builders module
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- This module defines a number of private helper functions used to simplify implementation
> -- of builders for PostgreSQL protocol message types in "Database.PostgreSQL.Protocol.Builders".
> -- These helper functions, which are kept internal to the package, are distilled into a separate
> -- module in order to eliminate the need for tediously-long export list in "Database.PostgreSQL.Protocol.Builders".

> {-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

> module Database.PostgreSQL.Protocol.Internal.Builders (
>   fromIntegralCheckBounds,
>   fromIntegralCheckMaxBound,
>   pgString, pgStringSize,
>   pgLazyString, pgLazyStringSize,
>   pgValue, pgValueSize,
>   pgArray, pgArraySize,
>   pgUArray, pgUArraySize,
> ) where

> import Control.Exception
> import Data.Array.Base
> import Data.ByteString (ByteString)
> import Data.ByteString.Builder
> import Data.Int
> import Data.Monoid

> import Database.PostgreSQL.Protocol.Types

> import qualified Data.ByteString as ByteString
> import qualified Data.ByteString.Lazy as Lazy

> fromIntegralCheckBounds :: (Integral a, Integral b, Num b) => b -> b -> a -> b
> fromIntegralCheckBounds b1 b2 x | fromIntegral b1 <= x && x <= fromIntegral b2 = fromIntegral x
>                                 | otherwise = throw Overflow

> fromIntegralCheckMaxBound :: (Integral a, Integral b, Num b) => b -> a -> b
> fromIntegralCheckMaxBound b x | x <= fromIntegral b = fromIntegral x
>                               | otherwise = throw Overflow

> pgString :: ByteString -> Builder
> pgString s = byteString s <> char8 '\0'

> pgStringSize :: ByteString -> Int32
> pgStringSize x = fromIntegral (ByteString.length x) + 1

> pgLazyString :: Lazy.ByteString -> Builder
> pgLazyString s = lazyByteString s <> char8 '\0'

> pgLazyStringSize :: Lazy.ByteString -> Int32
> pgLazyStringSize x = fromIntegral (Lazy.length x) + 1

> pgValue :: Value -> Builder
> pgValue = maybe (int32BE (-1)) pgValue'
>  where pgValue' x = int32BE (fromIntegral (Lazy.length x)) <> lazyByteString x

> pgValueSize :: Value -> Int32
> pgValueSize = maybe 4 pgValueSize'
>  where pgValueSize' x = 4 + fromIntegral (Lazy.length x)

> pgArray :: (e -> Builder) -> Array16 e -> Builder
> pgArray elementBuilder xs = int16BE (fromIntegralCheckMaxBound maxBound (numElements xs)) <> mconcat (map elementBuilder (elems xs))

> pgArraySize :: (e -> Int32) -> Array16 e -> Int32
> pgArraySize elementSize xs = 2 + sum (map elementSize (elems xs))

> pgUArray :: IArray UArray e => (e -> Builder) -> UArray16 e -> Builder
> pgUArray elementBuilder xs = int16BE (fromIntegralCheckMaxBound maxBound (numElements xs)) <> mconcat (map elementBuilder (elems xs))

> pgUArraySize :: IArray UArray e => Int32 -> UArray16 e -> Int32
> pgUArraySize elementSize xs = 2 + elementSize * fromIntegral (numElements xs)

