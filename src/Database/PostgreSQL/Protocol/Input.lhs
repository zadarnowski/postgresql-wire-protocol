> -- | Module:    Database.PostgreSQL.Protocol.Input
> -- Description: Input stream for PostgreSQL messages.
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- This module defines low-level builders for all defined PostgreSQL messages.
> -- At this level of abstraction, all message fields are rendered directly as
> -- binary data, with little or no marshalling into any more meaningful Haskell
> -- types, in order to provided higher-level libraries with maximum possible
> -- freedom of behaviour.

> module Database.PostgreSQL.Protocol.Input (
>   Input (..)
> ) where

> import Data.Int

> import qualified Data.ByteString.Lazy as Lazy

> data Input = Input {
>   readInput :: Int32 -> Lazy.ByteString
> }
