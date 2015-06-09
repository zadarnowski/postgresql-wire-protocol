> -- | Module:    Database.PostgreSQL.Protocol.Parsers
> -- Description: Parsers for PostgreSQL messages.
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

> module Database.PostgreSQL.Protocol.Parsers (
>   sessionMessageParser,
>   frontendMessageParser,
>   backendMessageParser
> ) where

> import Data.Serialize.Get

> import Database.PostgreSQL.Protocol.Types

> readSessionMessage :: MonadPlus m => (Int32 -> m Lazy.ByteString) -> m SessionMessage
> readSessionMessage read = do
>   (n, s) <- read 8 >>= run getHeader
>   case s of
>     80877102 -> guard (n == 16) >> read 8 >>= run getCancelRequest
>     80877103 -> guard (n ==  8) >> return SSLRequest
>     _        -> guard (n >=  8) >> read (n - 8) >>= return . Lazy.split 0 >>= parseStartupMessage
>  where
>   getHeader = liftM2 (,) getWord32be getWord32be
>   getCancelRequest = liftM2 CancelRequest getWord32be getWord32be
>   parseStartupMessage ps = do
>     let ps = Lazy.split 0 inp

> frontendMessageParser :: Lazy.ByteString -> Maybe SessionMessage
> frontendMessageParser = run getFrontendMessage

> backendMessageParser :: Lazy.ByteString -> Maybe BackendMessage
> backendMessageParser = run getBackendMessage

> run :: MonadPlus m => Get a -> Lazy.ByteString -> m a
> run g inp = case runGetLazyState g inp of
>               Right (x, inp') | Lazy.null inp' -> return x
>               _ -> mzero
