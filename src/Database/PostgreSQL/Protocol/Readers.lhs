> -- | Module:    Database.PostgreSQL.Protocol.Readers
> -- Description: Readers for PostgreSQL messages.
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- This module defines low-level readers for all defined PostgreSQL messages,
> -- abstracting over the actual source of data, which is represented simply
> -- by a @read@ function that retrieves within a 'MonadPlus' a lazy bytestring
> -- of a fixed size.

> module Database.PostgreSQL.Protocol.Readers (
>   readSessionMessage,
>   readFrontendMessage,
>   readBackendMessage
> ) where

> import Data.Serialize.Get

> import Database.PostgreSQL.Protocol.Types

> readSessionMessage :: MonadPlus m => (Int32 -> m Lazy.ByteString) -> m SessionMessage
> readSessionMessage read = do
>   (n, v) <- read 8 >>= run getHeader
>   case v of
>     80877102 -> do guard (n == 16)
>                    req <- read 8
>                    run getCancelRequest req
>     80877103 -> do guard (n == 8)
>                    return SSLRequest
>     _        -> do guard (n >= 8)
>                    let (vm', vn') = v `div` 65536
>                        vm = fromIntegral vm'
>                        vn = fromIntegral vn'
>                    guard (vm == currentMajorVersion && vn == currentMinorVersion) -- should we allow minor version deviations?
>                    req <- read (n - 8)
>                    ps <- makeParameters (Lazy.split 0 req)
>                    return (StartupMessage vm vn ps)
>  where
>   currentVersion = fromIntegral currentMajorVersion * 65536 + fromIntegral currentMinorVersion
>   getHeader = liftM2 (,) getWord32be getWord32be
>   getCancelRequest = liftM2 CancelRequest getWord32be getWord32be
>   makeParameters (x:y:xs) | not (Lazy.null x) = do
>     ps <- makeParameters xs
>     return ((Lazy.toStrict x, Lazy.toStrict y) : ps)
>   makeParameters _ = mzero

> readFrontendMessage :: MonadPlus m => (Int32 -> m Lazy.ByteString) -> m FrontendMessage
> readFrontendMessage read = do
>   (t, n) <- read 5 >>= run getHeader
>   case t of
>     'B' -> guard (n > 4) >> read (n - 4) >>= run getBind
>     'C' -> guard (n > 4) >> read (n - 4) >>= run getClose
>     'd' -> guard (n > 4) >> read (n - 4) >>= run getCopyData
>     'c' -> guard (n > 4) >> read (n - 4) >>= run getCopyDone
>     'f' -> guard (n > 4) >> read (n - 4) >>= run getCopyFail
>     'D' -> guard (n > 4) >> read (n - 4) >>= run getDescribe
>     'E' -> guard (n > 4) >> read (n - 4) >>= run getExecute
>  where
>   getHeader = liftM2 (,) (chr <$> getWord8) getWord32be

> readBackendMessage :: MonadPlus m => (Int32 -> m Lazy.ByteString) -> m BackendMessage
> readBackendMessage = error "TODO"

> run :: MonadPlus m => Get a -> Lazy.ByteString -> m a
> run g inp = case runGetLazyState g inp of
>               Right (x, inp') | Lazy.null inp' -> return x
>               _ -> mzero
