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

> import Control.Monad
> import Data.Array.IArray
> import Data.ByteString (ByteString)
> import Data.ByteString.Builder
> import Data.Char
> import Data.Int
> import Data.Monoid
> import Data.Serialize.Get

> import Database.PostgreSQL.Protocol.Types

> import qualified Data.ByteString.Lazy as Lazy

> readSessionMessage :: MonadPlus m => (Int32 -> m Lazy.ByteString) -> m SessionMessage
> readSessionMessage readChunk = do
>   (n, v) <- readChunk 8 >>= run getHeader
>   case v of
>     80877102 -> do guard (n == 16)
>                    req <- readChunk 8
>                    run getCancelRequest req
>     80877103 -> do guard (n == 8)
>                    return SSLRequest
>     _        -> do guard (8 <= n && n <= 0x7FFFFFFF)
>                    let (vm', vn') = v `divMod` 65536
>                        vm = fromIntegral vm'
>                        vn = fromIntegral vn'
>                    guard (vm == currentMajorVersion && vn == currentMinorVersion) -- should we allow minor version deviations?
>                    req <- readChunk (fromIntegral n - 8)
>                    ps <- makeParameters (Lazy.split 0 req)
>                    return (StartupMessage vm vn ps)
>  where
>   getHeader = liftM2 (,) getWord32be getWord32be
>   getCancelRequest = liftM2 CancelRequest getWord32be getWord32be
>   makeParameters (x:y:xs) | not (Lazy.null x) = do
>     ps <- makeParameters xs
>     return ((Lazy.toStrict x, Lazy.toStrict y) : ps)
>   makeParameters _ = mzero

> readFrontendMessage :: MonadPlus m => (Int32 -> m Lazy.ByteString) -> m FrontendMessage
> readFrontendMessage readChunk = do
>   (t, n) <- readChunk 5 >>= run getHeader
>   let n' = fromIntegral n - 4
>   case t of
>     'B' -> guard (4 < n && n <= 0x7FFFFFFF) >> readChunk n' >>= run getBind
>     'C' -> guard (n >  4) >> readChunk n' >>= run (getClose n')
>     'd' -> guard (n >  4) >> readChunk n' >>= run (getCopyData n')
>     'c' -> guard (n == 4) >> return CopyInDone
>     'f' -> guard (n >  4) >> readChunk n' >>= run (getCopyFail n')
>     'D' -> guard (n >  4) >> readChunk n' >>= run (getDescribe n')
>     'E' -> guard (n >  4) >> readChunk n' >>= run (getExecute n')
>     'H' -> guard (n == 4) >> return Flush
>     'F' -> guard (n >  4) >> readChunk n' >>= run getFunctionCall
>     'P' -> guard (n >  4) >> readChunk n' >>= run getParse
>     'p' -> guard (n >  4) >> readChunk n' >>= run (getPasswordMessage n)
>     'Q' -> guard (n >  4) >> readChunk n' >>= run (getQuery n)
>     'S' -> guard (n == 4) >> return Sync
>     'X' -> guard (n == 4) >> return Terminate
>     _   -> mzero
>  where
>   getHeader            = liftM2 (,) (chr <$> fromIntegral <$> getWord8) (getWord32be)
>   getBind              = liftM5 Bind (getByteStringZ) (getByteStringZ) (getArray getWord16be) (getArray getValue) (getArray getWord16be)
>   getClose n           = liftM2 Close (getWord8) (getByteString (fromIntegral (n - 1)))
>   getCopyData n        = liftM  CopyInData (getLazyByteString (fromIntegral n))
>   getCopyFail n        = liftM  CopyFail (getByteString (fromIntegral n))
>   getDescribe n        = liftM2 Describe (getWord8) (getByteString (fromIntegral (n - 1)))
>   getExecute n         = liftM2 Execute (getByteString (fromIntegral (n - 4))) (fromIntegral <$> getWord32be)
>   getFunctionCall      = liftM4 FunctionCall (getWord32be) (getArray getWord16be) (getArray getValue) (getWord16be)
>   getParse             = liftM3 Parse (getByteStringZ) (getLazyByteStringZ) (getArray getWord32be)
>   getPasswordMessage n = liftM  PasswordMessage (getByteString (fromIntegral n))
>   getQuery n           = liftM  Query (getLazyByteString (fromIntegral n))

> readBackendMessage :: MonadPlus m => (Int32 -> m Lazy.ByteString) -> m BackendMessage
> readBackendMessage = error "TODO"

> run :: MonadPlus m => Get a -> Lazy.ByteString -> m a
> run g inp = case runGetLazyState g inp of
>               Right (x, inp') | Lazy.null inp' -> return x
>               _ -> mzero

> getByteStringZ :: Get ByteString
> getByteStringZ = Lazy.toStrict <$> getLazyByteStringZ

> getLazyByteStringZ :: Get Lazy.ByteString
> getLazyByteStringZ = loop mempty
>  where
>   loop s = do
>     c <- getWord8
>     case c of
>       0 -> return (toLazyByteString s)
>       _ -> loop (s <> word8 c)

> getValue :: Get Value
> getValue = do
>   n <- getWord32be
>   if (n <= 0x7FFFFFFF) then do
>     Just <$> getLazyByteString (fromIntegral n)
>   else do
>     guard (n == 0xFFFFFFFF)
>     return Nothing

> getArray :: IArray a e => Get e -> Get (a Int16 e)
> getArray getElem = do
>   n <- fromIntegral <$> getWord16be
>   guard (n >= 0)
>   listArray (0, n - 1) <$> replicateM (fromIntegral n) getElem
