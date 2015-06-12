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
>   (n, v) <- readChunk 8 >>= run (liftM2 (,) (fromIntegral <$> getWord32be) getWord32be)
>   case v of
>     80877102 -> do require (n == 16) "invalid CancelRequest message length"
>                    req <- readChunk 8
>                    run (liftM2 CancelRequest getWord32be getWord32be) req
>     80877103 -> do require (n == 8) "invalid SSLRequest message length"
>                    return SSLRequest
>     _        -> do require (n >= 8) "invalid StartupMessage message length"
>                    let (vm', vn') = v `divMod` 65536
>                        vm = fromIntegral vm'
>                        vn = fromIntegral vn'
>                    require (vm == currentMajorVersion && vn == currentMinorVersion) "invalid protocol version in StartupMessage"
>                    req <- readChunk (n - 8)
>                    require (not (Lazy.null req) && Lazy.last req == 0) "missing session parameter list terminator"
>                    let req' = Lazy.init req
>                    ps <- if Lazy.null req' then return [] else makeParameters (Lazy.split 0 req')
>                    return (StartupMessage vm vn ps)
>  where
>   makeParameters (x:y:xs) = do
>     require (not (Lazy.null x)) "null session parameter name"
>     ps <- makeParameters xs
>     return ((Lazy.toStrict x, Lazy.toStrict y) : ps)
>   makeParameters [x] = do
>     require (Lazy.null x) "incomplete session parameter list"
>     return []
>   makeParameters [] = reject "missing session parameter list terminator"

> readFrontendMessage :: MonadPlus m => (Int32 -> m Lazy.ByteString) -> m FrontendMessage
> readFrontendMessage readChunk = do
>   (t, n) <- readChunk 5 >>= run getHeader
>   let n' = n - 4
>   case t of
>     'B' -> guard (n >  4) >> readChunk n' >>= run getBind
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
> readBackendMessage readChunk = do
>   (t, n) <- readChunk 5 >>= run getHeader
>   case t of
>     'R' -> readMessage n $ readChunk >=> AuthenticationResponse <$> run (getAuthenticationResponse n')
>     'K' -> readMessage n $ readChunk >=> run (liftM2 BackendKeyData getWord32be getWord32be)
>     '2' -> makeTrivialMessage n BindComplete
>     '3' -> makeTrivialMessage n CloseComplete
>     'C' -> readMessage n $ readChunk >=> CommandComplete <$> makeLazyByteStringZ
>     'd' -> readMessage n $ readChunk >=> CopyOutData <$> pure
>     'c' -> makeTrivialMessage n CopyOutDone
>     'G' -> readMessage n $ readChunk >=> run (liftM2 CopyInResponse getWord8 (getArray getWord16be))
>     'H' -> readMessage n $ readChunk >=> run (liftM2 CopyOutResponse getWord8 (getArray getWord16be))
>     'W' -> readMessage n $ readChunk >=> run (liftM2 CopyBothResponse getWord8 (getArray getWord16be))
>     'D' -> readMessage n $ readChunk >=> run (DataRow <$> getArray getValue)
>     'I' -> makeTrivialMessage n EmptyQueryResponse
>     'E' -> readMessage n $ readChunk >=> ErrorResponse <$> makeNoticeFields
>     'V' -> readMessage n $ readChunk >=> run (FunctionCallResponse <$> getValue)
>     'n' -> makeTrivialMessage n NoData
>     'N' -> readMessage n $ readChunk >=> NoticeResponse <$> makeNoticeFields
>     'A' -> readMessage n $ readChunk >=> run (liftM3 NotificationResponse getWord32be getByteStringZ getLazyByteStringZ) -- TODO: faster trailing string fetch
>     't' -> readMessage n $ readChunk >=> run (ParameterDescription <$> getArray getWord32be)
>     'S' -> readMessage n $ readChunk >=> run (liftM2 ParameterStatus getByteStringZ getLazyByteStringZ) -- TODO: faster trailing string fetch
>     '1' -> makeTrivialMessage n ParseComplete
>     's' -> makeTrivialMessage n PortalSuspended
>     'Z' -> readMessage n $ readChunk >=> run (ReadyForQuery <$> getWord8)
>     'T' -> readMessage n $ readChunk >=> run (RowDescription <$> getArray getFieldDescription)
>     _   -> reject "unrecognized backend message received"

  "require p msg" is the same as "guard p", but labeled with an informative message for documentation and possible future debugging purposes:

> require :: MonadPlus m => Bool -> String -> m ()
> require p _ = guard p

  "reject msg" is the same as "mzero", but labeled with an informative message for documentation and possible future debugging purposes:

> reject :: MonadPlus m => String -> m a
> reject _ = mzero

  Read a unary message of a total length @n@ using the monadic action @r@, accounting for
  the PostgreSQL inclusion of the length field in the total length value:

> readMessage :: MonadPlus m => Int32 -> (Int32 -> m a) -> m a
> readMessage n rk = require (n >= 4) "message too short" >> r (n - 4)

  Prepare a parameterless message of length @n@ with the Haskell constructor $k$.
  Because the message accepts no arguments, @n@ must be equal to 4:

> makeTrivialMessage :: MonadPlus m -> Int32 -> a -> m a
> makeTrivialMessage n k = require (n == 4) "invalid nullary message length" >> return k

  Process a newly-received PostgreSQL string value by stripping its NUL terminator
  and checking for any embedded NUL characters:

> makeLazyByteStringZ :: MonadPlus m => Lazy.ByteString -> m Lazy.ByteString
> makeLazyByteStringZ s = do
>   require (not (Lazy.null s) && Lazy.last s == 0) "invalid string received: missing NUL terminator byte"
>   let s' = Lazy.init s
>   require (Lazy.notElem 0 s') "invalid string received: embedded NUL bytes found"
>   return s'

  Process a newly-received PostgreSQL notice:

> makeNoticeFields :: MonadPlus m => Lazy.ByteString -> m NoticeFields
> makeNoticeFields s = do
>   require (not (Lazy.null s) && Lazy.last s == 0) "invalid notice received: missing NUL terminator byte"
>   let s' = Lazy.init s -- strip the NUL terminator of the entire message
>   if Lazy.null s' then
>     return []
>   else do
>     require (Lazy.last s' == 0) "invalid notice field received: missing NUL terminator byte"
>     let s'' = Lazy.init s' -- strip the NUL terminator of the final field
>     let ss = if Lazy.null s'' then [Nothing] else map Lazy.uncons (Lazy.split 0 s'')
>     mapM (maybe (reject "invalid notice field received: missing tag byte") (\(t, m) -> (t, Lazy.toStrict m))) ss

  Run a "Get" value inside an arbitrary MonadPlus, ensuring that the entire input string
  is consumed by the getter and discarding any internally-generated error messages:

> run :: MonadPlus m => Get a -> Lazy.ByteString -> m a
> run g inp = case runGetLazyState g inp of
>               Right (x, inp') | Lazy.null inp' -> return x
>               _ -> reject "corrupted message"

  Receive a message header, consisting of a tag byte that will be presented as a character value,
  and a 32-bit signed integer which represents the message length.

> getHeader :: Get (Char, Int32)
> getHeader = liftM2 (,) (chr <$> fromIntegral <$> getWord8) (fromIntegral <$> getWord32be)

> getAuthenticationResponse ::  Int32 -> Get AuthenticationResponse
> getAuthenticationResponse n = do
>   t <- getWord32be
>   case t of
>     0 -> return AuthenticationOK
>     2 -> return AuthenticationKerberosV5
>     3 -> return AuthenticationCleartextPassword
>     5 -> AuthenticationMD5Password <$> getWord32be
>     6 -> return AuthenticationSCMCredential
>     7 -> return AuthenticationGSS
>     8 -> AuthenticationGSSContinue <$> getLazyByteString (fromIntegral (n - 4))
>     9 -> return AuthenticationSSPI
>     _ -> AuthenticationMiscellaneous t <$> getLazyByteString (fromIntegral (n - 4))

> getArray :: IArray a e => Get e -> Get (a Int16 e)
> getArray getElem = do
>   n <- fromIntegral <$> getWord16be
>   require (n >= 0) "negative array size"
>   listArray (0, n - 1) <$> replicateM (fromIntegral n) getElem

> getValue :: Get Value
> getValue = do
>   n <- getWord32be
>   if (n <= 0x7FFFFFFF) then do
>     Just <$> getLazyByteString (fromIntegral n)
>   else do
>     require (n == 0xFFFFFFFF) "negative value size"
>     return Nothing

> getByteStringZ :: Get ByteString
> getByteStringZ = Lazy.toStrict <$> getVariableLazyByteStringZ

> getLazyByteStringZ :: Get Lazy.ByteString
> getLazyByteStringZ = loop mempty
>  where
>   loop s = do
>     c <- getWord8
>     case c of
>       0 -> return (toLazyByteString s)
>       _ -> loop (s <> word8 c)

