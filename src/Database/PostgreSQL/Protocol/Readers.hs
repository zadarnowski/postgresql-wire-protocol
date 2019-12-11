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
> import Data.Bits
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
>                    readChunk 8 >>= run (liftM2 CancelRequest getWord32be getWord32be)
>     80877103 -> do require (n == 8) "invalid SSLRequest message length"
>                    return SSLRequest
>     _        -> do require (n >= 8) "invalid StartupMessage message length"
>                    let vm = fromIntegral (v `shiftR` 16)
>                        vn = fromIntegral (v .&. 0xFFFF)
>                    require (vm == currentMajorVersion && vn == currentMinorVersion) "invalid protocol version in StartupMessage"
>                    req <- readChunk (n - 8)
>                    StartupMessage vm vn <$> makeParameters (Lazy.split 0 req)
>  where
>   makeParameters (pn:pv:ps')
>     | Lazy.null pn = require (Lazy.null pv && null ps') "missing session parameter list terminator" >> return []
>     | otherwise    = liftM2 (:) (pure (Lazy.toStrict pn, pv)) (makeParameters ps')
>   makeParameters _ = reject "missing session parameter list terminator"

> readFrontendMessage :: MonadPlus m => (Int32 -> m Lazy.ByteString) -> m FrontendMessage
> readFrontendMessage readChunk = do
>   (t, n) <- readChunk 5 >>= run getHeader
>   case t of
>     'B' -> readMessage n $ readChunk >=> run (liftM5 Bind getByteStringZ getByteStringZ (getArray getWord16be) (getArray getValue) (getArray getWord16be))
>     'C' -> readMessage n $ readChunk >=> run (liftM2 Close getWord8 getRemainingByteStringZ)
>     'd' -> readMessage n $ readChunk >=> return . CopyInData
>     'c' -> makeTrivialMessage n CopyInDone
>     'f' -> readMessage n $ readChunk >=> makeLazyByteStringZ >=> return . CopyFail
>     'D' -> readMessage n $ readChunk >=> run (liftM2 Describe getWord8 getRemainingByteStringZ)
>     'E' -> readMessage n $ readChunk >=> run (liftM2 Execute getByteStringZ (fromIntegral <$> getWord32be))
>     'H' -> makeTrivialMessage n Flush
>     'F' -> readMessage n $ readChunk >=> run (liftM4 FunctionCall getWord32be (getArray getWord16be) (getArray getValue) getWord16be)
>     'P' -> readMessage n $ readChunk >=> run (liftM3 Parse getByteStringZ getLazyByteStringZ (getArray getWord32be))
>     'p' -> readMessage n $ readChunk >=> makeLazyByteStringZ >=> return . PasswordMessage
>     'Q' -> readMessage n $ readChunk >=> makeLazyByteStringZ >=> return . Query
>     'S' -> makeTrivialMessage n Sync
>     'X' -> makeTrivialMessage n Terminate
>     _   -> reject "unrecognized message"

> readBackendMessage :: MonadPlus m => (Int32 -> m Lazy.ByteString) -> m BackendMessage
> readBackendMessage readChunk = do
>   (t, n) <- readChunk 5 >>= run getHeader
>   case t of
>     'R' -> readMessage n $ readChunk >=> run getAuthenticationResponse >=> pure . AuthenticationResponse
>     'K' -> readMessage n $ readChunk >=> run (liftM2 BackendKeyData getWord32be getWord32be)
>     '2' -> makeTrivialMessage n BindComplete
>     '3' -> makeTrivialMessage n CloseComplete
>     'C' -> readMessage n $ readChunk >=> makeLazyByteStringZ >=> return . CommandComplete
>     'd' -> readMessage n $ readChunk >=> return . CopyOutData
>     'c' -> makeTrivialMessage n CopyOutDone
>     'G' -> readMessage n $ readChunk >=> run (liftM2 CopyInResponse getWord8 (getArray getWord16be))
>     'H' -> readMessage n $ readChunk >=> run (liftM2 CopyOutResponse getWord8 (getArray getWord16be))
>     'W' -> readMessage n $ readChunk >=> run (liftM2 CopyBothResponse getWord8 (getArray getWord16be))
>     'D' -> readMessage n $ readChunk >=> run (liftM  DataRow (getArray getValue))
>     'I' -> makeTrivialMessage n EmptyQueryResponse
>     'E' -> readMessage n $ readChunk >=> makeNoticeFields >=> return . ErrorResponse
>     'V' -> readMessage n $ readChunk >=> run (liftM  FunctionCallResponse getValue)
>     'n' -> makeTrivialMessage n NoData
>     'N' -> readMessage n $ readChunk >=> makeNoticeFields >=> return . NoticeResponse
>     'A' -> readMessage n $ readChunk >=> run (liftM3 NotificationResponse getWord32be getByteStringZ getRemainingLazyByteStringZ)
>     't' -> readMessage n $ readChunk >=> run (liftM  ParameterDescription (getArray getWord32be))
>     'S' -> readMessage n $ readChunk >=> run (liftM2 ParameterStatus getByteStringZ getRemainingLazyByteStringZ)
>     '1' -> makeTrivialMessage n ParseComplete
>     's' -> makeTrivialMessage n PortalSuspended
>     'Z' -> readMessage n $ readChunk >=> run (liftM  ReadyForQuery getWord8)
>     'T' -> readMessage n $ readChunk >=> run (liftM  RowDescription (getArray getFieldDescription))
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
> readMessage n rk = require (n >= 4) "message too short" >> rk (n - 4)

  Prepare a parameterless message of length @n@ with the Haskell constructor $k$.
  Because the message accepts no arguments, @n@ must be equal to 4:

> makeTrivialMessage :: MonadPlus m => Int32 -> a -> m a
> makeTrivialMessage n k = require (n == 4) "invalid nullary message length" >> return k

  Process a newly-received PostgreSQL string value by stripping its NUL terminator
  and checking for any embedded NUL characters:

> makeLazyByteStringZ :: MonadPlus m => Lazy.ByteString -> m Lazy.ByteString
> makeLazyByteStringZ s = do
>   let ss = Lazy.split 0 s
>   require (not (Lazy.null s) && Lazy.last s == 0) "invalid string received: missing NUL terminator byte"
>   let s' = Lazy.init s
>   require (Lazy.notElem 0 s') "invalid string received: embedded NUL bytes found"
>   return s'

  Process a newly-received PostgreSQL notice:

> makeNoticeFields :: MonadPlus m => Lazy.ByteString -> m NoticeFields
> makeNoticeFields = makeParameters . Lazy.split 0
>  where
>   makeParameters (s:ss') =
>     case Lazy.uncons s of
>       Just tp -> liftM2 (:) (pure tp) (makeParameters ss')
>       Nothing -> case ss' of
>                    (s':ss'') -> require (Lazy.null s' && null ss'') "invalid notice received: missing parameter tag byte" >> return []
>                    [] -> reject "invalid notice received: missing NUL terminator byte"
>   makeParameters [] = reject "invalid notice received: missing NUL terminator byte"

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

> getAuthenticationResponse :: Get AuthenticationResponse
> getAuthenticationResponse = do
>   t <- getWord32be
>   case t of
>     0 -> return AuthenticationOk
>     2 -> return AuthenticationKerberosV5
>     3 -> return AuthenticationCleartextPassword
>     5 -> AuthenticationMD5Password <$> getWord32be
>     6 -> return AuthenticationSCMCredential
>     7 -> return AuthenticationGSS
>     8 -> AuthenticationGSSContinue <$> getRemainingLazyByteString
>     9 -> return AuthenticationSSPI
>     _ -> AuthenticationMiscellaneous t <$> getRemainingLazyByteString

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
> getByteStringZ = Lazy.toStrict <$> getLazyByteStringZ

> getLazyByteStringZ :: Get Lazy.ByteString
> getLazyByteStringZ = loop mempty
>  where
>   loop s = do
>     c <- getWord8
>     case c of
>       0 -> return (toLazyByteString s)
>       _ -> loop (s <> word8 c)

> getRemainingByteString :: Get ByteString
> getRemainingByteString = Lazy.toStrict <$> getRemainingLazyByteString

> getRemainingByteStringZ :: Get ByteString
> getRemainingByteStringZ = Lazy.toStrict <$> getRemainingLazyByteStringZ

> getRemainingLazyByteString :: Get Lazy.ByteString
> getRemainingLazyByteString = fromIntegral <$> remaining >>= getLazyByteString

> getRemainingLazyByteStringZ :: Get Lazy.ByteString
> getRemainingLazyByteStringZ = getRemainingLazyByteString >>= makeLazyByteStringZ

> getFieldDescription :: Get FieldDescription
> getFieldDescription = do
>   name <- getByteStringZ
>   tableID <- getWord32be
>   columnID <- fromIntegral <$> getWord16be
>   dataTypeID <- getWord32be
>   dataTypeSize <- fromIntegral <$> getWord16be
>   dataTypeModifier <- getWord32be
>   format <- getWord16be
>   return FieldDescription {
>      fieldName = name,
>      fieldTableID = tableID,
>      fieldColumnID = columnID,
>      fieldDataTypeID = dataTypeID,
>      fieldDataTypeSize = dataTypeSize,
>      fieldDataTypeModifier = dataTypeModifier,
>      fieldFormat = format
>    }
