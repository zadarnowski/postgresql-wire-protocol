-- | Module:    Database.PostgreSQL.Protocol.Builders.Messages
-- Description: Builders for PostgreSQL protocol messages
-- Copyright:   © 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  pat@jantar.org
-- Stability:   experimental
-- Portability: portable
--
-- This module defines low-level builders for all PostgreSQL protocol messages.
-- At this level of abstraction, all message fields are rendered directly as
-- binary data, with little or no marshalling into any more meaningful Haskell
-- types, in order to provided higher-level libraries with maximum possible
-- freedom of behaviour.

module Database.PostgreSQL.Protocol.Builders where

import Database.PostgreSQL.Protocol.Internal.Builders
import Database.PostgreSQL.Protocol.Types
import Database.PostgreSQL.Protocol.Version

import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Int
import Data.Word

-- | Builder for an 'AuthenticationOk' message.
authenticationOk :: Builder
authenticationOk = compact $ toBuilder $ withHeader 'R' $
  int32 0
{-# NOINLINE authenticationOk #-}

-- | Builder for an 'AuthenticationKerberosV5' message.
authenticationKerberosV5 :: Builder
authenticationKerberosV5 = compact $ toBuilder $ withHeader 'R' $
  int32 2
{-# NOINLINE authenticationKerberosV5 #-}

-- | Builder for an 'AuthenticationCleartextPassword' message.
authenticationCleartextPassword :: Builder
authenticationCleartextPassword = compact $ toBuilder $ withHeader 'R' $
  int32 3
{-# NOINLINE authenticationCleartextPassword #-}

-- | Builder for an @'AuthenticationMD5Password' s@ message with the salt @s@.
authenticationMD5Password :: Word32 -> Builder
authenticationMD5Password s = toBuilder $ withHeader 'R' $
  int32 5 <> num32 s

-- | Builder for an 'AuthenticationSCMCredential' message.
authenticationSCMCredential :: Builder
authenticationSCMCredential = compact $ toBuilder $ withHeader 'R' $
  int32 6
{-# NOINLINE authenticationSCMCredential #-}

-- | Builder for an 'AuthenticationGSS' message.
authenticationGSS :: Builder
authenticationGSS = compact $ toBuilder $ withHeader 'R' $
  int32 7
{-# NOINLINE authenticationGSS #-}

-- | Builder for an 'AuthenticationSSPI' message.
authenticationSSPI :: Builder
authenticationSSPI = compact $ toBuilder $ withHeader 'R' $
  int32 9
{-# NOINLINE authenticationSSPI #-}

-- | Builder for an @'AuthenticationGSSContinue' s@ with
--   the GSSAPI or SSPI authentication data @s@.
authenticationGSSContinue :: LazyByteString -> Builder
authenticationGSSContinue s = toBuilder $ withHeader 'R' $
  int32 8 <> lazyByteString s

-- | Builder for an @'AuthenticationSASL' ms@ message with
--   the list of supported SASL authentication mechanisms @ms@,
--   specified in the server's order of preference.
authenticationSASL :: Foldable t => t ByteString -> Builder
authenticationSASL ms = toBuilder $ withHeader 'R' $
  int32 10 <> foldMap byteStringZ ms <> int8 0

-- | Builder for an @'AuthenticationSASLContinue' s@ with
--   SASL data @s@ specific to the SASL mechanism being used.
authenticationSASLContinue :: LazyByteString -> Builder
authenticationSASLContinue s = toBuilder $ withHeader 'R' $
  int32 11 <> lazyByteString s

-- | Builder for an @'AuthenticationSASLFinal' s@ message with
--   SASL outcome /additional data/ @s@, specific to the SASL
--   mechanism being used.
authenticationSASLFinal :: LazyByteString -> Builder
authenticationSASLFinal s = toBuilder $ withHeader 'R' $
  int32 12 <> lazyByteString s

-- | Builder for a @'BackendKeyData' pid s@ with the server process
--   ID @pid@ and secret key @s@.
backendKeyData :: ProcessID -> Word32 -> Builder
backendKeyData pid s = toBuilder $ withHeader 'K' $
  num32 pid <> num32 s

-- | Builder for a @'Bind' p s pfs pvs rfs@ message that binds the /portal/ @p@
--   to a prepared statement @s@, with parameter formats @pfs@, parameter values
--   @pvs@ and result formats @rfs@. Both @p@ and @s@ can be 'ByteString.empty'
--   to request the default /unnamed/ portal and/or statement, respectively.
--   The list of parameter values @pvs@ must match the actual number of
--   parameter placeholders in the prepared statement @s@, while the format
--   lists @pfs@ and @rfs@ can either match the number of parameters or results
--   in @s@, specify a single format for all parameters or results, or be
--   empty to request the default text format.
bind :: (Foldable t1, Foldable t2, Foldable t3) =>
  Name -> Name -> t1 Format -> t2 (Maybe LazyByteString) -> t3 Format -> Builder
bind p s pfs pvs rfs = toBuilder $ withHeader 'B' $
  byteStringZ p <> byteStringZ s <>
  foldMapL16 num16 pfs <>
  foldMapL16 maybeLazyByteString pvs <>
  foldMapL16 num16 rfs

-- | Builder for a 'BindComplete' message.
bindComplete :: Builder
bindComplete = compact $ toBuilder $ withHeader '2' $
  mempty
{-# NOINLINE bindComplete #-}

-- | Builder for a @'CancelRequest' pid s@ message for the
--   server process ID @pid@ with the secret key @s@.
cancelRequest :: ProcessID -> Word32 -> Builder
cancelRequest pid s = toBuilder $ withSizeHeader $
  int32 80877102 <> num32 pid <> num32 s

-- | Builder for a @'Close' k x@ message requesting closure of
--   the named protocol object @x@ of type @k@ ('Statement' or 'Portal').
close :: SessionObjectType -> Name -> Builder
close k x = toBuilder $ withHeader 'C' $
  num8 k <> byteStringZ x

-- | Builder for a 'CloseComplete' message.
closeComplete :: Builder
closeComplete = compact $ toBuilder $ withHeader '3' $
  mempty
{-# NOINLINE closeComplete #-}

-- | Builder for a @'CommandComplete' t@ message with command tag @t@.
commandComplete :: LazyByteString -> Builder
commandComplete t = toBuilder $ withHeader 'C' $
  lazyByteStringZ t

-- | Builder for a @'CopyInData' s@ or @'CopyOutData' s@ message with data payload @s@.
copyData :: LazyByteString -> Builder
copyData s = toBuilder $ withHeader 'd' $
  lazyByteString s

-- | Builder for a 'CopyInDone' or 'CopyOutDone' message.
copyDone :: Builder
copyDone = compact $ toBuilder $ withHeader 'c' $
  mempty
{-# NOINLINE copyDone #-}

-- | Builder for a @'CopyInFail' s@ message with an error description @s@.
copyFail :: LazyByteString -> Builder
copyFail s = toBuilder $ withHeader 'f' $
  lazyByteStringZ s

-- | Builder for a @'CopyInResponse' f cfs@ message with the overall format @f@
--   and individual column formats @cfs@.
copyInResponse :: Foldable t => Format -> t Format -> Builder
copyInResponse f cfs = toBuilder $ withHeader 'G' $
  num8 f <> foldMapL16 num16 cfs

-- | Builder for a @'CopyOutResponse' f cfs@ message with the overall format @f@
--   and individual column formats @cfs@.
copyOutResponse :: Foldable t => Format -> t Format -> Builder
copyOutResponse f cfs = toBuilder $ withHeader 'H' $
  num8 f <> foldMapL16 num16 cfs

-- | Builder for a @'CopyBothResponse' f cfs@ message with the overall format @f@
--   and individual column formats @cfs@.
copyBothResponse :: Foldable t => Format -> t Format -> Builder
copyBothResponse f cfs = toBuilder $ withHeader 'W' $
  num8 f <> foldMapL16 num16 cfs

-- | Builder for a @'DataRow' cvs@ message with a list of column values @cvs@.
dataRow :: Foldable t => t (Maybe LazyByteString) -> Builder
dataRow cvs = toBuilder $ withHeader 'D' $
  foldMapL16 maybeLazyByteString cvs

-- | Builder for a @'Describe' k x@ message requesting description of
--   the named protocol object @x@ of type @k@ ('Statement' or 'Portal').
describe :: SessionObjectType -> Name -> Builder
describe k x = toBuilder $ withHeader 'D' $
  num8 k <> byteStringZ x

-- | Builder for a 'EmptyQueryResponse' message.
emptyQueryResponse :: Builder
emptyQueryResponse = compact $ toBuilder $ withHeader 'I' $
  mempty
{-# NOINLINE emptyQueryResponse #-}

-- | Builder for a @'ErrorResponse' fs@ message with notice fields @fs@.
errorResponse :: Foldable t => t (NoticeFieldTag, LazyByteString) -> Builder
errorResponse fs = toBuilder $ withHeader 'E' $
  foldMap (uncurry (<>) . bimap num8 lazyByteStringZ) fs <> char8 '\0'

-- | Builder for a @'Execute' p n@ message with portal name @p@ and
--   maximum number of rows to return @n@.
execute :: Name -> Int32 -> Builder
execute p n = toBuilder $ withHeader 'F' $
  byteStringZ p <> num32 n

-- | Builder for a 'Flush' message.
flush :: Builder
flush = compact $ toBuilder $ withHeader 'H' $
  mempty
{-# NOINLINE flush #-}

-- | Builder for a @'FunctionCall' oid afs avs rf@ message representing
--   a call to the stored procedure with object ID @oid@ with argument
--   values @avs@. The format of arguments and result is specified by
--   the format codes @afs@ and @rf@, respectively. Supplying a singleton
--   list for @afs@ indicates that all arguments are supplied in the same
--   format; supplying an empty list indicates that all arguments are
--   supplied in the default text format.
functionCall :: (Foldable t1, Foldable t2) =>
  ObjectID -> t1 Format -> t2 (Maybe LazyByteString) -> Format -> Builder
functionCall oid afs avs rf = toBuilder $ withHeader 'F' $
  num32 oid <>
  foldMapL16 num16 afs <>
  foldMapL16 maybeLazyByteString avs <>
  num16 rf

-- | Builder for a @'FunctionCallResponse' rv@ message with result value @rv@.
functionCallResponse :: Maybe LazyByteString -> Builder
functionCallResponse rv = toBuilder $ withHeader 'V' $
  maybeLazyByteString rv

-- | Builder for a @'GSSResponse' s@ message with GSSAPI/SSPI data @s@.
gssResponse :: LazyByteString -> Builder
gssResponse s = toBuilder $ withHeader 'p' $
  lazyByteString s

-- | Builder for a @'NegotiateProtocolVersion' v os@ message with minor protocl
--   version @v@ and the list of unrecognized protocol options @os@.
negotiateProtocolVersion :: Foldable t => Word32 -> t ByteString -> Builder
negotiateProtocolVersion v os = toBuilder $ withHeader 'v' $
  num32 v <> foldMapL32 byteStringZ os

-- | Builder for a 'NoData' message.
noData :: Builder
noData = compact $ toBuilder $ withHeader 'n' $
  mempty
{-# NOINLINE noData #-}

-- | Builder for a @'NoticeResponse' fs@ message with notice fields @fs@.
noticeResponse :: Foldable t => t (NoticeFieldTag, LazyByteString) -> Builder
noticeResponse fs = toBuilder $ withHeader 'N' $
  foldMap (uncurry (<>) . bimap num8 lazyByteStringZ) fs <> char8 '\0'

-- | Builder for a @'NotificationResponse' pid c s@ message from the server
--   process @pid@, representing a notification on channel @c@ with payload @s@.
notificationResponse :: ProcessID -> Name -> LazyByteString -> Builder
notificationResponse pid c s = toBuilder $ withHeader 'A' $
  num32 pid <> byteStringZ c <> lazyByteStringZ s

-- | Builder for a @'ParameterDescription' ts@ message for
--   the list of parameter type object IDs @ts@.
parameterDescription :: Foldable t => t ObjectID -> Builder
parameterDescription ts = toBuilder $ withHeader 't' $
  foldMapL16 num32 ts

-- | Builder for a @'ParameterStatus' p v@ message for
--   a run-time parameter with name @p@ and value @v@.
parameterStatus :: Name -> LazyByteString -> Builder
parameterStatus p v = toBuilder $ withHeader 'S' $
  byteStringZ p <> lazyByteStringZ v
{-# NOINLINE parameterStatus #-}

-- | Builder for a @'Parse' s q ts@ message for a prepared statement
--   with name @s@, query string @q@ and list of parameter data types @ts@.
parse :: Foldable t => Name -> LazyByteString -> t ObjectID -> Builder
parse s q ts = toBuilder $ withHeader 'P' $
  byteStringZ s <> lazyByteStringZ q <>
  foldMapL16 num32 ts

-- | Builder for a 'ParseComplete' message.
parseComplete :: Builder
parseComplete = compact $ toBuilder $ withHeader '1' $
  mempty
{-# NOINLINE parseComplete #-}

-- | Builder for a @'PasswordMessage' s@ message with the password string @s@.
passwordMessage :: LazyByteString -> Builder
passwordMessage s = toBuilder $ withHeader 'p' $
  lazyByteStringZ s

-- | Builder for a 'PortalSuspended' message.
portalSuspended :: Builder
portalSuspended = compact $ toBuilder $ withHeader 's' $
  mempty
{-# NOINLINE portalSuspended #-}

-- | Builder for a @'Query' q@ message with the query string @q@.
query :: LazyByteString -> Builder
query q = toBuilder $ withHeader 'Q' $
  lazyByteStringZ q

-- | Builder for a @'ReadyForQuery' s@ message with
--   transaction status indicator @s@.
readyForQuery :: TransactionStatus -> Builder
readyForQuery s = toBuilder $ withHeader 'Z' $
  num8 s

-- | Builder for a @'RowDescription' fs@ message with field list @fs@.
rowDescription :: Foldable t => t FieldDescription -> Builder
rowDescription rs = toBuilder $ withHeader 'T' $
  foldMapL16 fieldDescription rs
 where
  fieldDescription f =
    byteStringZ (fieldDescription'Name f) <>
    num32 (fieldDescription'Table f) <>
    num16 (fieldDescription'Column f) <>
    num32 (fieldDescription'Type f) <>
    num16 (fieldDescription'TypeSize f) <>
    num32 (fieldDescription'TypeModifier f) <>
    num16 (fieldDescription'Format f)

-- | Builder for a @'SASLInitialResponse' m r@ message for the
--   SASL authentication mechanism @m@ with initial client response $r$.
saslInitialResponse :: Name -> Maybe LazyByteString -> Builder
saslInitialResponse m r = toBuilder $ withHeader 'p' $
  byteStringZ m <> maybeLazyByteString r

-- | Builder for a @'SASLResponse' r@ message with
--   SASL mechanism-specific data @r@.
saslResponse :: LazyByteString -> Builder
saslResponse r = toBuilder $ withHeader 'p' $
  lazyByteString r

-- | Builder for a 'SSLRequest' message.
sslRequest :: Builder
sslRequest = compact $ toBuilder $ withSizeHeader $
  int32 80877103
{-# NOINLINE sslRequest #-}

-- | Builder for a @'StartupMessage' ps@ message with the current protocol
--   version specified by 'currentMajorVersion' and 'currentMinorVersion',
--   and the list of connection parameters @ps@.
startupMessage :: Foldable t => t (Name, LazyByteString) -> Builder
startupMessage = startupMessage' currentMajorVersion currentMinorVersion

-- | Builder for a @'StartupMessage' m n ps@ message with
--   the specified major version @m@, minor version @n@
--   and the list of connection parameters @ps@.
startupMessage' :: Foldable t => Word16 -> Word16 -> t (Name, LazyByteString) -> Builder
startupMessage' m n ps = toBuilder $ withSizeHeader $
  num16 m <> num16 n <>
  foldMap (uncurry (<>) . bimap byteStringZ lazyByteStringZ) ps <>
  char8 '\0'

-- | Builder for a 'Sync' message.
sync :: Builder
sync = compact $ toBuilder $ withHeader 'F' $
  mempty
{-# NOINLINE sync #-}

-- | Builder for a 'Terminate' message.
terminate :: Builder
terminate = compact $ toBuilder $ withHeader 'X' $
  mempty
{-# NOINLINE terminate #-}
