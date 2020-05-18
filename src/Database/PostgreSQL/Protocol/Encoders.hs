-- | Module:    Database.PostgreSQL.Protocol.Encoders
-- Description: Encoders for PostgreSQL messages.
-- Copyright:   © 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable
--
-- This module defines low-level encoders for serialising PostgreSQL messages
-- into lazy byte strings. At this level of abstraction, all message fields
-- are rendered directly as binary data, with little or no marshalling into
-- any more meaningful Haskell types, in order to provided higher-level
-- libraries with maximum possible freedom of behaviour.

{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Protocol.Encoders where

import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Int
import Data.Word

import Database.PostgreSQL.Protocol.Internal.Builders
import Database.PostgreSQL.Protocol.Internal.Utilities
import Database.PostgreSQL.Protocol.Tags
import Database.PostgreSQL.Protocol.Types

import qualified Data.ByteString.Lazy as LazyByteString

-- | Issued by the backend to signify successful authentication of the
-- frontend's credentials.
authenticationOk :: LazyByteString
authenticationOk = toCompactLazyByteString $ withAuthenticationResponseHeader 0 mempty
{-# NOINLINE authenticationOk #-}

-- | Issued by the backend to initiate Kerberos V5 authentication dialogue,
-- described separately in Kerberos specification. This authentication method
-- is no longer supported by recent versions of PostgreSQL software.
authenticationKerberosV5 :: LazyByteString
authenticationKerberosV5 = toCompactLazyByteString $ withAuthenticationResponseHeader 2 mempty
{-# NOINLINE authenticationKerberosV5 #-}

-- | Issued by the backend to request clear-text password authentication. The
-- frontend should respond with a 'PasswordMessage' containing an unencrypted
-- text of the user's password.
authenticationCleartextPassword :: LazyByteString
authenticationCleartextPassword = toCompactLazyByteString $ withAuthenticationResponseHeader 3 mempty
{-# NOINLINE authenticationCleartextPassword #-}

-- | A message of the form “@'AuthenticationMD5Password' s@” is issued by the
-- backend to request MD5-based password authentication with the specified
-- 32-bit /salt/ @s@.  The frontend should respond with
-- @'PasswordMessage' x@, in which @x@ is a byte string derived from the user's
-- login name @u@, password @p@ and the supplied salt @ss@ as follows:
--
-- @
--      "md5" <> md5 (md5 (/p/ <> /u/) <> /ss/)
-- @
--
-- where /s/ is a 4-byte byte string obtained from the big-endian encoding of
-- the supplied salt @s@, and @md5(x)@ is a function that returns a 32-byte
-- byte string obtained from the lower-case hexadecimal encoding of the MD5
-- signature of @x@.
authenticationMD5Password :: Word32 -> LazyByteString
authenticationMD5Password salt = toLazyByteString $ withAuthenticationResponseHeader 5 $
  word32BE salt

-- | Issued by the backend to request SCM credential authentication, possible
-- only on connections over local Unix-domain sockets on platforms that support
-- SCM credential messages. The frontend must issue an SCM credential message
-- and then send a single data byte. The contents of the data byte are
-- uninteresting; it's only used to ensure that the server waits long enough to
-- receive the credential message. If the credential is acceptable, the server
-- responds with an 'AuthenticationOk', otherwise it responds with an
-- 'ErrorResponse'. This message type is only issued by versions of PostgreSQL
-- servers earlier than 9.1 and may eventually be removed from the protocol
-- specification.
authenticationSCMCredential :: LazyByteString
authenticationSCMCredential = toCompactLazyByteString $ withAuthenticationResponseHeader 6 mempty
{-# NOINLINE authenticationSCMCredential #-}

-- | Issued by the backend to request GSS credential authentication. The
-- frontend should respond by initiating a GSSAPI negotiation, sending a
-- 'PasswordMessage' with the first part of the GSSAPI data stream. If further
-- messages are needed, the server will respond with an
-- 'AuthenticationGSSContinue' message.
authenticationGSS :: LazyByteString
authenticationGSS = toCompactLazyByteString $ withAuthenticationResponseHeader 7 mempty
{-# NOINLINE authenticationGSS #-}

-- | Issued by the backend to request SSPI credential authentication. The
-- frontend should respond by initiating a SSPI negotiation, sending a
-- 'PasswordMessage' with the first part of the SSPI data stream. If further
-- messages are needed, the server will respond with an
-- 'AuthenticationGSSContinue' message.
authenticationSSPI :: LazyByteString
authenticationSSPI = toCompactLazyByteString $ withAuthenticationResponseHeader 9 mempty
{-# NOINLINE authenticationSSPI #-}

-- | Issued by the backend as a response to the previous step of GSSAPI or SSPI
-- negotiation, i.e., an 'AuthenticationGSS', 'AuthenticationSSPI' or an
-- earlier 'AuthenticationGSSContinue' message. If the GSSAPI or SSPI data in
-- this message indicates more data is needed to complete the authentication,
-- the frontend must send that data as another 'PasswordMessage'.  If GSSAPI or
-- SSPI authentication is completed by this message, the server will eventually
-- send 'AuthenticationOk' to indicate successful authentication or
-- 'ErrorResponse' to indicate failure.
authenticationGSSContinue :: LazyByteString -> LazyByteString
authenticationGSSContinue content = toLazyByteString $ withAuthenticationResponseHeader 8 $
  lazyByteString content

-- | Issued by the backend to request SASL authentication, with the list of one
-- or more SASL authentication mechanisms, listed in the order of server's
-- preference.
authenticationSASL :: Foldable t => t ByteString -> LazyByteString
authenticationSASL mechanisms = toLazyByteString $ withAuthenticationResponseHeader 10 $
  foldMap byteStringNul mechanisms <>
  char8 '\0'

-- | Issued by the backend as a response to the previous step of SASL
-- authentication procedure, with SASL challenge data specific to the SASL
-- mechanism chosen by the frontend.
authenticationSASLContinue :: LazyByteString -> LazyByteString
authenticationSASLContinue content = toLazyByteString $ withAuthenticationResponseHeader 11 $
  lazyByteString content

-- | Issued by the backend to indicate completion of a SASL authentication
-- procedure, with outcome data specific to the SASL mechanism chosen by the
-- frontend.
authenticationSASLFinal :: LazyByteString -> LazyByteString
authenticationSASLFinal content = toLazyByteString $ withAuthenticationResponseHeader 12 $
  lazyByteString content

-- | A message of the form “@'BackendKeyData' pid k@” is sent by the backend as
-- part of the session establishment protocol, providing the frontend process
-- with the backend process ID @pid@ and secret @k@ required of the frontend to
-- issue query cancellation requests (see: 'CancelRequest' message.)
backendKeyData :: ProcessID -> Word32 -> LazyByteString
backendKeyData pid secret = toLazyByteString $ withMessageHeader BACKEND_KEY_DATA $
  word32BE pid <>
  word32BE secret

-- | A message of the form “@'Bind' p s pfs pvs rfs@” requests /binding/
-- (i.e., creation) of a new portal @p@ to an existing parsed statement @s@,
-- with parameter formats @pfs@, parameter values @pvs@ and result formats
-- @rfs@.  The default /unnamed portal/ and/or /unnamed statement/ can be
-- selected by setting @p@ and/or @s@ to an empty byte string
-- ('Data.ByteString.null').
--
-- The @pvs@ array must provide a field value (possibly 'Nothing' for SQL
-- @NULL@) for every actual parameter mentioned in the SQL command @s@ using
-- the @?@ or @$/n/@ syntax.  Each of these values can be encoded in either the
-- default textual or binary transfer format (both are represented in @Value@
-- as simple byte strings) and the actual choice of the format is determined by
-- the @pfs@ array. The @pfs@ array can be empty (indicating that all
-- parameters are encoded using the default textual format), singleton
-- (indicating that all parameters are encoded using the same explicitly
-- specified format) or else must have the same length as the @pvs@ array,
-- specifying the transfer formats individually for each parameter value.
--
-- Likewise, the @rfs@ array, which determines the transfer formats expected by
-- the frontend for any result values returned by the backend, can be left
-- empty, requesting the backend to use the default textual encoding of all
-- result values), specified as a singleton array (requesting the same encoding
-- for all result fields), or else match the number of columns in the result
-- set, thus specifying an individual format for each column.
bind :: (Foldable t1, Foldable t2, Foldable t3) =>
  ByteString -> ByteString -> t1 Format -> t2 (Maybe LazyByteString) -> t3 Format -> LazyByteString
bind p s pfs pvs rfs = toLazyByteString $ withMessageHeader BIND $
  byteStringNul p <>
  byteStringNul s <>
  array16BE word16BE pfs <>
  array16BE pgValue pvs <>
  array16BE word16BE rfs

-- | Sent by the backend to indicate successful completion of a 'Bind' request.
bindComplete :: LazyByteString
bindComplete = toCompactLazyByteString $ withMessageHeader BIND_COMPLETE mempty
{-# NOINLINE bindComplete #-}

-- | A message of the form “@'CancelRequest' pid secret@” requests cancellation
-- of a query currently being executed on the server by another backend process
-- with the process ID @pid@. In order to demonstrate authority to interact
-- with this backend process, the frontend must include in the message the
-- unique 32-bit key @secret@ generated by the backend process and supplied to
-- the frontend in a 'BackendKeyData' message sent as part of the session
-- establishment protocol of the targeted communication session.
cancelRequest :: ProcessID -> Word32 -> LazyByteString
cancelRequest pid secret = toLazyByteString $ withMessageSizeHeader $
  word32BE 80877102 <>
  word32BE pid <>
  word32BE secret

-- | A message of the form “@'Close' k x@” requests that the session object @x@
-- of type @k@ (either a 'STATEMENT_OBJECT' created by the 'Parse' message or a
-- 'PORTAL_OBJECT' created with 'Bind') is no longer required, and that its
-- underlying resources should be released by the server for other uses.
close :: SessionObjectKind -> SessionObjectName -> LazyByteString
close k x = toLazyByteString $ withMessageHeader CLOSE $
  word8 k <>
  byteStringNul x

-- | Sent by the backend to indicate successful completion of a 'Close'
-- request.
closeComplete :: LazyByteString
closeComplete = toCompactLazyByteString $ withMessageHeader CLOSE_COMPLETE mempty
{-# NOINLINE closeComplete #-}

-- | Sent by the backend to indicate successful completion of a 'Query' or
-- 'Execute' request, after any query results have been returned through an
-- appropriate number of 'DataRow' messages.
commandComplete :: LazyByteString -> LazyByteString
commandComplete tag = toLazyByteString $ withMessageHeader COMMAND_COMPLETE $
  lazyByteStringNul tag

-- | Transmits a chunk of a @COPY@ data string between the frontend to the
-- backend.  The actual format of the stream data is determined by the user as
-- part of the requesting @COPY@ command and communicated by the backend back
-- to the frontend in the 'CopyInResponse', 'CopyOutResponse' or
-- 'CopyBothResponse' message that heralds commencement of the @COPY@
-- sub-protocol session.
--
-- By convention, backends are expected to send complete data rows in a
-- 'CopyOutData' message, but frontends are allowed to divide stream data
-- into chunks arbitrarily without regard of data row boundaries.
copyData :: LazyByteString -> LazyByteString
copyData content = toLazyByteString $ withMessageHeader COPY_DATA $
  lazyByteString content

-- | Sent after the final 'CopyInData' or 'CopyOutData' message of a given
-- @COPY@ sub-protocol session, indicates successful completion of an entire
-- @COPY@ data stream.
copyDone :: LazyByteString
copyDone = toCompactLazyByteString $ withMessageHeader COPY_DONE mempty
{-# NOINLINE copyDone #-}

-- | A message of the form “@'CopyFail' msg@” should be sent by the frontend
-- to indicate inability to supply the required @COPY@ data stream. The byte
-- string @msg@ should provide a human-readable description of the exact error
-- condition behind the failure.
copyFail :: LazyByteString -> LazyByteString
copyFail msg = toLazyByteString $ withMessageHeader COPY_FAIL $
  lazyByteStringNul msg

-- | A message of the form “@'CopyInResponse' f fs@” is sent by the backend to
-- initiate an inbound @COPY@ sub-protocol session with the frontend. The
-- frontend should respond with zero or more 'CopyInData' messages followed by
-- a 'CopyInDone', or, if it is not prepared to do so, send a 'CopyFail'
-- message back to the server.
--
-- The /stream format/ parameter @f@ defines the overall format of the data
-- stream requested by the backend, while the array @fs@ defines the transfer
-- formats of the individual data fields in each row, and must always be set to
-- 'TEXT_FORMAT' if the overall format of the stream @f@ is set to
-- 'TEXT_STREAM_FORMAT'.
copyInResponse :: Foldable t => StreamFormat -> t Format -> LazyByteString
copyInResponse f fs = toLazyByteString $ withMessageHeader COPY_IN_RESPONSE $
  word8 f <>
  array16BE word16BE fs

-- | A message of the form “@'CopyOutResponse' f fs@” is sent by the backend to
-- initiate an outbound @COPY@ sub-protocol session with the frontend. It
-- should be followed immediately by zero or more 'CopyOutData' messages and
-- completed with 'CopyOutDone'.
--
-- The /stream format/ parameter @f@ defines the overall format of the data
-- stream requested by the backend, while the array @fs@ defines the transfer
-- formats of the individual data fields in each row, and must always be set to
-- 'TEXT_FORMAT' if the overall format of the stream @f@ is set to
-- 'TEXT_STREAM_FORMAT'.
copyOutResponse :: Foldable t => StreamFormat -> t Format -> LazyByteString
copyOutResponse f fs = toLazyByteString $ withMessageHeader COPY_OUT_RESPONSE $
  word8 f <>
  array16BE word16BE fs

-- | A message of the form “@'CopyBothResponse' f fs@” is sent by the backend
-- to initiate a bidirectional @COPY@ sub-protocol session, used only for
-- streaming replication.
--
-- The /stream format/ parameter @f@ defines the overall format of the data
-- stream requested by the backend, while the array @fs@ defines the transfer
-- formats of the individual data fields in each row, and must always be set to
-- 'TEXT_FORMAT' if the overall format of the stream @f@ is set to
-- 'TEXT_STREAM_FORMAT'.
copyBothResponse :: Foldable t => StreamFormat -> t Format -> LazyByteString
copyBothResponse f fs = toLazyByteString $ withMessageHeader COPY_BOTH_RESPONSE $
  word8 f <>
  array16BE word16BE fs

-- | Sent by the backend with a list of column or field values returned from a
-- data set returning SQL query such as @SELECT@ or @FETCH@.
dataRow :: Foldable t => t (Maybe LazyByteString) -> LazyByteString
dataRow vs = toLazyByteString $ withMessageHeader DATA_ROW $
  array16BE pgValue vs

-- | A message of the form “@'Describe' k x@” requests that the backend provide
-- details about the session object @x@ of type @k@ (either a
-- 'STATEMENT_OBJECT' created by the 'Parse' message or a 'PORTAL_OBJECT'
-- created with 'Bind'.) The backend should respond with a
-- 'ParameterDescription' or 'RowDescription' message for statement and portal
-- objects, respectively.
describe :: SessionObjectKind -> SessionObjectName -> LazyByteString
describe k x = toLazyByteString $ withMessageHeader DESCRIBE $
  word8 k <>
  byteStringNul x

-- | Sent by the backend in lieu of the 'CommandComplete' message as a response
-- to an attempt to execute an empty query string.
emptyQueryResponse :: LazyByteString
emptyQueryResponse = toCompactLazyByteString $ withMessageHeader EMPTY_QUERY_RESPONSE mempty
{-# NOINLINE emptyQueryResponse #-}

-- | Sent by the backend to indicate an error condition, with details of the
-- error communicated through a list of tagged /notice fields/ as described in
-- the definition of 'NoticeFieldTag'.
errorResponse :: Foldable t => t NoticeField -> LazyByteString
errorResponse nfs = toLazyByteString $ withMessageHeader ERROR_RESPONSE $
  foldMap (uncurry mappend . bimap word8 lazyByteStringNul) nfs <>
  char8 '\0'

-- | A message of the form “@'Execute' p n@” requests execution of a bound
-- portal @p@. If @n@ is greater than zero and @p@ represents an SQL query, at
-- most @n@ data rows should be returned by the backend; otherwise, the @n@
-- parameter is ignored and all data rows should be returned. If @p@ returns a
-- row set and @n@ is negative, the results are left unspecified by the
-- protocol.
execute :: SessionObjectName -> Int32 -> LazyByteString
execute p n = toLazyByteString $ withMessageHeader EXECUTE $
  byteStringNul p <>
  int32BE n

-- | Indicates that the backend should immediately return any pending command
-- result data.
flush :: LazyByteString
flush = toCompactLazyByteString $ withMessageHeader FLUSH mempty
{-# NOINLINE flush #-}

-- | A message of the form “@'FunctionCall' oid afs avs rf@” requests
-- execution of a PostgreSQL function with the given object ID @oid@, supplying
-- it an array of argument values @avs@ encoded in the transfer format
-- specified by the array @afs@, and expecting the function's sole result value
-- to be encoded using the transfer format @rf@. As for 'Bind' messages, @afs@
-- can be an empty array if all argument values are supplied in the default
-- text format, a singleton array to specify the same explicit transfer format
-- for all arguments, or else it must specify precisely one format for each of
-- the argument values in @avs@.
functionCall :: (Foldable t1, Foldable t2) =>
  ObjectID -> t1 Format -> t2 (Maybe LazyByteString) -> Format -> LazyByteString
functionCall oid afs avs rf = toLazyByteString $ withMessageHeader FUNCTION_CALL $
  word32BE oid <>
  array16BE word16BE afs <>
  array16BE pgValue avs <>
  word16BE rf

-- | Sent by the backend to indicate successful completion of a 'FunctionCall'
-- operation, with the sole value returned by the function call (possibly
-- @NULL@.)
functionCallResponse :: Maybe LazyByteString -> LazyByteString
functionCallResponse v = toLazyByteString $ withMessageHeader FUNCTION_CALL_RESPONSE $
  pgValue v

-- | Requests establishment of a GSSAPI-encrypted communication session.
-- The server should respond with an 'GSSResponse' message.
gssencRequest :: LazyByteString
gssencRequest = toCompactLazyByteString $ withMessageSizeHeader $
  int32BE 80877104
{-# NOINLINE gssencRequest #-}

-- | Indicates to the frontend that the backend has accepted the
-- 'GSSENCRequest'. The frontend should perform GSSAPI initialisation as per
-- RFC 2744 and, if successful, proceed to send the usual 'StartupMessage' or
-- 'CancelRequest' over the newly-established SSL channel.
gssencRequestAccepted :: LazyByteString
gssencRequestAccepted = "G"

-- | Indicates to the frontend that the backend has rejected the 'GSSENCRequest'.
-- The frontend has an option of abandoning the connection by closing the
-- underlying socket, or proceeding with an unencrypted session by sending the
-- usual 'StartupMessage' or 'CancelRequest' over the same socket without
-- encryption.
gssencRequestRejected :: LazyByteString
gssencRequestRejected = "N"

-- | Supplies a GSS or SSPI response in response to an 'AuthenticationGSS',
-- 'AuthenticationSSPI' or 'AuthenticationGSSContinue' message.
gssResponse :: LazyByteString -> LazyByteString
gssResponse content = toLazyByteString $ withMessageHeader GSS_RESPONSE $
  lazyByteString content

-- | A message of the form @'NegotiateProtocolVersion' m xs@ is issued by the
-- backend to indicate that it does not support either the minor protocol
-- version or some of the protocol options requested by the frontend in its
-- 'StartupMessage'. The message specifies the latest minor protocol version
-- @m@ that is supported by the backend and the complete list of protocol
-- options @xs@ that have been requested by the frontend but are not supported
-- by the backend.
negotiateProtocolVersion :: Foldable t => Word32 -> t ByteString -> LazyByteString
negotiateProtocolVersion m xs = toLazyByteString $ withMessageHeader NEGOTIATE_PROTOCOL_VERSION $
  word32BE m <>
  int32BE (fromIntegralCheckMaxBound $ length xs) <>
  foldMap byteStringNul xs

-- | Sent by the backend in lieu of the 'RowDescription' message, in response
-- to a 'Describe' message for a statement or portal which represents an SQL
-- command such as @CREATE@ or @INSERT@ that does not return a row set.
noData :: LazyByteString
noData = toCompactLazyByteString $ withMessageHeader NO_DATA mempty
{-# NOINLINE noData #-}

-- | Sent by the backend to inform the frontend of a condition such as a
-- warning or administrator action that may, or may be relevant to an operation
-- currently in progress and may be issued asynchronously to any other message
-- exchanges. Frontends must be prepared to accept such messages from the
-- backend at any time after the initial 'StartupMessage' of a communication
-- session.
noticeResponse :: Foldable t => t NoticeField -> LazyByteString
noticeResponse nfs = toLazyByteString $ withMessageHeader NOTICE_RESPONSE $
  foldMap (uncurry mappend . bimap word8 lazyByteStringNul) nfs <>
  char8 '\0'

-- | A message of the form “@'NotificationResponse pid' c x@” is sent by the
-- backend to inform the frontend of a @NOTIFY@ event issued by the backend
-- process @pid@, on the channel @c@ with a payload @x@. Frontends must be
-- prepared to accept such messages from the backend at any time after the
-- initial 'StartupMessage' of a communication session, irrespective of any
-- other message exchanges being conducted.
notificationResponse :: ProcessID -> ByteString -> LazyByteString -> LazyByteString
notificationResponse pid c x = toLazyByteString $ withMessageHeader NOTIFICATION_RESPONSE $
  word32BE pid <>
  byteStringNul c <>
  lazyByteStringNul x

-- | Sent by the backend in response to a statement variant of a 'Describe'
-- message, with object IDs of the types of all parameters required by the
-- statement.
parameterDescription :: Foldable t => t ObjectID -> LazyByteString
parameterDescription pts = toLazyByteString $ withMessageHeader PARAMETER_DESCRIPTION $
  array16BE word32BE pts

-- | A message of the form “@'ParameterStatus' p x@” is sent by the backend
-- whenever of the “significant” session parameters is changed, either
-- explicitly by the user with the SQL @SET@ command, or as a result of
-- administrator action.  Frontends must be prepared to accept such messages
-- from the backend at any time after the initial 'StartupMessage' of a
-- communication session, irrespective of any other message exchanges being
-- conducted.
--
-- What constitutes a “significant” message is currently left unspecified in
-- PostgreSQL documentation, and may even become configurable in future server
-- versions. At present time, these messages are issued for changes of the
-- following parameters: @server_version@, @server_encoding@,
-- @client_encoding@, @application_name@, @is_superuser@,
-- @session_authorization@, @DateStyle@, @IntervalStyle@, @TimeZone@,
-- @integer_datetimes@ and @standard_conforming_strings@.
parameterStatus :: ByteString -> LazyByteString -> LazyByteString
parameterStatus p x = toLazyByteString $ withMessageHeader PARAMETER_STATUS $
  byteStringNul p <>
  lazyByteStringNul x

-- | A message of the form “@'Parse' s q pts@” requests creation of a new
-- prepared statement object with the name @x@ in the current session from the
-- SQL command @q@.  The statement name can be set to 'Data.ByteString.null' to
-- create the default unnamed statement. The array @pts@ specifies object IDs
-- of PostgreSQL types for any query parameters appearing in @q@. It is not
-- required to specify types for all query parameters and may even be left
-- empty if not required; the types of any parameters omitted from @pts@ are
-- then inferred directly from the query string @q@ itself.
parse :: Foldable t => ByteString -> LazyByteString -> t ObjectID -> LazyByteString
parse s q pts = toLazyByteString $ withMessageHeader PARSE $
  byteStringNul s <>
  lazyByteStringNul q <>
  array16BE word32BE pts

-- | Sent by the backend in response to a successful completion of a 'Parse'
-- operation.
parseComplete :: LazyByteString
parseComplete = toCompactLazyByteString $ withMessageHeader PARSE_COMPLETE mempty
{-# NOINLINE parseComplete #-}

-- | Supplies a password string in response to an 'AuthenticationCleartextPassword'
-- or 'AuthenticationMD5Password' message from the backend.
passwordMessage :: LazyByteString -> LazyByteString
passwordMessage secret = toLazyByteString $ withMessageHeader PASSWORD_MESSAGE $
  lazyByteStringNul secret

-- | Supplies a password string in response to an @'AuthenticationMD5Password' salt@
-- message from the backend
md5PasswordMessage ::ByteString -> ByteString -> Word32 -> LazyByteString
md5PasswordMessage user secret salt =
  passwordMessage ("md5" <>
                   md5hex (md5hex (LazyByteString.fromChunks [secret, user]) <>
                           toLazyByteString (word32BE salt)))

-- | Sent by the backend after the maximum number of 'DataRow' messages
-- requested by an 'Execute' operation has been reached without exhausting the
-- entire result set.
portalSuspended :: LazyByteString
portalSuspended = toCompactLazyByteString $ withMessageHeader PORTAL_SUSPENDED mempty
{-# NOINLINE portalSuspended #-}

-- | A message of the form “@'Query' q@” requests a streamlined processing of
-- the SQL command @q@, which should be parsed, bound, executed and eventually
-- closed by the backend without further intervention by the frontend. The
-- backend is allowed to implement this interface using the default unnamed
-- session statement and portal, thus overwriting any such statements created
-- in the current session explicitly.
query :: LazyByteString -> LazyByteString
query q = toLazyByteString $ withMessageHeader QUERY $
  lazyByteStringNul q

-- | Sent by the backend as a synchronization point, indicating readiness to
-- process a new SQL command, carrying with it the status of the current
-- transaction (if any.)
readyForQuery :: TransactionStatus -> LazyByteString
readyForQuery ts = toLazyByteString $ withMessageHeader READY_FOR_QUERY $
  word8 ts

-- | Sent by the backend at the beginning of a result set as part of a simple
-- or extended query protocol, or in response to a 'Describe' message referring
-- to an SQL command that returns a row set.
rowDescription :: Foldable t => t FieldDescription -> LazyByteString
rowDescription fds = toLazyByteString $ withMessageHeader ROW_DESCRIPTION $
  array16BE pgFieldDescription fds
 where
  pgFieldDescription fd =
    byteStringNul (fieldName fd) <>
    word32BE    (fieldTableID fd) <>
    int16BE     (fieldColumnID fd) <>
    word32BE    (fieldDataTypeID fd) <>
    int16BE     (fieldDataTypeSize fd) <>
    word32BE    (fieldDataTypeModifier fd) <>
    word16BE    (fieldFormat fd)

-- | Supplies a password string in response to an 'AuthenticationSASL' message
-- from the backend.
saslInitialResponse :: ByteString -> Maybe LazyByteString -> LazyByteString
saslInitialResponse method content = toLazyByteString $ withMessageHeader SASL_INITIAL_RESPONSE $
  byteStringNul method <>
  pgValue content

-- | Requests establishment of an SSL-protected communication session.
-- The server should respond with an 'SSLResponse' message.
sslRequest :: LazyByteString
sslRequest = toCompactLazyByteString $ withMessageSizeHeader $
  int32BE 80877103
{-# NOINLINE sslRequest #-}

-- | Indicates to the frontend that the backend has accepted the 'SSLRequest'.
-- The frontend should perform a standard SSL startup handshake as per SSL
-- Specification and, if successful, proceed to send the usual 'StartupMessage'
-- or 'CancelRequest' over the newly-established SSL channel.
sslRequestAccepted :: LazyByteString
sslRequestAccepted = "S"

-- | Indicates to the frontend that the backend has rejected the 'SSLRequest'.
-- The frontend has an option of abandoning the connection by closing the
-- underlying socket, or proceeding with an unencrypted session by sending the
-- usual 'StartupMessage' or 'CancelRequest' over the same socket without
-- encryption.
sslRequestRejected :: LazyByteString
sslRequestRejected = "N"

-- | A message of the form “@'StartupMessage m n ps@” requests initiation of
-- a new database connection using protocol version @m.n@, optionally
-- configuring some session parameters to the specified default values. Besides
-- the usual set of server configuration parameters that can be configured at
-- runtime using the SQL @SET@ command, 'StartupMessage' accepts the following
-- three session-specific parameters:
--
--   * @user@, the database user name used to use,
--   * @database@, the target database, and
--   * @options@, command-line arguments for the backend.
--
-- The @user@ parameter is mandatory, but the other two may be omitted,
-- defaulting to a database with the same name as the @user@ and an empty set
-- of command-line arguments.  In addition, the use of @options@ parameter has
-- been deprecated in favour of setting individual run-time parameters.
--
-- The major and minor protocol version should be always set to
-- 'CURRENT_MAJOR_VERSION' and 'CURRENT_MINOR_VERSION', respectively, since
-- PostgreSQL does not maintain backward compatibility between releases of its
-- protocol, and the current version (3.0) is the only version guaranteed to be
-- supported by this library. Accordingly, this function should never be used
-- (unless you /really/ know what you're doing!) For every day use
-- 'startupMessage' (which requests current protocol version implicitly) should
-- be always used in preference to @startupMessage'@.
startupMessage :: Foldable t => Word16 -> Word16 -> t ConnectionParameter -> LazyByteString
startupMessage m n ps = toLazyByteString $ withMessageSizeHeader $
  int32BE (fromIntegral m * 65536 + fromIntegral n) <>
  foldMap (uncurry mappend . bimap byteStringNul byteStringNul) ps <>
  char8 '\0'

-- | Requests synchronisation point after a failed query in the backend. After
-- receiving a “'Sync'” message, the backend should exit error recovery mode
-- and prepare itself to receive the next valid query from the frontend.
sync :: LazyByteString
sync = toCompactLazyByteString $ withMessageHeader SYNC mempty
{-# NOINLINE sync #-}

-- | Requests graceful termination of a communication session. After
-- transmitting a “'Terminate'” message, the frontend should refrain from
-- sending any further messages to the backend and immediately close the
-- connection socket.
terminate :: LazyByteString
terminate = toCompactLazyByteString $ withMessageHeader TERMINATE mempty
{-# NOINLINE terminate #-}
