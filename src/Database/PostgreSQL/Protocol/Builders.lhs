> -- | Module:      Database.PostgreSQL.Protocol.Builders
> -- Description: Builders for PostgreSQL messages.
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

> {-# LANGUAGE OverloadedStrings #-}

> module Database.PostgreSQL.Protocol.Builders where

> import Data.ByteString (ByteString)
> import Data.ByteString.Builder
> import Data.Int
> import Data.Monoid
> import Data.Word

> import Database.PostgreSQL.Protocol.Types
> import Database.PostgreSQL.Protocol.Internal.Builders

> import qualified Data.ByteString.Lazy as Lazy


> -- * Session Message Builders

> -- | Builder for 'SessionMessage' values.
> --   In most cases, this function should probably be avoided in favour
> --   of a direct use of the individual message type builders defined below.
> sessionMessage :: SessionMessage -> Builder
> sessionMessage (StartupMessage ps) = startupMessage ps
> sessionMessage (CancelRequest pid secret) = cancelRequestMessage pid secret
> sessionMessage (SSLRequest) = sslRequestMessage

> -- | Requests initiation of a new database connection, optionally configuring some session
> --   parameters to the specified default values. Besides the usual set of server configuration
> --   parameters that can be configured at runtime using the SQL @SET@ command, 'StartupMessage'
> --   accepts the following three session-specific parameters:
> --
> --   * @user@, the database user name used to use,
> --   * @database@, the target database, and
> --   * @options@, command-line arguments for the backend.
> --
> --   The @user@ parameter is mandatory, but the other two may be omitted, defaulting to
> --   a database with the same name as the @user@ and an empty set of command-line arguments.
> --   In addition, the use of @options@ parameter has been deprecated in favour of setting
> --   individual run-time parameters.
> startupMessage :: [(SessionParameterName, ByteString)] -> Builder
> startupMessage ps = int32BE (9 + sum (map pgParameterSize ps)) <> int32BE 196608 <> mconcat (map pgParameter ps) <> word8 0
>  where
>   pgParameter (name, value) = pgString name <> pgString value
>   pgParameterSize (name, value) = pgStringSize name + pgStringSize value

> -- | A message of the form “@CancelRequest pid secret@” requests cancellation of a query
> --   currently being executed on the server by another backend process with the process
> --   ID @pid@. In order to demonstrate authority to interact with this backend process,
> --   the frontend must include in the message the unique 32-bit key @secret@ generated
> --   by the backend process and supplied to the frontend in a 'BackendKeyData' message
> --   sent as part of the session establishment protocol of the targeted communication session.
> cancelRequestMessage :: ProcessID -> Word32 -> Builder
> cancelRequestMessage pid secret = int32BE 16 <> word32BE 80877102 <> word32BE pid <> word32BE secret

> -- | Requests establishment of an SSL-protected communication session.
> --   The server should respond with an 'SSLResponse' message described below.
> sslRequestMessage :: Builder
> sslRequestMessage = int32BE 8 <> int32BE 80877103


> -- | Builder for the special-case response to an 'SSLRequest' message.
> --   In most cases, this function should probably be avoided in favour
> --   of a direct use of the individual message type builders defined below.
> sslResponseMessage :: SSLResponse -> Builder
> sslResponseMessage (SSLRequestAccepted)   = sslRequestAcceptedMessage
> sslResponseMessage (SSLRequestRejected)   = sslRequestRejectedMessage
> sslResponseMessage (SSLRequestFailed nfs) = sslRequestFailedMessage nfs

> -- | Indicates to the frontend that the backend has accepted the 'SSLRequest'. The frontend
> --   should perform a standard SSL startup handshake as per SSL Specification and, if successful,
> --   proceed to send the usual 'StartupMessage' or 'CancelRequest' over the newly-established
> --   SSL channel.
> sslRequestAcceptedMessage :: Builder
> sslRequestAcceptedMessage = char8 'S'

> -- | Indicates to the frontend that the backend has rejected the 'SSLRequest'. The frontend
> --   has an option of abandoning the connection by closing the underlying socket, or proceeding
> --   with an unencrypted session by sending the usual 'StartupMessage' or 'CancelRequest' over
> --   the same socket without encryption.
> sslRequestRejectedMessage :: Builder
> sslRequestRejectedMessage = char8 'N'

> -- | Indicates to the frontend that the backend does not understand 'SSLRequest' messages.
> --   This would only occur if the server predates the addition of SSL support to PostgreSQL.
> --   Such servers are now very ancient, and likely do not exist in the wild anymore. In this
> --   case the connection must be closed, but the frontend might choose to open another, fresh
> --   connection and proceed without requesting SSL. The notice returned by the backend is
> --   unlikely to continue meaningful error information and should most likely be ignored.
> sslRequestFailedMessage :: NoticeFields -> Builder
> sslRequestFailedMessage = errorResponseMessage


> -- * Frontend Message Builders

> -- | Builder for 'FrontendMessage' values.
> --   In most cases, this function should probably be avoided in favour
> --   of a direct use of the individual message type builders defined below.
> frontendMessage :: FrontendMessage -> Builder
> frontendMessage (Bind p s pfs pvs rfs)        = bindMessage p s pfs pvs rfs
> frontendMessage (Close k x)                   = closeMessage k x
> frontendMessage (CopyInData content)          = copyDataMessage content
> frontendMessage (CopyInDone)                  = copyDoneMessage
> frontendMessage (CopyFail msg)                = copyFailMessage msg
> frontendMessage (Describe k x)                = describeMessage k x
> frontendMessage (Execute p n)                 = executeMessage p n
> frontendMessage (Flush)                       = flushMessage
> frontendMessage (FunctionCall oid pfs pvs rf) = functionCallMessage oid pfs pvs rf
> frontendMessage (Parse s q pts)               = parseMessage s q pts
> frontendMessage (PasswordMessage x)           = passwordMessage x
> frontendMessage (Query q)                     = queryMessage q
> frontendMessage (Sync)                        = syncMessage
> frontendMessage (Terminate)                   = terminateMessage

> -- | A message of the form “@Bind p s pfs pvs rfs@” message requests /binding/ (i.e., creation)
> --   of a new portal @p@ to an existing parsed statement @s@, with parameter formats @pfs@,
> --   parameter values @pvs@ and result formats @rfs@. The default /unnamed portal/ and/or
> --   /unnamed statement/ can be selected by setting @p@ and/or @s@ to an empty byte string
> --   ('ByteString.null').
> --
> --   The @pvs@ array must provide a field value (possibly 'Nothing' for SQL @NULL@) for
> --   every actual parameter mentioned in the SQL command @s@ using the @?@ or @$/n/@ syntax.
> --   Each of these values can be encoded in either the default textual or binary transfer
> --   format (both are represented in @Value@ as simple byte strings) and the actual
> --   choice of the format is determined by the @pfs@ array. The @pfs@ array can be empty
> --   (indicating that all parameters are encoded using the default textual format),
> --   singleton (indicating that all parameters are encoded using the same explicitely
> --   specified format) or else must have the same length as the @pvs@ array, specifying
> --   the transfer formats individually for each parameter value.
> --
> --   Likewise, the @rfs@ array, which determines the transfer formats expected by the
> --   frontend for any result values returned by the backend, can be left empty, requesting
> --   the backend to use the default textual encoding of all result values), specified
> --   as a singleton array (requesting the same encoding for all result fields), or
> --   else match the number of columns in the result set, thus specifying an individual
> --   format for each column.
> bindMessage :: PortalName -> StatementName -> UArray16 Format -> Array16 Value -> UArray16 Format -> Builder
> bindMessage p s pfs pvs rfs =
>   char8 'B' <>
>   int32BE (4 + pgStringSize p
>              + pgStringSize s
>              + pgUArraySize 2 pfs
>              + pgArraySize pgValueSize pvs
>              + pgUArraySize 2 rfs) <>
>   pgString p <>
>   pgString s <>
>   pgUArray word16BE pfs <>
>   pgArray pgValue pvs <>
>   pgUArray word16BE rfs

> -- | A message of the form “@Close k x@” requests that the session object @x@ of type @k@
> --   (either a 'StatementObject' created by the 'Parse' message or a 'PortalObject' created
> --   with 'Bind') is no longer required, and that its underlying resources should be released
> --   by the server for other uses.
> closeMessage :: SessionObjectKind -> SessionObjectName -> Builder
> closeMessage k x = char8 'C' <> int32BE (5 + pgStringSize x) <> word8 k <> pgString x

> -- | Transmits a chunk of a @COPY@ data string between the frontend to the backend.
> --   The actual format of the stream data is determined by the user as part of the
> --   requesting @COPY@ command and communicated by the backend back to the frontend
> --   in the 'CopyInResponse', 'CopyOutResponse' or 'CopyBothResponse' message that
> --   heralds commencement of the @COPY@ subprotocol session.
> --
> --   By convention, backends are expected to send complete data rows in a 'CopyOutData'
> --   message, but frontends are allowed to divide stream data into chunks arbitriarly
> --   without regard of data row boundaries.
> copyDataMessage :: DataString -> Builder
> copyDataMessage content =
>   char8 'd' <> int32BE (4 + fromIntegralCheckMaxBound (maxBound - 4) (Lazy.length content)) <> lazyByteString content

> -- | Sent after the final 'CopyInData' or 'CopyOutData' message of a given @COPY@ subprotocol session,
> --   indicates successful completion of an entire @COPY@ data stream.
> copyDoneMessage :: Builder
> copyDoneMessage = char8 'c' <> int32BE 4

> -- | A message of the form “@CopyFail msg@” should be sent by the frontend to indicate
> --   inability to supply the required @COPY@ data stream. The byte string @msg@ should
> --   provide a human-readable description of the exact error condition behind the failure.
> copyFailMessage :: ByteString -> Builder
> copyFailMessage msg = char8 'f' <> int32BE (5 + pgStringSize msg) <> pgString msg

> -- | A message of the form “@Describe k x@” requests that the backend provide details about
> --   the session object @x@ of type @k@ (either a 'StatementObject' created by the 'Parse'
> --   message or a 'PortalObject' created with 'Bind'.) The backend should respond with
> --   a 'ParameterDescription' or 'RowDescription' message for statement and portal objects,
> --   respectively.
> describeMessage :: SessionObjectKind -> SessionObjectName -> Builder
> describeMessage k x = char8 'D' <> int32BE (5 + pgStringSize x) <> word8 k <> pgString x

> -- | A message of the form “@Execute p n@” requests execution of a bound portal @p@.
> --   If @n@ is greater than zero and @p@ represents an SQL query, at most @n@ data rows
> --   should be returned by the backend; otherwise, the @n@ parameter is ignored and all data
> --   rows should be returned. If @p@ returns a row set and @n@ is negative, the results are
> --   left unspecified by the protocol.
> executeMessage :: PortalName -> Int32 -> Builder
> executeMessage p n = char8 'E' <> int32BE (8 + pgStringSize p) <> pgString p <> int32BE n

> -- | Indicates that the backend should immediately return any pending command result data.
> flushMessage :: Builder
> flushMessage = char8 'H' <> int32BE 4

> -- | A message of the form “@FunctionCall oid afs avs rf@” requests execution of
> --   a PostgreSQL function with the given object ID @oid@, supplying it an array of
> --   argument values @avs@ encoded in the transfer format specified by the array @afs@,
> --   and expecting the function's sole result value to be encoded using the transfer
> --   format @rf@. As for 'Bind' messages, @afs@ can be an empty array if all argument
> --   values are supplied in the default text format, a singleton array to specify the
> --   same explicit transfer format for all arguments, or else it must specify precisely
> --   one format for each of the argument values in @avs@.
> functionCallMessage :: ObjectID -> UArray16 Format -> Array16 Value -> Format -> Builder
> functionCallMessage oid afs avs rf =
>   char8 'F' <>
>   int32BE (10 + pgUArraySize 2 afs + pgArraySize pgValueSize avs) <>
>   word32BE oid <>
>   pgUArray word16BE afs <>
>   pgArray pgValue avs <>
>   word16BE rf

> -- | A message of the form “@Parse s q pts@” requests creation of a new prepared statement
> --   object with the name @x@ in the current session from the SQL command @q@.
> --   The statement name can be set to 'ByteString.null' to create the default unnamed
> --   statement. The array @pts@ specifies object IDs of PostgreSQL types for any query
> --   parameters appearing in @q@. It is not required to specify types for all query
> --   parameters and may even be left empty if not required; the types of any parameters
> --   omitted from @pts@ are then inferred directly from the query string @q@ itself.
> parseMessage :: StatementName -> QueryString -> UArray16 ObjectID -> Builder
> parseMessage s q pts =
>   char8 'P' <>
>   int32BE (4 + pgStringSize s + pgLazyStringSize q + pgUArraySize 4 pts) <>
>   pgString s <>
>   pgLazyString q <>
>   pgUArray word32BE pts

> -- | Supplies a password string in response to an 'Authentication' message from the
> --   backend, encrypted if required using the method requested by the backend.
> passwordMessage :: ByteString -> Builder
> passwordMessage secret = char8 'p' <> int32BE (4 + pgStringSize secret) <> pgString secret

> -- | A message of the form “@Query q@” requests a streamlined processing of the SQL
> --   command @q@, which should be parsed, bound, executed and eventually closed by
> --   the backend without further intervention by the frontend. The backend is allowed
> --   to implement this interface using the default unnamed session statement and portal,
> --   thus overwriting any such statements created in the current session explicitly.
> queryMessage :: QueryString -> Builder
> queryMessage q = char8 'Q' <> int32BE (4 + pgLazyStringSize q) <> pgLazyString q

> -- | Requests synchronisation point after a failed query in the backend.
> --   After receiving a “@Sync@” message, the backend should exit error recovery
> --   mode and prepare itself to receive the next valid query from the frontend.
> syncMessage :: Builder
> syncMessage = char8 'S' <> int32BE 4

> -- | Requests graceful termination of a communication session. After transmitting
> --   a “@Terminate@” message, the frontend should refrain from sending any further
> --   messages to the backend and immediately close the connection socket.
> terminateMessage :: Builder
> terminateMessage = char8 'X' <> int32BE 4


> -- * Backend Message Builders

> -- | Builder for 'BackendMessage' values.
> --   In most cases, this function should probably be avoided in favour
> --   of a direct use of the individual message type builders defined below.
> backendMessage :: BackendMessage -> Builder
> backendMessage (AuthenticationResponse ar)    = authenticationResponseMessage ar
> backendMessage (BackendKeyData pid k)         = backendKeyDataMessage pid k
> backendMessage (BindComplete)                 = bindCompleteMessage
> backendMessage (CloseComplete)                = closeCompleteMessage
> backendMessage (CommandComplete rt)           = commandCompleteMessage rt
> backendMessage (CopyOutData content)          = copyDataMessage content
> backendMessage (CopyOutDone)                  = copyDoneMessage
> backendMessage (CopyInResponse f fs)          = copyInResponseMessage f fs
> backendMessage (CopyOutResponse f fs)         = copyOutResponseMessage f fs
> backendMessage (CopyBothResponse f fs)        = copyBothResponseMessage f fs
> backendMessage (DataRow vs)                   = dataRowMessage vs
> backendMessage (EmptyQueryResponse)           = emptyQueryResponseMessage
> backendMessage (ErrorResponse nfs)            = errorResponseMessage nfs
> backendMessage (FunctionCallResponse v)       = functionCallResponseMessage v
> backendMessage (NoData)                       = noDataMessage
> backendMessage (NoticeResponse nfs)           = noticeResponseMessage nfs
> backendMessage (NotificationResponse pid c x) = notificationResponseMessage pid c x
> backendMessage (ParameterDescription pts)     = parameterDescriptionMessage pts
> backendMessage (ParameterStatus p x)          = parameterStatusMessage p x
> backendMessage (ParseComplete)                = parseCompleteMessage
> backendMessage (PortalSuspended)              = portalSuspendedMessage
> backendMessage (ReadyForQuery ts)             = readyForQueryMessage ts
> backendMessage (RowDescription fds)           = rowDescriptionMessage fds

> -- | Sent by a backend in response to a 'StartupMessage' with details of any
> --   authentication requirements imposed on the frontend. In Chapter 49 of
> --   PostgreSQL manual, this is documented as an array of individual messages,
> --   but in the Haskell implementation we combine them into a single
> --   'AuthenticationResponse' constructor to simplify processing.
> authenticationResponseMessage :: AuthenticationResponse -> Builder
> authenticationResponseMessage (AuthenticationOk)                          = authenticationOkMessage
> authenticationResponseMessage (AuthenticationKerberosV5)                  = authenticationKerberosV5Message
> authenticationResponseMessage (AuthenticationCleartextPassword)           = authenticationCleartextPasswordMessage
> authenticationResponseMessage (AuthenticationMD5Password salt)            = authenticationMD5PasswordMessage salt
> authenticationResponseMessage (AuthenticationSCMCredential)               = authenticationSCMCredentialMessage
> authenticationResponseMessage (AuthenticationGSS)                         = authenticationGSSMessage
> authenticationResponseMessage (AuthenticationGSSContinue content)         = authenticationGSSContinueMessage content
> authenticationResponseMessage (AuthenticationSSPI)                        = authenticationSSPIMessage
> authenticationResponseMessage (AuthenticationMiscellaneous tag content)   = authenticationMiscellaneousMessage tag content


> -- | Issued by the backend to signify successful authentication of the frontend's credentials.
> authenticationOkMessage :: Builder
> authenticationOkMessage = char8 'R' <> int32BE 8 <> word32BE 0

> -- | Issued by the backend to initiate Kerberos V5 authentication dialogue,
> --   described separately in Kerberos specification. This authentication method
> --   is no longer supported by recent versions of PostgreSQL software.
> authenticationKerberosV5Message :: Builder
> authenticationKerberosV5Message = char8 'R' <> int32BE 8 <> word32BE 2

> -- | Issued by the backend to request clear-text password authentication.
> --   The frontend should respond with a 'PasswordMessage' containing an unencrypted
> --   text of the user's password.
> authenticationCleartextPasswordMessage :: Builder
> authenticationCleartextPasswordMessage = char8 'R' <> int32BE 8 <> word32BE 3

> -- | A message of the form “@AuthenticationMD5Password s@” is issued by the backend
> --   to request MD5-based password authentication with the specified 32-bit /salt/ @s@.
> --   The frontend should respond with a 'PasswordMessage x', in which @x@ is a byte string
> --   derived from the user's login name @u@, password @p@ and the supplied salt @ss@ as follows:
> --
> -- @
> --      "md5" <> md5 (md5 (/p/ <> /u/) <> /ss/
> -- @
> --
> --   where /s/ is a 4-byte byte string obtained from the big-endian encoding of the supplied
> --   salt @s@, and @md5(x)@ is a function that returns a 32-byte bytestring obtained from the
> --   lowercase hexadecimal encoding of the MD5 signature of @x@.
> authenticationMD5PasswordMessage :: Word32 -> Builder
> authenticationMD5PasswordMessage salt = char8 'R' <> int32BE 12 <> word32BE 5 <> word32BE salt

> -- | Issued by the backend to request SCM credential authentication, possible only on
> --   connections over local Unix-domain sockets on platforms that support SCM credential
> --   messages. The frontend must issue an SCM credential message and then send a single
> --   data byte. The contents of the data byte are uninteresting; it's only used to ensure
> --   that the server waits long enough to receive the credential message. If the credential
> --   is acceptable, the server responds with an 'AuthenticationOk', otherwise it responds
> --   with an 'ErrorResponse'. This message type is only issued by versions of PostgreSQL
> --   servers earlier than 9.1 and may eventually be removed from the protocol specification.
> authenticationSCMCredentialMessage :: Builder
> authenticationSCMCredentialMessage = char8 'R' <> int32BE 8 <> word32BE 6

> -- | Issued by the backend to request GSS credential authentication. The frontend should respond
> --   by initiating a GSSAPI negotiation, sending a 'PasswordMessage' with the first part of the
> --   GSSAPI data stream. If further messages are needed, the server will respond with an
> --   'AuthenticationGSSContinue' message.
> authenticationGSSMessage :: Builder
> authenticationGSSMessage = char8 'R' <> int32BE 8 <> word32BE 7

> -- | Issued by the backend to request SSPI credential authentication. The frontend should respond
> --   by initiating a SSPI negotiation, sending a 'PasswordMessage' with the first part of the
> --   SSPI data stream. If further messages are needed, the server will respond with an
> --   'AuthenticationGSSContinue' message.
> authenticationSSPIMessage :: Builder
> authenticationSSPIMessage = char8 'R' <> int32BE 8 <> word32BE 9

> -- | Issued by the backend as a response to the previous step of GSSAPI or SSPI negotiation,
> --   i.e., an 'AuthenticationGSS', 'AuthenticationSSPI' or an earlier 'AuthenticationGSSContinue'
> --   message. If the GSSAPI or SSPI data in this message indicates more data is needed to complete
> --   the authentication, the frontend must send that data as another 'PasswordMessage'.
> --   If GSSAPI or SSPI authentication is completed by this message, the server will eventually
> --   send 'AuthenticationOk' to indicate successful authentication or 'ErrorResponse' to indicate
> --   failure.
> authenticationGSSContinueMessage :: DataString -> Builder
> authenticationGSSContinueMessage content =
>   char8 'R' <> int32BE (8 + fromIntegralCheckMaxBound (maxBound - 8) (Lazy.length content)) <> word32BE 8 <> lazyByteString content

> -- | A message of the form “@AuthenticationMiscellaneous t x@” is used to encode possible future
> --   authentication methods that are not recognized by the current version of the library.
> --   The 32-bit tag @t@ describes the authentication method requested and @x@ described any
> --   authentication parameters (possibly 'Data.ByteString.Lazy.null'), in the method-specific
> --   format. The only sensible response to this message is to abandon the conection after
> --   issuing an appropriate notification message to the user.
> authenticationMiscellaneousMessage :: Word32 -> DataString -> Builder
> authenticationMiscellaneousMessage tag content =
>   char8 'R' <> int32BE (8 + fromIntegralCheckMaxBound (maxBound - 8) (Lazy.length content)) <> word32BE tag <> lazyByteString content

> -- | A message of the form “@BackendKeyData pid k@” is sent by the backend
> --   as part of the session establishment protocol, providing the frontend
> --   process with the backend process ID @pid@ and secret @k@ required of
> --   the frontend to issue query cancellation requests (see: 'CancelQuery'
> --   message type above.)
> backendKeyDataMessage :: ProcessID -> Word32 -> Builder
> backendKeyDataMessage pid secret = char8 'B' <> int32BE 12 <> word32BE pid <> word32BE secret

> -- | Sent by the backend to indicate successful completion of a 'Bind' request.
> bindCompleteMessage :: Builder
> bindCompleteMessage = char8 '2' <> int32BE 4

> -- | Sent by the backend to indicate successful completion of a 'Close' request.
> closeCompleteMessage :: Builder
> closeCompleteMessage = char8 '3' <> int32BE 4

> -- | Sent by the backend to indicate successful completion of a 'Query' or 'Execute'
> --   request, after any query results have been returned through an appropriate
> --   number of 'DataRow' messages.
> commandCompleteMessage :: ResultTag -> Builder
> commandCompleteMessage rt = char8 'C' <> int32BE (4 + pgLazyStringSize rt) <> pgLazyString rt

> -- | A message of the form “@CopyInResponse f fs@” is sent by the backend to initiate
> --   an inbound @COPY@ subprotocol session with the frontend. The frontend should
> --   respond with zero or more 'CopyInData' messages followed by a 'CopyInDone',
> --   or, if it is not prepared to do so, send a 'CopyFail' message back to the server.
> --
> --   The /stream format/ parameter @f@ defines the overall format of the data stream
> --   requested by the backend, while the array @fs@ defines the transfer formats of
> --   the individual data fields in each row, and must always be set to 'TextFormat'
> --   if the overal format of the stream @f@ is set to 'TextStreamFormat'.
> copyInResponseMessage :: StreamFormat -> UArray16 Format -> Builder
> copyInResponseMessage f fs = char8 'G' <> int32BE (5 + pgUArraySize 2 fs) <> word8 f <> pgUArray word16BE fs

> -- | A message of the form “@CopyOutResponse f fs@” is sent by the backend to initiate
> --   an outbound @COPY@ subprotocol session with the frontend. It should be followed
> --   immediately by zero or more 'CopyOutData' messages and completed with 'CopyOutDone'.
> --
> --   The /stream format/ parameter @f@ defines the overall format of the data stream
> --   requested by the backend, while the array @fs@ defines the transfer formats of
> --   the individual data fields in each row, and must always be set to 'TextFormat'
> --   if the overal format of the stream @f@ is set to 'TextStreamFormat'.
> copyOutResponseMessage :: StreamFormat -> UArray16 Format -> Builder
> copyOutResponseMessage f fs = char8 'H' <> int32BE (5 + pgUArraySize 2 fs) <> word8 f <> pgUArray word16BE fs

> -- | A message of the form “@CopyOutResponse f fs@” is sent by the backend to initiate
> --   a bidirectional @COPY@ subprotocol session, used only for streaming replication.
> --
> --   The /stream format/ parameter @f@ defines the overall format of the data stream
> --   requested by the backend, while the array @fs@ defines the transfer formats of
> --   the individual data fields in each row, and must always be set to 'TextFormat'
> --   if the overal format of the stream @f@ is set to 'TextStreamFormat'.
> copyBothResponseMessage :: StreamFormat -> UArray16 Format -> Builder
> copyBothResponseMessage f fs = char8 'W' <> int32BE (5 + pgUArraySize 2 fs) <> word8 f <> pgUArray word16BE fs

> -- | Sent by the backend with a list of column or field values returned from a data set
> --   returning SQL query such as @SELECT@ or @FETCH@.
> dataRowMessage :: Array16 Value -> Builder
> dataRowMessage vs = char8 'D' <> int32BE (4 + pgArraySize pgValueSize vs) <> pgArray pgValue vs

> -- | Sent by the backend in lieu of the 'CommandComplete' message as a response to
> --   an attempt to execute an empty query string.
> emptyQueryResponseMessage :: Builder
> emptyQueryResponseMessage = char8 'I' <> int32BE 4

> -- | Sent by the backend to indicate an error condition, with details of the error
> --   communicated through a list of tagged /notice fields/ as described in the
> --   definition of the 'NoticeFieldTag'.
> errorResponseMessage :: NoticeFields -> Builder
> errorResponseMessage nfs = char8 'E' <> int32BE (5 + sum (map pgNoticeFieldSize nfs)) <> mconcat (map pgNoticeField nfs) <> word8 0
>  where
>   pgNoticeField (t, x) = word8 t <> pgString x
>   pgNoticeFieldSize (_, x) = 1 + pgStringSize x

> -- | Sent by the backend to indicate successful completion of a 'FunctionCall'
> --   operation, with the sole value returned by the function call (possibly @NULL@.)
> functionCallResponseMessage :: Value -> Builder
> functionCallResponseMessage v = char8 'V' <> int32BE (4 + pgValueSize v) <> pgValue v

> -- | Sent by the backend in lieu of the 'RowDescription' message, in response
> --   to a 'Describe' message for a statement or portal which represents an SQL
> --   command such as @CREATE@ or @INSERT@ that does not return a row set.
> noDataMessage :: Builder
> noDataMessage = char8 'n' <> int32BE 4

> -- | Sent by the backend to inform the frontend of a condition such as a warning
> --   or administrator action that may, or may be relevant to an operation currently
> --   in progress and may be issued asynchronously to any other message exchanges.
> --   Frontends must be prepared to accept such messages from the backend at any
> --   time after the initial 'StartupMessage' of a communication session.
> noticeResponseMessage :: NoticeFields -> Builder
> noticeResponseMessage nfs = char8 'N' <> int32BE (5 + sum (map pgNoticeFieldSize nfs)) <> mconcat (map pgNoticeField nfs) <> word8 0
>  where
>   pgNoticeField (t, x) = word8 t <> pgString x
>   pgNoticeFieldSize (_, x) = 1 + pgStringSize x

> -- | A message of the form “@NotificationResponse pid c x@” is sent by the backend
> --   to inform the frontend of a @NOTIFY@ event issued by the backend process @pid@,
> --   on the channel @c@ with a payload @x@. Frontends must be prepared to accept
> --   such messages from the backend at any time after the initial 'StartupMessage'
> --   of a communication session, irrespective of any other message exchanges being
> --   conducted.
> notificationResponseMessage :: ProcessID -> ChannelName -> ByteString -> Builder
> notificationResponseMessage pid c x =
>   char8 'A' <> int32BE (8 + pgStringSize c + pgStringSize x) <> word32BE pid <> pgString c <> pgString x

> -- | Sent by the backend in response to a statement variant of a 'Describe' message,
> --   with object IDs of the types of all parameters required by the statement.
> parameterDescriptionMessage :: UArray16 ObjectID -> Builder
> parameterDescriptionMessage pts = char8 't' <> int32BE (4 + pgUArraySize 4 pts) <> pgUArray word32BE pts

> -- | A message of the form “@ParameterStatus p x@” is sent by the backend whenever
> --   of the “significant” session parameters is changed, either explicitly by the
> --   user with the SQL @SET@ comand, or as a result of administrator action.
> --   Frontends must be prepared to accept such messages from the backend at any
> --   time after the initial 'StartupMessage' of a communication session,
> --   irrespective of any other message exchanges being conducted.
> --
> --   What constitutes a “significant” message is currently left unspecified in
> --   PostgreSQL documentation, and may even become configurable in future server
> --   versions. At present time, these messages are issued for changes of the
> --   following parametes: @server_version@, @server_encoding@, @client_encoding@,
> --   @application_name@, @is_superuser@, @session_authorization@, @DateStyle@,
> --   @IntervalStyle@, @TimeZone@, @integer_datetimes@ and @standard_conforming_strings@.
> parameterStatusMessage :: SessionParameterName -> ByteString -> Builder
> parameterStatusMessage p x = char8 'S' <> int32BE (4 + pgStringSize p + pgStringSize x) <> pgString p <> pgString x

> -- | Sent by the backend in response to a successful completion of a 'Parse' operation.
> parseCompleteMessage :: Builder
> parseCompleteMessage = char8 '1' <> int32BE 4

> -- | Sent by the backend after the maximum number of 'DataRow' messages requested by
> --   an 'Execute' operation has been reached without exhausting the entire result set.
> portalSuspendedMessage :: Builder
> portalSuspendedMessage = char8 's' <> int32BE 4

> -- | Sent by the backend as a synchronization point, indicating readiness to process
> --   a new SQL command, carrying with it the status of the current transaction (if any.)
> readyForQueryMessage :: TransactionStatus -> Builder
> readyForQueryMessage ts = char8 'Z' <> int32BE 5 <> word8 ts

> -- | Sent by the backend at the beginning of a result set as part of a simple or extended
> --   query protocol, or in response to a 'Describe' message referring to an SQL command
> --   that returns a row set.
> rowDescriptionMessage :: Array16 FieldDescription -> Builder
> rowDescriptionMessage fds = char8 'T' <> int32BE (4 + pgArraySize pgFieldDescriptionSize fds) <> pgArray pgFieldDescription fds
>  where
>   pgFieldDescription fd =
>     pgString (fieldName fd) <>
>     word32BE (fieldTableID fd) <>
>     int16BE  (fieldColumnID fd) <>
>     word32BE (fieldDataTypeID fd) <>
>     int16BE  (fieldDataTypeSize fd) <>
>     word32BE (fieldDataTypeModifier fd) <>
>     word16BE (fieldFormat fd)
>   pgFieldDescriptionSize fd = pgStringSize (fieldName fd) + 18
