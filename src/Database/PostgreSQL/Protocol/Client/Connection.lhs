> -- | Module:      Database.PostgreSQL.Protocol.Client.Connection
> -- | Description: Connection type for PostgreSQL clients
> -- | Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- | License:     BSD3
> -- | Maintainer:  pat@jantar.org
> -- | Stability:   experimental
> -- | Portability: portable

> module Database.PostgreSQL.Protocol.Client.Connection (
>   Connection
> ) where

> import Control.Concurrent.MVar
> import Control.Exception
> import Database.PostgreSQL.Protocol.Internal.Queue (Queue)
> import Network.Socket (Socket, SockAddr)

> import qualified Network.Socket as Socket

> import qualified Database.PostgreSQL.Protocol.Internal.Queue as Queue

> data Connection = Connection {
>   connectionSocket           :: Socket,
>   connectionStatusVar        :: MVar Bool,    -- True: connection open; False: connection closed; empty: receiver thread busy executing user code
>   connectionReceiverThreadId :: ThreadId,
>   connectionReceiverQueueVar :: MVar (Queue (forall a. BackendMessageHandler a -> a))
> }

> data BackendMessageHandler = BackendMessageHandler {
>   ...
> }

> data BackendNotificationHandler = BackendNotificationHandler {
>   onNoticeResponse       :: BackendNotificationHandler -> [(Char, ByteString)] -> IO (),
>   onNotificationResponse :: BackendNotificationHandler -> Int32 -> ByteString -> ByteString -> IO (),
>   onParameterStatus      :: BackendNotificationHandler -> ByteString -> ByteString -> IO (),
>   onUnrecognizedMessage  :: BackendNotificationHandler -> Char -> IO (),
>   onCorruptedMessage     :: BackendNotificationHandler -> Char -> IO (),
>   onProtocolError        :: BackendNotificationHandler -> [(Char, ByteString)] -> IO (),
>   onCommunicationError   :: BackendNotificationHandler -> [(Char, ByteString)] -> IO ()
> }

> backendNotificationHandler :: BackendNotificationHandler
> backendNotificationHandler = BackendNotificationHandler {
>   onNoticeResponse       = \ _this  parameters -> printNotice parameters,
>   onNotificationResponse = \ _this _pid _channel _payload -> return (),
>   onParameterStatus      = \ _this _parameter _value -> return (),
>   onUnrecognizedMessage  = \  this  tag -> onProtocolError this this (unrecognizedMessageNotice tag),
>   onCorruptedMessage     = \  this  tag -> onProtocolError this this (corruptedMessageNotice tag),
>   onProtocolError        = \  this  parameters -> onCommunicationError (show parameters,
>   onCommunicationError   = \ _this  parameters -> printNotice communicationErrorNotice >> throw ...,
> }

> unrecognizedMessageNotice :: Char -> [(Char, ByteString)]
> unrecognizedMessageNotice tag = [ ('S', "ERROR"), ('C', "08P01"), ('M', "Unrecognized message received from server"), ('D', "Message tag: " <> fromString (show tag)) ]

> corruptedMessageNotice :: Char -> [(Char, ByteString)]
> corruptedMessageNotice tag = [ ('S', "ERROR"), ('C', "08P01"), ('M', "Corrupted message received from server"), ('D', "Message tag: " <> fromString (show tag)) ]

> communicationErrorNotice :: [(Char, ByteString)]
> communicationErrorNotice      = [ ('S', "FATAL"), ('C', "08006"), ('M', "Communication with server failed") ]

> printNotice :: [(Char, ByteString)] -> IO ()
> printNotice 

> showNotice :: [(Char, ByteString)] -> String
> showNotice params = severity <> ": " <> message
>  where
>   severity = fromMaybe "ERROR" . fmap fst . uncons . map snd . filter ((== 'S') . fst) params
>   message = fromMaybe "Notice message received from server"

> {-

> close :: Connection -> IO ()
> close c = bracket (takeMVar (connectionStatusVar c)) (const $ putMVar (connectionStatusVar c) False) $ flip when $ do
>   killThread (connectionReceiverThreadId c)
>   sendMessage (connectionSocket c) terminateMessage
>   Socket.sClose (connectionSocket c)

> connect :: SockAddr -> ByteString -> ByteString -> [(ByteString, ByteString)] -> IO Connection
> connect address user password parameters = do
>   socket <- Socket.socket (family addr) Socket.Stream 0
>   connect socket address
>   setupSession socket user password parameters
>  where
>   family (SockAddrInet _ _)       = Socket.AF_INET
>   family (SockAddrInet6 _ _ _ _)  = Socket.AF_INET6
>   family (SockAddrUnix _)         = Socket.AF_UNIX

> setupSession :: Socket -> ByteString -> IO Connection
> setupSession socket password parameters = do
>   sendMessage socket (startupMessage parameters)
>   receiveBackendMessage $ authenticationResponseHandler {
>     onAuthenticationCleartextPassword = performCleartextPasswordAuthentication
>   }
>  where

>   authenticationResponseHandler = backendMessageHandler {
>     onAuthenticationOk        = configureSession,
>     onErrorResponse           = const abortSession,
>     onUnexpectedMessage       = const abortSession,
>     onCorruptedMessage        = abortSession
>    }

>   performCleartextPasswordAuthentication = sendMessage socket (passwordMessage password) >> completeAuthentication

>   completeAuthentication = receiveBackendMessage authenticationResponseHandler

>   configureSession = receiveBackendMessage $ backendMessageHandler {
>     onBackendKeyData          = ...,
>     onParameterStatus         = ...,
>     onReadyForQuery           = commenceSession,
>     onErrorResponse           = ...,
>     onNoticeResponse          = ...,
>     onUnexpectedMessage       = const abortSession,
>     onCorruptedMessage        = abortSession
>    }

>   commenceSession = do
>     statusVar <- newMVar True
>     receiverThreadId <- forkIO $ forever $ receiveBackendMessage $ backendMessageHandler {
>       onNoticeResponse          = error "TODO: invoke user-supplied notice response handler",
>       onNotificationResponse    = error "TODO: invoke user-supplied notification response handler",
>       onParameterStatus         = error "TODO: invoke user-supplied parameter status handler",
>       onUnexpectedMessage       = const abortConnectedSession,
>       onCorruptedMessage        = abortConnectedSession
>      }
>     return Connection {
>       connectionSocket = socket,
>       connectionStatusVar = statusVar,
>       connectionReceiverThreadId = receiverThreadId,
>      }

>   abortConnectedSession = error "TODO: ..."

>   abortSession = error "TODO: ..."


> data Notice = Notice {
>   noticeSeverity          :: ByteString,
>   noticeCode              :: ByteString,
>   noticeMessage           :: ByteString,
>   noticeDetail            :: Maybe ByteString,
>   noticeHint              :: Maybe ByteString,
>   noticePosition          :: Maybe Integer,
>   noticeInternalPosition  :: Maybe Integer,
>   noticeInternalQuery     :: Maybe 
>   noticeContext
>   noticeSchema
>   noticeTable
>   noticeColumn
>   noticeDataType
>   noticeConstraint
>   noticeFile
>   noticeLine
>   noticeRoutine
> }

> data ErrorSeverity = ERROR | FATAL | PANIC deriving (Eq, Ord, Show, Enum)

> makeError :: [(Char, ByteString)] -> Error
> makeError 

> -}
