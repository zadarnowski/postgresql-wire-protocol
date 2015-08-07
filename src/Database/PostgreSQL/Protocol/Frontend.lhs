> -- | Module:      Database.PostgreSQL.Protocol.Frontend
> -- | Description: Generic asynchronous PostgreSQL frontend structure
> -- | Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- | License:     BSD3
> -- | Maintainer:  pat@jantar.org
> -- | Stability:   experimental
> -- | Portability: portable
> --
> -- This module defines a primitive low-level structure of a generic
> -- PostgreSQL frontend capable of asynchronous message transmission
> -- and reception.
> --
> -- Each @FrontendSession@ consists of a pair of FIFO queues (concurrent channels)
> -- used for sending messages to the backend and receiving messages back.
> -- Further, two threads are created to facilitate asynchronous servicing
> -- of these queues. All higher-level abstractions (such as performing an
> -- actual query using PostgreSQL simple or extended query protocol)
> -- are to be implemented separately on top of the 

> module Database.PostgreSQL.Protocol.Client.Connection (
>   Frontend
> ) where

> import Control.Concurrent.Chan
> import Control.Concurrent.MVar
> -- import Control.Exception
> -- import Network.Socket (Socket, SockAddr)

> -- import qualified Network.Socket as Socket

> data FrontendSession = FrontendSession {
>   frontendSendQueue  :: Chan FrontendMessage,
>   frontendRecvQueue  :: Chan BackendMessage,
>   frontendProcQueue  :: Chan BackendMessageHandler,
>   frontendSendThread :: ThreadId,
>   frontendRecvThread :: ThreadId,
>   frontendProcThread :: ThreadId
> }

> newtype BackendMessageHandler :: Consume (BackendMessage -> IO BackendMessageHandler) | Pass

  TODO: handle exceptions!

> beginFrontendSession :: (Int32 -> IO Lazy.ByteString) -> (Lazy.ByteString -> IO ()) -> IO FrontendSession
> beginFrontendSession readData writeData = do
>   sendQueue <- newChan
>   recvQueue <- newChan
>   procQueue <- newChan
>   sendThread <- forkIO $ forever $ readChan sendQueue >>= writeData . toLazyByteString . frontendMessage
>   recvThread <- forkIO $ forever $ readBackendMessage readData >>= writeChan recvQueue
>   procThread <- forkIO $ let getNextMessage h = readChan recvQueue >>= apply h
>                              apply (Consume h) m = h m >>= getNextMessage
>                              apply (Pass) m = readChan procQueue >>= flip apply m
>                           in getNextMessage Pass
>   return FrontendSession {
>     frontendSendQueue  = sendQueue,
>     frontendRecvQueue  = recvQueue,
>     frontendProcQueue  = procQueue,
>     frontendSendThread = sendThread,
>     frontendRecvThread = recvThread
>    }
