-- | Module:    Database.PostgreSQL.Protocol.Frontend
-- Description: Generic asynchronous PostgreSQL frontend structure
-- Copyright:   © 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable
--
-- This module defines a primitive low-level structure of a generic
-- PostgreSQL frontend capable of asynchronous message transmission
-- and reception.

module Database.PostgreSQL.Protocol.Frontend (
  Frontend,

  -- * Asynchronous Notifications
  getNotification, getNotificationIf,
  getSomeNotification, getSomeNotificationIf,
  getNotice, getNoticeIf,
  getParameterChange, getParameterChangeIf,
  getSomeParameterChange, getSomeParameterChangeIf,
) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
-- import Control.Exception
-- import Network.Socket (Socket, SockAddr)

-- import qualified Network.Socket as Socket

data Frontend = FRONTEND {
  -- Asynchronous queues:
  frontendNotificationResponseQueue :: Chan (ProcessID, ByteString, LazyByteString),
  frontendParameterStatusQueue :: Chan (ByteString, LazyByteString),
  frontendNoticeResponseQueue :: Chan NoticeFields,



  frontendSendQueue  :: Chan FrontendMessage,
  frontendRecvQueue  :: Chan BackendMessage,
  frontendProcQueue  :: Chan BackendMessageHandler,
  frontendSendThread :: ThreadId,
  frontendRecvThread :: ThreadId,
  frontendProcThread :: ThreadId,

  sendData :: LazyByteString -> IO (),
  recvData :: Int32 -> IO LazyByteString
}

newtype BackendMessageHandler :: Consume (BackendMessage -> IO BackendMessageHandler) | Pass

  TODO: handle exceptions!

createFrontend :: (Int32 -> IO LazyByteString) -> (LazyByteString -> IO ()) -> IO Frontend
createFrontend readData writeData = do
  sendQueue <- newChan
  recvQueue <- newChan
  procQueue <- newChan
  sendThread <- forkIO $ forever $ readChan sendQueue >>= writeData . toLazyByteString . frontendMessage
  recvThread <- forkIO $ forever $ readBackendMessage readData >>= writeChan recvQueue
  procThread <- forkIO $ let getNextMessage h = readChan recvQueue >>= apply h
                             apply (Consume h) m = h m >>= getNextMessage
                             apply (Pass) m = readChan procQueue >>= flip apply m
                          in getNextMessage Pass
  return FrontendSession {
    frontendSendQueue  = sendQueue,
    frontendRecvQueue  = recvQueue,
    frontendProcQueue  = procQueue,
    frontendSendThread = sendThread,
    frontendRecvThread = recvThread
   }

putMessage :: Frontend -> FrontendMessage -> IO ()
putMessage fe = sendBuilder fe . Builder.frontendMessage

sendBuilder :: Frontend -> Builder -> IO ()
sendBuilder fe msg = do
  writeChan (frontendSendQueue fe) $! toLazyByteString msg


getMessage :: Frontend -> IO BackendMessage
getMessage fe = do
  




getNotification :: ByteString -> Frontend -> IO (ProcessID, ByteString, LazyByteString)
getNotification channel = getNotificationIf (== channel)

getNotificationIf :: (ProcessIDByteString -> Bool) -> Frontend -> IO (ProcessID, ByteString, LazyByteString)
getNotificationIf pred = getSomeNotificationIf $ \ _ c _ -> pred c

getSomeNotification :: Frontend -> IO (ByteString, LazyByteString)
getSomeNotification = getSomeNotificationIf $ \ _ _ -> True

getSomeNotificationIf :: (ProcessID -> ByteString -> LazyByteString -> Bool) ->
  Frontend -> IO (ProcessID, ByteString, LazyByteString)
getSomeNotificationIf _pred _fe = fail "TODO"

getNotice :: Frontend -> IO NoticeFields
getNotice = getNoticeIf (const True)

getNoticeIf :: (NoticeFields -> Bool) -> Frontend -> IO NoticeFields
getNoticeIf _pred _fe = fail "TODO"

getParameterChange :: ByteString -> Frontend -> IO (ByteString, LazyByteString)
getParameterChange param = getParameterChangeIf (== param)

getParameterChangeIf :: (ByteString -> Bool) -> Frontend -> IO NoticeFields
getParameterChangeIf pred = getSomeParameterChangeIf $ \ p _ -> pred p

getSomeParameterChange :: Frontend -> IO (ByteString, LazyByteString)
getSomeParameterChange = getSomeParameterChangeIf $ \ _ _ -> True

getSomeParameterChangeIf :: (ByteString -> LazyByteString -> Bool) -> Frontend -> IO NoticeFields
getSomeParameterChangeIf _pred _fe = fail "TODO"
