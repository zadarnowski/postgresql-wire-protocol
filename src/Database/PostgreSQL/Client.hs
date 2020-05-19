-- | Module:    Database.PostgreSQL.Client
-- Description: Asynchronous database client
-- Copyright:   © 2015-2020 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable
-- Portability: portable
--
-- This module defines a high-level synchronous implementation of a
-- non-blocking, thread-safe PostgreSQL database client. Asynchronous
-- access is expected to be simulated using Haskell lightweight threads.

module Database.PostgreSQL.Client (
  Client
) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Function

import Database.PostgreSQL.Protocol.Connection
import Database.PostgreSQL.Protocol.Internal.SendQueue
import Database.PostgreSQL.Protocol.Types

data Client = Client {
  clientConnection :: Connection,
  clientRequestQueue :: SendQueue Request,
  clientRequestThreadResult :: MVar (Maybe SomeException),
  clientResponseQueue :: Chan Response,
  clientResponseThreadResult :: MVar (Maybe SomeException)
}

type Request = IO () -> IO ()
type Response = IO () -> IO ()

-- Starts a new client from an established, authenticated connection.

start :: Connection -> IO Client
start conn = do

  -- Prepare the response handler thread:
  response_queue <- newChan
  response_thread_result <- newEmptyMVar
  mask_ $ forkIOWithUnmask $ \unmask -> do
    r <- try $ unmask $ fix $ \cont -> do
      h <- readChan response_queue
      h cont
    putMVar response_thread_result $ either Just (const Nothing) r

  -- Prepare the request queue and thread:
  request_queue <- newSendQueue
  request_thread_result <- newEmptyMVar
  mask_ $ forkIOWithUnmask $ \unmask -> do
    r <- try $ unmask $ fix $ \cont -> do
      h <- readSendQueue request_queue
      h cont
    putMVar request_thread_result $ either Just (const Nothing) r

  -- All done:
  return Client {
    clientConnection = conn,
    clientRequestQueue = request_queue,
    clientRequestThreadResult = request_thread_result,
    clientResponseQueue = response_queue,
    clientResponseThreadResult = response_thread_result
  }

submitRequest :: Client -> LazyByteString -> Response -> IO ()
submitRequest client msg response = do
  ok <- writeSendQueue (clientRequestQueue client) req
  unless ok $ throwIO (userError "Connection closed")
 where
  req next = sendMessage (clientConnection client) msg >> next

-- TODO: connect = error "TODO"
-- TODO: close = error "TODO"

{-
close :: Client -> IO ()
close client = do
  ok <- closeSendQueue (clientRequestQueue client) $ const $ do
    writeChan (clientResponseQueue client) $ const $ do
      return () -- TODO...
  when ok $ do
    r <- readMVar 
    return () -- TODO...
-}

-- | Requests cancellation of any query currently being executed by the
-- connected backend process. For security reasons, PostgreSQL protocol makes
-- no guarantees that the operation will succeed and provides no way of
-- knowing if it did.  If the query execution is, in fact, terminated
-- prematurely, the backend will issue an error with SQLSTATE code of 57014
-- (@query_canceled@), which should be handled by the user like any other
-- recoverable SQL error. In particular, the client is never disconnected
-- from the backend as a result of query cancellation.
cancel :: Client -> IO ()
cancel = sendCancelRequest . clientConnection

-- runScript = error "TODO"

-- prepare = error "TODO"

-- bind = error "TODO"

-- execute = error "TODO"

-- call = error "TODO"

-- copyIn = error "TODO"

-- copyOut = error "TODO"

-- begin = error "TODO"

-- commit = error "TODO"

-- rollback = error "TODO"
