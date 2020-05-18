-- | Module:    Database.PostgreSQL.Protocol.Internal.SendQueue
-- Description: A message channel suitable for use for database client send queue
-- Copyright:   © 2015-2020 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable

{-# OPTIONS_GHC -funbox-strict-fields #-}

module Database.PostgreSQL.Protocol.Internal.SendQueue (
  SendQueue,
  newSendQueue,
  writeSendQueue,
  readSendQueue,
  closeSendQueue,
) where

import Control.Concurrent.MVar
import Control.Exception

-- | A @SendQueue@ is an implementation of an unbounded message queue,
-- identical to that provided in the standard Haskell "Control.Concurrent.Chan"
-- module, with an additional support for a /closed/ state, in which the queue
-- atomically guarantees that no new messages will be written, so that any
-- messages lingering in a half-closed communication channel can be processed
-- gracefully without worrying about race conditions.
data SendQueue a = SendQueue {
  readVar  :: MVar (Stream a),
  writeVar :: MVar (Maybe (Stream a))
} deriving (Eq)

type Stream a = MVar (Item a)
data Item a = Item a (Stream a)

-- | Creates a new empty send queue.
newSendQueue :: IO (SendQueue a)
newSendQueue = do
  hole <- newEmptyMVar
  SendQueue <$> newMVar hole <*> newMVar (Just hole)

-- | If the specified send queue is open, appends a new value to the end of the
-- queue and returns 'True'. If the send queue is closed, the new value is
-- discarded and @writeSendQueue@ returns 'False'.
writeSendQueue :: SendQueue a -> a -> IO Bool
writeSendQueue q x = do
  new_hole <- newEmptyMVar
  mask_ $ do
    maybe_old_hole <- takeMVar (writeVar q)
    case maybe_old_hole of
      Just old_hole -> do
        putMVar old_hole (Item x new_hole)
        putMVar (writeVar q) (Just new_hole)
        return True
      Nothing -> do
        putMVar (writeVar q) Nothing
        return False

-- | Reads next unread value from the queue, blocking until one becomes
-- available. If the specified queue is closed and empty, @readSendQueue@ will
-- block indefinitely.
readSendQueue :: SendQueue a -> IO a
readSendQueue q = do
  modifyMVar (readVar q) $ \read_end -> do
    Item x new_read_end <- readMVar read_end
    return (new_read_end, x)

-- | If the specified send queue is open, atomically appends one final (sentinel)
-- value to the end of the queue and then closes the queue for all further
-- writes, returning 'True'. If the send queue is already closed, the sentinel
-- value is discarded and @closeSendQueue@ returns 'False'.
closeSendQueue :: SendQueue a -> a -> IO Bool
closeSendQueue q x = do
  new_hole <- newEmptyMVar -- this will never be filled
  mask_ $ do
    maybe_old_hole <- takeMVar (writeVar q)
    putMVar (writeVar q) Nothing
    case maybe_old_hole of
      Just old_hole -> do
        putMVar old_hole (Item x new_hole)
        return True
      Nothing -> return False

