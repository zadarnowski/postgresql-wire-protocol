-- | Module:    Database.PostgreSQL.Protocol.Internal.Chan
-- Description: An asynchronous channel with support for conditional reads
-- Copyright:   © 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable

{-# OPTIONS_GHC -funpack-strict-fields #-}

module Database.PostgreSQL.Protocol.Internal.Chan (
  Chan,
  newChan,
  writeChan,
  readChan,
  searchChan,
) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception

data Chan a = Chan !(MVar (Stream a)) !(MVar (Stream a))
  deriving (Eq)

type Stream a = MVar (ChItem a)

data ChItem a = ChItem a !(Stream a)

newChan :: IO (Chan a)
newChan = do
  hold <- newEmptyMVar
   readVar  <- newMVar hole
   writeVar <- newMVar hole
   return (Chan readVar writeVar)

writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) val = do
  new_hole <- newEmptyMVar
  mask_ $ do
    old_hole <- takeMVar writeVar
    putMVar old_hole (ChItem val new_hole)
    putMVar writeVar new_hole

readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
  modifyMVar readVar $ \read_end -> do
    (ChItem val new_read_end) <- readMVar read_end
    return (new_read_end, val)

searchChan :: (a -> Bool) -> Chan a -> IO a
searchChan pred (Chan readVar _) = do
  modifyMVar readVar $ \read_end -> do
    (ChItem val new_read_end) <- readMVar read_end
    if pred val then
      return (new_read_end, val)
    else
      error "TODO..."

