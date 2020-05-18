-- | Module:    Database.PostgreSQL.Protocol.Client
-- Description: High-level PostgreSQL database client
-- Copyright:   © 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable
--
-- This module defines a high-level synchronous implementation of a
-- non-blocking, thread-safe PostgreSQL database client. Asynchronous
-- access is expected to be simulated using Haskell lightweight threads.

{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Client (
  Client,
) where

import Control.Exception
import Data.ByteString (ByteString)
import Data.Data
import Data.Word

import Database.PostgreSQL.Protocol.Connection
import Database.PostgreSQL.Protocol.Internal.SendQueue

data Client = Client {
  -- | Underlying connection
  connection :: Connection,
  -- | Outbound message queue
  sendQueue :: SendQueue LazyByteString,
}

