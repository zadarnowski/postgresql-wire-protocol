-- | Module:    Database.PostgreSQL.Protocol.Exception
-- Description: Exceptions
-- Copyright:   © 2015-2020 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable
--
-- Asynchronous exceptions issued by PostgreSQL backend.

module Database.PostgreSQL.Protocol.Exception (
  DatabaseException(..)
) where

import Control.Exception
import Data.Typeable
import Data.ByteString.Lazy.UTF8 (toString)

import Database.PostgreSQL.Protocol.Types

data DatabaseException =
  DatabaseErrorResponse NoticeFields |
  DatabaseProtocolError String
  deriving (Eq, Show, Typeable)

instance Exception DatabaseException where
  displayException (DatabaseErrorResponse fs) =
    case [ x | (NOTICE_MESSAGE, x) <- fs ] of
      (msg:_) -> toString msg
      _ -> "Database Error"
  displayException (DatabaseProtocolError msg) = msg
