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
  DatabaseException(..),
  throwErrorResponse,
  throwProtocolError,
  throwUnsupportedAuthenticationMethod,
) where

import Control.Exception
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Typeable

import Database.PostgreSQL.Protocol.Types

-- | The type of database-related exceptions
data DatabaseException =
  -- | An error condition reported by the database server. Details of the
  -- condition are specified by the list of 'NoticeFields'. Note that the
  -- 'displayException' method assumes that the server-supplied notice message
  -- is provided in the UTF-8 encoding.
  DatabaseErrorResponse NoticeFields |
  -- | A violation of the PostgreSQL frontend/backend protocol has been
  -- detected, such as an unexpected message type, truncated or otherwise
  -- corrupted message received from the database server.
  DatabaseProtocolError String |
  -- | Database client received an v'AuthenticationResponse' from the server
  -- that requests an unrecognised or unsupported authentication method.
  UnsupportedAuthenticationMethod AuthenticationResponse
  deriving (Eq, Show, Typeable)

instance Exception DatabaseException where
  displayException (DatabaseErrorResponse fs) =
    case [ x | (NOTICE_MESSAGE, x) <- fs ] of
      (msg:_) -> toString msg
      _ -> "Database Error"
  displayException (DatabaseProtocolError msg) = msg
  displayException (UnsupportedAuthenticationMethod t) =
    "Unsupported authentication method requested by server: " <> show t

-- | Raises a 'DatabaseErrorResponse' exception in the 'IO' monad.
throwErrorResponse :: NoticeFields -> IO a
throwErrorResponse = throwIO . DatabaseErrorResponse

-- | Raises a 'DatabaseProtocolError' exception in the 'IO' monad.
throwProtocolError :: String -> IO a
throwProtocolError = throwIO . DatabaseProtocolError

-- | Raises an 'UnsupportedAuthenticationMethod' exception in the 'IO' monad.
throwUnsupportedAuthenticationMethod :: AuthenticationResponse -> IO a
throwUnsupportedAuthenticationMethod = throwIO . UnsupportedAuthenticationMethod



