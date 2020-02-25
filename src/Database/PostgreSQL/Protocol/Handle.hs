-- | Module:    Database.PostgreSQL.Protocol.Handle
-- Description: I/O handle abstraction
-- Copyright:   © 2015-2020 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable
--
-- This module provides an abstraction of I/O handles for use in PostgreSQL
-- protocol sessions, abstracting over differences between plain OS handles
-- such as TCP\/IP or UNIX domain sockets, SSL\/TLS and GSSAPI-encrypted
-- communication sessions.

module Database.PostgreSQL.Protocol.Handle (
  Handle,
  newHandle, receiveBytes, sendBytes, closeHandle, duplicateHandle,
  openFileHandle
) where

import Database.PostgreSQL.Protocol.Types

import qualified Data.ByteString.Lazy as LazyByteString
import qualified System.IO as System

-- | A generic abstraction of a communication handle for use in PostgreSQL
-- protocol sessions, with predefined input ('receiveBytes'), output
-- ('sendBytes') and destruction ('closeHandle') operations.
data Handle = Handle {
  -- | Reads a lazy byte string of the specified length from the specified
  -- communication handle, blocking the caller until all of the required data
  -- becomes available.  Any returned values of length different from the one
  -- requested should be interpreted by the caller as unrecoverable I/O errors,
  -- although well-written handled should elaborate on the exact error
  -- condition by raising an appropriate 'Control.Exception.Exception'
  -- instead of returning an incomplete result string.
  receiveBytes :: Int -> IO LazyByteString,
  -- | Writes a lazy byte string to the specified communication handle,
  -- blocking until the operation has been completed and raising an I/O
  -- exception whenever some or all of the supplied data could not be written
  -- successfully.
  sendBytes :: LazyByteString -> IO (),
  -- | Closes the specified communication handle, releasing any underlying
  -- resources. Any subsequent operations on the handle, result in unspecified
  -- behaviour, preferably raising a meaningful I/O exception.
  closeHandle :: IO (),
  -- | Returns a handle that represents a new distinct connection to the same
  -- remote endpoint, as required to implement PostgreSQL query cancellation
  -- protocol.
  duplicateHandle :: IO Handle
}

-- | Creates a new communication handle with the specified 'receiveBytes',
-- 'sendBytes' and 'closeHandle' functions.
newHandle :: (Int -> IO LazyByteString) -> (LazyByteString -> IO ()) -> IO () -> IO Handle -> Handle
newHandle = Handle

-- | Creates a new communication handle from the specified named file,
-- which should presumably represent a named pipe or a UNIX domain socket.
openFileHandle :: FilePath -> IO Handle
openFileHandle path = do
  h <- System.openFile path System.ReadWriteMode
  return Handle {
    receiveBytes = LazyByteString.hGet h,
    sendBytes = LazyByteString.hPut h,
    closeHandle = System.hClose h,
    duplicateHandle = openFileHandle path
  }
