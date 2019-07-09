-- | Module:    Database.PostgreSQL.Protocol.Version
-- Description: Current protocol version information
-- Copyright:   (c) 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  pat@jantar.org
-- Stability:   experimental
-- Portability: portable
--
-- This module specifies the exact protocol version supported by this library, to be
-- used in the session startup protocol. The current version 3.0 has been introduced
-- in PostgreSQL 7.4 and is the only version supported by this library. The previous
-- version 2.0, introduced in PostgreSQL 6.4, is incompatible with version 3.0, and
-- I was unable to find documentation for the original version 1.0 of the protocol.

module Database.PostgreSQL.Protocol.Version where

import Data.Word

-- | (@3@) Current major version of the PostgreSQL wire protocol. Version numbers
--   1234 and higher are reserved for special connection headers such as
--   'Database.PostgreSQL.Protocol.Types.CancelRequest' and
--   'Database.PostgreSQL.Protocol.Types.SSLRequest', so that actual major version
--   numbers in 'Database.PostgreSQL.Protocol.Types.StartupMessage' must always
--   be equal to 1233 or lower.
currentMajorVersion :: Word16
currentMajorVersion = 3

-- | (@0@) Current minor version of the PostgreSQL wire protocol,
--   set to 0 in all versions of the PostgreSQL frontend/backend
--   protocol released so far.
currentMinorVersion :: Word16
currentMinorVersion = 0
