-- | Module:    Database.PostgreSQL.Protocol.Tags
-- Description: Tag byte values of PostgreSQL protocol messages
-- Copyright:   © 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable

{-# LANGUAGE PatternSynonyms #-}

module Database.PostgreSQL.Protocol.Tags where

import Data.Word

pattern AUTHENTICATION_RESPONSE :: Word8
pattern AUTHENTICATION_RESPONSE = 0x52 -- 'R'

pattern BACKEND_KEY_DATA :: Word8
pattern BACKEND_KEY_DATA = 0x4B -- 'K'

pattern BIND :: Word8
pattern BIND = 0x42 -- 'B'

pattern BIND_COMPLETE :: Word8
pattern BIND_COMPLETE = 0x32 -- '2'

pattern CLOSE :: Word8
pattern CLOSE = 0x43 -- 'C'

pattern CLOSE_COMPLETE :: Word8
pattern CLOSE_COMPLETE = 0x33 -- '3'

pattern COMMAND_COMPLETE :: Word8
pattern COMMAND_COMPLETE = 0x43 -- 'C'

pattern COPY_DATA :: Word8
pattern COPY_DATA = 0x64 -- 'd'

pattern COPY_DONE :: Word8
pattern COPY_DONE = 0x63 -- 'c'

pattern COPY_FAIL :: Word8
pattern COPY_FAIL = 0x66 -- 'f'

pattern COPY_IN_RESPONSE :: Word8
pattern COPY_IN_RESPONSE = 0x47 -- 'G'

pattern COPY_OUT_RESPONSE :: Word8
pattern COPY_OUT_RESPONSE = 0x48 -- 'H'

pattern COPY_BOTH_RESPONSE :: Word8
pattern COPY_BOTH_RESPONSE = 0x57 -- 'W'

pattern DATA_ROW :: Word8
pattern DATA_ROW = 0x44 -- 'D'

pattern DESCRIBE :: Word8
pattern DESCRIBE = 0x44 -- 'D'

pattern EMPTY_QUERY_RESPONSE :: Word8
pattern EMPTY_QUERY_RESPONSE = 0x49 -- 'I'

pattern ERROR_RESPONSE :: Word8
pattern ERROR_RESPONSE = 0x45 -- 'E'

pattern EXECUTE :: Word8
pattern EXECUTE = 0x45 -- 'E'

pattern FLUSH :: Word8
pattern FLUSH = 0x48 -- 'H'

pattern FUNCTION_CALL :: Word8
pattern FUNCTION_CALL = 0x46 -- 'F'

pattern FUNCTION_CALL_RESPONSE :: Word8
pattern FUNCTION_CALL_RESPONSE = 0x56 -- 'V'

pattern GSS_RESPONSE :: Word8
pattern GSS_RESPONSE = 0x70 -- 'p'

pattern NEGOTIATE_PROTOCOL_VERSION :: Word8
pattern NEGOTIATE_PROTOCOL_VERSION = 0x76 -- 'v'

pattern NO_DATA :: Word8
pattern NO_DATA = 0x6E -- 'n'

pattern NOTICE_RESPONSE :: Word8
pattern NOTICE_RESPONSE = 0x4E -- 'N'

pattern NOTIFICATION_RESPONSE :: Word8
pattern NOTIFICATION_RESPONSE = 0x41 -- 'A'

pattern PARAMETER_DESCRIPTION :: Word8
pattern PARAMETER_DESCRIPTION = 0x74 -- 't'

pattern PARAMETER_STATUS :: Word8
pattern PARAMETER_STATUS = 0x53 -- 'S'

pattern PARSE :: Word8
pattern PARSE = 0x50 -- 'P'

pattern PARSE_COMPLETE :: Word8
pattern PARSE_COMPLETE = 0x31 -- '1'

pattern PASSWORD_MESSAGE :: Word8
pattern PASSWORD_MESSAGE = 0x70 -- 'p'

pattern PORTAL_SUSPENDED :: Word8
pattern PORTAL_SUSPENDED = 0x73 -- 's'

pattern QUERY :: Word8
pattern QUERY = 0x51 -- 'Q'

pattern READY_FOR_QUERY :: Word8
pattern READY_FOR_QUERY = 0x5A -- 'Z'

pattern ROW_DESCRIPTION :: Word8
pattern ROW_DESCRIPTION = 0x54 -- 'T'

pattern SASL_INITIAL_RESPONSE :: Word8
pattern SASL_INITIAL_RESPONSE = 0x70 -- 'p'

pattern SASL_RESPONSE :: Word8
pattern SASL_RESPONSE = 0x70 -- 'p'

pattern SYNC :: Word8
pattern SYNC = 0x53 -- 'S'

pattern TERMINATE :: Word8
pattern TERMINATE = 0x58 -- 'X'
