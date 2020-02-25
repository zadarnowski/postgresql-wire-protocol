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

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.AuthenticationResponse' backend message.
pattern AUTHENTICATION_RESPONSE :: Word8
pattern AUTHENTICATION_RESPONSE = 0x52 -- 'R'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.BackendKeyData' backend message.
pattern BACKEND_KEY_DATA :: Word8
pattern BACKEND_KEY_DATA = 0x4B -- 'K'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.Bind' frontend message.
pattern BIND :: Word8
pattern BIND = 0x42 -- 'B'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.BindComplete' backend message.
pattern BIND_COMPLETE :: Word8
pattern BIND_COMPLETE = 0x32 -- '2'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.Close' frontend message.
pattern CLOSE :: Word8
pattern CLOSE = 0x43 -- 'C'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.CloseComplete' backend message.
pattern CLOSE_COMPLETE :: Word8
pattern CLOSE_COMPLETE = 0x33 -- '3'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.CommandComplete' backend message.
pattern COMMAND_COMPLETE :: Word8
pattern COMMAND_COMPLETE = 0x43 -- 'C'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.CopyInData' or 'Database.PostgreSQL.Protocol.Types.CopyOutData' message.
pattern COPY_DATA :: Word8
pattern COPY_DATA = 0x64 -- 'd'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.CopyInDone' or 'Database.PostgreSQL.Protocol.Types.CopyOutDone' message.
pattern COPY_DONE :: Word8
pattern COPY_DONE = 0x63 -- 'c'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.CopyFail' frontend message.
pattern COPY_FAIL :: Word8
pattern COPY_FAIL = 0x66 -- 'f'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.CopyInResponse' backend message.
pattern COPY_IN_RESPONSE :: Word8
pattern COPY_IN_RESPONSE = 0x47 -- 'G'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.CopyOutResponse' backend message.
pattern COPY_OUT_RESPONSE :: Word8
pattern COPY_OUT_RESPONSE = 0x48 -- 'H'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.CopyBothResponse' backend message.
pattern COPY_BOTH_RESPONSE :: Word8
pattern COPY_BOTH_RESPONSE = 0x57 -- 'W'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.DataRow' backend message.
pattern DATA_ROW :: Word8
pattern DATA_ROW = 0x44 -- 'D'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.Describe' backend message.
pattern DESCRIBE :: Word8
pattern DESCRIBE = 0x44 -- 'D'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.EmptyQueryResponse' backend message.
pattern EMPTY_QUERY_RESPONSE :: Word8
pattern EMPTY_QUERY_RESPONSE = 0x49 -- 'I'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.ErrorResponse' backend message.
pattern ERROR_RESPONSE :: Word8
pattern ERROR_RESPONSE = 0x45 -- 'E'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.Execute' frontend message.
pattern EXECUTE :: Word8
pattern EXECUTE = 0x45 -- 'E'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.Flush' frontend message.
pattern FLUSH :: Word8
pattern FLUSH = 0x48 -- 'H'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.FunctionCall' frontend message.
pattern FUNCTION_CALL :: Word8
pattern FUNCTION_CALL = 0x46 -- 'F'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.FunctionCallResponse' backend message.
pattern FUNCTION_CALL_RESPONSE :: Word8
pattern FUNCTION_CALL_RESPONSE = 0x56 -- 'V'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.GSSResponse' backend message.
pattern GSS_RESPONSE :: Word8
pattern GSS_RESPONSE = 0x70 -- 'p'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.NegotiateProtocolVersion' backend message.
pattern NEGOTIATE_PROTOCOL_VERSION :: Word8
pattern NEGOTIATE_PROTOCOL_VERSION = 0x76 -- 'v'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.NoData' backend message.
pattern NO_DATA :: Word8
pattern NO_DATA = 0x6E -- 'n'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.NoticeResponse' backend message.
pattern NOTICE_RESPONSE :: Word8
pattern NOTICE_RESPONSE = 0x4E -- 'N'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.NotificationResponse' backend message.
pattern NOTIFICATION_RESPONSE :: Word8
pattern NOTIFICATION_RESPONSE = 0x41 -- 'A'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.ParameterDescription' backend message.
pattern PARAMETER_DESCRIPTION :: Word8
pattern PARAMETER_DESCRIPTION = 0x74 -- 't'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.ParameterStatus' backend message.
pattern PARAMETER_STATUS :: Word8
pattern PARAMETER_STATUS = 0x53 -- 'S'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.Parse' frontend message.
pattern PARSE :: Word8
pattern PARSE = 0x50 -- 'P'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.ParseComplete' backend message.
pattern PARSE_COMPLETE :: Word8
pattern PARSE_COMPLETE = 0x31 -- '1'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.PasswordMessage' frontend message.
pattern PASSWORD_MESSAGE :: Word8
pattern PASSWORD_MESSAGE = 0x70 -- 'p'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.PortalSuspended' backend message.
pattern PORTAL_SUSPENDED :: Word8
pattern PORTAL_SUSPENDED = 0x73 -- 's'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.Query' frontend message.
pattern QUERY :: Word8
pattern QUERY = 0x51 -- 'Q'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.ReadyForQuery' backend message.
pattern READY_FOR_QUERY :: Word8
pattern READY_FOR_QUERY = 0x5A -- 'Z'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.RowDescription' backend message.
pattern ROW_DESCRIPTION :: Word8
pattern ROW_DESCRIPTION = 0x54 -- 'T'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.SASLInitialResponse' frontend message.
pattern SASL_INITIAL_RESPONSE :: Word8
pattern SASL_INITIAL_RESPONSE = 0x70 -- 'p'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.SASLResponse' frontend message.
pattern SASL_RESPONSE :: Word8
pattern SASL_RESPONSE = 0x70 -- 'p'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.Sync' frontend message.
pattern SYNC :: Word8
pattern SYNC = 0x53 -- 'S'

-- | Tag byte identifying an 'Database.PostgreSQL.Protocol.Types.Terminate' frontend message.
pattern TERMINATE :: Word8
pattern TERMINATE = 0x58 -- 'X'
