> -- | Module:      Database.PostgreSQL.Protocol.Builders
> -- | Description: Builders for PostgreSQL messages.
> -- | Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- | License:     BSD3
> -- | Maintainer:  pat@jantar.org
> -- | Stability:   experimental
> -- | Portability: portable

> -- | This module defines low-level builders for all defined PostgreSQL messages.
> -- | At this level of abstraction, all message fields are rendered directly as
> -- | binary data, with little or no marshalling into any more meaningful Haskell
> -- | types, in order to provided higher-level libraries with maximum possible
> -- | freedom of behaviour.

> {-# LANGUAGE OverloadedStrings #-}

> module Database.PostgreSQL.Protocol.Builders (
>   authenticationOkMessage,
>   authentication_kerberos_v5_message,
>   authentication_cleartext_password_message,
>   authentication_md5_password_message,
>   authentication_scm_credential_message,
>   authentication_gss_message,
>   authentication_sppi_message,
>   authentication_gss_continue_message,
>   backend_key_data_message,
>   bind_message,
>   bind_complete_message,
>   cancel_request_message,
>   close_statement_message,
>   close_portal_message,
>   close_complete_message,
>   command_complete_message,
>   lazy_command_complete_message,
>   insert_command_complete_message,
>   delete_command_complete_message,
>   update_command_complete_message,
>   select_command_complete_message,
>   move_command_complete_message,
>   fetch_command_complete_message,
>   copy_command_complete_message,
>   copy_data_message,
>   copy_lazy_data_message,
>   copy_complete_message,
>   copy_fail_message,
>   copy_in_response_message,
>   copy_out_response_message,
>   copy_both_response_message,
>   data_row_message,
>   describe_statement_message,
>   describe_portal_message,
>   empty_query_response_message,
>   error_response_message,
>   execute_message,
>   flush_message,
>   function_call_message,
>   function_call_response_message,
>   no_data_message,
>   notice_response_message,
>   notification_response_message,
>   parameter_description_message,
>   parameter_status_message,
>   parse_message,
>   parse_complete_message,
>   password_message,
>   portal_suspended_message,
>   query_message,
>   ready_for_query_message,
>   row_description_message,
>   ssl_request_message,
>   startup_message,
>   sync_message,
>   terminate_message,
> ) where

> import Data.ByteString (ByteString)
> import Data.ByteString.Builder
> import Data.Monoid

> import qualified Data.ByteString as ByteString
> import qualified Data.ByteString.Lazy as Lazy

> -- | Sent by backend to indicate succcessful authentication.

> authenticationOkMessage :: Builder
> authenticationOkMessage = char8 'R' <> int32 8 <> int32 0

> -- | Sent by backend to request Kerberos V5 authentication.

> authentication_kerberos_v5_message :: Builder
> authentication_kerberos_v5_message = char8 'R' <> int32 8 <> int32 2

> -- | Sent by backend to request clear-text password authentication.

> authentication_cleartext_password_message :: Builder
> authentication_cleartext_password_message = char8 'R' <> int32 8 <> int32 3

> -- | Sent by backend to request MD5-encrypted password authentication.
> -- | using the specified 32-bit salt.

> authentication_md5_password_message :: Word32 -> Builder
> authentication_md5_password_message salt = char8 'R' <> int32 12 <> int32 5 <> word32 salt

> -- | Sent by backend to request authentication with SCM credentials.

> authentication_scm_credential_message :: Builder
> authentication_scm_credential_message = char8 'R' <> int32 8 <> int32 6

> -- | Sent by backend to request GSSAPI authentication.

> authentication_gss_message :: Builder
> authentication_gss_message = char8 'R' <> int32 8 <> int32 7

> -- | Sent by backend to request SPPI authentication.

> authentication_sppi_message :: Builder
> authentication_sspi_message = char8 'R' <> int32 8 <> int32 9

> -- | Sent by backend to provide GSSAPI or SSPI authentication data.

  Assumptions: 8 + ByteString.length d < 2^31

> authentication_gss_continue_message :: ByteString -> Builder
> authentication_gss_continue_message d = char8 'R' <> int32 (8 + size_of_bytes d) <> int32 8 <> byteString d

> -- | Sent by backend to supply query cancellation data to the frontend.
> -- | The frontend must save these values if it wishes to be able to issue 'cancel_request_message' messages later.

> backend_key_data_message :: Word32 -> Word32 -> Builder
> backend_key_data_message pid secret = char8 'B' <> int32 12 <> word32 pid <> word32 secret

> -- | Sent by frontend to ...

  Assumptions:  numElements formats `elem` [ 0, 1, numElements params ]
                0 <= numElements params < 2^15
                0 <= numElements nresults < 2^15
                forall p `elem` elems params, fromMaybe (-1) (ByteString.length <$> p) < 2^31

> bind_message :: ByteString -> ByteString -> UArray Int16 Word16 -> Array Int16 (Maybe ByteString) -> UArray Int16 Word16 -> Builder
> bind_message portal_name statement_name parameter_formats parameters result_formats =
>   char8 'B' <>
>   int32 (4 + size_of_string portal_name +
>              size_of_string statement_name +
>              size_of_uarray 2 parameter_formats +
>              size_of_array size_of_nullable_value parameters +
>              size_of_uarray 2 result_formats) <>
>   string portal_name <>
>   string statement_name <>
>   uarray word16 parameter_formats <>
>   array nullable_value parameters <>
>   uarray word16 result_formats

> -- | Sent by backend to indicate successful completion of a bind operation requested by a 'bind_message'.

> bind_complete_message :: Builder
> bind_complete_message = char8 '2' <> int32 4

> -- | Sent by frontend to request cancellation of an earlier query operation.

> cancel_request_message :: Word32 -> Word32 -> Builder
> cancel_request_message pid secret = int32 16 <> word32 80877102 <> word32 pid <> word32 secret

> -- | Sent by frontend to close a prepared statement.

> close_statement_message :: ByteString -> Builder
> close_statement_message statement_name = char8 'C' <> int32 (5 + size_of_string statement_name) <> char8 'S' <> string statement_name

> -- | Sent by frontend to close a portal.

> close_portal_message :: ByteString -> Builder
> close_portal_message portal_name = char8 'C' <> int32 (5 + size_of_string portal_name) <> char8 'P' <> string portal_name

> -- | Sent by backend to indicate successful completion of a close operation requested by a 'close_statement_message' or 'close_portal_message'.

> close_complete_message :: Builder
> close_complete_message = char8 '3' <> int32 4

> -- | Sent by backend to indicate succesful completion of an SQL command.

> command_complete_message :: ByteString -> Builder
> command_complete_message tag = char8 'C' <> int32 (4 + size_of_string tag) <> string tag

> -- | Sent by backend to indicate succesful completion of an SQL command.

> lazy_command_complete_message :: Lazy.ByteString -> Builder
> lazy_command_complete_message tag = char8 'C' <> int32 (4 + size_of_lazy_string tag) <> string tag

> -- | Sent by backend to indicate succesful completion of an SQL INSERT command.

> insert_command_complete_message :: Integer -> Integer -> Builder
> insert_command_complete_message oid rows = lazy_command_complete_message (toLazyByteString (byteString "INSERT " <> integerDec oid <> char8 ' ' <> integerDec rows))

> -- | Sent by backend to indicate succesful completion of an SQL DELETE command.

> delete_command_complete_message :: Integer -> Builder
> delete_command_complete_message rows = lazy_command_complete_message (toLazyByteString (byteString "DELETE " <> integerDec rows))

> -- | Sent by backend to indicate succesful completion of an SQL UPDATE command.

> update_command_complete_message :: Integer -> Builder
> update_command_complete_message rows = lazy_command_complete_message (toLazyByteString (byteString "UPDATE " <> integerDec rows))

> -- | Sent by backend to indicate succesful completion of an SQL SELECT or CREATE TABLE AS command.

> select_command_complete_message :: Integer -> Builder
> select_command_complete_message rows = lazy_command_complete_message (toLazyByteString (byteString "SELECT " <> integerDec rows))

> -- | Sent by backend to indicate succesful completion of an SQL MOVE command.

> move_command_complete_message :: Integer -> Builder
> move_command_complete_message rows = lazy_command_complete_message (toLazyByteString (byteString "MOVE " <> integerDec rows))

> -- | Sent by backend to indicate succesful completion of an SQL FETCH command.

> fetch_command_complete_message :: Integer -> Builder
> fetch_command_complete_message rows = lazy_command_complete_message (toLazyByteString (byteString "FETCH " <> integerDec rows))

> -- | Sent by backend to indicate succesful completion of an SQL COPY command.
> -- | (Note: the row count appears only in PostgreSQL 8.2 and later.)

> copy_command_complete_message :: Integer -> Builder
> copy_command_complete_message rows = lazy_command_complete_message (toLazyByteString (byteString "COPY " <> integerDec rows))

> -- | Sent by either frontend or backend to exchange bulk data.

> copy_data_message :: ByteString -> Builder
> copy_data_message d = char8 'd' <> int32 (4 + size_of_bytes d) <> byteString d

> -- | Sent by either frontend or backend to exchange bulk data rendered as a lazy byte string.

> copy_lazy_data_message :: Lazy.ByteString -> Builder
> copy_lazy_data_message d = char8 'd' <> int32 (4 + size_of_lazy_bytes d) <> lazyByteString d

> -- | Sent by either frontend or backend to indicate successful completion of a COPY operation.

> copy_complete_message :: Builder
> copy_complete_message = char8 'c' <> int32 4

> -- | Sent by either frontend or backend to indicate failed completion of a COPY operation.

> copy_fail_message :: ByteString -> Builder
> copy_fail_message msg = char8 'f' <> int32 (5 + size_of_string msg) <> string msg

> -- | Sent by backend to ...

> copy_in_response_message :: Word8 -> UArray Int16 Word16 -> Builder
> copy_in_response_message format column_formats = char8 'G' <> int32 (5 + size_of_uarray 2 column_formats) <> word8 format <> uarray word16 column_formats

> -- | Sent by backend to ...

> copy_out_response_message :: Word8 -> UArray Int16 Word16 -> Builder
> copy_out_response_message format column_formats = char8 'H' <> int32 (5 + size_of_uarray 2 column_formats) <> word8 format <> uarray word16 column_formats

> -- | Sent by backend to ...

> copy_both_response_message :: Word8 -> UArray Int16 Word16 -> Builder
> copy_both_response_message format column_formats = char8 'W' <> int32 (5 + size_of_uarray 2 column_formats) <> word8 format <> uarray word16 column_formats

> -- | Sent by backend to ...

> data_row_message :: Array Int16 (Maybe ByteString) -> Builder
> data_row_message columns = char8 'D' <> int32 (size_of_array nullable_value columns) <> array nullable_value columns

> -- | Sent by frontend to ...

> describe_statement_message :: Builder
> describe_statement_message statement_name = char8 'D' <> int32 (5 + string_length statement_name) <> char8 'S' <> string statement_name

> -- | Sent by frontend to ...

> describe_portal_message :: Builder
> describe_portal_message portal_name = char8 'D' <> int32 (5 + string_length portal_name) <> char8 'P' <> string portal_name

> -- | Sent by backend to ...

> empty_query_response_message :: Builder
> empty_query_response_message = char8 'I' <> int32 4

> -- | Sent by backend to ...

> error_response_message :: [ByteString] -> Builder
> error_response_message fields = char8 'E' <> int32 (5 + sum (map size_of_string fields)) <> mconcat (map string fields) <> int8 0

> -- | Sent by frontend to execute a portal.

> execute_message :: ByteString -> Int32 -> Builder
> execute_message portal_name limit = char8 'E' <> int32 (8 + size_of_string portal_name) <> string portal_name <> int32 limit

> -- | Sent by frontend to flush backend's response buffer.

> flush_message :: Builder
> flush_message = char8 'H' <> int32 4

> -- | Sent by frontend

> function_call_message :: Int32 -> UArray Int16 Word16 -> Array Int16 (Maybe ByteString) -> Word16 -> Builder
> function_call_message oid argument_formats arguments result_format =
>   char8 'F' <> int32 (10 + size_of_uarray 2 argument_formats + size_of_array size_of_nullable_value arguments) <>
>   int32 oid <>
>   uarray word16 argument_formats <>
>   array nullable_value arguments <>
>   int16 result_format

> -- | Sent by backend

> function_call_response_message :: Maybe ByteString -> Builder
> function_call_response_message result = char8 'V' <> int32 (4 + size_of_nullable_value result) <> nullable_value result

> -- | Sent by backend

> no_data_message :: Builder
> no_data_message = char8 'n' <> int32 4

> -- | Sent by backend

> notice_response_message :: [ByteString] -> Builder
> notice_response_message fields = char8 'N' <> int32 (5 + sum (map size_of_string fields)) <> mconcat (map string fields) <> int8 0

> -- | Sent by backend

> notification_response_message :: Word32 -> ByteString -> ByteString -> Builder
> notification_response_message pid channel_name payload =
>   char8 'A' <> int32 (8 + size_of_string channel_name + size_of_string payload) <> word32 pid <> string channel_name <> string payload

> -- | Sent by backend

> parameter_description_message :: UArray Int16 Int32 -> Builder
> parameter_description_message parameter_type_oids = char8 't' <> int32 (4 + size_of_uarray 4 parameter_type_oids) <> uarray int32 parameter_type_oids

> -- | Sent by backend

> parameter_status_message :: ByteString -> ByteString -> Builder
> parameter_status_message parameter_name parameter_value =
>   char8 'S' <> int32 (4 + size_of_string parameter_name + size_of_string parameter_value) <> string parameter_name <> string parameter_value

> -- | Sent by frontend

> parse_message :: ByteString -> ByteString -> UArray Int16 Int32 -> Builder
> parse_message statement_name query parameter_type_oids =
>   char8 'P' <> int32 (4 + size_of_string statement_name + size_of_string query + size_of_uarray 4 parameter_type_oids) <>
>   string statement_name <>
>   string query <>
>   uarray int32 parameter_type_oids

> -- | Sent by backend

> parse_complete_message :: Builder
> parse_complete_message = char8 '1' <> int32 4

> -- | Sent by frontend

> password_message :: ByteString -> Builder
> password_message password = char8 'p' <> int32 (4 + size_of_string password) <> string password

> -- | Sent by backend to indicate that a portal has been suspended after reaching the row count limit specified in the corresponding 'execute_message'.

> portal_suspended_message :: Builder
> portal_suspended_message = char8 's' <> int32 4

> -- | Sent by frontend

> query_message :: ByteString -> Builder
> query_message query = char8 'Q' <> int32 (4 + size_of_string query) <> string query

> -- | Sent by backend

> ready_for_query_message :: Char -> Builder
> ready_for_query_message status = char8 'Z' <> int32 5 <> char8 status

> -- | Sent by backend

> row_description_message :: Builder
> row_description_message =
>   char8 'T' <> int32 () <> ...

> -- | Sent by frontend

> ssl_request_message :: Builder
> ssl_request_message = int32 8 <> int32 80877103

> -- | Sent by frontend

> startup_message :: [(ByteString, ByteString)] -> Builder
> startup_message parameters =
>   int32 (9 + sum (map size_of_parameter parameters)) <> int32 196608 <> mconcat parameter parameters <> word8 0
>  where
>   size_of_parameter (name, value) = size_of_string name + size_of_string value
>   parameter = string name <> string value

> -- | Sent by frontend

> sync_message :: Builder
> sync_message = char8 'S' <> int32 4

> -- | Sent by frontend

> terminate_message :: Builder
> terminate_message = char8 'X' <> int32 4

  Helpers:

> nullable_value :: Maybe ByteString -> Builder
> nullable_value = maybe (int32 (-1)) value

> value :: ByteString -> Builder
> value x = int32 (fromIntegral (ByteString.length x)) <> byteString x

> size_of_nullable_value :: Maybe ByteString -> Int32
> size_of_nullable_value = maybe 4 size_of_value

> size_of_value :: ByteString -> Int32
> size_of_value x = 4 + fromIntegral (ByteString.length x)

> string :: ByteString -> Builder
> string x = byteString x <> char8 '\0'

> size_of_string :: ByteString -> Int32
> size_of_string x = fromIntegral (ByteString.length x) + 1

> array :: (e -> Builder) -> Array Int16 e -> Builder
> array b a = int16 (fromIntegral (numElements a)) <> mconcat (map b (elems xs))

> size_of_array :: (e -> Int32) -> Array Int16 e -> Int32
> size_of_array sb a = 2 + sum (map sb (elems a))

> uarray :: (e -> Builder) -> UArray Int16 e -> Builder
> uarray b a = int16 (fromIntegral (numElements a)) <> mconcat (map b (elems xs))

> size_of_uarray :: Int32 -> UArray Int16 e -> Int32
> size_of_uarray se a = 2 + se * fromIntegral (numElements a)
