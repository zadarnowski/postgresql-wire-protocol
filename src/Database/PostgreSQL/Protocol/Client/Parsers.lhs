> module Database.FunPQ.Protocol.Frontend (
> ) where

> import Network.Socket.ByteString.Lazy

> max_msg_size :: Word32
> max_msg_size = 64 * 1024 * 1024 -- refuse to receive messages greater than 64MiB, to avoid denial of service

> data Frontend a = Frontend {
>   authentication_ok
>   authentication_kerberos_v5
>   authentication_cleartext_password
>   authentication_md5_password
>   authentication_scm_credential
>   authentication_gss
>   authentication_sppi
>   authentication_gss_continue
>   backend_key_data
>   -- bind
>   bind_complete
>   -- cancel_request
>   -- close
>   close_complete
>   command_complete
>   copy_data -- both
>   copy_done -- both
>   -- copy_fail
>   copy_in_response
>   copy_out_response
>   copy_both_response
>   data_row
>   invalid_message
> }

> get_message_handler :: Frontend a -> Char -> Int -> Get a

> get_message_handler frontend 'R' sz = getWord32be >=> get_authentication_message_handler
>  where
>   get_authentication_message_handler 0 = do return (authentication_ok frontend)
>   get_authentication_message_handler 2 = do return (authentication_kerberos frontend)
>   get_authentication_message_handler 3 = do return (authentication_cleartext_password frontend)
>   get_authentication_message_handler 5 = do salt <- getWord32be
>                                             return (authentication_md5_password frontend salt)
>   get_authentication_message_handler 6 = do return (authentication_scm_credential frontend)
>   get_authentication_message_handler 7 = do return (authentication_gss frontend)
>   get_authentication_message_handler 8 = do payload <- getBytes (sz - 8)
>                                             return (authentication_gss_continue frontend payload)
>   get_authentication_message_handler 9 = do return (authentication_sspi frontend)
>   get_authentication_message_handler _ = do fail ("unrecognized authentication response type: " ++ show t)

> get_message_handler frontend 'K' sz = do pid <- getWord32be
>                                          secret <- getWord32be
>                                          return (backend_key_data frontend pid secret)

> get_message_handler frontend '2' sz = do return (bind_complete frontend)
> get_message_handler frontend '3' sz = do return (close_complete frontend)
> get_message_handler frontend 'C' sz = do tag <- getString (ms - 4)
>                                          return (command_complete frontend tag)
> get_message_handler frontend 'd' sz = do payload <- getBytes (sz - 4)
>                                          return (copy_data frontend payload)
> get_message_handler frontend 'c' sz = do return (copy_done frontend)
> get_message_handler frontend 'd' sz = do msg <- getString (sz - 4)
>                                          return (copy_failed frontend msg)
> get_message_handler frontend 'G' sz = do format <- getWord8
>                                          formats <- getList16 getWord16
>                                          return (copy_in_response frontend format formats)
> get_message_handler frontend 'H' sz = do format <- getWord8
>                                          formats <- getList16 getWord16
>                                          return (copy_out_response frontend format formats)
> get_message_handler frontend 'W' sz = do format <- getWord8
>                                          formats <- getList16 getWord16
>                                          return (copy_both_response frontend format formats)
> get_message_handler frontend 'D' sz = do values <- getList16 getNullableValue
>                                          return (data_row frontend values)
> get_message_handler frontend 'I' sz = do return (empty_query_response frontend)
> get_message_handler frontend 'E' sz = do fields <- ByteString.split 0 <$> getString (sz - 4)
>                                          return (error_response frontend fields)
> get_message_handler frontend 'V' sz = do value <- getNullableValue
>                                          return (function_call_response frontend value)
> get_message_handler frontend 'n' sz = do return (no_data frontend)
> get_message_handler frontend 'N' sz = do fields <- ByteString.split 0 <$> getString (sz - 4)
>                                          return (notice_response frontend fields)
> get_message_handler frontend 'A' sz = do pid <- getWord32be
>                                          (channel, payload_opt) <- ByteString.break (== 0) <$> getString (sz - 8)
>                                          when (ByteString.null payload_opt) $ fail "missing notification payload" -- payload starts with the null terminator of channel
>                                          return (notification_response frontend pid channel (ByteString.tail payload_opt)
> get_message_handler frontend 't' sz = do oids <- getList (fromIntegral <$> getWord32be)
>                                          return (parameter_description frontend oids)
> get_message_handler frontend 'S' sz = do (name, value_opt) <- ByteString.break (== 0) <$> getString (sz - 4)
>                                          when (ByteString.null value_opt) $ fail "missing parameter value"
>                                          return (parameter_status frontend name (ByteString.tail value_opt) -- value_opt starts with the null terminator of name
> get_message_handler frontend '1' sz = do return (parse_complete frontend)
> get_message_handler frontend 's' sz = do return (portal_suspended frontend)
> get_message_handler frontend 'Z' sz = do status <- getChar8
>                                          return (ready_for_query frontend status)
> get_message_handler frontend 'T' sz = do fields <- getList16 (liftM7 FieldDescription
>                                                                      getNullTerminatedString
>                                                                      (fromIntegral <$> getWord32be)
>                                                                      (fromIntegral <$> getWord16be)
>                                                                      (fromIntegral <$> getWord32be)
>                                                                      (fromIntegral <$> getWord16be)
>                                                                      (fromIntegral <$> getWord32be)
>                                                                      (fromIntegral <$> getWord16be))
>                                          return (row_description frontend fields)

> data FieldDescription = FieldDescription {
>   field_name          :: !ByteString,
>   field_table_oid     :: {-# UNPACK #-} !Int32,
>   field_column_index  :: {-# UNPACK #-} !Int16,
>   field_type_oid      :: {-# UNPACK #-} !Int32,
>   field_type_size     :: {-# UNPACK #-} !Int16,
>   field_type_modifier :: {-# UNPACK #-} !Int32,
>   field_format_code   :: {-# UNPACK #-} !Int16
> } deriving (Eq, Ord, Show)

> getList16 :: Get e -> Get [Word16]
> getList16 getElem = do
>   n <- fromIntegral <$> getWord16be
>   unless (n >= 0) $ fail "negative array size"
>   replicateM n getElem

> getNullTerminatedString :: Get ByteString
> getNullTerminatedString = Lazy.toStrict <$> toLazyByteString <$> loop mempty
>  where loop  r = getWord8 >=> loop' r
>        loop' r 0 = return (Lazy.toStrict (Builder.toLazyByteString r))
>        loop' r c = loop (word8 c <> r)

> getNullableValue :: Get (Maybe ByteString)
> getNullableValue = do
>   sz <- fromIntegral <$> getWord32be
>   case compare sz 0 of
>     LT -> if (sz == -1) then return Nothing else fail "negative value length"
>     EQ -> return (Just ByteString.empty)
>     GT -> Just <$> getBytes sz

> getString :: Int -> Get ByteString
> getString sz = do text <- getBytes (sz - 1)
>                   null <- getWord8
>                   unless (null == 0) $ fail "unterminated string"
>                   return text

> getChar8 :: Get Char
> getChar8 = chr <$> fromIntegral <$> getWord8

> receive_backend_messages :: Frontend a -> Socket -> IO a
> receive_backend_messages frontend socket = loop
>  where
>   loop = do (msg_tag, msg_size) <- recv socket 5 >=> parse_message (liftM2 (,) (getChar8) (getWord32be))
>             when (msg_size - 4 > max_msg_size) $ fail "invalid message length specified"
>             let msg_data_size = fromIntegral (msg_size - 4)
>             msg_data <- recv socket msg_data_size
>             case runGetOrFail (get_message_handler frontend msg_tag msg_data_size) msg_data of
>               Right (excess_data, _, h) | ByteString.empty excess_data -> h
>                                         | otherwise -> fail "excess trailing data detected in message"
>               Left (_, _, err) -> unrecognized_message frontend msg_tag msg_size msg_data err
>             loop

> parse_message :: Get a -> ByteString -> IO a
> parse_message p t = liftEitherM (runGet p t)

> liftEitherM :: Monad m => Either String a -> m a
> liftEitherM (Right x) = return x
> liftEitherM (Left err) = fail err
