module Database.PostgreSQL.Protocol.Values where

import Data.ByteString.Builder (Builder)
import Data.Int
import Data.Word

import qualified Data.ByteString.Builder as Builder

data PgFormat = TEXT | BINARY
  deriving (Eq, Ord, Bounded, Enum, Read, Show)

class PgValue a where
  pgValueTypeID         :: a -> ObjectID
  pgValueTypeName       :: a -> ByteString
  pgValueFormat         :: a -> Format
  pgValueBuilder        :: a -> Builder
  pgValueTextBuilder    :: a -> Builder

instance PgValue Bool where
  pgValueTypeID         = const BOOLEAN
  pgValueTypeName       = const "BOOLEAN"
  pgValueFormat         = const BINARY
  pgValueBuilder        = Builder.int8 . fromIntegral . fromEnum
  pgValueTextBuilder    = bool "f" "t"

instance PgValue Int8 where

instance PgValue Int16 where

instance PgValue Int32 where

instance PgValue Int64 where

instance PgValue Integer where
  ...

instance PgValue Text where
  ...

instance PgValue ByteString where
  ...

instance PgValue LazyByteString where
  ...

