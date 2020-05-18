-- | Module:    Database.PostgreSQL.Protocol.Value
-- Description: Asynchronous database client
-- Copyright:   © 2015-2020 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable
--
-- ...

{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Value (
  IsValue(..), toQuotedSqlLiteral
) where

import Data.Bool
import Data.Int
import Data.Word
import Database.PostgreSQL.Protocol.Types (ObjectID)

import qualified Database.PostgreSQL.Protocol.ObjectIDs as ObjectID

class IsValue a where
  toSqlLiteral :: a -> String
  parseSqlLiteral ::  MonadFail m => String -> m a

  sqlTypeName :: a -> String
  sqlTypeName _ = ""

  sqlTypeID :: a -> ObjectID
  sqlTypeID _ = ObjectID.NULL

instance IsValue Bool where
  sqlTypeName _ = "boolean"
  sqlTypeID _ = ObjectID.BOOLEAN
  toSqlLiteral = bool "f" "t"
  parseSqlLiteral "f" = return False
  parseSqlLiteral "t" = return True
  parseSqlLiteral  x  = invalidSqlLiteral x

instance IsValue Int where
  sqlTypeName _ = "integer"
  sqlTypeID _ = ObjectID.INTEGER
  toSqlLiteral = show
  parseSqlLiteral = parseSqlInt

instance IsValue Int8 where
  sqlTypeName _ = "smallint"
  sqlTypeID _ = ObjectID.SMALLINT
  toSqlLiteral = show
  parseSqlLiteral = parseSqlInt

instance IsValue Int16 where
  sqlTypeName _ = "smallint"
  sqlTypeID _ = ObjectID.SMALLINT
  toSqlLiteral = show
  parseSqlLiteral = parseSqlInt

instance IsValue Int32 where
  sqlTypeName _ = "integer"
  sqlTypeID _ = ObjectID.INTEGER
  toSqlLiteral = show
  parseSqlLiteral = parseSqlInt

instance IsValue Int64 where
  sqlTypeName _ = "bigint"
  sqlTypeID _ = ObjectID.BIGINT
  toSqlLiteral = show
  parseSqlLiteral = parseSqlInt

instance IsValue Word where
  sqlTypeName _ = "integer"
  sqlTypeID _ = ObjectID.INTEGER
  toSqlLiteral = show
  parseSqlLiteral = parseSqlWord

instance IsValue Word8 where
  sqlTypeName _ = "smallint"
  sqlTypeID _ = ObjectID.SMALLINT
  toSqlLiteral = show
  parseSqlLiteral = parseSqlWord

instance IsValue Word16 where
  sqlTypeName _ = "smallint"
  sqlTypeID _ = ObjectID.SMALLINT
  toSqlLiteral = show
  parseSqlLiteral = parseSqlWord

instance IsValue Word32 where
  sqlTypeName _ = "integer"
  sqlTypeID _ = ObjectID.INTEGER
  toSqlLiteral = show
  parseSqlLiteral = parseSqlWord

instance IsValue Word64 where
  sqlTypeName _ = "bigint"
  sqlTypeID _ = ObjectID.BIGINT
  toSqlLiteral = show
  parseSqlLiteral = parseSqlWord

parseSqlInt :: (MonadFail m, IsValue a, Num a, Ord a) => String -> m a
parseSqlInt = parseSqlFromMaybe parseSqlInt'

parseSqlWord :: (MonadFail m, IsValue a, Num a, Ord a) => String -> m a
parseSqlWord = parseSqlFromMaybe parseSqlWord'

parseSqlInt' :: (Num a, Ord a) => String -> Maybe a
parseSqlInt' ('-':s) = negate <$> parseSqlWord' s
parseSqlInt' s = parseSqlWord' s

parseSqlWord' :: (Num a, Ord a) => String -> Maybe a
parseSqlWord' = parse 0
 where
  parse r ('0':xs) = parse' r 0 xs
  parse r ('1':xs) = parse' r 1 xs
  parse r ('2':xs) = parse' r 2 xs
  parse r ('3':xs) = parse' r 3 xs
  parse r ('4':xs) = parse' r 4 xs
  parse r ('5':xs) = parse' r 5 xs
  parse r ('6':xs) = parse' r 6 xs
  parse r ('7':xs) = parse' r 7 xs
  parse r ('8':xs) = parse' r 8 xs
  parse r ('9':xs) = parse' r 9 xs
  parse _ _ = fail "unrecognised digit"

  parse' r x xs
    | (r' < r) = fail "overflow"
    | (null xs) = return r
    | otherwise = parse r' xs
   where r' = 10 * r + x

parseSqlFromMaybe :: (MonadFail m, IsValue a) => (String -> Maybe a) -> String -> m a
parseSqlFromMaybe p s = maybe (invalidSqlLiteral s) return (p s)

invalidSqlLiteral :: (MonadFail m, IsValue a) => String -> m a
invalidSqlLiteral = invalidSqlLiteral' undefined

invalidSqlLiteral' :: (MonadFail m, IsValue a) => a -> String -> m a
invalidSqlLiteral' r s = fail ("Invalid " <> msg <> "value: " <> quotedSqlString s)
 where
  tn = sqlTypeName r
  msg | null tn = "SQL"
      | otherwise = "SQL " <> tn

toQuotedSqlLiteral :: IsValue a => a -> String
toQuotedSqlLiteral = quotedSqlString . toSqlLiteral

quotedSqlString :: String -> String
quotedSqlString s = '\'' : quoted s
 where
  quoted (c:xs) = c : case c of
                        '\'' -> c : quoted xs
                        _ -> quoted xs
  quoted [] = "\'"
