-- | Module:    Database.PostgreSQL.Protocol.Tuple
-- Description: Database tuples
-- Copyright:   © 2015-2020 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable
--
-- ...

{-# LANGUAGE TypeOperators #-}

module Database.PostgreSQL.Tuple (
  IsTuple(..), Row(..), (:.)(..),
) where

import Database.PostgreSQL.Value

class IsTuple a where
  cardinality :: a -> Int

instance IsTuple () where
  cardinality _ = 0

data Row a = Row a
  deriving (Eq, Ord, Read, Show)

instance IsValue a => IsTuple (Row a) where
  cardinality _ = 1

data a :. b = !a :. b
  deriving (Eq, Ord, Read, Show)

infixl 5 :.

instance (IsTuple a, IsValue b) => IsTuple (a :. b) where
  cardinality (x :. _) = cardinality x + 1

instance (IsValue a, IsValue b) => IsTuple (a, b) where
  cardinality _ = 2

instance (IsValue a, IsValue b, IsValue c) => IsTuple (a, b, c) where
  cardinality _ = 3

instance (IsValue a, IsValue b, IsValue c, IsValue d) => IsTuple (a, b, c, d) where
  cardinality _ = 4

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e) => IsTuple (a, b, c, d, e) where
  cardinality _ = 5

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f) => IsTuple (a, b, c, d, e, f) where
  cardinality _ = 6

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g) => IsTuple (a, b, c, d, e, f, g) where
  cardinality _ = 7

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h) => IsTuple (a, b, c, d, e, f, g, h) where
  cardinality _ = 8

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i) => IsTuple (a, b, c, d, e, f, g, h, i) where
  cardinality _ = 9

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j) => IsTuple (a, b, c, d, e, f, g, h, i, j) where
  cardinality _ = 10

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k) => IsTuple (a, b, c, d, e, f, g, h, i, j, k) where
  cardinality _ = 11

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l) => IsTuple (a, b, c, d, e, f, g, h, i, j, k, l) where
  cardinality _ = 12

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m) => IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  cardinality _ = 13

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m, IsValue n) => IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  cardinality _ = 14

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m, IsValue n, IsValue o) => IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  cardinality _ = 15

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m, IsValue n, IsValue o, IsValue p) => IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  cardinality _ = 16

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m, IsValue n, IsValue o, IsValue p, IsValue q) => IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) where
  cardinality _ = 17

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m, IsValue n, IsValue o, IsValue p, IsValue q, IsValue r) => IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) where
  cardinality _ = 18

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m, IsValue n, IsValue o, IsValue p, IsValue q, IsValue r, IsValue s) => IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) where
  cardinality _ = 19

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m, IsValue n, IsValue o, IsValue p, IsValue q, IsValue r, IsValue s, IsValue t) => IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) where
  cardinality _ = 20

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m, IsValue n, IsValue o, IsValue p, IsValue q, IsValue r, IsValue s, IsValue t, IsValue u) => IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) where
  cardinality _ = 21

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m, IsValue n, IsValue o, IsValue p, IsValue q, IsValue r, IsValue s, IsValue t, IsValue u, IsValue v) => IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) where
  cardinality _ = 22

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m, IsValue n, IsValue o, IsValue p, IsValue q, IsValue r, IsValue s, IsValue t, IsValue u, IsValue v, IsValue w) => IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) where
  cardinality _ = 23

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m, IsValue n, IsValue o, IsValue p, IsValue q, IsValue r, IsValue s, IsValue t, IsValue u, IsValue v, IsValue w, IsValue x) => IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) where
  cardinality _ = 24

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m, IsValue n, IsValue o, IsValue p, IsValue q, IsValue r, IsValue s, IsValue t, IsValue u, IsValue v, IsValue w, IsValue x, IsValue y) => IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) where
  cardinality _ = 25

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue e, IsValue f, IsValue g, IsValue h, IsValue i, IsValue j, IsValue k, IsValue l, IsValue m, IsValue n, IsValue o, IsValue p, IsValue q, IsValue r, IsValue s, IsValue t, IsValue u, IsValue v, IsValue w, IsValue x, IsValue y, IsValue z) => IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) where
  cardinality _ = 26

