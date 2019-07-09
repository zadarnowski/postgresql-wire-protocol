{-# LANGUAGE PatternSynonyms #-}

-- | Module:    Database.PostgreSQL.Protocol.ObjectIDs
-- Description: Well-known PostgreSQL object identifier values
-- Copyright:   (c) 2015-2019 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  pat@jantar.org
-- Stability:   experimental
-- Portability: portable
--
-- In PostgreSQL /object identifiers/ or /OIDs/ are used to refer to most database
-- objects, including runtime concepts such as tables, functions, types, cursors
-- and even transactions. Most of these OIDs are allocated dynamically at runtime
-- and must be queried from system tables, but a handful has been hard-coded in
-- the PostgreSQL source code to enable streamlined access to built-in types
-- and other useful concepts. For example, the use of these built-in OIDs makes
-- it easy to request binary encoding of results from queries returning standard
-- SQL and PostgreSQL types such as @INTEGER@ and @TEXT@ without resorting to
-- expensive system catalogue queries.
--
-- In PostgreSQL implementation, these OIDs are introduced by the @pg_type.h@
-- header and have somewhat awkward names such as @BYTEAOID@ and even @OIDOID@.
-- In this Haskell module, we define a set of pattern aliases for all current
-- well-known values of the 'Database.PostgreSQL.Protocol.Types.ObjectID' type,
-- including those that are introduced into the PostgreSQL catalogue in
-- @pg_type.h@ without a C constant.
--
-- For conciseness and predictability, we strive to give these 'ObjectID' patterns
-- uppercase names identical to those used in the PostgreSQL query language,
-- selecting the unabbreviated standard SQL spelling of type names whenever
-- available, e.g., @BIGINT@ or @CHARACTER@. For brevity, we drop the @OID@
-- suffix features in the C constants from @pg_type.h@.
--
-- Because the list of well-known OID values is guaranteed to grow in future
-- PostgreSQL releases, this module should always be imported qualified.
--
-- In the following documentation, OID constants are listed in the order
-- of their introduction in @pg_type.h@, which mostly follows their numeric
-- values and generally bears little relation to their semantic meanings.
--
-- It should be pointed out that PostgreSQL server introduces a number of
-- predefined object IDs for entities that appear to have little to do with
-- “types”, such as @SMGR@ which is described in the source code as
-- “storage manager”, but are nevertheless included in the @pg_type@
-- catalogue (probably for bootstrapping purposes) and hence can be used
-- as column types in actual database tables. You can even create a column
-- with an @UNKNOWN@ type! For completeness, all of these constants are
-- included below, but it's probably best to refrain from their use in
-- real programs.

module Database.PostgreSQL.Protocol.ObjectIDs where

import Database.PostgreSQL.Protocol.Types (ObjectID)


-- * Well-Known Object Identifiers

-- | Null object ID reserved to represent lack of explicit object ID information in PostgreSQL wire protocol exchanges.
pattern NULL :: ObjectID
pattern NULL = 0

-- | Well-known object ID of the standard SQL type @BOOLEAN@.
pattern BOOLEAN :: ObjectID
pattern BOOLEAN = 16

-- | Well-known object ID of PostgreSQL type @BYTEA@.
pattern BYTEA :: ObjectID
pattern BYTEA = 17

-- | Well-known object ID of the standard SQL type @CHARACTER@.
pattern CHARACTER :: ObjectID
pattern CHARACTER = 18

-- | Well-known object ID of PostgreSQL type @NAME@.
pattern NAME :: ObjectID
pattern NAME = 19

-- | Well-known object ID of the standard SQL type @BIGINT@.
pattern BIGINT :: ObjectID
pattern BIGINT = 20

-- | Well-known object ID of the standard SQL type @SMALLINT@.
pattern SMALLINT :: ObjectID
pattern SMALLINT = 21

-- | Well-known object ID of PostgreSQL type @INT2VECTOR@.
pattern INT2VECTOR :: ObjectID
pattern INT2VECTOR = 22

-- | Well-known object ID of the standard SQL type @INTEGER@.
pattern INTEGER :: ObjectID
pattern INTEGER = 23

-- | Well-known object ID of PostgreSQL type @REGPROC@.
pattern REGPROC :: ObjectID
pattern REGPROC = 24

-- | Well-known object ID of PostgreSQL type @TEXT@.
pattern TEXT :: ObjectID
pattern TEXT = 25

-- | Well-known object ID of PostgreSQL type @OID@.
pattern OID :: ObjectID
pattern OID = 26

-- | Well-known object ID of PostgreSQL type @TID@.
pattern TID :: ObjectID
pattern TID = 27

-- | Well-known object ID of PostgreSQL type @XID@.
pattern XID :: ObjectID
pattern XID = 28

-- | Well-known object ID of PostgreSQL type @CID@.
pattern CID :: ObjectID
pattern CID = 29

-- | Well-known object ID of PostgreSQL type @OIDVECTOR@.
pattern OIDVECTOR :: ObjectID
pattern OIDVECTOR = 30

-- | Well-known object ID of PostgreSQL type @PG_TYPE@.
pattern PG_TYPE :: ObjectID
pattern PG_TYPE = 71

-- | Well-known object ID of PostgreSQL type @PG_ATTRIBUTE@.
pattern PG_ATTRIBUTE :: ObjectID
pattern PG_ATTRIBUTE = 75

-- | Well-known object ID of PostgreSQL type @PG_PROC@.
pattern PG_PROC :: ObjectID
pattern PG_PROC = 81

-- | Well-known object ID of PostgreSQL type @PG_CLASS@.
pattern PG_CLASS :: ObjectID
pattern PG_CLASS = 83

-- | Well-known object ID of PostgreSQL type @JSON@.
pattern JSON :: ObjectID
pattern JSON = 114

-- | Well-known object ID of PostgreSQL type @XML@.
pattern XML :: ObjectID
pattern XML = 142

-- | Well-known object ID of PostgreSQL array type @XML[]@.
pattern XML_ARRAY :: ObjectID
pattern XML_ARRAY = 143

-- | Well-known object ID of PostgreSQL array type @JSON[]@.
pattern JSON_ARRAY :: ObjectID
pattern JSON_ARRAY = 199

-- | Well-known object ID of PostgreSQL type "PG_NODE_TREE".
pattern PG_NODE_TREE :: ObjectID
pattern PG_NODE_TREE = 194

-- | Well-known object ID of PostgreSQL type "SMGR".
pattern SMGR :: ObjectID
pattern SMGR = 210

-- | Well-known object ID of PostgreSQL type @POINT@.
pattern POINT :: ObjectID
pattern POINT = 600

-- | Well-known object ID of PostgreSQL type @LSEG@.
pattern LSEG :: ObjectID
pattern LSEG = 601

-- | Well-known object ID of PostgreSQL type @PATH@.
pattern PATH :: ObjectID
pattern PATH = 602

-- | Well-known object ID of PostgreSQL type @BOX@.
pattern BOX :: ObjectID
pattern BOX = 603

-- | Well-known object ID of PostgreSQL type @POLYGON@.
pattern POLYGON :: ObjectID
pattern POLYGON = 604

-- | Well-known object ID of PostgreSQL type @LINE@.
pattern LINE :: ObjectID
pattern LINE = 628

-- | Well-known object ID of PostgreSQL array type @LINE[]@.
pattern LINE_ARRAY :: ObjectID
pattern LINE_ARRAY = 629

-- | Well-known object ID of the standard SQL type @REAL@.
pattern REAL :: ObjectID
pattern REAL = 700

-- | Well-known object ID of the standard SQL type @DOUBLE PRECISION@.
pattern DOUBLE :: ObjectID
pattern DOUBLE = 701

-- | Well-known object ID of the obsoleted PostgreSQL type @ABSTIME@.
pattern ABSTIME :: ObjectID
pattern ABSTIME = 702

-- | Well-known object ID of the obsoleted PostgreSQL type @RELTIME@.
pattern RELTIME :: ObjectID
pattern RELTIME = 703

-- | Well-known object ID of the obsoleted PostgreSQL type @TINTERVAL@.
pattern TINTERVAL :: ObjectID
pattern TINTERVAL = 704

-- | Well-known object ID used to represented type errors.
pattern UNKNOWN :: ObjectID
pattern UNKNOWN = 705

-- | Well-known object ID of PostgreSQL type @CIRCLE@.
pattern CIRCLE :: ObjectID
pattern CIRCLE = 718

-- | Well-known object ID of PostgreSQL array type @CIRCLE[]@.
pattern CIRCLE_ARRAY :: ObjectID
pattern CIRCLE_ARRAY = 719

-- | Well-known object ID of PostgreSQL type @MONEY@.
pattern MONEY :: ObjectID
pattern MONEY = 790

-- | Well-known object ID of PostgreSQL array type @MONEY[]@.
pattern MONEY_ARRAY :: ObjectID
pattern MONEY_ARRAY = 791

-- | Well-known object ID of PostgreSQL type @MACADDR@.
pattern MACADDR :: ObjectID
pattern MACADDR = 829

-- | Well-known object ID of PostgreSQL type @INET@.
pattern INET :: ObjectID
pattern INET = 869

-- | Well-known object ID of PostgreSQL type @CIDR@.
pattern CIDR :: ObjectID
pattern CIDR = 650

-- | Well-known object ID of PostgreSQL array type @BOOLEAN[]@.
pattern BOOLEAN_ARRAY :: ObjectID
pattern BOOLEAN_ARRAY = 1000

-- | Well-known object ID of PostgreSQL array type @BYTEA[]@.
pattern BYTEA_ARRAY :: ObjectID
pattern BYTEA_ARRAY = 1001

-- | Well-known object ID of PostgreSQL array type @CHARACTER[]@.
pattern CHARACTER_ARRAY :: ObjectID
pattern CHARACTER_ARRAY = 1002

-- | Well-known object ID of PostgreSQL array type @NAME[]@.
pattern NAME_ARRAY :: ObjectID
pattern NAME_ARRAY = 1003

-- | Well-known object ID of PostgreSQL array type @SMALLINT[]@.
pattern SMALLINT_ARRAY :: ObjectID
pattern SMALLINT_ARRAY = 1005

-- | Well-known object ID of PostgreSQL array type @INT2VECTOR[]@.
pattern INT2VECTOR_ARRAY :: ObjectID
pattern INT2VECTOR_ARRAY = 1006

-- | Well-known object ID of PostgreSQL array type @INTEGER[]@.
pattern INTEGER_ARRAY :: ObjectID
pattern INTEGER_ARRAY = 1007

-- | Well-known object ID of PostgreSQL array type @REGPROC[]@.
pattern REGPROC_ARRAY :: ObjectID
pattern REGPROC_ARRAY = 1008

-- | Well-known object ID of PostgreSQL array type @TEXT[]@.
pattern TEXT_ARRAY :: ObjectID
pattern TEXT_ARRAY = 1009

-- | Well-known object ID of PostgreSQL array type @OID[]@.
pattern OID_ARRAY :: ObjectID
pattern OID_ARRAY = 1028

-- | Well-known object ID of PostgreSQL array type @TID[]@.
pattern TID_ARRAY :: ObjectID
pattern TID_ARRAY = 1010

-- | Well-known object ID of PostgreSQL array type @XID[]@.
pattern XID_ARRAY :: ObjectID
pattern XID_ARRAY = 1011

-- | Well-known object ID of PostgreSQL array type @CID[]@.
pattern CID_ARRAY :: ObjectID
pattern CID_ARRAY = 1012

-- | Well-known object ID of PostgreSQL array type @OIDVECTOR[]@.
pattern OIDVECTOR_ARRAY :: ObjectID
pattern OIDVECTOR_ARRAY = 1013

-- | Well-known object ID of PostgreSQL array type @CHARACTER(/n/)[]@ or @BPCHAR(/n/)[]@.
pattern BPCHAR_ARRAY :: ObjectID
pattern BPCHAR_ARRAY = 1014

-- | Well-known object ID of PostgreSQL array type @CHARACTER VARYING(/n/)[]@ or @VARCHAR(/n/)[]@.
pattern VARCHAR_ARRAY :: ObjectID
pattern VARCHAR_ARRAY = 1015

-- | Well-known object ID of PostgreSQL array type @BIGINT[]@.
pattern BIGINT_ARRAY :: ObjectID
pattern BIGINT_ARRAY = 1016

-- | Well-known object ID of PostgreSQL array type @POINT[]@.
pattern POINT_ARRAY :: ObjectID
pattern POINT_ARRAY = 1017

-- | Well-known object ID of PostgreSQL array type @LSEG[]@.
pattern LSEG_ARRAY :: ObjectID
pattern LSEG_ARRAY = 1018

-- | Well-known object ID of PostgreSQL array type @PATH[]@.
pattern PATH_ARRAY :: ObjectID
pattern PATH_ARRAY = 1019

-- | Well-known object ID of PostgreSQL array type @BOX[]@.
pattern BOX_ARRAY :: ObjectID
pattern BOX_ARRAY = 1020

-- | Well-known object ID of PostgreSQL array type @REAL[]@.
pattern REAL_ARRAY :: ObjectID
pattern REAL_ARRAY = 1021

-- | Well-known object ID of PostgreSQL array type @DOUBLE PRECISION[]@ or @DOUBLE[]@.
pattern DOUBLE_ARRAY :: ObjectID
pattern DOUBLE_ARRAY = 1022

-- | Well-known object ID of PostgreSQL array type @ABSTIME[]@.
pattern ABSTIME_ARRAY :: ObjectID
pattern ABSTIME_ARRAY = 1023

-- | Well-known object ID of PostgreSQL array type @RELTIME[]@.
pattern RELTIME_ARRAY :: ObjectID
pattern RELTIME_ARRAY = 1024

-- | Well-known object ID of PostgreSQL array type @TINTERVAL[]@.
pattern TINTERVAL_ARRAY :: ObjectID
pattern TINTERVAL_ARRAY = 1025

-- | Well-known object ID of PostgreSQL array type @POLYGON[]@.
pattern POLYGON_ARRAY :: ObjectID
pattern POLYGON_ARRAY = 1027

-- | Well-known object ID of PostgreSQL type @ACLITEM@ used to represent access control lists.
pattern ACLITEM :: ObjectID
pattern ACLITEM = 1033

-- | Well-known object ID of PostgreSQL array type @ACLITEM[]@ used to represent arrays of access control lists.
pattern ACLITEM_ARRAY :: ObjectID
pattern ACLITEM_ARRAY = 1034

-- | Well-known object ID of PostgreSQL array type @MACADDR[]@.
pattern MACADDR_ARRAY :: ObjectID
pattern MACADDR_ARRAY = 1040

-- | Well-known object ID of PostgreSQL array type @INET[]@.
pattern INET_ARRAY :: ObjectID
pattern INET_ARRAY = 1041

-- | Well-known object ID of PostgreSQL array type @CIDR[]@.
pattern CIDR_ARRAY :: ObjectID
pattern CIDR_ARRAY = 651

-- | Well-known object ID of PostgreSQL array type @CSTRING[]@.
pattern CSTRING_ARRAY :: ObjectID
pattern CSTRING_ARRAY = 1263

-- | Well-known object ID of the standard SQL type @CHARACTER(/n/)@ or @BPCHAR(/n/)@.
pattern BPCHAR :: ObjectID
pattern BPCHAR = 1042

-- | Well-known object ID of the standard SQL type @CHARACTER VARYING(/n/)@ or @VARCHAR(/n/)@.
pattern VARCHAR :: ObjectID
pattern VARCHAR = 1043

-- | Well-known object ID of the standard SQL type @DATE@.
pattern DATE :: ObjectID
pattern DATE = 1082

-- | Well-known object ID of the standard SQL type @TIME@.
pattern TIME :: ObjectID
pattern TIME = 1083

-- | Well-known object ID of PostgreSQL type @TIMESTAMP@ or @TIMESTAMP WITHOUT TIME ZONE@.
pattern TIMESTAMP :: ObjectID
pattern TIMESTAMP = 1114

-- | Well-known object ID of PostgreSQL array type @TIMESTAMP[]@ or @TIMESTAMP WITHOUT TIME ZONE[]@.
pattern TIMESTAMP_ARRAY :: ObjectID
pattern TIMESTAMP_ARRAY = 1115

-- | Well-known object ID of PostgreSQL array type @DATE[]@.
pattern DATE_ARRAY :: ObjectID
pattern DATE_ARRAY = 1182

-- | Well-known object ID of PostgreSQL array type @TIME[]@.
pattern TIME_ARRAY :: ObjectID
pattern TIME_ARRAY = 1183

-- | Well-known object ID of PostgreSQL type @TIMESTAMPTZ@ or @TIMESTAMP WITH TIME ZONE@.
pattern TIMESTAMPTZ :: ObjectID
pattern TIMESTAMPTZ = 1184

-- | Well-known object ID of PostgreSQL array type @TIMESTAMPTZ[]@ or @TIMESTAMP WITH TIME ZONE[]@.
pattern TIMESTAMPTZ_ARRAY :: ObjectID
pattern TIMESTAMPTZ_ARRAY = 1185

-- | Well-known object ID of PostgreSQL type @INTERVAL@.
pattern INTERVAL :: ObjectID
pattern INTERVAL = 1186

-- | Well-known object ID of PostgreSQL array type @INTERVAL[]@.
pattern INTERVAL_ARRAY :: ObjectID
pattern INTERVAL_ARRAY = 1187

-- | Well-known object ID of PostgreSQL array type @NUMERIC(/p/, /d/)[]@.
pattern NUMERIC_ARRAY :: ObjectID
pattern NUMERIC_ARRAY = 1231

-- | Well-known object ID of PostgreSQL type @TIMETZ@ or @TIME WITH TIME ZONE@.
pattern TIMETZ :: ObjectID
pattern TIMETZ = 1266

-- | Well-known object ID of PostgreSQL array type @TIMETZ[]@ or @TIME WITH TIME ZONE[]@.
pattern TIMETZ_ARRAY :: ObjectID
pattern TIMETZ_ARRAY = 1270

-- | Well-known object ID of PostgreSQL type @BIT(/n/)@ used to represent fixed-length bit strings.
pattern BIT :: ObjectID
pattern BIT = 1560

-- | Well-known object ID of PostgreSQL array type @BIT(/n/)[]@.
pattern BIT_ARRAY :: ObjectID
pattern BIT_ARRAY = 1561

-- | Well-known object ID of PostgreSQL type @BIT VARYING(/n/)@ or @VARBIT(/n/)@.
pattern VARBIT :: ObjectID
pattern VARBIT = 1562

-- | Well-known object ID of PostgreSQL array type @BIT VARYING(/n/)[]@ or @VARBIT(/n/)[]@.
pattern VARBIT_ARRAY :: ObjectID
pattern VARBIT_ARRAY = 1563

-- | Well-known object ID of the standard SQL type @NUMERIC(/p/, /d/)@.
pattern NUMERIC :: ObjectID
pattern NUMERIC = 1700

-- | Well-known object ID of PostgreSQL type @REFCURSOR@ used to represent references to cursors or portal names.
pattern REFCURSOR :: ObjectID
pattern REFCURSOR = 1790

-- | Well-known object ID of PostgreSQL array type @REFCURSOR[]@  used to represent arrays of references to cursors or portal names.
pattern REFCURSOR_ARRAY :: ObjectID
pattern REFCURSOR_ARRAY = 2201

-- | Well-known object ID of PostgreSQL type @REGPROCEDURE@ used to represent registered procedures with arguments.
pattern REGPROCEDURE :: ObjectID
pattern REGPROCEDURE = 2202

-- | Well-known object ID of PostgreSQL type @REGOPER@ used to represent registered operators.
pattern REGOPER :: ObjectID
pattern REGOPER = 2203

-- | Well-known object ID of PostgreSQL type @REGOPERATOR@ used to represent registered operators with arguments.
pattern REGOPERATOR :: ObjectID
pattern REGOPERATOR = 2204

-- | Well-known object ID of PostgreSQL type @REGCLASS@ used to represent registered classes.
pattern REGCLASS :: ObjectID
pattern REGCLASS = 2205

-- | Well-known object ID of PostgreSQL type @REGTYPE@ used to represent registered types.
pattern REGTYPE :: ObjectID
pattern REGTYPE = 2206

-- | Well-known object ID of PostgreSQL array type @REGPROCEDURE[]@ used to represent arrays of registered procedures with arguments.
pattern REGPROCEDURE_ARRAY :: ObjectID
pattern REGPROCEDURE_ARRAY = 2207

-- | Well-known object ID of PostgreSQL array type @REGOPER[]@ used to represent arrays of registered operators.
pattern REGOPER_ARRAY :: ObjectID
pattern REGOPER_ARRAY = 2208

-- | Well-known object ID of PostgreSQL array type @REGOPERATOR[]@ used to represent arrays of registered operators with arguments.
pattern REGOPERATOR_ARRAY :: ObjectID
pattern REGOPERATOR_ARRAY = 2209

-- | Well-known object ID of PostgreSQL array type @REGCLASS[]@ used to represent arrays of registered classes.
pattern REGCLASS_ARRAY :: ObjectID
pattern REGCLASS_ARRAY = 2210

-- | Well-known object ID of PostgreSQL array type @REGTYPE[]@ used to represent arrays of registered types.
pattern REGTYPE_ARRAY :: ObjectID
pattern REGTYPE_ARRAY = 2211

-- | Well-known object ID of PostgreSQL type @UUID@.
pattern UUID :: ObjectID
pattern UUID = 2950

-- | Well-known object ID of PostgreSQL array type @UUID[]@.
pattern UUID_ARRAY :: ObjectID
pattern UUID_ARRAY = 2951

-- | Well-known object ID of PostgreSQL type @PG_LSN@.
pattern PG_LSN :: ObjectID
pattern PG_LSN = 3220

-- | Well-known object ID of PostgreSQL array type @PG_LSN[]@.
pattern PG_LSN_ARRAY :: ObjectID
pattern PG_LSN_ARRAY = 3221

-- | Well-known object ID of PostgreSQL type @TSVECTOR@.
pattern TSVECTOR :: ObjectID
pattern TSVECTOR = 3614

-- | Well-known object ID of PostgreSQL type @GTSVECTOR@.
pattern GTSVECTOR :: ObjectID
pattern GTSVECTOR = 3642

-- | Well-known object ID of PostgreSQL type @TSQUERY@.
pattern TSQUERY :: ObjectID
pattern TSQUERY = 3615

-- | Well-known object ID of PostgreSQL type @REGCONFIG@.
pattern REGCONFIG :: ObjectID
pattern REGCONFIG = 3734

-- | Well-known object ID of PostgreSQL type @REGDICTIONARY@.
pattern REGDICTIONARY :: ObjectID
pattern REGDICTIONARY = 3769

-- | Well-known object ID of PostgreSQL array type @TSVECTOR[]@.
pattern TSVECTOR_ARRAY :: ObjectID
pattern TSVECTOR_ARRAY = 3643

-- | Well-known object ID of PostgreSQL array type @GTSVECTOR[]@.
pattern GTSVECTOR_ARRAY :: ObjectID
pattern GTSVECTOR_ARRAY = 3644

-- | Well-known object ID of PostgreSQL array type @TSQUERY[]@.
pattern TSQUERY_ARRAY :: ObjectID
pattern TSQUERY_ARRAY = 3645

-- | Well-known object ID of PostgreSQL array type @REGCONFIG[]@.
pattern REGCONFIG_ARRAY :: ObjectID
pattern REGCONFIG_ARRAY = 3735

-- | Well-known object ID of PostgreSQL array type @REGDICTIONARY[]@.
pattern REGDICTIONARY_ARRAY :: ObjectID
pattern REGDICTIONARY_ARRAY = 3770

-- | Well-known object ID of PostgreSQL type @JSONB@.
pattern JSONB :: ObjectID
pattern JSONB = 3802

-- | Well-known object ID of PostgreSQL array type @JSONB[]@.
pattern JSONB_ARRAY :: ObjectID
pattern JSONB_ARRAY = 3807

-- | Well-known object ID of PostgreSQL type @TXID_SNAPSHOT@.
pattern TXID_SNAPSHOT :: ObjectID
pattern TXID_SNAPSHOT = 2970

-- | Well-known object ID of PostgreSQL array type @TXID_SNAPSHOT[]@.
pattern TXID_SNAPSHOT_ARRAY :: ObjectID
pattern TXID_SNAPSHOT_ARRAY = 2949

-- | Well-known object ID of PostgreSQL range type @INT4RANGE@.
pattern INT4RANGE :: ObjectID
pattern INT4RANGE = 3904

-- | Well-known object ID of PostgreSQL array type @INT4RANGE[]@.
pattern INT4RANGE_ARRAY :: ObjectID
pattern INT4RANGE_ARRAY = 3905

-- | Well-known object ID of PostgreSQL range type @NUMRANGE@.
pattern NUMRANGE :: ObjectID
pattern NUMRANGE = 3906

-- | Well-known object ID of PostgreSQL array type @NUMRANGE[]@.
pattern NUMRANGE_ARRAY :: ObjectID
pattern NUMRANGE_ARRAY = 3907

-- | Well-known object ID of PostgreSQL range type @TSRANGE@.
pattern TSRANGE :: ObjectID
pattern TSRANGE = 3908

-- | Well-known object ID of PostgreSQL array type @TSRANGE[]@.
pattern TSRANGE_ARRAY :: ObjectID
pattern TSRANGE_ARRAY = 3909

-- | Well-known object ID of PostgreSQL range type @TSTZRANGE@.
pattern TSTZRANGE :: ObjectID
pattern TSTZRANGE = 3910

-- | Well-known object ID of PostgreSQL array type @TSTZRANGE[]@.
pattern TSTZRANGE_ARRAY :: ObjectID
pattern TSTZRANGE_ARRAY = 3911

-- | Well-known object ID of PostgreSQL range type @DATERANGE@.
pattern DATERANGE :: ObjectID
pattern DATERANGE = 3912

-- | Well-known object ID of PostgreSQL array type @DATERANGE[]@.
pattern DATERANGE_ARRAY :: ObjectID
pattern DATERANGE_ARRAY = 3913

-- | Well-known object ID of PostgreSQL range type @INT8RANGE@.
pattern INT8RANGE :: ObjectID
pattern INT8RANGE = 3926

-- | Well-known object ID of PostgreSQL array type @INT8RANGE[]@.
pattern INT8RANGE_ARRAY :: ObjectID
pattern INT8RANGE_ARRAY = 3927


-- * Pseudo-Types

-- | Well-known object ID of PostgreSQL pseudo-type @RECORD@.
pattern RECORD :: ObjectID
pattern RECORD = 2249

-- | Well-known object ID of PostgreSQL array pseudo-type @RECORD[]@.
pattern RECORD_ARRAY :: ObjectID
pattern RECORD_ARRAY = 2287

-- | Well-known object ID of PostgreSQL pseudo-type @CSTRING@.
pattern CSTRING :: ObjectID
pattern CSTRING = 2275

-- | Well-known object ID of PostgreSQL pseudo-type @ANY@.
pattern ANY :: ObjectID
pattern ANY = 2276

-- | Well-known object ID of PostgreSQL pseudo-type @ANYARRAY@.
pattern ANYARRAY :: ObjectID
pattern ANYARRAY = 2277

-- | Well-known object ID of PostgreSQL pseudo-type @VOID@.
pattern VOID :: ObjectID
pattern VOID = 2278

-- | Well-known object ID of PostgreSQL pseudo-type @TRIGGER@.
pattern TRIGGER :: ObjectID
pattern TRIGGER = 2279

-- | Well-known object ID of PostgreSQL pseudo-type @EVENT_TRIGGER@.
pattern EVENT_TRIGGER :: ObjectID
pattern EVENT_TRIGGER = 3838

-- | Well-known object ID of PostgreSQL pseudo-type @LANGUAGE_HANDLER@.
pattern LANGUAGE_HANDLER :: ObjectID
pattern LANGUAGE_HANDLER = 2280

-- | Well-known object ID of PostgreSQL pseudo-type @INTERNAL@.
pattern INTERNAL :: ObjectID
pattern INTERNAL = 2281

-- | Well-known object ID of PostgreSQL pseudo-type @OPAQUE@.
pattern OPAQUE :: ObjectID
pattern OPAQUE = 2282

-- | Well-known object ID of PostgreSQL pseudo-type @ANYELEMENT@.
pattern ANYELEMENT :: ObjectID
pattern ANYELEMENT = 2283

-- | Well-known object ID of PostgreSQL pseudo-type @ANYNONARRAY@.
pattern ANYNONARRAY :: ObjectID
pattern ANYNONARRAY = 2776

-- | Well-known object ID of PostgreSQL pseudo-type @ANYENUM@.
pattern ANYENUM :: ObjectID
pattern ANYENUM = 3500

-- | Well-known object ID of PostgreSQL pseudo-type @FDW_HANDLER@.
pattern FDW_HANDLER :: ObjectID
pattern FDW_HANDLER = 3115

-- | Well-known object ID of PostgreSQL pseudo-type @ANYRANGE@.
pattern ANYRANGE :: ObjectID
pattern ANYRANGE = 3831
