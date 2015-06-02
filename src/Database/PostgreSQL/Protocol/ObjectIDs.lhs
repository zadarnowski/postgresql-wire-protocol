> -- | Module:    Database.PostgreSQL.Protocol.ObjectIDs
> -- Description: Well-known PostgreSQL object identifier values
> -- Copyright:   (c) 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- In PostgreSQL /object identifiers/ or /OIDs/ are used to refer to most database
> -- objects, including runtime concepts such as tables, functions, types, cursors
> -- and even transactions. Most of these OIDs are allocated dynamically at runtime
> -- and must be queried from system tables, but a handful has been hard-coded in
> -- the PostgreSQL source code to enable streamlined access to built-in types
> -- and other useful concepts. For example, the use of these built-in OIDs makes
> -- it easy to request binary encoding of results from queries returning standard
> -- SQL and PostgreSQL types such as @INTEGER@ and @TEXT@ without resorting to
> -- expensive system catalogue queries.
> --
> -- In PostgreSQL implementaiton, these OIDs are introduced by the @pg_type.h@
> -- header and have somewhat awkard names such as @BYTEAOID@ and even @OIDOID@.
> -- In this Haskell module, we define a set of pattern aliases for all current
> -- well-known values of the 'Database.PostgreSQL.Protocol.Types.ObjectID' type,
> -- including those that are introduced into the PostgreSQL catalogue in
> -- @pg_type.h@ without a C constant.
> --
> -- For conciseness and predictability, we strive to give these 'ObjectID' patterns
> -- uppercase names identical to those used in the PostgreSQL query language,
> -- selecting the unabbreviated standard SQL spelling of type names whenever
> -- available, e.g., @BIGINT@ or @CHARACTER@. For brevity, we drop the @OID@
> -- suffix features in the C constants from @pg_type.h@.
> --
> -- Because the list of well-known OID values is guaranteed to grow in future
> -- PostgreSQL releases, this module should always be imported qualified.
> --
> -- In the following documentation, OID constants are listed in the order
> -- of their introduction in @pg_type.h@, which mostly follows their numeric
> -- values and generally bears little relation to their semantic meanings.
> --
> -- It should be pointed out that PostgreSQL server introduces a number of
> -- predefined object IDs for entities that appear to have little to do with
> -- “types”, such as @SMGR@ which is described in the source code as
> -- “storage manager”, but are nevertheless included in the @pg_type@
> -- catalogue (probably for bootstrapping purposes) and hence can be used
> -- as column types in actual database tables. You can even create a column
> -- with an @UNKNOWN@ type! For completeness, all of these constants are
> -- included below, but it's probably best to refrain from their use in
> -- real programs.

> {-# LANGUAGE PatternSynonyms, ScopedTypeVariables #-}

> module Database.PostgreSQL.Protocol.ObjectIDs where

> import Database.PostgreSQL.Protocol.Types (ObjectID)

> -- | Null object ID reserved to represent lack of explicit object ID information in PostgreSQL wire protocol exchanges.
> pattern NULL = 0 :: ObjectID

> -- | Well-known object ID of the standard SQL type @BOOLEAN@.
> pattern BOOLEAN = 16 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @BYTEA@.
> pattern BYTEA = 17 :: ObjectID

> -- | Well-known object ID of the standard SQL type @CHARACTER@.
> pattern CHARACTER = 18 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @NAME@.
> pattern NAME = 19 :: ObjectID

> -- | Well-known object ID of the standard SQL type @BIGINT@.
> pattern BIGINT = 20 :: ObjectID

> -- | Well-known object ID of the standard SQL type @SMALLINT@.
> pattern SMALLINT = 21 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @INT2VECTOR@.
> pattern INT2VECTOR = 22 :: ObjectID

> -- | Well-known object ID of the standard SQL type @INTEGER@.
> pattern INTEGER = 23 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @REGPROC@.
> pattern REGPROC = 24 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @TEXT@.
> pattern TEXT = 25 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @OID@.
> pattern OID = 26 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @TID@.
> pattern TID = 27 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @XID@.
> pattern XID = 28 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @CID@.
> pattern CID = 29 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @OIDVECTOR@.
> pattern OIDVECTOR = 30 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @PG_TYPE@.
> pattern PG_TYPE = 71 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @PG_ATTRIBUTE@.
> pattern PG_ATTRIBUTE = 75 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @PG_PROC@.
> pattern PG_PROC = 81 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @PG_CLASS@.
> pattern PG_CLASS = 83 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @JSON@.
> pattern JSON = 114 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @XML@.
> pattern XML = 142 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @XML[]@.
> pattern XML_ARRAY = 143 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @JSON[]@.
> pattern JSON_ARRAY = 199 :: ObjectID

> -- | Well-known object ID of PostgreSQL type "PG_NODE_TREE".
> pattern PG_NODE_TREE = 194 :: ObjectID

> -- | Well-known object ID of PostgreSQL type "SMGR".
> pattern SMGR = 210 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @POINT@.
> pattern POINT = 600 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @LSEG@.
> pattern LSEG = 601 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @PATH@.
> pattern PATH = 602 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @BOX@.
> pattern BOX = 603 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @POLYGON@.
> pattern POLYGON = 604 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @LINE@.
> pattern LINE = 628 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @LINE[]@.
> pattern LINE_ARRAY = 629 :: ObjectID

> -- | Well-known object ID of the standard SQL type @REAL@.
> pattern REAL = 700 :: ObjectID

> -- | Well-known object ID of the standard SQL type @DOUBLE PRECISION@.
> pattern DOUBLE = 701 :: ObjectID

> -- | Well-known object ID of the obsoleted PostgreSQL type @ABSTIME@.
> pattern ABSTIME = 702 :: ObjectID

> -- | Well-known object ID of the obsoleted PostgreSQL type @RELTIME@.
> pattern RELTIME = 703 :: ObjectID

> -- | Well-known object ID of the obsoleted PostgreSQL type @TINTERVAL@.
> pattern TINTERVAL = 704 :: ObjectID

> -- | Well-known object ID used to represented type errors.
> pattern UNKNOWN = 705 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @CIRCLE@.
> pattern CIRCLE = 718 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @CIRCLE[]@.
> pattern CIRCLE_ARRAY = 719 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @MONEY@.
> pattern MONEY = 790 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @MONEY[]@.
> pattern MONEY_ARRAY = 791 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @MACADDR@.
> pattern MACADDR = 829 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @INET@.
> pattern INET = 869 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @CIDR@.
> pattern CIDR = 650 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @BOOLEAN[]@.
> pattern BOOLEAN_ARRAY = 1000 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @BYTEA[]@.
> pattern BYTEA_ARRAY = 1001 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @CHARACTER[]@.
> pattern CHARACTER_ARRAY = 1002 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @NAME[]@.
> pattern NAME_ARRAY = 1003 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @SMALLINT[]@.
> pattern SMALLINT_ARRAY = 1005 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @INT2VECTOR[]@.
> pattern INT2VECTOR_ARRAY = 1006 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @INTEGER[]@.
> pattern INTEGER_ARRAY = 1007 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @REGPROC[]@.
> pattern REGPROC_ARRAY = 1008 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @TEXT[]@.
> pattern TEXT_ARRAY = 1009 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @OID[]@.
> pattern OID_ARRAY = 1028 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @TID[]@.
> pattern TID_ARRAY = 1010 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @XID[]@.
> pattern XID_ARRAY = 1011 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @CID[]@.
> pattern CID_ARRAY = 1012 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @OIDVECTOR[]@.
> pattern OIDVECTOR_ARRAY = 1013 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @CHARACTER(/n/)[]@ or @BPCHAR(/n/)[]@.
> pattern BPCHAR_ARRAY = 1014 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @CHARACTER VARYING(/n/)[]@ or @VARCHAR(/n/)[]@.
> pattern VARCHAR_ARRAY = 1015 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @BIGINT[]@.
> pattern BIGINT_ARRAY = 1016 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @POINT[]@.
> pattern POINT_ARRAY = 1017 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @LSEG[]@.
> pattern LSEG_ARRAY = 1018 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @PATH[]@.
> pattern PATH_ARRAY = 1019 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @BOX[]@.
> pattern BOX_ARRAY = 1020 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @REAL[]@.
> pattern REAL_ARRAY = 1021 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @DOUBLE PRECISION[]@ or @DOUBLE[]@.
> pattern DOUBLE_ARRAY = 1022 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @ABSTIME[]@.
> pattern ABSTIME_ARRAY = 1023 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @RELTIME[]@.
> pattern RELTIME_ARRAY = 1024 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @TINTERVAL[]@.
> pattern TINTERVAL_ARRAY = 1025 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @POLYGON[]@.
> pattern POLYGON_ARRAY = 1027 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @ACLITEM@ used to represent access control lists.
> pattern ACLITEM = 1033 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @ACLITEM[]@ used to represent arrays of access control lists.
> pattern ACLITEM_ARRAY = 1034 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @MACADDR[]@.
> pattern MACADDR_ARRAY = 1040 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @INET[]@.
> pattern INET_ARRAY = 1041 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @CIDR[]@.
> pattern CIDR_ARRAY = 651 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @CSTRING[]@.
> pattern CSTRING_ARRAY = 1263 :: ObjectID

> -- | Well-known object ID of the standard SQL type @CHARACTER(/n/)@ or @BPCHAR(/n/)@.
> pattern BPCHAR = 1042 :: ObjectID

> -- | Well-known object ID of the standard SQL type @CHARACTER VARYING(/n/)@ or @VARCHAR(/n/)@.
> pattern VARCHAR = 1043 :: ObjectID

> -- | Well-known object ID of the standard SQL type @DATE@.
> pattern DATE = 1082 :: ObjectID

> -- | Well-known object ID of the standard SQL type @TIME@.
> pattern TIME = 1083 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @TIMESTAMP@ or @TIMESTAMP WITHOUT TIME ZONE@.
> pattern TIMESTAMP = 1114 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @TIMESTAMP[]@ or @TIMESTAMP WITHOUT TIME ZONE[]@.
> pattern TIMESTAMP_ARRAY = 1115 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @DATE[]@.
> pattern DATE_ARRAY = 1182 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @TIME[]@.
> pattern TIME_ARRAY = 1183 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @TIMESTAMPTZ@ or @TIMESTAMP WITH TIME ZONE@.
> pattern TIMESTAMPTZ = 1184 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @TIMESTAMPTZ[]@ or @TIMESTAMP WITH TIME ZONE[]@.
> pattern TIMESTAMPTZ_ARRAY = 1185 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @INTERVAL@.
> pattern INTERVAL = 1186 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @INTERVAL[]@.
> pattern INTERVAL_ARRAY = 1187 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @NUMERIC(/p/, /d/)[]@.
> pattern NUMERIC_ARRAY = 1231 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @TIMETZ@ or @TIME WITH TIME ZONE@.
> pattern TIMETZ = 1266 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @TIMETZ[]@ or @TIME WITH TIME ZONE[]@.
> pattern TIMETZ_ARRAY = 1270 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @BIT(/n/)@ used to represent fixed-length bit strings.
> pattern BIT = 1560 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @BIT(/n/)[]@.
> pattern BIT_ARRAY = 1561 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @BIT VARYING(/n/)@ or @VARBIT(/n/)@.
> pattern VARBIT = 1562 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @BIT VARYING(/n/)[]@ or @VARBIT(/n/)[]@.
> pattern VARBIT_ARRAY = 1563 :: ObjectID

> -- | Well-known object ID of the standard SQL type @NUMERIC(/p/, /d/)@.
> pattern NUMERIC = 1700 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @REFCURSOR@ used to represent references to cursors or portal names.
> pattern REFCURSOR = 1790 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @REFCURSOR[]@  used to represent arrays of references to cursors or portal names.
> pattern REFCURSOR_ARRAY = 2201 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @REGPROCEDURE@ used to represent registered procedures with arguments.
> pattern REGPROCEDURE = 2202 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @REGOPER@ used to represent registered operators.
> pattern REGOPER = 2203 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @REGOPERATOR@ used to represent registered operators with arguments.
> pattern REGOPERATOR = 2204 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @REGCLASS@ used to represent registered classes.
> pattern REGCLASS = 2205 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @REGTYPE@ used to represent registered types.
> pattern REGTYPE = 2206 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @REGPROCEDURE[]@ used to represent arrays of registered procedures with arguments.
> pattern REGPROCEDURE_ARRAY = 2207 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @REGOPER[]@ used to represent arrays of registered operators.
> pattern REGOPER_ARRAY = 2208 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @REGOPERATOR[]@ used to represent arrays of registered operators with arguments.
> pattern REGOPERATOR_ARRAY = 2209 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @REGCLASS[]@ used to represent arrays of registered classes.
> pattern REGCLASS_ARRAY = 2210 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @REGTYPE[]@ used to represent arrays of registered types.
> pattern REGTYPE_ARRAY = 2211 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @UUID@.
> pattern UUID = 2950 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @UUID[]@.
> pattern UUID_ARRAY = 2951 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @PG_LSN@.
> pattern PG_LSN = 3220 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @PG_LSN[]@.
> pattern PG_LSN_ARRAY = 3221 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @TSVECTOR@.
> pattern TSVECTOR = 3614 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @GTSVECTOR@.
> pattern GTSVECTOR = 3642 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @TSQUERY@.
> pattern TSQUERY = 3615 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @REGCONFIG@.
> pattern REGCONFIG = 3734 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @REGDICTIONARY@.
> pattern REGDICTIONARY = 3769 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @TSVECTOR[]@.
> pattern TSVECTOR_ARRAY = 3643 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @GTSVECTOR[]@.
> pattern GTSVECTOR_ARRAY = 3644 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @TSQUERY[]@.
> pattern TSQUERY_ARRAY = 3645 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @REGCONFIG[]@.
> pattern REGCONFIG_ARRAY = 3735 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @REGDICTIONARY[]@.
> pattern REGDICTIONARY_ARRAY = 3770 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @JSONB@.
> pattern JSONB = 3802 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @JSONB[]@.
> pattern JSONB_ARRAY = 3807 :: ObjectID

> -- | Well-known object ID of PostgreSQL type @TXID_SNAPSHOT@.
> pattern TXID_SNAPSHOT = 2970 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @TXID_SNAPSHOT[]@.
> pattern TXID_SNAPSHOT_ARRAY = 2949 :: ObjectID

> -- | Well-known object ID of PostgreSQL range type @INT4RANGE@.
> pattern INT4RANGE = 3904 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @INT4RANGE[]@.
> pattern INT4RANGE_ARRAY = 3905 :: ObjectID

> -- | Well-known object ID of PostgreSQL range type @NUMRANGE@.
> pattern NUMRANGE = 3906 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @NUMRANGE[]@.
> pattern NUMRANGE_ARRAY = 3907 :: ObjectID

> -- | Well-known object ID of PostgreSQL range type @TSRANGE@.
> pattern TSRANGE = 3908 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @TSRANGE[]@.
> pattern TSRANGE_ARRAY = 3909 :: ObjectID

> -- | Well-known object ID of PostgreSQL range type @TSTZRANGE@.
> pattern TSTZRANGE = 3910 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @TSTZRANGE[]@.
> pattern TSTZRANGE_ARRAY = 3911 :: ObjectID

> -- | Well-known object ID of PostgreSQL range type @DATERANGE@.
> pattern DATERANGE = 3912 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @DATERANGE[]@.
> pattern DATERANGE_ARRAY = 3913 :: ObjectID

> -- | Well-known object ID of PostgreSQL range type @INT8RANGE@.
> pattern INT8RANGE = 3926 :: ObjectID

> -- | Well-known object ID of PostgreSQL array type @INT8RANGE[]@.
> pattern INT8RANGE_ARRAY = 3927 :: ObjectID

  Pseudo-types
  ============

> -- | Well-known object ID of PostgreSQL pseudo-type @RECORD@.
> pattern RECORD = 2249 :: ObjectID

> -- | Well-known object ID of PostgreSQL array pseudo-type @RECORD[]@.
> pattern RECORD_ARRAY = 2287 :: ObjectID

> -- | Well-known object ID of PostgreSQL pseudo-type @CSTRING@.
> pattern CSTRING = 2275 :: ObjectID

> -- | Well-known object ID of PostgreSQL pseudo-type @ANY@.
> pattern ANY = 2276 :: ObjectID

> -- | Well-known object ID of PostgreSQL pseudo-type @ANYARRAY@.
> pattern ANYARRAY = 2277 :: ObjectID

> -- | Well-known object ID of PostgreSQL pseudo-type @VOID@.
> pattern VOID = 2278 :: ObjectID

> -- | Well-known object ID of PostgreSQL pseudo-type @TRIGGER@.
> pattern TRIGGER = 2279 :: ObjectID

> -- | Well-known object ID of PostgreSQL pseudo-type @EVENT_TRIGGER@.
> pattern EVENT_TRIGGER = 3838 :: ObjectID

> -- | Well-known object ID of PostgreSQL pseudo-type @LANGUAGE_HANDLER@.
> pattern LANGUAGE_HANDLER = 2280 :: ObjectID

> -- | Well-known object ID of PostgreSQL pseudo-type @INTERNAL@.
> pattern INTERNAL = 2281 :: ObjectID

> -- | Well-known object ID of PostgreSQL pseudo-type @OPAQUE@.
> pattern OPAQUE = 2282 :: ObjectID

> -- | Well-known object ID of PostgreSQL pseudo-type @ANYELEMENT@.
> pattern ANYELEMENT = 2283 :: ObjectID

> -- | Well-known object ID of PostgreSQL pseudo-type @ANYNONARRAY@.
> pattern ANYNONARRAY = 2776 :: ObjectID

> -- | Well-known object ID of PostgreSQL pseudo-type @ANYENUM@.
> pattern ANYENUM = 3500 :: ObjectID

> -- | Well-known object ID of PostgreSQL pseudo-type @FDW_HANDLER@.
> pattern FDW_HANDLER = 3115 :: ObjectID

> -- | Well-known object ID of PostgreSQL pseudo-type @ANYRANGE@.
> pattern ANYRANGE = 3831 :: ObjectID
