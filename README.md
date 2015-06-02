PostgreSQL Wire Protocol
========================

    Copyright © 2015 Patryk Zadarnowski «pat@jantar.org».
    All rights reserved.

This is a pure Haskell implementation of the PostgreSQL wire protocol
for low-level interaction between PostgreSQL client and server.

It implements both the server and client endpoints of the protocol
(although the server endpoint is largely untested at this point in time),
abstracted at a number of levels, from raw message exchange to
transaction-oriented monadic interactions. It explicitly does NOT
hide any details of PostgreSQL's data type encoding, which are left
up to higher libraries implemented on top of `postgresql-wire`.

By interacting with the PostgreSQL server at this level, we hope to
avoid a number of limitations of PostgreSQL's official `libpq` library,
enabling use of Haskell's sophisticated concurrency platform for
pipelining of PostgreSQL messages, natural access from Haskell
to PostgreSQL asynchronous notifications, support for query
cancellation that actually works under the GHC virtual machine,
reduction to the amount of memory allocation and more efficient
use of mixed text and binary data exchange formats.
