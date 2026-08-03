Tablo Object File Format
========================

Purpose
-------

This document describes the structure of compiled Tablo object files.

At present, a Tablo object file stores:

* a file header
* a format version number
* compiled functions and their return metadata
* the program entry point
* backend-specific database queries
* runtime object-type descriptors
* source-level debugging information

The current format does not yet include a constant data section. The compiler must emit constants directly in bytecode instructions.

Conventions
-----------

Unless otherwise stated:

* all multi-byte integer values are encoded in little-endian byte order
* strings are encoded as UTF-8
* lengths, counts, indices, and slots are unsigned 32-bit integers
* instruction opcodes, tags, and discriminants use unsigned 8-bit integers
* a Boolean is one byte, where `0` is false and `1` is true
* a present flag is a Boolean followed by its value only when true
* byte offsets refer to absolute offsets within the object file

The notation used below is:

| Notation | Encoding                                                |
| -------- | ------------------------------------------------------- |
| `bool`   | one-byte Boolean                                        |
| `u8`     | unsigned 8-bit integer                                  |
| `u16`    | unsigned 16-bit integer                                 |
| `u32`    | unsigned 32-bit integer                                 |
| `i32`    | signed 32-bit integer                                   |
| `i64`    | signed 64-bit integer                                   |
| `i128`   | signed 128-bit integer                                  |
| `text`   | `u32` UTF-8 byte length followed by that many bytes     |
| `text[]` | `u32` element count followed by that many `text` values |
| `type`   | a data-type tag and its type-specific payload           |
| `code`   | a code body                                             |

Current Format
--------------

The current object file format is laid out as follows:

| Field                  | Encoding                                  |
| ---------------------- | ----------------------------------------- |
| Magic                  | four bytes containing `TBO0`              |
| Format version         | `u16`                                     |
| Function count         | `u32`                                     |
| Functions              | one function record per function          |
| Entry-point kind       | `u8`                                      |
| Entry-point payload    | depends on the entry-point kind           |
| Query count            | `u32`                                     |
| Queries                | one query record per query                |
| Object-type count      | `u32`                                     |
| Object-type descriptors| one descriptor per runtime object type    |
| Debug information      | source-file and code-body debug records   |

Trailing bytes after the debug information are invalid. The reader also accepts the end of the file immediately after the object-type descriptors, in which case the program has no debug information.

Function Records
----------------

Each function record contains:

| Field           | Encoding                              |
| --------------- | ------------------------------------- |
| Has name        | `bool`                                |
| Name            | `text` when `Has name` is true        |
| Has return type | `bool`                                |
| Return type     | `type` when `Has return type` is true |
| Body            | `code`                                |

`Has return type = false` represents a function that returns no value. Value-returning functions store their concrete return type. Function names are metadata but are preserved through an object-file round trip.

Entry Point
-----------

The entry-point kind has the following values:

| Value | Meaning              | Payload              |
| ----- | -------------------- | -------------------- |
| `0`   | Synthetic entry code | `code`               |
| `1`   | Function entry point | `u32 function index` |

Newly compiled Tablo applications normally use a function entry point. The synthetic-code form remains part of the internal bytecode model and object-file format.

Code Bodies
-----------

A code body contains:

| Field             | Encoding                                  |
| ----------------- | ----------------------------------------- |
| Instruction count | `u32`                                     |
| Instructions      | exactly that many serialized instructions |

Each instruction begins with a one-byte opcode followed by the payload shown in the opcode table.

Primitive Values
----------------

### Decimal

A decimal value is currently encoded as:

| Field       | Encoding |
| ----------- | -------- |
| Coefficient | `i128`   |
| Precision   | `u8`     |
| Scale       | `u8`     |

### Date

A date contains an `i32` year, `u8` month, and `u8` day.

### Time and Timestamp Values

`time`, `timetz`, `timestamp`, and `timestamptz` instruction operands are currently serialized as their canonical `text` representation.

### Inline Constants

Enum backing values use an inline constant encoding:

| Tag | Value   | Payload       |
| --- | ------- | ------------- |
| `1` | Boolean | `bool`        |
| `2` | Date    | date value    |
| `3` | Decimal | decimal value |
| `4` | Integer | `i64`         |
| `5` | Text    | `text`        |

Data Types
----------

Data types are encoded recursively:

| Tag  | Data type          | Payload                                            |
| ---- | ------------------ | -------------------------------------------------- |
| `1`  | `any`              | none                                               |
| `2`  | array              | element `type`                                     |
| `3`  | `bool`             | none                                               |
| `4`  | `date`             | none                                               |
| `5`  | `dec`              | none                                               |
| `6`  | empty-array marker | none                                               |
| `7`  | `int`              | none                                               |
| `8`  | nullable           | inner `type`                                       |
| `9`  | object             | qualified name as `text`                           |
| `10` | range              | element `type`                                     |
| `11` | record pointer     | database, schema, and table as three `text` values |
| `12` | `text`             | none                                               |
| `13` | `time`             | none                                               |
| `14` | `timestamp`        | none                                               |
| `15` | `timestamptz`      | none                                               |
| `16` | `timetz`           | none                                               |
| `17` | union              | `u32` member count followed by member types        |

The internal null-literal type is not serializable. Nullability is represented by tag `8`; a runtime null value is produced by `PushNull`.

Instruction Set
---------------

The following payload abbreviations are used:

* `sequence` means database `text`, schema-implicit `bool`, schema `text`, and sequence `text`
* `record type` means database `text`, schema `text`, and table `text`
* `type[]` means `u32` count followed by that many data types
* `u32[]` means `u32` count followed by that many `u32` values

| Opcode | Mnemonic                 | Payload                                                                           |
| ------ | ------------------------ | --------------------------------------------------------------------------------- |
| `1`    | `Add`                    | none                                                                              |
| `2`    | `AdvanceSequence`        | `sequence`                                                                        |
| `3`    | `And`                    | none                                                                              |
| `4`    | `BeginTransaction`       | none                                                                              |
| `5`    | `Call`                   | function index `u32`, argument count `u32`                                        |
| `6`    | `CallBuiltIn`            | built-in ID `u8`, argument count `u32`                                            |
| `7`    | `CommitTransaction`      | none                                                                              |
| `8`    | `CreateRecord`           | none                                                                              |
| `9`    | `CreateRecordIfPending`  | none                                                                              |
| `10`   | `DeleteRecord`           | none                                                                              |
| `11`   | `Divide`                 | none                                                                              |
| `12`   | `Dup2`                   | none                                                                              |
| `13`   | `Equal`                  | none                                                                              |
| `14`   | `ExecuteQuery`           | query index `u32`                                                                 |
| `15`   | `Exists`                 | none                                                                              |
| `16`   | `FieldPathExists`        | `text[]`                                                                          |
| `17`   | `GreaterThan`            | none                                                                              |
| `18`   | `GreaterThanOrEqual`     | none                                                                              |
| `19`   | `IterHasNext`            | none                                                                              |
| `20`   | `IterInit`               | none                                                                              |
| `21`   | `IterNext`               | none                                                                              |
| `22`   | `Jump`                   | target instruction index `u32`                                                    |
| `23`   | `JumpIfFalse`            | target instruction index `u32`                                                    |
| `24`   | `LessThan`               | none                                                                              |
| `25`   | `LessThanOrEqual`        | none                                                                              |
| `26`   | `LoadField`              | field name `text`                                                                 |
| `27`   | `LoadFieldPath`          | `text[]`                                                                          |
| `28`   | `LoadIndex`              | none                                                                              |
| `29`   | `LoadLocal`              | slot `u32`                                                                        |
| `30`   | `LoadProjectedValue`     | projected-value ID `u32`                                                          |
| `31`   | `LoadReference`          | slot `u32`                                                                        |
| `32`   | `LoadSequenceCurrent`    | `sequence`                                                                        |
| `33`   | `Locked`                 | none                                                                              |
| `34`   | `MakeArray`              | element count `u32`                                                               |
| `35`   | `MakeObject`             | object type ID `u32`, field names as `text[]`                                     |
| `36`   | `MakeRange`              | none                                                                              |
| `37`   | `MakeRecordPointer`      | field names `text[]`, field types `type[]`, `record type`, schema-implicit `bool` |
| `38`   | `MakeSteppedRange`       | none                                                                              |
| `39`   | `Modulo`                 | none                                                                              |
| `40`   | `Multiply`               | none                                                                              |
| `41`   | `Negate`                 | none                                                                              |
| `42`   | `Not`                    | none                                                                              |
| `43`   | `NotEqual`               | none                                                                              |
| `44`   | `Or`                     | none                                                                              |
| `45`   | `Pop`                    | none                                                                              |
| `46`   | `PushBoolean`            | `bool`                                                                            |
| `47`   | `PushCurrentDate`        | none                                                                              |
| `48`   | `PushCurrentTime`        | none                                                                              |
| `49`   | `PushCurrentTimestamp`   | none                                                                              |
| `50`   | `PushCurrentTimestampTz` | none                                                                              |
| `51`   | `PushCurrentTimeTz`      | none                                                                              |
| `52`   | `PushDate`               | date value                                                                        |
| `53`   | `PushDecimal`            | decimal value                                                                     |
| `54`   | `PushEnumValue`          | inline constant, enum name `text`, variant name `text`                            |
| `55`   | `PushInteger`            | `i64`                                                                             |
| `56`   | `PushNull`               | none                                                                              |
| `57`   | `PushText`               | `text`                                                                            |
| `58`   | `PushTime`               | canonical `text`                                                                  |
| `59`   | `PushTimestamp`          | canonical `text`                                                                  |
| `60`   | `PushTimestampTz`        | canonical `text`                                                                  |
| `61`   | `PushTimeTz`             | canonical `text`                                                                  |
| `62`   | `ReorderCallArguments`   | argument indices as `u32[]`                                                       |
| `63`   | `Return`                 | none                                                                              |
| `64`   | `ReturnNoValue`          | none                                                                              |
| `65`   | `StoreFieldPath`         | `text[]`                                                                          |
| `66`   | `StoreIndex`             | none                                                                              |
| `67`   | `StoreLocal`             | slot `u32`                                                                        |
| `68`   | `StoreSequenceCurrent`   | `sequence`                                                                        |
| `69`   | `Subtract`               | none                                                                              |
| `70`   | `UpdateRecord`           | none                                                                              |
| `71`   | `UpdateRecordIfChanged`  | none                                                                              |
| `72`   | `Xor`                    | none                                                                              |

Call Argument Plans
-------------------

Source-level call plans are not stored as a separate object-file structure. Omitted nullable arguments, declared defaults, named `default` requests, variadic materialization, and call-site evaluation order are resolved by the compiler. The resulting value-producing instructions and `ReorderCallArguments` instruction are serialized as ordinary bytecode.

This means the object file preserves the executable result of argument binding, not the original source spelling of a call.

Database Queries
----------------

Each query begins with a query-kind tag.

| Tag | Query kind |
| --- | ---------- |
| `1` | SQL query  |

An SQL query contains:

| Field                             | Encoding                                       |
| --------------------------------- | ---------------------------------------------- |
| Dialect                           | `u8`: SQLite `1`, PostgreSQL `2`, MySQL `3`    |
| Record lock mode                  | `u8`: none `0`, update `1`, update-no-wait `2` |
| Database name                     | `text`                                         |
| SQL statement                     | `text`                                         |
| Result shape                      | result-shape record                            |
| Parameter count and parameters    | `u32` followed by parameter records            |
| Grouping-item count and items     | `u32` followed by grouping records             |
| Scalar-projection count and items | `u32` followed by projection records           |
| Schema is implicit                | `bool`                                         |
| Schema name                       | `text`                                         |
| Table name                        | `text`                                         |

The SQL result-shape tags are:

| Tag | Shape                    | Additional payload |
| --- | ------------------------ | ------------------ |
| `1` | Integer scalar           | none               |
| `2` | Record pointer           | record layout      |
| `3` | Array of record pointers | record layout      |

A query parameter contains its `type`, field path as `text[]`, parameter index as `u32`, and local slot as `u32`.

A grouping item contains its `type` and grouping key names as `text[]`.

A scalar projection contains its result-column index as `u32`, its `type`, and its projected-value ID as `u32`.

### Record Layout

A record layout contains a schema description followed by a column selection.

Schema description tags:

| Tag | Meaning                   | Payload                                       |
| --- | ------------------------- | --------------------------------------------- |
| `1` | Known schema              | `u32` column count followed by column records |
| `2` | Runtime-determined schema | none                                          |

Each known column record contains the column name as `text`, its `type`, a nullable `bool`, and a primary-key `bool`.

Column selection tags:

| Tag | Meaning                      | Payload |
| --- | ---------------------------- | ------- |
| `1` | All columns                  | none    |
| `2` | Selected indices             | `u32[]` |
| `3` | Runtime-determined selection | none    |

Object-Type Descriptors
-----------------------

Each runtime object type has a stable object type ID. A descriptor contains:

| Field        | Encoding                          |
| ------------ | --------------------------------- |
| Object ID    | `u32`                             |
| Display name | `text`                            |
| Shape        | shape tag and shape-specific data |

The display name is the source-facing declaration name for an object from the root module and is module-qualified for an object originating in an imported module. It is intended for diagnostics and debugger views; object identity is always determined by the object ID.

Shape tags are:

| Tag | Shape       | Payload                                      |
| --- | ----------- | -------------------------------------------- |
| `1` | Fields      | `u32` field count followed by field records  |
| `2` | Root array  | one object value type                        |

Each field record contains its name as `text`, quoted-identifier status as `bool`, public visibility as `bool`, object value type, and an optional explicit default encoded as a present `object default`. Private fields remain in the descriptor because visibility controls source access rather than runtime shape.

Object value types are encoded recursively:

| Tag  | Type          | Payload                                  |
| ---- | ------------- | ---------------------------------------- |
| `1`  | `any`         | none                                     |
| `2`  | array         | element object value type                |
| `3`  | `bool`        | none                                     |
| `4`  | `date`        | none                                     |
| `5`  | `dec`         | none                                     |
| `6`  | enum          | enum display name as `text`              |
| `7`  | `int`         | none                                     |
| `8`  | nullable      | inner object value type                  |
| `9`  | object        | referenced object type ID as `u32`       |
| `10` | range         | element object value type                |
| `11` | `text`        | none                                     |
| `12` | `time`        | none                                     |
| `13` | `timetz`      | none                                     |
| `14` | `timestamp`   | none                                     |
| `15` | `timestamptz` | none                                     |
| `16` | union         | `u32` member count followed by members   |

Explicit object defaults use the following recursive encoding:

| Tag  | Value                       | Payload                                               |
| ---- | --------------------------- | ----------------------------------------------------- |
| `1`  | array                       | `u32` value count followed by object defaults         |
| `2`  | Boolean                     | `bool`                                                |
| `3`  | current date                | none                                                  |
| `4`  | current time                | none                                                  |
| `5`  | current time with zone      | none                                                  |
| `6`  | current timestamp           | none                                                  |
| `7`  | current timestamp with zone | none                                                  |
| `8`  | date                        | date value                                            |
| `9`  | decimal                     | decimal value                                         |
| `10` | enum                        | inline constant, enum name `text`, variant `text`     |
| `11` | integer                     | `i64`                                                 |
| `12` | null                        | none                                                  |
| `13` | object                      | object ID `u32`, field count `u32`, field/value pairs |
| `14` | text                        | `text`                                                |
| `15` | time                        | canonical `text`                                      |
| `16` | time with zone              | canonical `text`                                      |
| `17` | timestamp                   | canonical `text`                                      |
| `18` | timestamp with zone         | canonical `text`                                      |

Debug Information
-----------------

Debug information begins with a `u32` source-file count. Each source-file record contains:

| Field            | Encoding                         |
| ---------------- | -------------------------------- |
| Display name     | `text`                           |
| Line-start count | `u32`                            |
| Line starts      | one `u32` source offset per line |

The source-file records are followed by a `u32` code-body debug-record count. Each code-body debug record contains:

| Field                                    | Encoding             |
| ---------------------------------------- | -------------------- |
| Has body name and optional name          | present `text`       |
| Has source-file index and optional index | present `u32`        |
| Instruction-position count               | `u32`                |
| Instruction source positions             | one `u32` per entry  |
| Local-variable count                     | `u32`                |
| Local-variable records                   | one record per local |

Each local-variable record contains:

| Field                      | Encoding |
| -------------------------- | -------- |
| Name                       | `text`   |
| Slot                       | `u32`    |
| Declared type display name | `text`   |
| Is constant                | `bool`   |
| Scope start instruction    | `u32`    |
| Scope end instruction      | `u32`    |

Validity Rules
--------------

An object file is invalid if:

* its magic bytes are not `TBO0`
* its version is unsupported
* it ends before a required field, instruction, or operand is complete
* a Boolean contains a value other than `0` or `1`
* a string is not valid UTF-8
* a tag, discriminant, opcode, or built-in identifier is unknown
* it contains more than one entry point or no entry point
* object-type descriptor IDs are duplicated
* an object-type descriptor references an object type ID that is not present
* trailing data remains after all records have been read

The current format does not include checksums, signatures, a section directory, or validation of every non-object cross-reference during decoding.
