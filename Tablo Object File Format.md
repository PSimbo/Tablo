Tablo Object File Format
========================

Purpose
-------

This document describes the structure of compiled Tablo object files. `tabloc --emit object` writes this encoding with a `.tobj` extension by default; the file extension does not affect decoding. Legacy `TBO0` files must be recompiled. These files currently contain complete programs, not independently linkable modules.

At present, a Tablo object file stores:

* a file header
* the producing Tablo version
* compiled functions and their return metadata
* the program entry point
* backend-specific database queries
* runtime object-type descriptors
* source-level debugging information

The current format does not yet include a constant data section. The compiler must emit constants directly in bytecode instructions.

Tablo library container
-----------------------

`tabloc --emit library` writes a separate `.tlib` container. Its layout is four magic bytes `TLB1`, a little-endian `u32` JSON manifest byte length, the UTF-8 JSON manifest, and one object payload using the encoding below. The manifest records the exact compiler package version in `compiler`. The `interface` string contains module object declarations and public function signatures with empty bodies. The `exports` array gives function indices in interface declaration order. The optional `objects` map binds each interface object name, including nested types, to its descriptor ID in the payload. An omitted map is empty. The object payload contains the compiled implementations and their dependencies, with an empty code entry point. Libraries do not require `Main`.

For each record-pointer parameter in the public interface, the `record_tables` array stores the resolved `database_name`, `schema_name` and `table_name`. Entries follow function declaration order and, within each function, parameter order, counting only record pointers. An omitted array is empty. The interface declaration supplies the parameter name and the `&rec` or `&rec mut` access mode. The compiler uses the stored identity even when the importing module has a different set of active databases.

The optional `parameter_sequences` array stores one entry per public `seq` parameter in the same declaration order, counting only sequence parameters. Each entry contains `database_name`, `schema_name`, `sequence_name` and the Boolean `schema_is_implicit`. The compiler checks the binding against the consumer's schema catalog when available. The implicit-schema flag determines whether compiled sequence operations use the schema name and must agree with that catalog. An omitted array is empty.

A library that declares objects records a `module` string containing a digest of the original source path and its filename. An imported object is identified by its declaration name and the SHA-256 digest of the declaring library container. Moving or copying a container preserves that identity. A different build that produces different container bytes has a distinct identity.

The optional `dependencies` manifest entry carries imported object types through subsequent links. Its `modules` map is keyed by declaring library digest. Each entry stores the module label, object declarations and imported name bindings. The outer `imports` map resolves names in the current interface, and `ids` maps the qualified object names to descriptor IDs in this payload. This lets a consumer resolve the same type through multiple libraries and use a library after its dependencies' containers have been removed.

The compiler validates the declarations and export references before importing a library. At static link time, exported declarations receive their compiled bodies, internal functions are appended, and function calls, query indices, and debug source indices are relocated. Consumers retain no runtime dependency on the container. The linker maps interface object IDs to the consumer’s resolved types and allocates IDs for implementation dependencies. It rewrites references in bytecode, descriptors, field defaults and debug scopes. The `TLB1` header identifies the library container independently of the object format version.

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

| Field                   | Encoding                                |
| ----------------------- | --------------------------------------- |
| Magic                   | four bytes containing `TBO1`            |
| Producing Tablo version | packed `u32` (see below)                |
| Function count          | `u32`                                   |
| Functions               | one function record per function        |
| Entry-point kind        | `u8`                                    |
| Entry-point payload     | depends on the entry-point kind         |
| Query count             | `u32`                                   |
| Queries                 | one query record per query              |
| Object-type count       | `u32`                                   |
| Object-type descriptors | one descriptor per runtime object type  |
| Debug information       | source-file and code-body debug records |

Trailing bytes after the debug information are invalid. The reader also accepts the end of the file immediately after the object-type descriptors, in which case the program has no debug information.

Version Encoding and Compatibility
----------------------------------

The header contains one shared Tablo `major.minor.patch` version, taken from the producing compiler's package version. There is no independently maintained object-format version. Pack it as `(major << 24) | (minor << 16) | patch` and write the resulting `u32` in little-endian order.

| Bits  | Component | Range   |
| ----- | --------- | ------- |
| 31–24 | Major     | 0–255   |
| 23–16 | Minor     | 0–255   |
| 15–0  | Patch     | 0–65535 |

For example, `18.52.22136` packs as `0x12345678` and is stored as bytes `78 56 34 12`. Components exceeding these ranges cannot be encoded and must not be truncated.

A reader accepts an ordinary artifact only if its major and minor match exactly and the reader's patch is at least the artifact's patch. Equivalently, the packed values must match under mask `0xFFFF0000`, and the artifact's value under mask `0x0000FFFF` must be no greater than the reader's. Patch releases must preserve decoding and execution of earlier patches in the same major/minor series.

Artifact version `0.0.0` bypasses the version comparison only. Structural validation and runtime requirements still apply. A reader at `0.0.0` does not bypass checks for ordinary versioned artifacts.

`TBO1` distinguishes this header from the experimental `TBO0` layout with its two-byte marker `1`. Readers reject that old layout; recompile old objects and libraries containing old object payloads. The magic identifies the header layout, not a second compatibility version. The same packed header is used in standalone objects, library payloads, and embedded or shared executable payloads. Changing a filename extension does not migrate its contents.

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

### Float

Runtime floating-point values are encoded as eight little-endian IEEE 754 binary64 bytes. A binary32 value is widened exactly when written; a 32-bit runtime narrows decoded values to binary32. Signed zero and infinities are retained. NaN payload preservation is not guaranteed across widths.

Source float literals instead retain their unrounded decimal spelling as `text`. The reader rounds this once to its native float width and caches the value. Instructions and object defaults reuse this cached value without parsing during arithmetic. This avoids premature rounding to the compiler's architecture when artifacts are loaded on another target.

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
| `7`  | `float`            | none                                               |
| `8`  | `int`              | none                                               |
| `9`  | `json`             | none                                               |
| `10` | nullable           | inner `type`                                       |
| `11` | object             | qualified name as `text`                           |
| `12` | range              | element `type`                                     |
| `13` | record pointer     | database, schema, and table as three `text` values |
| `14` | `text`             | none                                               |
| `15` | `time`             | none                                               |
| `16` | `timestamp`        | none                                               |
| `17` | `timestamptz`      | none                                               |
| `18` | `timetz`           | none                                               |
| `19` | union              | `u32` member count followed by member types        |

The internal null-literal type is not serializable. Nullability is represented by tag `10`; a runtime null value is produced by `PushNull`.

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
| `5`    | `BeginTry`               | catch instruction index `u32`, first local slot in the protected scope `u32`      |
| `6`    | `Call`                   | function index `u32`, argument count `u32`                                        |
| `7`    | `CallBuiltIn`            | built-in ID `u8`, argument count `u32`                                            |
| `8`    | `CommitTransaction`      | none                                                                              |
| `9`    | `ConvertFloat`           | none                                                                              |
| `10`   | `ConvertJson`            | destination `type`                                                                |
| `11`   | `ConvertObject`          | target object ID `u32`, private-access object IDs as `u32[]`                      |
| `12`   | `CreateRecord`           | none                                                                              |
| `13`   | `CreateRecordIfPending`  | none                                                                              |
| `14`   | `DeleteRecord`           | none                                                                              |
| `15`   | `Divide`                 | none                                                                              |
| `16`   | `Dup2`                   | none                                                                              |
| `17`   | `EndTry`                 | none                                                                              |
| `18`   | `Equal`                  | none                                                                              |
| `19`   | `ExecuteQuery`           | query index `u32`                                                                 |
| `20`   | `Exists`                 | none                                                                              |
| `21`   | `Fail`                   | none                                                                              |
| `22`   | `FieldPathExists`        | `text[]`                                                                          |
| `23`   | `GreaterThan`            | none                                                                              |
| `24`   | `GreaterThanOrEqual`     | none                                                                              |
| `25`   | `IterHasNext`            | none                                                                              |
| `26`   | `IterInit`               | none                                                                              |
| `27`   | `IterNext`               | none                                                                              |
| `28`   | `Jump`                   | target instruction index `u32`                                                    |
| `29`   | `JumpIfFalse`            | target instruction index `u32`                                                    |
| `30`   | `LessThan`               | none                                                                              |
| `31`   | `LessThanOrEqual`        | none                                                                              |
| `32`   | `LoadField`              | field name `text`                                                                 |
| `33`   | `LoadFieldPath`          | `text[]`                                                                          |
| `34`   | `LoadIndex`              | none                                                                              |
| `35`   | `LoadLocal`              | slot `u32`                                                                        |
| `36`   | `LoadProjectedValue`     | projected-value ID `u32`                                                          |
| `37`   | `LoadQuotedField`        | field name `text`                                                                 |
| `38`   | `LoadReference`          | slot `u32`                                                                        |
| `39`   | `LoadSequenceCurrent`    | `sequence`                                                                        |
| `40`   | `Locked`                 | none                                                                              |
| `41`   | `MakeArray`              | element count `u32`                                                               |
| `42`   | `MakeJsonArray`          | element count `u32`                                                               |
| `43`   | `MakeJsonObject`         | property names as `text[]`                                                        |
| `44`   | `MakeObject`             | object type ID `u32`, field names as `text[]`                                     |
| `45`   | `MakeRange`              | none                                                                              |
| `46`   | `MakeRecordPointer`      | field names `text[]`, field types `type[]`, `record type`, schema-implicit `bool` |
| `47`   | `MakeSteppedRange`       | none                                                                              |
| `48`   | `MemberPathExists`       | `u32` count, then field name `text` and quoted `bool` for each member             |
| `49`   | `Modulo`                 | none                                                                              |
| `50`   | `Multiply`               | none                                                                              |
| `51`   | `Negate`                 | none                                                                              |
| `52`   | `Not`                    | none                                                                              |
| `53`   | `NotEqual`               | none                                                                              |
| `54`   | `Or`                     | none                                                                              |
| `55`   | `Pop`                    | none                                                                              |
| `56`   | `PushBoolean`            | `bool`                                                                            |
| `57`   | `PushCurrentDate`        | none                                                                              |
| `58`   | `PushCurrentTime`        | none                                                                              |
| `59`   | `PushCurrentTimestamp`   | none                                                                              |
| `60`   | `PushCurrentTimestampTz` | none                                                                              |
| `61`   | `PushCurrentTimeTz`      | none                                                                              |
| `62`   | `PushDate`               | date value                                                                        |
| `63`   | `PushDecimal`            | decimal value                                                                     |
| `64`   | `PushEnumValue`          | inline constant, enum name `text`, variant name `text`                            |
| `65`   | `PushFloat`              | floating-point runtime value                                                      |
| `66`   | `PushFloatLiteral`       | unrounded literal `text`                                                          |
| `67`   | `PushInteger`            | `i64`                                                                             |
| `68`   | `PushJsonNumber`         | exact JSON number as `text`                                                       |
| `69`   | `PushNull`               | none                                                                              |
| `70`   | `PushText`               | `text`                                                                            |
| `71`   | `PushTime`               | canonical `text`                                                                  |
| `72`   | `PushTimestamp`          | canonical `text`                                                                  |
| `73`   | `PushTimestampTz`        | canonical `text`                                                                  |
| `74`   | `PushTimeTz`             | canonical `text`                                                                  |
| `75`   | `ReorderCallArguments`   | argument indices as `u32[]`                                                       |
| `76`   | `Return`                 | none                                                                              |
| `77`   | `ReturnNoValue`          | none                                                                              |
| `78`   | `StoreFieldPath`         | `text[]`                                                                          |
| `79`   | `StoreIndex`             | none                                                                              |
| `80`   | `StoreJsonPath`          | tagged mutation path (see below)                                                  |
| `81`   | `StoreLocal`             | slot `u32`                                                                        |
| `82`   | `StoreSequenceCurrent`   | `sequence`                                                                        |
| `83`   | `Subtract`               | none                                                                              |
| `84`   | `UpdateRecord`           | none                                                                              |
| `85`   | `UpdateRecordIfChanged`  | none                                                                              |
| `86`   | `Xor`                    | none                                                                              |
| `87`   | `BeginCatch`             | first local slot `u32`, clause count `u32`, catch clauses in source order         |
| `88`   | `BeginFinally`           | first local slot `u32`, cleanup instruction index `u32`                           |
| `89`   | `RunFinally`             | none                                                                              |
| `90`   | `ResumeFinally`          | none                                                                              |
| `91`   | `DiscardFinally`         | none                                                                              |
| `92`   | `PreserveDynamicType`    | object value type descriptor                                                      |

`BeginTry` installs a single unconditional handler. `BeginCatch` installs a list of handlers. Each clause contains a target instruction index (`u32`) and a binding-present flag (`bool`). A bound clause also contains an object value type descriptor and its local slot (`u32`). An unbound clause is the unconditional fallback. `EndTry` retires the active catch list on normal exit.

`Fail` pops and raises one non-null value. Runtime-generated failures carry a `RuntimeError` object with type ID `0xFFFFFFFF` and a public `message` field of type `text`. The VM selects the first matching clause, unwinds intervening frames and protected locals, rolls back transactions exited by the failure, and enters the handler with the value in its binding slot. `PreserveDynamicType` retains an array's declared type or materialises a decimal when a value enters a dynamic context. This preserves type identity for handler selection even for empty arrays.

`BeginFinally` registers a cleanup target. `RunFinally` transfers control to that target on normal exit, saving the continuation and any pending return operand. On exceptional exit the VM saves the failure and its original stack trace. `ResumeFinally` resumes the saved transfer. `DiscardFinally` abandons it when a control transfer from inside the cleanup block takes precedence.

`ConvertFloat` converts integers to the runtime's native float width, recursively handles arrays, and preserves nulls for compiler-approved nullable conversions. `PushFloatLiteral` loads its cached native value. Existing opcodes and the format version are unchanged.

Record pointer values share a record buffer. `StoreLocal` releases the previous pointer when it replaces a record pointer variable, including when the slot is reached through a reference parameter. If no other pointer variable retains that buffer, its pending insert or update is written at that point. Expression temporaries, query iterators, and debugger views do not retain buffers for this purpose. The compiler emits `PushNull` followed by `StoreLocal` to release record pointer variables on normal scope exit. These writes use the active transaction and are not performed during runtime-error unwinding. Explicit record writes update the shared buffer's status.

`MakeJsonArray` consumes the specified number of values from the stack and pushes a JSON array. `MakeJsonObject` consumes one value per encoded property name and pushes a JSON object. The compiler evaluates elements or property values once, in source order, before construction; stack values are paired with names in that same order. Compatible scalar values and existing JSON contents are copied into the new container. Property names retain their spelling and must be unique under exact, case-sensitive comparison.

`PushJsonNumber` pushes JSON-domain numeric contents from an exact JSON number token, without converting through a primitive integer, decimal, or floating-point representation. This instruction allows direct signed numeric literals to exceed primitive capacity. Arithmetic expressions continue to use ordinary numeric instructions. Scalar numeric contents become part of a document through the container construction instructions.

`StoreJsonPath` encodes a `u32` component count, followed by components tagged `0` for a field (`text` name and quoted `bool`) or `1` for an index (no inline payload). Before execution the stack holds the JSON root, each integer index in path order, and the assigned value. The instruction returns the assigned JSON contents followed by the updated root; `StoreLocal` writes that root back. Indices are evaluated once in path order, followed by the assigned expression. Failed writes do not publish the updated root.

`LoadIndex` also reads JSON arrays using one-based integer indices. It preserves JSON-domain contents for subsequent access or typed conversion, and rejects out-of-bounds indices and non-array JSON values. No additional opcode or version change is needed for JSON indexing.

`LoadField` uses case-insensitive lookup for JSON properties, rejecting ambiguous matches. `LoadQuotedField` uses exact lookup. Both raise runtime errors for missing JSON fields or non-object parents. `FieldPathExists` treats JSON names as unquoted; `MemberPathExists` preserves each member's quotedness. Presence checks return false for missing or untraversable paths, but still reject ambiguous JSON names.

`ConvertJson` converts JSON field contents to the destination type, enforcing JSON kind, nullability, and numeric representability. It also converts elements recursively when materializing an ordinary typed array of JSON field reads. The internal JSON field-content expression type is not a source-declarable or serialized type. Source JSON documents retain their original values after conversion.

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

Enum type descriptors retain the first variant's name and backing value so implicit enum defaults can be constructed without compiler declarations. Explicit field defaults remain separately encoded; an omitted field and an explicitly supplied `null` therefore remain distinguishable. This extends the initial-development descriptor encoding in place; previously compiled artifacts containing enum descriptors must be rebuilt.

`MakeObject` passes its named values to the shared runtime constructor. The constructor checks resolved field names and value types, fills omitted fields from their explicit defaults or type defaults, and preserves nominal object identity. Defaults may recursively construct nested objects; nullable fields and empty arrays terminate ordinary recursion. Invalid descriptor/default graphs fail with an object-field path rather than recursing indefinitely. Source visibility and identifier resolution remain compiler responsibilities. Existing compiler emission still evaluates complete field values in its established order, using the same shared default resolver.

Root-array defaults still produce ordinary arrays, and implicit union/range defaults remain unsupported. Non-nullable `any` also has no implicit default; `any?` defaults to null. This construction path does not introduce JSON-to-object coercion or define its extra-field, visibility, or union-selection policies.

Object value types are encoded recursively:

| Tag  | Type          | Payload                                  |
| ---- | ------------- | ---------------------------------------- |
| `1`  | `any`         | none                                     |
| `2`  | array         | element object value type                |
| `3`  | `bool`        | none                                     |
| `4`  | `date`        | none                                     |
| `5`  | `dec`         | none                                     |
| `6`  | enum          | enum name `text`, default variant `text`, inline backing constant, variant count `u32`, then each variant name `text` and inline backing constant |
| `7`  | `float`       | none                                     |
| `8`  | `int`         | none                                     |
| `9`  | `json`        | none                                     |
| `10` | nullable      | inner object value type                  |
| `11` | object        | referenced object type ID as `u32`       |
| `12` | range         | element object value type                |
| `13` | `text`        | none                                     |
| `14` | `time`        | none                                     |
| `15` | `timetz`      | none                                     |
| `16` | `timestamp`   | none                                     |
| `17` | `timestamptz` | none                                     |
| `18` | union         | `u32` member count followed by members   |

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
| `11` | float runtime value         | floating-point value                                  |
| `12` | float literal               | unrounded literal `text`                              |
| `13` | integer                     | `i64`                                                 |
| `14` | JSON document               | JSON serialized as `text`                             |
| `15` | null                        | none                                                  |
| `16` | object                      | object ID `u32`, field count `u32`, field/value pairs |
| `17` | text                        | `text`                                                |
| `18` | time                        | canonical `text`                                      |
| `19` | time with zone              | canonical `text`                                      |
| `20` | timestamp                   | canonical `text`                                      |
| `21` | timestamp with zone         | canonical `text`                                      |

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

Full debug information also includes object-conversion scope metadata after the code-body records. This begins with a `u32` scope count. Each scope contains:

| Field | Encoding |
| ----- | -------- |
| Has source name and optional name | present `text` |
| Visible object-type binding count | `u32` |
| Visible object-type bindings | `text` name followed by `u32` object-type ID per binding |
| Private-access type count | `u32` |
| Private-access types | one `u32` object-type ID per entry |

The bindings retain source-visible names, including imported and qualified inline types. The private-access IDs identify types declared in that source module, including nested types. Watches select this metadata using the selected stack frame's source name and use the ordinary runtime conversion descriptors. No source-file access is required. Line-only and stripped debug information omit these scopes. Readers accept older debug information ending before the scope count as having no conversion scopes; rebuild with full debug information to enable object-conversion watches. The format version is unchanged.

Validity Rules
--------------

An object file is invalid if:

* its magic bytes are not `TBO1`
* its version is unsupported
* it ends before a required field, instruction, or operand is complete
* a Boolean contains a value other than `0` or `1`
* a string is not valid UTF-8
* a tag, discriminant, opcode, or built-in identifier is unknown
* it contains more than one entry point or no entry point
* object-type descriptor IDs are duplicated
* an object-type descriptor or debug conversion scope references an object type ID that is not present
* trailing data remains after all records have been read

The current format does not include checksums, signatures, a section directory, or validation of every non-object cross-reference during decoding.

The experimental JSON conversion path uses `ConvertObject` to consume JSON contents and construct the selected object type. Its private-access IDs record the object declarations owned by the calling source module; nested conversion uses the same access set. Conversion enforces property matching, presence, nullability, and exact numeric representation before producing a result. Root-array types retain their existing ordinary-array runtime representation.

Enum descriptors now retain all declared variants, including duplicate backing values, so conversion can require exactly one matching variant. `CallBuiltIn(JsonCast, 1)` retains text parsing and additionally exports native objects and arrays using the program's descriptors. Exports include public fields only. These prototype changes evolve the artifact encoding without changing the format version; rebuild artifacts before testing. The provisional policies are recorded in `Temp/JSON Conversion Prototype.md`.
