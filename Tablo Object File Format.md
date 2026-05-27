Tablo Object File Format
========================

Purpose
-------

This document describes the structure of compiled Tablo object files.

The current object file format is intentionally small. It is designed to be
easy to generate, easy to inspect, and easy for the reference VM to load. The
format will be expanded upon as I continue to develop the language and reference
implementation.

At present, a Tablo object file stores:

* a file header
* a format version number
* a single bytecode program

The current format does not yet include a constant data section, source-level
debug information, function tables, or section directories. These will be added as-and-when I need them.

Design Goals
------------

The object file format should:

* allow compiled Tablo code to be executed by a VM
* be simple enough to document and implement without ambiguity
* avoid depending on host architecture details such as native endianness

Conventions
-----------

Unless otherwise stated:

* all multi-byte integer values are encoded in little-endian byte order
* strings are encoded as UTF-8
* lengths and slot indices are unsigned
* byte offsets refer to absolute offsets within the object file

Current Format
--------------

The current object file format is laid out as follows:

1. magic bytes
2. format version
3. instruction count
4. serialized instruction stream

Header
------

The first 10 bytes of the file are:

* bytes `0..4`: magic bytes
* bytes `4..6`: 16-bit format version
* bytes `6..10`: 32-bit instruction count

Magic Bytes
-----------

The magic bytes are:

~~~text
TBO0
~~~

This identifies the file as a Tablo object file.

Format Version
--------------

At the time of writing, the current format version is still `1`.

The presence of a version field allows future implementations to reject object
files using an unsupported format. While Tablo is still in an early stage,
incompatible changes to the object file format will occur frequently.

Instruction Count
-----------------

The instruction count is a 32-bit unsigned integer giving the number of
serialized instructions that follow the header.

Instruction Stream
------------------

After the header, the file contains exactly the number of instructions
specified by the instruction count.

Each instruction is encoded as:

* a one-byte opcode
* zero or more operand bytes, depending on the opcode

No trailing data is currently permitted after the final instruction.

Primitive Encodings
-------------------

### Boolean

A Boolean value is encoded as a single byte:

* `0` = `false`
* `1` = `true`

Any other value is invalid.

### Integer

An integer value is encoded as a signed 64-bit integer.

### Decimal

A decimal value is currently encoded as:

* a signed 128-bit coefficient
* an unsigned 8-bit precision
* an unsigned 8-bit scale

This reflects the current structure used by the reference implementation.

### Text

A text value is encoded as:

* a 32-bit unsigned byte length
* exactly that many UTF-8 bytes

Opcodes
-------

The current opcode set is:

| Opcode | Mnemonic | Operands |
| --- | --- | --- |
| `1` | `Add` | none |
| `2` | `And` | none |
| `3` | `Divide` | none |
| `4` | `Equal` | none |
| `5` | `GreaterThan` | none |
| `6` | `GreaterThanOrEqual` | none |
| `7` | `LessThan` | none |
| `8` | `LessThanOrEqual` | none |
| `9` | `LoadLocal` | `u32 slot` |
| `10` | `Modulo` | none |
| `11` | `Multiply` | none |
| `12` | `PushBoolean` | `bool value` |
| `13` | `PushDecimal` | `decimal value` |
| `14` | `PushInteger` | `i64 value` |
| `15` | `StoreLocal` | `u32 slot` |
| `16` | `Subtract` | none |
| `17` | `Negate` | none |
| `18` | `NotEqual` | none |
| `19` | `Not` | none |
| `20` | `Or` | none |
| `21` | `Xor` | none |
| `22` | `Pop` | none |
| `23` | `PushText` | `text value` |

Object File Validity Rules
--------------------------

An object file is invalid if:

* the magic bytes are not `TBO0`
* the format version is unsupported
* the file ends before an instruction or operand is fully read
* a Boolean operand has a value other than `0` or `1`
* an opcode is unknown
* trailing data appears after the final instruction

The current format does not include internal checksums or signatures.

Example Layout
--------------

The following pseudo-layout shows the general shape of a current object file:

| Field | Value |
| --- | --- |
| Magic | TBO0 |
| Version | 1 |
| Instruction count | N |
| Instruction 0 | opcode + operands |
| Instruction 1 | opcode + operands |
| ... | ... |
| Instruction N - 1 | opcode + operands |

Future Evolution
----------------

The current object file format supports executing a single bytecode program, but
it does not yet support debugging.

Future revisions of the format will probably introduce a more structured layout,
most likely by adding one or more explicit sections after the file header. A
future object file may include sections such as:

* a code section
* a constant pool section
* a function table
* a source file table
* a debug information section

Debug Information
-----------------

In order to support debugging, future object files will likely need to record
metadata such as:

* source file names
* function names
* mappings from instruction offsets to source locations
* local variable names and lifetimes
* optionally, declared types for parameters and local variables

This debug information should be treated as supplementary metadata rather than
part of the core execution semantics of the bytecode itself.

In other words, the object file should eventually be able to answer questions
such as:

* Which source file and line produced this instruction?
* Which function is currently executing?
* Which local variables are in scope at this bytecode offset?

Those capabilities are expected to be added in a future revision rather than
the current one.
