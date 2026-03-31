Tablo Language Reference
========================

Introduction
------------

The Tablo language is a procedural language with first-class support for database access operations. You can write your database query code right within your procedural code and have both type-checked at compile time.

The syntax aims to be familiar to users of C-like languages such as C, C++, C#, Java, Javascript/Typescript, Go, etc. while the query syntax aims to be as similar to standard SQL as possible.

This file details the syntax of the Tablo language.

### A Simple Example

~~~
with exampledb;

obj ForumPost {
  id: int,
  author: text,
  "timestamp": timestamp,
  msg: text,
  comments: [
    {
      "timestamp": timestamp,
      comment: text,
    }
  ],
}

fn FindPosts(id: int, madeBy: int, since: date, until: date) [ForumPost]! {
  // If the user gets the dates in the wrong order, swap them.
  var tempDate: date = null;

  if until < since {
    tempDate = since;
    since = until;
    until = tempDate;
  }

  var mut forumPosts: [ForumPost]! = [];

  for rec post in posts
                  where posts.id = id
                    and author = madeBy
                    and "date" >= since
                    and "date" <= until
                  order by "date" desc, "time" desc {
    var forumPost: ForumPost = {
      id: post.id,
      author: post.author,
      "timestamp": timestamp(post.date, post.time),
      msg: post.message,
      comments: [],
    };

    for rec comment in comments
                       where comments.id = post.id
                       order by "timestamp" {
      forumPost.comments += {
        comment: comment.text,
        "timestamp": timestamp(comment.date, comment.time),
      };
    }

    forumPosts += forumPost;
  }

  return forumPosts;
}
~~~

Note that this project is somewhat inspired by the OpenEdge Progress ABL language but aims to be significantly more user-friendly as well as dropping the requirement to have an active database connection in order to compile the code.

Core Concepts
-------------

The language itself does not constrain the output that may be generated. For example, the code may be transpiled to another language (e.g. C#), where the first-class database access code is replaced with some idiomatic means of accessing the database(s). Alternatively, the code could be run in an interpreter with an open connection to the database(s). Or the code could be compiled into a standalone shared library. Or, perhaps support could be added to a particular database system to allow stored procedures to be written using Tablo syntax. In short, the language itself is unopinionated about how it may be used.

Many of the design decisions that drive the development of Tablo stem from the fact that code may need to be transpiled to a completely different language and needs to support database backends with differing capabilities. Therefore, it makes as few assumptions as possible about the capabilities of the target language and database backend.

Comments
--------

Tablo's syntax supports both single line and multi-line comments. As you would expect, comments have no semantic meaning. They are purely for the benefit of the programmer.

Single-line comments begin with `//` and continue until the next new line, the start of a multi-line comment, or the end of the file. If the `//` is not the first token on the line then the preceding lines are interpreted as normal. Only the `//` itself and the subsequent text is ignored.

Multi-line comments start with `/*` and end with `*/` and may span multiple lines. A multi-line comment may contain other multi-line comments.

Neither type of comment may begin inside a string literal.

~~~
obj Comments {
  singleLine: comments.single, // This is a single-line comment.
  /* Skip this:
  multiLine: comments.multi,
  */
}
~~~

Whitespace
----------

Whitespace has no semantic meaning and is ignored during parsing.

Primitive Types
---------------

Tablo supports the following primitive data types:

| Type          | Description              |
|---------------|--------------------------|
| `bin`         | Binary data              |
| `bool`        | Boolean                  |
| `date`        | Date                     |
| `dec`         | Decimal                  |
| `float`       | floating point           |
| `int`         | integer                  |
| `json`        | JSON                     |
| `text`        | Text                     |
| `time`        | Time                     |
| `timestamp`   | Timestamp                |
| `timestamptz` | Timestamp with time-zone |
| `timetz`      | Time with time-zone      |
| `uuid`        | Universally unique ID    |

It is up to the Tablo interpreter to determine how these types map to the available data types in the connected database and target language. Some target languages may support options by which the type mapping can be modified. In rare cases, some data types may be unsupported for a certain database type or target language.

Note that there are no separate types for `float`s and `int`s of different byte lengths. Both types are always treated as having the largest byte length supported by the operating system. Also note that integers do not overflow or underflow. Adding to an integer such that the results exceeds the maximum value results in the value being set to the maximum value. If a `float` or `int` is assigned to a database field that lacks the precision to store the value, the assigned value is clamped to the supported range.

Likewise, due to limited support in backend databases, Tablo does not have a dedicated type for unsigned integers.

Decimals that are read from a database field internally store the precision and scale of field. Uninitialized variables of type `dec` default to a precision of 7 and a scale of 2. Binary operators involving two decimals will typically produce a `dec` value whose precision and scale are such that the number of whole digits (precision minus scale) and the number of fractional digits are both the maxima of those for the operands. For example, multiply a decimal with precision 5 and scale 6 (-1 whole digits) with a decimal with precision 7 and scale 3 (4 whole digits) results in a decimal with precision 10 and scale 6 (4 whole digits). If a `dec` is assigned to a database field that lacks the range to store the value, the assigned value is clamped to the supported range. If the database field lacks the precision to store the value, the assigned value is truncated to the supported precision.

Note that the `json` data type does not require that the database backend have explicit support for storing JSON data. Tablo provides functions for converting JSON data to and from strings.

Beyond the primitive data types listed in this section, custom data types may be defined as objects (see the "Objects" section).

Technically, Tablo supports one more primitive data type: the record pointer. However, the semantics for record pointers are quite different compared to other variables. For one, a record pointer is declared using the `rec` keyword rather than the `var` keyword and is immutable unless the `mut` keyword is also specified. Record pointers are described further in the "Database Queries" section.

Identifiers
-----------

Identifiers are names that represent data entities such as variables, tables, fields, and records. There are two types: quoted and unquoted.

Unquoted identifiers may only contain the characters "A" to "Z", "a" to "z", "0" to "9", and "_". The first letter must not be a digit. Unquoted identifiers are not case-sensitive.

Quoted identifiers are delimited by `"` characters and may contain one or more Unicode characters (including new lines). Two `"` characters immediately next to one another are not considered delimiters and are treated as a literal `"` character instead. Quoted identifiers are case-sensitive.

Note that some tokens that match the definition of an identifier are considered to be reserved words and are not treated as identifiers by the compiler.

Reserved Words
--------------

Some identifiers are reserved as keywords within the syntax of the language. Keywords are always unquoted, formed of alphanumeric characters only, and are case-insensitive. If a quoted identifier matches a keyword in all respects except for the fact that it is quoted rather than unquoted then the identifier is valid and will not be treated as a keyword.

The following identifiers are reserved as keywords:

| Keyword       | Description                                         |
|---------------|-----------------------------------------------------|
| `and`         | Logical AND                                         |
| `as`          | Defines an alias for a query sub-expression         |
| `asc`         | Marks the sort order for a field as ascending       |
| `bin`         | Binary data data type                               |
| `bool`        | Boolean data type                                   |
| `by`          | Follows keywords `group` or `order`                 |
| `const`       | Starts a constant variable declaration              |
| `count`       | Returns the number of records matching a query      |
| `create`      | Commits a database record creation                  |
| `date`        | Date data type                                      |
| `dec`         | Decimal data type                                   |
| `delete`      | Commits a database record deletion                  |
| `desc`        | Marks the sort order for a field as descending      |
| `else`        | Starts an else-block for an if statement            |
| `fail`        | Exits a function by throwing an error               |
| `false`       | A literal false value                               |
| `first`       | Follows keyword `find`                              |
| `float`       | floating-point data type                            |
| `fn`          | Starts a function definition                        |
| `for`         | Starts a for-loop statement                         |
| `group`       | Starts a GROUP BY clause                            |
| `if`          | Starts an if statement                              |
| `in`          | Accesses elements in an iterator                    |
| `int`         | integer data type                                   |
| `json`        | JSON data type                                      |
| `last`        | Follows keyword `find`                              |
| `limit`       | Specifies the LIMIT value for a query               |
| `mut`         | Marks a variable as mutable                         |
| `not`         | Logical NOT                                         |
| `null`        | A literal null value                                |
| `obj`         | Starts an object definition                         |
| `or`          | Logical OR                                          |
| `order`       | Starts an ORDER BY clause                           |
| `pub`         | Marks a function as public                          |
| `rec`         | Record pointer data type                            |
| `return`      | Exits a function by returning a result              |
| `text`        | String data type                                    |
| `time`        | Time data type                                      |
| `timestamp`   | Timestamp data type                                 |
| `timestamptz` | Timestamp (with time-zone) data type                |
| `timetz`      | Time (with time-zone) data type                     |
| `true`        | A literal true value                                |
| `update`      | Commits a database record update                    |
| `uuid`        | UUID data type                                      |
| `var`         | Starts a variable declaration                       |
| `where`       | Starts a WHERE clause                               |
| `while`       | Starts a while-loop statement                       |
| `with`        | Specifies the active databases for table resolution |
| `xor`         | Logical XOR                                         |

Keyword Literals
----------------

Variables of type `bool` may be assigned the literal values `true` and `false`.

Unless marked as non-null, a variable of any data type may be assigned the null value. This is done using the reserved word `null`.

Unless marked as required, an argument of any data type my hold a void value. The void value cannot be manually assigned. Only arguments that have not been provided when a function is called receive the void value. For the purposes of conversion to and from JSON, the void value is equivalent to `undefined`.

Integer Literals
----------------

An integer literal is represented as a sequence of digits, which may be broken only by single `_` characters.

Unless otherwise specified, digits are interpreted as base-10 digits. Base-16 (hexadecimal), base-8 (octal), and base-2 (binary) digits are also supported. To specify a hexadecimal integer, prefix it with `0x`. To specify an octal integer, prefix it with `0o`. To specify a binary integer, prefix it with `0b`.

Negative integers are specified by prefixing the digits with a `-`. If a prefix is required to specify the base, the `-` is placed before the base prefix.

Attempting to assign an integer literal to a variable of a type for which the integer is outside the supported range results in a compile error.

At present, exponential notation is not supported.

Decimal Literals
----------------

A decimal literal is represented as, at minimum, a `.` followed by a sequence of digits. Optionally, the `.` may be preceded by another sequence of digits. If omited, the initial sequence of digits is treated as being equivalent to `0`. Each sequence of digits may only be broken by single `_` characters.

Negative values are specified be prefixing the digits with a `-`.

Attempting to assign a decimal literal to a variable of a type for which the number is outside the supported range results in a compile error.

At present, only base-10 digits are supported. Exponential notation is not supported.

String Literals
---------------

Strings are delimited with `'` characters and may contain any characters except the character with code zero. Some characters need to be escaped. Strings support interpolation using the `${` and `}` delimiters. Any valid expression may be placed within the string interpolation delimiters.

Note that there is no special syntax for multi-line strings. Any new line characters in the strings remain in the assigned value. For any string that spans mutliple lines, leading whitespace is trimmed up to the number of whitespace characters preceding either the initial or final `'` (whichever is smaller).

The following escape sequences are supported:

| Sequence | Description     |
|----------|-----------------|
| `\'`     | Single quote    |
| `\\`     | Backslash       |
| `\b`     | Backspace       |
| `\f`     | Form feed       |
| `\n`     | New line        |
| `\r`     | Carriage return |
| `\t`     | Tab             |
| `\v`     | Vertical tab    |

A single string literal may be composed of several string literals via concatenation using the `+` operator. In fact, the use of the binary `+` operator where only one of the operands is a string will result in the other operand being automatically converted to a string for concatenation.

Date & Time Literals
--------------------

Date literals are prefixed with `@` and expressed in `<year>-<month>-<day>` format.

Time literals are also prefixed with `@` and expressed in `<hour>:<minute>:<second>.<fractional part>` format. The fractional part may be omitted, in which case the fractional part is set to zero. If the fractional part is omited then the seconds may also be omitted, in which case both the fractional part and seconds are set to zero.

Time literals may also include a time-zone suffix expressed as either `+` or `-` and followed by an offset in `<hour>:<minute>` format. This is not required when assigning to `timetz` values. If omitted, the time-zone defaults to the default specified in the `--tz` compiler argument or to the current system locale time-zone if no default is specified. Specifying a time-zone when assigning to a `time` value results in a compile error.

Timestamp literals are also prefixed with `@` and expressed in ISO-8601 format. Specifying a time-zone is only valid when assigning to a `timestamptz` value, otherwise a compiler error is thrown.

~~~
var exampleDate: date = @1981-04-05;
var exampleTime: time = @12:34:56.98765;
var exampleTimeTz: timetz = @11:22:33+04:30;
var exampleTimestamp: timestamp = @2019-11-28T05:19:03;
var exampleTimestampTz: timestamptz = @2009-01-09T13:47Z;
~~~

Operators
---------

### Equality Operators

The `==` operator is used to compare two values for equality. It evaluates to `true` if the values are equal.

The `!=` operator is used to compare two values for inequality. It evaluates to `false` if the values are equal.

Equality comparison is supported for primitive scalar types such as numeric values, `bool`, date/time values, and `text`.

For `text` values, equality comparison is case-sensitive. Two `text` values are equal if and only if they contain the same sequence of characters.

If a case-insensitive text equality comparison is required, the expected approach is to normalize the casing of both operands explicitly. For example: `lower(a) == lower(b)`.

### Comparison Operators

The `>`, `>=`, `<`, and `<=` operators are used to compare the ordering of two values. Operands must be numeric, date/time, or `text`.

The `>` operator evaluates to `true` if the left value is strictly greater than the right. Operands must be numeric, date/time, or `text`.

The `>=` operator evaluates to `false` if the left value is strictly less than the right. Operands must be numeric, date/time, or `text`.

The `<` operator evaluates to `true` if the left value is strictly less than the right. Operands must be numeric, date/time, or `text`.

The `<=` operator evaluates to `false` if the left value is strictly greater than the right. Operands must be numeric, date/time, or `text`.

For `text` values, the comparison is guaranteed to be case-sensitive but the exact implementation is dependent on the database backend. If portability is a concern, code should not rely on a subtle ordering details of `text` value ordering.

If a case-insensitive text ordering comparison is required, the expected approach is to normalize the casing of both operands explicitly. For example: `lower(a) < lower(b)`.

### Logical Operators

The `and` operator evaluates to the result of a logical AND of the two operands. Operands must be of type `bool`.

The `or` operator evaluates to the result of a logical OR of the two operands. Operands must be of type `bool`.

The unary prefix `not` operator evaluates to the result of a logical NOT of its operand. Operand must be of type `bool`.

The `xor` operator evaluates to the result of a logical XOR of the two operands. Operands must be of type `bool`.

### Mathematical Operators

The `+` operator evaluates to the sum of its operands. In the case of strings, it is used for concatenation. Operands must be numeric, date/time, or `text`. The operands may be dissimilar if both types are numeric.

The `-` operator evaluates to the difference of its operands. As a unary perfix operator, it evaluates to the numeric negation of its operand. Operands must be numeric or date/time. The operands may be dissimilar if both types are numeric.

The `*` operator evaluates to the product of its operands. Operands must be numeric and may be dissimilar.

The `/` operator evaluates to the quotient of its operands. Operands must be numeric and may be dissimilar.

The `%` operator evaluates to the modulus of its operands. Operands must be numeric and may be dissimilar.

### Assignment Operators

The `=` operator is used to assign the expression on the right to the expression on the left.

The `+=` operator evaluates the sum of its operands and assigns the result to the left operand.

The `-=` operator evaluates the difference of its operands and assigns the result to the left operand.

The `*=` operator evaluates the product of its operands and assigns the result to the left operand.

The `/=` operator evaluates the quotient of its operands and assigns the result to the left operand.

The `%=` operator evaluates the modulus of its operands and assigns the result to the left operand.

### Column Access Operator

The `.` operator is used to express that the column on the right exists in the table on the left. Operands must be identifiers.

### Operator Precedence

~~~
.
not
* / %
+ -
== != > >= < <
and
xor
or
= += -= *= /= %=
~~~

### Automatic Type Conversions

Tablo supports a fairly limited set of automatic type conversions. In general, an explicit conversion is required when using one type in a place where another is expected.

Numeric values may be implicitly converted between `int` and `dec` where necessary. In these cases, the `int` operand is automatically converted to a `dec` representation using the same scale and precision as the `dec` operand.

Note that `float` and `dec` are not implicitly converted to one another. Any operation involving both `float` and `dec` requires an explicit conversion.

Tablo does not automatically convert `text` values to numeric or date/time values. Built-in functions exist for parsing strings as these types.

When the `+` operator is used and either operand is of type `text`, the other operand is implicitly converted to `text` and the result is the concatenation of the operands. Operator precedence rules still apply before this conversion is considered. For example, `1 + 2 + ' foo'` evaluates to `'3 foo'`, not `'12 foo'`.

In all of the above cases, if both operands are non-nullable then the result is also a non-nullable type. If either or both operands are nullable then the result is a nullable type.

With one notable exception, no automatic conversions occur when passing arguments to functions. A function must receive exactly the types it expects or a compile error results. The aforementioned exception is the automatic conversion from a nullable type to the corresponding non-nullable type. If a function expects a non-nullable value and the compiler can infer that the provided value cannot be null then the type is automatically converted. Alternatively, the `!` operator may be applied to an expression to assert that it is non-null. If the `!` operator is used on an expression that evaluates to null at runtime then the program fails.

Variables
---------

The `var` keyword is used to declare a new variable. At present, the data type must be specified. Optionally, an initial value may be assigned. If no initial value is assigned, the variable contains a null value.

Alternatively, the `const` keyword may be used to declare a new constant. Unlike variables declared with `var`, the value of a constant may not be modified.

~~~
var boolean: bool; // Initial value is `null`.
const integer: int = 5;
~~~

A variable may be declared as non-nullable by adding a `!` after the type name. In this case, if no initial value is assigned, the variable contains a non-null default value.

~~~
var decimal: dec!; // Initial value is 0.0.
var string: text!; // Initial value is '';
~~~

The non-null default values for each data type are:

| Type          | Default Value                   |
|---------------|---------------------------------|
| `bin`         | Empty binary sequence           |
| `bool`        | `false`                         |
| `date`        | The current local date          |
| `dec`         | `0.0` (precision: 7, scale: 2)  |
| `float`       | `0.0`                           |
| `int`         | `0`                             |
| `json`        | `{}`                            |
| `text`        | `''`                            |
| `time`        | The current local time          |
| `timestamp`   | The current local date and time |
| `timestamptz` | The current local date and time |
| `timetz`      | The current local time          |
| `uuid`        | A new random UUID (v4)          |

Objects
-------

Tablo supports a somewhat struct-like, somewhat JSON-like construct called "objects". Objects are like structs in the sense that they allow the users to create custom data types with a defined structure. Objects are like JSON in the sense that the outermost element may be an array and the internal structure supports deep nesting. You can think of objects as strongly-typed JSON data.

~~~
obj LocationData {
  addressLines: [text]! | {
    line1: text,
    line2: text,
    line3: text,
    line4: text,
  },
  name: text!,
  postalCode: text,
};

obj CustomerCollection [
  {
    id: int!,
    name: text,
    location: LocationData,
  }
];
~~~

Tablo supports bi-directional conversion between JSON data and objects.

Arrays
------

Tablo supports simple 1D arrays. For database backends that do not support arrays (i.e. anything except PostgreSQL), arrays may be used but no attempt is made to add support for assigning array values to database fields.

~~~
var array1d: int[] = [1, 2, 4, 8];
~~~

At present, there is no support for multi-dimensional arrays.

Scopes
------

Scopes in Tablo are lexical. A variable or function is valid only within the scope in which it is declared and within any nested scopes.

Both variables and functions may be declared either at file scope or within any nested scope.

If a variable within a nested scope matches the name of a variable in an outer scope then the variable in the nested scoped "shadows" the variable in the outer scope. Until the end of the nested scope, references to the variable will resolve to that scope's variable. After the nested scope ends, references to the variable will once again resolve to the outer scope's variable.

Within a given scope, a function may be called before its declaration appears in the source code. Functions do not need to be forward-declared and Tablo provides no syntax for doing so.

Anonymous scopes are supported and are created using a bare block:

~~~
{
  var i: int = 42;
}
~~~

Anonymous scopes are particularly useful for controlling the lifetime of mutable record pointers and, therefore, the point at which pending database changes are automatically committed.

If Statements
-------------

Tablo supports if statements. There is no need to enclose the condition in parentheses.

~~~
var allowAnyDates: bool = true;
var curDate: date = today();

if allowAnyDates {
  ...
}
else if curDate > @2029-12-31 or curDate < @2000-01-01 {
  ...
}
else {
  ...
}
~~~

For Loops
---------

Tablo supports for loops. For loops always use the `in` operator where the right operand is any iterable.

~~~
for i in 0...10 {
    var fib: int[] = [1, 1, 2, 3, 5, 8];

    for val in fib {
        ...
    }
}
~~~

Database Queries
----------------

Database queries are implicitly executed when any of the following code structures are encountered:

* `for` loop
* `find [first | last]` statement
* `count` statement

Database modifications are executed by the following statements:

* `create` statement
* `update` statement
* `delete` statement

### Query Syntax

#### Where Clause

A `where` clause filters the records considered by a database query. The syntax is intentionally similar to SQL. The expression following the `where` keyword must evaluate to a `bool` value.

~~~
for rec post in posts where posts.id = id and "date" >= since {
  ...
}
~~~

The expression in a `where` clause uses normal Tablo expression syntax. This includes literals, variables, field references, comparison operators, logical operators, grouping with parentheses, and function calls.

Any unquoted field references that match the name of a variable or function in scope must be specified as `<table>.<field>`.

In nested database queries, a `where` clause may reference fields from record pointers declared in the enclosing scope(s).

Any function used within a `where` clause must be valid for use in query expressions. The exact set of functions that may be used in query expressions is defined in the "Built-In Functions" section (TODO: update the Built-In Functions section).

If a `where` clause contains an expression that cannot be converted into valid query syntax for the backend database then compilation fails.

#### Order By Clause

An `order by` clause specifies the order in which records are returned by a database query. The syntax is intentionally similar to SQL.

~~~
for rec post in posts
                where posts.id = id
                order by "date" desc, "time" desc {
  ...
}
~~~

An `order by` clause consists of one or more comma-separated expressions. Each expression may optionally be followed by either `asc` or `desc` to specify ascending or descending order respectively. If neither is specified then ascending order is assumed.

The expressions in an `order by` clause use normal Tablo expression syntax. The exact set of functions that may be used in query expressions is defined in the "Built-In Functions" section.

If an `order by` clause contains an expression that cannot be converted into valid query syntax for the backend database then compilation fails.

If no `order by` clause is specified then Tablo makes no guarantee about the order in which records are returned.

#### Group By Clause

A `group by` clause partitions the records returned by a database query into contiguous groups. Unlike SQL, the purpose of grouping in Tablo is not to merge multiple records into a single aggregate row. Each iteration of the query still corresponds to a single database record.

~~~
for rec cust in customers
             group by lower(countryCode) as country, city {
  if firstof(country) {
    ...
  }

  if lastof(country, city) {
    ...
  }
}
~~~

The expressions in a `group by` clause use normal Tablo expression syntax. The exact set of functions that may be used in query expressions is defined in the "Built-In Functions" section. Each grouping expression may optionally be followed by the `as` keyword and an alias. If a grouping expression is not a simple field reference then an alias should be provided if the grouping level is to be referenced via `firstof(...)` or `lastof(...)`.

The expressions in the `group by` clause also determine the ordering of the query result. In effect, `group by a, b, c` implies ordering by `a`, then `b`, then `c`.

For this reason, a query expression must not specify both a `group by` clause and an `order by` clause.

The `firstof(...)` and `lastof(...)` functions may be used within the body of a grouped database query to detect the start and end boundaries of groups respectively. The arguments passed to these functions must identify the first grouping level, the first two grouping levels, and so on, in the same order as the enclosing `group by` clause. Each argument must be either the simple field reference from the corresponding grouping expression or the alias assigned to that grouping expression.

If a `group by` clause contains an expression that cannot be converted into valid query syntax for the backend database then compilation fails.

#### Limit Clause

A `limit` clause specifies the maximum number of records that may be returned by a database query. The syntax is intentionally similar to SQL.

~~~
for rec post in posts where author = madeBy limit 10 {
  ...
}
~~~

The expression following the `limit` keyword must evaluate to an `int` value.

If the evaluated limit value is negative then it is treated as zero.

If no `limit` clause is specified then Tablo does not impose a limit on the number of records returned.

### Record Pointers

A record pointer is a variable that provides the user with the ability to get and set the field values for a database table record. Unlike other variables, a record pointer is declared with the `rec` keyword instead of `var`. They are also immutable unless the user opts into mutability by specifying the `mut` keyword.

There are two valid ways to construct a record pointer. The first is with a database query of some kind. The second is as a mutable record pointer for the creation of a new record.

~~~
rec loc = find tblLocations where id = 42;

rec mut comp = new tblCompanies;
comp.name = "Acme Ltd.";
comp.locationName = loc.name;
comp.country = "SA";
create comp;
~~~

The fields that may be accessed via a record pointer are entirely dependent upon the base database table for the query from which the record pointer was constructed.

Note that the data provided by a record pointer represents a snapshot of the corresponding record. Tablo does not (and could not) guarantee that the underlying record data remains unmodified at the point in time at which the record pointer data is referenced.

When a record pointer's fields are modified, by default the changes are not committed to the database until the end of the enclosing scope. To force the changes to be committed at an earlier point in the code, the appropriate `create` or `update` keyword may be used.

If a record is deleted via its record pointer, the deletion is committed to the database at the point at which the `delete` keyword appears. From that point on, the record pointer may not be used to access field data and calling `exists()` on the record pointer will return `false`.

### For Loops

When a `for` loop's loop variable is preceded by the `rec` keyword, the `for` loop represents a database query. A record pointer is assigned to the loop variable for each iteration.

In order to modify the database data via the loop variable, it must be marked as `mut`.

~~~
for rec mut comp in tblCompanies where countryCode = "JP" {
  ...
}
~~~

Database access for loops may be nested:

~~~
for rec cust in tblCustomers where id >= 10 {
  var n: text = cust.name;

  for rec loc in tblCustomerLocations where id = cust.id {
    var address: [text] = [loc.addr1, loc.addr2, loc.addr3, loc.addr4];

    ...
  }
}
~~~

The following query syntax is valid for `for` loops:

* `where` clause
* `order by` clause
* `group by` clause
* `limit` clause

When a `group by` clause is present, each iteration of the `for` loop still corresponds to a single database record. Grouping does not merge multiple database records into a single record pointer. Instead, grouping simply partitions the ordered query result into contiguous groups.

The expressions in the `group by` clause define the grouping keys. Two adjacent iterations belong to the same group if and only if all grouping key values are equal.

The expressions in the `group by` clause also determine the ordering of the grouped query.

The built-in functions `firstof()` and `lastof()` may be used within the body of the `for` loop to detect group boundaries. These functions evaluate the current loop iteration relative to the current grouping.

~~~
for rec cust in tblCustomers
             where active = true
             group by lower(countryCode) as country {
  if firstof(country) {
    // First record in the current country group.
  }

  if lastof(country) {
    // Last record in the current country group.
  }
}
~~~

Note that Tablo is free to implement the database query logic in whichever way it deems appropriate for the given code structure and database backend. This may include re-ordering, batching, or merging database queries for the sake of performance, memory usage, or any other relevant criteria. However, it guarantees that:

1. The field list for the query shall be limited to the smallest possible set. Specifically:
  * If a field is referenced in the `for` loop's `where` clause but its value is otherwise never referenced, it is not included in the field list.
  * If a field is reference in dead code, it is not included in the field list.
  * If a field is referenced in a code path that may or may not be executed, it is included in the field list.
  * In any circumstance where it is not possible to determine the field list, the field list will include all fields for the table.
2. The runtime results shall be identical, absent any concurrent modification of the database data.

For database backends that support record locking, the `for` loop will include locked records if the loop variable is immutable but exclude locked records if the loop variable is mutable. If required, the lock state of a record may be determined using the `locked()` function.

### Find Statements

A `find` statement searches the database for a single record. By default, the first matching record is returned. This can be made explicit by writing `find first`. Alternatively, to return the last matching record, use `find last`. In determining which record is the first or last, the `order by` clause is considered. If no `order by` clause is specified then Tablo can make no guarantees about which record will be returned in either scenario.

~~~
rec custAcme = find first tblCustomers where name = 'Acme';
var l = count tblCustomerLocations where id = custAcme.id;
~~~

The following query syntax is valid for `find` statements:

* `where` clause
* `order by` clause

When assigning the result of a `find` expression to a record pointer, it may be that no record can be assigned. This may be for one of two reasons:

1. No matching record exists, or
2. A matching record exists but is locked

When no result can be assigned to the record pointer, it ends up in one of two "nullish" states. If no matching record could be found, this may be checked with `exists()` (returns `true` if the record exists but is locked). If a matching record exists but is locked, this may be checked with `locked()` (returns `false` if no matching record was found). For database backends that do not support record locking, the `locked()` function always returns false. Calling the `locked()` function on a read-only record pointer produces a compiler warning. If a record both exists and is not locked then it is not nullish and the record pointer may be used as the condition for an `if` statement, where it evaluates to `true`.

~~~
{
  rec mut custAcme = find first tblCustomers where name = 'Acme';

  if custAcme {
    custAcme.name += ' Ltd.';
  }
}
~~~

As with `for` loops, the field list for the query is automatically determined based on the fields that are referenced in code.

### Count Statements

The `count` statement executes a database query and returns the number of matching records. The result of a `count` statement is of type `int`.

~~~
var numCompanies: int = count tblCompanies where active = true;
~~~

The following query syntax is valid for `count` statements:

* `where` clause

### Create Statements

For the creation of new database records, a mutable record pointer must first be declared and assigned using a `new` expression. It is not valid to omit the assignment.

With the new record pointer, field values may be assigned as required. Finally, the `create` keyword is used to commit the new record to the database. If the database table contains required fields that have not been assigned a value then a compiler warning is produced. Note that the record pointer is automatically committed when code execution leaves the enclosing scope. This includes the case where the end of the scope is reached and when a `return` or `break` statement is encountered. The only exception is when a runtime error is thrown, in which case no new record is committed to the database.

~~~
rec mut newComp = new tblCompanies;
newComp.name = "Acme Ltd.";
create newComp;
~~~

Following a `create` statement, the record pointer remains valid and mutable and may be modified further with subsequent `update` statements.

### Update Statements

The `update` keyword is used to manually commit database changes via a mutable record pointer. Note that any changes made to a mutable record pointer's fields are automatically committed when code execution leaves the enclosing scope. This includes the case where the end of the scope is reached and when a `return` or `break` statement is encountered. The only exception is when a runtime error is thrown, in which case no database changes are committed.

~~~
rec mut loc = find first tblLocations where id = 101;
loc.country = "AU";
update loc;
~~~

If an `update` statement is used on a record pointer that doesn't exist or is locked then a runtime error is thrown. The compiler may generate warnings for attempts to update unchecked record pointers.

Following an `update` statement, the record pointer remains valid and mutable and may be modified further with subsequent `update` statements.

### Delete Statements

The `delete` keyword is used to delete a database record via a mutable record pointer. Any modifications made to the record pointers field data is discarded.

~~~
rec mut invalidComp = find first tblCompanies where id = -1;

if invalidComp {
  delete invalidComp;
}
~~~

If a `delete` statement is used on a record pointer that doesn't exist or is locked then a runtime error is thrown. The compiler may generate warnings for attempts to delete unchecked record pointers.

Following a `delete` statement, the record pointer is no longer valid. Calling the `exists()` function on the record pointer returns `false`.

Functions
---------

Functions are declared using the `fn` keyword followed by the function name, a parameter list, the return type, and a block containing the function body.

By default, a function may only be referenced within the module in which it is declared. In order for a function to be imported by another module, it must be marked with the `pub` keyword.

~~~
pub fn FindPosts(id: int, madeBy: int, since: date, until: date) [ForumPost]! {
  ...
}
~~~

The parameter list is enclosed between `(` and `)` characters, which are required even if the parameter list is empty. Each parameter is declared using the syntax `<name>: <type>`. Parameters are separated by commas.

The return type follows immediately after the parameter list. Any valid Tablo type may be used as the return type, including arrays, objects, and nullable or non-nullable types.

The function body is enclosed between `{` and `}` characters and contains zero or more statements. Function parameters are referenced by name within the body of the function.

The `return` statement exits the body of the function, optionally returning a value:

~~~
return forumPosts;
~~~

For a function that returns `void`, the final `return` statement may be omitted.

To call a function, use the function name followed by an argument list between `(` and `)` characters:

~~~
var ts: timestamp! = timestamp(post.date, post.time);
~~~

Arguments may be passed positionally or by name.

Functions may be defined both at file scope and nested within other scopes. Only functions defined at file scope may be marked as `pub`.

### Function Parameters

Tablo functions support:

* Function overloading
* Calling with named arguments
* Default parameter values
* Variable number of arguments ("varargs")

Any function may be called with a mix of positional arguments, named arguments, and "varargs" arguments provided that:

* All positional arguments are specified before all named arguments.
* Any unnamed "varargs" arguments are specified after all positional and named arguments.
* Each named argument must match the corresponding parameter name in the function declaration.
* No named argument is provided more than once.
* Any omitted parameter must either be nullable or have a default value.

~~~
fn Name(<Positional Args>, <Named Args>, <Varargs>): void {}
~~~

To specify that a function accepts a variable number of arguments ("varargs"), the `...` syntax is used to mark the "varargs" argument. If present, this parameter must be the final parameter in the parameter list, must have an array type, and may not define a default value. The "varargs" argument may be provided as a named argument but only if the value is passed as a single array of the appropriate type.

~~~
fn Example(arg1: text, arg2: int! = 1, ...args: [int]) void {
}

// `arg1` set to 'Foo', `arg2` defaults to 1, `args` is [].
Example('Foo');

// `arg1` set to 'Bar', `arg2` set to 5, `args` is [].
Example(arg1: 'Bar', arg2: 5);

// `arg1` set to 'Baz', `arg2` set to 3, `args` set to [7, 9].
Example('Baz', arg2: 3, 7, 9);

// `arg1` set to 'Qux', `arg2` set to 3, `args` set to [2, 1].
Example('Qux', 3, 2, 1);

// `arg1` set to 'Quux', `arg2` defaults to 1, `args` set to [3, 2, 1].
Example(arg1: 'Quux', args: [3, 2, 1]);

// `arg1` defaults to null, `arg2` defaults to 1, `args` set to [3, 2, 1].
// Must specify `args` as a named parameter or the first "varargs" argument would be interpreted as a positional argument.
Example(args: [3, 2, 1]);
~~~

### Function Overloading

Note that named arguments are not merely a convenience for readability. They also form part of the function overload resolution process and may be used to disambiguate otherwise ambiguous function calls.

The function overload resolution process is as follows:

1. Collect all functions in scope that have the same name as the called function.
2. For each candidate function, attempt to bind all supplied arguments to parameters:
  * Positional arguments are bound from left to right.
  * Named arguments are bound by parameter name.
  * Unnamed "varargs" arguments are bound to the final "varargs" parameter, if one exists.
3. A candidate function is discarded if:
  * Any supplied named argument does not match one of its parameters
  * Any parameter receives more than one value
  * Any supplied argument has an incompatible type
  * Any required parameter is left unbound after argument binding is complete
4. If multiple candidate functions remain after binding and type-checking then the call is ambiguous and a compile error is produced.
5. In the event of such an ambiguity, the caller may disambiguate the call by converting one or more positional arguments to named arguments.

Modules
-------

Each Tablo source file represents a single "module". Code from another module may be imported using the `use` keyword followed by a relative file path expressed as a string literal:

~~~
use '../Common/DateUtils';
use './Warehouse/ParcelQueries';
~~~

The path is resolved relative to the file containing the `use` statement.

`use` statements may appear at file scope or within any nested scope.

Unless otherwise specified, a `use` statement imports all `pub` function declarations from the target module into the scope in which the `use` statement appears. Imported functions may then be called within that scope as well as any nested scopes. For the time being, variables may not be imported by ordinary `use` statements.

In order to specify a subset of the target module's `pub` functions to import, insert a comma-separated list of function names and the `from` keyword after the `use` keyword.

~~~
use NewV4, NewV7 from '../Common/UuidUtils';
~~~

Normal function overloading and shadowing rules apply for imported functions, just as if the function had been defined locally. Imported functions do not become visible outside the scope in which the corresponding `use` statement appears. Functions that are not marked `pub` may not be imported from another module.

Note that `use` statements are *not* simple preprocessor directives for including file content as you may be familiar with from C or C++. Importing a module does not cause that module's own imports to be implicitly imported as well.

During compilation, Tablo first resolves the module graph and gathers the function declarations visible from each module. Function bodies are compiled only after this resolution step has completed. This allows modules to reference functions declared in each other, including in the presence of circular module dependencies, provided that each function call can be resolved unambiguously. Each function is compiled at most once, even if its containing module is imported by multiple other modules.

Tablo also supports global variables. A variable must be explicitly declared using the `global` keyword in order to be visible outside the module in which it is defined.

~~~
global var ConnectionString: text = '...';
~~~

Global variables are not imported by ordinary `use` statements. In order to access a global variable defined in another module, the importing module must explicitly import it using `use global ... from ...`:

~~~
use global ConnectionString from '../Common/Config';
~~~

As with imported functions, normal shadowing rules apply for imported global variables. A global variable imported into a scope does not become visible outside that scope.

All global variables are initialized before any user-written functions are executed. This guarantee applies regardless of which files import which globals, and it also applies to globals that are never imported or never referenced. This rule exists both for safety and so that any side-effects resulting from global initialization occur deterministically before normal program execution begins. It is forbidden for a global to attempt to use the value of another global to determine its initial value.

Built-In Functions
------------------

### `date(): date!`

Returns the current date in the local time-zone.

### `date(d: datetz!): date!`

Returns the date `d`.

### `date(ts: timestamp!): date!`

Returns the date portion of the timestamp `ts`.

### `date(ts: timestamptz!): date!`

Returns the date portion of the timestamp `ts`.

### `date(d: text!): date`

Returns the date obtained by parsing `d`. If `d` cannot be parsed as a date then the function fails.

### `date(tz: dec!): date!`

Returns the current date in the time-zone specified by `tz`.

### `firstof(v1!: any, v2: any, ...): bool!`

Returns `true` when the current iteration of a grouped `for` loop is the first iteration of the group identified by the supplied grouping expression values. Otherwise returns `false`.

It is valid to call `firstof()` only within the body of a grouped database `for` loop. The arguments passed to `firstof()` must identify the first grouping level, the first two grouping levels, and so on, in the same order as the enclosing `group by` clause. Each argument must be either the simple field reference from the corresponding grouping expression or the alias assigned to that grouping expression.

### `float(v: dec!): float`

Returns the `float` obtained by casting `v`. If `v` cannot be converted then the function fails.

### `float(v: int!): float`

Returns the `float` obtained by casting `v`. If `v` cannot be converted then the function fails.

### `float(v: text!): float`

Returns the `float` obtained by parsing `v`. If `v` cannot be parsed as a `float` then the function fails.

### `ilike(v: text!, pattern: text!): bool!`

Returns `true` if `v` matches `pattern` according to Tablo's case-insensitive SQL-style wildcard matching rules. Otherwise returns `false`.

**TODO**: Detail the exact wildcard syntax and escaping rules.

### `int(v: dec!): int`

Returns the `int` obtained by casting `v`. If `v` cannot be converted then the function fails.

### `int(v: float!): int`

Returns the `int` obtained by casting `v`. If `v` cannot be converted then the function fails.

### `int(v: text!): int`

Returns the `int` obtained by parsing `v`. If `v` cannot be parsed as an `int` then the function fails.

### `max(a: int!, b: int!): int!`

TODO

### `min(a: int!, b: int!): int!`

TODO

### `lastof(v1!: any, v2: any, ...): bool!`

Returns `true` when the current iteration of a grouped `for` loop is the last iteration of the group identified by the supplied grouping expression values. Otherwise returns `false`.

It is valid to call `lastof(...)` only within the body of a grouped database `for` loop. The arguments passed to `lastof(...)` must identify the first grouping level, the first two grouping levels, and so on, in the same order as the enclosing `group by` clause. Each argument must be either the simple field reference from the corresponding grouping expression or the alias assigned to that grouping expression.

### `like(v: text!, pattern: text!): bool!`

Returns `true` if `v` matches `pattern` according to Tablo's SQL-style wildcard matching rules. Otherwise returns `false`.

**TODO**: Detail the exact wildcard syntax and escaping rules.

### `text(v: bool): text`

Returns the string representation of `v`.

### `text(v: dec): text`

Returns the string representation of `v`.

### `text(v:float): text`

Returns the string representation of `v`.

### `text(v: int): text`

Returns the string representation of `v`.

### `text(v: time): text`

Returns the string representation of `v`.

### `text(v: timestamp): text`

Returns the string representation of `v`.

### `text(v: timestamptz): text`

Returns the string representation of `v`.

### `text(v: timetz): text`

Returns the string representation of `v`.

### `time(): time!`

Returns the current time in the local time-zone.

### `time(ts: timestamp!): time!`

Returns the time portion of the timestamp `ts`.

### `time(ts: timestamptz!): time!`

Returns the time portion of the timestamp `ts`.

### `time(t: timetz!): time!`

Returns the time `t`.

### `time(v: text!): time!`

Returns the time obtained by parsing `v`. If `v` cannot be parsed as a time then the function fails.

### `time(tz: dec!): time!`

Returns the current time in the time-zone specified by `tz`.

### `timestamp(): timestamp!`

Returns the current date and time in the local time-zone.

### `timestamp(ts: timestamptz!): timestamp!`

Returns the timestamp `ts`.

### `timestamp(d: date!, t: time!): timestamp!`

Returns the timestamp formed from the date `d` and the time `t`.

### `timestamp(v: text!): timestamp`

Returns the timestamp obtained by parsing `v`. If `v` cannot be parsed as a timestamp then the function fails.

### `timestamptz(): timestamptz!`

Returns the current date and time in the local time-zone.

### `timestamptz(d: date!, t: timetz!): timestamptz!`

Returns the timestamp formed from the date `d` and the time `t`.

### `timestamptz(d: date!, t: time!, tz: dec!): timestamptz!`

Returns the timestamp formed from the date `d` and the time `t` in time-zone `tz`.

### `timetz(): timetz!`

Returns the current time in the local time-zone.

### `timetz(ts: timestamptz!): timetz!`

Returns the time portion of the timestamp `ts`.

### `timetz(v!: text): timetz`

Returns the timestamp obtained by parsing `v`. If `v` cannot be parsed as a timestamp then the function fails.

### `timetz(t: time!, tz: dec!): timetz!`

Returns the time `t` in time-zone `tz`.

Grammar
-------

The following syntax is used to define Tablo's syntax:

~~~
Production = <Components>
|   One of the surrounding components
()  Groups the enclosed components
[]  At most one instance of the enclosed components
{}  Any number of repetitions of the enclosed components
…   Range of components
~~~

~~~
statement = [ block | expression | forStatement | ifStatement | returnStatement | whileStatement ] `;`

block = `{` { statement } `}`

variableDeclaration = `var` identifier `:` dataType [ `=` expression ]
dataType = unquotedIdentifier | arrayType
arrayType = dataType `[` integerLiteral `]`

expression = ( expression assignmentOperator expression ) | logicalOr
groupExpression = `(` expression `)`
logicalOr = logicalAnd { `or` logicalAnd }
logialAnd = equality { `and` equality }
equality = comparison { equalityOperator comparison }
comparison = factor { relationalOperator factor }
factor = term { multiplicativeOperator term }
term = unary { additiveOperator unary }
unary = ( unaryOperator unary ) | literal
literal = `null` | booleanLiteral | decimalLiteral | hexLiteral | integerLiteral | octalLiteral | stringLiteral | groupExpression | identifier
assignmentOperator = `=` | `+=` | `-=` | `*=` | `/=` | `%=`
equalityOperator = `==` | `!=`
relationalOperator = `>` | `>=` | `<` | `<=`
multiplicativeOperator = `*` | `/`
additiveOperator = `+` | `-`
unaryOperator = `!` | `-`

identifier = quotedIdentifier | unquotedIdentifier
quotedIdentifier = `"` { `""` | unicode } `"`
unquotedIdentifier = ( alpha | `_` ) { alpha | digit | `_` }

stringLiteral = `'` { escapeCharacter | characterLiteral | stringInterpolation | unicode } `'`
characterLiteral = `\u` hexDigit { hexDigit }
escapeCharacter = `\` ( `b` | `f` | `n` | `r` | `t` | `v` | `\` | `'` )
stringInterpolation = `${` expression `}`

decimalLiteral = [ integerDigits ] `.` integerDigits

integerLiteral = binaryLiteral | octalLiteral | hexLiteral | integerDigits
binaryLiteral = `0` ( `B` | `b` ) binaryDigits
octalLiteral = `0` ( `O` | `o` ) octalDigits
hexLiteral = `0` ( `X` | `x` ) hexDigits

binaryDigits = binaryDigit { [ `_` ] binaryDigit }
hexDigits = hexDigit { [ `_` ] hexDigit }
integerDigits = digit { [ `_` ] digit }
octalDigits = octalDigit { [ `_` ] octalDigit }

booleanLiteral = `false` | `true`

alpha = `A` … `Z` | `a` … `z`
binaryDigit = `0` | `1`
digit = `0` … `9`
hexDigit = digits | `A` … `F` | `a` … `F`
octalDigit = `0` … `7`
unicode = <Any Unicode character except code point zero>
~~~
