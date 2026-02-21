Tablo Language Reference
========================

Introduction
------------

The Tablo language is a procedural language with first-class support for database access operations. This allows you to write your query code right within your processing code and have both type-checked at compile time.

The syntax aims to be familiar to users of C-like languages such as C, C++, C#, Java, Javascript/Typescript, Go, etc.

This file details the syntax of the Table language.

### A Simple Example

~~~
with exampledb;

obj ForumPost {
  id: posts.id,
  author: users.surname + ', ' + users."given-names",
  "timestamp": timestamp(posts.date, posts.time),
  msg: posts.text,
  comments: {
    "timestamp": timestamp(comments.date, comments.time),
    comment: comments.text,
  }[],
}

fn FindPosts(id?: i32, madeBy?: i32, since?: date, until?: date): ForumPost[] {
  // If the user gets the dates in the wrong order, swap them.
  var tempDate: date = null;
  if until < since {
    tempDate = since;
    since = until;
    until = tempDate;
  }

  return find each ForumPost from posts
    left join users on users.id = posts.author
    left join comments on comments.post = posts.id
    where posts.id = id
      and posts.author = madeBy
      and posts.date >= since
      and posts.date <= until
    order by posts.date desc, posts.time desc;
}
~~~

The example above shows the definition of a data structure and a function that returns an array of data that adheres to the structure. Tablo is designed such that the functional parts look as much as possible like a typical C-like language while the query syntax looks as much as possible like regular SQL.

If we were to use the above code to generate some C# code, we might get something like this:

~~~C#
public sealed record ForumPost
{
    public int Id { get; init; }
    public string Author { get; init; }
    public DateTime Timestamp { get; init; }
    public Comment[] Comments { get; init; } = Array.Empty<Comment>();
    
    public sealed record Comment
    {
        public DateTime Timestamp { get; init; }
        public string Comment { get; init; }
    }
}

public async Task<ForumPost[]> FindPosts(DbContext ctx, object args)
{
    var _args = args
        .GetType()
        .GetProperties()
        .ToDictionary(p => p.Name, p => p.GetValue(args));

    // If the user gets the dates in the wrong order, swap them.
    DateTime? tempDate = null;
    if (_args.ContainsKey("until")
        && _args.ContainsKey("since")
        && (DateTime)_args["until"] < (DateTime)_args["since"])
    {
        tempDate = (DateTime)_args["since"];
        _args["since"] = (DateTime)_args["until"];
        _args["until"] = tempDate;
    }

    var query = from p in ctx.Set<Posts>()
                join u in ctx.Set<Users>() on u.Id equals p.Author into users
                from u in users.DefaultIfEmpty()
                join c in ctx.Set<Comments>() on c.Post equals p.Id into comments
                from c in comments.DefaultIfEmpty()
                select new { Post = p, User = u, Comment = c };

    if (_args.ContainsKey("id"))
        query = query.Where(q => q.Post.Id == (int)_args["id"]);

    if (_args.ContainsKey("madeBy"))
        query = query.Where(q => q.Post.Author == (int)_args["madeBy"]);

    if (_args.ContainsKey("since"))
        query = query.Where(q => q.Post.Date >= (DateTime)_args["since"]);

    if (_args.ContainsKey("until"))
        query = query.Where(q => p.Post.Date <= (DateTime)_args["until"]);

    var records = await query.ToListAsync();

    return records
        .GroupBy(r => r.Post.Id)
        .Select(g => 
            {
                var first = g.First();

                return new ForumPost
                {
                    Id = first.Post.Id,
                    Author = $"{first.User.Surname} {first.User.GivenNames}",
                    Timestamp = first.Post.Date.Add(first.Post.Time),
                    Comments = g
                        .Select(c => new ForumPost.Comment
                            {
                                Timestamp = c.Date.Add(c.Time),
                                Comment = c.Text,
                            }
                        )
                        .ToArray(),
                };
            }
        )
        .ToArray();
}
~~~

Core Concepts
-------------

Many of the design decisions that drive the development of Tablo stem from the fact that code may need to be generated in a completely different language based on the Tablo code. Therefore, it makes as few assumptions as possible about the capabilities of the target language.

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
| `blob`        | Binary data              |
| `bool`        | Boolean                  |
| `date`        | Date                     |
| `dec`         | Decimal                  |
| `float`       | floating point           |
| `int`         | integer                  |
| `json`        | JSON                     |
| `query`       | Query                    |
| `text`        | Text                     |
| `time`        | Time                     |
| `timestamp`   | Timestamp                |
| `timestamptz` | Timestamp with time-zone |
| `timetz`      | Time with time-zone      |
| `uuid`        | Universally unique ID    |
| `xml`         | XML                      |

It is up to the Tablo interpreter to determine how these types map to the available data types in the connected database and target language. Some target languages may support options by which the type mapping can be modified. In rare cases, some data types may be unsupported for a certain database type of target language.

Identifiers
-----------

Identifiers are names that represent data entities such as variables, tables, fields, and records. There are two types: quoted and unquoted.

Unquoted identifiers may only contain the characters "A" to "Z", "a" to "z", "0" to "9", and "_". The first letter must not be a digit. Unquoted identifiers are not case-sensitive.

Quoted identifiers are delimited by `"` characters and may contain one or more Unicode characters (including new lines). Two `"` characters immediately next to one another are not considered delimiters and are treated as a literal `"` character instead. Quoted identifiers are case-sensitive.

Note that some tokens that match the definition of an identifier are considered to be reserved words and are not treated as identifiers by the compiler.

Reserved Words
--------------

Some identifiers are reserved as keywords within the syntax of the language. Keywords are always unquoted, formed of alphanumeric characters only, and are case-insensitive. If a quoted identifier matches a keyword in all respects except for fact that it is quoted rather than unquoted then the identifier is valid and will not be treated as a keyword.

The following identifiers are reserved as keywords:

| Keyword       | Description                                           |
|---------------|-------------------------------------------------------|
| `and`         | Logical AND                                           |
| `blob`        | Binary data data type                                 |
| `bool`        | Boolean data type                                     |
| `by`          | Follows keywords `group` or `order`                   |
| `date`        | Date data type                                        |
| `dec`         | Decimal data type                                     |
| `each`        | Starts a query that returns multiple results          |
| `else`        | Starts an else-block for an if statement              |
| `fail`        | Exits a function by throwing an error                 |
| `false`       | A literal false value                                 |
| `first`       | Starts a query that returns the first matching result |
| `float`       | floating-point data type                              |
| `fn`          | Starts a function definition                          |
| `for`         | Starts a for-loop statement                           |
| `group`       | Starts a GROUP BY clause                              |
| `if`          | Starts an if statement                                |
| `in`          | Accesses elements in an iterator                      |
| `int`         | integer data type                                     |
| `json`        | JSON data type                                        |
| `last`        | Starts a query that returns the last matching result  |
| `limit`       | Specifies the LIMIT value for a query                 |
| `not`         | Logical NOT                                           |
| `null`        | A literal null value                                  |
| `obj`         | Starts an object definition                           |
| `or`          | Logical OR                                            |
| `order`       | Starts an ORDER BY clause                             |
| `query`       | Query data type                                       |
| `return`      | Exits a function by returning a result                |
| `text`        | String data type                                      |
| `time`        | Time data type                                        |
| `timestamp`   | Timestamp data type                                   |
| `timestamptz` | Timestamp (with time-zone) data type                  |
| `timetz`      | Time (with time-zone) data type                       |
| `true`        | A literal true value                                  |
| `uuid`        | UUID data type                                        |
| `var`         | Starts a variable declaration                         |
| `where`       | Starts a WHERE clause                                 |
| `while`       | Starts a while-loop statement                         |
| `with`        | Specifies the active databases for table resolution   |
| `xml`         | XML data type                                         |
| `xor`         | Logical XOR                                           |

Keyword Literals
----------------

Variables of type `bool` may be assigned the literal values `true` and `false`.

Unless marked as non-null, a variable of any data type may be assigned the null value. This is done using the reserved word `null`.

Unless marked as required, an argument of any data type my hold a void value. The void value cannot be manually assigned. Only arguments that have not been provided when a function is called receive the void value.

Integer Literals
----------------

An integer literal is represented as a sequence of digits, which may be broken only by single `_` characters.

Unless otherwise specified, digits are interpreted as base-10 digits. Both base-16 (hexadecimal), base-8 (octal), and base-2 (binary) digits are also supported. To specify a hexadecimal integer, prefix it with `0x`. To specify an octal integer, prefix it with `0o`. To specify a binary integer, prefix it with `0b`.

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

Strings are delimited with `'` characters and may contain any characters except the character with code zero. Some characters need to be escaped. String supports interpolation using the `${` and `}` delimiters. Any valid expression may be placed within the string interpolation delimiters.

Note that there is no special syntax for multi-line strings. And new line characters in the strings remain in the assigned value. For any string that spans mutliple lines, leading whitespace is trimmed up to the number of whitespace characters preceding either the initial or final `'` (whichever is the smaller).

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

Date & Time Literals
--------------------

Date literals are prefixed with `@` and expressed in `<year>-<month>-<day>` format.

Time literals are also prefixed with `@` and expressed in `<hour>:<minute>:<second>.<fractional part>` format. The fractional part may be omitted, in which case the fractional part is set to zero. Both the fractional part and the seconds may be omitted, in which case both the fractional part and seconds are set to zero.

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

The `!=` operator is used to compare two value for inequality. It evaluates to `false` if the values are equal.

### Comparison Operators

The `>`, `>=`, `<`, and `<=` operators are used to compare the numeric ordering of two values. Operands must be numeric or date/time.

The `>` operator evaluates to `true` if the left value is strictly greater than the right. Operands must be numeric or date/time.

The `>=` operator evaluates to `false` if the left value is strictly less than the right. Operands must be numeric or date/time.

The `<` operator evaluates to `true` if the left value is strictly less than the right. Operands must be numeric or date/time.

The `<=` operator evaluates to `false` if the left value is strictly greater than the right. Operands must be numeric or date/time.

### Logical Operators

The `and` operator evaluates to the result of a logical AND of the two operands. Operands must be of type `bool`.

The `or` operator evaluates to the result of a logical OR of the two operands. Operands must be of type `bool`.

The unary prefix `not` operator evaluates to the result of a logical NOT of its operand. Operand must be of type `bool`.

The `xor` operator evaluates to the result of a logical XOR of the two operands. Operands must be of type `bool`.

### Mathematical Operators

The `+` operator evaluates to the sum of its operands. In the case of strings, it is used for concatenation. Operands must be numeric, date/time, or `str`. The operands may be dissimilar if both types are numeric.

TODO: Create a section to describe automatic type conversions that result from operations on dissimilar operands.

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

Variables
---------

The `var` keyword is used to declare a new variable. At present, the data type must be specified. Optionally, an initial value may be assigned. If no initial value is assigned, the variable contains a null value.

Arrays
------

Tablo supports simple 1D arrays. Due to poor support for arrays in databases (the obvious exception being PostgreSQL), arrays may only be used when compiling against a PostgreSQL database.

~~~
var array1d: int[] = [1, 2, 4, 8];
~~~

At present, there is not support for multi-dimensional arrays.

Scopes
------

TODO

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
    var rec: ExampleData = fetchedExampleData[i];

    for val in rec[i].vals {
        ...
    }
}
~~~

Queries
-------

Tablo's `query` type allows database queries to be assigned to variables for re-use.

~~~
obj CompanyData {
  id: coymast.coyno,
  name: coymast.name,
}

// Define a query that returns multiple results.
var getExampleCompanies: query = each CompanyData from coymast where coymast.id > 10;

// Execute the query and iterate the results...
for c in getExampleCompanies {
  ...
}

// Define a query that returns a single result.
var getExampleCompany: query = first CompanyData from coymast where coymast.name == 'Example';

// Execute the query and assign the result.
var exampleCompany: CompanyData = getExampleCompanies();
~~~

Functions
---------

TODO

Built-In Functions
------------------

### `date(): date`

Returns the current date in the local time-zone.

### `date(d!: datetz): date`

Returns the date `d`.

### `date(ts!: timestamp): date`

Returns the date portion of the timestamp `ts`.

### `date(ts!: timestamptz): date`

Returns the date portion of the timestamp `ts`.

### `date(d!: text): date`

Returns the date obtained by parsing `d`. If `d` cannot be parsed as a date then the function fails.

### `date(tz!: dec): date`

Returns the current date in the time-zone specified by `tz`.

### `float(v!: dec): float`

Returns the `float` obtained by casting `v`. If `v` cannot be converted then the function fails.

### `float(v!: int): float`

Returns the `float` obtained by casting `v`. If `v` cannot be converted then the function fails.

### `float(v!: text): float`

Returns the `float` obtained by parsing `v`. If `v` cannot be parsed as a `float` then the function fails.

### `int(v!: dec): int`

Returns the `int` obtained by casting `v`. If `v` cannot be converted then the function fails.

### `int(v!: float): int`

Returns the `int` obtained by casting `v`. If `v` cannot be converted then the function fails.

### `int(v!: text): int`

Returns the `int` obtained by parsing `v`. If `v` cannot be parsed as an `int` then the function fails.

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

### `time(): time`

Returns the current time in the local time-zone.

### `time(ts!: timestamp): time`

Returns the time portion of the timestamp `ts`.

### `time(ts!: timestamptz): time`

Returns the time portion of the timestamp `ts`.

### `time(t!: timetz): time`

Returns the time `t`.

### `time(v!: text): time`

Returns the time obtained by parsing `v`. If `v` cannot be parsed as a time then the function fails.

### `time(tz!: dec): time`

Returns the current time in the time-zone specified by `tz`.

### `timestamp(): timestamp`

Returns the current date and time in the local time-zone.

### `timestamp(ts!: timestamptz): timestamp`

Returns the timestamp `ts`.

### `timestamp(d!: date, t!: time): timestamp`

Returns the timestamp formed from the date `d` and the time `t`.

### `timestamp(v!: text): timestamp`

Returns the timestamp obtained by parsing `v`. If `v` cannot be parsed as a timestamp then the function fails.

### `timestamptz(): timestamptz`

Returns the current date and time in the local time-zone.

### `timestamptz(d!: date, t!: timetz): timestamptz`

Returns the timestamp formed from the date `d` and the time `t`.

### `timestamptz(d!: date, t!: time, tz!: dec): timestamptz`

Returns the timestamp formed from the date `d` and the time `t` in time-zone `tz`.

### `timetz(): timetz`

Returns the current time in the local time-zone.

### `timetz(ts!: timestamptz): timetz`

Returns the time portion of the timestamp `ts`.

### `timetz(v!: text): timetz`

Returns the timestamp obtained by parsing `v`. If `v` cannot be parsed as a timestamp then the function fails.

### `timetz(t!: time, tz!: dec): timetz`

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
