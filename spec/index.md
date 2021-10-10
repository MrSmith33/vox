# Index

<!-- MarkdownTOC autolink="true" markdown_preview="github" -->

- [Types](#types)
    - [Basic types](#basic-types)
    - [Enums](#enums)
    - [Pointers](#pointers)
    - [Slices](#slices)
    - [Static arrays](#static-arrays)
    - [Structs](#structs)
    - [Function type](#function-type)
- [Functions](#functions)
    - [External functions](#external-functions)
    - [Calls](#calls)
- [Attributes](#attributes)
    - [Built-in attributes](#built-in-attributes)
- [Metaprogramming](#metaprogramming)
    - [Compile-time assert `#assert`](#compile-time-assert-assert)
    - [Conditional compilation](#conditional-compilation)
        - [\#if](#if)
        - [\#version](#version)
            - [Predefined versions](#predefined-versions)
        - [\#foreach](#foreach)
    - [Templates](#templates)
        - [Function templates](#function-templates)
        - [Struct templates](#struct-templates)
    - [Meta types \(NEI\)](#meta-types-nei)
        - [$alias](#alias)
        - [$type](#type)
        - [Builtin functions for working with meta types](#builtin-functions-for-working-with-meta-types)
        - [Using meta types in CTFE](#using-meta-types-in-ctfe)
    - [CTFE-only functions](#ctfe-only-functions)
    - [Compile Time Function Execution \(CTFE\)](#compile-time-function-execution-ctfe)

<!-- /MarkdownTOC -->

`NEI` marks not yet implemented features.

# Types

## Basic types

- `noreturn`
- `void`
- `typeof(null)`
- `bool`

- `i8`
- `i16`
- `i32`
- `i64`

- `u8`
- `u16`
- `u32`
- `u64`

- `f32`
- `f64`

- `$alias`
- `$type`
- `$value` `NEI`

`noreturn` implicitly casts to any other type. After calling the function returning `noreturn`, control flow will never return to the caller.

## Enums

Enums define a new type. Enums can have enum members. Enums have base type, which defaults to `i32`.

```D
enum E; // defines new enum type
enum e9 { e9 = 9 } // new enum type with member (implicit base type)
enum e7 : i32 { m7 = 7 } // new enum type `e7`, with member `m7` (explicit base type)

// enum members within anonymous enums are visible in the outer scope
enum { m5 = 5 } // anonymous enum with member
enum : i32 { m6 = 6 } // anonymous enum with explicit type and with member

// instead of defining enum members inside anonymous enums, you can define them separately
enum m3 = 3; // enum member
enum i32 m4 = 4; // enum member with explicit type
```

Enum members have the type of enum type (if inside named enum). In that case they will implicitly convert to the enum base type. But the conversion from base type to enum type requires explicit casting.

## Pointers

`<type>*`

Pointers behave like in C and D.

```D
i32 integer;
i32* pointer = &integer;
*pointer = 42;
pointer[0] = 42;
i32[] slice = pointer[0..1]; // slicing a pointer gives a slice result
```

## Slices

Slices are equivalent of `struct { size_t length; T* ptr; }`

`<type>[]`

Example:
```D
u8[] slice;
```

Members/operations:
```D
u8[] slice;
slice.length; // u64
slice.ptr; // u8*
slice[1..10]; // slicing. End is exclusive
slice[]; // slicing. Same as slice[0..slice.length]
```

## Static arrays

Static array has fixed size and is passed by value as a whole.

`<type>[<integer value>]`

Example:
```D
u8[256] buffer;
```

Members/operations:
```D
u8[256] array;
array.length; // Is a compile-time known constant
array.ptr; // u8*
array[1..10]; // slicing. End is exclusive
array[]; // slicing. Same as array[0..array.length]
```

## Structs

```D
struct <identifier> {
    <body>
}
```

Struct body can contain any declarations that modules can.

## Function type

Represents function pointer.

`<ret_type> function(<argument_list)`

Example:
```D
i32 i32Fun() { return 42; }
alias i32_funType = i32 function(); // alias of function type
enum i32_funType funPtrEnum = &i32Fun; // storing function pointer inside enum
enum i32 function() inlinefunPtrEnum = &i32Fun; // same, with explicitly specified type
i32_funType retFuncPtr() {
    return &i32Fun; // function returns function pointer
}

i32 sum(i32 a, i32 b) { return a + b; }
i32 callFunc(i32 function(i32, i32) func, i32 a, i32 b) {
    // call pointer to function with parameters
    return func(a, b);
}
i32 user() {
    return callFunc(&sum, 10, 40);
}
```

# Functions

Function declaration:

```D
<return_type> <identifier> (<arguments>) {
    <body>
}
```

## External functions

External functions are linked by name to symbol with matching name from `.dll` or from host program.

Syntax:

```D
<return_type> <identifier> (<arguments>);
```

Example:
```D
u8 WriteConsoleA(
    void* hConsoleOutput,
    u8* lpBuffer,
    u32 nNumberOfCharsToWrite,
    u32* lpNumberOfCharsWritten,
    void* lpReserved
);
```

When program is compiled like `vox main.vx C:\Windows\System32\kernel32.dll`, `WriteConsoleA` function will bind to the symbol from `kernel32.dll`.

Right now there is no precise control over the search scope per external function. 

## Calls

When calling function with 0 arguments parentheses are optional, except in the following situations:
- when taking alias of a function.
- when calling function returning $alias or function pointer

```D
$alias alias_var = func; // alias is taken
$alias alias_var = func(); // alias of return value is taken

$alias func2() { ... }
$alias alias_var = func2; // take alias of func2
$alias alias_var = func2(); // call func2
$alias alias_var = func2()(); // call return value of func2
```

# Attributes

Attributes can be specified in one of 3 forms

1. Apply attributes to the end of the current scope. Doesn't apply to nested scopes.
   @attr1 @attr2 @attr3 @attr4 apply to all declarations defined later in the current scope
```D
@attr1 @attr2:
@attr3 @attr4:
decl1;
decl2;
```

Here, only `decl3` and `decl5` will gain the `@attr` attribute:

```D
decl1;
{
    decl2;
  @attr:
    decl3;
    {
        decl4;
    }
    decl5;
}
decl6;
```

2. `NEI` Attribute block. Doesn't apply to nested scopes.
   @attr5 @attr6 apply to all declarations defined in curly braces (`decl1` and `decl3`). Curly braces do not introduce a new scope:

   ```
   @attr5 @attr6 {
       decl1;
       {
           decl2;
       }
       decl3;
   }
   ```

3. Direct attachment.

   Zero or more attributes can precede the declaration.

   In this case attributes will only affect single declaration.

   Can be applied to variable, enum, enum member, function, function parameter, template parameter, module declaration.

   ```D
   @attr i32 var = 1;
   
   @attr
   void foo(@attr int param) {}
   
   @attr struct S {}
   
   struct S[@attr T] {}
   void foo[@attr T]() {}
   
   @attr
   module mod;
   
   @attr
   enum E : i32 {
       @attr a,
       @attr b,
   }
   
   @attr enum CONST = 42;
   ```

## Built-in attributes

* `@extern(syscall, <int>)`
  Can be attached to a function without a body, to make it perform a syscall on call.

  ```D
  @extern(syscall, 60)
  void exit();
  ```
  Functions declared with this attribute will receive calling convention of the target system. For example when compiled for `linux-x64` target calling convention will match System V syscall convention. On targets that have no syscall defined this will result in error. The integer parameter specifies syscall number.

  If several attributes of this type are attached, only latest attribute applies.

* `NEI` `@extern(module, "external module name")`

  External functions with this attribute will generate entry in import table for the specified module (second parameter). By default the name of external module is used as a name of `.so`/`.dll` library when compiling as an executable. `.so`/`.dll` suffix will be added as (and if) required by the target plaform. External module name should not have `.so`/`.dll` suffix in the source file.

  To override the module file name use `--externModule=<external_mod>:<file_name>` CLI switch

  ```D
  @extern(module, "kernel32")
  noreturn ExitProcess(u32 uExitCode);
  ```

  If several attributes of this type are attached, only latest attribute applies.

  [See more](https://gist.github.com/MrSmith33/b55f6f36c211fc07eb581de2e676e256)




# Metaprogramming

## Compile-time assert `#assert`

```D
#assert(<condition>, <message>);
```

When condition evaluates to false triggers compilation error with provided message.

For now only single string literal can be used as a message.

## Conditional compilation

### \#if
Static if evaluates `<expr>` at compile time, and `<expr>` is true `#if` is replaced with `<then_stmt>`. Otherwise, if `else` clause is present, `#if` is replaced with `<else_stmt>`.
`#if` doesn't introduce a new scope.
`#if` can appear both as a declaration inside declaration scope (module or struct) and inside function body as a statement.

Syntax:
```D
#if(<expr>) <then_stmt>
#if(<expr>) <then_stmt> else <else_stmt>
```

Examples:
```D
enum debug_cond = true;

#if(debug_cond) { // Module scope
    i32 debug_counter = 0; // Conditionally declared global variable
}

struct S {
    #if(debug_cond) // Struct scope
        u8[] debug_buffer; // Conditionally declared struct member
}

void fun()
{
    // Condition inside function body
    #if(debug_cond) print("debug mode");
    else #if(debug_cond2) print("debug mode 2"); // When chaining #if must be used each time.
    else print("no debug mode");
}
```

### \#version
* Behaves in the same way as `#if`, but instead of an expression accepts one of the predefined identifiers. If the version identifier is defined it will evaluate to `true`.

```D
#version(linux) {
    // code for linux
} else #version(windows) {
    // code for windows
} else #version(macos) {
    // code for macos
} else {
    #assert(false, "OS not supported");
}
```

#### Predefined versions
* `windows` - defined if target OS is Windows
* `linux` - defined if target OS is Linux
* `macos` - defined if target OS is MacOS

### \#foreach
* `#foreach` will copy a body for each iteration, declaring element and optional index.
* `#foreach` does not introduce a new scope.
* Only alias arrays are supported for now.

```D
$alias selectPrintFunc($type T) {...}
void write[$alias[]... Args](Args args) {
    // i is enum member containing current index, argType points to the corresponding element of Args
    // index is optional
    #foreach(i, argType; Args) {
        selectPrintFunc(argType)(args[i]);
    }
    // or
    // args is $alias[], `arg` will point to the corresponding parameter of `write`
    #foreach(arg; args) {
        selectPrintFunc(typeof(arg))(arg);
    }
}
```

## Templates

Template parameters must be known by the compiler at instantiation time.

### Function templates
Function templates are defined as regular functions with additional list of compile-time parameters. Template agrument can be:

- `Identifier` just a type (shortcut for `$type Identifier`)

```D
T min[T](T a, T b) {
    if (a < b) return a;
    return b;
}
#assert(min[i32](42, 2) == 2);
```

- `T Identifier` some compile-time known value of type `T` (NEI).

```D
T min[$type T](T a, T b) {
    if (a < b) return a;
    return b;
}
#assert(min[i32](42, 2) == 2);
```

- `Identifier...` Variadic parameters. Must be the last template argument. (args with default value may follow).

`...` is needed on type of `args` because compiler needs to know that signature has expanded parameter type before `#if` and `#foreach` are expanded.

```D
void write[Args...](Args... args) {
    #foreach(i, arg; args) {
        alias func = selectPrintFunc(Args[i]);
        func(arg);
    }
}
```

### Struct templates

```D
struct vec[ElemType, i32 dim]
{
    ElemType[dim] array;
    ...
}
alias ivec2 = vec[i32, 2];
```

## Meta types (NEI)

They are built-in CTFE-only types.

User types that contain meta types are concidered CTFE-only too.

```D
$alias     // built-in meta type
$alias*    // complex meta-types
$alias[]   // complex meta-types
$alias[10] // complex meta-types
$alias func() { ... } // CTFE-only function
struct S { // CTFE-only user-defined type (struct)
    $alias a;
    $alias b;
}
S func2() { ... } // CTFE-only function

```

### $alias

- `$alias` is an alias to any symbol in the program.
- `$alias` values can be compared.
- 

```D
void fun(){}
struct ivec2 { i32 x; i32 y; }
enum val = 42;

$alias some_type = i32; // Ok. Type alias
$alias some_type = 42; // Ok. Value alias
$alias some_type = $alias(fun); // Ok. function alias
$alias some_type = $alias(42); // Ok. enum alias
```

### $type

- `$type` is a an alias that only accepts types.
- `$type` values can be compared.
- `$type` is implicitly convertible to `$alias`.

```D
$type some_type = i32; // Ok
$type some_type = 42; // Error: 42 is not a type
```

### Builtin functions for working with meta types

```D
bool $isType($alias a)
bool $isValue($alias a)
bool $isVariable($alias a)
bool $isCode($alias a)
bool $isFunction($alias a)
bool $isStruct($alias a)
bool $isTemplate($alias a)
bool $isTemplateInstance($alias a)
bool $isInstanceOf($alias a, $alias template)
bool $isEnum($alias a)

bool $isStruct($type type)
bool $isInteger($type type)
bool $isFloating($type type)

bool $isSlice($type type)
#assert($isSlice(i8[]));

bool $isSliceOf($type slice, $type elemType)
#assert($isSliceOf(i8[], i8));

$type baseOf($type type);
#assert($baseOf(i8*) == i8);
#assert($baseOf(i8[]) == i8);
#assert($baseOf(i8[10]) == i8);

bool $isArray($type type)
bool $isArrayOf($type array, $type elemType)

u8[] $getIdentifier($alias a)

// If type is struct returns all declarations inside struct including:
// functions, variables, enums, aliases, struct declarations etc.
// If type is enum returns all enum members
$alias[] $getMembers($type type)
$alias[] $getStructMembersVariables($type type)
$alias[] $getStructMembersMethods($type type)
```

### Using meta types in CTFE

Defining custom predicates on types:

```D
bool isInteger($type type) {
    return type == u8
        || type == i8
        || type == u16
        || type == i16
        || type == u32
        || type == i32
        || type == u64
        || type == i64;
}
#assert(isInteger(i32));
#assert(!isInteger(bool));
```

Selecting function depending on argument type:

```D
$alias selectPrintFunc($type T) {
    if ($isInteger(T)) return $alias(printInt);
    if (T.$isSliceOf(u8) || T.$isArrayOf(u8)) return $alias(print);
    $assert(false, "Invalid type");
}
void write[$alias[]... Args](Args args) {
    #foreach(i, $alias argType; T) {
        selectPrintFunc(argType)(args[i]);
    }
}
```

## CTFE-only functions

When function returns or receives one of meta-types it is consiedered CTFE-only. That means that it can only be called at compile-time. These functions will not appear in the resulting executable.

When CTFE-only function is called in runtime context, this call is evaluated at compile-time.

Example:
```D
i32 func() { return 42; } // some function
$alias getFunc() {
    return func; // return alias to `func`
}
i32 user() {
    getFunc()
}
```

## Compile Time Function Execution (CTFE)

Expressions are evaluated at compile time in such cases:
- enum initializer
- default parameter value
- static array size
- `#if(<cond>)` condition expression
- #assert() arguments
- global variable initializer
- template parameters
- it is call of CTFE-only function