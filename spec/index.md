# Index

<!-- MarkdownTOC autolink="true" markdown_preview="github" -->

- [Modules](#modules)
    - [Module declaration](#module-declaration)
    - [Import declaration](#import-declaration)
- [Declarations](#declarations)
    - [Variable declarations](#variable-declarations)
- [Types](#types)
    - [Basic types](#basic-types)
    - [Enums](#enums)
    - [Pointers](#pointers)
    - [Slices](#slices)
    - [Static arrays](#static-arrays)
    - [Structs](#structs)
    - [Unions](#unions)
    - [Bitstructs](#bitstructs)
    - [Anonymous structs/unions/bitstructs](#anonymous-structsunionsbitstructs)
    - [Function type](#function-type)
- [Properties](#properties)
    - [.sizeof property](#sizeof-property)
    - [.offsetof property](#offsetof-property)
- [Functions](#functions)
    - [External functions](#external-functions)
    - [Calls](#calls)
        - [Named arguments](#named-arguments)
- [Attributes](#attributes)
    - [Built-in attributes](#built-in-attributes)
    - [User-defined attributes](#user-defined-attributes)
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

# Modules

## Module declaration

By default module name is the same as the file it is contained in. Module declaration can be used to change the name of the module, as well as put it in a package.

Module declaration must be the first declaration of the file.

File can have only single module declaration.

It must be at the top level of the module file and not within any conditional declaration.

Syntax:
```D
module <module_name>;
```

```D
// module without packages
module mod;
// module inside of single package
module pack.mod;
// module inside of 2 packages
module pack1.pack2.mod;
```

## Import declaration

Syntax:
```D
import <module_name>;
```

Imports can occur in any scope where declaration is valid (in module, struct, union, function body)

Order of imports within the scope is not significant.

# Declarations

## Variable declarations

`<type> name [ = <initializer>] ;`

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

`_` character can be used to visually group digits of integer and float literals.

Integer and float literals can have a suffix that is the same as the type name. In that case literal will be of the suffix type. `_` can be placed between value and suffix.

```D
1i8  // 1 of type i8
1_i8 // same, but more readable
42_f32
0xFFFF_u64
```

`noreturn` implicitly casts to any other type. After calling the function returning `noreturn`, control flow will never return to the caller.

`auto` can be used as a type of variable declarations with initializer in modules, structs, unions or inside function body. Such variables will have its type inferred from the initializer.

```D
auto var = 1_f32; // var is of type f32
```

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
i32* pointer = &integer; // & takes address of `integer`
*pointer = 42; // prefix `*` deferences pointer
pointer[0] = 42; // pointers can be indexed like array
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

## Unions

Unions are the same as structs, but all members have offset of 0.

## Bitstructs

## Anonymous structs/unions/bitstructs

Structs and unions can contain anonymous structs, unions or, bitstructs.

```d
struct A {
    union {
        struct {}
        bitstruct {}
    }

    struct {
        union {}
        bitstruct {}
    }
}
```
Code above is equivalent to
```d
struct A {
    union __anon_union1_t {
        struct __anon_struct1_t {}
        __anon_struct1_t __anon_struct1;

        bitstruct __anon_bitstruct1_t {}
        __anon_bitstruct1_t __anon_bitstruct1;
    }
    __anon_union1_t __anon_union1;

    struct __anon_struct2_t {
        union __anon_union2_t {}
        __anon_union2_t __anon_union2;

        bitstruct __anon_bitstruct2_t {}
        __anon_bitstruct2_t __anon_bitstruct2;
    }
    __anon_struct2_t __anon_struct2;
}
```

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

# Properties

## .sizeof property

Can be applied to any type. Returns number of bytes the type takes in memory.

```D
struct Color {
    u8 r;
    u8 g;
    u8 b;
    u8 a;
}

Color.sizeof // yields 4
i32.sizeof // yields 4
u8.sizeof // yields 1
u8[].sizeof // yields 16 (on 64 bit system)
```

## .offsetof property

Can be used on non-static member variables of struct/union types.

```D
struct Color {
    u8 r;
    u8 g;
    u8 b;
    u8 a;
}

Color.r.offsetof // yields 0
Color.g.offsetof // yields 1
Color.b.offsetof // yields 2
Color.a.offsetof // yields 3
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

### Named arguments

Vox follows [DIP1030](https://github.com/dlang/DIPs/blob/master/DIPs/accepted/DIP1030.md), but see limitations below.

Named arguments can be used when calling functions or when constructing structs and unions.

Named and positional arguments can be mixed in a single function call.

```D
i32 func(i32 param1, i32 param2, i32 param3) { ... }
// all these calls are equivalent to func(1, 2, 3);
func(1, 2, 3);
func(param1: 1, 2, 3);
func(1, param2: 2, 3);
func(1, 2, param3: 3);
func(param1: 1, param2: 2, 3);
func(param2: 2, 3, param1: 1);
func(param1: 1, 2, param3: 3);
func(param3: 3, param1: 1, 2);
func(1, param2: 2, param3: 3);
func(1, param3: 3, param2: 2);
func(param1: 1, param2: 2, param3: 3);
func(param1: 1, param3: 3, param2: 2);
func(param2: 2, param1: 1, param3: 3);
func(param2: 2, param3: 3, param1: 1);
func(param3: 3, param1: 1, param2: 2);
func(param3: 3, param2: 2, param1: 1);
```

Named arguments work with default arguments.

```D
i32 func(i32 param1 = 1, i32 param2 = 2, i32 param3 = 3) { ... }
// all these calls are equivalent to func(1, 2, 3);
func(param1: 1);
func(1, param3: 3);
```

Positional arguments always match the parameter that comes after the previous argument. If positional argument is first, then it matches first parameter.

```D
void func(i32 param1, i32 param2) { ... }
// For example: positional argument `3`, that follows named argument `param2: 2`,
// will try to match third parameter, which doesn't exists
func(param2: 2, 3);
//              ^ Error: Trying to provide parameter 3, while `func` has 2 parameters
```

Arguments are evaluated left-to-right at call site.

This will evaluate `param3`, then `param2`, then `param1`:
```D
func(param3: 3, param2: 2, param1: 1);
```

Same syntax works for struct construction
```D
struct Color { u8 r; u8 g; u8 b = 3; u8 a = 4; }
auto col = Color(r: 1, g: 2, b: 3, a: 4); // Color(1, 2, 3, 4)
auto col = Color(a: 10); // Same as Color(0, 0, 3, 10)
```

When named arguments are used to construct a union type, only a single named argument must be used
```D
union Union { u32 u; bool b; f32 f; }
auto u = Union(u: 100);
auto u = Union(b: true);
auto u = Union(f: 10);
```

Limitations:
* Mixing named and positional argumentsStruct constructors are currently fobidden to . This restriction may be lifted in the future.
* Calling variadic functions with named arguments is not supported yet.
* Template arguments do not support named arguments yet.
* Union named arguments are not fully implemented yet.

# Attributes

Attributes can be specified in one of 3 forms

```D
@attr1 decl1; // 1. Directly attached attribute. Applies only to decl1

@attr2: // 2. 
```

1. Direct attachment

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

2. Apply attributes to the end of the current scope.
   
   Doesn't apply to nested scopes (unless it is an attribute block, or #if/#version/#foreach scope, ).

   Can only occur inside module or struct.

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

   Next example shows that `@:` attributes apply inside conditional blocks and attribute blocks. It also shows that those block terminate the effect of `@:` attributes like any other block.

   ```D
   @attr1: // applies to declarations inside #version, #if, #foreach, attribute blocks
   #version(windows) {
       @attr2:
       decl1; // @attr1 @attr2
   }
   #if(true) {
       @attr3:
       decl2; // @attr1 @attr3
   }
   #foreach(item; items) {
       @attr4:
       decl3; // @attr1 @attr4
   }
   @attr5 {
       @attr6:
       decl4; // @attr1 @attr5 @attr6
   }
   void foo() {
       decl5; // no attributes
   }
   struct S {
       decl6; // no attributes
   }
   ```

3. Attribute block. Doesn't apply to nested scopes.
   Attributes that are followed by a block apply to all declarations defined in the block (`decl1` and `decl3`).

   ```D
   @attr1:
   @attr2 @attr3 {
       decl1;        // @attr1 @attr2 @attr3
       @attr4 decl2; // @attr1 @attr2 @attr3 @attr4
   }
   decl3; // @attr1
   ```

   This is equivalent to:

   ```D
   @attr1:
   {
       @attr2 @attr3:
       decl1;
       @attr4 decl2;
   }
   decl3;
   ```

   Attribute block can only occur inside module or struct.

   Attribute block as well as conditionally compiled blocks do not introduce a new scope.

   Also, in those cases outer attributes will be applied to declarations inside curly braces.


## Built-in attributes

* `@extern(syscall, <int>)`
  Can be attached to a function without a body, to make it perform a syscall on call.

  ```D
  @extern(syscall, 60)
  void exit();
  ```
  Functions declared with this attribute will receive calling convention of the target system. For example when compiled for `linux-x64` target calling convention will match System V syscall convention. On targets that have no syscall defined this will result in error. The integer parameter specifies syscall number.

  If several attributes of this type are attached, only latest attribute applies.

* `@extern(module, "external module name")`

  External functions with this attribute will generate entry in import table for the specified module (second parameter). By default the name of external module is used as a name of `.so`/`.dll` library when compiling as an executable. `.so`/`.dll` suffix will be added as (and if) required by the target plaform. External module name should not have `.so`/`.dll` suffix in the source file.

  `NEI` To override the module file name use `--externModule=<external_mod>:<file_name>` CLI switch

  `NEI` Doesn't yet support binding to `.so` libraries

  ```D
  @extern(module, "kernel32")
  noreturn ExitProcess(u32 uExitCode);
  ```

  If several attributes of this type are attached, only latest attribute applies.

  [See more](https://gist.github.com/MrSmith33/b55f6f36c211fc07eb581de2e676e256)


## User-defined attributes

`NEI`

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

* `Identifier` just a type (shortcut for `$type Identifier`)

  ```D
  T min[T](T a, T b) {
      if (a < b) return a;
      return b;
  }
  #assert(min[i32](42, 2) == 2);
  ```

* `T Identifier` some compile-time known value of type `T` (NEI).

  ```D
  T min[$type T](T a, T b) {
      if (a < b) return a;
      return b;
  }
  #assert(min[i32](42, 2) == 2);
  ```

* `Identifier...` Variadic parameters. Must be the last template argument. (args with default value may follow).

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