# Index
* [Metaprogramming](#metaprogramming)

`NEI` marks not yet implemented features.

# Metaprogramming

## Compile-time assert `#assert` (NEI)

```D
#assert(<condition>, <items to print>...);
```

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


### \#foreach (NEI)
`#foreach` will copy a body for each iteration, declaring element and optional index.
`#foreach` does not introduce a new scope.

```D
$alias selectPrintFunc($type T) {...}
void write[$alias[]... Args](Args args) {
	#foreach(i, $alias argType; Args) {
		selectPrintFunc(argType)(args[i]);
	}
	// or
	#foreach($alias arg; args) {
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

- `<T> Identifier` some compile-time known value of type `<T>`
```D
T min[T](T a, T b) {
	if (a < b) return a;
	return b;
}
#assert(min[i32](42, 2) == 2);
```

- `$alias[]...` Variadic parameters. Must be the last template argument. (args with default value may follow). Intead of `$alias` its subtypes can be used.

### Struct templates
```D
struct vec[ElemType, i32 dim]
{
	ElemType[dim] array;
	...
}
alias ivec2 = vec2[i32, 2];
```

## Meta types
---
### $alias
`$alias` is an alias to any symbol in the program. `$alias` values can be compared.
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
`$type` is a an alias that only accepts types.
`$type` values can be compared.
`$type` is implicitly convertible to `$alias`.
```D
$alias some_type = i32; // Ok
$alias some_type = 42; // Error: 42 is not a type
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
bool $isEnum($alias a)

bool $isStruct($type type)
bool $isInteger($type type)
bool $isFloating($type type)
bool $isSlice($type type)
bool $isSliceOf($type slice, $type elemType)
#assert

bool $isArray($type type)
bool $isArrayOf($type array, $type elemType)
bool $isStruct($type type)

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
---
Selecting function depending on argument type:
```D
$alias selectPrintFunc($type T) {
	if (isInteger(T)) return $alias(printInt);
	if (T.$isSliceOf(u8) || T.$isArrayOf(u8)) return $alias(print);
	#assert(false, "Invalid type");
}
void write[$alias[]... Args](Args args) {
	#foreach(i, $alias argType; T) {
		selectPrintFunc(argType)(args[i]);
	}
}
```

### Compile Time Function Execution (CTFE)

Expressions are evaluated at compile time in such cases:
- enum initializer
- default parameter value
- static array size
- `#if(<cond>)` condition expression
- #assert() arguments
- global variable initializer
- template parameters