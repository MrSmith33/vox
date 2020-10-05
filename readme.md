# Small and fast JIT/AOT compiler with zero dependencies

[![CI](https://github.com/MrSmith33/vox/workflows/CI/badge.svg?branch=master&event=push)](https://github.com/MrSmith33/tiny_jit/releases/tag/CI)

* [Latest CI build](https://github.com/MrSmith33/tiny_jit/releases/tag/CI)
* [Channel on /r/ProgrammingLanguages discord](https://discord.gg/HpYYhH4)

# Vox programming language

**Vox** is a multiparadigm programming language inspired by D (60%), Jai (30%) and Zig (10%).

## Main features

- Fast compilation
- Strong metaprogramming
- Can be used for scripting and standalone programs (both JIT and AOT compilation)
- No dependencies (except D compiler)

## Similarities to D language

- Same syntax for most portions of the language (struct, function, enum, for, while, if, UFCS, slices, arrays)
- Conditional compilation
- Templates, Variadic templates and functions
- C interoperability

## Differences from D language

- No GC, minimal runtime, no classes (only structs), no exceptions
- More compile-time features, faster CTFE
- Using templates for heavy calculations is discouraged, instead CTFE can be used for introspection, and code generation.
- Macros (WIP)
- No C++ interoperability


## Syntax examples

```D
i32 fib(i32 number) {
    if (number < 1) return 0;
    if (number < 3) return 1;
    return fib(number-1) + fib(number-2);
}

struct Point {
    i32 x;
    i32 y;
}

T min[T](T a, T b) {
    if (a < b) return a;
    return b;
}
```

- Roguelike tutorial using SDL2 - [repo](https://github.com/MrSmith33/rltut_2019)
- Example of JIT compilation for amd64 from D code:

<details>
  <summary>code</summary>
  
```D
// Source code
string source = q{
    void test(i32* array, i32 index, i32 value) {
        array[index] = value;
    }
};

// Error handling is omitted
Driver driver;
driver.initialize(jitPasses);
scope(exit) driver.releaseMemory;
driver.beginCompilation();
driver.addModule(SourceFileInfo("test", source));
driver.compile();
driver.markCodeAsExecutable();

// Get function pointer
auto testFun = driver.context.getFunctionPtr!(void, int*, int, int)("test");

// Use compiled function
int[2] val = [42, 56];
testFun(val.ptr, 1, 10);
assert(val[1] == 10);
```
  
</details>

# Project goals

- Strong focus on application extensions
- Maximize user productivity
- Maximize application performance
- AOT and JIT, plugin support, runtime compilation, embedded compiler, tiered compilation
- Static typing
- Great error messages
- Fast / Incremental compilation
- Minimize effort needed for installation, setup, integration
    - Minimal dependencies/encapsulate dependency into module or package
    - Runtime as a library/minimal runtime/no runtime
    - Embedding/extern C
    - Code driven compilation, extending compiler from inside of the program being compiled
- Processor intrinsics
- Conditional compilation
- CTFE, Templates, Introspection, Code generation

Target platforms (Only win64 is supported now):
- amd64 (Windows, Linux, MacOS)
- WebAssembly (browsers)
- ARM (Android, Linux)
- SPIR-V (Vulkan/OpenCL/OpenGL shaders)

# Running & testing

In `main.d` uncomment one of the following lines:
```D
//version = bench; // Runs benchmark
//version = devtest; // Run single test with fine-tuned logging. Useful for development. Uses tester.runDevTests(). Toggle options there for precise analisys while developing.
//version = test; // Runs test suite. Uses tester.runAllTests().
```

and run with: `source> dmd -m64 -i main.d && main`

Benchmarking:
    `ldc2 -d-version=bench -m64 -O3 -release -boundscheck=off -enable-inlining -flto=full -i main.d && main`

Debug CLI build:
    `dmd -i -g -m64 -version=cli main.d -of=../test_work_dir/tjc.exe`
    
Release CLI build:
    `ldc2 -d-version=cli -m64 -O3 -release -boundscheck=off -enable-inlining -flto=full -i main.d -of=../test_work_dir/tjc.exe`

Pretty asserts with stack traces are disabled in non-debug builds by default.
* Add `--d-debug=PRETTY_ASSERT` flag to enable pretty asserts in ldc release build.
* Add `-debug=PRETTY_ASSERT` flag to enable pretty asserts in dmd release build.

# Using Commandline interface

## Getting help
Gives full list of flags
```D
tjc --help
```

## Hello world
Produces hello.exe

```
tjc hello.vx C:\Windows\System32\kernel32.dll
```

## Input files

* `.vx` files are source code files.
* `.har` files, aka [Human Readable Archive files](https://github.com/marler8997/har). Text file that combines multiple textual files.

Each file must begin with `--- <name>`, three dashes, space and name.

Files can be nested inside directories `--- dir/dir2/file.txt`.

Example:
```D
--- main.vx
import utils;
void main() { ExitProcess(42); }
--- kernel32.vx
void ExitProcess(u32 uExitCode);
```
Can be compiled with `tjc program.har C:\Windows\System32\kernel32.dll`

* `.dll` files. Compiler will link external functions by searching symbols inside provided `.dll` file(s).

# CLI Tools
Compiler contains embedded tools:

## PDB dump
Prints content of tjc.pdb file into stdout.
```
tjc pdb-dump tjc.pdb
```

# Compiler overview

## Stats

- Impl size: 30k LoC of D, 2MB exe
- Time to compile: 4.2s debug / 45s release
- Test suite: 52ms for 184 tests

## Passes

* Parsing - produces AST
* Semantic insert - Fills scopes with identifiers
* Semantic lookup - Resolves symbol references
* Semantic types - Type checking
* IR gen - Conversion of AST into linear IR in SSA form
* Optimization - optimizes machine independent IR
* IR to LIR - Conversion of high level IR to machine code IR in SSA form (LIR)
* Live intervals - Collects liveness info about values for use in register allocation
* Linear Scan Register Allocation - Replaces virtual registers with physical ones
* Stack layout - Calculates offsets for stack slots
* Code gen - Converts LIR into machine code
* Linking
* Executable generation (optional, used in non-JIT mode)

## Source code

`source` directory content:

* `/asmtest` - tests for instruction encodings.
* `/ir` - IR specific stuff.
* `amd64asm.d` - instruction encoding for amd64 architecture.
* `ast.d` - Abstract Syntax Tree nodes.
* `ast_to_ir.d` - IR generation.
* `bench.d` - compilation benchmark.
* `driver.d` - compiler driver.
* `emit_mc_amd64.d` - final machine code generation for amd64 arch.
* `identifier.d` - Identifier type.
* `ir_test.d` - implementation of SSA IR construction algorithm.
* `ir_to_lir_amd64.d` - instruction selection for amd64 arch.
* `lir_amd64.d` - LIR for amd64 arch.
* `liveness.d` - liveness analysis.
* `main.d` - compiler entry.
* `optimize.d` - some optimizations for IR.
* `pecoff.d` - loading of `.lib`, `.obj` files. Generation of `.exe` files. Linking utilities. Dumping utilities. Works with files in PE/COFF format.
* `register_allocation.d` - Linear Scan Register Allocation algorithm.
* `semantics.d` - semantic analysis passes.
* `stack_layout.d` - layout of stack slots.
* `tests.d` - compiler test suite.

# Test suite

It uses jitting for most tests, which allows D code to call into compiled test case and to pass D function pointers inside compiled code.
