# Small and fast JIT/AOT compiler with zero dependencies

[![CI](https://github.com/MrSmith33/vox/workflows/CI/badge.svg?branch=master&event=push)](https://github.com/MrSmith33/tiny_jit/releases/tag/CI)

* [Latest CI build](https://github.com/MrSmith33/tiny_jit/releases/tag/CI)
* [Channel on /r/ProgrammingLanguages discord](https://discord.gg/HpYYhH4)

# Vox programming language

**Vox** is a multiparadigm programming language inspired by D (60%), Jai (30%), and Zig (10%).

## Main features

- Fast compilation
- Strong metaprogramming
- Can be used for scripting and standalone programs (both JIT and AOT compilation)
- No dependencies (except D compiler)

## Similarities to the D language

- Same syntax for most portions of the language (struct, function, enum, for, while, if, UFCS, slices, arrays)
- Conditional compilation
- Templates, Variadic templates, and functions
- C interoperability
- Modules / Packages

## Differences from the D language

- No GC, minimal runtime, no classes (only structs), no exceptions
- More compile-time features, faster CTFE
- Using templates for heavy calculations is discouraged, instead, CTFE can be used for introspection, and code generation.
- Macros (WIP)
- No C++ interoperability

## Platforms

Supported:
- windows-x64 - host and target
- linux-x64 - host and target
- macos-x64 - only jit-mode

Planned:
- linux-arm64
- wasm
- windows-arm64?
- spirv (Vulkan/OpenCL/OpenGL shaders)

## Project goals

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

- Fibonacci <https://gist.github.com/MrSmith33/9645d9552b567fdbdc1a4d8822b4f1f7>
- Fannkuch <https://gist.github.com/MrSmith33/ac14e66a83b9d047793adede464ca1ef>
- Roguelike tutorial using SDL2 - [repo](https://github.com/MrSmith33/rltut_2019)
- Voxel engine that uses Vox as a scripting language: [Voxelman 2](https://github.com/MrSmith33/voxelman2)
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

## Running & testing

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
    `dmd -i -g -m64 -version=cli main.d -of=../test_work_dir/vox.exe`
    
Release CLI build:
    `ldc2 -d-version=cli -m64 -O3 -release -boundscheck=off -enable-inlining -flto=full -mcpu=native -i main.d -of=../test_work_dir/vox.exe`

Pretty asserts with stack traces are disabled in non-debug builds by default.
* Add `--d-debug=PRETTY_ASSERT` flag to enable pretty asserts in ldc release build.
* Add `-debug=PRETTY_ASSERT` flag to enable pretty asserts in dmd release build.

# Using Commandline interface

## Getting help
Gives the full list of flags
```D
vox --help
```

## Hello world
Produces hello.exe

```
vox hello.vx C:\Windows\System32\kernel32.dll
```

## Input files

* `.vx` files are source code files.
* `.har` files, aka [Human Readable Archive files](https://github.com/marler8997/har). A text file that combines multiple textual files.

Each file must begin with `--- <path>`, three dashes, space, and name.

Files can be nested inside directories `--- dir/dir2/file.txt`.

Example:
```D
--- main.vx
import kernel32;
void main() { ExitProcess(42); }
--- kernel32.vx
noreturn ExitProcess(u32 uExitCode);
```
Can be compiled with `vox program.har C:\Windows\System32\kernel32.dll`

* `.dll` files. Compiler will link external functions by searching symbols inside provided `.dll` file(s).

# CLI Tools
The compiler contains embedded tools:

## PDB dump
Prints content of vox.pdb file into stdout.
```
vox pdb-dump vox.pdb
```

# Compiler overview

## Stats

- Impl size: 35k LoC of D, 2.4MB exe
- Time to compile: 3.5s debug / 45s release
- Test suite: 60ms for 270 tests

For more in detail description of implementation see [internals.md](internals.md)
