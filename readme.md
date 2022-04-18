# Small and fast JIT/AOT compiler with zero dependencies

[![CI](https://github.com/MrSmith33/vox/workflows/CI/badge.svg?branch=master&event=push)](https://github.com/MrSmith33/tiny_jit/releases/tag/CI)

* [Latest CI build](https://github.com/MrSmith33/tiny_jit/releases/tag/CI)
* [Channel on /r/ProgrammingLanguages discord](https://discord.gg/HpYYhH4) - Projects Q-Y -> Vox

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

- Cross-platform hello world <https://gist.github.com/MrSmith33/34a7557ad5ac23ebe6cf27bef15a39a6>
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

`dmd -i -g -m64 -version=cli main.d -of=vox.exe`

Release CLI build:

`ldc2 -d-version=cli -m64 -O3 -release -boundscheck=off -enable-inlining -flto=full -mcpu=native -i main.d -of=vox.exe`

Debug shared library build:

`ldc2 -m64 -shared -g -d-debug -fvisibility=hidden -link-defaultlib-shared=false -i c_api.d -of=libvox.dll`

Compiling with Profile Guided Optimization:

```
ldc2 -d-version=test -m64 -release -fprofile-instr-generate -mcpu=native -i main.d -of=vox_instrumented.exe
vox_instrumented
ldc-profdata merge default.profraw -output vox.profdata
ldc2 -d-version=cli -m64 -O3 -release -boundscheck=off -enable-inlining -flto=full -mcpu=native -fprofile-instr-use=vox.profdata -i main.d -of=vox.exe
```

# Using Commandline interface

## Getting help

Gives the full list of flags
```D
vox --help
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
@extern(module, "kernel32")
noreturn ExitProcess(u32 uExitCode);
```
Can be compiled with `vox program.har`


# CLI Tools
The compiler contains embedded tools:

## PDB dump
Prints content of vox.pdb file into stdout.
```
vox pdb-dump vox.pdb
```

# Syntax highlighting

## GitHub

To get some syntax highlighting on GitHub define `.gitattributes` file in the repository with the following content ([docs](https://github.com/github/linguist/blob/master/docs/overrides.md)):

```
*.vx linguist-language=D
```

All `.vx` files will be highlighted and classified as D.

## Editor

* [Vim](https://github.com/jedekar/vim-vox)

# Compiler overview

## Stats

- Impl size: 40k LoC of D, 3MB exe
- Time to compile: 4s debug / 45s release
- Test suite: 95ms for 361 tests
- Time to compile 10MLoC of fibonacci: 8s on Windows, 7.5s on Linux

For more in detail description of implementation see [internals.md](internals.md)
