# Small and fast JIT/AOT compiler with zero dependencies

[![Build status](https://ci.appveyor.com/api/projects/status/3os1s4a34hl83r0b?svg=true)](https://ci.appveyor.com/project/MrSmith33/tiny-jit)

WIP language name is `Vox`, file extension is `.vx`.

[Latest CI build](https://github.com/MrSmith33/tiny_jit/releases/tag/CI)

# Major features

- Can be embedded into D programs (can call host functions)
- Works as a regular AOT compiler producing precoff excutables (with dll importing)
- Only requires D compiler, no other dependencies

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

# Compiler

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

# Running & testing

In `main.d` uncomment one of the following lines:
```D
//version = bench; // Runs benchmark
//version = devtest; // Run single test with fine-tuned logging. Useful for development. Uses tester.runDevTests(). Toggle options there for precise analisys while developing.
//version = test; // Runs test suite. Uses tester.runAllTests().
```

Run with: `source> dmd -m64 -i main.d && main`

Benchmarking:
    `ldc2 -d-version=bench -m64 -O3 -release -boundscheck=off -enable-inlining -flto=full -i main.d && main`

Debug CLI build:
    `dmd -i -g -m64 -version=cli main.d -of=tjc.exe && move /Y tjc.exe ../test_work_dir/tjc.exe`
    
Release CLI build:
    `ldc2 -d-version=cli -m64 -O3 -release -boundscheck=off -enable-inlining -flto=full -i main.d -of=tjc.exe && move /Y tjc.exe ../test_work_dir/tjc.exe`

# What works

- win64 executable generation
- dll importing
- Example of JIT compilation for amd64 from D code:
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

- Roguelike tutorial using SDL2 - [repo](https://github.com/MrSmith33/rltut_2019)
