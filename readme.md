# Small and fast JIT/AOT compiler with zero dependencies

Project/language name is still missing.

# Project goals

Strong focus on application extensions
- Maximize user productivity
    - Static typing
    - Great error messages
    - Fast / Incremental compilation
    - Minimize effort needed for installation, setup, integration
        - Minimal dependencies/encapsulate dependency into module or package
        - Runtime as a library/minimal runtime/no runtime
        - Embedding/extern C
        - Code driven compilation, extending compiler from inside of the program being compiled
    - Runtime module loading/runtime module compilation
    - AOT and JIT, plugin support, runtime compilation, embedded compiler, tiered compilation
- Maximize application performance
    - Static typing

Target platforms (Only win64 is supported now):
- amd64 (Windows, Linux, MacOS)
- WebAssembly (browsers)
- ARM (Android, Linux)
- SPIR-V (Vulkan/OpenCL/OpenGL Шейдеры)


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
runBench(); // Runs benchmark
runAllTests(StopOnFirstFail.yes); // Runs test suite
runDevTests(); // Run single test with fine-tuned logging. Useful for development.
```

Run with: `source> dmd -m64 -i main.d`

# What works

- You can use code in `compiler1.d` as JIT compiler for amd64:
```D
string source = q{
    void test(i32* array, i32 index, i32 value) {
        array[index] = value;
    }
};

// Error handling is omitted
Driver driver;
driver.initPasses();
ubyte[] codeBuffer = alloc_executable_memory(PAGE_SIZE * 8);

ModuleDeclNode* mod = driver.compileModule(source, codeBuffer);
FunctionDeclNode* funDecl = mod.findFunction("test", &driver.context);
alias FuncT = extern(C) void function(int*, int, int);
FuncT fun = cast(FuncT)funDecl.irData.funcPtr;

int[2] val = [42, 56];
fun(val.ptr, 1, 10);
assert(val[1] == 10);
```

- Executable generation:


# Roadmap

- [x] Finish code gen for function calls.
- [x] Add LIR (Lowlevel IR)
- [x] Implement loops.
- [x] Integrate code for executable generation in main code.
- [x] Add a way for binding with DLLs in a language. DLL import tables are already implemented by `pecoff.d` file.
- [x] Link with SDL library
- [x] Make standalone compiler: read files from disk, write executable.
- [ ] Make a game with SDL
- [ ] Implement modding system