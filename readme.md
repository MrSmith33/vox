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

Target platforms:
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
* Live intervals - Collects liveness info about values for use in register allocation
* Linear Scan Register Allocation - Replaces virtual registers with physical ones
* Stack layout - Calculates offsets for stack slots
* Code gen - Converts IR into machine code


## Source code

`source` directory content:

* `compiler1.d` - current implementation/tests. Single file.
* `amd64asm.d` - instruction encoding for amd64 architecture.
* `pecoff.d` - loading of `.lib`, `.obj` files. Generation of `.exe` files. Linking utilities. Dumping utilities. Works with files in PE/COFF format.
* `ir_test.d` - implementation of SSA IR construction algorithm.
* `tinyc.d` - port of tinyc compiler/VM from C to D.
* `phi_resolution.d` - SSA deconstruction algorithm.
* `/lang` - old implementation of compiler. Lacks newer features, but has function calls and loops working.
* `/asmtest` - tests for instruction encodings.


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

1. Finish code gen for function calls.
2. Add LIR (Lowlevel IR)
2. Implement loops.
3. Integrate code for executable generation in main code.
4. Add a way for binding with DLLs in a language. DLL import tables are already implemented by `pecoff.d` file.
5. Link with SDL library
6. Make standalone compiler: read files from disk, write executable.
7. Make a game with SDL
8. Implement modding system