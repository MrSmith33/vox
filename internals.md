# Memory management

Memory is allocated via a set of arenas. At the start compiler reserves a big chunk of memory (186 GiB atm), and bits of that memory are given to each arena.

On windows memory is reserved via a call to `VirtualAlloc` with `MEM_RESERVE` flag. Then each arena is responsible to commit memory on demand with `VirtualAlloc + MEM_COMMIT` call. I stopped on 64k commit size as higher sizes do not improve time too much.

On linux I wasn't able to archive pure reserve, so compiler relies on overcommit to be enabled and commits the whole arena pool at the start. The OS is then responsible for allocating memory pages on first access.

# Passes

## General overview

* Read source - reads source files into memory
* Lex - produces token array per source file
* Parse - produces AST
* Register names - register definitions within scopes
* Lookup names - resolves symbol references
* Type check
* Generate IR - conversion of AST into linear IR in SSA form
* Optimize - optimizes machine-independent IR
    - inline
    - DCE - Dead Code Elimination: removes instructions with unused results and no side-effects
* IR lower
    - lower ABI
    - lower aggregates to stack slots
    - lower switch
    - lower GEP
* IR to LIR AMD64 - Conversion of high-level IR to amd64 machine code IR in SSA form (LIR)
* Live intervals - Collects liveness info about values for use in register allocation
* Linear Scan Register Allocation - Replaces virtual registers with physical ones
* Stack layout - Calculates offsets for stack slots
* Code gen - Converts LIR into machine code
* Link executable
* Write executable (optional, used in non-JIT mode)

For me TAC means single operation per instruction, instruction takes arguments and returns some number of results (0 or 1 currently). In CFG basic blocks contain a list instructions with single entrance and single exit, and track a list of predecessor/successor basic blocks.

Generate IR creates the IR with those properties and uses abstract instructions and virtual registers. Lower ABI introduces machine registers. IR to LIR switches from abstract to machine instructions. Linear scan replaces virtual registers with machine registers, destructs SSA form and also fixes two-operand form instructions. Code gen emits actual machine code.

## Read source

Files use 2 arenas: one for file info records (`SourceFileInfo`) and one for file contents.

Source files are read sequentially into an arena.

Before each source there is a start of input char `char SOI_CHAR = '\2'`, and after last source char there is end of input char `char EOI_CHAR = '\3'`.

During lexing those turn into corresponding tokens: `TokenType.SOI`, `TokenType.EOI`.

## Lexing

Lexer produces `TokenType` and `SourceLocation` per token generated. Those are put into 2 arenas (`tokenBuffer` and `tokenLocationBuffer`). Tokens of all source files live in the same arenas, so they share index space. Token index is 32-bit number. AST nodes then store just a single `TokenIndex`.

Atm `SourceLocation` is 16 bytes:

```D
uint start;
uint end;
uint line;
uint col;
```

and `TokenType` is a single byte enumeration.

`SourceFileInfo` also stores index of the first token of the source file. And since all files are loaded sequentially, `firstTokenIndex` also grows monotonically with each file added. We can then use `firstTokenIndex` to find the file that contains any given token. This is used for error location printing.

## Parsing

Parsing uses recursive descend + Pratt parsing for expressions. Types are considered an expression too.

AST nodes are allocated in `Arena!uint astBuffer` with 4-byte allocation/addressing granularity. AST nodes use 32-bit indicies to refer to each other. This way arena has capacity for up to 16 GiB of nodes.

For the times where AST nodes need to allocate arays `ArrayArena` is used, which has multiple pools for fixed sizes.

All identifiers are interned and 32-bit identifier index is used everywhere (`struct Identifier`).

All AST node allocations are sequential in memory, so template system takes advantage of that. Templates store the range of AST arena slots that are within the template. When new template instance is created, a copy of that slot range is made at the end of the arena. Then a tree walk fixes indicies and duplicates the arrays.

Named declaration nodes (`NameUseExprNode`/`AliasDeclNode`/`EnumDeclaration`/`EnumMemberDecl`/`FunctionDeclNode`/`StructDeclNode`/`TemplateDeclNode`/`VariableDeclNode`) keep track of parent scope, so that in `Register names` pass they can register themselves into that scope.

## Semantic passes
### Register names

This pass walks the AST nodes in all files and registers itself in the parent scope.

When we have a single node to register it just registers itself and requests name registering from nested nodes.

Because Vox supports conditional compilation in the form of `#if` and `#foreach`, name registering pass happens in two steps for arrays of nodes (for example struct members, or block statements):

First we request self registration of nodes. This process yields a linked list of all nodes implementing conditional compilation. Then we walk the conditional nodes and expand them in-place. Right after insertion we recursively repeat the first step.

At the end we have all conditional nodes deleted and/or replaces with some of their children. All of the nodes in the sequence were self registered, and no nested nodes were registered yet.

Then we walk all the nodes in array and request registering of their children.

One non-ordinary thing here is that `#if` condition or `#foreach` expression need to be evaluated, which means that condition subtree needs to be processed up to `Type check` stage and evaluated.

### Lookup names

Every name use expression needs to resolve the node it references. To do this every name use node stores the parent scope in which it occured within source code. Parser tracks scopes and passes them into the node for future use.

So, when name lookup pass comes to name use node it consults the scope tree.

Algorithm is the same as in D (See <https://dlang.org/spec/module.html#name_lookup>).

1. Walk up the scope tree looking at each step in the scope. Once found it the symbol.
2. If not found. Then restart walking the scope tree, but this time check in imports.

Another place where name resolution happens is member lookup in member expression (`parent.member`). This only check the parent scope for names.

### Type check

Performs implicit type conversions. Checks types where specific type is expected, like function call argument (`foo(arg)`), variable assignment (`i32 a = 42;`) etc. Finds common type for binary operations (`a + b`).

## IR generation

Uses algorithm from `Simple and Efficient Construction of Static Single Assignment Form by Braun et al. 2013`

## IR Optimization



## IR lowering

It does 3 subpasses:
* ABI lowering
* Aggregate lowering
* GEP-instruction lowering

### ABI lowering

For each IR function it transforms `parameter` and `ret_val` instructions to follow own calling convention, and converts `call` instructions to follow callee's calling convention.

Currently Vox backend implements 3 CCs: `win64`, `sysv64`, and `sysv64_syscall`.



### Aggregate lowering

Rewrites virtual registers that contain aggregates, that do not fit into a register, into stack slots. Atm algorithm is unfinished and produces redundant copies.

### Switch lowering

Is written as part of aggregate lowering pass. Currently translates switch instruction into a chain of branches.

### GEP lowering

Rewrites Get Element Pointer instructions into pointer arithmetic instructions.

## IR to LIR translation
    aka instruction selection

Vox compiler uses basic macro expansion approach, where for each instruction of IR an instruction of target architecture is emitted (currently only for amd64).

* Empty LIR is created
* I create an array to store mapping from IR virtual registers to LIR virtual registers
* Stack slots are duplicated from IR to LIR without changes
* Basic blocks and their phi functions are recreated in LIR. New basic blocks have identical indicies with old ones.
* Old basic blocks are iterated
* BB instructions are iterated
* For each instruction we either emit a single instruction

## Liveness analysis
## Register allocation

Implements linear scan algorithm from `Linear Scan Register Allocation on SSA Form`

## Stack layout

Parameters layout is determined by ABI lowering pass.

Other stack slots are arranged by alignment (big to small). This way they are tightly packed and their alignment is respected.

## Code generation

Here actual machine code is emitted. So far only amd64 is supported.

First global variables are allocated to the static data sections (read-only and read-write). Symbols that are zeroed are allocated at the of those sections. In case we are producing executable, the file will not contain those zeroes.

Then we walk all functions in the program and generate their machine code.

First function prolog is emitted. It inserts code for frame pointer creation (if needed). Then stack reservation (if needed).

Function body is compiled. We walk all basic blocks and instruction in each BB. Appropriate instruction encoding is selected depending on argument kind (register, stack slot, global, function, constant), register class (GPR, XMM) and instruction size. For arguments that reference globals or functions a new `ObjectSymbolReference` is created (needed for linking later).

Some instructions modify stack pointer (`push`s before function call, `add`/`sub` to stack pointer), so we track that as an additional offset. Then, when compiling instructions that reference stack slots we need to add that offset to the stack slot offset to compensate.

Jumps between basic blocks need further fixup. Code generator allocates a table of 2 pointers per basic blocks, allowing to have 0-2 successors per basic block. When compiling branch/jump instructions it will store the address of 4 byte offset into that buffer. No jump is generated if the target is the next block.

Because IR guarantees single return instruction, epilog is generated as part of that instruction codegen. It deallocates stack and frame pointer as needed. Then return instruction is inserted.

After that jump fixup is performed and size of final machine code is calculated.

## Linking

When generating IR, frontend creates a new object symbol (`ObjectSymbol`), per function and per global variable. `ObjectModule` is created per module. There is also `ObjectSection`, which tracks section info of resulting excutable file, and `ObjectSymbolReference`, which represents reference from one symbol to another.

Symbols are abstracted as arrays of bytes within a section and module. Reference is a slice of a symbol that contains address (absolute or relative) to another symbol + extra data.

After code generation step all globals are put in the globals arena, and have finalized address in the section (read-only or read-write data sections). Same with functions that have machine code in code arena.

Linking pass walks all local modules, and for each reference performs a fixup.

When compiling in JIT-mode host can export its own functions as symbols to the compiled program. They are put into a host module. If the registered function address is too far from the code that refences it, then it is put into import table. Linker handles both relative reference and indirection through import table.

When compiling standalone executable, user can also pass `.dll` files to the compiler and all external functions will be resolved to one of them. Such references are implemented by generating import table in the executable (`.idata` for PE/COFF).

## Executable creation

Generation of `win64` and `elf64` executables is supported. For windows compiler can generate references to `.dll` files provided through CLI.

Following sections are generated (if they are non-empty):
* `.text` - Executable code section
* `.idata` - Import table section (only on win64)
* `.data` - Read-write static data section
* `.rdata` - Read-only static data section

First sections are created and import table is filled. Then linking pass is run. Necessary data is filled in the PE and COFF headers and written to the buffer. Section headers are written. Then data of each section is written. Now final executable is fully in the buffer.

## Writing executable

Executable that was stored in arena buffer is written to the target file. On posix platforms it is given `rwx` rights.

# AST

AST nodes are allocated sequentially in an arena.

```D
Arena!uint astBuffer;
```

AST nodes are addressed with indicies. Indexing goes in 4 byte granularity. 32-bit indicies x 4 bytes gives 16 GiB of addressable memory.

All arrays/hashmaps for AST are stored in a separate set of arenas. For example list of children node indicies are stored in such array. IR arrays use dedicated arena.

Each AST node has common header (8 bytes in total):

```D
struct AstNode
{
    // Index to the token from the lexer
    TokenIndex loc;     // 4 bytes

    // Type of AST node. Code often switches on it
    AstType astType;    // 1 byte

    // Tracks node state as it progresses through semantic passes
    // This is needed because nodes can be analysed in any order
    // Needed to find ciclic dependencies in the program
    AstNodeState state; // 4 bits

    // Some nodes may need to further differentiate node type
    uint subType;       // 4 bits

    // Bitflags
    ushort flags;       // 2 bytes
}
```



# IR

Program IR consists of globals, constants, types and functions. Functions belong to modules, but modules are pretty much unnessesary abstraction at this point.

## IR Function

### Storage

Function IR is stored in a set of arenas that reside in `CompilationContext`.

```D
Arena!IrInstrHeader     instrHeaderBuffer;
Arena!IrIndex           instrPayloadBuffer;
Arena!IrIndex           instrNextBuffer;
Arena!IrIndex           instrPrevBuffer;
Arena!IrPhi             phiBuffer;
Arena!IrVirtualRegister vregBuffer;
Arena!uint              arrayBuffer;
Arena!IrBasicBlock      basicBlockBuffer;
Arena!StackSlot         stackSlotBuffer;
```

Each IR function then points to a slice of items within each of those arenas. (see `IrFunction` struct)
Length of the slice is stored in the corresponding field of the `IrFunction`.

```D
// Pointers
IrInstrHeader*     instrPtr;
IrIndex*           instrPayloadPtr;
IrIndex*           instrNextPtr;
IrIndex*           instrPrevPtr;
IrPhi*             phiPtr;
uint*              arrayPtr;
IrVirtualRegister* vregPtr;
IrBasicBlock*      basicBlockPtr;
StackSlot*         stackSlotPtr;

// Length of:
uint numInstructions;     // instrPtr, instrNextPtr, instrPrevPtr
uint numPayloadSlots;     // instrPayloadPtr
uint numPhis;             // phiPtr
uint arrayLength;         // arrayPtr
uint numVirtualRegisters; // vregPtr
uint numBasicBlocks;      // basicBlockPtr
uint numStackSlots;       // stackSlotPtr
```

Because each arena works as virtually infinite array all new items are added to the end of the array.

This imposes a restriction that only one function IR can be created / modified at a time. Modification that needs to add new items requires a function copy to be created at the end of the arenas.

Copying function IR requires only one memcopy per arena without the need of any fixups, since all references are done relative to the start of respective item slice.

This also makes inlining fast. Function IR that is being inlined is appended to the end of the arena and all references in those items are offset by the size of the original IR. Such offset is done via an offset table thanks to clever design of `IrIndex`.

To add a new item to the any of the function arrays we need to tell the corresponding arena to provide space for one more item at the end and grow the corresponding length field in the `IrFunction`.

For example adding a new instruction looks like:

```D
ir.numInstructions += numSlots;
context.irStorage.instrHeaderBuffer.voidPut(numSlots);
context.irStorage.instrNextBuffer.voidPut(numSlots);
context.irStorage.instrPrevBuffer.voidPut(numSlots);
```

adding new virtual register:

```D
ir.numPhis += 1;
context.irStorage.phiBuffer.put(IrPhi());
```

etc. See [`ir_builder.d`](source/ir/ir_builder.d) for more.

---

All function IR entities are accessed by index into the corresponding array.

Function consists of a linked list of basic blocks (BB). There are two special BBs: `entry` and `exit` blocks.

BBs are stored in a memory described with `basicBlockPtr/numBasicBlocks` pair.

`entry` BB always has index `0`.

`exit` BB always has index `1`.


## IR Basic Block

Each BB has a linked list of phi functions and a linked list of instructions.

Each BB has pointers to the next and previous BBs in a function. They specify the order of BBs in a function.

Each BB also has a list of predecessor BBs and successor BBs. They represent the control flow edges between BBs. The order of predecessors matches the order of phi functions. Number of arguments of each phi function in that BB matches the number of predecessors and they correspond by index.

Each BB has at least one instruction. Each BB must end in an instruction that trasfers the control (jump, branch or return). The successors specify the target BBs of the that last instruction.

```D
IrIndex firstInstr; // first instruction
IrIndex lastInstr;  // last instruction
IrIndex prevBlock;  // null only if this is entryBasicBlock
IrIndex nextBlock;  // null only if this is exitBasicBlock
IrIndex firstPhi;   // may be null
IrSmallArray predecessors;
IrSmallArray successors;
```

## IR Instruction

### Header

All instructions have common header (`IrInstrHeader`), which is stored in `instrPtr/numInstructions` arena.

The instruction format supports instructions with 0 or 1 results and with up to 255 arguments.

Header contains:
- 16-bit opcode field
- 8-bit number of arguments
- 8-bit bitfield for various purposes
    - 1 bit for `hasResult` flag (on all instructions)
    - 4 bits for condition kind (for instructions with condition, like comparison or conditional jump)
- 32-bit payload offset index. Needed because number of arguments is not fixed.

```D
ushort op;
ubyte numArgs;
ubyte flags;
uint payloadOffset;
```

Separation of header from payload gives us dense indexing, which is important if we want to associate extra data with each instruction. This is handy during liveness anaysis and register allocation when we need to map between instruction and its global index for linear scan register allocation algorithm.

Dense indexing is also used during instruction selection, but with virtual registers.

### Payload

Instruction payload is stored in `instrPayloadPtr/numInstructions` arena.

The layout is the following:

`[0-or-1 result][0-255 arguments][optional payload]`

`payloadOffset` points to the 0-th argument.

If instruction has result, it is located before the arguments. Can be accessed with `instrPayloadPtr[payloadOffset-1]`.

Optional payload can be used to store additional data that is specific to concrete opcode. It's size is opcode specified. Atm only `parameter` instruction uses it to store parameter index.

`result` field (if present) specifies virtual register that stores the result of the instruction. In some cases it may specify the physical register, like in the case of machine instructions.

In case the result is of virtual register kind, the virtual register will point its `definition` field to the defining instruction.

Arguments specify instruction arguments and they may be of `virtualRegister`/`constant`/`constantAggregate`/`constantZero`/`func`/`stackSlot`/`global` kinds.

### Instruction order

Since we are storing all instructions in an array we cannot add/remove/reorder them easily and cheaply. To solve that each instruction has associated indicies of next/prev instruction. They are stored separately, so that only indicies for the relevant iteration direction are loaded into data cache, as passes usually iterate over all BB instructions either forwards or backwards.

Most of the instructions remain in the order they were generated by the frontend, so in most cases the next instruction will be adjacent in memory. (Disclaimer: I didn't measure different layouts of next/prev instruction indicies).

Next/prev indicies are stored in `instrNextPtr/numInstructions` and `instrPrevPtr/numInstructions` arenas.

## Phi functions

They are stored in `phiPtr/numPhis` arena.

```D
IrIndex blockIndex;
IrIndex result;
IrIndex var;
IrIndex nextPhi;
IrIndex prevPhi;
IrSmallArray args;
```

Each BB has index of the first phi function (`firstPhi` field).

After that all phi functions are stored in an intrusive linked list specified by `nextPhi`/`prevPhi`.

`blockIndex` specifies the BB that phi function resides in.

Order of arguments matches the `blockIndex.predecessors`.

`var` field is used during initial SSA IR construction, and is ignored afterwards.

`args` stores the array of arguments of the phi function. One argument per predecessor BB.

`result` specifies virtual register that stores the result.

## Virtual register

```D
IrIndex definition;
IrIndex type;
IrSmallSet users;
```

`definition` stores index of instruction or phi that defines this register. Since we have SSA form, each vreg has single definition point.

`type` stores type index.

`users` is a multi-hash-set that stores a set of all instruction or phi indicies that use this register. Before it was an array, but this caused O(n^2) time in DCE pass when a lot of instructions were dead. The precise number of users is needed because this info is used in liveness analysis.

The way allocation works is that IR of the function which is currently being created must be at the end of each arena. This way adding new items to any arena if O(1) with no reallocation.

## Arrays

In `arrayPtr/arrayLength` arena we store array payload for big `IrSmallArray`/`IrSmallSet`.

`IrSmallArray` consists of 2 `IrIndex` slots, which either encode 0-2 items inline, or `offset + length` into the `arrayPtr`.

`IrSmallSet` consists of 4 `IrIndex` slots, which either encode 0-4 items inline, or `offset + length + unique length + capacity` into the `arrayPtr`.

## IrIndex

`IrIndex` represents index of any IR entity.

It is a 32 bit value.

Top 4 bits of `IrIndex` stores the `kind`:

```
enum IrValueKind {
    none
    instruction
    basicBlock
    constant
    constantAggregate
    constantZero
    global
    phi
    stackSlot
    virtualRegister
    physicalRegister
    type
    variable
    func
    array
}
```

At the moment 15 out of 16 possible values are used.

The meaning of the rest of the 28 bits depends on the `kind` value.

- `none` - when the whole `IrIndex` is zero it represents undefined index. In some cases lowest 28 bits can be used to store plain integer data. For example it is used in such way in `IrSmallArray`/`IrSmallSet` to store length/capacity. Values of this kind will not be fixed during IR appending during inlining.
- `instruction` - index into the `IrFunction.instrPtr`
- `basicBlock` - index into the `IrFunction.basicBlockPtr`
- `constant` - see IR Constant section
- `constantAggregate` - lower 28 bits is an index into `IrConstantStorage.aggregateBuffer` arena, which stores `IrAggregateConstant` values. `IrAggregateConstant` has `IrIndex type` and `uint numMembers`. Then follow the values of the members of this aggregate. In case of array/struct value of each member follows. In case of union there are always 2 members: member 0 is an index, member 1 is a value.
- `constantZero` - means that it is a constant of some type where all values bits are zero. The type of the constant is encoded in the same way as when `IrIndex` is of `IrValueKind.type` `kind`.
- `global` - index of a global (`IrGlobal`). Globals are stored in `CompilationContext.globals.buffer` arena.
- `phi` - index into the `IrFunction.phiPtr`
- `stackSlot` - index into the `IrFunction.stackSlotPtr`
- `virtualRegister` - index into the `IrFunction.vregPtr`
- `physicalRegister`
    + next highest 8 bits specify register class
    + next highest 8 bits specify register size
    + lowest 12 bits specify register index
- `type` - next highest 4 bits specify type kind (`IrTypeKind`), and lowest 24 bits are an index of a type. Index points into `CompilationContext.types.buffer` arena.
- `variable` is used by frontend to refer to specific variable in the source language, and IR builder uses it in its SSA creation process. After IR is created `variable` is no longer used anywhere.
- `func` - atm lowest 28 bits just store an index of the AST node. Needs to point to backend owned data.
- `array` - used by `IrSmallArray`/`IrSmallSet` to refer to an offset into the `IrFunction.arrayPtr` when big array/map is needed.

### IR Constant

Next 2 highest bits specify the kind of a constant (`IrConstantKind`)
- `intUnsignedSmall` and `intSignedSmall` specify that lowest 24 bits store actual constant bits. Signedness is needed to correctly sign-extend the value to the full size.
- `intUnsignedBig` and `intSignedBig` specify that the value doesn't fit into lowest 24 bits of `IrIndex` and instead are stored in `IrConstantStorage.buffer` arena. That arena stores `IrConstant` 64 bit values.

Next 2 highest bits specify the size of a constant (8/16/32/64 bits)

### IR type

```D
enum IrTypeKind {
    basic,
    pointer,
    array,
    struct_t,
    func_t
}
```

- `basic` - lowest 24 bits encode one of the basic types (misnamed as `IrValueType` atm)
  ```D
  enum IrValueType {
      noreturn_t,
      void_t,
      i8,
      i16,
      i32,
      i64,
      f32,
      f64,
  }
  ```
- `pointer` - points into the type arena (`IrTypePointer`), where base type of the pointer is stored (`IrIndex baseType`).
- `array` - points into the type arena (`IrTypeArray`), where base type of the array (`IrIndex elemType`) and number of elements (`uint numElements`) is stored.
- `struct_t` - points into the type arena (`IrTypeStruct`).
  ```D
  struct IrTypeStruct {
      uint size;
      ubyte alignmentPower;
      bool isUnion;
      uint numMembers;
      IrTypeStructMember[0] members_payload; // all struct/union members follow in memory
  }
  struct IrTypeStructMember {
      IrIndex type;
      uint offset;
  }
  ```
- `func_t` - points into the type arena (`IrTypeFunction`).
```D
struct IrTypeFunction {
    uint numResults;
    uint numParameters;
    CallConvention callConv;
    ushort syscallNumber;
    IrIndex[0] payload; // result types followed by parameter types
}
```

### IR global

Stores its type and symbol index for linking process.

## Function inlining

Before inlining starts the caller is already in modifiable state, meaning that functions data is located at the end of the arenas.

### Appending the callee

We copy the callee's IR to the end of the callers IR.

Now we need to increment indicies in the appended IR to account for callers IR entities.

In `instrHeaderBuffer` only `_payloadOffset` needs an increment by `callerIr.numPayloadSlots`.

All other items are made in a way, so that they can be viewed as valid `IrIndex[]` and a single increment per `IrIndex` is performed. The increment depends on the kind of index. Currently there is only 16 kinds, so we build an offset table with 16 entries, populated by the arena sizes of caller IR.

```D
uint[16] offsets;
offsets[IrValueKind.instruction] = callerIr.numInstructions;
offsets[IrValueKind.basicBlock]  = callerIr.numBasicBlocks;
...
// items that need no increment remain 0 and do not affect IrIndex being fixed
```

Then we walk the `IrIndex[]` and per each item we do:

```D
index.asUint = oldIndex.asUint + offsets[oldIndex.kind];
```

# Function arguments and register allocation

Before register allocation there is ABI lowering pass that replaces calls of a form:

```D
call fun, i32 50, i32 50, i32 32, i8 0, i8 255, i8 0, {i64, i8*} {6, g0}, i8 1
```

into

```D
 0: store {i64, i8*}* s0, {i64, i8*} {6, g0}
 2: push i8 1
 4: push {i64, i8*}* s0
 6: push i8 0
 8: push i8 255
10: r1<c0 s2> = move i32 50
12: r2<c0 s2> = move i32 50
14: r8<c0 s2> = move i32 32
16: r9<c0 s0> = move i8 0
18: grow_stack i8 32
20: call fun, r1<c0 s3>, r2<c0 s3>, r8<c0 s3>, r9<c0 s3>
22: shrink_stack i8 64
```

Liveness analysis then needs to assign correct liveness intervals.
r1 for example must be live in [10; 20) interval, so that no virtual register gets this physical register.

Liveness analysis pass iterates basic blocks from end to start. And it needs to handle any instruction that takes/returns data in physical registers. In general such instruction can be preceded by movs to physical registers and followed by movs from physical registers.

Function calls can also have an instruction between the call and movs for stack alignment. This needs to be accounted for. For that call instruction has 2 bit flags saying to extend live ranges before or after the instruction for physical registers.

Example:

```D
 2: eax = v.20 // args[0]
 4: ecx = v.45 // args[2]
 6: optional non-mov instruction (for call, which has extendFixedArgRange)
 8: edx, ecx = some_instr(eax, v.100, ecx) // edx is results[0]
10: optional non-mov instruction (for call, which has extendFixedResultRange)
12: v.200 = edx // results[0] (aka result)
14: v.300 = ecx // results[1]
```

The algorithm is to iterate through arguments and count the number of physical registers.
Then for each physical register argument add fixed live interval of correct length.

For example eax is 0-th physical argument of 2 and we have extendFixedArgRange set.
Then we add live range `[2; 8)` for eax, calculated as `((NUM_REGS - REG_INDEX) + extendFixedArgRange) * STEP == ((2 - 0) + 1) * 2 == 6. [8-6; 8)`

Or one can reverse the order of movs or the order of iteration to simplify the equation.

For functions liveness analysis also adds fixed intervals for all volatile registers.

Here is the resulting live ranges
```D
 |            26| ecx = mov i32 50
 ||           28| edx = mov i32 50
 ||     |     30| r8d = mov i32 32
 || |   ||    32| r9b = mov i8 0
 || |   ||    34| rsp = sub rsp, i8 32
||| |   ||||  36| call fun, rcx, rdx, r8, r9
    |         38| rsp = add rsp, i8 64

 0 rax [rax]: [36; 37)
 1 rcx [rcx]: [26; 37)
 2 rdx [rdx]: [28; 37)
 4 rsp [rsp]: [32; 40)
 8 r8  [r8]:  [30; 37)
 9 r9  [r9]:  [32; 37)
10 r10 [r10]: [36; 37)
11 r11 [r11]: [36; 37)
```

Same idea works with instructions that use fixed registers for arguments and results such as `idiv`.

# Handling two-address form instructions

Two-address form is used in instructions of form `dst = op src0 src1`, where `dst` is the same register as `src0`.

It is handled after register allocation.

Ideally register allocator needs to be hinted to allocate `dst` to the same register as `src0` (I do not do this) and if they are still different the basic idea is to insert a `mov dst, src0` before this instruction, so you get:

```D
mov dst, src0
dst = op dst src1
```

But you need to make sure that `dst != src1`, otherwise that `mov` will overwrite the `src1`. To avoid this problem live interval for `src1` is extended by 1, thus preventing the same register from being allocated for both `src1` and `dst`. This check is done in liveness analysis code.

Here is detailed algorithm (implemented in [`linear_scan.d`](https://github.com/MrSmith33/vox/blob/35ec440d0c9a475cd4add6093d122cd249b03be9/source/be/reg_alloc/linear_scan.d#L891-L918)):

```D
Rewrite
input: r1 = r2 op r3
as
output: r1 = r2
output: r1 = r1 op r3

if r2 != r3
{
    if r1 == r2 { // input: r1 = r1 op r3
        output: r1 = r1 op r3
    } else if r1 == r3 { // input: "r1 = r2 op r1"
        if (op.isCommutative) { // input: "r1 = r3 op r2"
            output: "r1 = r1 op r2"
        } else { // input: "r1 = r2 op r1"
            // error, we cannot swap r2 with r1 here to get r2 = r1 op r2
            // because r2 will be overwritten. But r2 may be used later
            // this case is handled inside liveness analysis pass by extending
            // the live range of r3 by 1. This way register allocator will never
            // allocate r1 to be the same as r3
        }
    } else { // input: "r1 = r2 op r3"
        output: "mov r1 r2"
        output: "r1 = op r1 r3"
    }
}
else // input: "r1 = op r2 r2"
{
    output: "mov r1 r2" if r1 != r2
    output: "r1 = op r1 r1"
}
```

# Testing

Tests in Vox are defined in the source code via string literals annotated with `TestInfo` value. Test runner then iterates all definitions in test modules and gets those that marked with `@TestInfo`.

Here is what inside `TestInfo`:
```D
struct TestInfo
{
    void function(ref TestContext) tester;
    HostSymbol[] hostSymbols;
    DllModule[] dllModules;
}
```

Here is how the simplest test case looks like:
```D
@TestInfo() // annotation
immutable test = q{}; // string with the source code
```

`immutable test = q{};` is equivalent to `immutable string test = "";`, in D you can omit `string` in this case and `q{}` denotes token literal, which for most cases just makes editor syntax highlight string content.

Inside the test string I use [https://github.com/marler8997/har (HAR)](https://github.com/marler8997/har) format. Basic idea of which is that you can concatenate multiple text files into one, like an archive:

```D
--- main.vx
import kernel32;
void main() { ExitProcess(42); }
--- kernel32.vx
void ExitProcess(u32 uExitCode);
```

each file begins with the header of `--- <path>` format. This way we can define test cases with as many files as we want, in a single string.

Now we can define a test case that defines `test.vx` module, with `get42` function in it:

```D
@TestInfo()
immutable test = q{
--- test.vx
    i32 get42() {
        return 42;
    }
};
```

Since we jit-compile test case we can also run the code we compiled:

```D
@TestInfo(&tester)
immutable test = q{
--- test.vx
    i32 get42() {
        return 42;
    }
};
void tester(ref TestContext ctx) {
    auto run = ctx.getFunctionPtr!(int)("get42"); // get pointer to get42
    assert(run() == 42); // call it
}
```
here we define D function called `tester`, and pass pointer to it to the `@TestInfo` annotation. After test runner compiles the test, it check if `TestInfo.tester` is set and if yes, `tester` is called.

Inside the `tester` we can introspect compiled case. Usually test cases retreive function pointer to some function in the test case and call it. This is possible because Vox uses C calling convention by default, and D can call functions marked with `extern(C)`, which is what `getFunctionPtr` returns.

After we get the function pointer we can call it with any arguments for testing purposes.

But what if we want Vox program to call D code? This is possible too. There are two options:
* bind D function to an external declaration within compiled test case
* or pass function pointer to the Vox program

Example of the first one:
```D
@TestInfo(&tester, [HostSymbol("add", cast(void*)&external_func)])
immutable test = q{
--- test.vx
    i32 run(i32 par) {
        return add(par, 10);
    }
    i32 add(i32, i32); // external declaration
};
extern(C) int external_func(int par1, int par2) {
    return par1 + par2;
}
void tester(ref TestContext ctx) {
    auto run = ctx.getFunctionPtr!(int, int)("run");
    int result = run(13);
    assert(result == 23);
}
```

We declare a D function called `external_func` that sums two ints. `extern(C)` makes it use C calling convention. With the `HostSymbol("add", cast(void*)&external_func)` we define binding from `add` name to the `external_func` function. And when compiler stumbles upon external declaration of `add` it will link it to the provided pointer.

Here is an example of the second option of passing function pointer manually:
```D
@TestInfo(&tester)
immutable test = q{
--- test.vx
    i32 run(i32 par, i32 function(i32, i32) add) {
        return add(par, 10);
    }
};
extern(C) int external_func(int par1, int par2) {
    return par1 + par2;
}
void tester(ref TestContext ctx) {
    auto run = ctx.getFunctionPtr!(int, int, extern(C) int function(int, int))("run");
    int result = run(13, &external_func);
    assert(result == 23);
}
```

To define a test case that must fail the compilation you need to add a special file named `<error>`. Test runner will look for it when setting the compiler. If it is present, the content of this file must match the error message that compiler generates.

```D
@TestInfo()
immutable failing = q{
--- failing.vx
    i32 fun(){
        bar();
    }
--- <error>
failing.vx:2:3: Error: undefined identifier `bar`
};
```

Test runner also supports building executables, which I use to test executable generation. I only have 2 tests that produce an executable, because it is much slower than doing everything in memory.

At the time of writing I have 270 test, all of which take ~60ms. Of which 2 executable tests take ~10ms. This is on Windows. So on average 5ms per executable test, which compiles, writes executable file, runs the executable. And 0.19ms per JIT test, which doesn't perform any IO. Together with compiling compiler from scratch it takes me 3.6s to compile and run compiler with whole test suite. So, iteration time is quite nice.