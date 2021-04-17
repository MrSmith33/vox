## Memory management

Memory is allocated via a set of arenas. At the start compiler reserves a big chunk of memory (186 GiB atm), and bits of that memory are given to each arena.

On windows memory is reserved via a call to `VirtualAlloc` with `MEM_RESERVE` flag. Then each arena is responsible to commit memory on demand with `VirtualAlloc + MEM_COMMIT` call. I stopped on 64k commit size as higher sizes do not improve time too much.

On linux I wasn't able to archive pure reserve, so compiler relies on overcommit to be enabled and commits the whole arena pool at the start. The OS is then responsible for allocating memory pages on first access.

## Passes

### Read source

Files use 2 arenas: one for file info records (`SourceFileInfo`) and one for file contents.

Source files are read sequentially into an arena.

Before each source there is a start of input char `char SOI_CHAR = '\2'`, and after last source char there is end of input char `char EOI_CHAR = '\3'`.

During lexing those turn into corresponding tokens: `TokenType.SOI`, `TokenType.EOI`.

### Lexing

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

### Parsing

Parsing uses recursive descend + Pratt parsing for expressions. Types are considered an expression too.

AST nodes are allocated in `Arena!uint astBuffer` with 4-byte allocation/addressing granularity. AST nodes use 32-bit indicies to refer to each other. This way arena has capacity for up to 16 GiB of nodes.

For the times where AST nodes need to allocate arays `ArrayArena` is used, which has multiple pools for fixed sizes.

All identifiers are interned and 32-bit identifier index is used everywhere (`struct Identifier`).

All AST node allocations are sequential in memory, so template system takes advantage of that. Templates store the range of AST arena slots that are within the template. When new template instance is created, a copy of that slot range is made at the end of the arena. Then a tree walk fixes indicies and duplicates the arrays.

Named declaration nodes (`NameUseExprNode`/`AliasDeclNode`/`EnumDeclaration`/`EnumMemberDecl`/`FunctionDeclNode`/`StructDeclNode`/`TemplateDeclNode`/`VariableDeclNode`) keep track of parent scope, so that in `Register names` pass they can register themselves into that scope.

### Semantic passes
#### Register names

This pass walks the AST nodes in all files and registers itself in the parent scope.

When we have a single node to register it just registers itself and requests name registering from nested nodes.

Because Vox supports conditional compilation in the form of `#if` and `#foreach`, name registering pass happens in two steps for arrays of nodes (for example struct members, or block statements):

First we request self registration of nodes. This process yields a linked list of all nodes implementing conditional compilation. Then we walk the conditional nodes and expand them in-place. Right after insertion we recursively repeat the first step.

At the end we have all conditional nodes deleted and/or replaces with some of their children. All of the nodes in the sequence were self registered, and no nested nodes were registered yet.

Then we walk all the nodes in array and request registering of their children.

One non-ordinary thing here is that `#if` condition or `#foreach` expression need to be evaluated, which means that condition subtree needs to be processed up to `Type check` stage and evaluated.

#### Lookup names



#### Type check
### IR generation
### IR Optimization
### IR lowering
### IR to LIR translation
    aka instruction selection
### Liveness analysis
### Register allocation
### Stack layout
### Code generation
### Linking
### Writing executable

## IR

Program IR consists of globals, constants, types and functions. Functions belong to modules, but modules are pretty much unnessesary abstraction.

### Storage

Function IR is stored in a set of arenas.

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

Each IR function then points to a slice of items within each of those arenas. (see IrFunction struct)

```D
IrInstrHeader*     instrPtr;
IrIndex*           instrPayloadPtr;
IrIndex*           instrNextPtr;
IrIndex*           instrPrevPtr;
IrPhi*             phiPtr;
uint*              arrayPtr;
IrVirtualRegister* vregPtr;
IrBasicBlock*      basicBlockPtr;
StackSlot*         stackSlotPtr;
```

The way allocation works is that IR of the function which is currently being created must be at the end of each arena. This way adding new items to any arena if O(1) with no reallocation.

This imposes a restriction that only one function IR can be created / modified at a time. Modification that needs to add new items requires function copy to be created at the end of the arenas.

Copying function IR requires only one memcopy per arena without the need of any fixups, since all references are done relative to the start of respective item slice.

This also makes inlining fast. Function IR that is being inlined is appended to the end of the arena and all references in those items are offset by the size of the original IR. Such offset is done via an offset table thanks to clever design of `IrIndex`.

### IrIndex

`IrIndex` represents index of any IR entity.

### Basic blocks

Atm it is 40 bytes

### Function inlining



## Function arguments and register allocation

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

## Handling two-address form instructions

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
