module regalloc;

import std.bitmanip : bitfields;
import std.format : formattedWrite;
import std.stdio;

void main()
{
	IrFunction fun = createFunction();
	// 'Simple and Efficient Construction of Static Single Assignment Form' example 1
	// 0: a <- 42;
	// 1: b <- a;
	// 2: c <- a + b;
	// 3: a <- c + 23;
	// 4: c <- a + d;

	// This come from AST
	VarId var_a = VarId(0); // a
	VarId var_b = VarId(1); // b
	VarId var_c = VarId(2); // c
	VarId var_d = VarId(3); // d

	// This is a pass converting AST to IR
	BlockId startBlock = BlockId(0);

	// 0
	IrRef op0_42 = fun.put(42);
	fun.writeVariable(var_a, startBlock, op0_42);
	// 1
	IrRef op1_a = fun.readVariable(var_a, startBlock);
	fun.writeVariable(var_b, startBlock, op1_a);
	// 2
	IrRef op2_a = fun.readVariable(var_a, startBlock);
	IrRef op2_b = fun.readVariable(var_b, startBlock);
	IrRef op2 = fun.put(makeInstr2(IrOpcode.o_add, IrValueType.i32, op2_a, op2_b));
	fun.writeVariable(var_c, startBlock, op2);
	// 3
	IrRef op3_c = fun.readVariable(var_c, startBlock);
	IrRef op3_23 = fun.put(23);
	IrRef op3 = fun.put(makeInstr2(IrOpcode.o_add, IrValueType.i32, op3_c, op3_23));
	fun.writeVariable(var_a, startBlock, op3);
	// 4
	IrRef op4_a = fun.readVariable(var_a, startBlock);
	IrRef op4_d = fun.readVariable(var_d, startBlock);
	IrRef op4 = fun.put(makeInstr2(IrOpcode.o_add, IrValueType.i32, op4_a, op4_d));
	fun.writeVariable(var_c, startBlock, op4);
	fun.print();
	// %const_0 = 0
	// %const_1 = 1
	// %const_2 = 42
	// %const_3 = 23
	// %0 = i32 o_add    i32 42, i32 42
	// %1 = i32 o_add    i32 %0, i32 23
	// %2 = i32 o_add    i32 %1, i32 %255 //same as ??
}

void testSign()
{
	IrFunction fun = createFunction();
	/*
	// i32 sign(i32 number)
	// {
	//   i32 result; // = 0;
	//   if (number < 0) result = 0-1;
	//   else if (number > 0) result = 1;
	//   else result = 0;
	//   return result;
	// }
	// This come from AST
	VarId numberVar = VarId(0); // parameter
	VarId resultVar = VarId(0); // variable

	// This is a pass converting AST to IR
	BlockId startBlock = BlockId(0);
	IrRef numRef = fun.put(makeInstr0(IrOpcode.o_param, IrValueType.i32));
	fun.writeVariable(numberVar, startBlock, numRef);
	IrRef resRef = fun.put(makeInstr1(IrOpcode.o_assign, IrValueType.i32, ZERO_I32_REF));
	IrRef cmpRef = fun.put(makeInstrCmp(IrOpcode.o_icmp, Cond.ge, numRef, ZERO_I32_REF));
	IrRef brnRef = fun.put(makeInstr0(IrOpcode.o_branch, IrValueType.i1));
	IrRef tmp1Ref = fun.put(makeInstr2(IrOpcode.o_sub, IrValueType.i32, ZERO_I32_REF, ONE_I32_REF));
	fun.instructions[brnRef.index].arg0 = tmp1Ref;
	IrRef res2Ref = fun.put(makeInstr2(IrOpcode.o_assign, IrValueType.i32, resRef, tmp1Ref));
	*/
	fun.print();
}

IrFunction createFunction() {
	IrFunction fun;
	fun.constants ~= [IrConstant(0), IrConstant(1)];
	return fun;
}

enum ZERO_I1_REF  = IrRef(0, IrValueKind.con, IrValueType.i1);
enum ZERO_I32_REF = IrRef(0, IrValueKind.con, IrValueType.i32);
enum ZERO_I64_REF = IrRef(0, IrValueKind.con, IrValueType.i64);
enum ONE_I1_REF   = IrRef(1, IrValueKind.con, IrValueType.i1);
enum ONE_I32_REF  = IrRef(1, IrValueKind.con, IrValueType.i32);
enum ONE_I64_REF  = IrRef(1, IrValueKind.con, IrValueType.i64);

struct IrFunction
{
	// first constant is always 0/false, second is alway 1/true
	IrConstant[] constants;
	IrInstruction[] instructions;
	IrRef[BlockVarPair] blockVarDef;

	IrRef put(long value) {
		foreach(uint i, con; constants)
			if (con.i64 == value)
				return IrRef(i, IrValueKind.con, IrValueType.i64);
		uint index = cast(uint)constants.length;
		constants ~= IrConstant(value);
		return IrRef(index, IrValueKind.con, IrValueType.i64);
	}

	IrRef put(int value) {
		foreach(uint i, con; constants)
			if (con.i32 == value)
				return IrRef(i, IrValueKind.con, IrValueType.i32);
		uint index = cast(uint)constants.length;
		constants ~= IrConstant(value);
		return IrRef(index, IrValueKind.con, IrValueType.i32);
	}

	IrRef put(IrInstruction instr) {
		uint index = cast(uint)instructions.length;
		instructions ~= instr;
		return IrRef(index, IrValueKind.instr, instr.type);
	}

	void print() {
		foreach(int i, con; constants)
			writefln("%%const_%s = %s", i, con.i64);
		foreach(int i, instr; instructions) {
			if (hasInstrReturn[instr.op]) writef("%%%s = %- 3s %- 8s", i, instr.type, instr.op);
			else  writef("%%%s %s", i, instr.op);
			switch (numInstrArgs[instr.op]) {
				case 1: writef(" %s", RefPr(&this, instr.arg0)); break;
				case 2: writef(" %s, %s", RefPr(&this, instr.arg0), RefPr(&this, instr.arg1)); break;
				default:
					if (instr.op == IrOpcode.o_branch)
						writef(" @%s, @%s", instr.arg0.index, instr.arg1.index);
					break;
			}
			writeln;
		}
	}

	void writeVariable(VarId variable, BlockId block, IrRef value)
	{
		blockVarDef[BlockVarPair(block, variable)] = value;
	}

	IrRef readVariable(VarId variable, BlockId block)
	{
		return blockVarDef.get(BlockVarPair(block, variable), IrRef(255, IrValueKind.instr, IrValueType.i32));
	}
}

struct BlockVarPair
{
	BlockId blockId;
	VarId varId;
}

// print helper for refs
struct RefPr
{
	IrFunction* fun;
	IrRef r;
	void toString(scope void delegate(const(char)[]) sink) {
		final switch(r.kind) {
			case IrValueKind.con:
				final switch(r.type) {
					case IrValueType.i1:  sink.formattedWrite("i1 %s",  fun.constants[r.index].i1);  break;
					case IrValueType.i32: sink.formattedWrite("i32 %s", fun.constants[r.index].i32); break;
					case IrValueType.i64: sink.formattedWrite("i64 %s", fun.constants[r.index].i64); break;
				} break;
			case IrValueKind.instr: sink.formattedWrite("%s %%%s", r.type, r.index); break;
		}
	}
}

struct IrRef
{
	this(uint index, IrValueKind kind, IrValueType type) {
		this.index = index; this.kind = kind; this.type = type;
	}
	mixin(bitfields!(
		// instruction/constant index
		uint,        "index",  29,
		IrValueKind, "kind",    1,
		IrValueType, "type",    2
	));
}

struct VarId { uint id; alias id this; }
struct BlockId { uint id; alias id this; }

union IrConstant
{
	this(int value)  { this.i32 = value; }
	this(long value) { this.i64 = value; }
	bool i1;
	int i32;
	long i64;
}

IrInstruction makeInstr0(IrOpcode op, IrValueType type) { return IrInstruction(op, type); }
IrInstruction makeInstr1(IrOpcode op, IrValueType type, IrRef arg0) { return IrInstruction(op, type, Cond.min, 0, arg0); }
IrInstruction makeInstr2(IrOpcode op, IrValueType type, IrRef arg0, IrRef arg1) { return IrInstruction(op, type, Cond.min, 0, arg0, arg1); }
IrInstruction makeInstrCmp(IrOpcode op, Cond cond, IrRef arg0, IrRef arg1) { return IrInstruction(op, IrValueType.i1, cond, 0, arg0, arg1); }

struct IrInstruction
{
	IrOpcode op;
	IrValueType type;
	Cond cond; // condition of icmp instruction
	ushort numUses;
	IrRef arg0, arg1;
}

enum IrOpcode : ubyte
{
	// zero args
	o_nop,
	o_param,
	o_branch,

	//one args
	o_not,
	o_assign,

	// two args
	o_icmp,
	o_add,
	o_sub,
	o_mul,
	o_div,
}

enum Cond : ubyte {
	eq,
	ne,
	l,
	le,
	g,
	ge
}

ubyte[IrOpcode.max+1] numInstrArgs = [0,0,0,1,1,2,2,2,2,2];
bool[IrOpcode.max+1] hasInstrReturn = [0,0,1,1,1,1,1,1,1,1];

// Type of constant or return type of instruction
enum IrValueType : ubyte
{
	i1,
	i32,
	i64
}

// To mark IR reference
enum IrValueKind : ubyte
{
	con,
	instr
}
