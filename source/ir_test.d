module ir_test;

import std.bitmanip : bitfields;
import std.format : formattedWrite;
import std.stdio;

// 'Simple and Efficient Construction of Static Single Assignment Form'
// When local value numbering for one block is finished, we call this block FILLED
// Basic block is SEALED if no further predecessors will be added to the block
// As only filled blocks may have successors, predecessors are always filled
// Sealed block is not necessarily filled


void main()
{
	//example1;
	example2;
}

void example1()
{
	IrFunction fun = createFunction();
	// 'Simple and Efficient Construction of Static Single Assignment Form' example 1
	// 0: a <- 42;
	// 1: b <- a;
	// 2: c <- a + b;
	// 3: a <- c + 23;
	// 4: c <- a + d;

	// This comes from AST
	Var var_a = Var(VarId(0), IrValueType.i32); // a
	Var var_b = Var(VarId(1), IrValueType.i32); // b
	Var var_c = Var(VarId(2), IrValueType.i32); // c
	Var var_d = Var(VarId(3), IrValueType.i32); // d

	// This is a pass converting AST to IR
	BlockId startBlock = fun.addBlock();

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

void example2()
{
	IrFunction fun = createFunction();
	// 'Simple and Efficient Construction of Static Single Assignment Form' example 2
	// x = 42
	// while(3 < 4)
	// {
	//   if (4 > 3) {
	//     x = 84
	//   }
	//   else
	//   {}
	// }
	// y = x + 1;

	Var var_x = Var(VarId(0), IrValueType.i32); // x
	Var var_y = Var(VarId(1), IrValueType.i32); // y

	// x = 42
	BlockId startBlock = fun.addBlock();
	IrRef op0_42 = fun.put(42);
	fun.writeVariable(var_x, startBlock, op0_42);
	fun.sealBlock(startBlock);

	BlockId whileHeader = fun.addBlock(startBlock);
	// i < 4
	IrRef op2_3 = fun.put(3);
	IrRef op2_4 = fun.put(4);
	IrRef op2 = fun.put(makeInstrCmp(IrCond.l, op2_3, op2_4));
	// while()
	IrRef op_while_branch = fun.put(makeBranch(op2));

	BlockId whileBodyStart = fun.addBlock(whileHeader);
	fun.branchArg1(op_while_branch, whileBodyStart);
	// if (i > 3)
	IrRef op3_4 = fun.put(4);
	IrRef op3_3 = fun.put(3);
	IrRef op3 = fun.put(makeInstrCmp(IrCond.g, op3_4, op3_3));
	IrRef if_branch = fun.put(makeBranch(op3));
	fun.sealBlock(whileBodyStart);

	BlockId thenBlock = fun.addBlock(whileBodyStart);
	IrRef op4_84 = fun.put(84);
	fun.writeVariable(var_x, thenBlock, op4_84);
	fun.branchArg1(if_branch, thenBlock);
	IrRef while_end_jump_1 = fun.put(makeJump());
	fun.sealBlock(thenBlock);

	BlockId elseBlock = fun.addBlock(whileBodyStart);
	fun.branchArg2(if_branch, elseBlock);
	IrRef while_end_jump_2 = fun.put(makeJump());
	fun.sealBlock(elseBlock);

	BlockId whileBodyEnd = fun.addBlock();
	fun.addBlockPred(whileBodyEnd, thenBlock);
	fun.addBlockPred(whileBodyEnd, elseBlock);
	fun.branchArg1(while_end_jump_1, whileBodyEnd);
	fun.branchArg1(while_end_jump_2, whileBodyEnd);
	IrRef header_jump = fun.put(makeJump());
	fun.branchArg1(header_jump, whileHeader);
	fun.addBlockPred(whileHeader, whileBodyEnd);
	fun.sealBlock(whileBodyEnd);
	fun.sealBlock(whileHeader);

	BlockId afterWhileBlock = fun.addBlock(whileHeader);
	fun.sealBlock(afterWhileBlock);
	fun.branchArg2(op_while_branch, afterWhileBlock);
	IrRef op5_x = fun.readVariable(var_x, afterWhileBlock);
	IrRef op5_1 = fun.put(1);
	IrRef op5 = fun.put(makeInstr2(IrOpcode.o_add, IrValueType.i32, op5_x, op5_1));
	fun.writeVariable(var_y, afterWhileBlock, op5);

	fun.print();
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
	IrRef cmpRef = fun.put(makeInstrCmp(IrCond.ge, numRef, ZERO_I32_REF));
	IrRef brnRef = fun.put(makeInstr0(IrOpcode.o_branch, IrValueType.i1));
	IrRef tmp1Ref = fun.put(makeInstr2(IrOpcode.o_sub, IrValueType.i32, ZERO_I32_REF, ONE_I32_REF));
	fun.instructions[brnRef.index].arg0 = tmp1Ref;
	IrRef res2Ref = fun.put(makeInstr2(IrOpcode.o_assign, IrValueType.i32, resRef, tmp1Ref));
	*/
	fun.print();
}

IrFunction createFunction() {
	IrFunction fun;
	//fun.constants ~= [IrConstant(0), IrConstant(1)];
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
	IrPhi[] phis;
	IrBlock[] blocks;
	IrRef[BlockVarPair] blockVarDef;

	BlockId addBlock(BlockId predecessor) {
		BlockId blockId = addBlock();
		blocks[blockId].preds ~= predecessor;
		return blockId;
	}

	void addBlockPred(BlockId blockId, BlockId predecessor) {
		blocks[blockId].preds ~= predecessor;
	}

	BlockId addBlock() {
		uint index = cast(uint)blocks.length;
		IrInstruction blockInstr;
		blockInstr.op = IrOpcode.o_block;
		blockInstr.arg0.index = index;
		auto irRef = put(blockInstr);
		blocks ~= IrBlock(irRef);
		return BlockId(index);
	}

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
		auto irRef = IrRef(index, IrValueKind.instr, instr.type);
		foreach(IrRef argRef; instr.args[0..numInstrArgs[instr.op]])
			if (argRef.kind == IrValueKind.phi)
				phis[argRef.index].addUser(irRef);
		return irRef;
	}

	void branchArg1(IrRef branchInstrRef, BlockId target1) { instructions[branchInstrRef.index].arg1.index = target1; }
	void branchArg2(IrRef branchInstrRef, BlockId target2) { instructions[branchInstrRef.index].arg2.index = target2; }

	IrRef addPhi(BlockId blockId, IrValueType type) {
		uint index = cast(uint)phis.length;
		auto irRef = IrRef(index, IrValueKind.phi, type);
		phis ~= IrPhi(blockId, type);
		blocks[blockId].phis ~= irRef;
		return irRef;
	}

	void print() {
		foreach(int i, con; constants)
			writefln("  %%const_%s = %s", i, con.i64);
		foreach(int i, instr; instructions) {
			switch(instr.op)
			{
				case IrOpcode.o_branch:
					writefln("  branch %s @%s, @%s", RefPr(&this, instr.arg0), instr.arg1.index, instr.arg2.index);
					break;

				case IrOpcode.o_jump:
					writefln("  jump @%s", instr.arg1.index);
					break;

				case IrOpcode.o_block:
					auto block = &blocks[instr.arg0.index];
					writef("@%s", instr.arg0.index);
					foreach(pred; block.preds)
						writef(" <- @%s", pred);
					writeln;
					foreach(phiRef; block.phis)
					{
						writef("  %s phi.%s(", phiRef.type, phiRef.index);
						foreach(phi_i, arg; phis[phiRef.index].args)
						{
							if (phi_i > 0) write(", ");
							writef("%s", RefPr(&this, arg));
						}
						writeln(")");
					}
					break;

				case IrOpcode.o_icmp:
					writef("  %%%s = %- 3s %- 8s", i, instr.type, instr.op);
					writef(" %s %s, %s", instr.cond, RefPr(&this, instr.arg0), RefPr(&this, instr.arg1));
					writeln;
					break;

				default:
					if (hasInstrReturn[instr.op])
						writef("  %%%s = %- 3s %- 8s", i, instr.type, instr.op);
					else  writef("  %%%s %s", i, instr.op);

					switch (numInstrArgs[instr.op]) {
						case 1: writef(" %s", RefPr(&this, instr.arg0)); break;
						case 2: writef(" %s, %s", RefPr(&this, instr.arg0),
							RefPr(&this, instr.arg1)); break;
						default: break;
					}
					writeln;
					break;
			}
		}
	}

	// Algorithm 1: Implementation of local value numbering
	void writeVariable(Var variable, BlockId blockId, IrRef value)
	{
		blockVarDef[BlockVarPair(blockId, variable.id)] = value;
	}

	// ditto
	IrRef readVariable(Var variable, BlockId blockId)
	{
		if (auto irRef = BlockVarPair(blockId, variable.id) in blockVarDef)
			return *irRef;
		return readVariableRecursive(variable, blockId);
	}

	// Algorithm 2: Implementation of global value numbering
	IrRef readVariableRecursive(Var variable, BlockId blockId)
	{
		IrRef value;
		auto block = &blocks[blockId];
		if (!block.isSealed) {
			// Incomplete CFG
			value = addPhi(blockId, variable.type);
			block.incompletePhis ~= IncompletePhi(variable, value);
		}
		else if (block.preds.length == 1) {
			// Optimize the common case of one predecessor: No phi needed
			value = readVariable(variable, block.preds[0]);
		}
		else
		{
			// Break potential cycles with operandless phi
			value = addPhi(blockId, variable.type);
			writeVariable(variable, blockId, value);
			value = addPhiOperands(variable, value, blockId);
		}
		writeVariable(variable, blockId, value);
		return value;
	}

	// ditto
	IrRef addPhiOperands(Var variable, IrRef phiRef, BlockId blockId)
	{
		// Determine operands from predecessors
		foreach (BlockId pred; blocks[blockId].preds)
		{
			auto val = readVariable(variable, pred);
			// Phi should not be cached before loop, since readVariable can add phi to phis, reallocating the array
			phis[phiRef.index].addArg(val);
		}
		return tryRemoveTrivialPhi(phiRef);
	}

	// Algorithm 3: Detect and recursively remove a trivial φ function
	IrRef tryRemoveTrivialPhi(IrRef phiRef)
	{
		IrRef same = IrRef();
		foreach (arg; phis[phiRef.index].args) {
			if (arg == same || arg == phiRef) {
				continue; // Unique value or self−reference
			}
			if (same != IrRef())
			{
				return phiRef; // The phi merges at least two values: not trivial
			}
			same = arg;
		}
		if (same == IrRef()) {
			//same = new Undef(); // The phi is unreachable or in the start block
		}
		IrRef[] users = phis[phiRef.index].users.dup; // Remember all users except the phi itself
		replaceBy(phis[phiRef.index], phiRef, same); // Reroute all uses of phi to same and remove phi

		// Try to recursively remove all phi users, which might have become trivial
		foreach (use; users)
			if (use.kind == IrValueKind.phi)
				if (use != phiRef)
					tryRemoveTrivialPhi(use);
		return same;
	}

	// ditto
	void replaceBy(ref IrPhi phi, IrRef phiRef, IrRef byWhat)
	{
		foreach (IrRef userRef; phi.users)
		{
			final switch (userRef.kind) {
				case IrValueKind.con: break;
				case IrValueKind.label: break;
				case IrValueKind.instr:
					auto instr = instructions[userRef.index];
					foreach (ref IrRef arg; instr.args[0..numInstrArgs[instr.op]])
						if (arg == phiRef) arg = byWhat;
					break;
				case IrValueKind.phi:
					auto otherPhi = &phis[userRef.index];
					foreach (ref IrRef arg; otherPhi.args)
						if (arg == phiRef) arg = byWhat;
					break;
			}
		}
		blocks[phi.blockId].phis.removeInPlace(phiRef);
		phi = IrPhi();
	}

	// Algorithm 4: Handling incomplete CFGs
	void sealBlock(BlockId blockId)
	{
		foreach (pair; blocks[blockId].incompletePhis)
			addPhiOperands(pair.var, pair.phi, blockId);
		blocks[blockId].isSealed = true;
	}
}

T[] removeInPlace(T)(T[] array, T what)
{
	size_t i = 0;
	size_t length = array.length;
	while(i < length)
	{
		if (array[i] == what)
		{
			array[i] = array[length-1];
			--length;
		}
		++i;
	}
	return assumeSafeAppend(array[0..length]);
}

unittest
{
	assert(removeInPlace([], 1) == []);
	assert(removeInPlace([1], 1) == []);
	assert(removeInPlace([1], 2) == [1]);
	assert(removeInPlace([1, 2], 2) == [1]);
	assert(removeInPlace([2, 1], 2) == [1]);
}

struct IrPhi
{
	BlockId blockId;
	IrValueType type;
	IrRef[] args;
	IrRef[] users;

	void addArg(IrRef arg)
	{
		args ~= arg;
	}

	void addUser(IrRef user)
	{
		users ~= user;
	}
}

struct BlockVarPair
{
	BlockId blockId;
	VarId varId;
}

struct IncompletePhi
{
	Var var;
	IrRef phi;
}

struct IrBlock
{
	IrRef instrRef;
	IncompletePhi[] incompletePhis;
	BlockId[] preds;
	IrRef[] phis;
	mixin(bitfields!(
		bool, "isSealed",  1,
		uint, "",          7
	));
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
					case IrValueType.i1:  sink.formattedWrite("i1  %s",  fun.constants[r.index].i1);  break;
					case IrValueType.i32: sink.formattedWrite("i32 %s", fun.constants[r.index].i32); break;
					case IrValueType.i64: sink.formattedWrite("i64 %s", fun.constants[r.index].i64); break;
				} break;
			case IrValueKind.label: sink.formattedWrite("@%s", r.index); break;
			case IrValueKind.instr: sink.formattedWrite("%s %%%s", r.type, r.index); break;
			case IrValueKind.phi:  sink.formattedWrite("%- 3s phi.%s", r.type, r.index); break;
		}
	}
}

struct IrRef
{
	//enum Null = IrRef.init;
	this(uint idx, IrValueKind k, IrValueType t) {
		index = idx; isDefined = true; kind = k; type = t;
	}
	mixin(bitfields!(
		// instruction/constant index
		uint,        "index",     27,
		bool,        "isDefined",  1,
		IrValueKind, "kind",       2,
		IrValueType, "type",       2
	));
}

struct Var { VarId id; IrValueType type; }
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

IrInstruction makeInstr0(IrOpcode op, IrValueType type) {
	return IrInstruction(op, type); }
IrInstruction makeInstr1(IrOpcode op, IrValueType type, IrRef arg0) {
	auto i = IrInstruction(op, type); i.arg0 = arg0; return i; }
IrInstruction makeInstr2(IrOpcode op, IrValueType type, IrRef arg0, IrRef arg1) {
	auto i = IrInstruction(op, type); i.arg0 = arg0; i.arg1 = arg1; return i; }
IrInstruction makeInstrCmp(IrCond cond, IrRef arg0, IrRef arg1) {
	auto i = IrInstruction(IrOpcode.o_icmp, IrValueType.i1); i.cond = cond; i.arg0 = arg0; i.arg1 = arg1; return i; }
IrInstruction makeBranch(IrRef arg0) {
	auto i = IrInstruction(IrOpcode.o_branch); i.arg0 = arg0; return i; }
IrInstruction makeJump() {
	auto i = IrInstruction(IrOpcode.o_jump); return i; }

struct IrInstruction
{
	IrOpcode op;
	IrValueType type;
	union
	{
		struct
		{
			IrCond cond; // condition of icmp instruction
			ushort numUses;
		}
		IrRef arg2;
	}
	union {
		struct {
			IrRef arg0, arg1;
		}
		IrRef[2] args;
	}
}

enum IrOpcode : ubyte
{
	// zero args
	o_nop,
	o_branch,
	o_jump,
	o_block,

	//one args
	o_not,
	o_assign,

	// two args
	o_icmp,
	o_add,
	o_sub,
	o_mul,
	o_div,

	// multi args
}

enum IrCond : ubyte {
	eq,
	ne,
	l,
	le,
	g,
	ge
}

ubyte[IrOpcode.max+1] numInstrArgs = [0,0,0,0,1,1,2,2,2,2,2];
bool[IrOpcode.max+1] hasInstrReturn = [0,0,0,0,1,1,1,1,1,1,1];

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
	label,
	instr,
	phi
}
