module old_ir;

import std.bitmanip : bitfields;
import std.format : formattedWrite;
import std.string : format;

import compiler1;
import utils;
import ir_to_amd64;

struct IrModule
{
	IrFunction*[] functions;

	ubyte[] codeBuffer;
	ubyte[] code;

	void addFunction(IrFunction* fun)
	{
		functions ~= fun;
	}

	void dump(ref TextSink sink, CompilationContext* context)
	{
		foreach (func; functions) dumpFunction(*func, sink, context);
	}
}

/// Linear SSA IR for function
struct IrFunction
{
	this(this)
	{
		constants = constants.dup();
		instructions = instructions.dup();
		phis = phis.dup();
		foreach (ref phi; phis) phi.args = phi.args.dup;
		operands = operands.dup();
		foreach (ref IrBasicBlock block; basicBlocks.range)
		{
			block.predecessors = block.predecessors.dup;
			block.successors = block.successors.dup;
			block.phis = block.phis.dup;
		}
	}

	/// Ast node
	FunctionDeclNode* node;

	/// Calling convention info. Contains helpers for CC specific parts.
	CallConv* callingConvention;

	/// Function return type. Only valid if function has return value. Check AST node.
	IrValueType returnType;

	LinkedList!(IrBasicBlock, BasicBlockIndex) basicBlocks;

	// All constants. All have unique value.
	IrConstant[] constants;

	/// This array begins with all parameters
	IrInstruction[] instructions;

	/// All phi-functions
	IrPhi[] phis;

	// Dense array of values produced by instructions or phi nodes
	IrOperand[] operands;

	/// Stores list of uses per IrOperand
	MultiLinkedList!IrRef opdUses;

	/// Parameters are saved as first `numParameters` instructions of start basic block.
	/// They immediately follow o_block instruction, and have indicies 1, 2, 3 ...
	uint numParameters;

	/// Returns a slice of all param instructions
	IrInstruction[] parameters(){
		return instructions[1..1+numParameters];
	}

	/// Info from buildIntervals pass
	FunctionLiveIntervals liveIntervals;

	StackLayout stackLayout;

	// can return null (for constants)
	IrOperandId getOperand(IrRef irRef)
	{
		final switch (irRef.kind)
		{
			case IrValueKind.instr: return instructions[irRef.index].result;
			case IrValueKind.con: return IrOperandId();
			case IrValueKind.phi: return phis[irRef.index].result;
		}
	}

	// TODO: store positions in separate array
	uint linearPosition(IrRef irRef) {
		final switch (irRef.kind)
		{
			case IrValueKind.con: assert(false);
			case IrValueKind.instr: return irRef.index;
			case IrValueKind.phi: return cast(uint)(basicBlocks[phis[irRef.index].blockIndex].firstInstr);
		}
	}

	/// Set hint for register allocator
	void setStorageHint(IrRef irRef, StorageHint storageHint) {
		final switch (irRef.kind) {
			case IrValueKind.con: break;
			case IrValueKind.instr:
				IrOperandId opdId = instructions[irRef.index].result;
				liveIntervals.setStorageHint(opdId, storageHint);
				break;
			case IrValueKind.phi:
				auto phi = &phis[irRef.index];
				liveIntervals.setStorageHint(phi.result, storageHint);
				foreach (IrPhiArg arg; phi.args)
					setStorageHint(arg.value, storageHint);
				break;
		}
	}

	void assignSequentialBlockIndices()
	{
		uint index;
		foreach (ref IrBasicBlock block; basicBlocks.range)
		{
			block.seqIndex = index;
			++index;
		}
	}

	/// returns true if block `a` directly precedes block `b` in sequential order
	bool areBlockSequential(ref IrBasicBlock a, ref IrBasicBlock b) {
		return (a.seqIndex + 1) == b.seqIndex;
	}

	/// Returns true if `block` is last in sequential order
	bool isLastBlock(ref IrBasicBlock block) {
		return (block.seqIndex + 1) == basicBlocks.length;
	}
}

struct IrBuilder
{
	/// Must be called before compilation of each function. Allows reusing internal buffers.
	void begin(IrFunction* ir)
	{
		this.ir = ir;
		currentBB = BasicBlockIndex.NULL;
		blockVarDef.clear();
		nextIrVarId = IrVarId(0);
	}

	IrFunction* ir;

	/// Instructions are emitted to this block
	BasicBlockIndex currentBB;
	ref IrBasicBlock start() { return ir.basicBlocks[BasicBlockIndex(ir.basicBlocks.listInfo.first)]; }
	ref IrBasicBlock last() { return ir.basicBlocks[BasicBlockIndex(ir.basicBlocks.listInfo.last)]; }

	/// Stores current definition of variable per block during SSA-form IR construction.
	IrRef[BlockVarPair] blockVarDef;

	IrVarId nextIrVarId;

	/// Allocates new variable id for this function. It should be bound to a variable
	/// and used with writeVariable, readVariable functions
	IrVarId newIrVarId()
	{
		return IrVarId(nextIrVarId++);
	}

	/// Automatically sets `start`, sets last and links blocks together
	/// Sets currentBB to this block
	BasicBlockIndex addBasicBlock(IrName name) {
		finishBlock();

		currentBB = ir.basicBlocks.putBack(IrBasicBlock(name));

		auto newBlock = &ir.basicBlocks[currentBB];
		newBlock.index = currentBB; // save index inside block

		uint instrIndex = cast(uint)ir.instructions.length;
		newBlock.firstInstr = instrIndex;
		newBlock.lastInstr = instrIndex;

		put(IrInstruction(IrOpcode.o_block));
		return currentBB;
	}

	/// Places ending instruction of current Basic Block
	void finishBlock() {
		if (!currentBB.isNull) put(IrInstruction(IrOpcode.o_block_end));
	}

	/// Adds control-flow edge pointing `block` -> `target`.
	void addBlockTarget(BasicBlockIndex block, BasicBlockIndex target) {
		ir.basicBlocks[block].successors ~= target;
		incBlockRefcount(target);
		ir.basicBlocks[target].predecessors ~= block;
	}

	/// Disconnects Basic Block from it from predecessors/successors
	/// Does not remove its instructions/phis
	void removeBasicBlock(BasicBlockIndex blockIndex) {
		// TODO: no predecessors must be left at this point
		ir.basicBlocks.freeNode(blockIndex);
		foreach(BasicBlockIndex target; ir.basicBlocks[blockIndex].successors) decBlockRefcount(target);
		ir.basicBlocks[blockIndex].successors = null;
	}

	void incBlockRefcount(BasicBlockIndex blockIndex) { ++ir.basicBlocks[blockIndex].refCount; }
	void decBlockRefcount(BasicBlockIndex blockIndex) { assert(ir.basicBlocks[blockIndex].refCount); --ir.basicBlocks[blockIndex].refCount; }

	/// Adds a constant to a list of constants of this function.
	/// It tries to find existing constant with the same value and returns it if it does.
	IrRef put(long value) {
		foreach(uint i, con; ir.constants)
			if (con.i64 == value)
				return IrRef(i, IrConstKind.literal, IrValueType.i64);
		uint index = cast(uint)ir.constants.length;
		auto con = IrConstant(value);
		ir.constants ~= con;
		if (con.numSignedBytes <= 2)
			return IrRef(index, IrConstKind.literal, IrValueType.i32);
		else
			return IrRef(index, IrConstKind.literal, IrValueType.i64);
	}

	///
	IrRef emitInstr0(IrOpcode op, IrValueType type) {
		return put(IrInstruction(op, type)); }
	///
	IrRef emitInstr1(IrOpcode op, IrValueType type, IrRef arg0) {
		auto i = IrInstruction(op, type); i.arg0 = arg0; return put(i); }
	///
	IrRef emitInstr2(IrOpcode op, IrValueType type, IrRef arg0, IrRef arg1) {
		auto i = IrInstruction(op, type); i.arg0 = arg0; i.arg1 = arg1; return put(i); }
	///
	IrRef emitInstrCmp(IrCond cond, IrRef arg0, IrRef arg1) {
		auto i = IrInstruction(IrOpcode.o_icmp, IrValueType.i1);
		i.condition = cond; i.arg0 = arg0; i.arg1 = arg1;
		auto irRef = put(i);
		//instructions[irRef.index].result.hint.hintReg = RegisterRef(0, RegClass.flags); // flag
		return irRef;
	}

	/// Append instruction to current block (`currentBB`)
	/// Adds user to instr arguments and creates operand for result.
	IrRef put(IrInstruction instr) {
		uint index = cast(uint)ir.instructions.length;
		ir.basicBlocks[currentBB].lastInstr = index;
		auto irRef = IrRef(index, IrValueKind.instr, instr.type);
		//assert(irRef.isDefined, format("irRef is undefined. i %s, instr %s", index, instr.op));
		if (instr.returnsValue)
		{
			instr.result = makeOperand(irRef);
		}
		ir.instructions ~= instr;

		foreach(IrRef argRef; instr.args)
			addUser(irRef, argRef);

		return irRef;
	}

	/// Puts `user` into a list of users of `used` value
	void addUser(IrRef user, IrRef used)
	{
		//assert(user.isDefined, "user is undefined");
		//assert(used.isDefined, "used is undefined");
		void addOpdUser(IrOperandId opdId)
		{
			assert(!opdId.isNull, format("result of %s is null", RefPr(ir, used)));
			ir.opdUses.putBack(ir.operands[opdId].usesList, user);
			ir.operands[opdId].addUser(user);
		}

		final switch (used.kind)
		{
			case IrValueKind.instr:
				auto opdId = ir.instructions[used.index].result;
				addOpdUser(opdId); break;
			case IrValueKind.con:
				final switch(used.constKind) {
					case IrConstKind.literal: ir.constants[used.constIndex].addUser(); break;
					case IrConstKind.stackSlotId: ir.stackLayout.slots[used.constIndex].addUser(); break;
				}
				break;
			case IrValueKind.phi: auto opdId = ir.phis[used.index].result; addOpdUser(opdId); break;
		}
	}

	// Creates operand for result of phi/instruction
	private IrOperandId makeOperand(IrRef irRef)
	{
		uint index = cast(uint)ir.operands.length;
		ir.operands ~= IrOperand(irRef);
		return IrOperandId(index);
	}

	// ignores null opdId
	private void removeOperand(IrOperandId opdId)
	{
		if (opdId.isNull) return;
		size_t length = ir.operands.length;

		if (opdId.index != length-1)
		{
			ir.operands[opdId] = ir.operands[length-1];
			IrRef irRef = ir.operands[opdId].irRef;
			// Update reference from IrValue instr/phi to IrOperand
			// old .result points to length-1, new .result points to opdId
			final switch (irRef.kind)
			{
				case IrValueKind.instr: ir.instructions[irRef.index].result = opdId; break;
				case IrValueKind.con: assert(false, "constants have no operand");
				case IrValueKind.phi: ir.phis[irRef.index].result = opdId; break;
			}
		}
		ir.operands = assumeSafeAppend(ir.operands[0..length-1]);
	}

	// Adds phi function to specified block
	private IrRef addPhi(BasicBlockIndex blockIndex, IrValueType type) {
		uint index = cast(uint)ir.phis.length;
		IrRef irRef = IrRef(index, IrValueKind.phi, type);
		IrOperandId operand = makeOperand(irRef);
		ir.phis ~= IrPhi(blockIndex, type, operand);
		ir.basicBlocks[blockIndex].phis ~= irRef;
		return irRef;
	}

	// Algorithm 1: Implementation of local value numbering
	/// Redefines `variable` with `value`. Is used for assignment to variable
	void writeVariable(IrVar variable, IrRef value) { writeVariable(variable, currentBB, value); }
	void writeVariable(IrVar variable, BasicBlockIndex blockIndex, IrRef value) {
		//writefln("writeVariable %s:%s <- %s", variable.id, variable.name, RefPr(&this, value));
		blockVarDef[BlockVarPair(blockIndex, variable.id)] = value;
	}

	/// Returns the value that currently defines `variable`
	IrRef readVariable(IrVar variable) { return readVariable(variable, currentBB); }
	IrRef readVariable(IrVar variable, BasicBlockIndex blockIndex)
	{
		//writefln("readVariable %s:%s", variable.id, variable.name);
		if (auto irRef = BlockVarPair(blockIndex, variable.id) in blockVarDef)
			return *irRef;
		return readVariableRecursive(variable, blockIndex);
	}

	// Algorithm 2: Implementation of global value numbering
	private IrRef readVariableRecursive(IrVar variable, BasicBlockIndex blockIndex)
	{
		IrRef value;
		if (!ir.basicBlocks[blockIndex].isSealed) {
			// Incomplete CFG
			value = addPhi(blockIndex, variable.type);
			ir.basicBlocks[blockIndex].incompletePhis ~= IncompletePhi(variable, value);
		}
		else
		{
			BasicBlockIndex[] preds = ir.basicBlocks[blockIndex].predecessors;
			if (preds.length == 1) {
				// Optimize the common case of one predecessor: No phi needed
				value = readVariable(variable, preds[0]);
			}
			else
			{
				// Break potential cycles with operandless phi
				value = addPhi(blockIndex, variable.type);
				writeVariable(variable, blockIndex, value);
				value = addPhiOperands(variable, value, blockIndex);
			}
		}
		writeVariable(variable, blockIndex, value);
		return value;
	}

	// ditto
	private IrRef addPhiOperands(IrVar variable, IrRef phiRef, BasicBlockIndex blockIndex)
	{
		// Determine operands from predecessors
		foreach (BasicBlockIndex predIndex; ir.basicBlocks[blockIndex].predecessors)
		{
			IrRef val = readVariable(variable, predIndex);
			// Phi should not be cached before loop, since readVariable can add phi to phis, reallocating the array
			ir.phis[phiRef.index].addArg(val, predIndex);
			addUser(phiRef, val);
		}
		return tryRemoveTrivialPhi(phiRef);
	}

	// Algorithm 3: Detect and recursively remove a trivial φ function
	private IrRef tryRemoveTrivialPhi(IrRef phiRef)
	{
		IrPhiArg same = IrPhiArg();
		foreach (IrPhiArg arg; ir.phis[phiRef.index].args) {
			if (arg == same || arg.value == phiRef) {
				continue; // Unique value or self−reference
			}
			if (same != IrPhiArg())
			{
				return phiRef; // The phi merges at least two values: not trivial
			}
			same = arg;
		}
		if (same == IrPhiArg()) {
			//same = new Undef(); // The phi is unreachable or in the start block
		}
		// Remember all users except the phi itself
		IrOperandId opdId = ir.phis[phiRef.index].result;
		ListInfo!NodeIndex usesList = ir.operands[opdId].usesList;

		// Reroute all uses of phi to same and remove phi
		replaceBy(usesList.first, phiRef, same);
		ir.basicBlocks[ir.phis[phiRef.index].blockIndex].phis.removeInPlace(phiRef);
		ir.phis[phiRef.index] = IrPhi();

		// Try to recursively remove all phi users, which might have become trivial
		NodeIndex cur = usesList.first;
		while(!cur.isNull)
		{
			auto node = ir.opdUses.nodes[cur];
			IrRef use = node.data;
			if (use.kind == IrValueKind.phi && use != phiRef)
				tryRemoveTrivialPhi(use);
			cur = node.nextIndex;
		}
		removeOperand(opdId);
		return same.value;
	}

	// ditto
	/// Rewrites all users of phi `phiRef` to point to `byWhat` instead.
	private void replaceBy(NodeIndex firstUser, IrRef phiRef, IrPhiArg byWhat)
	{
		for (NodeIndex cur = firstUser; !cur.isNull; cur = ir.opdUses.nodes[cur].nextIndex)
		{
			IrRef userRef = ir.opdUses.nodes[cur].data;
			final switch (userRef.kind) {
				case IrValueKind.con: assert(false);
				case IrValueKind.instr:
					auto instr = ir.instructions[userRef.index];
					foreach (ref IrRef arg; instr.args)
						if (arg == phiRef) arg = byWhat.value;
					break;
				case IrValueKind.phi:
					auto otherPhi = &ir.phis[userRef.index];
					foreach (ref IrPhiArg arg; otherPhi.args)
						if (arg.value == phiRef) arg = byWhat;
					break;
			}
		}
	}

	// Algorithm 4: Handling incomplete CFGs
	/// Basic block is sealed if no further predecessors will be added to the block.
	/// Sealed block is not necessarily filled.
	/// Ignores already sealed blocks.
	void sealBlock(BasicBlockIndex blockIndex)
	{
		IrBasicBlock* bb = &ir.basicBlocks[blockIndex];
		if (bb.isSealed) return;
		foreach (pair; bb.incompletePhis)
			addPhiOperands(pair.var, pair.phi, blockIndex);
		bb.incompletePhis = null;
		bb.isSealed = true;
	}
}

pragma(msg, "func size ", IrFunction.sizeof);

void dumpFunction(ref IrFunction func, ref TextSink sink, CompilationContext* ctx)
{
	sink.putf("function %s $%s (", func.returnType, IrNameProxy(ctx, IrName(func.node.id)));
	//foreach (i, param; func.parameters)
	//{
	//	sink.putf("%s %%%s", param.type, IrNameProxy(ctx, func.variableNames[i]));
	//	if (i+1 < func.parameters.length) sink.put(", ");
	//}
	sink.putln(") {");

	bool printVars = false;
	bool printBlockIns =  true;
	bool printBlockRefs = false;
	bool printBlockInstrRange = false;
	bool printInstrIndex = true;
	bool printUses = false;
	bool printLive = true;

	void printOpdUses(IrOperandId opdId) {
		if (opdId.isNull) return;
		NodeIndex cur = func.operands[opdId].usesList.first;
		if (cur.isNull) return;
		sink.put(" uses [");
		auto nodes = func.opdUses.nodes.data;
		while(!cur.isNull)
		{
			IrRef use = nodes[cur].data;
			sink.putf(" %s", RefPr(&func, use));
			cur = nodes[cur].nextIndex;
		}
		sink.put("]");
	}

	if (printVars)
	{
		sink.putln("constants:");
		foreach(int i, con; func.constants)
			sink.putfln("  %%const_%s = %s", i, con.i64);
		sink.putln;
	}

	foreach (BasicBlockIndex blockIndex, ref IrBasicBlock block; func.basicBlocks.range)
	{
		if (printInstrIndex) sink.put("   |");
		sink.putf("  @%s:%s", IrNameProxy(ctx, block.name), blockIndex);
		if (printBlockRefs) sink.putf(" %s refs", block.refCount);
		if (printBlockInstrRange) sink.putf(" instr[%s..%s]", block.firstInstr, block.lastInstr);
		if (printBlockIns && block.predecessors)
		{
			sink.put(" in[");
			foreach(i, pred; block.predecessors) {
				if (i > 0) sink.put(", ");
				sink.putf("@%s", IrNameProxy(ctx, func.basicBlocks[pred].name));
			}
			sink.put("]");
		}
		sink.putln;
		foreach(phiRef; block.phis)
		{
			if (printInstrIndex) sink.putf("% 3s|", block.firstInstr);
			auto res = func.phis[phiRef.index].result;
			sink.putf("    %%%s = %s phi.%s(", res, phiRef.type, phiRef.index);
			foreach(phi_i, arg; func.phis[phiRef.index].args)
			{
				if (phi_i > 0) sink.put(", ");
				sink.putf("%s @%s", RefPr(&func, arg.value), arg.blockIndex);
			}
			sink.put(")");
			if (printUses) printOpdUses(res);
			sink.putln;
		}

		// skip block_op
		auto instrs = func.instructions[block.firstInstr+1..block.lastInstr+1];
		// print all instructions
		size_t i = block.firstInstr+1;
		size_t numArgInstrs = 0;

		foreach(localIndex, ref instr; instrs)
		{
			if (numArgInstrs > 0)
				ctx.assertf(instr.op == IrOpcode.o_call_arg ||
					instr.op == IrOpcode.o_call, "Malformed IR: Call args followed by %s", instr.op);

			switch(instr.op)
			{
				case IrOpcode.o_block: break;
				case IrOpcode.o_block_end: break;
				case IrOpcode.o_icmp:
					if (printInstrIndex) sink.putf("% 3s|", i);
					sink.putf("    %%%s = %- 3s %- 8s", instr.result, instr.type, instr.op);
					sink.putf(" %s %s, %s", instr.condition, RefPr(&func, instr.arg0), RefPr(&func, instr.arg1));
					if (printUses) printOpdUses(instr.result);
					sink.putln;
					break;

				case IrOpcode.o_call_arg:
					++numArgInstrs;
					goto default;

				default:
					if (printInstrIndex) sink.putf("% 3s|", i);
					if (instr.returnsValue) {
						sink.putf("    %%%s = %- 3s %- 8s", instr.result, instr.type, instr.op);
					}
					else  sink.putf("    %s", instr.op);

					switch (instr.numArgs) {
						case 1: sink.putf(" %s", RefPr(&func, instr.arg0)); break;
						case 2: sink.putf(" %s, %s", RefPr(&func, instr.arg0),
							RefPr(&func, instr.arg1)); break;
						default: break;
					}

					if (instr.op == IrOpcode.o_call)
					{
						auto argsInstrs = instrs[localIndex-numArgInstrs..localIndex];
						sink.put("$");
						sink.put(instr.callee.strId(ctx));
						numArgInstrs = 0;
					}

					if (printUses) printOpdUses(instr.result);

					sink.putln;
					break;
			}
			++i;
		}

		// print jump/return at the end of block
		final switch(block.exit.type) with(IrJump.Type) {
			case none: sink.putln("  // block not sealed"); break;
			case ret0: sink.putln("        return"); break;
			case ret1: sink.putfln("        return %s", RefPr(&func, block.exit.value)); break;
			case jmp:  sink.putfln("        jmp @%s", IrNameProxy(ctx, func.basicBlocks[block.successors[0]].name)); break;
			case branch:
			{
				sink.putfln("        branch %s @%s, @%s",
					RefPr(&func, block.exit.value),
					IrNameProxy(ctx, func.basicBlocks[block.successors[0]].name),
					IrNameProxy(ctx, func.basicBlocks[block.successors[1]].name));
			} break;
		}
	}
	sink.putln("}");

	if (printLive) func.liveIntervals.dump(sink);
}

enum IrValueKind : ubyte
{
	instr,
	con,
	phi
}

enum IrConstKind : ubyte
{
	literal,
	stackSlotId
	// Later add function address, global address, table address
	// array data? (binary blobs)
}

enum IrValueType : ubyte
{
	i1, // bool
	i32,
	i64,
	//f32,
	//f64,

	ptr,
}

struct BlockVarPair
{
	BasicBlockIndex blockId;
	IrVarId varId;
}

struct IrVarId { uint id; alias id this; }
struct IrVar { string name; IrVarId id; IrValueType type; }

struct BasicBlockIndex {
	this(size_t index) { this.index = cast(uint)index; }
	enum BasicBlockIndex NULL = BasicBlockIndex(uint.max);
	uint index = uint.max;
	bool isNull() { return index == NULL; }
	alias index this;
}

// print helper for refs
struct RefPr
{
	IrFunction* fun;
	IrRef r;
	void toString(scope void delegate(const(char)[]) sink) {
		if (!r.isDefined) {
			sink("<undefined>");
			return;
		}
		final switch(r.kind) {
			case IrValueKind.con:
				final switch (r.constKind)
				{
					case IrConstKind.literal:
					final switch (r.type) {
						case IrValueType.i1:  sink.formattedWrite("i1  %s",  fun.constants[r.constIndex].i1);  break;
						case IrValueType.i32: sink.formattedWrite("i32 %s", fun.constants[r.constIndex].i32); break;
						case IrValueType.i64: sink.formattedWrite("i64 %s", fun.constants[r.constIndex].i64); break;
						case IrValueType.ptr: sink.formattedWrite("ptr 0x%x", fun.constants[r.constIndex].i64); break;
					} break;

					case IrConstKind.stackSlotId: sink.formattedWrite("slot(%s)", r.constIndex); break;
				}
				break;
			case IrValueKind.instr:
				auto res = fun.instructions[r.index].result;
				if (res.isNull) sink.formattedWrite("instr.%s", r.index);
				else sink.formattedWrite("%s %%%s", r.type, res);
				break;
			case IrValueKind.phi:  sink.formattedWrite("%s %%%s", r.type, fun.phis[r.index].result); break;
		}
	}
}

struct IrRef
{
	this(uint idx, IrValueKind k, IrValueType t) {
		index = idx; isDefined = true; kind = k; type = t;
	}
	this(StackSlotId idx) {
		constIndex = idx; isDefined = true; constKind = IrConstKind.stackSlotId; kind = IrValueKind.con; type = IrValueType.ptr;
	}
	this(uint idx, IrConstKind c, IrValueType t) {
		constIndex = idx; isDefined = true; constKind = c; kind = IrValueKind.con; type = t;
	}
	union
	{
		mixin(bitfields!(
			uint,        "index",     27, // instruction/phi index
			bool,        "isDefined",  1,
			IrValueKind, "kind",       2,
			IrValueType, "type",       2
		));
		mixin(bitfields!(
			uint,        "constIndex",     25,
			IrConstKind, "constKind",       2, // 2 bits are taken out of `index`
			uint,        "",                5
		));
	}
	static assert(IrConstKind.max <= 3, "2 bits are reserved");
	static assert(IrValueType.max <= 3, "2 bits are reserved");
	static assert(IrValueKind.max <= 3, "2 bits are reserved");
	bool isInstr() { return kind == IrValueKind.instr; }
	bool isCon() { return kind == IrValueKind.con; }
	bool isLiteral() { return kind == IrValueKind.con && constKind == IrConstKind.literal; }
}

/// result/argument of instruction
struct IrOperand
{
	// instruction or phi index
	IrRef irRef;
	/// Indicates number of uses. There can be multiple uses per instruction.
	/// 0 means unused, 1 means temporary, or more
	ushort numUses;
	void addUser(IrRef user) { ++numUses; }

	// info for the list of operand uses
	ListInfo!NodeIndex usesList;
}

/// Id for IrOperand
struct IrOperandId
{
	this(size_t idx) { index = cast(uint)idx; }
	uint index = uint.max;
	alias index this;
	enum NULL = IrOperandId();
	bool isNull() { return index == uint.max; }
}

/// Stores numeric constant data
struct IrConstant
{
	this(long value) {
		this.i64 = value;

		if (cast(byte)(value & 0xFF) == value)
			numSignedBytes = 1;
		else if (cast(short)(value & 0xFFFF) == value)
			numSignedBytes = 2;
		else if (cast(int)(value & 0xFFFF_FFFF) == value)
			numSignedBytes = 4;
		else
			numSignedBytes = 8;

		if (cast(ubyte)(value & 0xFF) == value)
			numUnsignedBytes = 1;
		else if (cast(ushort)(value & 0xFFFF) == value)
			numUnsignedBytes = 2;
		else if (cast(uint)(value & 0xFFFF_FFFF) == value)
			numUnsignedBytes = 4;
		else
			numUnsignedBytes = 8;
	}
	ubyte numSignedBytes;
	ubyte numUnsignedBytes;
	ushort numUses;
	union {
		bool i1;
		byte i8;
		short i16;
		int i32;
		long i64;
	}

	void addUser() { ++numUses; }
}

/// Convenience struct for Id + num suffix
struct IrName
{
	Identifier id;
	uint suffix;
}

/// Helper for printing
struct IrNameProxy
{
	CompilationContext* ctx;
	IrName name;
	void toString(scope void delegate(const(char)[]) sink) {
		if (name.suffix == 0) sink.formattedWrite("%s", ctx.idString(name.id));
		else sink.formattedWrite("%s_%s", ctx.idString(name.id), name.suffix);
	}
}

struct IrBasicBlock
{
	IrName name;

	/// Sequential basic block index (index space without holes)
	uint seqIndex;

	/// Index of basic block in storage (this index space may have holes)
	BasicBlockIndex index;

	/// The jump or return that must be the last instruction is stored in `exit`
	uint firstInstr;

	///
	uint lastInstr;

	/// Address of this Basic Block in generated code
	PC startPC;

	/// BBs that jump to this block
	BasicBlockIndex[] predecessors;

	/// Number of jumps to this block. Starting block allways has additional ref.
	int refCount;

	/// BBs this block jumps or branches to. Null if ends with no jump/branch
	BasicBlockIndex[] successors;

	///
	IrRef lastInstrRef() {
		return IrRef(cast(uint)lastInstr, IrValueKind.instr, IrValueType.init);
	}

	/// Specifies the last instruction of this Basic Block
	/// Jumps use `successors` for targets
	IrJump exit;

	// TODO: move to ir builder
	IncompletePhi[] incompletePhis;
	IrRef[] phis;

	mixin(bitfields!(
		bool, "isSealed",  1,
		uint, "",          7
	));

	bool isFinished() { return exit.type != IrJump.Type.none; }
}
pragma(msg, "BB size ", IrBasicBlock.sizeof);

/// Jumps use information stored in basic block
struct IrJump
{
	enum Type : ubyte
	{
		none, /// Basic Block is not yet complete
		ret0, /// without return value
		ret1, /// with return value
		jmp,  /// unconditional jump
		branch   /// conditional jump
	}

	Type type;
	IrRef value; /// return value reference, or condition for branch

	/// Used for branch instruction fixup
	IrCond condition = IrCond.ne;

	/// Used by backend, for:
	///   - first `branch` instruction
	///   - `jmp` instruction
	///   - `ret0`, `ret1` jump instruction when jumping to the end of function
	PC fixup0;
	// Used by backend, for second target of `branch` instruction
	PC fixup1;
}

/// Per Basic Block info for unresolved Phi functions, when CFG is incomplete.
/// Finished IR contains no such values
struct IncompletePhi
{
	IrVar var;
	IrRef phi;
}


struct IrPhi
{
	BasicBlockIndex blockIndex;
	IrValueType type;
	IrOperandId result;
	IrPhiArg[] args;

	void addArg(IrRef arg, BasicBlockIndex argBlockIndex)
	{
		args ~= IrPhiArg(arg, argBlockIndex);
	}
}

struct IrPhiArg
{
	IrRef value;
	BasicBlockIndex blockIndex;
}

enum IrCond : ubyte {
	eq,
	ne,
	l,
	le,
	g,
	ge
}
IrCond[IrCond.max+1] inverseIrCond = [IrCond.ne,IrCond.eq,IrCond.ge,IrCond.g,IrCond.le,IrCond.l];

///
struct IrInstruction
{
	this(IrOpcode op, IrValueType type = IrValueType.init)
	{
		this.op = op;
		this.type = type;
		numArgs = numInstrArgs[op];
		returnsValue = hasInstrReturn[op];
	}
	IrOpcode op;
	IrValueType type;
	mixin(bitfields!(
		// number of `args` fields used: 0, 1, or 2
		uint,        "numArgs",       2,
		// true if `result` field is used
		bool,        "returnsValue",  1,
		IrCond,      "condition",     3,
		uint,        "",              2
	));
	IrOperandId result;
	union {
		struct {
			IrRef arg0, arg1;
		}
		IrRef[2] _args;
		struct { // parameter data
			uint paramIndex;
			IrRef stackSlot;
		}
		struct { // argument data
			uint _; // arg0
			uint argIndex;
		}
		struct { // call data
			FunctionDeclNode* callee;
		}
	}
	IrRef[] args() { return _args[0..numArgs]; }
}
pragma(msg, IrInstruction.sizeof);

enum IrOpcode : ubyte
{
	o_nop,
	o_param,
	o_block, // place of block's phi nodes
	o_block_end, // place of block exit instruction
	o_not,

	o_icmp,
	o_add,
	o_sub,
	o_mul,
	o_div,

	o_call, // stores return value and callee info
	o_call_arg, // represents function call argument

	o_load, // arg0 - address
	o_store, // arg0 - address, arg1 - value

	o_conv, // IrInstruction.type is target type
}

immutable numInstrArgs =  cast(ubyte[IrOpcode.max+1])[0,0,0,0,1,2,2,2,2,2,0,1,1,2,1];
immutable hasInstrReturn = cast(bool[IrOpcode.max+1])[0,1,0,0,1,1,1,1,1,1,0,0,1,0,1];

immutable irOpcodeNames = cast(string[IrOpcode.max+1])
["nop", "param", "block", "block_end", "not", "icmp", "add", "sub", "mul", "div",
"call", "call_arg", "load", "store", "conv"];

immutable binOpToIrOpcode = cast(IrOpcode[BinOp.max+1])[
	IrOpcode.o_icmp,
	IrOpcode.o_icmp,
	IrOpcode.o_icmp,
	IrOpcode.o_icmp,
	IrOpcode.o_icmp,
	IrOpcode.o_icmp,

	IrOpcode.o_sub,
	IrOpcode.o_add,
	IrOpcode.o_div,
	IrOpcode.o_mul];

//              #     #  #######   #####   #         #####
//              #     #     #        #     #        #     #
//              #     #     #        #     #        #
//              #     #     #        #     #         #####
//              #     #     #        #     #              #
//              #     #     #        #     #        #     #
//               #####      #      #####   ######    #####
// -----------------------------------------------------------------------------
struct NodeIndex {
	this(size_t id) { this.id = cast(uint)id; }
	enum NodeIndex NULL = NodeIndex(uint.max);
	uint id = uint.max;
	bool isNull() { return id == NULL; }
	alias id this;
}

struct ListInfo(IndexT = NodeIndex)
{
	IndexT first;
	IndexT last;
}

struct LinkedList(NodeData, IndexT = NodeIndex)
{
	MultiLinkedList!(NodeData, IndexT) multilist;
	ListInfo!IndexT listInfo;
	size_t length() { return multilist.numAllocatedNodes; }
	multilist.NodeRange range() { return multilist.listRange(listInfo); }
	multilist.NodeRangeReverse rangeReverse() { return multilist.listRangeReverse(listInfo); }
	ref NodeData opIndex(IndexT nodeIndex) { return multilist.nodes[nodeIndex].data; }
	IndexT putFront(NodeData nodeData) { return multilist.putFront(listInfo, nodeData); }
	IndexT putBack(NodeData nodeData) { return multilist.putBack(listInfo, nodeData); }
	IndexT freeNode(IndexT nodeIndex) { return multilist.freeNode(listInfo, nodeIndex); }
}

struct MultiLinkedList(NodeData, IndexT = NodeIndex)
{
	alias ListType = typeof(this);

	Buffer!Node nodes;
	uint numAllocatedNodes;
	// head of free nodes linked list
	IndexT firstFreeNode;

	struct Node
	{
		IndexT prevIndex;
		IndexT nextIndex;
		NodeData data;
	}

	struct NodeRange
	{
		ListType* list;
		IndexT frontIndex;
		int opApply(scope int delegate(ref NodeData) dg)
		{
			while (!frontIndex.isNull)
			{
				if (int res = dg(list.nodes[frontIndex].data))
					return res;
				frontIndex = list.nodes[frontIndex].nextIndex;
			}
			return 0;
		}
		int opApply(scope int delegate(IndexT, ref NodeData) dg)
		{
			while (!frontIndex.isNull)
			{
				if (int res = dg(frontIndex, list.nodes[frontIndex].data))
					return res;
				frontIndex = list.nodes[frontIndex].nextIndex;
			}
			return 0;
		}
	}

	NodeRange listRange(ListInfo!IndexT list) { return NodeRange(&this, list.first); }

	struct NodeRangeReverse
	{
		ListType* list;
		IndexT backIndex;
		int opApply(scope int delegate(ref NodeData) dg)
		{
			while (!backIndex.isNull)
			{
				if (int res = dg(list.nodes[backIndex].data))
					return res;
				backIndex = list.nodes[backIndex].prevIndex;
			}
			return 0;
		}
	}

	NodeRangeReverse listRangeReverse(ListInfo!IndexT list) { return NodeRangeReverse(&this, list.last); }

	IndexT putFront(ref ListInfo!IndexT listInfo, NodeData nodeData)
	{
		IndexT firstIndex = listInfo.first;
		IndexT index = allocNode(Node(IndexT.NULL, IndexT.NULL, nodeData));

		listInfo.first = index;
		if (firstIndex.isNull)
		{
			listInfo.last = index;
		}
		else
		{
			Node* node = &nodes[index];
			node.nextIndex = firstIndex;

			Node* first = &nodes[firstIndex];
			first.prevIndex = index;
		}

		return index;
	}

	IndexT putBack(ref ListInfo!IndexT listInfo, NodeData nodeData)
	{
		IndexT lastIndex = listInfo.last;
		IndexT index = allocNode(Node(IndexT.NULL, IndexT.NULL, nodeData));

		listInfo.last = index;
		if (lastIndex.isNull)
		{
			listInfo.first = index;
		}
		else
		{
			Node* node = &nodes[index];
			node.prevIndex = lastIndex;

			Node* last = &nodes[lastIndex];
			last.nextIndex = index;
		}

		return index;
	}

	private IndexT allocNode(Node node)
	{
		++numAllocatedNodes;
		if (firstFreeNode.isNull)
		{
			IndexT index = IndexT(nodes.length);
			nodes.put(node);
			return index;
		}
		else
		{
			IndexT index = firstFreeNode;
			firstFreeNode = nodes[firstFreeNode].nextIndex;
			nodes[index] = node;
			return index;
		}
	}

	// returns IndexT of the next node
	IndexT freeNode(ref ListInfo!IndexT listInfo, IndexT nodeIndex)
	{
		--numAllocatedNodes;
		Node* node = &nodes[nodeIndex];

		if (nodeIndex == listInfo.first) listInfo.first = node.nextIndex;
		if (nodeIndex == listInfo.last) listInfo.last = node.prevIndex;

		IndexT nextIndex = node.nextIndex;

		if (node.prevIndex != IndexT.NULL)
			nodes[node.prevIndex].nextIndex = node.nextIndex;
		if (node.nextIndex != IndexT.NULL)
			nodes[node.nextIndex].prevIndex = node.prevIndex;

		// add to free list
		nodes[nodeIndex].nextIndex = firstFreeNode;
		firstFreeNode = nodeIndex;

		return nextIndex;
	}

	void print(R)(R lists) {
		import std.stdio;
		//write("nodes: ");
		//writeln(nodes.data);

		{
			write("free:");
			IndexT cur = firstFreeNode;
			while (!cur.isNull)
			{
				writef(" %s", cur);
				cur = nodes[cur].nextIndex;
			}
			writeln;
		}

		size_t i;
		foreach (list; lists) {
			scope(exit) ++i;
			IndexT cur = list.first;

			if (cur.isNull)
			{
				writefln("% 3s: empty", i);
				continue;
			}

			writef("% 3s:", i);

			while (!cur.isNull)
			{
				Node* node = &nodes[cur];
				writef(" (%s)", node.data);
				cur = node.nextIndex;
			}
			writeln;
		}
	}
}
