/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module ir.ir_instructions;

import std.traits : getUDAs, Parameters;
import std.bitmanip : bitfields;

import all;
import ir.ir_index;


enum HasResult : bool { no, yes }

struct InstrInfo
{
	ushort opcode;
	uint numArgs;
	HasResult hasResult;
}

enum getInstrInfo(T) = getUDAs!(T, InstrInfo)[0];
enum getIrValueKind(T) = getUDAs!(T, IrValueKind)[0];


///
@(IrValueKind.phi)
struct IrPhiInstr
{
	IrIndex blockIndex;
	IrIndex result;
	IrIndex nextPhi;
	IrIndex prevPhi;
	IrIndex firstArgListItem;

	PhiArgIterator args(ref IrFunction ir) { return PhiArgIterator(&ir, firstArgListItem); }
}

struct PhiArgIterator
{
	IrFunction* ir;
	IrIndex firstArgListItem;
	int opApply(scope int delegate(size_t, ref IrPhiArg) dg) {
		IrIndex next = firstArgListItem;
		size_t i = 0;
		while (next.isDefined)
		{
			IrPhiArg* arg = &ir.get!IrPhiArg(next);
			if (int res = dg(i, *arg))
				return res;
			++i;
			next = arg.nextListItem;
		}
		return 0;
	}
}

///
@(IrValueKind.listItem)
struct IrPhiArg
{
	IrIndex value;
	/// Immediate predecessor that provides the value
	IrIndex basicBlock;
	IrIndex nextListItem;
}

/// Per Basic Block info for unresolved Phi functions, when CFG is incomplete.
/// Finished IR contains no such values
@(IrValueKind.listItem)
struct IrIncompletePhi
{
	IrVar var;
	IrIndex phi;
	IrIndex nextListItem;
}

enum IrOpcode : ushort
{
	// used as placeholder inside generic instructions. Must not remain in IR.
	invalid,

	block_exit_jump,
	block_exit_unary_branch,
	block_exit_binary_branch,
	block_exit_return_void,
	block_exit_return_value,

	parameter,

	set_binary_cond,
	set_unary_cond,

	store,
	load,

	add,
	sub
}

@(IrValueKind.virtualRegister)
struct IrVirtualRegister
{
	/// Index of instruction that defines this register
	IrIndex definition;
	/// List of instruction indicies that use this register
	SmallVector users;
	IrIndex prevVirtReg; /// null only if this is firstVirtualReg
	IrIndex nextVirtReg; /// null only if this is lastVirtualReg
	/// Sequential index for random access
	uint seqIndex;
}

/// Generates temporary array of all virtual register indicies
IrIndex[] virtualRegArray(CompilationContext* context, IrFunction* ir)
{
	IrIndex[] result = cast(IrIndex[])context.tempBuffer.voidPut(ir.numVirtualRegisters);
	for (IrIndex vreg = ir.firstVirtualReg; vreg.isDefined; vreg = ir.getVirtReg(vreg).nextVirtReg)
	{
		result[ir.getVirtReg(vreg).seqIndex] = vreg;
	}
	return result;
}

/// Must end with one of block_exit_... instructions
@(IrValueKind.basicBlock)
struct IrBasicBlockInstr
{
	IrIndex firstInstr; // null or first instruction
	IrIndex lastInstr; // null or last instruction
	IrIndex prevBlock; // null only if this is entryBasicBlock
	IrIndex nextBlock; // null only if this is exitBasicBlock
	IrIndex firstPhi; // may be null

	PhiIterator phis(ref IrFunction ir) { return PhiIterator(&ir, &this); }
	InstrIterator instructions(ref IrFunction ir) { return InstrIterator(&ir, &this); }
	InstrReverseIterator instructionsReverse(ref IrFunction ir) { return InstrReverseIterator(&ir, &this); }

	SmallVector predecessors;
	SmallVector successors;

	uint seqIndex;

	mixin(bitfields!(
		/// True if all predecessors was added
		bool, "isSealed",   1,
		/// True if block_exit instruction is in place
		bool, "isFinished", 1,
		uint, "",           6
	));

	IrName name;
}
//pragma(msg, "BB size: ", cast(int)IrBasicBlockInstr.sizeof, " bytes");

struct PhiIterator
{
	IrFunction* ir;
	IrBasicBlockInstr* block;
	int opApply(scope int delegate(IrIndex, ref IrPhiInstr) dg) {
		IrIndex next = block.firstPhi;
		while (next.isDefined)
		{
			IrPhiInstr* phi = &ir.get!IrPhiInstr(next);
			if (int res = dg(next, *phi))
				return res;
			next = phi.nextPhi;
		}
		return 0;
	}
}

struct InstrIterator
{
	IrFunction* ir;
	IrBasicBlockInstr* block;
	int opApply(scope int delegate(IrIndex, ref IrInstrHeader) dg) {
		IrIndex next = block.firstInstr;
		while (next.isDefined)
		{
			IrInstrHeader* header = &ir.get!IrInstrHeader(next);
			if (int res = dg(next, *header))
				return res;
			next = header.nextInstr;
		}
		return 0;
	}
}

struct InstrReverseIterator
{
	IrFunction* ir;
	IrBasicBlockInstr* block;
	int opApply(scope int delegate(IrIndex, ref IrInstrHeader) dg) {
		IrIndex prev = block.lastInstr;
		while (prev.isDefined)
		{
			IrInstrHeader* header = &ir.get!IrInstrHeader(prev);
			if (int res = dg(prev, *header))
				return res;
			prev = header.prevInstr;
		}
		return 0;
	}
}

/// Common prefix of all IR instruction structs
@(IrValueKind.instruction) @InstrInfo()
struct IrInstrHeader
{
	ushort op;
	ubyte numArgs;

	mixin(bitfields!(
		HasResult,  "hasResult", 1,
		ubyte,      "cond",      4,
		uint, "",                3
	));

	static assert(IrBinaryCondition.max <= 0b1111, "4 bits are reserved");
	static assert(IrUnaryCondition.max <= 0b1111, "4 bits are reserved");

	IrIndex prevInstr;
	IrIndex nextInstr;

	IrIndex[0] _payload;

	ref IrIndex result() {
		assert(hasResult);
		return _payload.ptr[0];
	}

	IrIndex[] args() {
		return _payload.ptr[cast(size_t)hasResult..cast(size_t)hasResult+numArgs];
	}
}

template IrGenericInstr(ushort opcode, uint numArgs, HasResult hasResult)
{
	@(IrValueKind.instruction) @InstrInfo(opcode, numArgs, hasResult)
	struct IrGenericInstr
	{
		IrInstrHeader header;
		static if (hasResult)   IrIndex result;
		static if (numArgs > 0) IrIndex[numArgs] args;

		/// takes result + all arguments
		void initialize(IrIndex[hasResult + numArgs] payload ...)
		{
			header.op = opcode;
			header.numArgs = numArgs;
			header.hasResult = hasResult;
			header._payload.ptr[0..hasResult + numArgs] = payload;
		}
	}
}

alias IrReturnValueInstr = IrGenericInstr!(IrOpcode.block_exit_return_value, 1, HasResult.no);
alias IrReturnVoidInstr = IrGenericInstr!(IrOpcode.block_exit_return_void, 0, HasResult.no);
alias IrStoreInstr = IrGenericInstr!(IrOpcode.store, 1, HasResult.no);
alias IrLoadInstr = IrGenericInstr!(IrOpcode.load, 1, HasResult.yes);
alias IrSetBinaryCondInstr = IrGenericInstr!(IrOpcode.set_binary_cond, 2, HasResult.yes);
alias IrBinaryExprInstr(ushort opcode) = IrGenericInstr!(opcode, 2, HasResult.yes);
alias IrInstrJump = IrGenericInstr!(IrOpcode.block_exit_jump, 0, HasResult.no);

enum IrBinaryCondition : ubyte {
	eq,
	ne,
	g,
	ge,
	l,
	le,
}

string[] binaryCondStrings = cast(string[IrBinaryCondition.max+1])["==", "!=", ">", ">=", "<", "<="];

IrBinaryCondition invertBinaryCond(IrBinaryCondition cond)
{
	final switch(cond) with(IrBinaryCondition)
	{
		case eq: return ne;
		case ne: return eq;
		case g:  return le;
		case ge: return l;
		case l:  return ge;
		case le: return g;
	}
}

/// Uses header.cond
alias IrInstrBinaryBranch = IrGenericInstr!(IrOpcode.block_exit_binary_branch, 2, HasResult.no);

enum IrUnaryCondition : ubyte {
	zero,
	not_zero
}
string[] unaryCondStrings = cast(string[IrUnaryCondition.max+1])["", "!"];

IrUnaryCondition invertUnaryCond(IrUnaryCondition cond)
{
	final switch(cond) with(IrUnaryCondition)
	{
		case zero: return not_zero;
		case not_zero: return zero;
	}
}

/// Uses header.cond
alias IrInstrUnaryBranch = IrGenericInstr!(IrOpcode.block_exit_unary_branch, 1, HasResult.no);

@(IrValueKind.instruction) @InstrInfo(IrOpcode.parameter, 0, HasResult.yes)
struct IrInstrParameter
{
	IrInstrHeader header;
	IrIndex result;
	uint index;
}
