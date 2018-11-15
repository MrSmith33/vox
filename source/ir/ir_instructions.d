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


struct InstrInfo
{
	ushort opcode;
	uint numArgs;
	/// Set of IrInstrFlags
	uint flags;
	static assert(IrInstrFlags.max <= uint.max, "Not enough bits for flags");

	bool isMov() { return (flags & IFLG.isMov) != 0; }
	bool isJump() { return (flags & IFLG.isJump) != 0; }
	bool isBranch() { return (flags & IFLG.isBranch) != 0; }
	bool isLoad() { return (flags & IFLG.isLoad) != 0; }
	bool isStore() { return (flags & IFLG.isStore) != 0; }
	bool modifiesMemory() { return (flags & IFLG.modifiesMemory) != 0; }
	bool hasResult() { return (flags & IFLG.hasResult) != 0; }
	bool hasVariadicArgs() { return (flags & IFLG.hasVariadicArgs) != 0; }
	bool hasVariadicResult() { return (flags & IFLG.hasVariadicResult) != 0; }
	bool hasCondition() { return (flags & IFLG.hasCondition) != 0; }
}

enum IrInstrFlags : uint {
	hasResult = 1 << 0,
	isMov = 1 << 1,
	isBranch = 1 << 2,
	isJump = 1 << 3,
	isLoad = 1 << 4,
	isStore = 1 << 5,
	modifiesMemory = 1 << 6,
	/// If set InstrInfo.numArgs must be zero
	/// and IrInstrHeader.numArgs is set at runtime
	hasVariadicArgs = 1 << 7,
	/// If set IFLG.hasResult flag must not be set
	/// and IrInstrHeader.hasResult is set at runtime
	hasVariadicResult = 1 << 8,
	/// If set IrInstrHeader.cond is used
	hasCondition = 1 << 9,
}

alias IFLG = IrInstrFlags;

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

	conv,

	add,
	sub,
	mul,
}

bool hasSideEffects(IrOpcode opcode)
{
	return opcode == IrOpcode.store;
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
		IrIndex current = block.firstInstr;
		while (current.isDefined)
		{
			IrIndex indexCopy = current;
			IrInstrHeader* header = &ir.get!IrInstrHeader(current);

			// save current before invoking delegate, which can remove current instruction
			current = header.nextInstr;

			if (int res = dg(indexCopy, *header))
				return res;
		}
		return 0;
	}
}

struct InstrReverseIterator
{
	IrFunction* ir;
	IrBasicBlockInstr* block;
	int opApply(scope int delegate(IrIndex, ref IrInstrHeader) dg) {
		IrIndex current = block.lastInstr;
		while (current.isDefined)
		{
			IrIndex indexCopy = current;
			IrInstrHeader* header = &ir.get!IrInstrHeader(current);

			// save current before invoking delegate, which can remove current instruction
			current = header.prevInstr;

			if (int res = dg(indexCopy, *header))
				return res;
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
		bool,       "hasResult", 1,
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

template IrGenericInstr(ushort opcode, uint numArgs, uint flags = 0)
{
	enum hasResult = (flags & IFLG.hasResult) != 0;

	@(IrValueKind.instruction) @InstrInfo(opcode, numArgs, flags)
	struct IrGenericInstr
	{
		IrInstrHeader header;
		static if (hasResult)   IrIndex result;
		static if (numArgs > 0) IrIndex[numArgs] args;
	}
}

alias IrReturnValueInstr = IrGenericInstr!(IrOpcode.block_exit_return_value, 1);
alias IrReturnVoidInstr = IrGenericInstr!(IrOpcode.block_exit_return_void, 0);
alias IrStoreInstr = IrGenericInstr!(IrOpcode.store, 1);
alias IrLoadInstr = IrGenericInstr!(IrOpcode.load, 1, IFLG.hasResult);
alias IrSetBinaryCondInstr = IrGenericInstr!(IrOpcode.set_binary_cond, 2, IFLG.hasResult | IFLG.hasCondition);
alias IrInstr_add = IrGenericInstr!(IrOpcode.add, 2, IFLG.hasResult);
alias IrInstr_sub = IrGenericInstr!(IrOpcode.sub, 2, IFLG.hasResult);
alias IrInstr_mul = IrGenericInstr!(IrOpcode.mul, 2, IFLG.hasResult);
alias IrBinaryExprInstr(ushort opcode) = IrGenericInstr!(opcode, 2, IFLG.hasResult);
alias IrUnaryExprInstr(ushort opcode) = IrGenericInstr!(opcode, 1, IFLG.hasResult);
alias IrInstrJump = IrGenericInstr!(IrOpcode.block_exit_jump, 0);

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
alias IrInstrBinaryBranch = IrGenericInstr!(IrOpcode.block_exit_binary_branch, 2, IFLG.hasCondition);

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
alias IrInstrUnaryBranch = IrGenericInstr!(IrOpcode.block_exit_unary_branch, 1, IFLG.hasCondition);

@(IrValueKind.instruction) @InstrInfo(IrOpcode.parameter, 0, IFLG.hasResult)
struct IrInstrParameter
{
	IrInstrHeader header;
	IrIndex result;
	uint index;
}
