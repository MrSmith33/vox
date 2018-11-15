/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// IR instruction format and metadata
module ir.ir_instructions;

import std.traits : getUDAs;
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

alias IrInstr_return_value = IrGenericInstr!(IrOpcode.block_exit_return_value, 1);
alias IrInstr_return_void = IrGenericInstr!(IrOpcode.block_exit_return_void, 0);
alias IrInstr_store = IrGenericInstr!(IrOpcode.store, 1);
alias IrInstr_load = IrGenericInstr!(IrOpcode.load, 1, IFLG.hasResult);
alias IrInstr_set_binary_cond = IrGenericInstr!(IrOpcode.set_binary_cond, 2, IFLG.hasResult | IFLG.hasCondition);
alias IrInstr_add = IrGenericInstr!(IrOpcode.add, 2, IFLG.hasResult);
alias IrInstr_sub = IrGenericInstr!(IrOpcode.sub, 2, IFLG.hasResult);
alias IrInstr_mul = IrGenericInstr!(IrOpcode.mul, 2, IFLG.hasResult);
alias IrInstr_jump = IrGenericInstr!(IrOpcode.block_exit_jump, 0);

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
alias IrInstr_binary_branch = IrGenericInstr!(IrOpcode.block_exit_binary_branch, 2, IFLG.hasCondition);

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
alias IrInstr_unary_branch = IrGenericInstr!(IrOpcode.block_exit_unary_branch, 1, IFLG.hasCondition);

@(IrValueKind.instruction) @InstrInfo(IrOpcode.parameter, 0, IFLG.hasResult)
struct IrInstr_parameter
{
	IrInstrHeader header;
	IrIndex result;
	uint index;
}
