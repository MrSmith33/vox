/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
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
	this(ushort _opcode, ubyte _numArgs = 0, uint _flags = 0, ubyte _numHiddenArgs = 0) {
		opcode = _opcode;
		numArgs = _numArgs;
		flags = _flags;
		numHiddenArgs = _numHiddenArgs;
	}

	ushort opcode;
	/// If hasVariadicArgs is set, then determines the minimum required number of arguments
	/// Otherwise specifies exact number of arguments
	/// Is copied to IrInstrHeader.numArgs
	ubyte numArgs;
	/// those are allocated after results and arguments
	/// Doesn't affect IrInstrHeader.numArgs
	ubyte numHiddenArgs;
	/// Set of IrInstrFlags
	uint flags;
	static assert(IrInstrFlags.max <= uint.max, "Not enough bits for flags");

	bool isMov() const { return (flags & IFLG.isMov) != 0; }
	bool isJump() const { return (flags & IFLG.isJump) != 0; }
	bool isBranch() const { return (flags & IFLG.isBranch) != 0; }
	bool isBlockExit() const { return (flags & IFLG.isBlockExit) != 0; }
	bool isLoad() const { return (flags & IFLG.isLoad) != 0; }
	bool isStore() const { return (flags & IFLG.isStore) != 0; }
	bool modifiesMemory() const { return (flags & IFLG.modifiesMemory) != 0; }
	bool hasResult() const { return (flags & IFLG.hasResult) != 0; }
	bool hasVariadicArgs() const { return (flags & IFLG.hasVariadicArgs) != 0; }
	bool hasVariadicResult() const { return (flags & IFLG.hasVariadicResult) != 0; }
	bool hasCondition() const { return (flags & IFLG.hasCondition) != 0; }
	bool isResultInDst() const { return (flags & IFLG.isResultInDst) != 0; }
	bool isCommutative() const { return (flags & IFLG.isCommutative) != 0; }
	bool isCall() const { return (flags & IFLG.isCall) != 0; }

	bool mayHaveResult() { return hasResult || hasVariadicResult; }
}

InstrInfo[] gatherInstrInfos(alias instrsEnum)()
{
	InstrInfo[] res = new InstrInfo[__traits(allMembers, instrsEnum).length];
	foreach (i, m; __traits(allMembers, instrsEnum))
	{
		res[i] = __traits(getAttributes, __traits(getMember, instrsEnum, m))[0];
	}
	return res;
}

enum IrInstrFlags : uint {
	none = 0,
	hasResult = 1 << 0,
	isMov = 1 << 1,
	isBranch = 1 << 2,
	isJump = 1 << 3,
	isBlockExit = 1 << 4,
	isLoad = 1 << 5,
	isStore = 1 << 6,
	modifiesMemory = 1 << 7,
	/// If set, InstrInfo.numArgs defines minimaly required number of args
	/// and IrInstrHeader.numArgs is set at runtime
	/// IrBuilder automatically allocates nesessary amount of argument slots after the result slot
	hasVariadicArgs = 1 << 8,
	/// If set IFLG.hasResult flag must not be set
	/// and IrInstrHeader.hasResult is set at runtime
	hasVariadicResult = 1 << 9,
	/// If set IrInstrHeader.cond is used
	hasCondition = 1 << 10,
	/// If set machine instruction requires a = a op b, or a = op a form
	/// while IR instruction has a = b op c, or a = op b form
	isResultInDst = 1 << 11,
	/// If order of arguments doesn't change the result
	isCommutative = 1 << 12,
	isCall = 1 << 13,
	/// For reg allocator
	/// For instructions with single arg no restrictions
	/// For binary instructions - only one of two args or one of result or arg may be memory
	/// If not set, then no args/results can be memory
	/// If only one of non-fixed arguments can be memory operand
	singleMemArg = 1 << 14,
	/// If all non-fixed arguments can be memory operands
	allMemArg = 1 << 15,
}

alias IFLG = IrInstrFlags;

enum getInstrInfo(T) = getUDAs!(T, InstrInfo)[0];
enum getIrValueKind(T) = getUDAs!(T, IrValueKind)[0];

immutable InstrInfo[] irInstrInfos = gatherInstrInfos!IrOpcode;
private alias _ii = InstrInfo;
enum IrOpcode : ushort
{
	// used as placeholder inside generic instructions. Must not remain in IR.
	@_ii(0) invalid,

	@_ii(0, 0, IFLG.isBlockExit) block_exit_jump,
	@_ii(0, 1, IFLG.isBlockExit) block_exit_unary_branch,
	@_ii(0, 2, IFLG.isBlockExit) block_exit_binary_branch,
	@_ii(0, 0, IFLG.isBlockExit) block_exit_return_void,
	@_ii(0, 1, IFLG.isBlockExit) block_exit_return_value,

	@_ii(0, 0, IFLG.none, 1) parameter,
	@_ii(0) call,

	@_ii(0) set_binary_cond,
	@_ii(0) set_unary_cond,

	@_ii(0) store,
	@_ii(0) load,
	@_ii(0) get_element_ptr,
	@_ii(0) load_aggregate,
	@_ii(0) create_aggregate,
	@_ii(0) get_element,
	@_ii(0) insert_element,

	@_ii(0) not, // One's Complement Negation
	@_ii(0) neg, // Two's Complement Negation

	@_ii(0) conv,

	@_ii(0) add,
	@_ii(0) sub,
	@_ii(0) and,
	@_ii(0) or,
	@_ii(0) xor,

	@_ii(0) umul,
	@_ii(0) smul,
	@_ii(0) udiv,
	@_ii(0) sdiv,
	@_ii(0) urem,
	@_ii(0) srem,

	@_ii(0) shl,
	@_ii(0) lshr,
	@_ii(0) ashr,
}

bool hasSideEffects(IrOpcode opcode)
{
	return opcode == IrOpcode.store || opcode == IrOpcode.call;
}

enum IrArgSize : ubyte {
	size8,
	size16,
	size32,
	size64,
}

IrArgSize sizeToIrArgSize(uint typeSize, CompilationContext* context) {
	switch (typeSize) {
		case 1: return IrArgSize.size8;
		case 2: return IrArgSize.size16;
		case 4: return IrArgSize.size32;
		case 8: return IrArgSize.size64;
		default:
			context.internal_error("Type of size %s cannot be stored in a register", typeSize);
			assert(false);
	}
}

IrArgSize typeToIrArgSize(IrIndex type, CompilationContext* context) {
	uint typeSize = context.types.typeSize(type);
	return sizeToIrArgSize(typeSize, context);
}

/// Common prefix of all IR instruction structs
@(IrValueKind.instruction) @InstrInfo()
struct IrInstrHeader
{
	ushort op;
	ubyte numArgs;

	enum MAX_ARGS = 255;

	union {
		mixin(bitfields!(
			bool,       "hasResult", 1,
			ubyte,      "cond",      3,
			// Not always possible to infer arg size from arguments (like in store ptr, imm)
			IrArgSize,  "argSize",   2,
			uint, "",                2
		));

		mixin(bitfields!(
			uint, "",                       1, // hasResult
			// marks instruction that has non-mov instruction between argument movs and itself
			bool, "extendFixedArgRange",    1,
			// marks instruction that has non-mov instruction between result movs and itself
			bool, "extendFixedResultRange", 1,
			uint, "",                       5
		));
	}

	static assert(IrBinaryCondition.max <= 0b111, "3 bits are reserved");
	static assert(IrUnaryCondition.max <= 0b111, "3 bits are reserved");

	// points to first argument (result is immediately before first arg)
	uint _payloadOffset;

	// points to basic block if first instruction of basic block
	IrIndex prevInstr(IrFunction* ir, IrIndex instrIndex) {
		return ir.instrPrevPtr[instrIndex.storageUintIndex];
	}
	// points to basic block if last instruction of basic block
	IrIndex nextInstr(IrFunction* ir, IrIndex instrIndex) {
		return ir.instrNextPtr[instrIndex.storageUintIndex];
	}

	ref IrIndex result(IrFunction* ir) {
		assert(hasResult);
		return ir.instrPayloadPtr[_payloadOffset-1];
	}

	// returns result or undefined
	IrIndex tryGetResult(IrFunction* ir) {
		if (hasResult) return ir.instrPayloadPtr[_payloadOffset-1];
		return IrIndex();
	}

	IrIndex[] args(IrFunction* ir) {
		return ir.instrPayloadPtr[_payloadOffset.._payloadOffset + numArgs];
	}

	ref IrIndex arg(IrFunction* ir, uint index) {
		return ir.instrPayloadPtr[_payloadOffset + index];
	}

	IrIndex[] extraPayload(IrFunction* ir, uint numSlots) {
		uint start = _payloadOffset + numArgs;
		return ir.instrPayloadPtr[start..start+numSlots];
	}
}

template IrGenericInstr(ushort opcode, ubyte numArgs, uint flags = 0, ubyte numExtraArgs = 0)
{
	enum hasResult = (flags & IFLG.hasResult) != 0;

	@(IrValueKind.instruction) @InstrInfo(opcode, numArgs, flags, numExtraArgs)
	struct IrGenericInstr
	{
		IrInstrHeader header;
	}
}

alias IrInstr_return_value = IrGenericInstr!(IrOpcode.block_exit_return_value, 1, IFLG.isBlockExit);
alias IrInstr_return_void = IrGenericInstr!(IrOpcode.block_exit_return_void, 0, IFLG.isBlockExit);
alias IrInstr_store = IrGenericInstr!(IrOpcode.store, 2);
alias IrInstr_load = IrGenericInstr!(IrOpcode.load, 1, IFLG.hasResult);
alias IrInstr_not = IrGenericInstr!(IrOpcode.not, 1, IFLG.hasResult); // one's complement negation
alias IrInstr_neg = IrGenericInstr!(IrOpcode.neg, 1, IFLG.hasResult); // two's complement negation
alias IrInstr_set_binary_cond = IrGenericInstr!(IrOpcode.set_binary_cond, 2, IFLG.hasResult | IFLG.hasCondition);
alias IrInstr_set_unary_cond = IrGenericInstr!(IrOpcode.set_unary_cond, 1, IFLG.hasResult | IFLG.hasCondition);

alias IrInstr_add =  IrGenericInstr!(IrOpcode.add,  2, IFLG.hasResult);
alias IrInstr_sub =  IrGenericInstr!(IrOpcode.sub,  2, IFLG.hasResult);
alias IrInstr_umul =  IrGenericInstr!(IrOpcode.umul,  2, IFLG.hasResult); // unsigned multiply
alias IrInstr_smul = IrGenericInstr!(IrOpcode.smul, 2, IFLG.hasResult); // signed multiply
alias IrInstr_udiv =  IrGenericInstr!(IrOpcode.udiv,  2, IFLG.hasResult); // unsigned division
alias IrInstr_sdiv = IrGenericInstr!(IrOpcode.sdiv, 2, IFLG.hasResult); // signed division
alias IrInstr_urem =  IrGenericInstr!(IrOpcode.urem,  2, IFLG.hasResult); // unsigned remainder
alias IrInstr_srem = IrGenericInstr!(IrOpcode.srem, 2, IFLG.hasResult); // signed remainder
alias IrInstr_shl =  IrGenericInstr!(IrOpcode.shl,  2, IFLG.hasResult);
alias IrInstr_lshr =  IrGenericInstr!(IrOpcode.lshr,  2, IFLG.hasResult);
alias IrInstr_ashr =  IrGenericInstr!(IrOpcode.ashr,  2, IFLG.hasResult);
alias IrInstr_and =  IrGenericInstr!(IrOpcode.and,  2, IFLG.hasResult);
alias IrInstr_or  =  IrGenericInstr!(IrOpcode.or,   2, IFLG.hasResult);
alias IrInstr_xor =  IrGenericInstr!(IrOpcode.xor,  2, IFLG.hasResult);

// used when generating any of above binary instructions. Actual opcode is passed via ExtraInstrArgs.opcode
alias IrInstr_any_binary_instr =  IrGenericInstr!(IrOpcode.invalid,  2, IFLG.hasResult);

alias IrInstr_conv = IrGenericInstr!(IrOpcode.conv, 1, IFLG.hasResult);
alias IrInstr_jump = IrGenericInstr!(IrOpcode.block_exit_jump, 0, IFLG.isBlockExit);
// first argument is function or function pointer
alias IrInstr_call = IrGenericInstr!(IrOpcode.call, 0, IFLG.hasVariadicArgs | IFLG.hasVariadicResult);

/// args: aggregate pointer, 1 or more index
alias IrInstr_get_element_ptr = IrGenericInstr!(IrOpcode.get_element_ptr, 2, IFLG.hasVariadicArgs | IFLG.hasResult);
/// args: aggregate, 1 or more index
alias IrInstr_create_aggregate = IrGenericInstr!(IrOpcode.create_aggregate, 1, IFLG.hasVariadicArgs | IFLG.hasResult);
///
alias IrInstr_load_aggregate = IrGenericInstr!(IrOpcode.load_aggregate, 1, IFLG.hasResult);

/// args: aggregate, 1 or more index
alias IrInstr_get_element = IrGenericInstr!(IrOpcode.get_element, 2, IFLG.hasVariadicArgs | IFLG.hasResult);
/// args: aggregate, new element value, 1 or more index
alias IrInstr_insert_element = IrGenericInstr!(IrOpcode.insert_element, 3, IFLG.hasVariadicArgs | IFLG.hasResult);

enum IrBinaryCondition : ubyte {
	eq,
	ne,
	g,
	ge,
	l,
	le,
}

string[] binaryCondStrings = cast(string[IrBinaryCondition.max+1])["==", "!=", ">", ">=", "<", "<="];
string[] binaryCondStringsEscapedForDot = cast(string[IrBinaryCondition.max+1])[`==`, `!=`, `\>`, `\>=`, `\<`, `\<=`];

bool evalBinCondition(ref CompilationContext context, IrBinaryCondition cond, IrIndex conLeft, IrIndex conRight)
{
	IrConstant left = context.constants.get(conLeft);
	IrConstant right = context.constants.get(conRight);
	final switch(cond) with(IrBinaryCondition)
	{
		case eq: return left.i64 == right.i64;
		case ne: return left.i64 != right.i64;
		case g:  return left.i64 >  right.i64;
		case ge: return left.i64 >= right.i64;
		case l:  return left.i64 <  right.i64;
		case le: return left.i64 <= right.i64;
	}
}

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
alias IrInstr_binary_branch = IrGenericInstr!(IrOpcode.block_exit_binary_branch, 2, IFLG.hasCondition | IFLG.isBlockExit);

enum IrUnaryCondition : ubyte {
	zero,
	not_zero
}
string[] unaryCondStrings = cast(string[IrUnaryCondition.max+1])["", "not"];

IrUnaryCondition invertUnaryCond(IrUnaryCondition cond)
{
	final switch(cond) with(IrUnaryCondition)
	{
		case zero: return not_zero;
		case not_zero: return zero;
	}
}

/// Uses header.cond
alias IrInstr_unary_branch = IrGenericInstr!(IrOpcode.block_exit_unary_branch, 1, IFLG.hasCondition | IFLG.isBlockExit);

@(IrValueKind.instruction) @InstrInfo(IrOpcode.parameter, 0, IFLG.hasResult, 1)
struct IrInstr_parameter
{
	IrInstrHeader header;
	ref uint index(IrFunction* ir) {
		return header.extraPayload(ir, 1)[0].asUint;
	}
}
