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

enum IrInstructionSet : ubyte
{
	ir,
	lir_amd64
}
immutable string[] instr_set_names = ["IR", "LIR Amd64"];
static assert(instr_set_names.length == IrInstructionSet.max+1);

immutable InstrInfo[][] allInstrInfos = [
	irInstrInfos,
	amd64InstrInfos
];

struct InstrInfo
{
	this(byte _numArgs, uint _flags = 0, ubyte _numHiddenArgs = 0) {
		numArgs = _numArgs;
		flags = _flags;
		numHiddenArgs = _numHiddenArgs;
	}

	/// If hasVariadicArgs is set, then determines the minimum required number of arguments
	/// Otherwise specifies exact number of arguments
	/// Is copied to IrInstrHeader.numArgs
	ubyte numArgs;

	/// those are allocated after results and arguments
	/// Doesn't affect IrInstrHeader.numArgs
	/// If contains non IrIndex data, then must be of type IrValueKind.none. (Only lowest 28 bits can be used)
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
	bool isGeneric() const { return (flags & IFLG.isGeneric) != 0; }

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
	/// If set opcode of instruction is set from ExtraInstrArgs.opcode
	isGeneric = 1 << 16,
}

// shortcut
alias IFLG = IrInstrFlags;

enum getInstrInfo(alias T) = getUDAs!(T, InstrInfo)[0];
enum getIrValueKind(T) = getUDAs!(T, IrValueKind)[0];

// InstrInfo array gathered from IrOpcode enum
immutable InstrInfo[] irInstrInfos = gatherInstrInfos!IrOpcode;

// shortcut
private alias _ii = InstrInfo;

/// List of machine-independent opcodes
enum IrOpcode : ushort
{
	// used as placeholder inside generic instructions. Must not remain in IR.
	@_ii() invalid,

	@_ii(0, IFLG.isBlockExit) jump,
	/// Uses IrUnaryCondition inside IrInstrHeader.cond
	@_ii(1, IFLG.hasCondition | IFLG.isBlockExit) branch_unary,
	/// Uses IrBinaryCondition inside IrInstrHeader.cond
	@_ii(2, IFLG.hasCondition | IFLG.isBlockExit) branch_binary,
	/// Args:
	///   iNN value
	///   _k_ >= 0 integer constants (no duplicated constants allowed)
	///   default basic block
	///   0 or more case blocks
	@_ii(1, IFLG.hasVariadicArgs | IFLG.isBlockExit) branch_switch,
	@_ii(0, IFLG.isBlockExit) ret,
	/// Only for ABI handling
	@_ii(1, IFLG.isBlockExit) ret_val,
	@_ii(0, IFLG.isBlockExit) unreachable,

	/// Emitted by frontend and replaced in lowering pass
	/// Extra argument represents parameter index and stored as plain uint of type IrValueKind.none.
	@_ii(0, IFLG.hasResult, 1) parameter,
	// first argument is function or function pointer
	@_ii(1, IFLG.hasVariadicArgs | IFLG.hasVariadicResult) call,
	// first argument is syscall number
	@_ii(1, IFLG.hasVariadicArgs | IFLG.hasVariadicResult) syscall,
	// Special instruction used during inlining. Should not occur in other places.
	@_ii(0) inline_marker,

	/// Args: iNN a, iNN b
	/// Returns boolean result of binary comparison: a cond b
	@_ii(2, IFLG.hasResult | IFLG.hasCondition) set_binary_cond,
	/// Args: iNN a
	/// Returns boolean result of unary comparison: cond a
	@_ii(1, IFLG.hasResult | IFLG.hasCondition) set_unary_cond,

	/// Only for ABI handling
	/// Args: T
	/// Returns: T
	@_ii(1, IFLG.hasResult) move,
	/// Lowered into load+store sequence
	/// Args: T* dst, T* src
	@_ii(2) copy,
	/// Only for ABI handling
	/// Args: int that is added to stack pointer
	@_ii(1) shrink_stack,
	/// Only for ABI handling
	/// Args: int that is substracted from stack pointer
	@_ii(1) grow_stack,
	/// Only for ABI handling
	/// Args: iNN
	@_ii(1) push,

	/// Args: T*, T
	@_ii(2) store,
	/// Args: T*
	/// Returns: T
	@_ii(1, IFLG.hasResult) load,
	/// Args: aggregate pointer, 1 or more index
	/// Returns: member pointer
	@_ii(2, IFLG.hasVariadicArgs | IFLG.hasResult) get_element_ptr,
	/// Args: aggregate pointer, 1 or more index
	/// Returns: member pointer
	/// Same as get_element_ptr, but first index is hardcoded to be 0.
	/// 0 indixies do not make sense, because then instuction is no op and can replaced with first arg
	/// Indicies are compatible with ones from get_element and insert_element
	@_ii(2, IFLG.hasVariadicArgs | IFLG.hasResult) get_element_ptr_0,
	@_ii(1, IFLG.hasResult) load_aggregate, // TODO: remove. Use load instead
	/// Args: aggregate members
	@_ii(1, IFLG.hasVariadicArgs | IFLG.hasResult) create_aggregate,
	/// Args: aggregate, 1 or more index
	/// Returns: member of aggregate
	/// For now indicies must be constant
	@_ii(2, IFLG.hasVariadicArgs | IFLG.hasResult) get_element,
	/// Gets a register sized slice of aggregate
	/// Args: aggregate, 1 index of 8byte chunk
	@_ii(2, IFLG.hasResult) get_aggregate_slice,
	/// Args: aggregate, new element value, 1 or more index
	/// Returns: aggregate with replaced element
	@_ii(3, IFLG.hasVariadicArgs | IFLG.hasResult) insert_element,

	/// One's complement negation
	/// Args: iNN a
	/// Returns ~a
	@_ii(1, IFLG.hasResult) not,
	/// Two's complement negation
	/// Args: iNN a
	/// Returns -a
	@_ii(1, IFLG.hasResult) neg,

	/// Args: source value, target size is represented with argSize field
	/// Currently is used as bitcast. Need clearer semantics
	@_ii(1, IFLG.hasResult) conv,
	/// Args: iNN a, argSize sets MM. MM must be > NN
	/// Returns iMM
	@_ii(1, IFLG.hasResult) zext,
	/// Args: iNN a, argSize sets MM. MM must be > NN
	/// Returns iMM
	@_ii(1, IFLG.hasResult) sext,
	/// Args: iNN a, argSize sets MM. MM must be < NN
	/// Returns iMM
	@_ii(1, IFLG.hasResult) trunc,

	/// Used when generating any of binary instructions.
	/// Actual opcode is passed via ExtraInstrArgs.opcode
	@_ii(2, IFLG.hasResult | IFLG.isGeneric) generic_binary,
	@_ii(2, IFLG.hasResult) add,
	@_ii(2, IFLG.hasResult) sub,
	@_ii(2, IFLG.hasResult) and,
	@_ii(2, IFLG.hasResult) or,
	@_ii(2, IFLG.hasResult) xor,

	/// Unsigned multiply
	@_ii(2, IFLG.hasResult) umul,
	/// Signed multiply
	@_ii(2, IFLG.hasResult) smul,
	/// Unsigned division
	@_ii(2, IFLG.hasResult) udiv,
	/// Signed division
	@_ii(2, IFLG.hasResult) sdiv,
	/// Unsigned remainder
	@_ii(2, IFLG.hasResult) urem,
	/// Signed remainder
	@_ii(2, IFLG.hasResult) srem,

	@_ii(2, IFLG.hasResult) shl,
	@_ii(2, IFLG.hasResult) lshr,
	@_ii(2, IFLG.hasResult) ashr,

	@_ii(2, IFLG.hasResult) fadd,
	@_ii(2, IFLG.hasResult) fsub,
	@_ii(2, IFLG.hasResult) fmul,
	@_ii(2, IFLG.hasResult) fdiv,
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
	size128,
	size256,
	size512,
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
@(IrValueKind.instruction)
struct IrInstrHeader
{
	ushort op;
	ubyte numArgs;

	enum MAX_ARGS = 255;

	union {
		mixin(bitfields!(
			// Always used
			bool,       "hasResult", 1,
			// Only used when IrInstrFlags.hasCondition is set
			ubyte,      "cond",      4,
			// Not always possible to infer arg size from arguments (like in store ptr, imm)
			IrArgSize,  "argSize",   2,
			// Only used for loads to mark source pointer as uniqely owned by load
			bool, "isUniqueLoad",    1,
		));

		mixin(bitfields!(
			uint, "",                       1, // hasResult
			// marks instruction that has non-mov instruction between argument movs and itself
			bool, "extendFixedArgRange",    1,
			// marks instruction that has non-mov instruction between result movs and itself
			bool, "extendFixedResultRange", 1,
			// Used on call instruction
			bool, "alwaysInline",           1,
			uint, "",                       4
		));
	}

	static assert(IrBinaryCondition.max <= 0b1111, "4 bits are reserved");
	static assert(IrUnaryCondition.max <= 0b1111, "4 bits are reserved");

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
		assert(hasResult, "No result");
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
		assert(index < numArgs, "arg index out of bounds");
		return ir.instrPayloadPtr[_payloadOffset + index];
	}

	IrIndex[] extraPayload(IrFunction* ir, uint numSlots) {
		uint start = _payloadOffset + numArgs;
		return ir.instrPayloadPtr[start..start+numSlots];
	}
}


enum IrBinaryCondition : ubyte {
	eq,
	ne,

	ugt,
	uge,
	ult,
	ule,

	sgt,
	sge,
	slt,
	sle,
}

string[] binaryCondStrings = cast(string[IrBinaryCondition.max+1])["==", "!=", "u>", "u>=", "u<", "u<=", "s>", "s>=", "s<", "s<="];
string[] binaryCondStringsEscapedForDot = cast(string[IrBinaryCondition.max+1])[`==`, `!=`, `u\>`, `u\>=`, `u\<`, `u\<=`, `s\>`, `s\>=`, `s\<`, `s\<=`];

bool evalBinCondition(ref CompilationContext context, IrBinaryCondition cond, IrIndex conLeft, IrIndex conRight)
{
	IrConstant left = context.constants.get(conLeft);
	IrConstant right = context.constants.get(conRight);
	final switch(cond) with(IrBinaryCondition)
	{
		case eq: return left.i64 == right.i64;
		case ne: return left.i64 != right.i64;

		case ugt: return left.u64 >  right.u64;
		case uge: return left.u64 >= right.u64;
		case ult: return left.u64 <  right.u64;
		case ule: return left.u64 <= right.u64;

		case sgt: return left.i64 >  right.i64;
		case sge: return left.i64 >= right.i64;
		case slt: return left.i64 <  right.i64;
		case sle: return left.i64 <= right.i64;
	}
}

IrBinaryCondition invertBinaryCond(IrBinaryCondition cond)
{
	final switch(cond) with(IrBinaryCondition)
	{
		case eq: return ne;
		case ne: return eq;

		case ugt: return ule;
		case uge: return ult;
		case ult: return uge;
		case ule: return ugt;

		case sgt: return sle;
		case sge: return slt;
		case slt: return sge;
		case sle: return sgt;
	}
}

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

@(IrValueKind.instruction)
struct IrInstr_parameter
{
	IrInstrHeader header;
	ref uint index(IrFunction* ir) {
		return header.extraPayload(ir, 1)[0].asUint;
	}
}
