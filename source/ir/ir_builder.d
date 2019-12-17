/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// IR Builder. IR creation API
module ir.ir_builder;

import std.stdio;
import std.string : format;

import all;
import ir.ir_index;

//version = IrPrint;

struct InstrWithResult
{
	IrIndex instruction;
	IrIndex result;
}

struct ExtraInstrArgs
{
	IrOpcode opcode; // used when `InstrInfo.opcode` is IrOpcode.invalid
	ubyte cond; // used when IrInstrFlags.hasCondition is set
	IrArgSize argSize;

	bool addUsers = true;

	/// Is checked when instruction has variadic result (IrInstrFlags.hasVariadicResult).
	/// If `hasResult` is false, no result is allocated and `result` value is ignored.
	/// If `hasResult` is true, then `result` is checked:
	///    If `result` is defined:
	///       then instrHeader.result is set to its value.
	///       or else a new virtual register is created.
	bool hasResult;

	/// If instruction has variadic result, see 'hasResult' comment.
	/// If instruction always has result, then 'result' is used when defined.
	///    when not defined, new virtual register is created
	/// If instruction always has no result, 'result' value is ignored
	IrIndex result;

	/// When instruction has virtual regiter as result, result.type is set to 'type'
	IrIndex type;
}

// papers:
// 1. Simple and Efficient Construction of Static Single Assignment Form
struct IrBuilder
{
	CompilationContext* context;
	IrFunction* ir;

	// Stores current definition of variable per block during SSA-form IR construction.
	private HashMap!(BlockVarPair, IrIndex, BlockVarPair.init) blockVarDef;

	private uint nextIrVarIndex;

	private IrIndex returnVar;
	private uint numRemovedVregs;

	void free() {
		blockVarDef.free(context.arrayArena);
	}

	private void setPointers(IrFunction* ir, CompilationContext* context)
	{
		this.context = context;
		this.ir = ir;

		ir.instrPtr = context.irStorage.instrHeaderBuffer.nextPtr;
		ir.instrPayloadPtr = context.irStorage.instrPayloadBuffer.nextPtr;
		ir.instrNextPtr = context.irStorage.instrNextBuffer.nextPtr;
		ir.instrPrevPtr = context.irStorage.instrPrevBuffer.nextPtr;
		ir.phiPtr = context.irStorage.phiBuffer.nextPtr;
		ir.arrayPtr = context.irStorage.arrayBuffer.nextPtr;
		ir.vregPtr = context.irStorage.vregBuffer.nextPtr;
		ir.basicBlockPtr = context.irStorage.basicBlockBuffer.nextPtr;
	}

	private void reset() {
		blockVarDef.clear();
		numRemovedVregs = 0;
	}

	/// Must be called before compilation of each function. Allows reusing temp buffers.
	/// Sets up entry and exit basic blocks.
	void begin(IrFunction* ir, CompilationContext* context)
	{
		setPointers(ir, context);
		reset();

		setupEntryExitBlocks();

		IrIndex returnType = context.types.getReturnType(ir.type, *context);
		if (!returnType.isTypeVoid)
		{
			returnVar = newIrVarIndex(returnType);
			IrIndex retValue = readVariable(ir.exitBasicBlock, returnVar);
			emitInstr!(IrOpcode.ret_val)(ir.exitBasicBlock, retValue);
		}
		else
		{
			emitInstr!(IrOpcode.ret)(ir.exitBasicBlock);
		}
		ir.getBlock(ir.exitBasicBlock).isFinished = true;
	}

	/// Must be called before IR to LIR pass
	void beginLir(IrFunction* ir, IrFunction* oldIr, CompilationContext* context)
	{
		setPointers(ir, context);
		reset();
	}

	/// Copies ir data to the end of IR buffer, to allow for modification
	void beginDup(IrFunction* ir, CompilationContext* context) {
		this.context = context;
		this.ir = ir;

		dupIrStorage(ir, context);
		reset();
	}

	/// perfoms GC of removed entities
	void finalizeIr() {
		version(IrPrint) writefln("[IR] finalizeIr removed %s", numRemovedVregs);

		// --------------- GC REMOVED VREGS ---------------
		IrIndex lastUsedReg = ir.lastVirtReg;
		IrIndex firstRemovedReg = ir.firstVirtReg;

		// if zero registers exists, this must not be called
		// if only removed registers exist, then `lastUsedReg` becomes null
		// otherwise it's last used register
		void updateLastUsedReg() {
			if (lastUsedReg.isUndefined) return; // already no used regs left

			while(ir.getVirtReg(lastUsedReg).isRemoved)
			{
				if (lastUsedReg.storageUintIndex == 0) {
					// we reached the start of array of vregs. No used regs can be found anymore
					lastUsedReg = IrIndex();
					return;
				}
				// get prev register
				lastUsedReg.storageUintIndex = lastUsedReg.storageUintIndex - 1;
			}
			// `lastUsedReg` is not removed
		}

		// called once per number of removed regs
		void updateFirstRemovedReg() {
			while(!ir.getVirtReg(firstRemovedReg).isRemoved)
			{
				// get next register
				firstRemovedReg.storageUintIndex = firstRemovedReg.storageUintIndex + 1;
			}
		}

		uint numProcessedVregs = 0;

		// max loop iterations == min(numUsedRegs, numRemovedVregs)
		// Actual time complexity is O(numVirtualRegisters)
		// 0 times in best case, were all removed regs are already at the end
		while (numProcessedVregs < numRemovedVregs)
		{
			updateLastUsedReg;

			// no used regs left
			if (lastUsedReg.isUndefined) break;

			updateFirstRemovedReg;

			// all removed regs are already at the end
			if (firstRemovedReg.storageUintIndex > lastUsedReg.storageUintIndex) break;

			// move last used reg into the place of first removed register
			moveVreg(lastUsedReg, firstRemovedReg);
			// mark as removed for updateLastUsedReg
			ir.getVirtReg(lastUsedReg).type = lastUsedReg;

			++numProcessedVregs;
		}

		// all removed regs were moved to the end of array
		ir.numVirtualRegisters -= numRemovedVregs;
		//writefln("remove %s %s, %s", ir.numVirtualRegisters, context.irStorage.vregBuffer.length, numRemovedVregs);
		context.irStorage.vregBuffer.unput(numRemovedVregs);
	}

	void setupEntryExitBlocks()
	{
		assert(ir.numBasicBlocks == 0);
		// Canonical function CFG has entry block, and single exit block.
		appendBasicBlockSlot; // entry block at index 0
		appendBasicBlockSlot; // exit block at index 1

		ir.getBlock(ir.entryBasicBlock).nextBlock = ir.exitBasicBlock;
		sealBlock(ir.entryBasicBlock);
		ir.getBlock(ir.exitBasicBlock).prevBlock = ir.entryBasicBlock;
	}

	// memory is not initialized
	void appendInstructionSlots(uint numSlots) {
		ir.numInstructions += numSlots;
		context.irStorage.instrHeaderBuffer.voidPut(numSlots);
		context.irStorage.instrNextBuffer.voidPut(numSlots);
		context.irStorage.instrPrevBuffer.voidPut(numSlots);
	}

	// memory is not initialized
	// slots for instruction result and arguments
	void appendPayloadSlots(uint numSlots) {
		ir.numPayloadSlots += numSlots;
		context.irStorage.instrPayloadBuffer.voidPut(numSlots);
	}

	// memory is initialized
	IrIndex appendPhiSlot() {
		IrIndex result = IrIndex(ir.numPhis, IrValueKind.phi);
		ir.numPhis += 1;
		context.irStorage.phiBuffer.put(IrPhi());
		return result;
	}

	// memory is not initialized
	IrIndex appendVirtRegSlot() {
		IrIndex result = IrIndex(ir.numVirtualRegisters, IrValueKind.virtualRegister);
		ir.numVirtualRegisters += 1;
		context.irStorage.vregBuffer.voidPut(1);
		//writefln("add %s %s", ir.numVirtualRegisters, context.irStorage.vregBuffer.length);
		return result;
	}

	// memory is initialized
	IrIndex appendBasicBlockSlot() {
		IrIndex result = IrIndex(ir.numBasicBlocks, IrValueKind.basicBlock);
		ir.numBasicBlocks += 1;
		context.irStorage.basicBlockBuffer.put(IrBasicBlock());
		return result;
	}

	IrIndex appendListItem(T)(uint numItems)
	{
		static assert(T.alignof == 4, "Can only store types aligned to 4 bytes");
		static assert(getIrValueKind!T == IrValueKind.listItem, "Can only store list types");

		IrIndex result = IrIndex(ir.arrayLength, IrValueKind.listItem);

		enum uint allocSize = divCeil(T.sizeof, uint.sizeof);
		uint numSlots = numItems * allocSize;
		context.irStorage.arrayBuffer.voidPut(numSlots);
		ir.get!T(result) = T.init;
		ir.arrayLength += numSlots;

		return result;
	}

	/// Adds control-flow edge pointing `fromBlock` -> `toBlock`.
	void addBlockTarget(IrIndex fromBasicBlockIndex, IrIndex toBasicBlockIndex) {
		ir.getBlock(fromBasicBlockIndex).successors.append(&this, toBasicBlockIndex);
		ir.getBlock(toBasicBlockIndex).predecessors.append(&this, fromBasicBlockIndex);
	}

	/// Creates new block and inserts it after lastBasicBlock and sets lastBasicBlock
	IrIndex addBasicBlock() {
		assert(ir.lastBasicBlock.isDefined);
		IrIndex lastBasicBlock = ir.lastBasicBlock;
		IrIndex newBlock = appendBasicBlockSlot;
		ir.getBlock(newBlock).nextBlock = ir.exitBasicBlock;
		ir.getBlock(newBlock).prevBlock = lastBasicBlock;
		ir.getBlock(ir.exitBasicBlock).prevBlock = newBlock;
		ir.getBlock(lastBasicBlock).nextBlock = newBlock;
		return newBlock;
	}

	/// Does not remove its instructions/phis
	/*void removeBasicBlock(IrIndex basicBlockToRemove) {
		--numBasicBlocks;
		IrBasicBlock* bb = &get!IrBasicBlock(basicBlockToRemove);
		if (bb.prevBlock.isDefined)
			getBlock(bb.prevBlock).nextBlock = bb.nextBlock;
		if (bb.nextBlock.isDefined)
			getBlock(bb.nextBlock).prevBlock = bb.prevBlock;
	}*/

	// Algorithm 4: Handling incomplete CFGs
	/// Basic block is sealed if no further predecessors will be added to the block.
	/// Sealed block is not necessarily filled.
	/// Ignores already sealed blocks.
	void sealBlock(IrIndex basicBlockToSeal) {
		//dumpFunction(context, ir);
		version(IrPrint) writefln("[IR] seal %s", basicBlockToSeal);

		IrBasicBlock* bb = &ir.getBlock(basicBlockToSeal);
		if (bb.isSealed) return;

		// all phis added to this block are incomplete and need to get their arguments
		foreach(IrIndex phiIndex, ref IrPhi phi; bb.phis(ir)) {
			addPhiOperands(basicBlockToSeal, phi.var, phiIndex);
		}

		bb.isSealed = true;
	}

	/// Allocates new variable id for this function. It should be bound to a variable
	/// and used with writeVariable, readVariable functions
	IrIndex newIrVarIndex(IrIndex varType) {
		IrIndex varId = context.appendTemp!IrVariableInfo;
		context.getTemp!IrVariableInfo(varId).type = varType;
		return varId;
	}

	private IrIndex getVarType(IrIndex varId) {
		return context.getTemp!IrVariableInfo(varId).type;
	}

	// Algorithm 1: Implementation of local value numbering
	/// Redefines `variable` with `value`. Is used for assignment to variable
	void writeVariable(IrIndex blockIndex, IrIndex var, IrIndex value) {
		context.assertf(var.kind == IrValueKind.variable, "Variable kind is %s", var.kind);
		context.assertf(
			value.kind == IrValueKind.func ||
			value.kind == IrValueKind.constant ||
			value.kind == IrValueKind.constantAggregate ||
			value.kind == IrValueKind.global ||
			value.kind == IrValueKind.virtualRegister ||
			value.kind == IrValueKind.physicalRegister ||
			value.kind == IrValueKind.stackSlot,
			"writeVariable(block %s, variable %s, value %s)",
			blockIndex, var, value);

		version(IrPrint) writefln("[IR]  blockVarDef[%s %s] <- %s", blockIndex, var, value);
		blockVarDef.put(context.arrayArena, BlockVarPair(blockIndex, var), value);
	}

	/// Returns the value that currently defines `var` within `blockIndex`
	IrIndex readVariable(IrIndex blockIndex, IrIndex var) {
		context.assertf(var.kind == IrValueKind.variable, "Variable kind is %s", var.kind);
		if (auto irRef = BlockVarPair(blockIndex, var) in blockVarDef)
			return *irRef;
		return readVariableRecursive(blockIndex, var);
	}

	/// Puts `user` into a list of users of `used` value
	void addUser(IrIndex user, IrIndex used) {
		//if (!used.isDefined) dumpFunction(*ir, *context);
		context.assertf(user.isDefined && used.isDefined, "%s addUser(%s, %s)",
			context.idString(ir.backendData.name), user, used);
		final switch (used.kind) with(IrValueKind) {
			case none: assert(false, "addUser none");
			case listItem: assert(false, "addUser listItem");
			case instruction: assert(false, "addUser instruction");
			case basicBlock: break; // allowed. As argument of jmp jcc
			case constant: break; // allowed, noop
			case constantAggregate: break;
			case constantZero: break;
			case global:
				context.globals.get(used).addUser(user);
				break;
			case phi: assert(false, "addUser phi"); // must be virt reg instead
			case stackSlot: break; // allowed, noop
			case virtualRegister:
				ir.getVirtReg(used).users.append(&this, user);
				break;
			case physicalRegister: break; // allowed, noop
			case type: break; // allowed, noop (no user tracking)
			case variable: assert(false, "addUser variable");
			case func: break; // allowed, noop (no user tracking)
		}
	}

	/// Returns InstrWithResult (if instr has result) or IrIndex instruction otherwise
	/// Always returns InstrWithResult when instruction has variadic result
	///   in this case result can be null if no result is requested
	/// See: ExtraInstrArgs
	auto emitInstr(alias I)(IrIndex blockIndex, IrIndex[] args ...)
	{
		// TODO assert if I requires ExtraInstrArgs data
		return emitInstr!I(blockIndex, ExtraInstrArgs(), args);
	}

	/// ditto
	auto emitInstr(alias I)(IrIndex blockIndex, ExtraInstrArgs extra, IrIndex[] args ...)
	{
		static if (getInstrInfo!I.mayHaveResult) {
			InstrWithResult result = emitInstr!I(extra, args);
			appendBlockInstr(blockIndex, result.instruction);
			return result;
		} else {
			IrIndex result = emitInstr!I(extra, args);
			appendBlockInstr(blockIndex, result);
			return result;
		}
	}

	/// ditto
	/// Only creates instruction, doesn't add to basic block
	auto emitInstr(alias I)(ExtraInstrArgs extra, IrIndex[] args ...)
	{
		IrIndex instr = IrIndex(ir.numInstructions, IrValueKind.instruction);
		appendInstructionSlots(1);

		IrInstrHeader* instrHeader = &ir.get!IrInstrHeader(instr);
		*instrHeader = IrInstrHeader.init;

		enum iinfo = getInstrInfo!I;

		// opcode
		static if (iinfo.isGeneric)
			instrHeader.op = extra.opcode;
		else
			instrHeader.op = I;

		instrHeader.argSize = extra.argSize;

		// payload offset must points to first argument
		instrHeader._payloadOffset = ir.numPayloadSlots;

		// result
		static if (iinfo.hasVariadicResult) {
			if (extra.hasResult) {
				appendPayloadSlots(1);
				instrHeader.hasResult = true;
			} else {
				instrHeader.hasResult = false;
			}
		} else static if (iinfo.hasResult) {
			appendPayloadSlots(1);
			instrHeader.hasResult = true;
		} else {
			instrHeader.hasResult = false;
		}

		// set result
		static if (iinfo.mayHaveResult)
		{
			if (instrHeader.hasResult)
			{
				// advance pointer to point to arguments
				++instrHeader._payloadOffset;

				if (extra.result.isDefined) {
					instrHeader.result(ir) = extra.result;
					// fix definition
					if (extra.result.isVirtReg) {
						IrVirtualRegister* virtReg = &ir.getVirtReg(extra.result);
						virtReg.definition = instr;
						assert(extra.type.isType, format("Invalid extra.type (%s)", extra.type));
						virtReg.type = extra.type;
					}
				} else {
					instrHeader.result(ir) = addVirtualRegister(instr, extra.type);
				}
			}
		}

		// condition
		static if (iinfo.hasCondition) {
			instrHeader.cond = extra.cond;
		}

		// arguments
		static if (iinfo.hasVariadicArgs)
		{
			context.assertf(args.length <= IrInstrHeader.numArgs.max,
				"Too many arguments (%s), max is %s",
				args.length,
				IrInstrHeader.numArgs.max);

			context.assertf(args.length >= iinfo.numArgs,
				"Instruction %s requires at least %s arguments, while passed %s",
				I.stringof,
				iinfo.numArgs,
				args.length);

			instrHeader.numArgs = cast(typeof(instrHeader.numArgs))args.length;

			// allocate argument slots after optional result
			appendPayloadSlots(cast(uint)args.length);
		}
		else
		{
			context.assertf(iinfo.numArgs == args.length,
				"Instruction %s requires %s args, while passed %s",
				I.stringof, iinfo.numArgs, args.length);

			instrHeader.numArgs = iinfo.numArgs;
			appendPayloadSlots(iinfo.numArgs);
		}

		// allocate hidden args
		appendPayloadSlots(iinfo.numHiddenArgs);

		// set arguments
		instrHeader.args(ir)[] = args;

		// Instruction uses its arguments
		if (extra.addUsers) {
			foreach(IrIndex arg; args) {
				addUser(instr, arg);
			}
		}

		static if (iinfo.mayHaveResult)
		{
			if (instrHeader.hasResult)
				return InstrWithResult(instr, instrHeader.result(ir));
			else
				return InstrWithResult(instr, IrIndex());
		} else {
			return instr;
		}
	}

	/// Adds instruction to the end of basic block
	/// Doesn't set any instruction info except prevInstr, nextInstr index
	void appendBlockInstr(IrIndex blockIndex, IrIndex instr)
	{
		IrBasicBlock* block = &ir.getBlock(blockIndex);

		ir.nextInstr(instr) = blockIndex;

		if (block.firstInstr.isDefined) {
			// points to prev instruction
			ir.prevInstr(instr) = block.lastInstr;
			ir.nextInstr(block.lastInstr) = instr;
			block.lastInstr = instr;
		} else {
			ir.prevInstr(instr) = blockIndex;
			block.firstInstr = instr;
			block.lastInstr = instr;
		}
	}

	/// Adds instruction to the start of basic block
	/// Doesn't set any instruction info except prevInstr, nextInstr index
	void prependBlockInstr(IrIndex blockIndex, IrIndex instr)
	{
		IrBasicBlock* block = &ir.getBlock(blockIndex);

		ir.prevInstr(instr) = blockIndex;

		if (block.lastInstr.isDefined) {
			// points to next instruction
			ir.nextInstr(instr) = block.firstInstr;
			ir.prevInstr(block.firstInstr) = instr;
			block.firstInstr = instr;
		} else {
			ir.nextInstr(instr) = blockIndex;
			block.lastInstr = instr;
			block.firstInstr = instr;
		}
	}

	/// Inserts 'instr' after 'afterInstr'
	void insertAfterInstr(IrIndex afterInstr, IrIndex instr)
	{
		ir.prevInstr(instr) = afterInstr;
		ir.nextInstr(instr) = ir.nextInstr(afterInstr);

		if (ir.nextInstr(afterInstr).isBasicBlock) {
			// 'afterInstr' is the last instr in the block
			ir.getBlock(ir.nextInstr(afterInstr)).lastInstr = instr;
		} else {
			// There must be instr after 'afterInstr'
			ir.prevInstr(ir.nextInstr(afterInstr)) = instr;
		}
		ir.nextInstr(afterInstr) = instr;
	}

	/// Inserts 'instr' before lastInstr of basic block 'blockIndex'
	void insertBeforeLastInstr(IrIndex blockIndex, IrIndex instr)
	{
		IrBasicBlock* block = &ir.getBlock(blockIndex);
		if (block.lastInstr.isDefined) {
			insertBeforeInstr(block.lastInstr, instr);
		} else {
			appendBlockInstr(blockIndex, instr);
		}
	}

	/// Inserts 'instr' before 'beforeInstr'
	void insertBeforeInstr(IrIndex beforeInstr, IrIndex instr)
	{
		IrInstrHeader* beforeInstrHeader = &ir.get!IrInstrHeader(beforeInstr);

		ir.nextInstr(instr) = beforeInstr;
		ir.prevInstr(instr) = ir.prevInstr(beforeInstr);

		if (ir.prevInstr(beforeInstr).isBasicBlock) {
			// 'beforeInstr' is the first instr in the block
			ir.getBlock(ir.prevInstr(beforeInstr)).firstInstr = instr;
		} else {
			// There must be instr before 'beforeInstr'
			ir.nextInstr(ir.prevInstr(beforeInstr)) = instr;
		}

		ir.prevInstr(beforeInstr) = instr;
	}

	IrIndex addBinBranch(IrIndex blockIndex, IrBinaryCondition cond, IrArgSize argSize, IrIndex arg0, IrIndex arg1, ref IrLabel trueExit, ref IrLabel falseExit)
	{
		auto res = addBinBranch(blockIndex, cond, argSize, arg0, arg1);
		forceAllocLabelBlock(trueExit, 1);
		forceAllocLabelBlock(falseExit, 1);
		addBlockTarget(blockIndex, trueExit.blockIndex);
		addBlockTarget(blockIndex, falseExit.blockIndex);
		return res;
	}

	IrIndex addBinBranch(IrIndex blockIndex, IrBinaryCondition cond, IrArgSize argSize, IrIndex arg0, IrIndex arg1)
	{
		IrBasicBlock* block = &ir.getBlock(blockIndex);
		assert(!block.isFinished);
		block.isFinished = true;
		ExtraInstrArgs extra = { cond : cond, argSize : argSize };
		return emitInstr!(IrOpcode.branch_binary)(blockIndex, extra, arg0, arg1);
	}

	IrIndex addUnaryBranch(IrIndex blockIndex, IrUnaryCondition cond, IrArgSize argSize, IrIndex arg0, ref IrLabel trueExit, ref IrLabel falseExit)
	{
		auto res = addUnaryBranch(blockIndex, cond, argSize, arg0);
		forceAllocLabelBlock(trueExit, 1);
		forceAllocLabelBlock(falseExit, 1);
		addBlockTarget(blockIndex, trueExit.blockIndex);
		addBlockTarget(blockIndex, falseExit.blockIndex);
		return res;
	}

	IrIndex addUnaryBranch(IrIndex blockIndex, IrUnaryCondition cond, IrArgSize argSize, IrIndex arg0)
	{
		IrBasicBlock* block = &ir.getBlock(blockIndex);
		assert(!block.isFinished);
		block.isFinished = true;
		ExtraInstrArgs extra = { cond : cond, argSize : argSize };
		return emitInstr!(IrOpcode.branch_unary)(blockIndex, extra, arg0);
	}

	void addReturn(IrIndex blockIndex, IrIndex returnValue)
	{
		context.assertf(returnValue.isDefined, "addReturn %s", returnValue);
		IrIndex returnType = context.types.getReturnType(ir.type, *context);
		context.assertf(!returnType.isTypeVoid, "Trying to return value from void function");
		writeVariable(blockIndex, returnVar, returnValue);
		addJump(blockIndex);
		addBlockTarget(blockIndex, ir.exitBasicBlock);
	}

	void addReturn(IrIndex blockIndex)
	{
		IrIndex returnType = context.types.getReturnType(ir.type, *context);
		context.assertf(returnType.isTypeVoid, "Trying to return void from non-void function");
		addJump(blockIndex);
		addBlockTarget(blockIndex, ir.exitBasicBlock);
	}

	IrIndex addJump(IrIndex blockIndex)
	{
		IrBasicBlock* block = &ir.getBlock(blockIndex);
		context.assertf(!block.isFinished, "%s.%s is already finished", context.idString(ir.backendData.name), blockIndex);
		block.isFinished = true;
		return emitInstr!(IrOpcode.jump)(blockIndex);
	}

	void addJumpToLabel(IrIndex blockIndex, ref IrLabel label)
	{
		if (label.isAllocated)
		{
			// label.blockIndex points to label's own block
			++label.numPredecessors;
			addBlockTarget(blockIndex, label.blockIndex);
			addJump(blockIndex);
		}
		else
		switch (label.numPredecessors)
		{
			case 0:
				// label.blockIndex points to block that started the scope
				// no block was created for label yet
				label.numPredecessors = 1;
				label.blockIndex = blockIndex;
				break;
			case 1:
				// label.blockIndex points to the only predecessor of label block
				// no block was created for label yet
				IrIndex firstPred = label.blockIndex;
				IrIndex secondPred = blockIndex;

				IrIndex labelBlock = addBasicBlock;

				addJump(firstPred);
				addJump(secondPred);
				addBlockTarget(firstPred, labelBlock);
				addBlockTarget(secondPred, labelBlock);

				label.blockIndex = labelBlock;
				label.numPredecessors = 2;
				label.isAllocated = true;
				break;
			default:
				context.unreachable;
				assert(false);
		}
	}

	void forceAllocLabelBlock(ref IrLabel label, int newPredecessors = 0)
	{
		if (!label.isAllocated)
		{
			switch (label.numPredecessors)
			{
				case 0:
					// label.blockIndex points to block that started the scope
					// no block was created for label yet
					label.blockIndex = addBasicBlock;
					label.isAllocated = true;
					break;
				case 1:
					// label.blockIndex points to the only predecessor of label block
					// no block was created for label yet
					IrIndex firstPred = label.blockIndex;
					label.blockIndex = addBasicBlock;
					addBlockTarget(firstPred, label.blockIndex);
					addJump(firstPred);
					label.isAllocated = true;
					break;
				default:
					context.unreachable;
					assert(false);
			}
		}

		label.numPredecessors += newPredecessors;
	}

	private void incBlockRefcount(IrIndex basicBlock) { assert(false); }
	private void decBlockRefcount(IrIndex basicBlock) { assert(false); }

	/// Creates virtual register to represent result of phi/instruction
	/// `definition` is phi/instruction that produces a value
	IrIndex addVirtualRegister(IrIndex definition, IrIndex type)
	{
		IrIndex virtRegIndex = appendVirtRegSlot();

		assert(type.isType, format("Invalid type (%s)", type));
		ir.getVirtReg(virtRegIndex) = IrVirtualRegister(definition, type);

		return virtRegIndex;
	}

	// ignores null opdId
	private void removeVirtualRegister(IrIndex virtRegIndex)
	{
		version(IrPrint) writefln("[IR] remove vreg %s", virtRegIndex);
		if (!ir.getVirtReg(virtRegIndex).isRemoved)
			++numRemovedVregs;
		// note: removing register while blockVarDef table contains values of this register is difficult
		// postpone removal until the end of IR construction
		// also, this way we can return memory from removed registers to arena
		// we will do removal after IR construction in `finalizeIr`
		ir.getVirtReg(virtRegIndex).type = virtRegIndex; // mark as removed
	}

	private void moveVreg(IrIndex fromSlot, IrIndex toSlot) {
		// redirect users
		redirectVregUsersTo(fromSlot, toSlot);
		// redirect definition (phi or instr)
		IrIndex definition = ir.getVirtReg(fromSlot).definition;
		switch (definition.kind) {
			case IrValueKind.phi: ir.getPhi(definition).result = toSlot; break;
			case IrValueKind.instruction: ir.get!IrInstrHeader(definition).result(ir) = toSlot; break;
			default: context.internal_error("Invalid definition %s of %s", definition.kind, fromSlot);
		}
		// move data
		version(IrPrint) writefln("[IR] moveVreg %s -> %s", fromSlot, toSlot);
		ir.getVirtReg(toSlot) = ir.getVirtReg(fromSlot);
	}

	// Adds phi function to specified block
	IrIndex addPhi(IrIndex blockIndex, IrIndex type, IrIndex var)
	{
		IrIndex phiIndex = appendPhiSlot;

		IrIndex vreg = addVirtualRegister(phiIndex, type);
		version(IrPrint) writefln("[IR] add %s %s", vreg, phiIndex);
		ir.get!IrPhi(phiIndex) = IrPhi(blockIndex, vreg, var);
		IrBasicBlock* block = &ir.getBlock(blockIndex);
		if (block.firstPhi.isDefined) {
			ir.get!IrPhi(block.firstPhi).prevPhi = phiIndex;
			ir.get!IrPhi(phiIndex).nextPhi = block.firstPhi;
		}
		block.firstPhi = phiIndex;
		return phiIndex;
	}

	private void removePhi(IrIndex phiIndex)
	{
		version(IrPrint) writefln("[IR] remove phi %s", phiIndex);
		IrPhi* phi = &ir.get!IrPhi(phiIndex);
		IrBasicBlock* block = &ir.getBlock(phi.blockIndex);
		version(IrPrint) {
			foreach(IrIndex phiIndex, ref IrPhi phi; block.phis(ir)) {
				writefln("[IR]   %s = %s", phi.result, phiIndex);
			}
		}
		// TODO: free list of phis
		if (block.firstPhi == phiIndex) block.firstPhi = phi.nextPhi;
		if (phi.nextPhi.isDefined) ir.get!IrPhi(phi.nextPhi).prevPhi = phi.prevPhi;
		if (phi.prevPhi.isDefined) ir.get!IrPhi(phi.prevPhi).nextPhi = phi.nextPhi;
		version(IrPrint) writefln("[IR] after remove phi %s", phiIndex);
		version(IrPrint) {
			foreach(IrIndex phiIndex, ref IrPhi phi; block.phis(ir)) {
				writefln("[IR]   %s = %s", phi.result, phiIndex);
			}
		}

		// mark as removed
		phi.blockIndex = IrIndex();
	}

	// Algorithm 2: Implementation of global value numbering
	/// Returns the last value of the variable in basic block
	private IrIndex readVariableRecursive(IrIndex blockIndex, IrIndex variable) {
		IrIndex value;
		if (!ir.getBlock(blockIndex).isSealed) {
			// Incomplete CFG
			IrIndex phiIndex = addPhi(blockIndex, getVarType(variable), variable);
			value = ir.get!IrPhi(phiIndex).result;
		}
		else
		{
			SmallVector preds = ir.getBlock(blockIndex).predecessors;
			if (preds.length == 1) {
				// Optimize the common case of one predecessor: No phi needed
				value = readVariable(preds[0, ir], variable);
			}
			else
			{
				// Break potential cycles with operandless phi
				IrIndex phiIndex = addPhi(blockIndex, getVarType(variable), variable);
				value = ir.get!IrPhi(phiIndex).result;
				writeVariable(blockIndex, variable, value);
				value = addPhiOperands(blockIndex, variable, phiIndex);
			}
		}
		with(IrValueKind)
		{
			assert(
				value.kind == constant ||
				value.kind == virtualRegister ||
				value.kind == physicalRegister, format("%s", value));
		}
		writeVariable(blockIndex, variable, value);
		return value;
	}

	// Adds all values of variable as arguments of phi. Values are gathered from block's predecessors.
	// Returns either φ result virtual register or one of its arguments if φ is trivial
	private IrIndex addPhiOperands(IrIndex blockIndex, IrIndex variable, IrIndex phi)
	{
		version(IrPrint) writefln("[IR] addPhiOperands %s %s %s %s", blockIndex, variable, phi, ir.get!IrPhi(phi).result);
		//dumpFunction(context, ir);
		// Determine operands from predecessors
		foreach (i, predIndex; ir.getBlock(blockIndex).predecessors.range(ir))
		{
			IrIndex value = readVariable(predIndex, variable);
			version(IrPrint) writefln("[IR] phi operand %s %s", predIndex, value);
			// Phi should not be cached before loop, since readVariable can add phi to phis, reallocating the array
			addPhiArg(phi, predIndex, value);
			addUser(phi, value);
		}
		return tryRemoveTrivialPhi(phi);
	}

	void addPhiArg(IrIndex phiIndex, IrIndex blockIndex, IrIndex value)
	{
		IrIndex phiArg = appendListItem!IrPhiArg(1);
		IrPhi* phi = &ir.get!IrPhi(phiIndex);
		// try to set phi's type if parameter is not a self reference
		if (value != phi.result)
		{
			IrVirtualRegister* resReg = &ir.getVirtReg(phi.result);
			// type is already set. Check if types match
			if (resReg.type.isDefined)
			{
				// do not test here, because ir to lir pass will produce invalid values at first
				//context.assertf(resReg.type == argType,
				//	"Types of phi arguments must match %s %s != %s",
				//	value, blockIndex, resReg.type);
			}
			else
			{
				IrIndex argType = ir.getValueType(context, value);
				context.assertf(argType.isType, "Invalid type (%s) of %s", argType, value);
				resReg.type = argType;
			}
		}

		ir.get!IrPhiArg(phiArg) = IrPhiArg(value, blockIndex, phi.firstArgListItem);
		phi.firstArgListItem = phiArg;
	}

	// Algorithm 3: Detect and recursively remove a trivial φ function
	// Returns either φ result virtual register or one of its arguments if φ is trivial
	private IrIndex tryRemoveTrivialPhi(IrIndex phiIndex) {
		// skip removed phi
		if (ir.get!IrPhi(phiIndex).isRemoved) return IrIndex();

		IrPhiArg same;
		IrIndex phiResultIndex = ir.get!IrPhi(phiIndex).result;
		foreach (size_t i, ref IrPhiArg phiArg; ir.get!IrPhi(phiIndex).args(ir))
		{
			version(IrPrint) writefln("[IR] arg %s %s", phiArg.value, phiArg.basicBlock);
			if (phiArg.value == same.value || phiArg.value == phiResultIndex) {
				version(IrPrint) writefln("[IR]   same");
				continue; // Unique value or self−reference
			}
			if (same != IrPhiArg()) {
				version(IrPrint) writefln("[IR]   %s is non-trivial", phiIndex);
				return phiResultIndex; // The phi merges at least two values: not trivial
			}
			version(IrPrint) writefln("[IR]   same = %s", phiArg.value);
			same = phiArg;
		}
		version(IrPrint) writefln("[IR]   %s is trivial", phiIndex);
		assert(same.value.isDefined, "Phi function got no arguments");

		// Remember all users except the phi itself
		assert(phiResultIndex.kind == IrValueKind.virtualRegister, format("%s", phiResultIndex));

		SmallVector users = ir.getVirtReg(phiResultIndex).users;

		// Reroute all uses of phi to same and remove phi
		replaceBy(phiIndex, users, phiResultIndex, same);

		// Update mapping from old phi result to same, since we may need to read
		// this variable in later blocks, which will cause us to read removed phi
		IrIndex maybePhiVar = ir.get!IrPhi(phiIndex).var;
		if (maybePhiVar.isDefined)
		{
			IrIndex blockIndex = ir.get!IrPhi(phiIndex).blockIndex;
			updatePhiVarDefs(blockIndex, maybePhiVar, phiResultIndex, same.value);
		}

		removePhi(phiIndex);

		// Try to recursively remove all phi users, which might have become trivial
		foreach (i, index; users.range(ir))
			if (index.kind == IrValueKind.phi && index != phiIndex)
				tryRemoveTrivialPhi(index);

		removeVirtualRegister(phiResultIndex);
		return same.value;
	}

	private void updatePhiVarDefs(IrIndex blockIndex, IrIndex var, IrIndex oldValue, IrIndex newValue)
	{
		version(IrPrint) writefln("[IR]   updatePhiVarDefs %s %s %s: %s", blockIndex, var, newValue, blockVarDef);
		if (auto val = BlockVarPair(blockIndex, var) in blockVarDef)
		{
			if (*val == oldValue)
			{
				version(IrPrint) writefln("[IR]   phi update blockVarDef %s %s %s -> %s", blockIndex, var, *val, newValue);
				*val = newValue;

				foreach (i, succIndex; ir.getBlock(blockIndex).successors.range(ir)) {
					updatePhiVarDefs(succIndex, var, oldValue, newValue);
				}
			}
		}
		version(IrPrint) writefln("[IR]   updatePhiVarDefs %s %s %s: %s", blockIndex, var, newValue, blockVarDef);
	}

	IrIndex definitionOf(IrIndex someIndex)
	{
		final switch (someIndex.kind) with(IrValueKind) {
			case none: assert(false);
			case listItem: assert(false);
			case instruction: return someIndex;
			case basicBlock: assert(false);
			case constant: assert(false);
			case constantAggregate: assert(false);
			case constantZero: assert(false);
			case global: assert(false);
			case phi: return someIndex;
			case func: assert(false); // TODO
			case stackSlot: assert(false); // TODO
			case virtualRegister: return ir.getVirtReg(someIndex).definition;
			case physicalRegister: assert(false);
			case type: assert(false);
			case variable: assert(false);
		}
	}

	/// Replaces all 'vreg' uses with `byWhat`
	void redirectVregUsersTo(IrIndex vreg, IrIndex byWhat) {
		context.assertf(vreg.isVirtReg, "'vreg' must be virtual register, not %s", vreg.kind);
		version(IrPrint) writefln("[IR] redirectVregUsersTo %s -> %s", vreg, byWhat);

		SmallVector users = ir.getVirtReg(vreg).users;
		foreach (size_t i, IrIndex userIndex; users.range(ir))
		{
			switch (userIndex.kind) with(IrValueKind) {
				case instruction:
					foreach (ref IrIndex arg; ir.get!IrInstrHeader(userIndex).args(ir))
						if (arg == vreg) {
							arg = byWhat;
							addUser(userIndex, byWhat);
						}
					break;
				case phi:
					foreach (size_t i, ref IrPhiArg phiArg; ir.get!IrPhi(userIndex).args(ir))
						if (phiArg.value == vreg) {
							phiArg.value = byWhat;
							addUser(userIndex, byWhat);
						}
					break;
				default: assert(false);
			}
		}
	}

	// ditto
	/// Rewrites all users of phi to point to `byWhat` instead of its result `what`.
	/// `what` is the result of phi (vreg), `phiUsers` is users of `what`
	private void replaceBy(IrIndex phiIndex, SmallVector phiUsers, IrIndex what, IrPhiArg byWhat) {
		version(IrPrint) writefln("[IR]     replaceBy %s %s -> %s", phiIndex, what, byWhat);

		foreach (size_t i, IrIndex phiUserIndex; phiUsers.range(ir))
		{
			version(IrPrint) writefln("[IR]     user %s %s", i, phiUserIndex);

			// skip self-reference (we will delete phi anyway)
			if (phiUserIndex == phiIndex) continue;

			final switch (phiUserIndex.kind) with(IrValueKind) {
				case none: assert(false);
				case listItem: assert(false);
				case instruction:
					foreach (ref IrIndex arg; ir.get!IrInstrHeader(phiUserIndex).args(ir))
						if (arg == what)
						{
							arg = byWhat.value;
							replaceUserWith(byWhat.value, phiIndex, phiUserIndex);
						}
					break;
				case basicBlock: assert(false);
				case constant, constantAggregate, constantZero: assert(false);
				case global: assert(false);
				case phi:
					if (ir.get!IrPhi(phiUserIndex).isRemoved) continue; // skip
					foreach (size_t i, ref IrPhiArg phiArg; ir.get!IrPhi(phiUserIndex).args(ir))
					{
						if (phiArg.value == what)
						{
							phiArg.value = byWhat.value;
							phiArg.basicBlock = byWhat.basicBlock;
							replaceUserWith(byWhat.value, phiIndex, phiUserIndex);
						}
					}
					break;
				case stackSlot: assert(false); // TODO
				case virtualRegister: assert(false);
				case physicalRegister: assert(false);
				case type: assert(false);
				case variable: assert(false);
				case func: assert(false);
			}
		}
	}

	// Replace a user 'what' that uses 'used' by 'byWhat' in a list of users inside 'what'
	private void replaceUserWith(IrIndex used, IrIndex what, IrIndex byWhat) {
		// If argument is used once, then user appears only once.
		// When replacing users with phi users, replacement wil occur only for first phi user.
		// Other phi users will not find any users to replace.
		// So add append users instead if no replacement was done.
		void replaceVregUser(ref IrVirtualRegister vreg) {
			bool replaced = vreg.users.replaceFirst(ir, what, byWhat);
			if (!replaced) vreg.users.append(&this, byWhat);
		}
		final switch (used.kind) with(IrValueKind) {
			case none, listItem, basicBlock, physicalRegister: assert(false);
			case instruction: return replaceVregUser(ir.getVirtReg(ir.get!IrInstrHeader(used).result(ir)));
			case constant, constantAggregate, constantZero: return; // constants dont track individual users
			case global: return; // globals dont track individual users
			case phi: return replaceVregUser(ir.getVirtReg(ir.get!IrPhi(used).result));
			case stackSlot: assert(false); // TODO
			case virtualRegister: return replaceVregUser(ir.getVirtReg(used));
			case type: return; // no user tracking
			case variable: assert(false);
			case func: return; // no user tracking
		}
	}
}
