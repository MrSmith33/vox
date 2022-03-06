/// Copyright: Copyright (c) 2018-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

module vox.be.reg_alloc.move_solver;

import vox.all;

/// Reorders a set of moves, to produce correct behavior
/// Nodes can be in 3 states:
///   RO - value is only read. Those are not added to writtenNodes array.
///   RW - value is read 1 or more times and written 1 time. Indices of these nodes are at the beginning of writtenNodes
///   WO - value is only written. Indices are at the end of writtenNodes.
struct MoveSolver
{
	CompilationContext* context;
	IrFunction* ir;

	ValueInfo[] stackSlots;
	ValueInfo[NUM_REGS_PER_CLASS][NUM_REG_CLASSES] registers;
	ValueInfo anyConstant;
	IrIndex* writtenNodesPtr;
	size_t savedBufLength;
	uint numWrittenNodes;
	uint numWriteOnlyValues;

	// allocate buffers
	// takes unique ownership of tempBuffer
	// Inits all register and stack slot infos
	// after each `placeMovesBeforeInstr` call they need to be in init state for reuse
	void setup(IrFunction* ir)
	{
		this.ir = ir; // for errors
		savedBufLength = context.tempBuffer.length;
		stackSlots = context.allocateTempArray!ValueInfo(ir.numStackSlots);
		writtenNodesPtr = cast(IrIndex*)context.tempBuffer.nextPtr;
	}

	void reset()
	{
		assert(anyConstant == ValueInfo.init);
		assert(numWriteOnlyValues == 0);
		assert(numWrittenNodes == 0);
	}

	// releases unique ownership of tempBuffer
	void release()
	{
		context.tempBuffer.length = savedBufLength;

		savedBufLength = 0;
		stackSlots = null;
		writtenNodesPtr = null;
		assert(anyConstant == ValueInfo.init);
		assert(numWriteOnlyValues == 0);
		assert(numWrittenNodes == 0);
	}

	ref ValueInfo getInfo(IrIndex index) return {
		switch(index.kind) with(IrValueKind) {
			case constant, constantZero, global, func: return anyConstant;
			case stackSlot: return stackSlots[index.storageUintIndex];
			case physicalRegister:  return registers[index.physRegClass][index.physRegIndex];
			default: context.internal_error("getInfo(%s)", index);
		}
	}

	void addMove(IrIndex fromIndex, IrIndex toIndex, IrArgSize argSize, FromValue fromIsValue)
	{
		assert(fromIndex.isDefined);
		assert(toIndex.isDefined);
		if (fromIndex == toIndex) return;

		ValueInfo* from = &getInfo(fromIndex);
		ValueInfo* to = &getInfo(toIndex);

		from.onRead(fromIndex, fromIsValue);
		// no longer write only
		if (from.numReads == 1 && from.readFrom.isDefined) {
			wo_to_rw(from.arrayPos);
		}

		context.assertf(toIndex.isPhysReg || toIndex.isStackSlot, "toIndex is %s", toIndex.kind);
		context.assertf(!to.readFrom.isDefined, "Second write to %s detected", IrIndexDump(toIndex, context, ir));

		to.onWrite(fromIndex, toIndex, argSize);
		to.arrayPos = numWrittenNodes;
		context.tempBuffer.put(toIndex.asUint);
		++numWrittenNodes;
		++numWriteOnlyValues;

		if (to.numReads > 0) {
			wo_to_rw(to.arrayPos);
		}
	}

	void print() {
		IrIndex[] writtenNodes = writtenNodesPtr[0..numWrittenNodes];
		foreach(IrIndex index; writtenNodes[0..$-numWriteOnlyValues])
			writef("(%s %s) ", index, getInfo(index));
		write("| ");
		foreach(IrIndex index; writtenNodes[$-numWriteOnlyValues..$])
			writef("(%s %s) ", index, getInfo(index));
		writeln;
	}

	void wo_to_rw(uint arrayPos)
	{
		IrIndex[] writtenNodes = writtenNodesPtr[0..numWrittenNodes];
		assert(numWriteOnlyValues > 0);
		size_t from = arrayPos;
		size_t to = numWrittenNodes - numWriteOnlyValues;
		if (from != to) {
			swap(writtenNodes[from], writtenNodes[to]);
			getInfo(writtenNodes[from]).arrayPos = cast(uint)from;
			getInfo(writtenNodes[to]).arrayPos = cast(uint)to;
		}
		--numWriteOnlyValues;
	}

	void rw_to_wo(uint arrayPos)
	{
		IrIndex[] writtenNodes = writtenNodesPtr[0..numWrittenNodes];
		++numWriteOnlyValues;
		size_t from = arrayPos;
		size_t to = numWrittenNodes - numWriteOnlyValues;
		if (from != to) {
			swap(writtenNodes[from], writtenNodes[to]);
			getInfo(writtenNodes[from]).arrayPos = cast(uint)from;
			getInfo(writtenNodes[to]).arrayPos = cast(uint)to;
		}
	}

	void removeItem(ValueInfo* item)
	{
		IrIndex[] writtenNodes = writtenNodesPtr[0..numWrittenNodes];
		size_t from = numWrittenNodes-1;
		size_t to = item.arrayPos;
		if (from != to) {
			writtenNodes[to] = writtenNodes[from];
			getInfo(writtenNodes[to]).arrayPos = cast(uint)to;
		}
		*item = ValueInfo();
		--numWrittenNodes;
		context.tempBuffer.unput(1);
	}

	void placeMovesBeforeInstr(IrBuilder* builder, IrIndex beforeInstr, IrIndex delegate() getScratchSpillSlot)
	{
		void makeStore(IrIndex dst, IrIndex src, IrArgSize argSize)
		{
			ExtraInstrArgs extra = { argSize : argSize };
			IrIndex instr = builder.emitInstr!(Amd64Opcode.store)(extra, dst, src);
			builder.insertBeforeInstr(beforeInstr, instr);
		}

		void makeLoad(IrIndex dst, IrIndex src, IrArgSize argSize)
		{
			ExtraInstrArgs extra = { result : dst, argSize : argSize };
			InstrWithResult instr = builder.emitInstr!(Amd64Opcode.load)(extra, src);
			builder.insertBeforeInstr(beforeInstr, instr.instruction);
		}

		void makeMov(IrIndex dst, IrIndex src, IrArgSize argSize)
		{
			ExtraInstrArgs extra = { result : dst, argSize : argSize };
			InstrWithResult instr = builder.emitInstr!(Amd64Opcode.mov)(extra, src);
			builder.insertBeforeInstr(beforeInstr, instr.instruction);
		}

		while (numWrittenNodes)
		{
			IrIndex toIndex = writtenNodesPtr[numWrittenNodes-1];
			ValueInfo* to = &getInfo(toIndex);
			IrIndex fromIndex = to.readFrom;
			ValueInfo* from = &getInfo(fromIndex);

			if (to.numReads == 0)
			{
				version(RAPrint_resolve) writefln("insert move %s <- %s", toIndex, fromIndex);
				if (fromIndex.isStackSlot)
				{
					if (toIndex.isStackSlot) // stack slot -> stack slot
					{
						IrIndex scratchSpillSlot = getScratchSpillSlot();
						IrIndex scratchReg = IrIndex(0, IrArgSize.size64, 0);

						makeStore(scratchSpillSlot, scratchReg, IrArgSize.size64);

						// we don't have correct argSize inside `from` because size is only registered for move dst
						scratchReg.physRegSize = to.argSize;
						makeLoad(scratchReg, fromIndex, to.argSize);
						makeStore(toIndex, scratchReg, to.argSize);

						scratchReg.physRegSize = IrArgSize.size64;
						makeLoad(scratchReg, scratchSpillSlot, IrArgSize.size64);
					}
					else // stack slot -> con or reg
					{
						assert(toIndex.isSomeReg);
						if (from.isValue)
							makeMov(toIndex, fromIndex, to.argSize);
						else
							makeLoad(toIndex, fromIndex, to.argSize);
					}
				}
				else // from is reg or constant
				{
					if (toIndex.isStackSlot) // con or reg -> stack slot
					{
						makeStore(toIndex, fromIndex, to.argSize);
					}
					else // con or reg -> reg
					{
						makeMov(toIndex, fromIndex, to.argSize);
					}
				}

				--from.numReads;
				--numWriteOnlyValues;
				removeItem(to);

				if (from.numReads == 0 && from.readFrom.isDefined)
					rw_to_wo(from.arrayPos);
			}
			else // to.numReads > 0
			{
				// process cycled items
				// from is non-constant in this scope

				// to <-- from <-- from.readFrom
				// mark from as removed and rewrite as:
				// to <-- from.readFrom

				version(RAPrint_resolve) writefln("xchg from %s to %s %s", *from, *to, toIndex);

				// For now assume both are physical registers
				IrIndex reg0 = fromIndex;
				IrIndex reg1 = from.readFrom;
				context.assertf(reg0.isPhysReg, "%s is not phys reg", reg0);
				context.assertf(reg1.isPhysReg, "%s is not phys reg", reg1);

				// In case one register is bigger that the other, make them of equal size, so that code emitter doesn't complain
				uint physRegSize = max(reg0.physRegSize, reg1.physRegSize);
				reg0.physRegSize = physRegSize;
				reg1.physRegSize = physRegSize;

				IrIndex instr = builder.emitInstr!(Amd64Opcode.xchg)(ExtraInstrArgs(), reg0, reg1);
				builder.insertBeforeInstr(beforeInstr, instr);

				if (from.readFrom == toIndex) {
					// handle case when from.readFrom == to
					// ... to <-- from <-- to ...
					removeItem(to);
				} else {
					to.readFrom = from.readFrom;
					--from.numReads;
				}
				removeItem(from);
			}
		}
	}
}

enum FromValue : bool {
	no = false,
	yes = true,
}

struct ValueInfo
{
	uint arrayPos; // index into writtenNodes
	IrIndex readFrom; // can be null
	ushort numReads;
	IrArgSize argSize; // used for memory moves
	FromValue isValue;

	void onRead(IrIndex self, FromValue isValue)
	{
		++numReads;
		this.isValue = isValue;
	}

	void onWrite(IrIndex from, IrIndex self, IrArgSize argSize)
	{
		readFrom = from;
		this.argSize = argSize;
	}
}
