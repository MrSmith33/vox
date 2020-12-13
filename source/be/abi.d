/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

// Missing case of System V ABI implementation:
// - Aggregates with alignment > 16 bytes
// - xmm/ymm/zmm register passing
// - x87
module be.abi;

import std.bitmanip : bitfields;
import std.stdio;
import all;

/// 2 AbiClass slots is enough to classify structs containing m128/m256/m512 types.
/// They can be the only value in the struct.
/// The only rules to filter this needed, are:
/// - m128/m256/m512 must have offset of 0
/// - basic types must have offset < 16

enum AbiClass : ubyte {
	/// This class is used as initializer in the algorithms. It will be used for padding and empty structures and unions.
	no_class,
	/// This class consists of integral types that fit into one of the general purpose registers.
	integer,
	/// The class consists of types that fit into a vector register.
	sse,
	/// The class consists of types that fit into a vector register and can be passed and returned in the upper bytes of it.
	sse_up,
	/// These classes consists of types that will be returned via the x87 FPU.
	x87,
	/// ditto
	x87_up,
	/// This class consists of types that will be returned via the x87 FPU.
	complex_x87,
	/// This class consists of types that will be passed and returned in memory via the stack.
	memory,
}

// Plaform-independent classification of function parameter/result
enum PassClass : ubyte {
	// Register stores value
	// sysv64 & win64 ABI: parameters, return value
	byValueReg,
	// 2 registers, possibly of different classes store value
	// sysv64: parameters, return value
	byValueRegMulti,
	// Pointer to stack allocated value is passed via GPR
	// win64 ABI: parameters, return value
	// sysv64 ABI: return value
	byPtrReg,
	// Value is pushed on the stack
	// sysv64 & win64 ABI: parameters
	byValueMemory,
	// Pointer to stack allocated value is passed via stack
	// win64 ABI: parameters
	byPtrMemory,
	// Value is ignored. Used for empty structs
	// sysv64 ABI: parameters, return value
	ignore
}

bool isMemory(PassClass passClass) { return passClass == PassClass.byValueMemory || passClass == PassClass.byPtrMemory; }

struct Sysv_AbiParamClass {
	mixin(bitfields!(
		AbiClass,      "low", 4,
		AbiClass,     "high", 4,
	));
	PassClass passClass;
	ubyte len() { return high == AbiClass.no_class ? 1 : 2; }

	this(PassClass passClass, AbiClass a) {
		this.passClass = passClass;
		low = a;
	}
	this(PassClass passClass, AbiClass a, AbiClass b) {
		this.passClass = passClass;
		low = a;
		high = b;
	}

	void toString(scope void delegate(const(char)[]) sink) {
		if (high == AbiClass.no_class)
			sink.formattedWrite("(%s)", low);
		else
			sink.formattedWrite("(%s,%s)", low, high);
	}
}

enum MAX_ARG_REGS = 2;

struct AbiState {
	FunctionAbi abi;

	PhysReg[][2] abiRegs;
	PhysReg[] retRegs;
	IrTypeFunction* type;

	uint[] paramClassBuf;

	void run(CompilationContext* c, IrTypeFunction* type) {
		this.type = type;
		auto cc = callConventions[type.callConv];
		abi.stackAlignment = cc.minStackAlignment;
		paramClassBuf = c.allocateTempArray!uint(alignValue(type.numParameters, 4) / 4);
		PassClass[] paramClasses = (cast(PassClass[])paramClassBuf)[0..type.numParameters];
		abi.paramClasses = paramClasses[0..type.numParameters];
		abi.paramData = c.allocateTempArray!ParamLocation(type.numParameters);

		abiRegs[AMD64_REG_CLASS.GPR] = cc.gprParamRegs;
		abiRegs[AMD64_REG_CLASS.XMM] = cc.sseParamRegs;
		ArgClassifier classify = classify_abis[type.callConv];
		classify(c, this);

		calc_stack(c);
	}

	void free(CompilationContext* c) {
		c.freeTempArray(abi.paramData);
		c.freeTempArray(paramClassBuf);
	}

	void calc_stack(CompilationContext* c) {
		// choose stack ordering
		int start = 0;
		int end = cast(int)abi.paramClasses.length;
		int inc = 1;

		if (abi.reverseStackOrder) {
			swap(start, end);
			--start;
			--end;
			inc = -1;
		}

		enum MIN_STACK_SLOT_SIZE = 8;

		// returns assigned register to the pool
		// item added will have increasing stack offset
		void assignToMem(ref ParamLocation loc, SizeAndAlignment item) {
			abi.stackAlignment = max(abi.stackAlignment, item.alignment);
			// each item takes a whole number of stack slots (8 byte slots on 64 bit arch)
			uint itemAlignment = max(MIN_STACK_SLOT_SIZE, item.alignment);
			abi.stackSize = alignValue!uint(abi.stackSize, itemAlignment);
			loc.stackOffset = abi.stackSize;
			loc.stackSize = max(MIN_STACK_SLOT_SIZE, item.size);

			abi.stackSize += loc.stackSize;
		}

		// actually assign memory offsets
		for (int i = start; i != end; i += inc) {
			PassClass paramClass = abi.paramClasses[i];
			if (paramClass == PassClass.byValueMemory) {
				SizeAndAlignment sizeAlign = c.types.typeSizeAndAlignment(type.parameterTypes[i]);
				assignToMem(abi.paramData[i], sizeAlign);
				//writefln("offset %s %s %s", i, abi.paramData[i].stackOffset, sizeAlign.size);
			} else if (paramClass == PassClass.byPtrMemory) {
				assignToMem(abi.paramData[i], SizeAndAlignment(8, 3));
			}
		}
		abi.stackSize = alignValue!uint(abi.stackSize, MIN_STACK_SLOT_SIZE);
	}
}

union ParamLocation {
	PhysReg[MAX_ARG_REGS] regs;
	struct {
		// offset from the first byte of first parameter on the stack
		// first parameter will have offset of 0
		uint stackOffset;
		uint stackSize;
	}
}

struct FunctionAbi
{
	// must be byValueReg when function has no result
	PassClass returnClass;
	// if is pass by ptr, then first slot is hidden parameter and second is return register
	ParamLocation returnLoc;
	// length is number of function parameters
	// hidden parameter is not included here, instead it is handled immediately
	PassClass[] paramClasses;
	// when same index is memory, this is int offset
	// when same index is register, this is register
	ParamLocation[] paramData;
	uint stackSize;
	uint stackAlignment;
	bool reverseStackOrder;
	ubyte numRegistersUsed;
	// if defined, syscall instruction is used instead of call
	PhysReg syscallRegister;
	bool useSyscall;
}

// classification callback will classify the argument and immediately try to assign register if available
// If there is no free registers left, it will reclassify the argument to be passed via memory.
void win64_classify(CompilationContext* c, ref AbiState state)
{
	// used for gpr and sse regs
	ubyte regsRemaining = cast(ubyte)state.abiRegs[AMD64_REG_CLASS.GPR].length;
	scope(exit) {
		state.abi.numRegistersUsed = cast(ubyte)(state.abiRegs[AMD64_REG_CLASS.GPR].length - regsRemaining);
	}

	state.abi.returnClass = PassClass.ignore;
	if (state.type.numResults == 1) {
		IrIndex resType = state.type.resultTypes[0];
		if (resType.isTypeFloat) {
			state.abi.returnClass = PassClass.byValueReg;
			state.abi.returnLoc.regs[0] = amd64_reg.xmm0;
		} else if (resType.fitsIntoRegister(c)) {
			state.abi.returnClass = PassClass.byValueReg;
			state.abi.returnLoc.regs[0] = amd64_reg.ax;
		} else {
			// hidden pointer is passed as first parameter
			state.abi.returnClass = PassClass.byPtrReg;
			PhysReg reg = state.abiRegs[AMD64_REG_CLASS.GPR][0];
			state.abi.returnLoc.regs[0] = reg;
			state.abi.returnLoc.regs[1] = amd64_reg.ax; // we store return register in second slot
			--regsRemaining;
		}
	}
	//writefln("result %s %s", state.abi.returnClass, state.abi.returnLoc.regs);

	foreach(uint i; 0..cast(uint)state.type.numParameters) {
		IrIndex paramType = state.type.parameterTypes[i];
		if (paramType.isTypeFloat) {
			if (regsRemaining) {
				state.abi.paramClasses[i] = PassClass.byValueReg;
				PhysReg reg = state.abiRegs[AMD64_REG_CLASS.XMM][$-regsRemaining];
				state.abi.paramData[i].regs[0] = reg;
				--regsRemaining;
			} else {
				state.abi.paramClasses[i] = PassClass.byValueMemory;
			}
		} else if (fitsIntoRegister(paramType, c)) {
			if (regsRemaining) {
				state.abi.paramClasses[i] = PassClass.byValueReg;
				PhysReg reg = state.abiRegs[AMD64_REG_CLASS.GPR][$-regsRemaining];
				state.abi.paramData[i].regs[0] = reg;
				--regsRemaining;
			} else {
				state.abi.paramClasses[i] = PassClass.byValueMemory;
			}
		} else {
			if (regsRemaining) {
				state.abi.paramClasses[i] = PassClass.byPtrReg;
				PhysReg reg = state.abiRegs[AMD64_REG_CLASS.GPR][$-regsRemaining];
				state.abi.paramData[i].regs[0] = reg;
				--regsRemaining;
			} else {
				state.abi.paramClasses[i] = PassClass.byPtrMemory;
			}
		}

		//writefln("param %s %s %s", i, state.abi.paramClasses[i], state.abi.returnLoc.regs);
	}
}

enum InMemory : bool {
	no = false,
	yes = true,
}

Sysv_AbiParamClass classify_value(CompilationContext* c, IrIndex paramType) {
	AbiClass[2] resultClasses = [AbiClass.no_class, AbiClass.no_class];
	InMemory in_mem = classify_value_impl(c, paramType, resultClasses, 0);
	if (in_mem) return Sysv_AbiParamClass(PassClass.byValueMemory, AbiClass.memory);

	// parse classes to PassClass
	if (resultClasses[1] == AbiClass.no_class) {
		if (resultClasses[0] == AbiClass.no_class)
			return Sysv_AbiParamClass(PassClass.ignore, AbiClass.no_class);
		else
			return Sysv_AbiParamClass(PassClass.byValueReg, resultClasses[0]);
	}
	assert(resultClasses[0] != AbiClass.no_class);
	return Sysv_AbiParamClass(PassClass.byValueRegMulti, resultClasses[0], resultClasses[1]);
}

InMemory classify_value_impl(CompilationContext* c, IrIndex paramType, ref AbiClass[2] resultClasses, uint offset) {
	assert(paramType.isType);
	// if anything starts after 16 bytes, the whole thing is passed through memory
	if (offset >= 16) return InMemory.yes;

	SizeAndAlignment sizealign = c.types.typeSizeAndAlignment(paramType);
	if (sizealign.size == 0) return InMemory.no;
	// until we have support for m256 and m512, 16 bytes is max size for register
	if (sizealign.size > 16) return InMemory.yes;
	// if it is unaligned, the whole argument is passed in memory
	if (paddingSize(offset, sizealign.alignment) > 0) return InMemory.yes;

	AbiClass basic_class;
	final switch (paramType.typeKind) with(IrTypeKind) {
		case basic:
			final switch(paramType.basicType) with(IrValueType) {
				case noreturn_t: basic_class = AbiClass.no_class; break;
				case void_t: basic_class = AbiClass.no_class; break;
				case i8:     basic_class = AbiClass.integer; break;
				case i16:    basic_class = AbiClass.integer; break;
				case i32:    basic_class = AbiClass.integer; break;
				case i64:    basic_class = AbiClass.integer; break;
				case f32:    basic_class = AbiClass.sse; break;
				case f64:    basic_class = AbiClass.sse; break;
			}
			break;
		case pointer:
		case func_t:
			basic_class = AbiClass.integer;
			break;
		case array:
			return classify_array(c, paramType, resultClasses, offset);
		case struct_t:
			return classify_struct(c, paramType, resultClasses, offset);
	}

	uint slotIndex = offset / 8; // index of 8-byte
	c.assertf(slotIndex < 2, "Incorrect 8-byte index: %s", slotIndex); // must be 0 or 1
	resultClasses[slotIndex] = merge_classes(resultClasses[slotIndex], basic_class);
	return InMemory.no;
}

AbiClass merge_classes(AbiClass a, AbiClass b) {
	if (a == b) return a;
	if (a > b) swap(a, b);
	switch(a) with(AbiClass) {
		case no_class: return b;
		case memory: return memory;
		case integer: return integer;
		case sse: return sse;
		default: assert(false);
	}
}

InMemory classify_array(CompilationContext* c, IrIndex paramType, ref AbiClass[2] resultClasses, uint offset) {
	IrTypeArray type = c.types.get!IrTypeArray(paramType);
	SizeAndAlignment element_sizealign = c.types.typeSizeAndAlignment(type.elemType);
	foreach(i; 0..type.numElements) {
		auto memberOffset = offset + element_sizealign.size * i;
		InMemory in_mem = classify_value_impl(c, type.elemType, resultClasses, memberOffset);
		// if any member ends up in memory, the whole thing is in memory
		if (in_mem) return InMemory.yes;
	}
	return InMemory.no;
}

InMemory classify_struct(CompilationContext* c, IrIndex paramType, ref AbiClass[2] resultClasses, uint offset) {
	IrTypeStructMember[] members = c.types.get!IrTypeStruct(paramType).members;
	foreach(m; members) {
		auto memberOffset = offset + m.offset;
		InMemory in_mem = classify_value_impl(c, m.type, resultClasses, memberOffset);
		// if any member ends up in memory, the whole thing is in memory
		if (in_mem) return InMemory.yes;
	}
	return InMemory.no;
}

void sysv64_classify(CompilationContext* c, ref AbiState state)
{
	ubyte[2] regsRemaining;

	regsRemaining[AMD64_REG_CLASS.GPR] = cast(ubyte)state.abiRegs[AMD64_REG_CLASS.GPR].length;
	regsRemaining[AMD64_REG_CLASS.XMM] = cast(ubyte)state.abiRegs[AMD64_REG_CLASS.XMM].length;
	scope(exit) {
		size_t gpr = state.abiRegs[AMD64_REG_CLASS.GPR].length - regsRemaining[AMD64_REG_CLASS.GPR];
		size_t xmm = state.abiRegs[AMD64_REG_CLASS.XMM].length - regsRemaining[AMD64_REG_CLASS.XMM];
		state.abi.numRegistersUsed = cast(ubyte)(gpr + xmm);
	}

	state.abi.returnClass = PassClass.ignore;
	if (state.type.numResults == 1) {
		IrIndex resType = state.type.resultTypes[0];
		Sysv_AbiParamClass resClass = classify_value(c, resType);
		state.abi.returnClass = resClass.passClass;
		if (resClass.low == AbiClass.sse) {
			state.abi.returnLoc.regs[0] = amd64_reg.xmm0;
			if (resClass.high == AbiClass.sse)
				state.abi.returnLoc.regs[1] = amd64_reg.xmm1;
			else if (resClass.high == AbiClass.integer)
				state.abi.returnLoc.regs[1] = amd64_reg.ax;
		} else if (resClass.low == AbiClass.integer) {
			state.abi.returnLoc.regs[0] = amd64_reg.ax;
			if (resClass.high == AbiClass.sse)
				state.abi.returnLoc.regs[1] = amd64_reg.xmm0;
			else if (resClass.high == AbiClass.integer)
				state.abi.returnLoc.regs[1] = amd64_reg.dx;
		} else if (resClass.passClass == PassClass.byValueMemory) {
			// hidden pointer is passed as first parameter
			state.abi.returnClass = PassClass.byPtrReg;
			PhysReg reg = state.abiRegs[AMD64_REG_CLASS.GPR][0];
			state.abi.returnLoc.regs[0] = reg;
			state.abi.returnLoc.regs[1] = amd64_reg.ax; // we store return register in second slot
			--regsRemaining[AMD64_REG_CLASS.GPR];
		}
		//writefln("res %s %s", state.abi.returnClass, state.abi.returnLoc.regs);
	}

	// assign register or fallback to memory class
	foreach(uint i; 0..cast(uint)state.type.numParameters) {
		IrIndex paramType = state.type.parameterTypes[i];
		Sysv_AbiParamClass paramClass_sysv = classify_value(c, paramType);
		state.abi.paramClasses[i] = paramClass_sysv.passClass;
		switch(paramClass_sysv.low)
		{
			case AbiClass.integer:
			case AbiClass.sse:
				// 1 or 2 registers of same or different class
				ubyte getRegClass(AbiClass abiClass) {
					switch(abiClass) {
						case AbiClass.integer: return AMD64_REG_CLASS.GPR;
						case AbiClass.sse: return AMD64_REG_CLASS.XMM;
						default: assert(false);
					}
				}

				AbiClass[2] classes = [paramClass_sysv.low, paramClass_sysv.high];
				ubyte[2] regsNeeded;

				foreach(uint j; 0..paramClass_sysv.len) {
					ubyte regClass = getRegClass(classes[j]);
					++regsNeeded[regClass];
				}

				foreach(ubyte regClass; 0..2) {
					if (regsRemaining[regClass] < regsNeeded[regClass]) {
						state.abi.paramClasses[i] = PassClass.byValueMemory;
						goto case AbiClass.memory;
					}
				}

				// assign regs
				foreach(uint j; 0..paramClass_sysv.len) {
					ubyte regClass = getRegClass(classes[j]);
					assert(regsRemaining[regClass]);
					PhysReg reg = state.abiRegs[regClass][$-regsRemaining[regClass]];
					state.abi.paramData[i].regs[j] = reg;
					--regsRemaining[regClass];
				}
				break;

			case AbiClass.memory: break;
			case AbiClass.no_class: break; // for empty structs
			default:
				c.internal_error("%s not implemented", paramClass_sysv.low);
		}
	}
}

void sysv64_syscall_classify(CompilationContext* c, ref AbiState state)
{
	state.abi.syscallRegister = amd64_reg.ax;
	state.abi.useSyscall = true;
	if (c.targetOs != TargetOs.linux)
		c.error("Cannot use System V syscall calling convention on %s", c.targetOs);

	sysv64_classify(c, state);

	if (state.type.numParameters > 6) {
		c.error("Cannot have more than 6 parameters in System V syscall calling convention");
	}
	if (state.abi.returnClass != PassClass.byValueReg &&
		state.abi.returnClass != PassClass.byPtrReg &&
		state.abi.returnClass != PassClass.ignore)
		c.error("Cannot have return of class %s in System V syscall calling convention", state.abi.returnClass);
	foreach(PassClass paramClass; state.abi.paramClasses) {
		if (paramClass == PassClass.byValueReg || paramClass == PassClass.byPtrReg || paramClass == PassClass.ignore) continue;
		c.error("Cannot have parameters of class %s in System V syscall calling convention", paramClass);
	}
}

alias ArgClassifier = void function(CompilationContext* c, ref AbiState state);
__gshared ArgClassifier[] classify_abis = [
	&win64_classify,
	&sysv64_classify,
	&sysv64_syscall_classify
];

// Handle ABI
void func_pass_lower_abi(CompilationContext* c, IrFunction* ir, IrIndex funcIndex, ref IrBuilder builder)
{
	//writefln("lower_abi %s %s", builder.context.idString(ir.backendData.name), ir.getCallConvEnum(c));
	IrTypeFunction* irFuncType = &c.types.get!IrTypeFunction(ir.type);
	c.assertf(irFuncType.numResults <= 1, "%s results is not implemented", irFuncType.numResults);

	AbiState state;
	state.run(c, irFuncType);
	scope(exit) state.free(c);

	IrIndex hiddenParameter;
	if (state.abi.returnClass == PassClass.byPtrReg) {
		// pointer to return value is passed via hidden parameter, read it into virt reg
		IrIndex retType = c.types.appendPtr(state.type.resultTypes[0]);
		IrIndex paramReg = IrIndex(state.abi.returnLoc.regs[0], typeToRegSize(retType, c));
		ExtraInstrArgs extra = { type : retType };
		auto moveInstr = builder.emitInstr!(IrOpcode.move)(extra, paramReg);
		builder.prependBlockInstr(ir.entryBasicBlock, moveInstr.instruction);
		hiddenParameter = moveInstr.result;
	}

	void convParam(IrIndex instrIndex, ref IrInstrHeader instrHeader)
	{
		IrInstr_parameter* param = ir.get!IrInstr_parameter(instrIndex);
		uint paramIndex = param.index(ir);

		IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;

		PassClass paramClass = state.abi.paramClasses[paramIndex];
		final switch(paramClass) {
			case PassClass.byValueReg:
				IrIndex paramReg = IrIndex(state.abi.paramData[paramIndex].regs[0], typeToRegSize(type, c));
				ExtraInstrArgs extra = { result : instrHeader.result(ir) };
				auto moveInstr = builder.emitInstr!(IrOpcode.move)(extra, paramReg).instruction;
				replaceInstruction(ir, instrIndex, moveInstr);
				break;

			case PassClass.byValueRegMulti:
				PhysReg[2] paramRegs = state.abi.paramData[paramIndex].regs;
				IrIndex instr = receiveMultiValue(ir.nextInstr(instrIndex), paramRegs, instrHeader.result(ir), builder);
				replaceInstruction(ir, instrIndex, instr);
				break;

			case PassClass.byPtrReg:
				type = c.types.appendPtr(type);
				IrIndex paramReg = IrIndex(state.abi.paramData[paramIndex].regs[0], typeToRegSize(type, c));
				ExtraInstrArgs extra1 = { type : type };
				auto moveInstr = builder.emitInstr!(IrOpcode.move)(extra1, paramReg);
				replaceInstruction(ir, instrIndex, moveInstr.instruction);

				ExtraInstrArgs extra2 = { result : instrHeader.result(ir) };
				IrIndex loadInstr = builder.emitInstr!(IrOpcode.load_aggregate)(extra2, moveInstr.result).instruction;
				ir.getInstr(loadInstr).isUniqueLoad = true;
				builder.insertAfterInstr(moveInstr.instruction, loadInstr);
				break;

			case PassClass.byValueMemory:
				IrIndex slot = ir.backendData.stackLayout.addStackItem(c, type, c.types.typeSizeAndAlignment(type), StackSlotKind.parameter, cast(ushort)paramIndex);
				ir.backendData.stackLayout[slot].displacement = state.abi.paramData[paramIndex].stackOffset;

				// is directly in stack
				ExtraInstrArgs extra = { result : instrHeader.result(ir) };
				IrIndex loadInstr;
				if (type.isTypeArray || type.isTypeStruct) {
					// happens on sysv64
					loadInstr = builder.emitInstr!(IrOpcode.load_aggregate)(extra, slot).instruction;
				} else {
					extra.argSize = getTypeArgSize(type, c);
					loadInstr = builder.emitInstr!(IrOpcode.load)(extra, slot).instruction;
				}
				replaceInstruction(ir, instrIndex, loadInstr);
				break;

			case PassClass.byPtrMemory:
				// stack contains pointer to data
				type = c.types.appendPtr(type);
				IrIndex slot = ir.backendData.stackLayout.addStackItem(c, type, c.types.typeSizeAndAlignment(type), StackSlotKind.parameter, cast(ushort)paramIndex);
				ir.backendData.stackLayout[slot].displacement = state.abi.paramData[paramIndex].stackOffset;

				IrArgSize argSize = getTypeArgSize(type, c);
				ExtraInstrArgs extra = { argSize : argSize, type : type };
				InstrWithResult loadInstr = builder.emitInstr!(IrOpcode.load)(extra, slot);
				// remove parameter instruction
				replaceInstruction(ir, instrIndex, loadInstr.instruction);

				// load aggregate
				ExtraInstrArgs extra2 = { result : instrHeader.result(ir) };
				InstrWithResult loadInstr2 = builder.emitInstr!(IrOpcode.load_aggregate)(extra2, loadInstr.result);
				ir.getInstr(loadInstr2.instruction).isUniqueLoad = true;
				builder.insertAfterInstr(loadInstr.instruction, loadInstr2.instruction);
				break;

			case PassClass.ignore:
				// use zeroinited struct
				builder.redirectVregUsersTo(instrHeader.result(ir), c.constants.addZeroConstant(type));
				removeInstruction(ir, instrIndex);
				break;
		}
	}

	void convCall(IrIndex instrIndex, ref IrInstrHeader instrHeader)
	{
		ir.backendData.stackLayout.numCalls += 1;

		IrIndex callee = instrHeader.arg(ir, 0);
		IrIndex calleeTypeIndex = ir.getValueType(c, callee);
		if (calleeTypeIndex.isTypePointer)
			calleeTypeIndex = c.types.getPointerBaseType(calleeTypeIndex);
		IrTypeFunction* calleeType = &c.types.get!IrTypeFunction(calleeTypeIndex);

		AbiState callee_state;
		callee_state.run(c, calleeType);
		scope(exit) callee_state.free(c);

		CallConv* callConv = c.types.getCalleeCallConv(callee, ir, c);
		IrIndex[] args = instrHeader.args(ir)[1..$]; // exclude callee
		IrIndex originalResult;
		IrIndex hiddenPtr;
		bool hasHiddenPtr = false;

		// allocate stack slot for big return value
		if (callee_state.abi.returnClass == PassClass.byPtrReg)
		{
			IrIndex resType = callee_state.type.resultTypes[0];
			originalResult = instrHeader.result(ir); // we reuse result slot

			// reuse result slot of instruction as first argument
			instrHeader._payloadOffset -= 1;
			instrHeader.hasResult = false;
			instrHeader.numArgs += 1;

			args = instrHeader.args(ir)[1..$];
			// move callee in first arg
			instrHeader.arg(ir, 0) = callee;
			// place return arg slot in second arg
			hiddenPtr = ir.backendData.stackLayout.addStackItem(c, resType, c.types.typeSizeAndAlignment(resType), StackSlotKind.argument, 0);
			args[0] = hiddenPtr;
			hasHiddenPtr = true;
		}

		enum STACK_ITEM_SIZE = 8;
		size_t numArgs = args.length;
		// how many bytes are allocated on the stack before func call
		size_t stackReserve;
		if (callConv.hasShadowSpace)
		{
			stackReserve = 4 * STACK_ITEM_SIZE;
		}

		// Copy args to stack if necessary (big structs or run out of regs)
		foreach (size_t i; cast(size_t)hasHiddenPtr..args.length)
		{
			IrIndex arg = args[i];
			removeUser(c, ir, instrIndex, arg);

			PassClass paramClass = callee_state.abi.paramClasses[i-cast(size_t)hasHiddenPtr];
			IrIndex type = callee_state.type.parameterTypes[i-cast(size_t)hasHiddenPtr];
			final switch(paramClass) {
				case PassClass.byValueReg:
					args[i] = simplifyConstant(arg, c);
					break;
				case PassClass.byValueRegMulti: break;
				case PassClass.byPtrReg, PassClass.byPtrMemory:
					//allocate stack slot, store value there and use slot pointer as argument
					args[i] = ir.backendData.stackLayout.addStackItem(c, type, c.types.typeSizeAndAlignment(type), StackSlotKind.argument, 0);
					IrIndex instr = builder.emitInstr!(IrOpcode.store)(ExtraInstrArgs(), args[i], arg);
					builder.insertBeforeInstr(instrIndex, instr);
					break;
				case PassClass.byValueMemory:
					if (type.fitsIntoRegister(c)) args[i] = simplifyConstant(arg, c);
					break; // handled later
				case PassClass.ignore: break;
			}
		}

		// Stack layouting code makes sure that local data has 16 byte alignment if we have calls in IR.
		// align stack and push args that didn't fit into registers (register size args)
		if (callee_state.abi.stackSize > 0)
		{
			if (callee_state.abi.stackAlignment > 16) {
				c.unrecoverable_error(TokenIndex(), "Stack alignment of %s > 16 is not implemented", callee_state.abi.stackAlignment);
			}

			if (callee_state.abi.stackSize % callee_state.abi.stackAlignment != 0)
			{
				uint padding = paddingSize(callee_state.abi.stackSize, callee_state.abi.stackAlignment);
				// align stack to 16 bytes
				// TODO: SysV ABI needs 32byte alignment if __m256 is passed
				stackReserve += padding;
				IrIndex paddingSize = c.constants.add(padding, IsSigned.no);
				builder.emitInstrBefore!(IrOpcode.grow_stack)(instrIndex, ExtraInstrArgs(), paddingSize);
			}

			// choose stack ordering
			int start = cast(int)callee_state.abi.paramClasses.length-1;
			int end = 0;
			int inc = -1;

			if (callee_state.abi.reverseStackOrder) {
				swap(start, end);
				++end;
				inc = 1;
			}
			uint stackOffset = 0;
			// push args to stack
			for (int i = start; i != end; i += inc) {
				PassClass paramClass = callee_state.abi.paramClasses[i];
				IrIndex arg = args[i + cast(int)hasHiddenPtr];
				if (paramClass == PassClass.byValueMemory || paramClass == PassClass.byPtrMemory) {
					ParamLocation paramData = callee_state.abi.paramData[i];

					IrIndex type = ir.getValueType(c, arg);
					uint size = c.types.typeSize(type);
					//writefln("param %s %s %s %s", i, stackOffset, paramData.stackOffset, size);

					// push cannot be used with xmm registers. Convert those to grow_stack + store
					if (size <= 8 && !type.isTypeFloat) {
						auto pushInstr = builder.emitInstr!(IrOpcode.push)(ExtraInstrArgs(), arg);
						builder.insertBeforeInstr(instrIndex, pushInstr);
						stackOffset += 8;
					} else {
						// this must be multiple of 8
						uint allocSize = paramData.stackSize;
						if (allocSize > 0) {
							IrIndex paddingSize = c.constants.add(allocSize, IsSigned.no);
							builder.emitInstrBefore!(IrOpcode.grow_stack)(instrIndex, ExtraInstrArgs(), paddingSize);
						}

						IrIndex ptrType = c.types.appendPtr(type);
						ExtraInstrArgs extra = { type : ptrType };
						IrIndex ptr = builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra, IrIndex(amd64_reg.sp, ArgType.QWORD)).result;
						builder.emitInstrBefore!(IrOpcode.store)(instrIndex, ExtraInstrArgs(), ptr, arg);

						stackOffset += allocSize;
					}
				}
			}
			assert(stackOffset == callee_state.abi.stackSize);
			stackReserve += callee_state.abi.stackSize;
		}

		if (callee_state.abi.returnClass == PassClass.byPtrReg) {
			IrIndex type = ir.getValueType(c, args[0]);
			IrIndex argRegister = IrIndex(callee_state.abi.returnLoc.regs[0], typeToRegSize(type, c));
			ExtraInstrArgs extra = { result : argRegister };
			builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra, args[0]);
		}

		// move args to registers
		foreach(i, paramClass; callee_state.abi.paramClasses) {
			IrIndex arg = args[i + cast(int)hasHiddenPtr];
			ParamLocation paramData = callee_state.abi.paramData[i];

			IrIndex type = ir.getValueType(c, arg);
			final switch(paramClass) {
				case PassClass.byValueReg, PassClass.byPtrReg:
					IrIndex argRegister = IrIndex(paramData.regs[0], typeToRegSize(type, c));
					ExtraInstrArgs extra = { result : argRegister };
					builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra, arg);
					break;
				case PassClass.byValueRegMulti:
					IrIndex[2] vals = simplifyConstant128(instrIndex, arg, builder, c);

					IrIndex reg1 = IrIndex(paramData.regs[0], ArgType.QWORD);
					ExtraInstrArgs extra3 = { result : reg1 };
					builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra3, vals[0]);

					IrIndex reg2 = IrIndex(paramData.regs[1], ArgType.QWORD);
					ExtraInstrArgs extra4 = { result : reg2 };
					builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra4, vals[1]);
					break;
				case PassClass.byValueMemory: break; // handled below
				case PassClass.byPtrMemory: break; // handled below
				case PassClass.ignore:
					break;
			}
		}

		if (callConv.hasShadowSpace)
		{	// Allocate shadow space for 4 physical registers
			IrIndex const_32 = c.constants.add(32, IsSigned.no);
			auto growStackInstr = builder.emitInstr!(IrOpcode.grow_stack)(ExtraInstrArgs(), const_32);
			builder.insertBeforeInstr(instrIndex, growStackInstr);
			ir.getInstr(instrIndex).extendFixedArgRange = true;
		}

		// fix arguments
		scope(exit) {
			ubyte regsUsed = callee_state.abi.numRegistersUsed;

			void fillRegs(IrIndex[] instrArgs) {
				assert(instrArgs.length == regsUsed);
				uint nextIndex = 0;
				// order must be preserved, because liveness analisys relies on that
				foreach(i, PassClass paramClass; callee_state.abi.paramClasses) {
					final switch(paramClass) with(PassClass) {
						case byValueReg, byPtrReg:
							ParamLocation loc = callee_state.abi.paramData[i];
							// size is irrelevant here because register is only mentioned here to aid register allocation
							instrArgs[nextIndex] = IrIndex(loc.regs[0], ArgType.QWORD);
							++nextIndex;
							break;
						case byValueRegMulti:
							ParamLocation loc = callee_state.abi.paramData[i];
							// size is irrelevant here because register is only mentioned here to aid register allocation
							instrArgs[nextIndex] = IrIndex(loc.regs[0], ArgType.QWORD);
							instrArgs[nextIndex+1] = IrIndex(loc.regs[1], ArgType.QWORD);
							nextIndex += 2;
							break;
						case byValueMemory, byPtrMemory, ignore: break; // skip, non register param
					}
				}
			}

			if (regsUsed + 1 <= instrHeader.numArgs) {
				// reuse instruction
				instrHeader.numArgs = cast(ubyte)(regsUsed + 1); // include callee

				fillRegs(instrHeader.args(ir)[1..$]); // fill with regs

				if (callee_state.abi.useSyscall) {
					instrHeader.op = IrOpcode.syscall;
					IrIndex syscallRegister = IrIndex(callee_state.abi.syscallRegister, ArgType.DWORD);
					ExtraInstrArgs extra = { result : syscallRegister };
					builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra, c.constants.add(callee_state.type.syscallNumber, IsSigned.no));
					// We leave callee here, so that liveness analysis can get calling convention
				}
			} else {
				// make bigger instruction
				ExtraInstrArgs extra = {
					extraArgSlots : regsUsed
				};
				if (instrHeader.hasResult)
					extra.result = instrHeader.result(ir);
				IrIndex newCallInstr;
				assert(!callee_state.abi.useSyscall); // Syscalls are not allowed to introduce extra parameters
				newCallInstr = builder.emitInstr!(IrOpcode.call)(extra, instrHeader.arg(ir, 0)).instruction;
				IrInstrHeader* callHeader = ir.getInstr(newCallInstr);

				fillRegs(callHeader.args(ir)[1..$]); // fill with regs

				replaceInstruction(ir, instrIndex, newCallInstr);
			}
		}

		{
			// If function is noreturn we don't need to insert cleanup code
			if (calleeType.numResults == 1)
			{
				IrIndex resType = callee_state.type.resultTypes[0];
				if (resType.isTypeNoreturn) return;
			}

			// Instructions will be added after this one
			IrIndex lastInstr = instrIndex;

			// Deallocate stack after call
			if (stackReserve > 0)
			{
				IrIndex conReservedBytes = c.constants.add(stackReserve, IsSigned.no);
				auto shrinkStackInstr = builder.emitInstr!(IrOpcode.shrink_stack)(ExtraInstrArgs(), conReservedBytes);
				builder.insertAfterInstr(lastInstr, shrinkStackInstr);
				lastInstr = shrinkStackInstr; // insert next instr after this one
				ir.getInstr(instrIndex).extendFixedResultRange = true;
			}

			// for calls that return in register
			final switch (callee_state.abi.returnClass) {
				case PassClass.byValueReg:
					// mov result to virt reg
					IrIndex returnReg = IrIndex(callee_state.abi.returnLoc.regs[0], typeToIrArgSize(ir.getVirtReg(instrHeader.result(ir)).type, c));
					ExtraInstrArgs extra = { result : instrHeader.result(ir) };
					auto moveInstr = builder.emitInstr!(IrOpcode.move)(extra, returnReg).instruction;
					builder.insertAfterInstr(lastInstr, moveInstr);
					instrHeader.result(ir) = returnReg;
					break;
				case PassClass.byValueRegMulti:
					PhysReg[2] retRegs = callee_state.abi.returnLoc.regs;
					IrIndex instr = receiveMultiValue(ir.nextInstr(instrIndex), retRegs, instrHeader.result(ir), builder);
					builder.insertAfterInstr(lastInstr, instr);
					instrHeader.result(ir) = IrIndex(retRegs[0], ArgType.QWORD); // TODO: need to put both registers as result
					break;
				case PassClass.byPtrReg:
					ExtraInstrArgs extra = { result : originalResult };
					IrIndex loadInstr = builder.emitInstr!(IrOpcode.load_aggregate)(extra, hiddenPtr).instruction;
					ir.getInstr(loadInstr).isUniqueLoad = true;
					builder.insertAfterInstr(lastInstr, loadInstr);
					break;
				case PassClass.byPtrMemory:
				case PassClass.byValueMemory:
					c.internal_error("invalid return class", callee_state.abi.returnClass);
					break;
				case PassClass.ignore: break; // no result, or empty struct
			}
		}
	}

	void convReturn(IrIndex instrIndex, ref IrInstrHeader instrHeader)
	{
		// rewrite ret_val as ret in-place
		instrHeader.op = IrOpcode.ret;

		removeUser(c, ir, instrIndex, instrHeader.arg(ir, 0));
		PhysReg[2] resRegs = state.abi.returnLoc.regs;

		final switch (state.abi.returnClass)
		{
			case PassClass.byPtrReg:
				// store struct into pointer, then return pointer
				IrIndex value = instrHeader.arg(ir, 0);
				IrIndex instr = builder.emitInstr!(IrOpcode.store)(ExtraInstrArgs(), hiddenParameter, value);
				builder.insertBeforeInstr(instrIndex, instr);
				IrIndex result = IrIndex(resRegs[1], ArgType.QWORD); // we store return register in second slot
				ExtraInstrArgs extra = { result : result };
				IrIndex copyInstr = builder.emitInstr!(IrOpcode.move)(extra, hiddenParameter).instruction;
				builder.insertBeforeInstr(instrIndex, copyInstr);
				break;
			case PassClass.byValueReg:
				IrIndex value = simplifyConstant(instrHeader.arg(ir, 0), c);
				IrIndex type = irFuncType.resultTypes[0];
				IrIndex result = IrIndex(resRegs[0], typeToRegSize(type, c));
				ExtraInstrArgs extra = { result : result };
				IrIndex copyInstr = builder.emitInstr!(IrOpcode.move)(extra, value).instruction;
				builder.insertBeforeInstr(instrIndex, copyInstr);
				break;
			case PassClass.byValueRegMulti:
				IrIndex[2] vals = simplifyConstant128(instrIndex, instrHeader.arg(ir, 0), builder, c);

				IrIndex result1 = IrIndex(resRegs[0], ArgType.QWORD);
				ExtraInstrArgs extra3 = { result : result1 };
				builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra3, vals[0]);

				IrIndex result2 = IrIndex(resRegs[1], ArgType.QWORD);
				ExtraInstrArgs extra4 = { result : result2 };
				builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra4, vals[1]);
				break;
			case PassClass.byValueMemory, PassClass.byPtrMemory:
				c.internal_error("Invalid return class %s", state.abi.returnClass);
				break;
			case PassClass.ignore:
				break;
		}
		// rewrite ret_val as ret in-place
		instrHeader.op = IrOpcode.ret;
		instrHeader.numArgs = 0;
	}

	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(ir))
		{
			switch(instrHeader.op)
			{
				case IrOpcode.parameter: convParam(instrIndex, instrHeader); break;
				case IrOpcode.call: convCall(instrIndex, instrHeader); break;
				case IrOpcode.ret_val: convReturn(instrIndex, instrHeader); break;
				default: break;
			}
		}
	}
}

// glue 2 registers into aggregate
IrIndex receiveMultiValue(IrIndex beforeInstr, PhysReg[2] regs, IrIndex result, ref IrBuilder builder) {
	IrIndex type = builder.ir.getVirtReg(result).type;
	IrIndex reg1 = IrIndex(regs[0], ArgType.QWORD);
	auto sizealign = builder.context.types.typeSizeAndAlignment(type);
	IrIndex reg2 = IrIndex(regs[1], ArgType.QWORD);

	ExtraInstrArgs extra1 = { type : makeBasicTypeIndex(IrValueType.i64) };
	auto move1 = builder.emitInstr!(IrOpcode.move)(extra1, reg1);
	builder.insertBeforeInstr(beforeInstr, move1.instruction);

	ExtraInstrArgs extra2 = { type : makeBasicTypeIndex(IrValueType.i64) };
	auto move2 = builder.emitInstr!(IrOpcode.move)(extra2, reg2);
	builder.insertBeforeInstr(beforeInstr, move2.instruction);

	// store both regs into stack slot, then load aggregate
	IrIndex slot = builder.ir.backendData.stackLayout.addStackItem(builder.context, type, SizeAndAlignment(16, sizealign.alignmentPower), StackSlotKind.local, 0);

	IrIndex addr1 = genAddressOffset(slot, 0, builder.context.i64PtrType, beforeInstr, builder);
	IrIndex store1 = builder.emitInstr!(IrOpcode.store)(ExtraInstrArgs(), addr1, move1.result);
	builder.insertBeforeInstr(beforeInstr, store1);

	IrIndex addr2 = genAddressOffset(slot, 8, builder.context.i64PtrType, beforeInstr, builder);
	IrIndex store2 = builder.emitInstr!(IrOpcode.store)(ExtraInstrArgs(), addr2, move2.result);
	builder.insertBeforeInstr(beforeInstr, store2);

	ExtraInstrArgs extra3 = { result : result };
	IrIndex loadInstr = builder.emitInstr!(IrOpcode.load_aggregate)(extra3, slot).instruction;
	builder.ir.getInstr(loadInstr).isUniqueLoad = true;
	return loadInstr;
}

// For sysv64 ABI
// Given aggregate constant of (size > 8 && size <= 16) with all members aligned, produces 2 64bit values
IrIndex[2] simplifyConstant128(IrIndex insertBefore, IrIndex value, ref IrBuilder builder, CompilationContext* c)
{
	IrIndex[2] vals;
	if (value.isConstantZero) {
		vals[] = c.constants.ZERO;
	} else if (value.isConstantAggregate) {
		IrAggregateConstant* con = &c.constants.getAggregate(value);
		union Repr {
			ubyte[16] buf;
			ulong[2] items;
		}
		Repr repr;
		void onGlobal(ubyte[] subbuffer, IrIndex index, CompilationContext* c) {
			if (subbuffer.ptr == repr.buf.ptr) {
				vals[0] = index;
			} else {
				assert(subbuffer.ptr == repr.buf.ptr + 8);
				vals[1] = index;
			}
		}
		constantToMem(repr.buf[], value, c, &onGlobal);
		if (vals[0].isUndefined) vals[0] = c.constants.add(repr.items[0], IsSigned.no, IrArgSize.size64);
		if (vals[1].isUndefined) vals[1] = c.constants.add(repr.items[1], IsSigned.no, IrArgSize.size64);
	} else {
		ExtraInstrArgs extra1 = { type : makeBasicTypeIndex(IrValueType.i64) };
		vals[0] = builder.emitInstrBefore!(IrOpcode.get_aggregate_slice)(insertBefore, extra1, value, c.constants.ZERO).result;

		ExtraInstrArgs extra2 = { type : makeBasicTypeIndex(IrValueType.i64) };
		vals[1] = builder.emitInstrBefore!(IrOpcode.get_aggregate_slice)(insertBefore, extra2, value, c.constants.add(8, IsSigned.no)).result;
	}
	return vals;
}
