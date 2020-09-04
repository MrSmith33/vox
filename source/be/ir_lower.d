/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module be.ir_lower;

import std.bitmanip : bitfields;
import std.stdio;
import all;


void pass_ir_lower(CompilationContext* c, ModuleDeclNode* mod, FunctionDeclNode* func)
{
	IrFunction* optimizedIrData = c.getAst!IrFunction(func.backendData.optimizedIrData);
	func.backendData.loweredIrData = c.appendAst!IrFunction;
	IrFunction* loweredIrData = c.getAst!IrFunction(func.backendData.loweredIrData);
	*loweredIrData = *optimizedIrData; // copy

	IrBuilder builder;
	builder.beginDup(loweredIrData, c);
	IrIndex funcIndex = func.getIrIndex(c);

	void doPass(FuncPassIr pass) {
		pass(c, loweredIrData, funcIndex, builder);
		if (c.validateIr)
			validateIrFunction(c, loweredIrData);
		if (c.printIrLowerEach && c.printDumpOf(func)) dumpFunction(c, loweredIrData, "IR lowering each");
	}

	doPass(&func_pass_lower_abi);
	doPass(&func_pass_lower_aggregates);
	doPass(&func_pass_lower_gep);

	if (!c.printIrLowerEach && c.printIrLower && c.printDumpOf(func)) dumpFunction(c, loweredIrData, "IR lowering all");
	builder.finalizeIr;
}

bool fitsIntoRegister(IrIndex type, CompilationContext* c) {
	if (type.isTypeStruct) {
		IrTypeStruct* structRes = &c.types.get!IrTypeStruct(type);
		switch(structRes.size) {
			case 1: return true;
			case 2: return true;
			case 4: return true;
			case 8: return true;
			default: return false;
		}
	}
	return true;
}

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

enum MAX_ARG_CLASSES = 2;

struct AbiState {
	FunctionAbi abi;

	IrIndex[] gprRegs;
	IrIndex[] retRegs;
	IrTypeFunction* type;

	ubyte gprRemaining;
	ubyte sseRemaining;

	ubyte gprRegistersUsed() {
		return cast(ubyte)(gprRegs.length - gprRemaining);
	}

	ArgClassifier classify;
	uint[] paramClassBuf;

	void init(CompilationContext* c, IrTypeFunction* type) {
		this.type = type;
		auto cc = callConventions[type.callConv];
		abi.stackAlignment = cc.minStackAlignment;
		paramClassBuf = c.allocateTempArray!uint(alignValue(type.numParameters, 4) / 4);
		PassClass[] paramClasses = (cast(PassClass[])paramClassBuf)[0..type.numParameters];
		abi.paramClasses = paramClasses[0..type.numParameters];
		abi.paramData = c.allocateTempArray!ParamLocation(type.numParameters);

		gprRegs = cc.paramsInRegs;
		gprRemaining = cast(ubyte)cc.paramsInRegs.length;
		classify = classify_abis[type.callConv];

		calc_stack(c);
	}

	void free(CompilationContext* c) {
		c.freeTempArray(abi.paramData);
		c.freeTempArray(paramClassBuf);
	}

	void calc_stack(CompilationContext* c) {
		classify(c, this);

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
				assignToMem(abi.paramData[i], SizeAndAlignment(8, 8));
			}
		}
		abi.stackSize = alignValue!uint(abi.stackSize, MIN_STACK_SLOT_SIZE);
	}
}

union ParamLocation {
	IrIndex[MAX_ARG_CLASSES] regs;
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
}

// classification callback will classify the argument and immediately try to assign register if available
// If there is no free registers left, it will reclassify the argument to be passed via memory.
void win64_classify(CompilationContext* c, ref AbiState state)
{
	state.abi.returnClass = PassClass.ignore;
	if (state.type.numResults == 1) {
		IrIndex resType = state.type.resultTypes[0];
		if (resType.fitsIntoRegister(c)) {
			state.abi.returnClass = PassClass.byValueReg;
			state.abi.returnLoc.regs[0] = amd64_reg.ax;
		} else {
			state.abi.returnClass = PassClass.byPtrReg;
			state.abi.returnLoc.regs[0] = state.gprRegs[0];
			// TODO: also reduce number of sse regs
			--state.gprRemaining;
		}
	}

	foreach(uint i; 0..cast(uint)state.type.numParameters) {
		IrIndex paramType = state.type.parameterTypes[i];
		PassClass paramClass;
		if (fitsIntoRegister(paramType, c)) {
			if (state.gprRemaining) {
				state.abi.paramClasses[i] = PassClass.byValueReg;
				state.abi.paramData[i].regs[0] = state.gprRegs[$-state.gprRemaining];
				// TODO: also reduce number of sse regs
				--state.gprRemaining;
			} else {
				state.abi.paramClasses[i] = PassClass.byValueMemory;
			}
		} else {
			if (state.gprRemaining) {
				state.abi.paramClasses[i] = PassClass.byPtrReg;
				state.abi.paramData[i].regs[0] = state.gprRegs[$-state.gprRemaining];
				// TODO: also reduce number of sse regs
				--state.gprRemaining;
			} else {
				state.abi.paramClasses[i] = PassClass.byPtrMemory;
			}
		}

		//writefln("param %s %s", i, state.abi.paramClasses[i]);
	}
}

Sysv_AbiParamClass classify_param(CompilationContext* c, IrIndex paramType) {
	assert(paramType.isType);

	final switch (paramType.typeKind) with(IrTypeKind) {
		case basic:
			final switch(paramType.basicType) with(IrValueType) {
				case noreturn_t: return Sysv_AbiParamClass(PassClass.ignore, AbiClass.no_class);
				case void_t: return Sysv_AbiParamClass(PassClass.ignore, AbiClass.no_class);
				case i8: return Sysv_AbiParamClass(PassClass.byValueReg, AbiClass.integer);
				case i16: return Sysv_AbiParamClass(PassClass.byValueReg, AbiClass.integer);
				case i32: return Sysv_AbiParamClass(PassClass.byValueReg, AbiClass.integer);
				case i64: return Sysv_AbiParamClass(PassClass.byValueReg, AbiClass.integer);
				// floats are SSE class, TODO
			}
		case pointer:
		case func_t:
			return Sysv_AbiParamClass(PassClass.byValueReg, AbiClass.integer);
		case array:
		case struct_t:
			uint size = c.types.typeSize(paramType);
			// until we have support for __m256 and __m512, 16 bytes is max size
			if (size == 0) return Sysv_AbiParamClass(PassClass.ignore, AbiClass.no_class);
			if (size > 16) return Sysv_AbiParamClass(PassClass.byValueMemory, AbiClass.memory);
			if (size <= 8) return Sysv_AbiParamClass(PassClass.byValueReg, AbiClass.integer);
			return Sysv_AbiParamClass(PassClass.byValueRegMulti, AbiClass.integer, AbiClass.integer);
	}
}

void sysv64_classify(CompilationContext* c, ref AbiState state)
{
	state.abi.returnClass = PassClass.ignore;
	if (state.type.numResults == 1) {
		IrIndex resType = state.type.resultTypes[0];
		Sysv_AbiParamClass resClass = classify_param(c, resType);
		state.abi.returnClass = resClass.passClass;
		if (resClass.low == AbiClass.integer) {
			state.abi.returnLoc.regs[0] = amd64_reg.ax;
			if (resClass.high == AbiClass.integer)
				state.abi.returnLoc.regs[1] = amd64_reg.dx;
		} else if (resClass.passClass == PassClass.byValueMemory) {
			state.abi.returnClass = PassClass.byPtrReg;
			state.abi.returnLoc.regs[0] = state.gprRegs[0];
			--state.gprRemaining;
		}
		//writefln("res %s", state.abi.returnClass);
	}

	// assign register or fallback to memory class
	foreach(uint i; 0..cast(uint)state.type.numParameters) {
		IrIndex paramType = state.type.parameterTypes[i];
		Sysv_AbiParamClass paramClass_sysv = classify_param(c, paramType);
		state.abi.paramClasses[i] = paramClass_sysv.passClass;
		switch(paramClass_sysv.low)
		{
			case AbiClass.integer:
				if (state.gprRemaining < paramClass_sysv.len) {
					state.abi.paramClasses[i] = PassClass.byValueMemory;
					goto case AbiClass.memory;
				}

				// assign regs
				foreach(uint j; 0..paramClass_sysv.len) {
					assert(state.gprRemaining);
					state.abi.paramData[i].regs[j] = state.gprRegs[$-state.gprRemaining];
					--state.gprRemaining;
				}
				break;

			case AbiClass.memory: break;
			case AbiClass.no_class: break; // for empty structs
			default:
				c.internal_error("%s not implemented", paramClass_sysv.low);
		}
	}
}

alias ArgClassifier = void function(CompilationContext* c, ref AbiState state);
__gshared ArgClassifier[] classify_abis = [
	&win64_classify,
	&sysv64_classify
];

// Handle ABI
void func_pass_lower_abi(CompilationContext* c, IrFunction* ir, IrIndex funcIndex, ref IrBuilder builder)
{
	//writefln("lower_abi %s %s", builder.context.idString(ir.backendData.name), ir.getCallConvEnum(c));
	IrTypeFunction* irFuncType = &c.types.get!IrTypeFunction(ir.type);
	c.assertf(irFuncType.numResults <= 1, "%s results is not implemented", irFuncType.numResults);

	AbiState state;
	state.init(c, irFuncType);
	scope(exit) state.free(c);

	IrIndex hiddenParameter;
	if (state.abi.returnClass == PassClass.byPtrReg) {
		// pointer to return value is passed via hidden parameter, read it into virt reg
		IrIndex retType = c.types.appendPtr(state.type.resultTypes[0]);
		IrIndex paramReg = state.abi.returnLoc.regs[0];

		paramReg.physRegSize = typeToRegSize(retType, c);
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
				IrIndex[2] paramRegs = state.abi.paramData[paramIndex].regs;
				paramRegs[0].physRegSize = typeToRegSize(type, c);
				ExtraInstrArgs extra = { result : instrHeader.result(ir) };
				auto moveInstr = builder.emitInstr!(IrOpcode.move)(extra, paramRegs[0]).instruction;
				replaceInstruction(ir, instrIndex, moveInstr);
				break;

			case PassClass.byValueRegMulti:
				IrIndex[2] paramRegs = state.abi.paramData[paramIndex].regs;
				IrIndex instr = receiveMultiValue(ir.nextInstr(instrIndex), paramRegs, instrHeader.result(ir), builder);
				replaceInstruction(ir, instrIndex, instr);
				break;

			case PassClass.byPtrReg:
				type = c.types.appendPtr(type);
				IrIndex[2] paramRegs = state.abi.paramData[paramIndex].regs;
				paramRegs[0].physRegSize = typeToRegSize(type, c);
				ExtraInstrArgs extra1 = { type : type };
				auto moveInstr = builder.emitInstr!(IrOpcode.move)(extra1, paramRegs[0]);
				replaceInstruction(ir, instrIndex, moveInstr.instruction);

				ExtraInstrArgs extra2 = { result : instrHeader.result(ir) };
				IrIndex loadInstr = builder.emitInstr!(IrOpcode.load_aggregate)(extra2, moveInstr.result).instruction;
				ir.getInstr(loadInstr).isUniqueLoad = true;
				builder.insertAfterInstr(moveInstr.instruction, loadInstr);
				break;

			case PassClass.byValueMemory:
				IrIndex slot = ir.backendData.stackLayout.addStackItem(c, type, StackSlotKind.parameter, cast(ushort)paramIndex);
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
				IrIndex slot = ir.backendData.stackLayout.addStackItem(c, type, StackSlotKind.parameter, cast(ushort)paramIndex);
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
		callee_state.init(c, calleeType);
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
			hiddenPtr = ir.backendData.stackLayout.addStackItem(c, resType, StackSlotKind.argument, 0);
			args[0] = hiddenPtr;
			hasHiddenPtr = true;
		}

		enum STACK_ITEM_SIZE = 8;
		size_t numArgs = args.length;
		size_t numParamsInRegs = callConv.paramsInRegs.length;
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

			PassClass paramClass = callee_state.abi.paramClasses[i];
			final switch(paramClass) {
				case PassClass.byValueReg: break;
				case PassClass.byValueRegMulti: break;
				case PassClass.byPtrReg, PassClass.byPtrMemory:
					//allocate stack slot, store value there and use slot pointer as argument
					IrIndex type = callee_state.type.parameterTypes[i-cast(size_t)hasHiddenPtr];
					args[i] = ir.backendData.stackLayout.addStackItem(c, type, StackSlotKind.argument, 0);
					IrIndex instr = builder.emitInstr!(IrOpcode.store)(ExtraInstrArgs(), args[i], arg);
					builder.insertBeforeInstr(instrIndex, instr);
					break;
				case PassClass.byValueMemory: break; // handled later
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
				if (paramClass == PassClass.byValueMemory || paramClass == PassClass.byPtrMemory) {
					ParamLocation paramData = callee_state.abi.paramData[i];

					IrIndex type = ir.getValueType(c, args[i]);
					uint size = c.types.typeSize(type);
					//writefln("param %s %s %s %s", i, stackOffset, paramData.stackOffset, size);

					if (size <= 8) {
						auto pushInstr = builder.emitInstr!(IrOpcode.push)(ExtraInstrArgs(), args[i]);
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
						IrIndex ptr = builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra, amd64_reg.sp).result;
						builder.emitInstrBefore!(IrOpcode.store)(instrIndex, ExtraInstrArgs(), ptr, args[i]);

						stackOffset += allocSize;
					}
				}
			}
			assert(stackOffset == callee_state.abi.stackSize);
			stackReserve += callee_state.abi.stackSize;
		}

		// move args to registers
		foreach(i, paramClass; callee_state.abi.paramClasses) {
			IrIndex arg = args[i];
			IrIndex type = ir.getValueType(c, arg);
			final switch(paramClass) {
				case PassClass.byValueReg:
					ParamLocation paramData = callee_state.abi.paramData[i];
					IrIndex value = simplifyConstant(arg, c);
					IrIndex argRegister = paramData.regs[0];
					argRegister.physRegSize = typeToRegSize(type, c);
					ExtraInstrArgs extra = { result : argRegister };
					builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra, value);
					break;
				case PassClass.byValueRegMulti:
					ParamLocation paramData = callee_state.abi.paramData[i];
					IrIndex[2] vals = simplifyConstant128(instrIndex, arg, builder, c);

					IrIndex reg1 = paramData.regs[0];
					reg1.physRegSize = IrArgSize.size64;
					ExtraInstrArgs extra3 = { result : reg1 };
					builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra3, vals[0]);

					IrIndex reg2 = paramData.regs[1];
					reg2.physRegSize = IrArgSize.size64;
					ExtraInstrArgs extra4 = { result : reg2 };
					builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra4, vals[1]);
					break;
				case PassClass.byPtrReg:
					//allocate stack slot, store value there and use slot pointer as argument
					args[i] = ir.backendData.stackLayout.addStackItem(c, type, StackSlotKind.argument, 0);
					IrIndex instr = builder.emitInstr!(IrOpcode.store)(ExtraInstrArgs(), args[i], arg);
					builder.insertBeforeInstr(instrIndex, instr);
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
			ubyte regsUsed = callee_state.gprRegistersUsed;
			if (regsUsed + 1 <= instrHeader.numArgs) {
				// reuse instruction
				instrHeader.numArgs = cast(ubyte)(regsUsed + 1); // include callee
				instrHeader.args(ir)[1..$] = callee_state.gprRegs[0..regsUsed]; // fill with regs
			} else {
				// make bigger instruction
				ExtraInstrArgs extra = {
					extraArgSlots : regsUsed
				};
				if (instrHeader.hasResult)
					extra.result = instrHeader.result(ir);
				auto newCallInstr = builder.emitInstr!(IrOpcode.call)(extra, instrHeader.arg(ir, 0)).instruction;
				IrInstrHeader* callHeader = ir.getInstr(newCallInstr);
				callHeader.args(ir)[1..$] = callee_state.gprRegs[0..regsUsed];
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
					IrIndex returnReg = callee_state.abi.returnLoc.regs[0];
					returnReg.physRegSize = typeToIrArgSize(ir.getVirtReg(instrHeader.result(ir)).type, c);
					ExtraInstrArgs extra = { result : instrHeader.result(ir) };
					auto moveInstr = builder.emitInstr!(IrOpcode.move)(extra, returnReg).instruction;
					builder.insertAfterInstr(lastInstr, moveInstr);
					instrHeader.result(ir) = returnReg;
					break;
				case PassClass.byValueRegMulti:
					IrIndex[2] paramRegs = state.abi.returnLoc.regs;
					IrIndex instr = receiveMultiValue(ir.nextInstr(instrIndex), paramRegs, instrHeader.result(ir), builder);
					builder.insertAfterInstr(lastInstr, instr);
					instrHeader.result(ir) = paramRegs[0];
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
		IrIndex[2] resRegs = state.abi.returnLoc.regs;

		final switch (state.abi.returnClass)
		{
			case PassClass.byPtrReg:
				// store struct into pointer, then return pointer
				IrIndex value = instrHeader.arg(ir, 0);
				IrIndex instr = builder.emitInstr!(IrOpcode.store)(ExtraInstrArgs(), hiddenParameter, value);
				builder.insertBeforeInstr(instrIndex, instr);
				IrIndex result = resRegs[0];
				ExtraInstrArgs extra = { result : result };
				IrIndex copyInstr = builder.emitInstr!(IrOpcode.move)(extra, hiddenParameter).instruction;
				builder.insertBeforeInstr(instrIndex, copyInstr);
				break;
			case PassClass.byValueReg:
				IrIndex value = simplifyConstant(instrHeader.arg(ir, 0), c);
				IrIndex result = resRegs[0];
				IrIndex type = irFuncType.resultTypes[0];
				result.physRegSize = typeToRegSize(type, c);
				ExtraInstrArgs extra = { result : result };
				IrIndex copyInstr = builder.emitInstr!(IrOpcode.move)(extra, value).instruction;
				builder.insertBeforeInstr(instrIndex, copyInstr);
				break;
			case PassClass.byValueRegMulti:
				IrIndex[2] vals = simplifyConstant128(instrIndex, instrHeader.arg(ir, 0), builder, c);

				IrIndex result1 = resRegs[0];
				result1.physRegSize = IrArgSize.size64;
				ExtraInstrArgs extra3 = { result : result1 };
				builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra3, vals[0]);

				IrIndex result2 = resRegs[1];
				result2.physRegSize = IrArgSize.size64;
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

/// Converts complex constants fitting in a single register into an integer constant
IrIndex simplifyConstant(IrIndex index, CompilationContext* c)
{
	union U {
		ulong bufferValue;
		ubyte[8] buffer;
	}
	U data;
	uint typeSize;
	if (index.isConstantZero)
	{
		typeSize = c.types.typeSize(index.constantZeroType);
	}
	else if (index.isConstantAggregate)
	{
		IrAggregateConstant* con = &c.constants.getAggregate(index);
		typeSize = c.types.typeSize(con.type);
	}
	else
	{
		return index;
	}

	constantToMem(data.buffer[0..typeSize], index, c);
	return c.constants.add(data.bufferValue, IsSigned.no, sizeToIrArgSize(typeSize, c));
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

// glue 2 registers into aggregate
IrIndex receiveMultiValue(IrIndex beforeInstr, IrIndex[2] regs, IrIndex result, ref IrBuilder builder) {
	IrIndex type = builder.ir.getVirtReg(result).type;
	regs[0].physRegSize = IrArgSize.size64;
	regs[1].physRegSize = IrArgSize.size64;

	ExtraInstrArgs extra1 = { type : makeBasicTypeIndex(IrValueType.i64) };
	auto move1 = builder.emitInstr!(IrOpcode.move)(extra1, regs[0]);
	builder.insertBeforeInstr(beforeInstr, move1.instruction);

	ExtraInstrArgs extra2 = { type : makeBasicTypeIndex(IrValueType.i64) };
	auto move2 = builder.emitInstr!(IrOpcode.move)(extra2, regs[1]);
	builder.insertBeforeInstr(beforeInstr, move2.instruction);

	// store both regs into stack slot, then load aggregate
	IrIndex slot = builder.ir.backendData.stackLayout.addStackItem(builder.context, type, StackSlotKind.local, 0);

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

IrIndex genAddressOffset(IrIndex ptr, uint offset, IrIndex ptrType, IrIndex beforeInstr, ref IrBuilder builder) {
	if (offset == 0) {
		ExtraInstrArgs extra = { type : ptrType };
		InstrWithResult movInstr = builder.emitInstrBefore!(IrOpcode.move)(beforeInstr, extra, ptr);
		return movInstr.result;
	} else {
		IrIndex offsetIndex = builder.context.constants.add(offset, IsSigned.no);
		ExtraInstrArgs extra = { type : ptrType };
		InstrWithResult addressInstr = builder.emitInstrBefore!(IrOpcode.add)(beforeInstr, extra, ptr, offsetIndex);
		return addressInstr.result;
	}
}

IrIndex genCopy(IrIndex dst, IrIndex src, IrIndex beforeInstr, ref IrBuilder builder) {
	if (src.isSomeConstant)
		return builder.emitInstrBefore!(IrOpcode.store)(beforeInstr, ExtraInstrArgs(), dst, src);
	else
		return builder.emitInstrBefore!(IrOpcode.copy)(beforeInstr, ExtraInstrArgs(), dst, src);
}

IrIndex genLoad(IrIndex ptr, uint offset, IrIndex ptrType, IrIndex beforeInstr, ref IrBuilder builder) {
	ptr = genAddressOffset(ptr, offset, ptrType, beforeInstr, builder);
	IrIndex valType = builder.context.types.getPointerBaseType(ptrType);
	IrArgSize argSize = typeToIrArgSize(valType, builder.context);
	ExtraInstrArgs extra = { type : valType, argSize : argSize };
	auto instr = builder.emitInstrBefore!(IrOpcode.load)(beforeInstr, extra, ptr);
	return instr.result;
}

struct LowerVreg
{
	IrIndex redirectTo;
}

void func_pass_lower_aggregates(CompilationContext* c, IrFunction* ir, IrIndex funcIndex, ref IrBuilder builder)
{
	//writefln("lower_aggregates %s", c.idString(ir.backendData.name));

	// buffer for call/instruction arguments
	enum MAX_ARGS = 255;
	IrIndex[MAX_ARGS] argBuffer = void;

	LowerVreg[] vregInfos = makeParallelArray!LowerVreg(c, ir.numVirtualRegisters);

	foreach (IrIndex vregIndex, ref IrVirtualRegister vreg; ir.virtualRegisters)
	{
		if (vreg.type.isTypeStruct || vreg.type.isTypeArray)
		{
			//writefln("- vreg %s", vregIndex);

			IrInstrHeader* definition = ir.getInstr(vreg.definition);
			if (definition.op == IrOpcode.load_aggregate)
			{
				// we can omit stack allocation and reuse source memory
				if (definition.isUniqueLoad)
				{
					vregInfos[vregIndex.storageUintIndex].redirectTo = definition.arg(ir, 0);
					removeInstruction(ir, vreg.definition);
				}
			}
		}
	}

	// transforms instructions
	// gathers all registers to be promoted to pointer
	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		foreach(IrIndex phiIndex, ref IrPhi phi; block.phis(ir))
		{
			IrIndex type = ir.getVirtReg(phi.result).type;
			if (!type.fitsIntoRegister(c)) {
				//writefln("- phi %s", phiIndex);
			}
			foreach(size_t arg_i, ref IrIndex phiArg; phi.args(ir))
			{
			}
		}

		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(ir))
		{
			switch(instrHeader.op)
			{
				case IrOpcode.store:
					IrIndex ptr = instrHeader.arg(ir, 0);
					IrIndex val = instrHeader.arg(ir, 1);
					if (ptr.isPhysReg || val.isPhysReg) break;

					//writefln("- store %s %s %s", instrIndex, ptr, val);
					IrIndex ptrType = ir.getValueType(c, ptr);
					IrIndex valType = ir.getValueType(c, val);

					// value will be replaced with pointer, replace store with copy
					if (!valType.fitsIntoRegister(c) && !val.isSomeConstant)
					{
						instrHeader.op = IrOpcode.copy;
					}
					break;

				case IrOpcode.load_aggregate:
					//writefln("- load_aggregate %s", instrIndex);
					IrIndex ptr = instrHeader.arg(ir, 0);
					IrIndex ptrType = ir.getValueType(c, ptr);
					IrIndex base = c.types.getPointerBaseType(ptrType);

					if (base.fitsIntoRegister(c))
					{
						IrArgSize argSize = typeToIrArgSize(base, c);
						ExtraInstrArgs extra = { result : instrHeader.result(ir), argSize : argSize };
						builder.emitInstrBefore!(IrOpcode.load)(instrIndex, extra, ptr);
					}
					else
					{
						IrIndex slot = ir.backendData.stackLayout.addStackItem(c, base, StackSlotKind.local, 0);
						genCopy(slot, instrHeader.arg(ir, 0), instrIndex, builder);

						vregInfos[instrHeader.result(ir).storageUintIndex].redirectTo = slot;
					}
					removeInstruction(ir, instrIndex);
					break;

				case IrOpcode.create_aggregate:
					//writefln("- create_aggregate %s", instrIndex);
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;

					if (!type.fitsIntoRegister(c)) {
						IrTypeStruct* structType = &c.types.get!IrTypeStruct(type);
						IrIndex slot = ir.backendData.stackLayout.addStackItem(c, type, StackSlotKind.local, 0);
						vregInfos[instrHeader.result(ir).storageUintIndex].redirectTo = slot;

						IrIndex[] members = instrHeader.args(ir);
						c.assertf(members.length == structType.numMembers, "%s != %s", members.length, structType.numMembers);

						foreach (i, IrTypeStructMember member; structType.members)
						{
							IrIndex ptrType = c.types.appendPtr(member.type);
							IrIndex ptr = genAddressOffset(slot, member.offset, ptrType, instrIndex, builder);
							if (member.type.fitsIntoRegister(c))
							{
								IrArgSize argSize = getTypeArgSize(member.type, c);
								ExtraInstrArgs extra = { argSize : argSize };
								builder.emitInstrBefore!(IrOpcode.store)(instrIndex, extra, ptr, members[i]);
							}
							else
							{
								builder.emitInstrBefore!(IrOpcode.copy)(instrIndex, ExtraInstrArgs(), ptr, members[i]);
							}
						}
						//convertAggregateVregToPointer(instrHeader.result(ir), ir, builder);
						removeInstruction(ir, instrIndex);
					}
					else
						createSmallAggregate(instrIndex, type, instrHeader, ir, builder);
					break;

				case IrOpcode.get_element:
					// if source is stored inside register - extract with bit manipulation, otherwise lower to GEP

					//writefln("- get_element %s", instrIndex);
					// instruction is reused

					IrIndex[] args = instrHeader.args(ir);
					IrIndex sourceType = getValueType(args[0], ir, c);
					IrTypeStructMember member = c.types.getAggregateMember(sourceType, c, args[1..$]);
					IrIndex resultType = member.type;
					uint resultSize = c.types.typeSize(resultType);

					if (sourceType.fitsIntoRegister(c))
					{
						// do simple variant where all indicies are constant
						IrIndex value = args[0];
						if (member.offset > 0)
						{
							// shift right
							IrIndex rightArg = c.constants.add(member.offset * 8, IsSigned.no);
							ExtraInstrArgs extra = { argSize : getTypeArgSize(sourceType, c), type : sourceType };
							value = builder.emitInstrBefore!(IrOpcode.lshr)(instrIndex, extra, value, rightArg).result;
						}

						// mask if not 1, 2, 4 or 8 bytes in size
						if (!resultType.fitsIntoRegister(c))
						{
							// and
							IrIndex mask = c.constants.add((1 << (resultSize * 8)) - 1, IsSigned.no);
							ExtraInstrArgs extra = { type : member.type };
							value = builder.emitInstrBefore!(IrOpcode.and)(instrIndex, extra, value, mask).result;
						}
						else
						{
							ExtraInstrArgs extra = { argSize : sizeToIrArgSize(resultSize, c), type : member.type };
							value = builder.emitInstrBefore!(IrOpcode.trunc)(instrIndex, extra, value).result;
						}

						vregInfos[instrHeader.result(ir).storageUintIndex].redirectTo = value;
						removeInstruction(ir, instrIndex);
						break;
					}

					// reuse the same indicies from get_element and perform GEP on them, then do load
					instrHeader.op = IrOpcode.get_element_ptr_0;

					if (resultType.fitsIntoRegister(c))
					{
						IrIndex loadResult = instrHeader.result(ir);
						IrIndex ptrType = c.types.appendPtr(resultType);
						IrIndex gepResult = builder.addVirtualRegister(instrIndex, ptrType);
						instrHeader.result(ir) = gepResult;

						ExtraInstrArgs extra2 = { argSize : getTypeArgSize(resultType, c), result : loadResult };
						IrIndex loadInstr = builder.emitInstr!(IrOpcode.load)(extra2, gepResult).instruction;
						builder.insertAfterInstr(instrIndex, loadInstr);
					}
					break;

				case IrOpcode.get_aggregate_slice:
					//writefln("- get_aggregate_slice %s", instrIndex);
					IrIndex[] args = instrHeader.args(ir);

					long indexVal = c.constants.get(args[1]).i64;
					IrIndex addr;
					if (indexVal == 0) {
						ExtraInstrArgs extra1 = { type : c.i64PtrType };
						InstrWithResult movInstr = builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra1, args[0]);
						addr = movInstr.result;
					} else {
						ExtraInstrArgs extra1 = { type : c.i64PtrType };
						InstrWithResult addressInstr = builder.emitInstrBefore!(IrOpcode.add)(instrIndex, extra1, args[0], args[1]);
						addr = addressInstr.result;
					}

					ExtraInstrArgs extra2 = { argSize : IrArgSize.size64, type : makeBasicTypeIndex(IrValueType.i64) };
					InstrWithResult loadInstr = builder.emitInstrBefore!(IrOpcode.load)(instrIndex, extra2, addr);

					instrHeader.numArgs = 1;
					instrHeader.op = IrOpcode.move;
					args[0] = loadInstr.result;
					builder.addUser(instrIndex, args[0]);
					break;
				case IrOpcode.insert_element:
					//writefln("- insert_element %s", instrIndex);
					break;

				case IrOpcode.branch_switch:
					// unroll switch into a chain of compare branches
					IrIndex[] args = instrHeader.args(ir);
					IrSmallArray successors = block.successors;
					block.successors = IrSmallArray.init;
					IrIndex[] succ = successors.data(ir);

					assert(args.length == succ.length);
					assert(args.length > 0);
					IrIndex value = args[0];
					IrIndex valueType = ir.getValueType(c, value);
					IrArgSize argSize = typeToIrArgSize(valueType, c);
					IrIndex defaultBlock = succ[0];

					// replace switch with branch to first case block
					ExtraInstrArgs extra = { cond : IrBinaryCondition.eq, argSize : argSize };
					IrIndex firstInstr = builder.emitInstr!(IrOpcode.branch_binary)(extra, value, args[1]);
					replaceInstruction(ir, instrIndex, firstInstr);
					block.successors.append(&builder, succ[1]);
					// predecessor is already correct for this block

					// build a chain
					IrIndex lastBlock = blockIndex;
					foreach(i; 2..args.length)
					{
						IrIndex branchBlockIndex = builder.addBasicBlock;
						IrBasicBlock* branchBlock = ir.getBlock(branchBlockIndex);
						branchBlock.isSealed = true;
						branchBlock.isFinished = true;

						builder.addBlockTarget(lastBlock, branchBlockIndex);
						ir.getBlock(succ[i]).predecessors[0, ir] = branchBlockIndex;
						branchBlock.successors.append(&builder, succ[i]);

						builder.emitInstr!(IrOpcode.branch_binary)(branchBlockIndex, extra, value, args[i]);
						moveBlockAfter(ir, branchBlockIndex, lastBlock);
						lastBlock = branchBlockIndex;
					}

					successors.free(ir);
					block.successors.append(&builder, succ[1]);

					ir.getBlock(lastBlock).successors.append(&builder, defaultBlock);
					ir.getBlock(defaultBlock).predecessors[0, ir] = lastBlock;
					break;

				default:
					//c.internal_error("IR lower unimplemented IR instr %s", cast(IrOpcode)instrHeader.op);
					break;
			}
		}
	}

	foreach(i, info; vregInfos)
	{
		if (info.redirectTo.isDefined)
		{
			builder.redirectVregUsersTo(IrIndex(cast(uint)i, IrValueKind.virtualRegister), info.redirectTo);
		}
	}
}

// pack values and constants into a register via `shift` and `binary or` instructions
void createSmallAggregate(IrIndex instrIndex, IrIndex type, ref IrInstrHeader instrHeader, IrFunction* ir, ref IrBuilder builder)
{
	CompilationContext* c = builder.context;

	uint targetTypeSize = c.types.typeSize(type);
	IrArgSize argSize = sizeToIrArgSize(targetTypeSize, c);
	c.assertf(instrHeader.numArgs <= 8, "too much args %s", instrHeader.numArgs);
	ulong constant = 0;
	// how many non-constants are prepared in argBuffer
	uint numBufferedValues = 0;

	IrIndex[2] argBuffer;

	void insertNonConstant(IrIndex value, uint bit_offset, uint size)
	{
		if (size < targetTypeSize) {
			ExtraInstrArgs extra = { argSize : argSize, type : type };
			switch(size) { // zero extend 8 and 16 bit args to 32bit
				case 1: value = builder.emitInstrBefore!(IrOpcode.zext)(instrIndex, extra, value).result; break;
				case 2: value = builder.emitInstrBefore!(IrOpcode.zext)(instrIndex, extra, value).result; break;
				default: break;
			}
		}

		// shift
		if (bit_offset == 0)
			argBuffer[numBufferedValues] = value;
		else
		{
			IrIndex rightArg = c.constants.add(bit_offset, IsSigned.no);
			ExtraInstrArgs extra1 = { argSize : argSize, type : type };
			IrIndex shiftRes = builder.emitInstrBefore!(IrOpcode.shl)(instrIndex, extra1, value, rightArg).result;
			argBuffer[numBufferedValues] = shiftRes;
		}
		++numBufferedValues;

		if (numBufferedValues == 2)
		{
			// or
			ExtraInstrArgs extra2 = { argSize : argSize, type : type };
			argBuffer[0] = builder.emitInstrBefore!(IrOpcode.or)(instrIndex, extra2, argBuffer[0], argBuffer[1]).result;
			numBufferedValues = 1;
		}
	}

	void insertAt(IrIndex value, uint offset, uint size)
	{
		if (value.isConstant) {
			constant |= c.constants.get(value).i64 << (offset * 8);
		} else {
			insertNonConstant(value, offset * 8, size);
		}
	}

	switch(type.typeKind) with(IrTypeKind) {
		case struct_t:
			IrTypeStruct* structType = &c.types.get!IrTypeStruct(type);
			IrIndex[] args = instrHeader.args(ir);
			foreach_reverse (i, IrTypeStructMember member; structType.members)
			{
				uint memberSize = c.types.typeSize(member.type);
				insertAt(args[i], member.offset, memberSize);
			}
			break;
		case array:
			IrTypeArray* arrayType = &c.types.get!IrTypeArray(type);
			uint elemSize = c.types.typeSize(arrayType.elemType);
			IrIndex[] args = instrHeader.args(ir);
			foreach_reverse (i; 0..arrayType.size)
			{
				insertAt(args[i], i * elemSize, elemSize);
			}
			break;
		default: assert(false);
	}

	IrIndex constIndex = c.constants.add(constant, IsSigned.no, argSize);
	IrIndex result;
	if (numBufferedValues == 1)
	{
		if (constant == 0)
		{
			result = argBuffer[0];
		}
		else
		{
			bool isBigConstant = c.constants.get(constIndex).payloadSize(constIndex) == IrArgSize.size64;

			if (isBigConstant)
			{
				// copy to temp register
				ExtraInstrArgs extra = { argSize : argSize, type : type };
				constIndex = builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra, constIndex).result;
			}

			ExtraInstrArgs extra3 = { argSize : argSize, type : type };
			result = builder.emitInstrBefore!(IrOpcode.or)(instrIndex, extra3, argBuffer[0], constIndex).result;
		}
	}
	else
	{
		result = constIndex;
	}
	builder.redirectVregUsersTo(instrHeader.result(ir), result);
	removeInstruction(ir, instrIndex);
}

void func_pass_lower_gep(CompilationContext* context, IrFunction* ir, IrIndex funcIndex, ref IrBuilder builder)
{
	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(ir))
		{
			switch(cast(IrOpcode)instrHeader.op) with(IrOpcode)
			{
				case get_element_ptr, get_element_ptr_0:
					lowerGEP(context, builder, instrIndex, instrHeader);
					break;
				default: break;
			}
		}
	}
}

// TODO some typecasts are needed for correct typing
void lowerGEP(CompilationContext* context, ref IrBuilder builder, IrIndex instrIndex, ref IrInstrHeader instrHeader)
{
	IrIndex buildOffset(IrIndex basePtr, long offsetVal, IrIndex resultType) {
		if (offsetVal == 0) {
			// Shortcut for 0-th index
			IrIndex basePtrType = getValueType(basePtr, builder.ir, context);
			// TODO: prefer proper typing for now, until IR lowering is implemented
			if (basePtrType == resultType) return basePtr;

			ExtraInstrArgs extra = { type : resultType };
			InstrWithResult instr = builder.emitInstr!(IrOpcode.conv)(extra, basePtr);
			builder.insertBeforeInstr(instrIndex, instr.instruction);
			return instr.result;
		} else {
			IrIndex offset = context.constants.add(offsetVal, IsSigned.yes);

			ExtraInstrArgs extra = { type : resultType };
			InstrWithResult addressInstr = builder.emitInstr!(IrOpcode.add)(extra, basePtr, offset);
			builder.insertBeforeInstr(instrIndex, addressInstr.instruction);

			return addressInstr.result;
		}
	}

	IrIndex buildIndex(IrIndex basePtr, IrIndex index, uint elemSize, IrIndex resultType)
	{
		IrIndex scale = context.constants.add(elemSize, IsSigned.no);
		IrIndex indexVal = index;

		if (elemSize > 1) {
			ExtraInstrArgs extra1 = { type : makeBasicTypeIndex(IrValueType.i64) };
			InstrWithResult offsetInstr = builder.emitInstr!(IrOpcode.umul)(extra1, index, scale);
			builder.insertBeforeInstr(instrIndex, offsetInstr.instruction);
			indexVal = offsetInstr.result;
		}

		ExtraInstrArgs extra2 = { type : resultType };
		InstrWithResult addressInstr = builder.emitInstr!(IrOpcode.add)(extra2, basePtr, indexVal);
		builder.insertBeforeInstr(instrIndex, addressInstr.instruction);

		return addressInstr.result;
	}

	IrIndex aggrPtr = instrHeader.arg(builder.ir, 0); // aggregate ptr
	IrIndex aggrPtrType = getValueType(aggrPtr, builder.ir, context);

	context.assertf(aggrPtrType.isTypePointer,
		"First argument to GEP instruction must be pointer, not %s", aggrPtr.typeKind);

	IrIndex aggrType = context.types.getPointerBaseType(aggrPtrType);
	uint aggrSize = context.types.typeSize(aggrType);

	IrIndex[] args;

	// get_element_ptr_0 first index is zero, hence no op
	if (cast(IrOpcode)instrHeader.op == IrOpcode.get_element_ptr)
	{
		IrIndex firstIndex = instrHeader.arg(builder.ir, 1);

		if (firstIndex.isSimpleConstant) {
			long indexVal = context.constants.get(firstIndex).i64;
			long offset = indexVal * aggrSize;
			aggrPtr = buildOffset(aggrPtr, offset, aggrPtrType);
		} else {
			aggrPtr = buildIndex(aggrPtr, firstIndex, aggrSize, aggrPtrType);
		}

		args = instrHeader.args(builder.ir)[2..$]; // 0 is ptr, 1 is first index
	}
	else
	{
		args = instrHeader.args(builder.ir)[1..$]; // 0 is ptr
	}

	foreach(IrIndex memberIndex; args)
	{
		final switch(aggrType.typeKind)
		{
			case IrTypeKind.basic:
				context.internal_error("Cannot index basic type %s", aggrType.typeKind);
				break;

			case IrTypeKind.pointer:
				context.internal_error("Cannot index pointer with GEP instruction, use load first");
				break;

			case IrTypeKind.array:
				IrIndex elemType = context.types.getArrayElementType(aggrType);
				IrIndex elemPtrType = context.types.appendPtr(elemType);
				uint elemSize = context.types.typeSize(elemType);

				if (memberIndex.isSimpleConstant) {
					long indexVal = context.constants.get(memberIndex).i64;
					long offset = indexVal * elemSize;
					aggrPtr = buildOffset(aggrPtr, offset, elemPtrType);
				} else {
					aggrPtr = buildIndex(aggrPtr, memberIndex, elemSize, elemPtrType);
				}

				aggrType = elemType;
				break;

			case IrTypeKind.struct_t:
				context.assertf(memberIndex.isSimpleConstant, "Structs can only be indexed with constants, not with %s", memberIndex);

				long memberIndexVal = context.constants.get(memberIndex).i64;
				IrTypeStructMember[] members = context.types.get!IrTypeStruct(aggrType).members;

				context.assertf(memberIndexVal < members.length,
					"Indexing member %s of %s-member struct",
					memberIndexVal, members.length);

				IrTypeStructMember member = members[memberIndexVal];
				IrIndex memberPtrType = context.types.appendPtr(member.type);

				aggrPtr = buildOffset(aggrPtr, member.offset, memberPtrType);
				aggrType = member.type;
				break;

			case IrTypeKind.func_t:
				context.internal_error("Cannot index function type");
				break;
		}
	}

	builder.redirectVregUsersTo(instrHeader.result(builder.ir), aggrPtr);
	removeInstruction(builder.ir, instrIndex);
}
