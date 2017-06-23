/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module amd64asm;

enum Register : ubyte {AX, CX, DX, BX, SP, BP, SI, DI, R8, R9, R10, R11, R12, R13, R14, R15}
enum RegisterMax  = cast(Register)(Register.max+1);

bool is_SP_or_R12(Register reg) { return (reg & 0b111) == 0b100; }
bool is_BP_or_R13(Register reg) { return (reg & 0b111) == 0b101; }

enum ArgType : ubyte { BYTE, WORD, DWORD, QWORD }

// ensures REX prefix for ah ch dh bh
bool regNeedsRexPrefix(ArgType argType)(Register reg) {
	static if (argType == ArgType.BYTE) return reg >= 4;
	else return false;
}

import std.string : format;
struct Imm8  { ubyte  value; enum argT = ArgType.BYTE;  string toString(){ return format("0X%02X", value); } }
struct Imm16 { ushort value; enum argT = ArgType.WORD;  string toString(){ return format("0X%02X", value); } }
struct Imm32 { uint   value; enum argT = ArgType.DWORD; string toString(){ return format("0X%02X", value); } }
struct Imm64 { ulong  value; enum argT = ArgType.QWORD; string toString(){ return format("0X%02X", value); } }
enum bool isAnyImm(I) = is(I == Imm64) || is(I == Imm32) || is(I == Imm16) || is(I == Imm8);


enum ubyte REX_PREFIX = 0b0100_0000;
enum ubyte REX_W      = 0b0000_1000;
enum ubyte REX_R      = 0b0000_0100;
enum ubyte REX_X      = 0b0000_0010;
enum ubyte REX_B      = 0b0000_0001;

enum LegacyPrefix : ubyte {
	// Prefix group 1
	LOCK = 0xF0, // LOCK prefix
	REPN = 0xF2, // REPNE/REPNZ prefix
	REP  = 0xF3, // REP or REPE/REPZ prefix
	// Prefix group 2
	CS = 0x2E, // CS segment override
	SS = 0x36, // SS segment override
	DS = 0x3E, // DS segment override
	ES = 0x26, // ES segment override
	FS = 0x64, // FS segment override
	GS = 0x65, // GS segment override
	BNT = 0x2E, // Branch not taken
	BT = 0x3E, // Branch taken
	// Prefix group 3
	OPERAND_SIZE = 0x66, // Operand-size override prefix
	// Prefix group 4
	ADDRESS_SIZE = 0x67, // Address-size override prefix
}

/// The terms "less" and "greater" are used for comparisons of signed integers.
/// The terms "above" and "below" are used for unsigned integers.
enum Condition : ubyte {
	O   = 0x0, /// overflow (OF=1).
	NO  = 0x1, /// not overflow (OF=0).
	B   = 0x2, /// below (CF=1).
	C   = 0x2, /// carry (CF=1).
	NAE = 0x2, /// not above or equal (CF=1).
	AE  = 0x3, /// above or equal (CF=0).
	NB  = 0x3, /// not below (CF=0).
	NC  = 0x3, /// not carry (CF=0).
	E   = 0x4, /// equal (ZF=1).
	Z   = 0x4, /// zero (ZF = 1).
	NE  = 0x5, /// not equal (ZF=0).
	NZ  = 0x5, /// not zero (ZF=0).
	BE  = 0x6, /// below or equal (CF=1 or ZF=1).
	NA  = 0x6, /// not above (CF=1 or ZF=1).
	A   = 0x7, /// above (CF=0 and ZF=0).
	NBE = 0x7, /// not below or equal (CF=0 andZF=0).
	S   = 0x8, /// sign (SF=1).
	NS  = 0x9, /// not sign (SF=0).
	P   = 0xA, /// parity (PF=1).
	PE  = 0xA, /// parity even (PF=1).
	NP  = 0xB, /// not parity (PF=0).
	PO  = 0xB, /// parity odd (PF=0).
	L   = 0xC, /// less (SF≠ OF).
	NGE = 0xC, /// not greater or equal (SF≠ OF).
	GE  = 0xD, /// greater or equal (SF=OF).
	NL  = 0xD, /// not less (SF=OF).
	LE  = 0xE, /// less or equal (ZF=1 or SF≠ OF).
	NG  = 0xE, /// not greater (ZF=1 or SF≠ OF).
	G   = 0xF, /// greater (ZF=0 and SF=OF).
	NLE = 0xF, /// not less or equal (ZF=0 andSF=OF).
}

// place 1 MSB of register into appropriate bit field of REX prefix
ubyte regTo_Rex_W(Register reg) pure nothrow @nogc { return (reg & 0b1000) >> 0; } // 0100 WRXB
ubyte regTo_Rex_R(Register reg) pure nothrow @nogc { return (reg & 0b1000) >> 1; } // 0100 WRXB
ubyte regTo_Rex_X(Register reg) pure nothrow @nogc { return (reg & 0b1000) >> 2; } // 0100 WRXB
ubyte regTo_Rex_B(Register reg) pure nothrow @nogc { return (reg & 0b1000) >> 3; } // 0100 WRXB

// place 3 LSB of register into appropriate bit field of ModR/M byte
ubyte regTo_ModRm_Reg(Register reg) pure nothrow @nogc { return (reg & 0b0111) << 3; }
ubyte regTo_ModRm_Rm(Register reg) pure nothrow @nogc { return (reg & 0b0111) << 0; }

struct SibScale { ubyte bits; ubyte value() { return cast(ubyte)(1 << bits); } }
struct ModRmMod { ubyte bits; }

ubyte encodeSibByte(SibScale ss, Register index, Register base) pure nothrow @nogc {
	return cast(ubyte)(ss.bits << 6) | (index & 0b0111) << 3 | (base & 0b0111);
}

ubyte encodeModRegRmByte(ModRmMod mod, Register reg, Register rm) pure nothrow @nogc {
	return cast(ubyte)(mod.bits << 6) | (reg & 0b0111) << 3 | (rm & 0b0111);
}

enum MemAddrType : ubyte {
	disp32,           // [                     disp32]
	indexDisp32,      // [       (index * s) + disp32]
	base,             // [base                       ]
	baseDisp32,       // [base +             + disp32]
	baseIndex,        // [base + (index * s)         ]
	baseIndexDisp32,  // [base + (index * s) + disp32]
	baseDisp8,        // [base +             + disp8 ]
	baseIndexDisp8    // [base + (index * s) + disp8 ]
}
ubyte sibAddrType(MemAddrType type) { return 0b1_0000 | type; }

ubyte[8] memAddrType_to_mod = [0,0,0,2,0,2,1,1];
ubyte[8] memAddrType_to_dispType = [1,1,0,1,0,1,2,2]; // 0 - none, 1 - disp32, 2 - disp8

// memory location that can be passed to assembly instructions
struct MemAddress {
	ubyte typeStorage; // MemAddrType | 0b1_0000;
	Register indexReg = Register.SP;
	Register baseReg  = Register.BP;
	SibScale scale;
	uint disp; // disp8 is stored here too

	MemAddrType type() { return cast(MemAddrType)(typeStorage & 0b1111); }
	Imm32 disp32() @property { return Imm32(disp); }
	Imm8 disp8() @property { return Imm8(cast(ubyte)(disp & 0xFF)); }

	ubyte rexBits() { return regTo_Rex_X(indexReg) | regTo_Rex_B(baseReg); }
	ubyte modRmByte(ubyte reg = 0) {
		return encodeModRegRmByte(ModRmMod(memAddrType_to_mod[type]), cast(Register)reg, hasSibByte ? Register.SP : baseReg);
	}
	ModRmMod mod() { return ModRmMod(memAddrType_to_mod[type]); }
	ubyte sibByte() { return encodeSibByte(scale, indexReg, baseReg); }
	bool hasDisp32() { return memAddrType_to_dispType[type] == 1; }
	bool hasDisp8 () { return memAddrType_to_dispType[type] == 2; }
	bool hasSibByte() { return cast(bool)(typeStorage & 0b1_0000); }

	string toString() {
		final switch(type) {
			case MemAddrType.disp32: return format("[0x%x]", disp32.value);
			case MemAddrType.indexDisp32: return format("[(%s*%s) + 0x%x]", indexReg, scale.value, disp32.value);
			case MemAddrType.base: return format("[%s]", baseReg);
			case MemAddrType.baseDisp32: return format("[%s + 0x%x]", baseReg, disp32.value);
			case MemAddrType.baseIndex: return format("[%s + (%s*%s)]", baseReg, indexReg, scale.value);
			case MemAddrType.baseIndexDisp32: return format("[%s + (%s*%s) + 0x%x]", baseReg, indexReg, scale.value, disp32.value);
			case MemAddrType.baseDisp8: return format("[%s + 0x%x]", baseReg, disp8.value);
			case MemAddrType.baseIndexDisp8: return format("[%s + (%s*%s) + 0x%x]", baseReg, indexReg, scale.value, disp8.value);
		}
	}
}

// variant 1  [disp32]
MemAddress memAddrDisp32(uint disp32) {
	return MemAddress(sibAddrType(MemAddrType.disp32), Register.SP, Register.BP, SibScale(), disp32); // with SIB
}
// variant 2  [(index * s) + disp32]
MemAddress memAddrIndexDisp32(Register indexReg, SibScale scale, uint disp32) {
	assert(indexReg != Register.SP, "Cannot encode [RSP * scale + disp32]");
	return MemAddress(sibAddrType(MemAddrType.indexDisp32), indexReg, Register.BP, scale, disp32); // with SIB
}
// variant 3  [base]
MemAddress memAddrBase(Register baseReg) {
	if (is_BP_or_R13(baseReg)) // fallback to variant 7 [base + 0x0]
		return memAddrBaseDisp8(baseReg, 0); // with or without SIB
	else if (is_SP_or_R12(baseReg)) // cannot encode SP,R12 without SIB
		return MemAddress(sibAddrType(MemAddrType.base), Register.SP, baseReg); // with SIB
	else
		return MemAddress(MemAddrType.base, Register.SP, baseReg); // no SIB
}
// variant 4  [base + disp32]
MemAddress memAddrBaseDisp32(Register baseReg, uint disp32) {
	if (is_SP_or_R12(baseReg))
		return MemAddress(sibAddrType(MemAddrType.baseDisp32), Register.SP, baseReg, SibScale(), disp32); // with SIB
	else
		return MemAddress(MemAddrType.baseDisp32, Register.SP, baseReg, SibScale(), disp32); // no SIB
}
// variant 5  [base + index * s]
MemAddress memAddrBaseIndex(Register baseReg, Register indexReg, SibScale scale) {
	assert(indexReg != Register.SP, "Cannot encode [base + RSP * scale]");
	if (is_BP_or_R13(baseReg)) // fallback to variant 8 [base + (index * s) + disp8]
		return memAddrBaseIndexDisp8(baseReg, indexReg, scale, 0); // with SIB
	else
		return MemAddress(sibAddrType(MemAddrType.baseIndex), indexReg, baseReg, scale); // with SIB
}
// variant 6  [base + index * s + disp32]
MemAddress memAddrBaseIndexDisp32(Register baseReg, Register indexReg, SibScale scale, uint disp32) {
	assert(indexReg != Register.SP, "Cannot encode [base + RSP * scale + disp32]");
	return MemAddress(sibAddrType(MemAddrType.baseIndexDisp32), indexReg, baseReg, scale, disp32); // with SIB
}
// variant 7  [base + disp8]
MemAddress memAddrBaseDisp8(Register baseReg, ubyte disp8) {
	if (is_SP_or_R12(baseReg)) // cannot encode SP,R12 without SIB
		return MemAddress(sibAddrType(MemAddrType.baseDisp8), Register.SP, baseReg, SibScale(), disp8); // with SIB
	else
		return MemAddress(MemAddrType.baseDisp8, Register.SP, baseReg, SibScale(), disp8); // no SIB
}
// variant 8  [base + (index * s) + disp8]
MemAddress memAddrBaseIndexDisp8(Register baseReg, Register indexReg, SibScale scale, ubyte disp8) {
	assert(indexReg != Register.SP, "Cannot encode [base + RSP * scale + disp8]");
	return MemAddress(sibAddrType(MemAddrType.baseIndexDisp8), indexReg, baseReg, scale, disp8); // with SIB
}

// Opcode structures for 1-byte and 2-byte encodings
struct OP1 { enum size = 1; ubyte op0; }
struct OP2 { enum size = 2; ubyte op0; ubyte op1; }
enum bool isAnyOpcode(O) = is(O == OP1) || is(O == OP2);

alias PC = ubyte*;

struct Encoder
{
	private ubyte[] mem;
	private PC pc;

	void setBuffer(ubyte[] buf) { mem = buf; pc = mem.ptr; }
	void resetPC() { pc = mem.ptr; }
	ubyte[] code() { return mem[0..pc - mem.ptr]; }

	void sink_put(T)(T value)
	{
		*(cast(T*)pc) = value;
		pc += value.sizeof;
	}

	void putRexByteChecked(ArgType argType)(ubyte bits, bool forceRex = false) {
		static if (argType == ArgType.QWORD)
			sink_put!ubyte(REX_PREFIX | REX_W | bits);
		else
			if (bits || forceRex) sink_put!ubyte(REX_PREFIX | bits);
	}
	void putRexByte_RB(ArgType argType)(Register reg, Register rm) { // reg reg
		putRexByteChecked!argType(regTo_Rex_R(reg) | regTo_Rex_B(rm), regNeedsRexPrefix!argType(reg) || regNeedsRexPrefix!argType(rm)); }
	void putRexByte_regB(ArgType argType)(Register rm) { // R.R/M reg
		putRexByteChecked!argType(regTo_Rex_B(rm), regNeedsRexPrefix!argType(rm)); }
	void putRexByte_B(ArgType argType)(Register base) { // base
		putRexByteChecked!argType(regTo_Rex_B(base)); }
	void putRexByte_RXB(ArgType argType)(Register r, Register index, Register base) { // reg index base
		putRexByteChecked!argType(regTo_Rex_R(r) | regTo_Rex_X(index) | regTo_Rex_B(base), regNeedsRexPrefix!argType(r)); }
	void putRexByte_XB(ArgType argType)(Register index, Register base) { // index base
		putRexByteChecked!argType(regTo_Rex_X(index) | regTo_Rex_B(base)); }

	void putInstrBinaryRegReg(ArgType argType, O)(O opcode, Register dst_rm, Register src_reg) if (isAnyOpcode!O) {
		static if (argType == ArgType.WORD) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_RB!argType(src_reg, dst_rm);                                 // REX
		sink_put(opcode);                                                       // Opcode
		sink_put(encodeModRegRmByte(ModRmMod(0b11), src_reg, dst_rm));          // ModR/r
	}
	// PUSH, POP, MOV, XCHG, BSWAP
	void putInstrBinaryRegImm1(ArgType argType, I)(ubyte opcode, Register dst_rm, I src_imm) if (isAnyImm!I) {
		static if (argType == ArgType.WORD) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_regB!argType(dst_rm);                                        // REX
		sink_put!ubyte(opcode | (dst_rm & 0b0111));                             // Opcode + reg
		sink_put(src_imm);                                                      // Imm8/16/32/64
	}
	void putInstrBinaryRegImm2(ArgType argType, I)(ubyte opcode, ubyte regOpcode, Register dst_rm, I src_imm) if (isAnyImm!I) {
		static if (argType == ArgType.WORD) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_regB!argType(dst_rm);                                        // REX
		sink_put(opcode);                                                       // Opcode
		sink_put(encodeModRegRmByte(ModRmMod(0b11), cast(Register)regOpcode, dst_rm));  // ModO/R
		sink_put(src_imm);                                                      // Imm8/16/32/64
	}
	// if isReg == true then dst_r is register, otherwise it is extra opcode
	void putInstrBinaryRegMem(ArgType argType, bool isReg = true, O)(O opcode, Register reg_or_opcode, MemAddress src_mem) if (isAnyOpcode!O) {
		static if (argType == ArgType.WORD) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		static if (isReg) putRexByte_RXB!argType(reg_or_opcode, src_mem.indexReg, src_mem.baseReg); // REX
		else putRexByte_XB!argType(src_mem.indexReg, src_mem.baseReg);          // REX
		sink_put(opcode);                                                       // Opcode
		sink_put(src_mem.modRmByte(reg_or_opcode));                             // ModR/M
		if (src_mem.hasSibByte)	   sink_put(src_mem.sibByte);                   // SIB
		if (src_mem.hasDisp32)     sink_put(src_mem.disp32);                    // disp32
		else if (src_mem.hasDisp8) sink_put(src_mem.disp8);                     // disp8
	}
	void putInstrBinaryMemImm(ArgType argType, I, O)(O opcode, ubyte regOpcode, MemAddress dst_mem, I src_imm) if (isAnyImm!I && isAnyOpcode!O) {
		putInstrBinaryRegMem!(argType, false)(opcode, cast(Register)regOpcode, dst_mem);
		sink_put(src_imm);                                                      // Imm8/16/32
	}

	void putInstrNullary(O)(O opcode) if(isAnyOpcode!O) {
		sink_put(opcode);                                                       // Opcode
	}
	void putInstrNullaryImm(O, I)(O opcode, I imm) if(isAnyOpcode!O && isAnyImm!I) {
		sink_put(opcode);                                                       // Opcode
		sink_put(imm);                                                          // Imm8/16/32/64
	}
	void putInstrUnaryReg1(ArgType argType, O)(O opcode, ubyte regOpcode, Register dst_rm) if (isAnyOpcode!O) {
		static if (argType == ArgType.WORD) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_regB!argType(dst_rm);                                        // REX
		sink_put(opcode);                                                       // Opcode
		sink_put(encodeModRegRmByte(ModRmMod(0b11), cast(Register)regOpcode, dst_rm));// ModO/R
	}
	void putInstrUnaryReg2(ArgType argType)(ubyte opcode, Register dst_rm) {
		static if (argType == ArgType.WORD) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_regB!argType(dst_rm);                                        // REX
		sink_put!ubyte(opcode | (dst_rm & 0b0111));                             // Opcode
	}
	void putInstrUnaryMem(ArgType argType, O)(O opcode, ubyte regOpcode, MemAddress dst_mem) if (isAnyOpcode!O) {
		putInstrBinaryRegMem!(argType, false)(opcode, cast(Register)regOpcode, dst_mem);
	}
}

struct Fixup
{
	private CodeGen_x86_64* codeGen;
	private PC fixupPC;

	void opDispatch(string member, Args...)(Args args) {
		auto tempPC = codeGen.encoder.pc;
		codeGen.encoder.pc = fixupPC;
		mixin("codeGen."~member~"(args);");
		codeGen.encoder.pc = tempPC;
	}
}

Imm32 jumpOffset(PC from, PC to) {
	return Imm32(cast(int)(to - from));
}

// Sink defines put(T) for ubyte, ubyte[], Imm8, Imm16, Imm32, Imm64
struct CodeGen_x86_64
{
	Encoder encoder;

	Fixup saveFixup() {
		return Fixup(&this, encoder.pc);
	}

	PC pc() {
		return encoder.pc;
	}

	/// Used for versions of instructions without argument size suffix.
	/// mov, add, sub, instead of movq, addb, subd.
	/// mov(Register.AX, Register.DI, ArgType.QWORD); instead of movq(Register.AX, Register.DI);
	void opDispatch(string s, Arg1, Arg2)(Arg1 dst, Arg2 src, ArgType argType) {
		switch(argType) {
			static if (__traits(hasMember, typeof(this), s~'b')) { case ArgType.BYTE:  mixin(s~"b(dst, src);"); break; }
			static if (__traits(hasMember, typeof(this), s~'w')) { case ArgType.WORD:  mixin(s~"w(dst, src);"); break; }
			static if (__traits(hasMember, typeof(this), s~'d')) { case ArgType.DWORD: mixin(s~"d(dst, src);"); break; }
			static if (__traits(hasMember, typeof(this), s~'q')) { case ArgType.QWORD: mixin(s~"q(dst, src);"); break; }
			default: assert(false, format("Cannot encode %s(%s, %s, ArgType.%s)", s, dst, src, argType));
		}
	}

	/// ditto
	void opDispatch(string s, Arg1)(Arg1 dst, ArgType argType) {
		switch(argType) {
			static if (__traits(hasMember, typeof(this), s~'b')) { case ArgType.BYTE:  mixin(s~"b(dst);"); break; }
			static if (__traits(hasMember, typeof(this), s~'w')) { case ArgType.WORD:  mixin(s~"w(dst);"); break; }
			static if (__traits(hasMember, typeof(this), s~'d')) { case ArgType.DWORD: mixin(s~"d(dst);"); break; }
			static if (__traits(hasMember, typeof(this), s~'q')) { case ArgType.QWORD: mixin(s~"q(dst);"); break; }
			default: assert(false, format("Cannot encode %s(%s, ArgType.%s)", s, dst, argType));
		}
	}

	void beginFunction() {
		// Copies parameters from registers to shadow space
		// Pushes registers to be preserved on stack
		// Allocates room on stack for local variables
		// Sets a frame pointer (so frame pointer is set AFTER local variables!) if needed
		pushq(Register.BP);
		movq(Register.BP, Register.SP);
		// Allocates space needed to store volatile registers that must be preserved in function calls
		// Allocates shadow space for called functions.
	}
	void endFunction() {
		popq(Register.BP);
		ret();
	}

	void addb(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (OP1(0x00), src, dst); }
	void addw(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (OP1(0x01), src, dst); }
	void addd(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(OP1(0x01), src, dst); }
	void addq(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(OP1(0x01), src, dst); }

	void addb(Register dst,   Register src){ encoder.putInstrBinaryRegReg!(ArgType.BYTE) (OP1(0x00), dst, src); }
	void addw(Register dst,   Register src){ encoder.putInstrBinaryRegReg!(ArgType.WORD) (OP1(0x01), dst, src); }
	void addd(Register dst,   Register src){ encoder.putInstrBinaryRegReg!(ArgType.DWORD)(OP1(0x01), dst, src); }
	void addq(Register dst,   Register src){ encoder.putInstrBinaryRegReg!(ArgType.QWORD)(OP1(0x01), dst, src); }

	void addb(Register dst,   MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (OP1(0x02), dst, src); }
	void addw(Register dst,   MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (OP1(0x03), dst, src); }
	void addd(Register dst,   MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(OP1(0x03), dst, src); }
	void addq(Register dst,   MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(OP1(0x03), dst, src); }

	void addb(Register dst,   Imm8  src){ encoder.putInstrBinaryRegImm2!(ArgType.BYTE) (0x80, 0, dst, src); }
	void addw(Register dst,   Imm16 src){ encoder.putInstrBinaryRegImm2!(ArgType.WORD) (0x81, 0, dst, src); }
	void addd(Register dst,   Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.DWORD)(0x81, 0, dst, src); }
	void addq(Register dst,   Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(0x81, 0, dst, src); }

	void addw(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.WORD) (0x83, 0, dst, src); }
	void addd(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.DWORD)(0x83, 0, dst, src); }
	void addq(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(0x83, 0, dst, src); }

	void addb(MemAddress dst, Imm8  src){ encoder.putInstrBinaryMemImm!(ArgType.BYTE) (OP1(0x80), 0, dst, src); }
	void addw(MemAddress dst, Imm16 src){ encoder.putInstrBinaryMemImm!(ArgType.WORD) (OP1(0x81), 0, dst, src); }
	void addd(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.DWORD)(OP1(0x81), 0, dst, src); }
	void addq(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.QWORD)(OP1(0x81), 0, dst, src); }

	void addw(MemAddress dst, Imm8 src){ encoder.putInstrBinaryMemImm!(ArgType.WORD) (OP1(0x83), 0, dst, src); }
	void addd(MemAddress dst, Imm8 src){ encoder.putInstrBinaryMemImm!(ArgType.DWORD)(OP1(0x83), 0, dst, src); }
	void addq(MemAddress dst, Imm8 src){ encoder.putInstrBinaryMemImm!(ArgType.QWORD)(OP1(0x83), 0, dst, src); }


	void movb(Register dst, Imm8  src){ encoder.putInstrBinaryRegImm1!(ArgType.BYTE) (0xB0, dst, src); }
	void movw(Register dst, Imm16 src){ encoder.putInstrBinaryRegImm1!(ArgType.WORD) (0xB8, dst, src); }
	void movd(Register dst, Imm32 src){ encoder.putInstrBinaryRegImm1!(ArgType.DWORD)(0xB8, dst, src); }
	void movq(Register dst, Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(0xC7, 0, dst, src); }
	void movq(Register dst, Imm64 src){ encoder.putInstrBinaryRegImm1!(ArgType.QWORD)(0xB8, dst, src); }

	void movb(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.BYTE) (OP1(0x88), dst, src); }
	void movw(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.WORD) (OP1(0x89), dst, src); }
	void movd(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.DWORD)(OP1(0x89), dst, src); }
	void movq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.QWORD)(OP1(0x89), dst, src); }

	void movb(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (OP1(0x8A), dst, src); }
	void movw(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (OP1(0x8B), dst, src); }
	void movd(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(OP1(0x8B), dst, src); }
	void movq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(OP1(0x8B), dst, src); }

	void movb(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (OP1(0x88), src, dst); }
	void movw(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (OP1(0x89), src, dst); }
	void movd(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(OP1(0x89), src, dst); }
	void movq(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(OP1(0x89), src, dst); }

	void movb(MemAddress dst, Imm8  src){ encoder.putInstrBinaryMemImm!(ArgType.BYTE) (OP1(0xC6), 0, dst, src); }
	void movw(MemAddress dst, Imm16 src){ encoder.putInstrBinaryMemImm!(ArgType.WORD) (OP1(0xC7), 0, dst, src); }
	void movd(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.DWORD)(OP1(0xC7), 0, dst, src); }
	void movq(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.QWORD)(OP1(0xC7), 0, dst, src); }


	void subb(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (OP1(0x28), src, dst); }
	void subw(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (OP1(0x29), src, dst); }
	void subd(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(OP1(0x29), src, dst); }
	void subq(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(OP1(0x29), src, dst); }

	void subb(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.BYTE) (OP1(0x28), dst, src); }
	void subw(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.WORD) (OP1(0x29), dst, src); }
	void subd(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.DWORD)(OP1(0x29), dst, src); }
	void subq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.QWORD)(OP1(0x29), dst, src); }

	void subb(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (OP1(0x2A), dst, src); }
	void subw(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (OP1(0x2B), dst, src); }
	void subd(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(OP1(0x2B), dst, src); }
	void subq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(OP1(0x2B), dst, src); }

	void subb(Register dst, Imm8  src){ encoder.putInstrBinaryRegImm2!(ArgType.BYTE) (0x80, 5, dst, src); }
	void subw(Register dst, Imm16 src){ encoder.putInstrBinaryRegImm2!(ArgType.WORD) (0x81, 5, dst, src); }
	void subd(Register dst, Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.DWORD)(0x81, 5, dst, src); }
	void subq(Register dst, Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(0x81, 5, dst, src); }

	void subw(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.WORD) (0x83, 5, dst, src); }
	void subd(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.DWORD)(0x83, 5, dst, src); }
	void subq(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(0x83, 5, dst, src); }

	void subb(MemAddress dst, Imm8  src){ encoder.putInstrBinaryMemImm!(ArgType.BYTE) (OP1(0x80), 5, dst, src); }
	void subw(MemAddress dst, Imm16 src){ encoder.putInstrBinaryMemImm!(ArgType.WORD) (OP1(0x81), 5, dst, src); }
	void subd(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.DWORD)(OP1(0x81), 5, dst, src); }
	void subq(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.QWORD)(OP1(0x81), 5, dst, src); }

	void subw(MemAddress dst, Imm8 src){ encoder.putInstrBinaryMemImm!(ArgType.WORD) (OP1(0x83), 5, dst, src); }
	void subd(MemAddress dst, Imm8 src){ encoder.putInstrBinaryMemImm!(ArgType.DWORD)(OP1(0x83), 5, dst, src); }
	void subq(MemAddress dst, Imm8 src){ encoder.putInstrBinaryMemImm!(ArgType.QWORD)(OP1(0x83), 5, dst, src); }


	void cmpb(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (OP1(0x38), src, dst); }
	void cmpw(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (OP1(0x39), src, dst); }
	void cmpd(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(OP1(0x39), src, dst); }
	void cmpq(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(OP1(0x39), src, dst); }

	void cmpb(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.BYTE) (OP1(0x38), dst, src); }
	void cmpw(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.WORD) (OP1(0x39), dst, src); }
	void cmpd(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.DWORD)(OP1(0x39), dst, src); }
	void cmpq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.QWORD)(OP1(0x39), dst, src); }

	void cmpb(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (OP1(0x3A), dst, src); }
	void cmpw(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (OP1(0x3B), dst, src); }
	void cmpd(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(OP1(0x3B), dst, src); }
	void cmpq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(OP1(0x3B), dst, src); }

	void cmpb(Register dst,   Imm8  src){ encoder.putInstrBinaryRegImm2!(ArgType.BYTE) (0x80, 7, dst, src); }
	void cmpw(Register dst,   Imm16 src){ encoder.putInstrBinaryRegImm2!(ArgType.WORD) (0x81, 7, dst, src); }
	void cmpd(Register dst,   Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.DWORD)(0x81, 7, dst, src); }
	void cmpq(Register dst,   Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(0x81, 7, dst, src); }

	void cmpw(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.WORD) (0x83, 7, dst, src); }
	void cmpd(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.DWORD)(0x83, 7, dst, src); }
	void cmpq(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(0x83, 7, dst, src); }

	void cmpb(MemAddress dst, Imm8  src){ encoder.putInstrBinaryMemImm!(ArgType.BYTE) (OP1(0x80), 7, dst, src); }
	void cmpw(MemAddress dst, Imm16 src){ encoder.putInstrBinaryMemImm!(ArgType.WORD) (OP1(0x81), 7, dst, src); }
	void cmpd(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.DWORD)(OP1(0x81), 7, dst, src); }
	void cmpq(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.QWORD)(OP1(0x81), 7, dst, src); }

	void cmpw(MemAddress dst, Imm8 src){ encoder.putInstrBinaryMemImm!(ArgType.WORD) (OP1(0x83), 7, dst, src); }
	void cmpd(MemAddress dst, Imm8 src){ encoder.putInstrBinaryMemImm!(ArgType.DWORD)(OP1(0x83), 7, dst, src); }
	void cmpq(MemAddress dst, Imm8 src){ encoder.putInstrBinaryMemImm!(ArgType.QWORD)(OP1(0x83), 7, dst, src); }


	mixin unaryInstr_Reg!("inc", [0xFE,0xFF], 0);
	mixin unaryInstr_Mem!("inc", [0xFE,0xFF], 0);

	mixin unaryInstr_Reg!("dec", [0xFE,0xFF], 1);
	mixin unaryInstr_Mem!("dec", [0xFE,0xFF], 1);

	mixin unaryInstr_Reg!("mul", [0xF6,0xF7], 4);
	mixin unaryInstr_Mem!("mul", [0xF6,0xF7], 4);

	mixin unaryInstr_Reg!("div", [0xF6,0xF7], 6);
	mixin unaryInstr_Mem!("div", [0xF6,0xF7], 6);

	mixin unaryInstr_Reg!("not", [0xF6,0xF7], 2);
	mixin unaryInstr_Mem!("not", [0xF6,0xF7], 2);


	void nop() { encoder.putInstrNullary(OP1(0x90)); }

	/// relative call to target virtual address.
	void call(PC target) { encoder.putInstrNullaryImm(OP1(0xE8), jumpOffset(encoder.pc + 5, target)); } // relative to next instr

	/// jump relative to next instr.
	void jmp(Imm8 offset ) { encoder.putInstrNullaryImm(OP1(0xEB), offset); }
	void jmp(Imm32 offset) { encoder.putInstrNullaryImm(OP1(0xE9), offset);	}

	/// jump relative to next instr.
	void jcc(Condition condition, Imm8  offset) { encoder.putInstrNullaryImm(OP1(0x70 | condition), offset); }
	void jcc(Condition condition, Imm32 offset) { encoder.putInstrNullaryImm(OP2(0x0F, 0x80 | condition), offset); }

	void setcc(Condition condition, Register dst)   { encoder.putInstrUnaryReg1!(ArgType.BYTE)(OP2(0x0F, 0x90 | condition), 0, dst); }
	void setcc(Condition condition, MemAddress dst) { encoder.putInstrUnaryMem !(ArgType.BYTE)(OP2(0x0F, 0x90 | condition), 0, dst); }

	void testb(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.BYTE) (OP1(0x84), dst, src); }
	void testw(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.WORD) (OP1(0x85), dst, src); }
	void testd(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.DWORD)(OP1(0x85), dst, src); }
	void testq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.QWORD)(OP1(0x85), dst, src); }

	void movzx_btow(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.WORD) (OP2(0x0F, 0xB6), dst, src); }
	void movzx_btod(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.DWORD)(OP2(0x0F, 0xB6), dst, src); }
	void movzx_btoq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.QWORD)(OP2(0x0F, 0xB6), dst, src); }
	void movzx_wtod(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.DWORD)(OP2(0x0F, 0xB7), dst, src); }
	void movzx_wtoq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.QWORD)(OP2(0x0F, 0xB7), dst, src); }

	void popw(Register dst)   { encoder.putInstrUnaryReg2!(ArgType.WORD )(0x58, dst); }
	void popq(Register dst)   { encoder.putInstrUnaryReg2!(ArgType.DWORD)(0x58, dst); } // use DWORD to omit REX.W
	void popw(MemAddress dst) { encoder.putInstrUnaryMem!( ArgType.WORD )(OP1(0x8F), 0, dst); }
	void popq(MemAddress dst) { encoder.putInstrUnaryMem!( ArgType.DWORD)(OP1(0x8F), 0, dst); } // use DWORD to omit REX.W

	void pushw(Register dst)   { encoder.putInstrUnaryReg2!(ArgType.WORD )(0x50, dst); }
	void pushq(Register dst)   { encoder.putInstrUnaryReg2!(ArgType.DWORD)(0x50, dst); } // use DWORD to omit REX.W
	void pushw(MemAddress dst) { encoder.putInstrUnaryMem!( ArgType.WORD )(OP1(0xFF), 6, dst); }
	void pushq(MemAddress dst) { encoder.putInstrUnaryMem!( ArgType.DWORD)(OP1(0xFF), 6, dst); } // use DWORD to omit REX.W

	void ret() { encoder.putInstrNullary(OP1(0xC3)); }
	void ret(Imm16 bytesToPop) { encoder.putInstrNullaryImm(OP1(0xC2), bytesToPop); }

	void int3() { encoder.putInstrNullary(OP1(0xCC)); }
}

mixin template unaryInstr_Reg(string name, ubyte[2] opcodes, ubyte extraOpcode) {
	mixin(format("void %sb(Register dst) { encoder.putInstrUnaryReg1!(ArgType.BYTE) (OP1(%s), %s, dst); }", name, opcodes[0], extraOpcode));
	mixin(format("void %sw(Register dst) { encoder.putInstrUnaryReg1!(ArgType.WORD) (OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sd(Register dst) { encoder.putInstrUnaryReg1!(ArgType.DWORD)(OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sq(Register dst) { encoder.putInstrUnaryReg1!(ArgType.QWORD)(OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));
}
mixin template unaryInstr_Mem(string name, ubyte[2] opcodes, ubyte extraOpcode) {
	mixin(format("void %sb(MemAddress dst) { encoder.putInstrUnaryMem!(ArgType.BYTE) (OP1(%s), %s, dst); }", name, opcodes[0], extraOpcode));
	mixin(format("void %sw(MemAddress dst) { encoder.putInstrUnaryMem!(ArgType.WORD) (OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sd(MemAddress dst) { encoder.putInstrUnaryMem!(ArgType.DWORD)(OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sq(MemAddress dst) { encoder.putInstrUnaryMem!(ArgType.QWORD)(OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));
}
