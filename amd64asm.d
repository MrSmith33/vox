/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module amd64asm;
import std.stdio;

enum Register : ubyte {AX, CX, DX, BX, SP, BP, SI, DI, R8, R9, R10, R11, R12, R13, R14, R15}
enum RegisterMax  = cast(Register)(Register.max+1);

bool is_SP_or_R12(Register reg) { return (reg & 0b111) == 0b100; }
bool is_BP_or_R13(Register reg) { return (reg & 0b111) == 0b101; }

enum ArgType : ubyte { BYTE, WORD, DWORD, QWORD }

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

ubyte[8] memAddrType_to_mod = [0,0,0,2,0,2,1,1];
ubyte[8] memAddrType_to_dispType = [1,1,0,1,0,1,2,2]; // 0 - none, 1 - disp32, 2 - disp8

// memory location that can be passed to assembly instructions
struct MemAddress {
	MemAddrType type;
	Register indexReg = Register.SP;
	Register baseReg  = Register.BP;
	SibScale scale;
	uint disp; // disp8 is stored here too
	Imm32 disp32() @property { return Imm32(disp); }
	Imm8 disp8() @property { return Imm8(cast(ubyte)(disp & 0xFF)); }

	ubyte rexBits() { return regTo_Rex_X(indexReg) | regTo_Rex_B(baseReg); }
	ubyte modRmByte(ubyte reg = 0) { return encodeModRegRmByte(ModRmMod(memAddrType_to_mod[type]), cast(Register)reg, Register.SP); }
	ModRmMod mod() { return ModRmMod(memAddrType_to_mod[type]); }
	ubyte sibByte() { return encodeSibByte(scale, indexReg, baseReg); }
	bool hasDisp32() { return memAddrType_to_dispType[type] == 1; }
	bool hasDisp8 () { return memAddrType_to_dispType[type] == 2; }

	string toString() {
		final switch(type) {
			case MemAddrType.disp32: return format("[0x%x]", disp32.value);
			case MemAddrType.indexDisp32: return format("[(%s*%s) + 0x%x]", indexReg, scale.value, disp32.value);
			case MemAddrType.base: return format("[%s]", baseReg);
			case MemAddrType.baseDisp32: return format("[%s + 0x%x]", baseReg, disp32.value);
			case MemAddrType.baseIndex: return format("[%s + (%s*%s)]", baseReg, indexReg, scale.value);
			case MemAddrType.baseIndexDisp32: return format("[%s + (%s*%s) + 0x%x]", baseReg, indexReg, scale.value, disp32.value);
			case MemAddrType.baseDisp8: return format("[0x%x]", disp8.value);
			case MemAddrType.baseIndexDisp8: return format("[(%s*%s) + 0x%x]", indexReg, scale.value, disp8.value);
		}
	}
}

MemAddress memAddrDisp32(uint disp32) {
	return MemAddress(MemAddrType.disp32, Register.SP, Register.BP, SibScale(), disp32); }
MemAddress memAddrIndexDisp32(Register indexReg, SibScale scale, uint disp32) {
	return MemAddress(MemAddrType.indexDisp32, indexReg, Register.BP, scale, disp32); }
MemAddress memAddrBase(Register baseReg) {
	if (is_BP_or_R13(baseReg)) // fallback to variant 7 with SIB byte [base + 0x0], MemAddrType.baseDisp8
		return memAddrBaseDisp8(baseReg, 0);
	else
		return MemAddress(MemAddrType.base, Register.SP, baseReg); }
MemAddress memAddrBaseDisp32(Register baseReg, uint disp32) {
	return MemAddress(MemAddrType.baseDisp32, Register.SP, baseReg, SibScale(), disp32); }
MemAddress memAddrBaseIndex(Register baseReg, Register indexReg, SibScale scale) {
	return MemAddress(MemAddrType.baseIndex, indexReg, baseReg, scale); }
MemAddress memAddrBaseIndexDisp32(Register baseReg, Register indexReg, SibScale scale, uint disp32) {
	return MemAddress(MemAddrType.baseIndexDisp32, indexReg, baseReg, scale, disp32); }
MemAddress memAddrBaseDisp8(Register baseReg, ubyte disp8) {
	return MemAddress(MemAddrType.baseDisp8, Register.SP, baseReg, SibScale(), disp8); }
MemAddress memAddrBaseIndexDisp8(Register baseReg, Register indexReg, SibScale scale, ubyte disp8) {
	return MemAddress(MemAddrType.baseIndexDisp8, indexReg, baseReg, scale, disp8); }

struct Encoder
{
	import utils : ArraySink;
	ArraySink sink;

	void putRexByteChecked(ArgType argType)(ubyte bits, bool forceRex = false) {
		//writefln("REX bits %08b force %s", bits, forceRex);
		static if (argType == ArgType.QWORD)
			sink.put(REX_PREFIX | REX_W | bits);
		else
			if (bits || forceRex) sink.put(REX_PREFIX | bits);
	}
	void putRexByte_RB(ArgType argType)(Register reg, Register rm) { // reg reg
		putRexByteChecked!argType(regTo_Rex_R(reg) | regTo_Rex_B(rm), regNeedsRexPrefix!argType(reg) || regNeedsRexPrefix!argType(rm)); }
	void putRexByte_rB(ArgType argType)(Register rm) { // R.R/M reg
		putRexByteChecked!argType(regTo_Rex_B(rm), regNeedsRexPrefix!argType(rm)); } // ensures REX prefix for ah ch dh bh
	void putRexByte_B(ArgType argType)(Register base) { // base
		putRexByteChecked!argType(regTo_Rex_B(base)); }
	void putRexByte_RXB(ArgType argType)(Register r, Register index, Register base) { // reg index base
		putRexByteChecked!argType(regTo_Rex_R(r) | regTo_Rex_X(index) | regTo_Rex_B(base), regNeedsRexPrefix!argType(r)); }
	void putRexByte_XB(ArgType argType)(Register index, Register base) { // index base
		putRexByteChecked!argType(regTo_Rex_X(index) | regTo_Rex_B(base)); }

	void putInstrBinaryRegReg(ArgType argType)(ubyte opcode, Register dst_rm, Register src_reg) {
		static if (argType == ArgType.WORD) sink.put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_RB!argType(src_reg, dst_rm);                                 // REX
		sink.put(opcode);                                                       // Opcode
		sink.put(encodeModRegRmByte(ModRmMod(0b11), src_reg, dst_rm));          // ModR/r
	}
	// PUSH, POP, MOV, XCHG, BSWAP
	void putInstrBinaryRegImm1(ArgType argType, I)(ubyte opcode, Register dst_rm, I src_imm) if (isAnyImm!I) {
		static if (argType == ArgType.WORD) sink.put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_rB!argType(dst_rm);                                          // REX
		sink.put(opcode | (dst_rm & 0b0111));                                   // Opcode + reg
		sink.put(src_imm);                                                      // Imm8/16/32/64
	}
	void putInstrBinaryRegImm2(ArgType argType, I)(ubyte opcode, ubyte regOpcode, Register dst_rm, I src_imm) if (isAnyImm!I) {
		static if (argType == ArgType.WORD) sink.put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_rB!argType(dst_rm);                                          // REX
		sink.put(opcode);                                                       // Opcode
		sink.put(encodeModRegRmByte(ModRmMod(0b11), cast(Register)regOpcode, dst_rm));  // ModO/R
		sink.put(src_imm);                                                      // Imm8/16/32/64
	}
	void putInstrBinaryRegMem(ArgType argType)(ubyte opcode, Register dst_r, MemAddress src_mem) {
		static if (argType == ArgType.WORD) sink.put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_RXB!argType(dst_r, src_mem.indexReg, src_mem.baseReg);       // REX
		sink.put(opcode);                                                       // Opcode
		sink.put(src_mem.modRmByte(dst_r));                                     // ModR/M
		sink.put(src_mem.sibByte);                                              // SIB
		if (src_mem.hasDisp32)
			sink.put(src_mem.disp32);                                           // disp32
		else if (src_mem.hasDisp8)
			sink.put(src_mem.disp8);                                            // disp8
	}
	void putInstrBinaryOpMem(ArgType argType)(ubyte opcode, Register regOpcode, MemAddress src_mem) {
		static if (argType == ArgType.WORD) sink.put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_XB!argType(src_mem.indexReg, src_mem.baseReg);               // REX
		sink.put(opcode);                                                       // Opcode
		sink.put(src_mem.modRmByte(regOpcode));                                 // ModO/M
		sink.put(src_mem.sibByte);                                              // SIB
		if (src_mem.hasDisp32)
			sink.put(src_mem.disp32);                                           // disp32
		else if (src_mem.hasDisp8)
			sink.put(src_mem.disp8);                                            // disp8
	}
	void putInstrBinaryMemImm(ArgType argType, I)(ubyte opcode, ubyte regOpcode, MemAddress dst_mem, I src_imm) if (isAnyImm!I) {
		static assert( // allow special case of QwordPtr and Imm32
			argType == I.argT || (argType == ArgType.QWORD && I.argT == ArgType.DWORD),
			"Sizes of ptr and imm must be equal");
		static if (argType == ArgType.WORD) sink.put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_XB!argType(dst_mem.indexReg, dst_mem.baseReg);               // REX
		sink.put(opcode);                                                       // Opcode
		sink.put(dst_mem.modRmByte(cast(Register)regOpcode));                   // ModO/M
		sink.put(dst_mem.sibByte);                                              // SIB
		if (dst_mem.hasDisp32) sink.put(dst_mem.disp32);                        // disp32
		else if (dst_mem.hasDisp8) sink.put(dst_mem.disp8);                     //   or disp8
		sink.put(src_imm);                                                      // Imm8/16/32
	}

	void putInstrUnaryRegOpMod(ArgType argType)(ubyte opcode, Register dst_rm, Register regOpcode, ModRmMod mod) {
		static if (argType == ArgType.WORD) sink.put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_rB!argType(dst_rm);                                          // REX
		sink.put(opcode);                                                       // Opcode
		sink.put(encodeModRegRmByte(mod, regOpcode, dst_rm));                   // ModO/R
	}
	void putInstrUnaryMemOpMod(ArgType argType)(ubyte opcode, Register base, Register regOpcode, ModRmMod mod) {
		static if (argType == ArgType.WORD) sink.put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_B!argType(base);                                             // REX
		sink.put(opcode);                                                       // Opcode
		sink.put(encodeModRegRmByte(mod, regOpcode, base));                     // ModO/B
	}
	void putInstrUnaryReg1(ArgType argType)(ubyte opcode, ubyte regOpcode, Register dst_rm) {
		putInstrUnaryRegOpMod!argType(opcode, dst_rm, cast(Register)regOpcode, ModRmMod(0b11));
	}
	void putInstrUnaryReg2(ArgType argType)(ubyte opcode, Register dst_rm) {
		static if (argType == ArgType.WORD) sink.put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_rB!argType(dst_rm);                                          // REX
		sink.put(opcode | (dst_rm & 0b0111));                                   // Opcode
	}
	void putInstrUnaryMem(ArgType argType)(ubyte opcode, ubyte regOpcode, MemAddress dst_mem) {
		switch(dst_mem.type) {
			//case MemAddrType.disp32: // 1 TODO, implement RIP relative addressing
				//putInstrUnaryMemOpMod!argType(opcode, Register.BP, cast(Register)regOpcode, dst_mem.mod);
				//sink.put(dst_mem.disp32);
				//return;
			case MemAddrType.base: // 3
				if (is_SP_or_R12(dst_mem.baseReg)) break; // fallback to variant 3 [base] with SIB byte
				if (is_BP_or_R13(dst_mem.baseReg)) { // fallback to variant 7 [base + 0] for BP and R13
					dst_mem = memAddrBaseDisp8(dst_mem.baseReg, 0); // [base] -> [base + 0]
					goto case MemAddrType.baseDisp8; // goto variant 7
				}
				putInstrUnaryMemOpMod!argType(opcode, dst_mem.baseReg, cast(Register)regOpcode, dst_mem.mod);
				return;
			case MemAddrType.baseDisp32: // 4
				if (is_SP_or_R12(dst_mem.baseReg)) break; // cannot encode SP,R12 without SIB
				putInstrUnaryMemOpMod!argType(opcode, dst_mem.baseReg, cast(Register)regOpcode, dst_mem.mod);
				sink.put(dst_mem.disp32);
				return;
			case MemAddrType.baseDisp8: // 7
				if (is_SP_or_R12(dst_mem.baseReg)) break; // cannot encode SP,R12 without SIB
				putInstrUnaryMemOpMod!argType(opcode, dst_mem.baseReg, cast(Register)regOpcode, dst_mem.mod);
				sink.put(dst_mem.disp8);
				return;
			default: break;
		}

		putInstrBinaryOpMem!argType(opcode, cast(Register)regOpcode, dst_mem); // fallback to SIB byte
	}
}

struct Fixup
{
	private CodeGen_x86_64* codeGen;
	private int fixupOffset;

	void opDispatch(string member, Args...)(Args args) {
		int temp = cast(int)codeGen.encoder.sink.length;
		codeGen.encoder.sink.length = fixupOffset;
		mixin("codeGen."~member~"(args);");
		codeGen.encoder.sink.length = temp;
	}
}

// Sink defines put(T) for ubyte, ubyte[], Imm8, Imm16, Imm32, Imm64
struct CodeGen_x86_64
{
	Encoder encoder;

	Fixup saveFixup() {
		return Fixup(&this, currentOffset());
	}

	int currentOffset() {
		return cast(int)encoder.sink.length;
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

	void addb(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (0x00, src, dst); }
	void addw(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (0x01, src, dst); }
	void addd(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(0x01, src, dst); }
	void addq(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(0x01, src, dst); }

	void addb(Register dst,   Register src){ encoder.putInstrBinaryRegReg!(ArgType.BYTE) (0x00, dst, src); }
	void addw(Register dst,   Register src){ encoder.putInstrBinaryRegReg!(ArgType.WORD) (0x01, dst, src); }
	void addd(Register dst,   Register src){ encoder.putInstrBinaryRegReg!(ArgType.DWORD)(0x01, dst, src); }
	void addq(Register dst,   Register src){ encoder.putInstrBinaryRegReg!(ArgType.QWORD)(0x01, dst, src); }

	void addb(Register dst,   MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (0x02, dst, src); }
	void addw(Register dst,   MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (0x03, dst, src); }
	void addd(Register dst,   MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(0x03, dst, src); }
	void addq(Register dst,   MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(0x03, dst, src); }

	void addb(Register dst,   Imm8  src){ encoder.putInstrBinaryRegImm2!(ArgType.BYTE) (0x80, 0, dst, src); }
	void addw(Register dst,   Imm16 src){ encoder.putInstrBinaryRegImm2!(ArgType.WORD) (0x81, 0, dst, src); }
	void addd(Register dst,   Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.DWORD)(0x81, 0, dst, src); }
	void addq(Register dst,   Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(0x81, 0, dst, src); }

	void addw(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.WORD) (0x83, 0, dst, src); }
	void addd(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.DWORD)(0x83, 0, dst, src); }
	void addq(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(0x83, 0, dst, src); }

	void addb(MemAddress dst, Imm8  src){ encoder.putInstrBinaryMemImm!(ArgType.BYTE) (0x80, 0, dst, src); }
	void addw(MemAddress dst, Imm16 src){ encoder.putInstrBinaryMemImm!(ArgType.WORD) (0x81, 0, dst, src); }
	void addd(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.DWORD)(0x81, 0, dst, src); }
	void addq(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.QWORD)(0x81, 0, dst, src); }


	void movb(Register dst, Imm8  src){ encoder.putInstrBinaryRegImm1!(ArgType.BYTE) (0xB0, dst, src); }
	void movw(Register dst, Imm16 src){ encoder.putInstrBinaryRegImm1!(ArgType.WORD) (0xB8, dst, src); }
	void movd(Register dst, Imm32 src){ encoder.putInstrBinaryRegImm1!(ArgType.DWORD)(0xB8, dst, src); }
	void movq(Register dst, Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(0xC7, 0, dst, src); }
	void movq(Register dst, Imm64 src){ encoder.putInstrBinaryRegImm1!(ArgType.QWORD)(0xB8, dst, src); }

	void movb(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.BYTE) (0x88, dst, src); }
	void movw(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.WORD) (0x89, dst, src); }
	void movd(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.DWORD)(0x89, dst, src); }
	void movq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.QWORD)(0x89, dst, src); }

	void movb(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (0x8A, dst, src); }
	void movw(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (0x8B, dst, src); }
	void movd(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(0x8B, dst, src); }
	void movq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(0x8B, dst, src); }

	void movb(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (0x88, src, dst); }
	void movw(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (0x89, src, dst); }
	void movd(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(0x89, src, dst); }
	void movq(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(0x89, src, dst); }

	void movb(MemAddress dst, Imm8  src){ encoder.putInstrBinaryMemImm!(ArgType.BYTE) (0xC6, 0, dst, src); }
	void movw(MemAddress dst, Imm16 src){ encoder.putInstrBinaryMemImm!(ArgType.WORD) (0xC7, 0, dst, src); }
	void movd(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.DWORD)(0xC7, 0, dst, src); }
	void movq(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.QWORD)(0xC7, 0, dst, src); }


	void subb(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (0x28, src, dst); }
	void subw(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (0x29, src, dst); }
	void subd(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(0x29, src, dst); }
	void subq(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(0x29, src, dst); }

	void subb(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.BYTE) (0x28, dst, src); }
	void subw(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.WORD) (0x29, dst, src); }
	void subd(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.DWORD)(0x29, dst, src); }
	void subq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.QWORD)(0x29, dst, src); }

	void subb(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (0x2A, dst, src); }
	void subw(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (0x2B, dst, src); }
	void subd(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(0x2B, dst, src); }
	void subq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(0x2B, dst, src); }

	void subb(Register dst, Imm8  src){ encoder.putInstrBinaryRegImm2!(ArgType.BYTE) (0x80, 5, dst, src); }
	void subw(Register dst, Imm16 src){ encoder.putInstrBinaryRegImm2!(ArgType.WORD) (0x81, 5, dst, src); }
	void subd(Register dst, Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.DWORD)(0x81, 5, dst, src); }
	void subq(Register dst, Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(0x81, 5, dst, src); }

	void subw(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.WORD) (0x83, 5, dst, src); }
	void subd(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.DWORD)(0x83, 5, dst, src); }
	void subq(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(0x83, 5, dst, src); }

	void subb(MemAddress dst, Imm8  src){ encoder.putInstrBinaryMemImm!(ArgType.BYTE) (0x80, 5, dst, src); }
	void subw(MemAddress dst, Imm16 src){ encoder.putInstrBinaryMemImm!(ArgType.WORD) (0x81, 5, dst, src); }
	void subd(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.DWORD)(0x81, 5, dst, src); }
	void subq(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.QWORD)(0x81, 5, dst, src); }


	void cmpb(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (0x38, src, dst); }
	void cmpw(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (0x39, src, dst); }
	void cmpd(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(0x39, src, dst); }
	void cmpq(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(0x39, src, dst); }

	void cmpb(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.BYTE) (0x38, dst, src); }
	void cmpw(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.WORD) (0x39, dst, src); }
	void cmpd(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.DWORD)(0x39, dst, src); }
	void cmpq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.QWORD)(0x39, dst, src); }

	void cmpb(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (0x3A, dst, src); }
	void cmpw(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (0x3B, dst, src); }
	void cmpd(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(0x3B, dst, src); }
	void cmpq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(0x3B, dst, src); }

	void cmpb(Register dst,   Imm8  src){ encoder.putInstrBinaryRegImm2!(ArgType.BYTE) (0x80, 7, dst, src); }
	void cmpw(Register dst,   Imm16 src){ encoder.putInstrBinaryRegImm2!(ArgType.WORD) (0x81, 7, dst, src); }
	void cmpd(Register dst,   Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.DWORD)(0x81, 7, dst, src); }
	void cmpq(Register dst,   Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(0x81, 7, dst, src); }

	void cmpw(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.WORD) (0x83, 7, dst, src); }
	void cmpd(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.DWORD)(0x83, 7, dst, src); }
	void cmpq(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(0x83, 7, dst, src); }

	void cmpb(MemAddress dst, Imm8  src){ encoder.putInstrBinaryMemImm!(ArgType.BYTE) (0x80, 7, dst, src); }
	void cmpw(MemAddress dst, Imm16 src){ encoder.putInstrBinaryMemImm!(ArgType.WORD) (0x81, 7, dst, src); }
	void cmpd(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.DWORD)(0x81, 7, dst, src); }
	void cmpq(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.QWORD)(0x81, 7, dst, src); }


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


	void nop() { encoder.sink.put(0x90); }

	// relative call to target virtual address.
	void call(Imm32 target) {
		int rip_offset = cast(int)(target.value - encoder.sink.length - 5); // relative to start of next instr
		encoder.sink.put(0xE8);
		encoder.sink.put(rip_offset);
	}

	void popw(Register dst)   { encoder.putInstrUnaryReg2!(ArgType.WORD )(0x58, dst); }
	void popq(Register dst)   { encoder.putInstrUnaryReg2!(ArgType.DWORD)(0x58, dst); } // use DWORD to omit REX.W
	void popw(MemAddress dst) { encoder.putInstrUnaryMem!( ArgType.WORD )(0x8F, 0, dst); }
	void popq(MemAddress dst) { encoder.putInstrUnaryMem!( ArgType.DWORD)(0x8F, 0, dst); } // use DWORD to omit REX.W

	void pushw(Register dst)   { encoder.putInstrUnaryReg2!(ArgType.WORD )(0x50, dst); }
	void pushq(Register dst)   { encoder.putInstrUnaryReg2!(ArgType.DWORD)(0x50, dst); } // use DWORD to omit REX.W
	void pushw(MemAddress dst) { encoder.putInstrUnaryMem!( ArgType.WORD )(0xFF, 6, dst); }
	void pushq(MemAddress dst) { encoder.putInstrUnaryMem!( ArgType.DWORD)(0xFF, 6, dst); } // use DWORD to omit REX.W

	void ret() { encoder.sink.put(0xC3); }
	void ret(Imm16 bytesToPop) {
		encoder.sink.put(0xC2);
		encoder.sink.put(bytesToPop);
	}

	void int3() { encoder.sink.put(0xCC); }
}

mixin template unaryInstr_Reg(string name, ubyte[2] opcodes, ubyte extraOpcode) {
	mixin(format("void %sb(Register dst) { encoder.putInstrUnaryReg1!(ArgType.BYTE) (%s, %s, dst); }", name, opcodes[0], extraOpcode));
	mixin(format("void %sw(Register dst) { encoder.putInstrUnaryReg1!(ArgType.WORD) (%s, %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sd(Register dst) { encoder.putInstrUnaryReg1!(ArgType.DWORD)(%s, %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sq(Register dst) { encoder.putInstrUnaryReg1!(ArgType.QWORD)(%s, %s, dst); }", name, opcodes[1], extraOpcode));
}
mixin template unaryInstr_Mem(string name, ubyte[2] opcodes, ubyte extraOpcode) {
	mixin(format("void %sb(MemAddress dst) { encoder.putInstrUnaryMem!(ArgType.BYTE) (%s, %s, dst); }", name, opcodes[0], extraOpcode));
	mixin(format("void %sw(MemAddress dst) { encoder.putInstrUnaryMem!(ArgType.WORD) (%s, %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sd(MemAddress dst) { encoder.putInstrUnaryMem!(ArgType.DWORD)(%s, %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sq(MemAddress dst) { encoder.putInstrUnaryMem!(ArgType.QWORD)(%s, %s, dst); }", name, opcodes[1], extraOpcode));
}
