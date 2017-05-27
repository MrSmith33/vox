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

// Sink defines put(T) for ubyte, ubyte[], Imm8, Imm16, Imm32, Imm64
struct CodeGen_x86_64(Sink)
{
	Sink sink;

	private void putRexByteChecked(ArgType argType)(ubyte bits, bool forceRex = false) {
		//writefln("REX bits %08b force %s", bits, forceRex);
		static if (argType == ArgType.QWORD)
			sink.put(REX_PREFIX | REX_W | bits);
		else
			if (bits || forceRex) sink.put(REX_PREFIX | bits);
	}
	private void putRexByte_RB(ArgType argType)(Register reg, Register rm) { // reg reg
		putRexByteChecked!argType(regTo_Rex_R(reg) | regTo_Rex_B(rm), regNeedsRexPrefix!argType(reg) || regNeedsRexPrefix!argType(rm)); }
	private void putRexByte_rB(ArgType argType)(Register rm) { // R.R/M reg
		putRexByteChecked!argType(regTo_Rex_B(rm), regNeedsRexPrefix!argType(rm)); } // ensures REX prefix for ah ch dh bh
	private void putRexByte_B(ArgType argType)(Register base) { // base
		putRexByteChecked!argType(regTo_Rex_B(base)); }
	private void putRexByte_RXB(ArgType argType)(Register r, Register index, Register base) { // reg index base
		putRexByteChecked!argType(regTo_Rex_R(r) | regTo_Rex_X(index) | regTo_Rex_B(base), regNeedsRexPrefix!argType(r)); }
	private void putRexByte_XB(ArgType argType)(Register index, Register base) { // index base
		putRexByteChecked!argType(regTo_Rex_X(index) | regTo_Rex_B(base)); }

	private void putInstrBinaryRegReg(ArgType argType)(ubyte opcode, Register dst_rm, Register src_reg) {
		static if (argType == ArgType.WORD) sink.put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_RB!argType(src_reg, dst_rm);                                 // REX
		sink.put(opcode);                                                       // Opcode
		sink.put(encodeModRegRmByte(ModRmMod(0b11), src_reg, dst_rm));          // ModR/M
	}
	private void putInstrBinaryRegImm(ArgType argType, I)(ubyte opcode, Register dst_rm, I src_imm) if (isAnyImm!I) {
		static assert(argType == I.argT, "Sizes of imm and reg must be equal");
		static if (argType == ArgType.WORD) sink.put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_rB!argType(dst_rm);                                          // REX
		sink.put(opcode | (dst_rm & 0b0111));                                   // Opcode | reg
		sink.put(src_imm);                                                      // Imm8/16/32/64
	}
	private void putInstrBinaryRegMem(ArgType argType)(ubyte opcode, Register dst_r, MemAddress src_mem) {
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
	private void putInstrBinaryOpMem(ArgType argType)(ubyte opcode, Register regOpcode, MemAddress src_mem) {
		static if (argType == ArgType.WORD) sink.put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_XB!argType(src_mem.indexReg, src_mem.baseReg);               // REX
		sink.put(opcode);                                                       // Opcode
		sink.put(src_mem.modRmByte(regOpcode));                                 // ModR/M
		sink.put(src_mem.sibByte);                                              // SIB
		if (src_mem.hasDisp32)
			sink.put(src_mem.disp32);                                           // disp32
		else if (src_mem.hasDisp8)
			sink.put(src_mem.disp8);                                            // disp8
	}
	private void putInstrBinaryMemImm(ArgType argType, I)(ubyte opcode, MemAddress dst_mem, I src_imm) if (isAnyImm!I) {
		static assert( // allow special case of QwordPtr and Imm32
			argType == I.argT || (argType == ArgType.QWORD && I.argT == ArgType.DWORD),
			"Sizes of ptr and imm must be equal");
		static if (argType == ArgType.WORD) sink.put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_XB!argType(dst_mem.indexReg, dst_mem.baseReg);               // REX
		sink.put(opcode);                                                       // Opcode
		sink.put(dst_mem.modRmByte);                                            // ModR/M
		sink.put(dst_mem.sibByte);                                              // SIB
		if (dst_mem.hasDisp32) sink.put(dst_mem.disp32);                        // disp32
		else if (dst_mem.hasDisp8) sink.put(dst_mem.disp8);                     //   or disp8
		sink.put(src_imm);                                                      // Imm8/16/32
	}

	private void putInstrUnaryRegOpMod(ArgType argType)(ubyte opcode, Register dst_rm, Register regOpcode, ModRmMod mod) {
		static if (argType == ArgType.WORD) sink.put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_rB!argType(dst_rm);                                          // REX
		sink.put(opcode);                                                       // Opcode
		sink.put(encodeModRegRmByte(mod, regOpcode, dst_rm));                   // ModR/M
	}
	private void putInstrUnaryMemOpMod(ArgType argType)(ubyte opcode, Register base, Register regOpcode, ModRmMod mod) {
		static if (argType == ArgType.WORD) sink.put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_B!argType(base);                                             // REX
		sink.put(opcode);                                                       // Opcode
		sink.put(encodeModRegRmByte(mod, regOpcode, base));                     // ModR/M
	}
	private void putInstrUnaryReg(ArgType argType)(ubyte opcode, ubyte regOpcode, Register dst_rm) {
		putInstrUnaryRegOpMod!argType(opcode, dst_rm, cast(Register)regOpcode, ModRmMod(0b11));
	}
	private void putInstrUnaryMem(ArgType argType)(ubyte opcode, ubyte regOpcode, MemAddress dst_mem) {
		switch(dst_mem.type) {
			//case MemAddrType.disp32: // 1 TODO, implement RIP relative addressing
				//putInstrUnaryMemOpMod!argType(opcode, Register.BP, cast(Register)regOpcode, dst_mem.mod);
				//sink.put(dst_mem.disp32);
				//return;
			case MemAddrType.base: // 3
				if (is_SP_or_R12(dst_mem.baseReg)) break; // fallback to variant 3 [base] with SIB byte
				if (is_BP_or_R13(dst_mem.baseReg)) { // fallback to variant 7 [base + 0] for BP and R13
					dst_mem = memAddrBaseDisp8(dst_mem.baseReg, 0); // [base] -> [base + 0]
					goto case MemAddrType.baseDisp8; // goto var 7
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

	void beginFunction() {
		push(Register.BP);
		movq(Register.BP, Register.SP);
	}
	void endFunction() {
		pop(Register.BP);
		ret();
	}

	mixin staticBinaryInstr!"add";
	mixin binaryInstr_Mem_Reg!("add", [0x00,0x01]);
	mixin binaryInstr_Reg_Mem!("add", [0x02,0x03]);
	mixin binaryInstr_Reg_Reg!("add", [0x04,0x05]);
	mixin binaryInstr_Reg_Imm!("add", [0x80,0x81]);
	mixin binaryInstr_Mem_Imm!("add", [0x80,0x81]);

	mixin unaryInstr_Reg!("div", [0xF6,0xF7], 6);
	mixin unaryInstr_Mem!("div", [0xF6,0xF7], 6);

	mixin staticBinaryInstr!"mov";
	mixin binaryInstr_Reg_Reg!("mov", [0x88,0x89]);
	mixin binaryInstr_Reg_Imm!("mov", [0xB0,0xB8]);
	mixin binaryInstr_Reg_Mem!("mov", [0x8A,0x8B]);
	mixin binaryInstr_Mem_Reg!("mov", [0x88,0x89]);
	mixin binaryInstr_Mem_Imm!("mov", [0xC6,0xC7]);

	mixin unaryInstr_Reg!("inc", [0xFE,0xFF], 0);
	mixin unaryInstr_Mem!("inc", [0xFE,0xFF], 0);

	mixin unaryInstr_Reg!("dec", [0xFE,0xFF], 1);
	mixin unaryInstr_Mem!("dec", [0xFE,0xFF], 1);

	mixin unaryInstr_Reg!("mul", [0xF6,0xF7], 4);
	mixin unaryInstr_Mem!("mul", [0xF6,0xF7], 4);

	mixin unaryInstr_Reg!("not", [0xF6,0xF7], 2);
	mixin unaryInstr_Mem!("not", [0xF6,0xF7], 2);

	void nop() { sink.put(0x90); }

	//void pop(Register dst_rm) {}
	//void pop(MemAddress dst_rm) {}
	//void push(Register dst_rm) {}
	//void push(MemAddress dst_rm) {}

	void ret() { sink.put(0xC3); }
	void ret(Imm16 bytesToPop) { sink.put(0xC2); sink.put(bytesToPop); }

	void push(Register reg) {
		if (reg > Register.DI) sink.put(0x41); // REX prefix
		sink.put(0x50 | (reg & 0b0111));     // Opcode
	}
	void push(Imm8 imm8) { // 32 64
		sink.put(0x6A);
		sink.put(imm8);
	}
	void push(Imm16 imm8) { // 32 64
		sink.put(0x6A);
		sink.put(imm8);
	}
	void pop(Register reg) { // 32 64
		if (reg > Register.DI) sink.put(0x41); // REX prefix
		sink.put(0x58 | reg); // opcode
	}
	void int3() { // 32 64
		sink.put(0xCC);
	}
}

mixin template staticBinaryInstr(string name)
{
	mixin("void "~name~"(Arg1, Arg2)(Arg1 dst, Arg2 src, ArgType argType) {
		final switch(argType) {
			case ArgType.BYTE:  "~name~"b(dst, src); break;
			case ArgType.WORD:  "~name~"w(dst, src); break;
			case ArgType.DWORD: "~name~"d(dst, src); break;
			case ArgType.QWORD: "~name~"q(dst, src); break; }}");
}

mixin template unaryInstr_Reg(string name, ubyte[2] opcodes, ubyte extraOpcode) {
	mixin(format("void %sb(Register dst) { putInstrUnaryReg!(ArgType.BYTE) (%s, %s, dst); }", name, opcodes[0], extraOpcode));
	mixin(format("void %sw(Register dst) { putInstrUnaryReg!(ArgType.WORD) (%s, %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sd(Register dst) { putInstrUnaryReg!(ArgType.DWORD)(%s, %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sq(Register dst) { putInstrUnaryReg!(ArgType.QWORD)(%s, %s, dst); }", name, opcodes[1], extraOpcode));
}
mixin template unaryInstr_Mem(string name, ubyte[2] opcodes, ubyte extraOpcode) {
	mixin(format("void %sb(MemAddress dst) { putInstrUnaryMem!(ArgType.BYTE) (%s, %s, dst); }", name, opcodes[0], extraOpcode));
	mixin(format("void %sw(MemAddress dst) { putInstrUnaryMem!(ArgType.WORD) (%s, %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sd(MemAddress dst) { putInstrUnaryMem!(ArgType.DWORD)(%s, %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sq(MemAddress dst) { putInstrUnaryMem!(ArgType.QWORD)(%s, %s, dst); }", name, opcodes[1], extraOpcode));
}

mixin template binaryInstr_Reg_Reg(string name, ubyte[2] opcodes) {
	mixin(format("void %sb(Register dst, Register src){ putInstrBinaryRegReg!(ArgType.BYTE) (%s, dst, src); }", name, opcodes[0]));
	mixin(format("void %sw(Register dst, Register src){ putInstrBinaryRegReg!(ArgType.WORD) (%s, dst, src); }", name, opcodes[1]));
	mixin(format("void %sd(Register dst, Register src){ putInstrBinaryRegReg!(ArgType.DWORD)(%s, dst, src); }", name, opcodes[1]));
	mixin(format("void %sq(Register dst, Register src){ putInstrBinaryRegReg!(ArgType.QWORD)(%s, dst, src); }", name, opcodes[1]));
}
mixin template binaryInstr_Reg_Imm(string name, ubyte[2] opcodes) {
	mixin(format("void %sb(Register dst, Imm8  src){ putInstrBinaryRegImm!(ArgType.BYTE) (%s, dst, src); }", name, opcodes[0]));
	mixin(format("void %sw(Register dst, Imm16 src){ putInstrBinaryRegImm!(ArgType.WORD) (%s, dst, src); }", name, opcodes[1]));
	mixin(format("void %sd(Register dst, Imm32 src){ putInstrBinaryRegImm!(ArgType.DWORD)(%s, dst, src); }", name, opcodes[1]));
	mixin(format("void %sq(Register dst, Imm64 src){ putInstrBinaryRegImm!(ArgType.QWORD)(%s, dst, src); }", name, opcodes[1]));
}
mixin template binaryInstr_Reg_Mem(string name, ubyte[2] opcodes) {
	mixin(format("void %sb(Register dst, MemAddress src){ putInstrBinaryRegMem!(ArgType.BYTE) (%s, dst, src); }", name, opcodes[0]));
	mixin(format("void %sw(Register dst, MemAddress src){ putInstrBinaryRegMem!(ArgType.WORD) (%s, dst, src); }", name, opcodes[1]));
	mixin(format("void %sd(Register dst, MemAddress src){ putInstrBinaryRegMem!(ArgType.DWORD)(%s, dst, src); }", name, opcodes[1]));
	mixin(format("void %sq(Register dst, MemAddress src){ putInstrBinaryRegMem!(ArgType.QWORD)(%s, dst, src); }", name, opcodes[1]));
}
mixin template binaryInstr_Mem_Reg(string name, ubyte[2] opcodes) {
	mixin(format("void %sb(MemAddress dst, Register src){ putInstrBinaryRegMem!(ArgType.BYTE) (%s, src, dst); }", name, opcodes[0]));
	mixin(format("void %sw(MemAddress dst, Register src){ putInstrBinaryRegMem!(ArgType.WORD) (%s, src, dst); }", name, opcodes[1]));
	mixin(format("void %sd(MemAddress dst, Register src){ putInstrBinaryRegMem!(ArgType.DWORD)(%s, src, dst); }", name, opcodes[1]));
	mixin(format("void %sq(MemAddress dst, Register src){ putInstrBinaryRegMem!(ArgType.QWORD)(%s, src, dst); }", name, opcodes[1]));
}
mixin template binaryInstr_Mem_Imm(string name, ubyte[2] opcodes) {
	mixin(format("void %sb(MemAddress dst, Imm8  src){ putInstrBinaryMemImm!(ArgType.BYTE) (%s, dst, src); }", name, opcodes[0]));
	mixin(format("void %sw(MemAddress dst, Imm16 src){ putInstrBinaryMemImm!(ArgType.WORD) (%s, dst, src); }", name, opcodes[1]));
	mixin(format("void %sd(MemAddress dst, Imm32 src){ putInstrBinaryMemImm!(ArgType.DWORD)(%s, dst, src); }", name, opcodes[1]));
	mixin(format("void %sq(MemAddress dst, Imm32 src){ putInstrBinaryMemImm!(ArgType.QWORD)(%s, dst, src); }", name, opcodes[1])); // special imm32
}
