/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module amd64asm;

enum Reg8  : ubyte {AL, CL, DL, BL, SPL,BPL,SIL,DIL,R8B,R9B,R10B,R11B,R12B,R13B,R14B,R15B}
enum Reg16 : ubyte {AX, CX, DX, BX, SP, BP, SI, DI, R8W,R9W,R10W,R11W,R12W,R13W,R14W,R15W}
enum Reg32 : ubyte {EAX,ECX,EDX,EBX,ESP,EBP,ESI,EDI,R8D,R9D,R10D,R11D,R12D,R13D,R14D,R15D}
enum Reg64 : ubyte {RAX,RCX,RDX,RBX,RSP,RBP,RSI,RDI,R8, R9, R10, R11, R12, R13, R14, R15 }
enum bool isAnyReg(R) = is(R == Reg64) || is(R == Reg32) || is(R == Reg16) || is(R == Reg8);

enum Reg64Max = cast(Reg64)(Reg64.max+1);
enum Reg32Max = cast(Reg32)(Reg32.max+1);
enum Reg16Max = cast(Reg16)(Reg16.max+1);
enum Reg8Max  = cast(Reg8 )(Reg8. max+1);

bool regNeedsRexPrefix(R)(R reg) { return false; }
bool regNeedsRexPrefix(Reg8 reg) { return reg >= 4; }


import std.string : format;
struct Imm8  { ubyte  value; alias RegT = Reg8 ; string toString(){ return format("0X%02X", value); } }
struct Imm16 { ushort value; alias RegT = Reg16; string toString(){ return format("0X%02X", value); } }
struct Imm32 { uint   value; alias RegT = Reg32; string toString(){ return format("0X%02X", value); } }
struct Imm64 { ulong  value; alias RegT = Reg64; string toString(){ return format("0X%02X", value); } }
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
ubyte regTo_Rex_W(R)(R reg) pure nothrow @nogc if(isAnyReg!R) { return (reg & 0b1000) >> 0; } // 0100 WRXB
ubyte regTo_Rex_R(R)(R reg) pure nothrow @nogc if(isAnyReg!R) { return (reg & 0b1000) >> 1; } // 0100 WRXB
ubyte regTo_Rex_X(R)(R reg) pure nothrow @nogc if(isAnyReg!R) { return (reg & 0b1000) >> 2; } // 0100 WRXB
ubyte regTo_Rex_B(R)(R reg) pure nothrow @nogc if(isAnyReg!R) { return (reg & 0b1000) >> 3; } // 0100 WRXB

// place 3 LSB of register into appropriate bit field of ModR/M byte
ubyte regTo_ModRm_Reg(R)(R reg) pure nothrow @nogc if(isAnyReg!R) { return (reg & 0b0111) << 3; }
ubyte regTo_ModRm_Rm(R)(R reg) pure nothrow @nogc if(isAnyReg!R) { return (reg & 0b0111) << 0; }

struct SibScale { ubyte bits; ubyte value() { return cast(ubyte)(1 << bits); } }
struct ModRmMod { ubyte bits; }

ubyte encodeSibByte(R)(SibScale ss, R index, R base) pure nothrow @nogc if(isAnyReg!R) {
	return cast(ubyte)(ss.bits << 6) | (index & 0b0111) << 3 | (base & 0b0111);
}

ubyte encodeModRegRmByte(R)(ModRmMod mod, R reg, R rm) pure nothrow @nogc if(isAnyReg!R) {
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
	Reg32 indexReg = Reg32.ESP;
	Reg32 baseReg = Reg32.EBP;
	SibScale scale;
	int disp32; // disp8 is stored here too
	byte disp8() @property { return cast(byte)(disp32 & 0xFF); }

	ubyte rexBits() { return regTo_Rex_X(indexReg) | regTo_Rex_B(baseReg); }
	ubyte modRmByte() { return encodeModRegRmByte(ModRmMod(memAddrType_to_mod[type]), cast(Reg32)0, Reg32.ESP); }
	ubyte sibByte() { return encodeSibByte(scale, indexReg, baseReg); }
	bool hasDisp32() { return memAddrType_to_dispType[type] == 1; }
	bool hasDisp8 () { return memAddrType_to_dispType[type] == 2; }

	string toString() {
		final switch(type) {
			case MemAddrType.disp32: return format("[0x%x]", disp32);
			case MemAddrType.indexDisp32: return format("[(%s*%s) + 0x%x]", indexReg, scale.value, disp32);
			case MemAddrType.base: return format("[%s]", baseReg);
			case MemAddrType.baseDisp32: return format("[%s + 0x%x]", baseReg, disp32);
			case MemAddrType.baseIndex: return format("[%s + (%s*%s)]", baseReg, indexReg, scale.value);
			case MemAddrType.baseIndexDisp32: return format("[%s + (%s*%s) + 0x%x]", baseReg, indexReg, scale.value, disp32);
			case MemAddrType.baseDisp8: return format("[0x%x]", disp8);
			case MemAddrType.baseIndexDisp8: return format("[(%s*%s) + 0x%x]", indexReg, scale.value, disp8);
		}
	}
}

MemAddress memAddrDisp32(uint disp32) {
	return MemAddress(MemAddrType.disp32, Reg32.ESP, Reg32.EBP, SibScale(), disp32); }
MemAddress memAddrIndexDisp32(Reg32 indexReg, SibScale scale, uint disp32) {
	return MemAddress(MemAddrType.indexDisp32, indexReg, Reg32.EBP, scale, disp32); }
MemAddress memAddrBase(Reg32 baseReg) {
	return MemAddress(MemAddrType.base, Reg32.ESP, baseReg); }
MemAddress memAddrBaseDisp32(Reg32 baseReg, uint disp32) {
	return MemAddress(MemAddrType.baseDisp32, Reg32.ESP, baseReg, SibScale(), disp32); }
MemAddress memAddrBaseIndex(Reg32 baseReg, Reg32 indexReg, SibScale scale) {
	return MemAddress(MemAddrType.baseIndex, indexReg, baseReg, scale); }
MemAddress memAddrBaseIndexDisp32(Reg32 baseReg, Reg32 indexReg, SibScale scale, uint disp32) {
	return MemAddress(MemAddrType.baseIndexDisp32, indexReg, baseReg, scale, disp32); }
MemAddress memAddrBaseDisp8(Reg32 baseReg, ubyte disp8) {
	return MemAddress(MemAddrType.baseDisp8, Reg32.ESP, baseReg, SibScale(), disp8); }
MemAddress memAddrBaseIndexDisp8(Reg32 baseReg, Reg32 indexReg, SibScale scale, ubyte disp8) {
	return MemAddress(MemAddrType.baseIndexDisp8, indexReg, baseReg, scale, disp8); }

struct BytePtr  { MemAddress addr; alias RegT = Reg8 ; alias addr this; }
struct WordPtr  { MemAddress addr; alias RegT = Reg16; alias addr this; }
struct DwordPtr { MemAddress addr; alias RegT = Reg32; alias addr this; }
struct QwordPtr { MemAddress addr; alias RegT = Reg64; alias addr this; }
enum bool isAnyPtr(M) = is(M == BytePtr) || is(M == WordPtr) || is(M == DwordPtr) || is(M == QwordPtr);


// Sink defines put(T) for ubyte, ubyte[], Imm8, Imm16, Imm32, Imm64
struct CodeGen_x86_64(Sink)
{
	Sink sink;

	private void putRexByteChecked(R)(ubyte bits, bool forceRex = false) {
		static if (is(R == Reg64))
			sink.put(REX_PREFIX | REX_W | bits);
		else
			if (bits || forceRex) sink.put(REX_PREFIX | bits);
	}
	private void putRexByte_RB(R)(R reg, R rm) if (isAnyReg!R) {
		putRexByteChecked!R(regTo_Rex_R(reg) | regTo_Rex_B(rm));
	}
	private void putRexByte_B(R)(R rm) if (isAnyReg!R) {
		putRexByteChecked!R(regTo_Rex_B(rm), regNeedsRexPrefix(rm));
	}
	private void putRexByte_RXB(R)(R r, R x, R b) if (isAnyReg!R) {
		putRexByteChecked!R(regTo_Rex_R(r) | regTo_Rex_X(x) | regTo_Rex_B(b), regNeedsRexPrefix(r));
	}

	private void putInstrRegReg(R)(ubyte opcode, R dst_rm, R src_reg) if (isAnyReg!R) {
		static if (is(R == Reg16)) sink.put(LegacyPrefix.OPERAND_SIZE);         // 16 bit operand prefix
		putRexByte_RB(src_reg, dst_rm);                                         // REX
		sink.put(opcode);                                                       // Opcode
		sink.put(encodeModRegRmByte(ModRmMod(0b11), src_reg, dst_rm));          // ModR/M
	}
	private void putInstrRegImm(R, I)(ubyte opcode, R dst_rm, I src_imm) if (isAnyReg!R && isAnyImm!I) {
		static assert(is(R == I.RegT), "Sizes of imm and reg must be equal");
		static if (is(R == Reg16)) sink.put(LegacyPrefix.OPERAND_SIZE);         // 16 bit operand prefix
		putRexByte_B(dst_rm);                                                   // REX
		sink.put(opcode | (dst_rm & 0b0111));                                   // Opcode
		sink.put(src_imm);                                                      // Imm
	}
	private void putInstrRegMem(R, M)(ubyte opcode, R dst_r, M src_mem) if (isAnyReg!R && isAnyPtr!M) {
		static assert(is(R == M.RegT), "Sizes of reg and ptr must be equal");
		static if (is(R == Reg16)) sink.put(LegacyPrefix.OPERAND_SIZE);         // 16 bit operand prefix
		putRexByte_RXB(dst_r, cast(R)src_mem.indexReg, cast(R)src_mem.baseReg); // REX
		sink.put(opcode);                                                       // Opcode
		sink.put(src_mem.modRmByte | (dst_r & 0b0111) << 3);                    // ModR/M
		sink.put(src_mem.sibByte);                                              // SIB
		if (src_mem.hasDisp32)
			sink.put(Imm32(src_mem.disp32));                                    // disp32
		else if (src_mem.hasDisp8)
			sink.put(src_mem.disp8);                                            // disp8
	}
	private void putInstrMemImm(M, I)(ubyte opcode, M dst_mem, I src_imm) if (isAnyPtr!M && isAnyImm!I) {
		static assert( // allow special case of QwordPtr and Imm32
			is(M.RegT == I.RegT) || ( is(M == QwordPtr) && is(I == Imm32) ),
			"Sizes of ptr and imm must be equal");
		static if (is(M == WordPtr)) sink.put(LegacyPrefix.OPERAND_SIZE);     // 16 bit operand prefix
		putRexByte_RXB(cast(M.RegT)0, cast(M.RegT)dst_mem.indexReg, cast(M.RegT)dst_mem.baseReg);// REX
		sink.put(opcode);                                                       // Opcode
		sink.put(dst_mem.modRmByte);                                            // ModR/M
		sink.put(dst_mem.sibByte);                                              // SIB
		if (dst_mem.hasDisp32)
			sink.put(Imm32(dst_mem.disp32));                                    // disp32
		else if (dst_mem.hasDisp8)
			sink.put(dst_mem.disp8);                                            // disp8
		sink.put(src_imm);                                                      // Mem8/16/32 Imm8/16/32
	}

	void beginFunction() {
		push(Reg64.RBP);
		mov(Reg64.RBP, Reg64.RSP);
	}
	void endFunction() {
		pop(Reg64.RBP);
		ret();
	}

	void mov(Reg8  dst_rm, Reg8  src_reg) { putInstrRegReg(0x88, dst_rm, src_reg); }
	void mov(Reg16 dst_rm, Reg16 src_reg) { putInstrRegReg(0x89, dst_rm, src_reg); }
	void mov(Reg32 dst_rm, Reg32 src_reg) { putInstrRegReg(0x89, dst_rm, src_reg); }
	void mov(Reg64 dst_rm, Reg64 src_reg) { putInstrRegReg(0x89, dst_rm, src_reg); }

	void mov(Reg8  dst_rm, Imm8  src_imm) { putInstrRegImm(0xB0, dst_rm, src_imm); }
	void mov(Reg16 dst_rm, Imm16 src_imm) { putInstrRegImm(0xB8, dst_rm, src_imm); }
	void mov(Reg32 dst_rm, Imm32 src_imm) { putInstrRegImm(0xB8, dst_rm, src_imm); }
	void mov(Reg64 dst_rm, Imm64 src_imm) { putInstrRegImm(0xB8, dst_rm, src_imm); }

	void mov(Reg8  dst_reg, BytePtr  src_rm) { putInstrRegMem(0x8A, dst_reg, src_rm); }
	void mov(Reg16 dst_reg, WordPtr  src_rm) { putInstrRegMem(0x8B, dst_reg, src_rm); }
	void mov(Reg32 dst_reg, DwordPtr src_rm) { putInstrRegMem(0x8B, dst_reg, src_rm); }
	void mov(Reg64 dst_reg, QwordPtr src_rm) { putInstrRegMem(0x8B, dst_reg, src_rm); }

	void mov(BytePtr  dst_rm, Reg8  src_reg) { putInstrRegMem(0x88, src_reg, dst_rm); }
	void mov(WordPtr  dst_rm, Reg16 src_reg) { putInstrRegMem(0x89, src_reg, dst_rm); }
	void mov(DwordPtr dst_rm, Reg32 src_reg) { putInstrRegMem(0x89, src_reg, dst_rm); }
	void mov(QwordPtr dst_rm, Reg64 src_reg) { putInstrRegMem(0x89, src_reg, dst_rm); }

	void mov(BytePtr  dst_rm, Imm8  src_imm) { putInstrMemImm(0xC6, dst_rm, src_imm); }
	void mov(WordPtr  dst_rm, Imm16 src_imm) { putInstrMemImm(0xC7, dst_rm, src_imm); }
	void mov(DwordPtr dst_rm, Imm32 src_imm) { putInstrMemImm(0xC7, dst_rm, src_imm); }
	void mov(QwordPtr dst_rm, Imm32 src_imm) { putInstrMemImm(0xC7, dst_rm, src_imm); }

	void ret() { sink.put(0xC3); }
	//void ret(uint retValue) { sink.put(0xC3); }
	void nop() { sink.put(0x90); }

	void push(Reg64 reg) {
		if (reg > Reg64.RDI) sink.put(0x41); // REX prefix
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
	void pop(Reg64 reg) { // 32 64
		if (reg > Reg64.RDI) sink.put(0x41); // REX prefix
		sink.put(0x58 | reg); // opcode
	}
	void int3() { // 32 64
		sink.put(0xCC);
	}
}
