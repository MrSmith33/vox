/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Parts that are ported from LLVM: llvm/DebugInfo/CodeView/CodeView.h.

/// CV constants
///
module be.debug_info.pdb.codeview;

import be.debug_info.pdb : StreamReader;

// enum CV_CPU_TYPE_e
// https://docs.microsoft.com/en-us/visualstudio/debugger/debug-interface-access/cv-cpu-type-e
enum CV_CPUType : ushort {
	Intel8080 = 0x0,
	Intel8086 = 0x1,
	Intel80286 = 0x2,
	Intel80386 = 0x3,
	Intel80486 = 0x4,
	Pentium = 0x5,
	PentiumPro = 0x6,
	Pentium3 = 0x7,
	MIPS = 0x10,
	MIPS16 = 0x11,
	MIPS32 = 0x12,
	MIPS64 = 0x13,
	MIPSI = 0x14,
	MIPSII = 0x15,
	MIPSIII = 0x16,
	MIPSIV = 0x17,
	MIPSV = 0x18,
	M68000 = 0x20,
	M68010 = 0x21,
	M68020 = 0x22,
	M68030 = 0x23,
	M68040 = 0x24,
	Alpha = 0x30,
	Alpha21164 = 0x31,
	Alpha21164A = 0x32,
	Alpha21264 = 0x33,
	Alpha21364 = 0x34,
	PPC601 = 0x40,
	PPC603 = 0x41,
	PPC604 = 0x42,
	PPC620 = 0x43,
	PPCFP = 0x44,
	PPCBE = 0x45,
	SH3 = 0x50,
	SH3E = 0x51,
	SH3DSP = 0x52,
	SH4 = 0x53,
	SHMedia = 0x54,
	ARM3 = 0x60,
	ARM4 = 0x61,
	ARM4T = 0x62,
	ARM5 = 0x63,
	ARM5T = 0x64,
	ARM6 = 0x65,
	ARM_XMAC = 0x66,
	ARM_WMMX = 0x67,
	ARM7 = 0x68,
	ARM64 = 0x69,
	Omni = 0x70,
	Ia64 = 0x80,
	Ia64_2 = 0x81,
	CEE = 0x90,
	AM33 = 0xa0,
	M32R = 0xb0,
	TriCore = 0xc0,
	X64 = 0xd0,
	EBC = 0xe0,
	Thumb = 0xf0,
	ARMNT = 0xf4,
	D3D11_Shader = 0x100,
}

/// enum CV_CFL_LANG
/// https://docs.microsoft.com/en-us/visualstudio/debugger/debug-interface-access/cv-cfl-lang
enum CV_SourceLanguage : ubyte {
  C = 0x00,
  Cpp = 0x01,
  Fortran = 0x02,
  Masm = 0x03,
  Pascal = 0x04,
  Basic = 0x05,
  Cobol = 0x06,
  Link = 0x07,
  Cvtres = 0x08,
  Cvtpgd = 0x09,
  CSharp = 0x0a,
  VB = 0x0b,
  ILAsm = 0x0c,
  Java = 0x0d,
  JScript = 0x0e,
  MSIL = 0x0f,
  HLSL = 0x10,
  D = 'D',
};


enum CV_CallConv : ubyte {
	NEAR_C      = 0x00, // near right to left push, caller pops stack
	FAR_C       = 0x01, // far right to left push, caller pops stack
	NEAR_PASCAL = 0x02, // near left to right push, callee pops stack
	FAR_PASCAL  = 0x03, // far left to right push, callee pops stack
	NEAR_FAST   = 0x04, // near left to right push with regs, callee pops stack
	FAR_FAST    = 0x05, // far left to right push with regs, callee pops stack
	SKIPPED     = 0x06, // skipped (unused) call index
	NEAR_STD    = 0x07, // near standard call
	FAR_STD     = 0x08, // far standard call
	NEAR_SYS    = 0x09, // near sys call
	FAR_SYS     = 0x0a, // far sys call
	THISCALL    = 0x0b, // this call (this passed in register)
	MIPSCALL    = 0x0c, // Mips call
	GENERIC     = 0x0d, // Generic call sequence
	ALPHACALL   = 0x0e, // Alpha call
	PPCCALL     = 0x0f, // PPC call
	SHCALL      = 0x10, // Hitachi SuperH call
	ARMCALL     = 0x11, // ARM call
	AM33CALL    = 0x12, // AM33 call
	TRICALL     = 0x13, // TriCore Call
	SH5CALL     = 0x14, // Hitachi SuperH-5 call
	M32RCALL    = 0x15, // M32R Call
	CLRCALL     = 0x16, // clr call
	INLINE      = 0x17, // Marker for routines always inlined and thus lacking a convention
	NEAR_VECTOR = 0x18, // near left to right push with regs, callee pops stack
	RESERVED    = 0x19  // first unused call enumeration

	// Do NOT add any more machine specific conventions.  This is to be used for
	// calling conventions in the source only (e.g. __cdecl, __stdcall).
}

// enum TRAMP_e
// Trampoline subtype
enum TrampolineType : ushort {
	TrampIncremental, // incremental thunks
	BranchIsland // Branch island thunks
}

// enum THUNK_ORDINAL
enum ThunkOrdinal : ubyte {
	standard,
	thisAdjustor,
	virtualCall,
	pcode,
	jumpDestLoad,
	// trampoline thunk ordinals - only for use in Trampoline thunk symbols
	trampIncremental,
	branchIsland,
};

// enum CV_HREG_e
// llvm: CodeViewRegisters.def
enum RegisterId : ushort {
	ERR = 30000,
	TEB = 30001,
	TIMER = 30002,
	EFAD1 = 30003,
	EFAD2 = 30004,
	EFAD3 = 30005,
	VFRAME = 30006,
	HANDLE = 30007,
	PARAMS = 30008,
	LOCALS = 30009,
	TID = 30010,
	ENV = 30011,
	CMDLN = 30012,

	NONE = 0,
	AL = 1,
	CL = 2,
	DL = 3,
	BL = 4,
	AH = 5,
	CH = 6,
	DH = 7,
	BH = 8,
	AX = 9,
	CX = 10,
	DX = 11,
	BX = 12,
	SP = 13,
	BP = 14,
	SI = 15,
	DI = 16,
	EAX = 17,
	ECX = 18,
	EDX = 19,
	EBX = 20,
	ESP = 21,
	EBP = 22,
	ESI = 23,
	EDI = 24,
	ES = 25,
	CS = 26,
	SS = 27,
	DS = 28,
	FS = 29,
	GS = 30,
	IP = 31,
	FLAGS = 32,
	EIP = 33,
	EFLAGS = 34,
	TEMP = 40,
	TEMPH = 41,
	QUOTE = 42,
	PCDR3 = 43,
	PCDR4 = 44,
	PCDR5 = 45,
	PCDR6 = 46,
	PCDR7 = 47,
	CR0 = 80,
	CR1 = 81,
	CR2 = 82,
	CR3 = 83,
	CR4 = 84,
	DR0 = 90,
	DR1 = 91,
	DR2 = 92,
	DR3 = 93,
	DR4 = 94,
	DR5 = 95,
	DR6 = 96,
	DR7 = 97,
	GDTR = 110,
	GDTL = 111,
	IDTR = 112,
	IDTL = 113,
	LDTR = 114,
	TR = 115,

	PSEUDO1 = 116,
	PSEUDO2 = 117,
	PSEUDO3 = 118,
	PSEUDO4 = 119,
	PSEUDO5 = 120,
	PSEUDO6 = 121,
	PSEUDO7 = 122,
	PSEUDO8 = 123,
	PSEUDO9 = 124,

	ST0 = 128,
	ST1 = 129,
	ST2 = 130,
	ST3 = 131,
	ST4 = 132,
	ST5 = 133,
	ST6 = 134,
	ST7 = 135,
	CTRL = 136,
	STAT = 137,
	TAG = 138,
	FPIP = 139,
	FPCS = 140,
	FPDO = 141,
	FPDS = 142,
	ISEM = 143,
	FPEIP = 144,
	FPEDO = 145,

	MM0 = 146,
	MM1 = 147,
	MM2 = 148,
	MM3 = 149,
	MM4 = 150,
	MM5 = 151,
	MM6 = 152,
	MM7 = 153,

	XMM0 = 154,
	XMM1 = 155,
	XMM2 = 156,
	XMM3 = 157,
	XMM4 = 158,
	XMM5 = 159,
	XMM6 = 160,
	XMM7 = 161,

	MXCSR = 211,

	EDXEAX = 212,

	EMM0L = 220,
	EMM1L = 221,
	EMM2L = 222,
	EMM3L = 223,
	EMM4L = 224,
	EMM5L = 225,
	EMM6L = 226,
	EMM7L = 227,

	EMM0H = 228,
	EMM1H = 229,
	EMM2H = 230,
	EMM3H = 231,
	EMM4H = 232,
	EMM5H = 233,
	EMM6H = 234,
	EMM7H = 235,

	MM00 = 236,
	MM01 = 237,
	MM10 = 238,
	MM11 = 239,
	MM20 = 240,
	MM21 = 241,
	MM30 = 242,
	MM31 = 243,
	MM40 = 244,
	MM41 = 245,
	MM50 = 246,
	MM51 = 247,
	MM60 = 248,
	MM61 = 249,
	MM70 = 250,
	MM71 = 251,

	BND0 = 396,
	BND1 = 397,
	BND2 = 398,


	XMM8 = 252,
	XMM9 = 253,
	XMM10 = 254,
	XMM11 = 255,
	XMM12 = 256,
	XMM13 = 257,
	XMM14 = 258,
	XMM15 = 259,


	SIL = 324,
	DIL = 325,
	BPL = 326,
	SPL = 327,

	RAX = 328,
	RBX = 329,
	RCX = 330,
	RDX = 331,
	RSI = 332,
	RDI = 333,
	RBP = 334,
	RSP = 335,

	R8 = 336,
	R9 = 337,
	R10 = 338,
	R11 = 339,
	R12 = 340,
	R13 = 341,
	R14 = 342,
	R15 = 343,

	R8B = 344,
	R9B = 345,
	R10B = 346,
	R11B = 347,
	R12B = 348,
	R13B = 349,
	R14B = 350,
	R15B = 351,

	R8W = 352,
	R9W = 353,
	R10W = 354,
	R11W = 355,
	R12W = 356,
	R13W = 357,
	R14W = 358,
	R15W = 359,

	R8D = 360,
	R9D = 361,
	R10D = 362,
	R11D = 363,
	R12D = 364,
	R13D = 365,
	R14D = 366,
	R15D = 367,

	// cvconst.h defines both CV_REG_YMM0 (252) and CV_AMD64_YMM0 (368). Keep the
	// original prefix to distinguish them.
	AMD64_YMM0 = 368,
	AMD64_YMM1 = 369,
	AMD64_YMM2 = 370,
	AMD64_YMM3 = 371,
	AMD64_YMM4 = 372,
	AMD64_YMM5 = 373,
	AMD64_YMM6 = 374,
	AMD64_YMM7 = 375,
	AMD64_YMM8 = 376,
	AMD64_YMM9 = 377,
	AMD64_YMM10 = 378,
	AMD64_YMM11 = 379,
	AMD64_YMM12 = 380,
	AMD64_YMM13 = 381,
	AMD64_YMM14 = 382,
	AMD64_YMM15 = 383,
}


enum CV_TYPE : ushort {
/*
TYPE_RECORD(LF_POINTER, 0x1002, Pointer)
TYPE_RECORD(LF_MODIFIER, 0x1001, Modifier)
TYPE_RECORD(LF_PROCEDURE, 0x1008, Procedure)
TYPE_RECORD(LF_MFUNCTION, 0x1009, MemberFunction)
TYPE_RECORD(LF_LABEL, 0x000e, Label)
TYPE_RECORD(LF_ARGLIST, 0x1201, ArgList)

TYPE_RECORD(LF_FIELDLIST, 0x1203, FieldList)

TYPE_RECORD(LF_ARRAY, 0x1503, Array)
TYPE_RECORD(LF_CLASS, 0x1504, Class)
TYPE_RECORD_ALIAS(LF_STRUCTURE, 0x1505, Struct, Class)
TYPE_RECORD_ALIAS(LF_INTERFACE, 0x1519, Interface, Class)
TYPE_RECORD(LF_UNION, 0x1506, Union)
TYPE_RECORD(LF_ENUM, 0x1507, Enum)
TYPE_RECORD(LF_TYPESERVER2, 0x1515, TypeServer2)
TYPE_RECORD(LF_VFTABLE, 0x151d, VFTable)
TYPE_RECORD(LF_VTSHAPE, 0x000a, VFTableShape)

TYPE_RECORD(LF_BITFIELD, 0x1205, BitField)

// Member type records. These are generally not length prefixed, and appear
// inside of a field list record.
MEMBER_RECORD(LF_BCLASS, 0x1400, BaseClass)
MEMBER_RECORD_ALIAS(LF_BINTERFACE, 0x151a, BaseInterface, BaseClass)

MEMBER_RECORD(LF_VBCLASS, 0x1401, VirtualBaseClass)
MEMBER_RECORD_ALIAS(LF_IVBCLASS, 0x1402, IndirectVirtualBaseClass,
					VirtualBaseClass)

MEMBER_RECORD(LF_VFUNCTAB, 0x1409, VFPtr)
MEMBER_RECORD(LF_STMEMBER, 0x150e, StaticDataMember)
MEMBER_RECORD(LF_METHOD, 0x150f, OverloadedMethod)
MEMBER_RECORD(LF_MEMBER, 0x150d, DataMember)
MEMBER_RECORD(LF_NESTTYPE, 0x1510, NestedType)
MEMBER_RECORD(LF_ONEMETHOD, 0x1511, OneMethod)
MEMBER_RECORD(LF_ENUMERATE, 0x1502, Enumerator)
MEMBER_RECORD(LF_INDEX, 0x1404, ListContinuation)

// ID leaf records. Subsequent leaf types may be referenced from .debug$S.
TYPE_RECORD(LF_FUNC_ID, 0x1601, FuncId)
TYPE_RECORD(LF_MFUNC_ID, 0x1602, MemberFuncId)
TYPE_RECORD(LF_BUILDINFO, 0x1603, BuildInfo)
TYPE_RECORD(LF_SUBSTR_LIST, 0x1604, StringList)
TYPE_RECORD(LF_STRING_ID, 0x1605, StringId)
TYPE_RECORD(LF_UDT_SRC_LINE, 0x1606, UdtSourceLine)
TYPE_RECORD(LF_UDT_MOD_SRC_LINE, 0x1607, UdtModSourceLine)


TYPE_RECORD(LF_METHODLIST, 0x1206, MethodOverloadList)
*/
	// 16 bit type records.
	LF_MODIFIER_16t = 0x0001,
	LF_POINTER_16t = 0x0002,
	LF_ARRAY_16t = 0x0003,
	LF_CLASS_16t = 0x0004,
	LF_STRUCTURE_16t = 0x0005,
	LF_UNION_16t = 0x0006,
	LF_ENUM_16t = 0x0007,
	LF_PROCEDURE_16t = 0x0008,
	LF_MFUNCTION_16t = 0x0009,
	LF_COBOL0_16t = 0x000b,
	LF_COBOL1 = 0x000c,
	LF_BARRAY_16t = 0x000d,
	LF_NULLLEAF = 0x000f, // LF_NUL
	LF_NOTTRAN = 0x0010,
	LF_DIMARRAY_16t = 0x0011,
	LF_VFTPATH_16t = 0x0012,
	LF_PRECOMP_16t = 0x0013,
	LF_ENDPRECOMP = 0x0014,
	LF_OEM_16t = 0x0015,
	LF_TYPESERVER_ST = 0x0016,

	LF_SKIP_16t = 0x0200,
	LF_ARGLIST_16t = 0x0201,
	LF_DEFARG_16t = 0x0202,
	LF_LIST = 0x0203,
	LF_FIELDLIST_16t = 0x0204,
	LF_DERIVED_16t = 0x0205,
	LF_BITFIELD_16t = 0x0206,
	LF_METHODLIST_16t = 0x0207,
	LF_DIMCONU_16t = 0x0208,
	LF_DIMCONLU_16t = 0x0209,
	LF_DIMVARU_16t = 0x020a,
	LF_DIMVARLU_16t = 0x020b,
	LF_REFSYM = 0x020c,

	// 16 bit member types. Generally not length prefixed.
	LF_BCLASS_16t = 0x0400,
	LF_VBCLASS_16t = 0x0401,
	LF_IVBCLASS_16t = 0x0402,
	LF_ENUMERATE_ST = 0x0403,
	LF_FRIENDFCN_16t = 0x0404,
	LF_INDEX_16t = 0x0405,
	LF_MEMBER_16t = 0x0406,
	LF_STMEMBER_16t = 0x0407,
	LF_METHOD_16t = 0x0408,
	LF_NESTTYPE_16t = 0x0409,
	LF_VFUNCTAB_16t = 0x040a,
	LF_FRIENDCLS_16t = 0x040b,
	LF_ONEMETHOD_16t = 0x040c,
	LF_VFUNCOFF_16t = 0x040d,

	LF_TI16_MAX = 0x1000,

	LF_ARRAY_ST = 0x1003,
	LF_CLASS_ST = 0x1004,
	LF_STRUCTURE_ST = 0x1005,
	LF_UNION_ST = 0x1006,
	LF_ENUM_ST = 0x1007,
	LF_COBOL0 = 0x100a,
	LF_BARRAY = 0x100b,
	LF_DIMARRAY_ST = 0x100c,
	LF_VFTPATH = 0x100d,
	LF_PRECOMP_ST = 0x100e,
	LF_OEM = 0x100f,
	LF_ALIAS_ST = 0x1010,
	LF_OEM2 = 0x1011,

	LF_SKIP = 0x1200,
	LF_DEFARG_ST = 0x1202,
	LF_DERIVED = 0x1204,
	LF_DIMCONU = 0x1207,
	LF_DIMCONLU = 0x1208,
	LF_DIMVARU = 0x1209,
	LF_DIMVARLU = 0x120a,

	// Member type records. These are generally not length prefixed, and appear
	// inside of a field list record.
	LF_FRIENDFCN_ST = 0x1403,
	LF_MEMBER_ST = 0x1405,
	LF_STMEMBER_ST = 0x1406,
	LF_METHOD_ST = 0x1407,
	LF_NESTTYPE_ST = 0x1408,
	LF_FRIENDCLS = 0x140a,
	LF_ONEMETHOD_ST = 0x140b,
	LF_VFUNCOFF = 0x140c,
	LF_NESTTYPEEX_ST = 0x140d,
	LF_MEMBERMODIFY_ST = 0x140e,
	LF_MANAGED_ST = 0x140f,

	LF_ST_MAX = 0x1500,
	LF_TYPESERVER = 0x1501,
	LF_DIMARRAY = 0x1508,
	LF_PRECOMP = 0x1509,
	LF_ALIAS = 0x150a,
	LF_DEFARG = 0x150b,
	LF_FRIENDFCN = 0x150c,
	LF_NESTTYPEEX = 0x1512,
	LF_MEMBERMODIFY = 0x1513,
	LF_MANAGED = 0x1514,
	LF_STRIDED_ARRAY = 0x1516,
	LF_HLSL = 0x1517,
	LF_MODIFIER_EX = 0x1518,
	LF_VECTOR = 0x151b,
	LF_MATRIX = 0x151c,

	// ID leaf records. Subsequent leaf types may be referenced from .debug$S.

	// Numeric leaf types. These are generally contained in other records, and not
	// encountered in the main type stream.
	LF_NUMERIC = 0x8000,
	LF_CHAR = 0x8000,
	LF_SHORT = 0x8001,
	LF_USHORT = 0x8002,
	LF_LONG = 0x8003,
	LF_ULONG = 0x8004,
	LF_REAL32 = 0x8005,
	LF_REAL64 = 0x8006,
	LF_REAL80 = 0x8007,
	LF_REAL128 = 0x8008,
	LF_QUADWORD = 0x8009,
	LF_UQUADWORD = 0x800a,
	LF_REAL48 = 0x800b,
	LF_COMPLEX32 = 0x800c,
	LF_COMPLEX64 = 0x800d,
	LF_COMPLEX80 = 0x800e,
	LF_COMPLEX128 = 0x800f,
	LF_VARSTRING = 0x8010,
	LF_OCTWORD = 0x8017,
	LF_UOCTWORD = 0x8018,
	LF_DECIMAL = 0x8019,
	LF_DATE = 0x801a,
	LF_UTF8STRING = 0x801b,
	LF_REAL16 = 0x801c,

	// Padding bytes. These are emitted into alignment bytes in the type stream.
	LF_PAD0 = 0xf0,
	LF_PAD1 = 0xf1,
	LF_PAD2 = 0xf2,
	LF_PAD3 = 0xf3,
	LF_PAD4 = 0xf4,
	LF_PAD5 = 0xf5,
	LF_PAD6 = 0xf6,
	LF_PAD7 = 0xf7,
	LF_PAD8 = 0xf8,
	LF_PAD9 = 0xf9,
	LF_PAD10 = 0xfa,
	LF_PAD11 = 0xfb,
	LF_PAD12 = 0xfc,
	LF_PAD13 = 0xfd,
	LF_PAD14 = 0xfe,
	LF_PAD15 = 0xff,
}

// enum CV_cookietype_e
enum FrameCookieType : ubyte {
	copy,
	xorStackPointer,
	xorFramePointer,
	xorR13,
};

// enum BinaryAnnotationOpcode
enum BinaryAnnotationsOpcode : uint
{
	invalid,              // link time pdb contains PADDINGs
	codeOffset,           // param : start offset
	changeCodeOffsetBase, // param : nth separated code chunk (main code chunk == 0)
	changeCodeOffset,     // param : delta of offset
	changeCodeLength,     // param : length of code, default next start
	changeFile,           // param : fileId
	changeLineOffset,     // param : line offset (signed)
	changeLineEndDelta,   // param : how many lines, default 1
	changeRangeKind,      // param : either 1 (default, for statement) or 0 (for expression)

	changeColumnStart,    // param : start column number, 0 means no column info
	changeColumnEndDelta, // param : end column number delta (signed)

	// Combo opcodes for smaller encoding size.
	changeCodeOffsetAndLineOffset, // param : ((sourceDelta << 4) | CodeDelta)
	changeCodeLengthAndCodeOffset, // param : codeLength, codeOffset

	changeColumnEnd,      // param : end column number
}

// CVUncompressData
uint cvReadCompressedUint(ref StreamReader stream)
{
	ubyte data = stream.read!ubyte;

	if ((data & 0b1000_0000) == 0x00) {
		// 0??? ???? - 1 byte
		return data;
	}
	else if ((data & 0b1100_0000) == 0b1000_0000) {
		// 10?? ???? - 2 bytes

		if (stream.remainingBytes < 1) return uint.max; // invalid value

		uint res = (data & 0b0011_1111) << 8;
		res |= stream.read!ubyte;
		return res;
	}
	else if ((data & 0b1110_0000) == 0b1100_0000) {
		// 110? ???? - 4 bytes

		if (stream.remainingBytes < 3) return uint.max; // invalid value

		uint res = (data & 0b0001_1111) << 24;
		res |= stream.read!ubyte << 16;
		res |= stream.read!ubyte << 8;
		res |= stream.read!ubyte;
		return res;
	}

	return uint.max; // invalid value
}

uint numBinaryAnnotationsOpcodeArgs(BinaryAnnotationsOpcode op) {
	return op == BinaryAnnotationsOpcode.changeCodeLengthAndCodeOffset ? 2 : 1;
}

bool binaryAnnotationsIsIntArg(BinaryAnnotationsOpcode op) {
	return op == BinaryAnnotationsOpcode.changeLineOffset ||
		op == BinaryAnnotationsOpcode.changeColumnEndDelta;
}

// encodes negative int as positive int. Uses 0th bit as sign.
uint cvEncodeSignedInt32(int input) {
	return (input >= 0)
		? (( input) << 1) | 0
		: ((-input) << 1) | 1;
}

int cvDecodeSignedInt32(uint input) {
	return (input & 1)
		? -cast(int)(input >> 1)
		:  cast(int)(input >> 1);
}
