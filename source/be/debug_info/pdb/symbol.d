/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Code View format symbols
module be.debug_info.pdb.symbol;

import std.bitmanip : bitfields;
import be.debug_info.pdb;

// SYM_ENUM_e
enum SymbolKind : ushort
{
	S_END           = 0x0006,

	// 16 bit symbols omitted

	// sym records with 32-bit types embedded instead of 16-bit
	// all have 0x1000 bit set for easy identification
	// only do the 32-bit target versions since we don't really
	// care about 16-bit ones anymore.
	S_TI16_MAX      =  0x1000,

	S_REGISTER_ST   =  0x1001,  // Register variable
	S_CONSTANT_ST   =  0x1002,  // constant symbol
	S_UDT_ST        =  0x1003,  // User defined type
	S_COBOLUDT_ST   =  0x1004,  // special UDT for cobol that does not symbol pack
	S_MANYREG_ST    =  0x1005,  // multiple register variable
	S_BPREL32_ST    =  0x1006,  // BP-relative
	S_LDATA32_ST    =  0x1007,  // Module-local symbol
	S_GDATA32_ST    =  0x1008,  // Global data symbol
	S_PUB32_ST      =  0x1009,  // a public symbol (CV internal reserved)
	S_LPROC32_ST    =  0x100a,  // Local procedure start
	S_GPROC32_ST    =  0x100b,  // Global procedure start
	S_VFTABLE32     =  0x100c,  // address of virtual function table
	S_REGREL32_ST   =  0x100d,  // register relative address
	S_LTHREAD32_ST  =  0x100e,  // local thread storage
	S_GTHREAD32_ST  =  0x100f,  // global thread storage

	S_LPROCMIPS_ST  =  0x1010,  // Local procedure start
	S_GPROCMIPS_ST  =  0x1011,  // Global procedure start

	S_FRAMEPROC     =  0x1012,  // extra frame and proc information
	S_COMPILE2_ST   =  0x1013,  // extended compile flags and info

	// new symbols necessary for 16-bit enumerates of IA64 registers
	// and IA64 specific symbols

	S_MANYREG2_ST   =  0x1014,  // multiple register variable
	S_LPROCIA64_ST  =  0x1015,  // Local procedure start (IA64)
	S_GPROCIA64_ST  =  0x1016,  // Global procedure start (IA64)

	// Local symbols for IL
	S_LOCALSLOT_ST  =  0x1017,  // local IL sym with field for local slot index
	S_PARAMSLOT_ST  =  0x1018,  // local IL sym with field for parameter slot index

	S_ANNOTATION    =  0x1019,  // Annotation string literals

	// symbols to support managed code debugging
	S_GMANPROC_ST   =  0x101a,  // Global proc
	S_LMANPROC_ST   =  0x101b,  // Local proc
	S_RESERVED1     =  0x101c,  // reserved
	S_RESERVED2     =  0x101d,  // reserved
	S_RESERVED3     =  0x101e,  // reserved
	S_RESERVED4     =  0x101f,  // reserved
	S_LMANDATA_ST   =  0x1020,
	S_GMANDATA_ST   =  0x1021,
	S_MANFRAMEREL_ST=  0x1022,
	S_MANREGISTER_ST=  0x1023,
	S_MANSLOT_ST    =  0x1024,
	S_MANMANYREG_ST =  0x1025,
	S_MANREGREL_ST  =  0x1026,
	S_MANMANYREG2_ST=  0x1027,
	S_MANTYPREF     =  0x1028,  // Index for type referenced by name from metadata
	S_UNAMESPACE_ST =  0x1029,  // Using namespace

	// Symbols w/ SZ name fields. All name fields contain utf8 encoded strings.
	S_ST_MAX        =  0x1100,  // starting point for SZ name symbols

	S_OBJNAME       =  0x1101,  // path to object file name
	S_THUNK32       =  0x1102,  // Thunk Start
	S_BLOCK32       =  0x1103,  // block start
	S_WITH32        =  0x1104,  // with start
	S_LABEL32       =  0x1105,  // code label
	S_REGISTER      =  0x1106,  // Register variable
	S_CONSTANT      =  0x1107,  // constant symbol
	S_UDT           =  0x1108,  // User defined type
	S_COBOLUDT      =  0x1109,  // special UDT for cobol that does not symbol pack
	S_MANYREG       =  0x110a,  // multiple register variable
	S_BPREL32       =  0x110b,  // BP-relative
	S_LDATA32       =  0x110c,  // Module-local symbol
	S_GDATA32       =  0x110d,  // Global data symbol
	S_PUB32         =  0x110e,  // a public symbol (CV internal reserved)
	S_LPROC32       =  0x110f,  // Local procedure start
	S_GPROC32       =  0x1110,  // Global procedure start
	S_REGREL32      =  0x1111,  // register relative address
	S_LTHREAD32     =  0x1112,  // local thread storage
	S_GTHREAD32     =  0x1113,  // global thread storage

	S_LPROCMIPS     =  0x1114,  // Local procedure start
	S_GPROCMIPS     =  0x1115,  // Global procedure start
	S_COMPILE2      =  0x1116,  // extended compile flags and info
	S_MANYREG2      =  0x1117,  // multiple register variable
	S_LPROCIA64     =  0x1118,  // Local procedure start (IA64)
	S_GPROCIA64     =  0x1119,  // Global procedure start (IA64)
	S_LOCALSLOT     =  0x111a,  // local IL sym with field for local slot index
	S_SLOT          = S_LOCALSLOT,  // alias for LOCALSLOT
	S_PARAMSLOT     =  0x111b,  // local IL sym with field for parameter slot index

	// symbols to support managed code debugging
	S_LMANDATA      =  0x111c,
	S_GMANDATA      =  0x111d,
	S_MANFRAMEREL   =  0x111e,
	S_MANREGISTER   =  0x111f,
	S_MANSLOT       =  0x1120,
	S_MANMANYREG    =  0x1121,
	S_MANREGREL     =  0x1122,
	S_MANMANYREG2   =  0x1123,
	S_UNAMESPACE    =  0x1124,  // Using namespace

	// ref symbols with name fields
	S_PROCREF       =  0x1125,  // Reference to a procedure
	S_DATAREF       =  0x1126,  // Reference to data
	S_LPROCREF      =  0x1127,  // Local Reference to a procedure
	S_ANNOTATIONREF =  0x1128,  // Reference to an S_ANNOTATION symbol
	S_TOKENREF      =  0x1129,  // Reference to one of the many MANPROCSYM's

	// continuation of managed symbols
	S_GMANPROC      =  0x112a,  // Global proc
	S_LMANPROC      =  0x112b,  // Local proc

	// short, light-weight thunks
	S_TRAMPOLINE    =  0x112c,  // trampoline thunks
	S_MANCONSTANT   =  0x112d,  // constants with metadata type info

	// native attributed local/parms
	S_ATTR_FRAMEREL =  0x112e,  // relative to virtual frame ptr
	S_ATTR_REGISTER =  0x112f,  // stored in a register
	S_ATTR_REGREL   =  0x1130,  // relative to register (alternate frame ptr)
	S_ATTR_MANYREG  =  0x1131,  // stored in >1 register

	// Separated code (from the compiler) support
	S_SEPCODE       =  0x1132,

	S_LOCAL_2005    =  0x1133,  // defines a local symbol in optimized code
	S_DEFRANGE_2005 =  0x1134,  // defines a single range of addresses in which symbol can be evaluated
	S_DEFRANGE2_2005 =  0x1135,  // defines ranges of addresses in which symbol can be evaluated

	S_SECTION       =  0x1136,  // A COFF section in a PE executable
	S_COFFGROUP     =  0x1137,  // A COFF group
	S_EXPORT        =  0x1138,  // A export

	S_CALLSITEINFO  =  0x1139,  // Indirect call site information
	S_FRAMECOOKIE   =  0x113a,  // Security cookie information

	S_DISCARDED     =  0x113b,  // Discarded by LINK /OPT:REF (experimental, see richards)

	S_COMPILE3      =  0x113c,  // Replacement for S_COMPILE2
	S_ENVBLOCK      =  0x113d,  // Environment block split off from S_COMPILE2

	S_LOCAL         =  0x113e,  // defines a local symbol in optimized code
	S_DEFRANGE      =  0x113f,  // defines a single range of addresses in which symbol can be evaluated
	S_DEFRANGE_SUBFIELD =  0x1140,           // ranges for a subfield

	S_DEFRANGE_REGISTER =  0x1141,           // ranges for en-registered symbol
	S_DEFRANGE_FRAMEPOINTER_REL =  0x1142,   // range for stack symbol.
	S_DEFRANGE_SUBFIELD_REGISTER =  0x1143,  // ranges for en-registered field of symbol
	S_DEFRANGE_FRAMEPOINTER_REL_FULL_SCOPE =  0x1144, // range for stack symbol span valid full scope of function body, gap might apply.
	S_DEFRANGE_REGISTER_REL =  0x1145, // range for symbol address as register + offset.

	// S_PROC symbols that reference ID instead of type
	S_LPROC32_ID     =  0x1146,
	S_GPROC32_ID     =  0x1147,
	S_LPROCMIPS_ID   =  0x1148,
	S_GPROCMIPS_ID   =  0x1149,
	S_LPROCIA64_ID   =  0x114a,
	S_GPROCIA64_ID   =  0x114b,

	S_BUILDINFO      = 0x114c, // build information.
	S_INLINESITE     = 0x114d, // inlined function callsite.
	S_INLINESITE_END = 0x114e,
	S_PROC_ID_END    = 0x114f,

	S_DEFRANGE_HLSL  = 0x1150,
	S_GDATA_HLSL     = 0x1151,
	S_LDATA_HLSL     = 0x1152,

	S_FILESTATIC     = 0x1153,

	S_LOCAL_DPC_GROUPSHARED = 0x1154, // DPC groupshared variable
	S_LPROC32_DPC = 0x1155, // DPC local procedure start
	S_LPROC32_DPC_ID =  0x1156,
	S_DEFRANGE_DPC_PTR_TAG =  0x1157, // DPC pointer tag definition range
	S_DPC_SYM_TAG_MAP = 0x1158, // DPC pointer tag value to symbol record map

	S_ARMSWITCHTABLE  = 0x1159,
	S_CALLEES = 0x115a,
	S_CALLERS = 0x115b,
	S_POGODATA = 0x115c,
	S_INLINESITE2 = 0x115d,      // extended inline site information

	S_HEAPALLOCSITE = 0x115e,    // heap allocation site

	S_MOD_TYPEREF = 0x115f,      // only generated at link time

	S_REF_MINIPDB = 0x1160,      // only generated at link time for mini PDB
	S_PDBMAP      = 0x1161,      // only generated at link time for mini PDB

	S_GDATA_HLSL32 = 0x1162,
	S_LDATA_HLSL32 = 0x1163,

	S_GDATA_HLSL32_EX = 0x1164,
	S_LDATA_HLSL32_EX = 0x1165,

	S_RECTYPE_MAX,               // one greater than last
	S_RECTYPE_LAST  = S_RECTYPE_MAX - 1,
	S_RECTYPE_PAD   = S_RECTYPE_MAX + 0x100 // Used *only* to verify symbol record types so that current PDB code can potentially read
	                                        // future PDBs (assuming no format change, etc).

}

// S_PUB32
align(2) struct PublicSym32
{
	PublicSymFlags flags;
	uint offset;
	ushort segment;
	// next follows zero-terminated string
}
static assert(PublicSym32.sizeof == 10);

// CV_PUBSYMFLAGS
enum PublicSymFlags : uint {
	none = 0,
	code = 1 << 0,
	func = 1 << 1,
	managed = 1 << 2,
	MSIL = 1 << 3,
}

// S_PROCREF, S_LPROCREF
align(2) struct ProcRefSym
{
	uint   sumName; // SUC of the name
	uint symOffset; // Offset of actual symbol in $$Symbols
	ushort     mod; // Module containing the actual symbol
	// next follows zero-terminated string
}
static assert(ProcRefSym.sizeof == 10);

// struct PROCSYM32
// S_GPROC32, S_LPROC32, S_GPROC32_ID, S_LPROC32_ID, S_LPROC32_DPC or S_LPROC32_DPC_ID
align(1) struct ProcSym {
	uint parent;         // pointer to the parent
	uint end;            // pointer to this blocks end
	uint next;           // pointer to next symbol
	uint length;         // Proc length
	uint dbgStart;       // Debug start offset
	uint dbgEnd;         // Debug end offset
	TypeIndex typeIndex; // Type index or ID
	uint offset;
	ushort segment;
	ubyte flags;         // set of ProcSymFlags
	// next follows zero-terminated string
}

enum ProcSymFlags : ubyte {
	none = 0,
	hasFP = 1 << 0,
	hasIRET = 1 << 1,
	hasFRET = 1 << 2,
	isNoReturn = 1 << 3,
	isUnreachable = 1 << 4,
	hasCustomCallingConv = 1 << 5,
	isNoInline = 1 << 6,
	hasOptimizedDebugInfo = 1 << 7,
}

// struct LOCALSYM
// S_LOCAL
align(1) struct LocalSym
{
    TypeIndex typeIndex; // type index
    ushort    flags;     // set of LocalSymFlags
	// next follows zero-terminated string
}

enum LocalSymFlags : ushort {
	none = 0,
	isParameter = 1 << 0,
	isAddressTaken = 1 << 1,
	isCompilerGenerated = 1 << 2,
	isAggregate = 1 << 3,
	isAggregated = 1 << 4,
	isAliased = 1 << 5,
	isAlias = 1 << 6,
	isReturnValue = 1 << 7,
	isOptimizedOut = 1 << 8,
	isEnregisteredGlobal = 1 << 9,
	isEnregisteredStatic = 1 << 10,
}

// S_DEFRANGE_REGISTER_REL
// struct DEFRANGESYMREGISTERREL
align(1) struct DefRangeRegisterRelSym
{
	ushort baseReg;                    // Register to hold the base pointer of the symbol
	mixin(bitfields!(
		bool,  "spilledUdtMember",  1, // Spilled member for s.i.
		ubyte, "",                  3,
		ubyte, "offsetInParent",   12, // Offset in parent variable.
	));
    int basePointerOffset;             // offset to register

    LocalVariableAddrRange range;      // Range of addresses where this program is valid
}

// struct COMPILESYM3
// S_COMPILE3
align(1) struct CompileSym3 {
	mixin(bitfields!(
		CV_SourceLanguage, "sourceLanguage",  8, // Language index
		bool,  "EC",              1, // Compiled for edit and continue
		bool,  "NoDbgInfo",       1, // Compiled without debugging info
		bool,  "LTCG",            1, // Compiled with LTCG
		bool,  "NoDataAlign",     1, // Compiled with /bzalign
		bool,  "ManagedPresent",  1, // Managed code/data present
		bool,  "SecurityChecks",  1, // Compiled with /GS
		bool,  "HotPatch",        1, // Compiled with /hotpatch
		bool,  "CVTCIL",          1, // Converted with CVTCIL
		bool,  "MSILModule",      1, // MSIL module
		bool,  "Sdl",             1, // Compiled with /sdl
		bool,  "PGO",             1, // Compiled with /ltcg:pgo or pgu
		bool,  "Exp",             1, // .exp module
		uint,  "padding",        12, // padding
	));
    CV_CPUType machine;    // Target processor
    ushort verFEMajor; // Front end major version #
    ushort verFEMinor; // Front end minor version #
    ushort verFEBuild; // Front end build version #
    ushort verFEQFE;   // Front end QFE version #
    ushort verMajor;   // Back end major version #
    ushort verMinor;   // Back end minor version #
    ushort verBuild;   // Back end build version #
    ushort verQFE;     // Back end QFE version #
	// next follows zero-terminated string, Version string
}

// struct ENVBLOCKSYM
// S_ENVBLOCK
align(1) struct EnvBlockSym {
	mixin(bitfields!(
		bool,  "EC",        1, // Compiled for edit and continue
		uint,  "pad",        7, // padding
	));
	// next follows sequence of pairs of zero-terminated strings, terminated by empty string
}

// struct OBJNAMESYM
// S_OBJNAME
align(1) struct ObjNameSym {
	uint signature;
	// next follows zero-terminated string of name
}

// CV_LVAR_ADDR_RANGE
// represents an address range, used for optimized code debug info
struct LocalVariableAddrRange
{
	uint offsetStart;
	ushort isectStart;
	ushort range;
}

// struct SECTIONSYM
// S_SECTION
align(1) struct SectionSym
{
    ushort section;               // Section number
    ubyte  alignment;              // Alignment of this section (power of 2)
    ubyte  reserved;          // Reserved.  Must be zero.
    uint   rva;
    uint   length;
    uint   characteristics;
	// next follows zero-terminated string
}
