/**
Copyright: Copyright (c) 2018-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

module context;

import std.stdio;
import std.string : format;
import all;

enum BuildType : ubyte {
	jit,
	exe,
	//dll
}

///
class CompilationException : Exception
{
	this(bool isICE = false)
	{
		super(null);
		this.isICE = isICE;
	}
	/// True if Internal Compiler Error and not regular compilation error
	bool isICE;
}

///
enum IceBehavior : ubyte {
	error,
	breakpoint
}

///
struct CompilationContext
{
	/// Module source
	string input;
	///
	string outputFilename = "out.exe";

	/// Module declaration
	ModuleDeclNode* mod;
	/// Set when BuildType.exe is used
	FunctionDeclNode* entryPoint;

	// build settings

	/// Target machine info
	MachineInfo* machineInfo = &mach_info_amd64;
	///
	BuildType buildType;
	/// Build executable
	WindowsSubsystem windowsSubsystem = WindowsSubsystem.WINDOWS_CUI;
	///
	uint sectionAlignemnt = 512;
	/// If true attempt to maximize debuggability
	bool buildDebug = false;
	///
	bool useFramePointer = false;

	// storage

	/// Buffer for sources of all modules
	char[] sourceBuffer;
	FixedBuffer!SourceFileInfo files;
	/// Buffer for resulting machine code
	ubyte[] codeBuffer;
	ubyte[] importBuffer;
	ubyte[] binaryBuffer;
	/// Identifier interning/deduplication
	IdentifierMap idMap;
	/// Token buffer
	TokenType[] tokenBuffer;
	/// Token locations in source code
	SourceLocation[] tokenLocationBuffer;
	/// Buffer for function IR generation
	FixedBuffer!uint irBuffer;
	///
	IrTypeStorage types;
	/// Global constant storage
	IrConstantStorage constants;
	/// Module global values and literals.
	IrGlobalStorage globals;
	/// Buffer for intra-pass temporary data
	FixedBuffer!uint tempBuffer;
	/// Buffer for string/array/struct literals
	/// String literals have \0 after last character
	/// Must be allocated before or after code segment to allow relative addressing
	FixedBuffer!ubyte staticDataBuffer;
	/// Symbols sections and references
	ObjectSymbolTable objSymTab;
	/// Symbols provided by the environment
	LinkIndex[Identifier] externalSymbols;

	// sections
	LinkIndex hostSectionIndex;
	LinkIndex importSectionIndex;
	LinkIndex dataSectionIndex;
	LinkIndex textSectionIndex;

	// errors and debug

	/// True if current/last pass had errors
	bool hasErrors;
	/// Text output for errors
	TextSink sink;
	/// If true, stack traces are added to output
	bool printTraceOnError;
	/// What happens on Internal Compiler Error
	IceBehavior iceBehavior = IceBehavior.breakpoint;
	/// If true, every pass that generates/changes IR, performs validation
	bool validateIr = false;
	bool runTesters = true;

	bool printTodos = false;
	/// Print source before lexing
	bool printSource = false;
	/// Print lexemes after lexing
	bool printLexemes = false;
	/// Print AST right after parsing
	bool printAstFresh = false;
	/// Print AST after semantic analysis
	bool printAstSema = false;
	/// Print IR after AST to IR pass
	bool printIr = false;
	/// Print LIR after IR to LIR pass
	bool printLir = false;
	bool printLiveIntervals = false;
	bool printStaticData = false;
	bool printStackLayout = false;
	bool printSymbols = false;
	bool printCodeHex = false;
	bool printTimings = false;

	const(char)[] getTokenString(TokenIndex tokenIndex) pure
	{
		return tokenLocationBuffer[tokenIndex].getTokenString(sourceBuffer);
	}

	SourceLocation tokenLoc(TokenIndex tokenIndex)
	{
		return tokenLocationBuffer[tokenIndex];
	}

	///
	string idString(const Identifier id) { return idMap.get(id); }

	static __gshared BasicTypeNode[] basicTypes = [
		basicTypeNode(0, BasicType.t_error),
		basicTypeNode(0, BasicType.t_void),
		basicTypeNode(1, BasicType.t_bool , BasicTypeFlag.isBoolean),

		basicTypeNode(1, BasicType.t_i8   , BasicTypeFlag.isInteger),
		basicTypeNode(2, BasicType.t_i16  , BasicTypeFlag.isInteger),
		basicTypeNode(4, BasicType.t_i32  , BasicTypeFlag.isInteger),
		basicTypeNode(8, BasicType.t_i64  , BasicTypeFlag.isInteger),
		//basicTypeNode(1, BasicType.t_isize, BasicTypeFlag.isInteger), // this is alias

		basicTypeNode(1, BasicType.t_u8   , BasicTypeFlag.isInteger | BasicTypeFlag.isUnsigned),
		basicTypeNode(2, BasicType.t_u16  , BasicTypeFlag.isInteger | BasicTypeFlag.isUnsigned),
		basicTypeNode(4, BasicType.t_u32  , BasicTypeFlag.isInteger | BasicTypeFlag.isUnsigned),
		basicTypeNode(8, BasicType.t_u64  , BasicTypeFlag.isInteger | BasicTypeFlag.isUnsigned),
		//basicTypeNode(1, BasicType.t_usize, BasicTypeFlag.isInteger | BasicTypeFlag.isUnsigned), // this is alias

		basicTypeNode(4, BasicType.t_f32  , BasicTypeFlag.isFloat),
		basicTypeNode(8, BasicType.t_f64  , BasicTypeFlag.isFloat),
	];

	TypeNode* basicTypeNodes(BasicType basicType) { return cast(TypeNode*)&basicTypes[basicType]; }

	void error(Args...)(TokenIndex tokIdx, string format, Args args)
	{
		SourceLocation loc = tokenLocationBuffer[tokIdx];
		sink.putf("file(%s, %s): Error: ", loc.line+1, loc.col+1);
		sink.putfln(format, args);
		if (printTraceOnError)
			try
				throw new Exception(null);
			catch (Exception e)
				sink.putf("%s", e.info);
		hasErrors = true;
	}

	void unrecoverable_error(Args...)(TokenIndex tokIdx, string format, Args args)
	{
		SourceLocation loc = tokenLocationBuffer[tokIdx];
		sink.putf("file(%s, %s): Error: ", loc.line+1, loc.col+1);
		sink.putfln(format, args);
		if (printTraceOnError)
			try
				throw new Exception(null);
			catch (Exception e)
				sink.putf("%s", e.info);
		hasErrors = true;
		throw new CompilationException();
	}

	void assertf(Args...)(bool cond, string fmt, lazy Args args, string file = __MODULE__, int line = __LINE__)
	{
		if (cond) return;
		sink.putf("%s(%s): ICE: Assertion failure: ", file, line);
		sink.putfln(fmt, args);
		hasErrors = true;
		handleICE;
		throw new CompilationException(true);
	}

	void assertf(Args...)(bool cond, TokenIndex tokIdx, string fmt, lazy Args args, string file = __MODULE__, int line = __LINE__)
	{
		if (cond) return;
		SourceLocation loc = tokenLocationBuffer[tokIdx];
		sink.putf("%s(%s): file(%s, %s): ICE: ", file, line, loc.line+1, loc.col+1);
		sink.putfln(fmt, args);
		hasErrors = true;
		handleICE;
		throw new CompilationException(true);
	}

	void unreachable(string file = __MODULE__, int line = __LINE__)
	{
		internal_error_impl("Unreachable", file, line);
	}

	void internal_error(Args...)(TokenIndex tokIdx, string format, Args args, string file = __MODULE__, int line = __LINE__)
	{
		SourceLocation loc = tokenLocationBuffer[tokIdx];
		sink.putf("source(%s, %s): ", loc.line+1, loc.col+1);
		internal_error_impl(format, file, line, args);
	}

	void internal_error(Args...)(string format, Args args, string file = __MODULE__, int line = __LINE__)
	{
		internal_error_impl(format, file, line, args);
	}

	private void internal_error_impl(Args...)(string format, string file, int line, Args args)
	{
		sink.putf("ICE(%s:%s): ", file, line);
		sink.putfln(format, args);
		hasErrors = true;
		handleICE;
		throw new CompilationException(true);
	}

	void todo(Args...)(string format, Args args, string file = __MODULE__, int line = __LINE__)
	{
		if (printTodos) {
			writef("TODO(%s:%s): ", file, line);
			writefln(format, args);
		}
	}

	private void handleICE()
	{
		final switch(iceBehavior)
		{
			case IceBehavior.error: assert(false);
			case IceBehavior.breakpoint:
				writeln(sink.text);
				stdout.flush;
				version(DMD) asm { db 0xCC; } // breakpoint
				version(LDC) assert(false); // LDC has no data in assembler
				break;
		}
	}

	void throwOnErrors()
	{
		if (hasErrors) throw new CompilationException();
	}

	IrIndex appendTemp(T)(uint howMany = 1)
	{
		static assert(T.alignof == 4, "Can only store types aligned to 4 bytes");

		IrIndex result;
		result.storageUintIndex = tempBuffer.length;
		result.kind = getIrValueKind!T;

		size_t numAllocatedSlots = divCeil(T.sizeof, uint.sizeof)*howMany;
		tempBuffer.voidPut(numAllocatedSlots);

		(&getTemp!T(result))[0..howMany] = T.init;
		return result;
	}

	ref T getTemp(T)(IrIndex index)
	{
		assert(index.kind != IrValueKind.none, "null index");
		assert(index.kind == getIrValueKind!T, format("%s != %s", index.kind, getIrValueKind!T));
		return *cast(T*)(&tempBuffer.bufPtr[index.storageUintIndex]);
	}

	T[] allocateTempArray(T)(uint howMany)
	{
		static assert(T.sizeof % uint.sizeof == 0, "T.sizeof is not multiple of uint.sizeof");
		static assert(T.alignof == 4, "Can only store types aligned to 4 bytes");

		size_t numAllocatedSlots = divCeil(T.sizeof, uint.sizeof)*howMany;
		T[] result = cast(T[])tempBuffer.voidPut(numAllocatedSlots);
		result[] = T.init;
		return result;
	}
}
