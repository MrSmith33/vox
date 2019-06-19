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
	///
	string outputFilename = "out.exe";

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

	Arena!SourceFileInfo files;
	/// Buffer for sources of all modules
	Arena!char sourceBuffer;
	/// Token buffer
	Arena!TokenType tokenBuffer;
	/// Token locations in source code
	Arena!SourceLocation tokenLocationBuffer;
	/// For AST nodes
	Arena!ubyte astBuffer;
	/// For arrays and hashmaps used in AST nodes
	ArrayArena arrayArena;
	/// Identifier interning/deduplication
	IdentifierMap idMap;

	/// Buffer for intra-pass temporary data
	Arena!uint tempBuffer;
	/// Buffer for function IR generation
	Arena!uint irBuffer;
	///
	IrTypeStorage types;
	/// Global constant storage
	IrConstantStorage constants;
	/// Module global values and literals.
	IrGlobalStorage globals;

	/// Buffer for string/array/struct literals
	/// String literals have \0 after last character
	/// Must be allocated before or after code segment to allow relative addressing
	Arena!ubyte staticDataBuffer;
	/// Buffer for resulting machine code
	Arena!ubyte codeBuffer;
	/// Buffer for indirect addresses when in JIT mode
	/// Buffer for import section when in exe mode
	Arena!ubyte importBuffer;

	/// Symbols provided by the environment
	LinkIndex[Identifier] externalSymbols;
	/// Symbols, sections and references
	ObjectSymbolTable objSymTab;

	Arena!ubyte binaryBuffer;

	// sections
	LinkIndex hostSectionIndex;
	LinkIndex importSectionIndex;
	LinkIndex dataSectionIndex;
	LinkIndex textSectionIndex;

	// errors and debug

	/// True if current/last pass had errors
	bool hasErrors;
	/// Text output for errors
	TextSink errorSink;
	/// Text output for errors and stack traces
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
	/// Print IR after optimization pass
	bool printIrOpt = false;
	/// Print LIR after IR to LIR pass
	bool printLir = false;
	/// Print liveness analisys info
	bool printLiveIntervals = false;
	/// Print LIR after Register Allocation
	bool printLirRA = false;
	bool printStaticData = false;
	bool printStackLayout = false;
	bool printSymbols = false;
	bool printCodeHex = false;
	bool printTimings = false;
	Identifier printOnlyFun;

	/// Check if printing of only this function needed (including if all functions are requested)
	bool printDumpOf(FunctionDeclNode* fun) {
		if (printOnlyFun.isUndefined) return true;
		if (printOnlyFun == fun.id) return true;
		return false;
	}

	/// Check if printing of only this function needed (not all functions)
	bool printDumpOnlyOf(FunctionDeclNode* fun) {
		return printOnlyFun == fun.id;
	}

	/// Check if printing of all functions requested
	bool printDumpOfAll() {
		return printOnlyFun.isUndefined;
	}

	const(char)[] getTokenString(TokenIndex tokenIndex) pure
	{
		return tokenLocationBuffer[tokenIndex].getTokenString(sourceBuffer.data);
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
		basicTypeNode(8, BasicType.t_null),

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
		size_t startLen = sink.data.length;
		sink.putf("%s: Error: ", FmtSrcLoc(tokIdx, &this));
		sink.putfln(format, args);
		errorSink.put(sink.data[startLen..$]);
		if (printTraceOnError)
			try
				throw new Exception(null);
			catch (Exception e)
				sink.putf("%s", e.info);
		hasErrors = true;
	}

	void error(Args...)(string format, Args args)
	{
		size_t startLen = sink.data.length;
		sink.put("Error: ");
		sink.putfln(format, args);
		errorSink.put(sink.data[startLen..$]);
		if (printTraceOnError)
			try
				throw new Exception(null);
			catch (Exception e)
				sink.putf("%s", e.info);
		hasErrors = true;
	}

	void unrecoverable_error(Args...)(TokenIndex tokIdx, string format, Args args)
	{
		size_t startLen = sink.data.length;
		sink.putf("%s: Error: ", FmtSrcLoc(tokIdx, &this));
		sink.putfln(format, args);
		errorSink.put(sink.data[startLen..$]);
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
		size_t startLen = sink.data.length;
		sink.putf("%s(%s): ICE: Assertion failure: ", file, line);
		sink.putfln(fmt, args);
		errorSink.put(sink.data[startLen..$]);
		hasErrors = true;
		handleICE;
		throw new CompilationException(true);
	}

	void assertf(Args...)(bool cond, TokenIndex tokIdx, string fmt, lazy Args args, string file = __MODULE__, int line = __LINE__)
	{
		if (cond) return;
		size_t startLen = sink.data.length;
		sink.putf("%s(%s): %s: ICE: Assertion failure: ", file, line, FmtSrcLoc(tokIdx, &this));
		sink.putfln(fmt, args);
		errorSink.put(sink.data[startLen..$]);
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
		size_t startLen = sink.data.length;
		sink.putf("%s: ", FmtSrcLoc(tokIdx, &this));
		errorSink.put(sink.data[startLen..$]);
		internal_error_impl(format, file, line, args);
	}

	void internal_error(Args...)(string format, Args args, string file = __MODULE__, int line = __LINE__)
	{
		internal_error_impl(format, file, line, args);
	}

	private void internal_error_impl(Args...)(string format, string file, int line, Args args)
	{
		size_t startLen = sink.data.length;
		sink.putf("ICE(%s:%s): ", file, line);
		sink.putfln(format, args);
		errorSink.put(sink.data[startLen..$]);
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
		result.storageUintIndex = tempBuffer.uintLength;
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

	T* appendAst(T, Args...)(Args args) {
		T* result = cast(T*)astBuffer.nextPtr;
		astBuffer.voidPut(T.sizeof);
		*result = T(args);
		return result;
	}

	ModuleDeclNode* getModuleFromToken(TokenIndex tokIndex)
	{
		assert(tokIndex < tokenBuffer.length, format("getModuleFromToken(%s), numTokens %s", tokIndex, tokenBuffer.length));

		ModuleDeclNode* lastMod;
		foreach(ref SourceFileInfo file; files.data)
		{
			// We are parsing and this module is not parsed yet, so it's previous one
			if (!file.firstTokenIndex.isValid) break;

			if (tokIndex < file.firstTokenIndex) {
				assertf(lastMod !is null,
					"Cannot find file of token %s, before first file starting at %s",
					tokIndex, file.firstTokenIndex);
				return lastMod;
			}
			lastMod = file.mod;
		}
		if (lastMod is null) {
			internal_error("Cannot find file of token %s, no files", tokIndex);
		}
		return lastMod;
	}

	ModuleDeclNode* findModule(string moduleId)
	{
		Identifier id = idMap.find(moduleId);
		if (id.isUndefined) return null;
		return findModule(id);
	}

	ModuleDeclNode* findModule(const Identifier moduleId)
	{
		foreach(ref SourceFileInfo file; files.data)
			if (file.mod.id == moduleId)
				return file.mod;
		return null;
	}

	ModuleDeclNode* getModule(ModuleIndex index)
	{
		return files[index.fileIndex].mod;
	}

	FunctionDeclNode* getFunction(FunctionIndex index)
	{
		return files[index.moduleIndex.fileIndex].mod.functions[index.functionIndex];
	}

	FunctionDeclNode* findFunction(string moduleName, string funcName)
	{
		ModuleDeclNode* mod = findModule(moduleName);
		if (mod is null) return null;

		return mod.findFunction(funcName, &this);
	}

	/// Will throw exception if function exists in more than 1 module
	void findFunction(string funcName, void delegate(ModuleDeclNode*, FunctionDeclNode*) onFunction)
	{
		Identifier funcId = idMap.find(funcName);
		if (funcId.isUndefined) return;

		foreach (ref SourceFileInfo file; files.data)
		{
			FunctionDeclNode* fun = file.mod.findFunction(funcId);
			if (fun !is null) onFunction(file.mod, fun);
		}
	}

	auto getFunctionPtr(ResultType, ParamTypes...)(string funcName)
	{
		FunctionDeclNode* funDecl;
		foreach (ref SourceFileInfo file; files.data)
		{
			FunctionDeclNode* fun = file.mod.findFunction(funcName, &this);
			if (fun !is null)
			{
				if (funDecl !is null)
					internal_error("Test function %s is found in 2 places", funcName);
				funDecl = fun;
			}
		}

		if (funDecl is null)
			internal_error("Test function `%s` is not found in %s modules", funcName, files.length);

		return getFunctionPtr!(ResultType, ParamTypes)(funDecl);
	}

	auto getFunctionPtr(ResultType, ParamTypes...)(string moduleName, string funcName)
	{
		FunctionDeclNode* func = findFunction(moduleName, funcName);
		if (func is null) return null;
		return getFunctionPtr!(ResultType, ParamTypes)(func);
	}

	auto getFunctionPtr(ResultType, ParamTypes...)(FunctionDeclNode* funcDecl)
	{
		// TODO: check that passed D types match retrieved function signature
		//assert(funcDecl.returnType.isSameTypeAs!ParamType, "wrong result type");

		auto numRequestedParams = ParamTypes.length;
		auto numParams = funcDecl.parameters.length;

		Identifier funcId = funcDecl.id;

		if (numRequestedParams < numParams)
			internal_error("Insufficient parameters to '%s', got %s, expected %s",
				idString(funcId), numRequestedParams, numParams);
		else if (numRequestedParams > numParams)
			internal_error("Too much parameters to '%s', got %s, expected %s",
				idString(funcId), numRequestedParams, numParams);

		//foreach(i, ParamType; ParamTypes)
		//{
		//	assert(funcDecl.parameters[i].type.isSameTypeAs!ParamType, "wrong param type");
		//}

		alias JittedFunc = extern(C) ResultType function(ParamTypes);
		return cast(JittedFunc)funcDecl.backendData.funcPtr;
	}

	void printMemSize()
	{
		writefln("Arena sizes:       used  committed  reserved");
		void printArena(A)(ref A arena, string name) {
			writefln("  %-14s%-6iB    %-6iB   %-6iB",
				name,
				scaledNumberFmt(arena.byteLength),
				scaledNumberFmt(arena.committedBytes),
				scaledNumberFmt(arena.reservedBytes));
		}
		printArena(sourceBuffer, "source");
		printArena(files, "files");
		printArena(tokenBuffer, "tokens");
		printArena(tokenLocationBuffer, "token loc");
		printArena(astBuffer, "AST");
		printArena(arrayArena, "arrays");
		printArena(irBuffer, "IR");
		printArena(tempBuffer, "temp");
		printArena(types.buffer, "types");
		printArena(staticDataBuffer, "static data");
		printArena(globals.buffer, "globals");
		printArena(constants.buffer, "constants");
		printArena(importBuffer, "imports");
		printArena(objSymTab.buffer, "symbols");
		printArena(codeBuffer, "machine code");
		printArena(binaryBuffer, "binary");
	}

	void clear()
	{
		hasErrors = false;
		sourceBuffer.clear;
		files.clear;
		codeBuffer.clear;
		importBuffer.clear;
		tokenBuffer.clear;
		tokenLocationBuffer.clear;
		binaryBuffer.clear;
		irBuffer.clear;
		types.buffer.clear;
		tempBuffer.clear;
		staticDataBuffer.clear;
		objSymTab.buffer.clear;
		objSymTab.firstModule = LinkIndex();
		globals.buffer.clear;
		constants.buffer.clear;
		astBuffer.clear;
		arrayArena.clear;
		entryPoint = null;
		sink.clear;
		errorSink.clear;

		externalSymbols.clear();
	}
}
