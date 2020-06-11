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

debug (ASSERTF) {
	enum PRETTY_ASSERT = true;
} else {
	version (assert) {
		enum PRETTY_ASSERT = true;
	} else {
		enum PRETTY_ASSERT = false;
	}
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

	/// Source file info storage
	Arena!SourceFileInfo files;
	/// Buffer for sources of all modules
	Arena!char sourceBuffer;
	/// Token buffer
	Arena!TokenType tokenBuffer;
	/// Token locations in source code
	Arena!SourceLocation tokenLocationBuffer;
	/// For AST nodes
	Arena!uint astBuffer;
	/// For arrays and hashmaps used in AST nodes
	ArrayArena arrayArena;
	/// Identifier interning/deduplication
	IdentifierMap idMap;

	/// Buffer for intra-pass temporary data
	Arena!uint tempBuffer;
	/// Buffer for frame data of IR interpreter
	Arena!ubyte vmBuffer;
	/// Buffer for function IR generation
	IrFuncStorage irStorage;
	/// Type storage
	IrTypeStorage types;
	/// Global constant storage
	IrConstantStorage constants;
	/// Module global values and literals.
	IrGlobalStorage globals;

	/// Buffer for string/array/struct literals
	/// String literals have \0 after last character
	/// Must be allocated before or after code segment to allow relative addressing
	/// Static read-only data
	Arena!ubyte roStaticDataBuffer;
	/// Static read-write data. zero-initialized data is stored after initialized data
	Arena!ubyte staticDataBuffer;
	uint zeroDataLength;
	/// Buffer for resulting machine code
	Arena!ubyte codeBuffer;
	/// Buffer for indirect addresses when in JIT mode
	/// Buffer for import section when in exe mode
	Arena!ubyte importBuffer;

	/// Symbols provided by the environment (host/dll symbols)
	LinkIndex[Identifier] externalSymbols;
	/// Symbols, sections and references
	ObjectSymbolTable objSymTab;

	/// Buffer for executable generation
	Arena!ubyte binaryBuffer;

	// Stores astBuffer.length after initialize() call
	// to be reset on beginCompilation()
	private size_t initializedAstBufSize;
	private size_t initializedIrTypeBufSize; // ditto, types.buffer.length
	private size_t initializedSourceBufSize; // ditto, sourceBuffer.length
	size_t initializedTokenLocBufSize; // ditto, tokenLocationBuffer.length

	// sections
	LinkIndex hostSectionIndex;
	LinkIndex importSectionIndex;
	LinkIndex dataSectionIndex;
	LinkIndex rdataSectionIndex;
	LinkIndex textSectionIndex;

	// errors and debug

	Array!AstIndex analisysStack;

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
	/// Print IR after all IR lowering passes
	/// Will not activate if printIrLowerEach is true
	bool printIrLower = false;
	/// Print IR after each IR lowering pass
	bool printIrLowerEach = false;
	/// Print IR after all optimization passes
	/// Will not activate if printIrOptEach is true
	bool printIrOpt = false;
	/// Print IR after each optimization pass
	bool printIrOptEach = false;
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
	// limit number of regs for allocation
	bool debugRegAlloc = false;

	// Disables Dead Code Elimination
	bool disableDCE = false;
	// Disables inlining
	bool disableInline = false;

	Identifier printOnlyFun;
	void setDumpFilter(string name) { printOnlyFun = idMap.getOrRegNoDup(&this, name); }

	// Counters
	uint numCtfeRuns = 0;
	uint numTemplateInstanceLookups = 0;
	uint numTemplateInstantiations = 0;

	/// Check if printing of this function needed (including if all functions are requested)
	bool printDumpOf(FunctionDeclNode* fun) {
		if (printOnlyFun.isUndefined) return true;
		if (printOnlyFun == fun.id) return true;
		return false;
	}

	/// Check if printing of this function only is needed (not all functions)
	bool printDumpOnlyOf(FunctionDeclNode* fun) {
		return printOnlyFun == fun.id;
	}

	/// Check if printing of all functions requested
	bool printDumpOfAll() {
		return printOnlyFun.isUndefined;
	}

	const(char)[] getTokenString(TokenIndex tokenIndex) pure
	{
		return tokenLocationBuffer[tokenIndex].getTokenString(tokenBuffer[tokenIndex], sourceBuffer.data);
	}

	SourceLocation tokenLoc(TokenIndex tokenIndex)
	{
		return tokenLocationBuffer[tokenIndex];
	}

	///
	string idString(const Identifier id) { return idMap.get(id); }

	AstIndex basicTypeNodes(BasicType basicType) {
		return basicTypesArray[basicType];
	}
	AstIndex builtinNodes(BuiltinId builtinId) {
		return builtinsArray[builtinId];
	}

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

	void assertf(Args...)(bool cond, string fmt, Args args, string file = __MODULE__, int line = __LINE__)
	{
		if (cond) return;

		static if (PRETTY_ASSERT)
		{
			size_t startLen = sink.data.length;
			sink.putf("%s(%s): ICE: Assertion failure: ", file, line);
			sink.putfln(fmt, args);
			errorSink.put(sink.data[startLen..$]);
			hasErrors = true;
			handleICE;
			throw new CompilationException(true);
		}
		else assert(false);
	}

	void assertf(Args...)(bool cond, TokenIndex tokIdx, string fmt, Args args, string file = __MODULE__, int line = __LINE__)
	{
		if (cond) return;

		static if (PRETTY_ASSERT)
		{
			size_t startLen = sink.data.length;
			sink.putf("%s(%s): %s: ICE: Assertion failure: ", file, line, FmtSrcLoc(tokIdx, &this));
			sink.putfln(fmt, args);
			errorSink.put(sink.data[startLen..$]);
			hasErrors = true;
			handleICE;
			throw new CompilationException(true);
		}
		else assert(false);
	}

	void unreachable(string file = __MODULE__, int line = __LINE__)
	{
		static if (PRETTY_ASSERT)
		{
			internal_error_impl("Unreachable", file, line);
		}
		else assert(false);
	}

	void internal_error(Args...)(TokenIndex tokIdx, string format, Args args, string file = __MODULE__, int line = __LINE__)
	{
		static if (PRETTY_ASSERT)
		{
			size_t startLen = sink.data.length;
			sink.putf("%s: ", FmtSrcLoc(tokIdx, &this));
			errorSink.put(sink.data[startLen..$]);
			internal_error_impl(format, file, line, args);
		}
		else assert(false);
	}

	void internal_error(Args...)(string format, Args args, string file = __MODULE__, int line = __LINE__)
	{
		static if (PRETTY_ASSERT)
		{
			internal_error_impl(format, file, line, args);
		}
		else assert(false);
	}

	void circular_dependency()
	{
		AstIndex same;
		if (!analisysStack.empty) same = analisysStack.back;
		size_t startLen = sink.data.length;
		sink.putfln("Error: Circular dependency");
		while(!analisysStack.empty)
		{
			AstIndex currentIndex = analisysStack.back;
			TokenIndex tokIdx = currentIndex.loc(&this);
			if (currentIndex == same) sink.put("> ");
			else sink.put("  ");
			sink.putf("%s: %s %s ", FmtSrcLoc(tokIdx, &this), currentIndex.state(&this), currentIndex.astType(&this));
			print_node_name(sink, currentIndex, &this);
			sink.putln;
			analisysStack.unput(1);
		}
		errorSink.put(sink.data[startLen..$]);
		if (printTraceOnError)
			try
				throw new Exception(null);
			catch (Exception e)
				sink.putf("%s", e.info);
		hasErrors = true;
		throw new CompilationException();
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

	void push_analized_node(AstIndex index) {
		analisysStack.put(arrayArena, index);
	}

	void pop_analized_node() {
		assertf(!analisysStack.empty, "Excessive popping detected");
		analisysStack.unput(1);
	}

	IrVmSlotInfo pushVmStack(uint numBytes)
	{
		IrVmSlotInfo result;
		result.offset = vmBuffer.uintLength;
		result.length = numBytes;
		vmBuffer.voidPut(numBytes);
		//writefln("pushVmStack %s %s", result, vmBuffer.uintLength);
		return result;
	}

	void popVmStack(IrVmSlotInfo slot)
	{
		//writefln("popVmStack %s %s", slot, vmBuffer.uintLength);
		vmBuffer.unput(slot.length);
		assertf(slot.offset == vmBuffer.uintLength,
			"popped item is in the middle of the stack");
	}

	IrIndex appendTemp(T)(uint howMany = 1)
	{
		static assert(T.alignof <= 4, "Can only store types aligned to 4 bytes");

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
		//static assert(T.alignof == 4, "Can only store types aligned to 4 bytes");

		size_t numAllocatedSlots = divCeil(T.sizeof, uint.sizeof)*howMany;
		T[] result = cast(T[])tempBuffer.voidPut(numAllocatedSlots);
		result[] = T.init;
		return result;
	}

	void freeTempArray(T)(T[] array)
	{
		static assert(T.sizeof % uint.sizeof == 0, "T.sizeof is not multiple of uint.sizeof");
		static assert(T.alignof == 4, "Can only store types aligned to 4 bytes");
		uint[] buf = cast(uint[])array;
		tempBuffer.free(buf);
	}

	AstIndex appendAst(T, Args...)(Args args) {
		uint resIndex = astBuffer.uintLength;
		T* obj = cast(T*)astBuffer.nextPtr;
		enum size_t numAllocatedSlots = divCeil(T.sizeof, uint.sizeof);
		astBuffer.voidPut(numAllocatedSlots);
		*obj = T(args);
		return AstIndex(resIndex);
	}

	T* getAst(T)(AstIndex index) {
		if (!index) return null;
		T* result = cast(T*)(&astBuffer.bufPtr[index.storageIndex]);
		static if (hasAstNodeType!T)
		{
			assertf(result.astType == getAstNodeType!T, "getAst(%s) got %s", T.stringof, result.astType);
		}
		return result;
	}

	AstIndex getAstNodeIndex(T)(T* node) {
		assert(node, "node is null");
		uint* ptr = cast(uint*)node;
		ptrdiff_t diff = ptr - astBuffer.bufPtr;
		assert(diff > 0, "<= 0");
		assert(diff < astBuffer.length, "> length");
		return AstIndex(cast(uint)diff);
	}

	Scope* getAstScope(AstIndex index) { return getAst!Scope(index); }
	AstNode* getAstNode(AstIndex index) { return getAst!AstNode(index); }
	TypeNode* getAstType(AstIndex index) {
		TypeNode* t = getAst!TypeNode(index);
		assertf(t.isType, "node is %s", t.astType);
		return t;
	}
	ExpressionNode* getAstExpr(AstIndex index) {
		ExpressionNode* t = getAst!ExpressionNode(index);
		assertf(t.isExpression, "node is %s", t.astType);
		return t;
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
		{
			if (file.mod.id == moduleId)
				return file.mod;
		}
		return null;
	}

	ModuleDeclNode* getModule(ModuleIndex index)
	{
		return files[index.fileIndex].mod;
	}

	FunctionDeclNode* getFunction(IrIndex index)
	{
		assertf(index.isFunction, "index is %s", index);
		AstIndex astIndex = AstIndex(index.storageUintIndex);
		return getAst!FunctionDeclNode(astIndex);
	}

	FunctionDeclNode* findFunction(string moduleName, string funcName)
	{
		ModuleDeclNode* mod = findModule(moduleName);
		if (mod is null) return null;

		return mod.findFunction(funcName, &this);
	}

	void findFunction(string funcName, void delegate(ModuleDeclNode*, FunctionDeclNode*) onFunction)
	{
		Identifier funcId = idMap.find(funcName);
		if (funcId.isUndefined) return;

		foreach (ref SourceFileInfo file; files.data)
		{
			FunctionDeclNode* fun = file.mod.findFunction(funcId, &this);
			if (fun) onFunction(file.mod, fun);
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
		auto numParams = funcDecl.signature.get!FunctionSignatureNode(&this).parameters.length;

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
		ObjectSymbol* funcSym = &objSymTab.getSymbol(funcDecl.backendData.objectSymIndex);
		return cast(JittedFunc)funcSym.dataPtr;
	}

	void printMemSize() {
		TextSink sink;
		printMemSize(sink);
		write(cast(string)sink.data.data);
	}
	void printMemSize(ref TextSink sink)
	{
		size_t byteLength;
		size_t committedBytes;
		size_t reservedBytes;
		sink.putfln("Arena sizes:         used  committed  reserved");
		void printArena(A)(ref A arena, string name) {
			sink.putfln("  %-16s%-6iB    %-6iB   %-6iB",
				name,
				scaledNumberFmt(arena.byteLength),
				scaledNumberFmt(arena.committedBytes),
				scaledNumberFmt(arena.reservedBytes));
			byteLength += arena.byteLength;
			committedBytes += arena.committedBytes;
			reservedBytes += arena.reservedBytes;
		}
		printArena(sourceBuffer, "source");
		printArena(files, "files");
		printArena(tokenBuffer, "tokens");
		printArena(tokenLocationBuffer, "token loc");
		printArena(astBuffer, "AST");
		printArena(arrayArena, "arrays");

		printArena(irStorage.instrHeaderBuffer, "IR instr header");
		printArena(irStorage.instrPayloadBuffer, "IR instr payload");
		printArena(irStorage.instrNextBuffer, "IR next ptr");
		printArena(irStorage.instrPrevBuffer, "IR prev ptr");
		printArena(irStorage.vregBuffer, "IR virt regs");
		printArena(irStorage.phiBuffer, "IR phi");
		printArena(irStorage.basicBlockBuffer, "IR basic blocks");
		printArena(irStorage.arrayBuffer, "IR arrays");
		irStorage.printMemSize(sink);

		printArena(vmBuffer, "vm");
		printArena(tempBuffer, "temp");
		printArena(types.buffer, "types");
		printArena(staticDataBuffer, "static RW data");
		printArena(roStaticDataBuffer, "static RO data");
		printArena(globals.buffer, "globals");
		printArena(globals.initializerBuffer, "global ini-rs");
		printArena(constants.buffer, "constants");
		printArena(constants.aggregateBuffer, "aggregates");
		printArena(importBuffer, "imports");
		printArena(objSymTab.buffer, "symbols");
		printArena(codeBuffer, "machine code");
		printArena(binaryBuffer, "binary");

		sink.putfln("  %-16s%-6iB    %-6iB   %-6iB",
			"  Total",
			scaledNumberFmt(byteLength),
			scaledNumberFmt(committedBytes),
			scaledNumberFmt(reservedBytes));
	}

	void initialize()
	{
		// populates idMap with common identifiers like this, length, ptr, min, max, sizeof...
		idMap.regCommonIds(&this);

		// Next we create ast node per CommonAstNodes entry. Make sure the order is the same

		// CommonAstNodes.undefined
		astBuffer.voidPut(1); // 0th slot is reserved for undefined index

		// CommonAstNodes.node_error
		AstIndex node_error = appendAst!ErrorAstNode();
		assertf(node_error == CommonAstNodes.node_error, "AstIndex mismatch for node_error %s != %s", node_error, cast(AstIndex)CommonAstNodes.node_error);

		// add basic types
		void makeBasic(AstIndex reqIndex, uint size, ulong minValue, ulong maxValue, BasicType basicType, int typeFlags = 0)
		{
			uint startPos = sourceBuffer.uintLength;
			string str = basicTypeNames[basicType];
			uint endPos = cast(uint)(startPos + str.length);
			sourceBuffer.put(str);
			TokenIndex tokIndex = TokenIndex(tokenLocationBuffer.uintLength);
			tokenLocationBuffer.put(SourceLocation(startPos, endPos, 0, 0));
			tokenBuffer.voidPut(1); // bump token buf too

			// we want the index returned from appendAst to be equal to reqIndex
			// because we have CommonAstNodes enum
			AstIndex index = appendAst!BasicTypeNode(tokIndex, size, minValue, maxValue, basicType, cast(ubyte)typeFlags);
			assertf(index == reqIndex,
				"Result AstIndex of basic type (%s) is not equal to required (%s). Creation order must match CommonAstNodes order",
				index, reqIndex);
		}

		// type nodes
		makeBasic(CommonAstNodes.type_error, 0, 0, 0, BasicType.t_error);
		makeBasic(CommonAstNodes.type_noreturn, 0, 0, 0, BasicType.t_noreturn);
		makeBasic(CommonAstNodes.type_void,  0, 0, 0, BasicType.t_void);
		makeBasic(CommonAstNodes.type_bool,  1, 0, 1, BasicType.t_bool , BasicTypeFlag.isBoolean);
		makeBasic(CommonAstNodes.type_null,  8, 0, 0, BasicType.t_null);

		// basic type nodes
		makeBasic(CommonAstNodes.type_i8,  1, byte.min, byte.max, BasicType.t_i8, BasicTypeFlag.isInteger | BasicTypeFlag.isSigned);
		makeBasic(CommonAstNodes.type_i16, 2, short.min, short.max, BasicType.t_i16, BasicTypeFlag.isInteger | BasicTypeFlag.isSigned);
		makeBasic(CommonAstNodes.type_i32, 4, int.min, int.max, BasicType.t_i32, BasicTypeFlag.isInteger | BasicTypeFlag.isSigned);
		makeBasic(CommonAstNodes.type_i64, 8, long.min, long.max, BasicType.t_i64, BasicTypeFlag.isInteger | BasicTypeFlag.isSigned);

		makeBasic(CommonAstNodes.type_u8,  1, ubyte.min, ubyte.max, BasicType.t_u8, BasicTypeFlag.isInteger);
		makeBasic(CommonAstNodes.type_u16, 2, ushort.min, ushort.max, BasicType.t_u16, BasicTypeFlag.isInteger);
		makeBasic(CommonAstNodes.type_u32, 4, uint.min, uint.max, BasicType.t_u32, BasicTypeFlag.isInteger);
		makeBasic(CommonAstNodes.type_u64, 8, ulong.min, ulong.max, BasicType.t_u64, BasicTypeFlag.isInteger);

		makeBasic(CommonAstNodes.type_f32, 4, 0, 0, BasicType.t_f32, BasicTypeFlag.isFloat);
		makeBasic(CommonAstNodes.type_f64, 8, 0, 0, BasicType.t_f64, BasicTypeFlag.isFloat);

		makeBasic(CommonAstNodes.type_alias, 4, uint.min, uint.max, BasicType.t_alias);
		makeBasic(CommonAstNodes.type_type, 4, uint.min, uint.max, BasicType.t_type);

		// custom types
		auto type_u8Ptr = appendAst!PtrTypeNode(TokenIndex(), basicTypeNodes(BasicType.t_u8));
		assertf(type_u8Ptr == CommonAstNodes.type_u8Ptr, "AstIndex mismatch for type_u8Ptr %s != %s", type_u8Ptr, cast(AstIndex)CommonAstNodes.type_u8Ptr);
		type_u8Ptr.gen_ir_type(&this); // we need to cache IR types too

		auto type_u8Slice = appendAst!SliceTypeNode(TokenIndex(), basicTypeNodes(BasicType.t_u8));
		assertf(type_u8Slice == CommonAstNodes.type_u8Slice, "AstIndex mismatch for type_u8Slice %s != %s", type_u8Slice, cast(AstIndex)CommonAstNodes.type_u8Slice);
		type_u8Slice.gen_ir_type(&this); // we need to cache IR types too

		// builtin nodes
		void makeBuiltin(AstIndex reqIndex, Identifier id, BuiltinId builtin) {
			AstIndex index = appendAst!BuiltinNode(TokenIndex(), id, builtin);
			assertf(index == reqIndex,
				"Result AstIndex of builtin node (%s) is not equal to required (%s). Creation order must match CommonAstNodes order",
				index, reqIndex);
		}

		makeBuiltin(CommonAstNodes.builtin_min, CommonIds.id_min, BuiltinId.int_min);
		makeBuiltin(CommonAstNodes.builtin_max, CommonIds.id_max, BuiltinId.int_max);
		makeBuiltin(CommonAstNodes.builtin_slice_length, CommonIds.id_length, BuiltinId.slice_length);
		makeBuiltin(CommonAstNodes.builtin_slice_ptr, CommonIds.id_ptr, BuiltinId.slice_ptr);
		makeBuiltin(CommonAstNodes.builtin_array_length, CommonIds.id_length, BuiltinId.array_length);
		makeBuiltin(CommonAstNodes.builtin_array_ptr, CommonIds.id_ptr, BuiltinId.array_ptr);
		makeBuiltin(CommonAstNodes.builtin_sizeof, CommonIds.id_sizeof, BuiltinId.type_sizeof);

		// CommonAstNodes end

		initializedAstBufSize = astBuffer.length;
		initializedIrTypeBufSize = types.buffer.length;

		// cache buitin type strings for error reporting
		initializedSourceBufSize = sourceBuffer.length;
		initializedTokenLocBufSize = tokenLocationBuffer.length;
	}

	void beginCompilation()
	{
		hasErrors = false;
		sourceBuffer.length = initializedSourceBufSize;
		files.clear;
		codeBuffer.clear;
		importBuffer.clear;
		tokenBuffer.length = initializedTokenLocBufSize; // same size as tok loc buf
		tokenLocationBuffer.length = initializedTokenLocBufSize;
		binaryBuffer.clear;
		irStorage.instrHeaderBuffer.clear;
		irStorage.instrPayloadBuffer.clear;
		irStorage.instrNextBuffer.clear;
		irStorage.instrPrevBuffer.clear;
		irStorage.vregBuffer.clear;
		irStorage.phiBuffer.clear;
		irStorage.basicBlockBuffer.clear;
		irStorage.arrayBuffer.clear;
		types.buffer.length = initializedIrTypeBufSize;
		vmBuffer.clear;
		tempBuffer.clear;
		roStaticDataBuffer.clear;
		staticDataBuffer.clear;
		objSymTab.buffer.clear;
		objSymTab.firstModule = LinkIndex();
		globals.buffer.clear;
		globals.initializerBuffer.clear;
		constants.buffer.clear;
		constants.aggregateBuffer.clear;
		astBuffer.length = initializedAstBufSize;
		arrayArena.clear;
		entryPoint = null;
		sink.clear;
		errorSink.clear;
		idMap.stringDataBuffer.clear;
		idMap.strings.clear;
		idMap.map = typeof(idMap.map).init;

		externalSymbols.clear();

		// needed because all arrays are cleared
		idMap.regCommonIds(&this);
	}
}

enum CommonAstNodes : AstIndex
{
	// reserved for undefined
	undefined                = AstIndex(0),

	// error. Nodes can point to error when name resolution failed
	node_error               = AstIndex(1),

	// basic type nodes
	// The order is the same as in TokenType enum
	// The order is the same as in BasicType enum
	type_error               = AstIndex(3),
	type_noreturn            = AstIndex(13),
	type_void                = AstIndex(23),
	type_bool                = AstIndex(33),
	type_null                = AstIndex(43),

	type_i8                  = AstIndex(53),
	type_i16                 = AstIndex(63),
	type_i32                 = AstIndex(73),
	type_i64                 = AstIndex(83),

	type_u8                  = AstIndex(93),
	type_u16                 = AstIndex(103),
	type_u32                 = AstIndex(113),
	type_u64                 = AstIndex(123),

	type_f32                 = AstIndex(133),
	type_f64                 = AstIndex(143),

	type_alias               = AstIndex(153),
	type_type                = AstIndex(163),
	// basic type nodes end

	// common custom types
	type_u8Ptr               = AstIndex(173),
	type_u8Slice             = AstIndex(177),

	// builtin nodes
	// The order is the same as in BuiltinId enum
	builtin_min              = AstIndex(182),
	builtin_max              = AstIndex(186),
	builtin_slice_length     = AstIndex(190),
	builtin_slice_ptr        = AstIndex(194),
	builtin_array_length     = AstIndex(198),
	builtin_array_ptr        = AstIndex(202),
	builtin_sizeof           = AstIndex(206),
	// builtin nodes end
}

private immutable AstIndex[BasicType.max + 1] basicTypesArray = [
	CommonAstNodes.type_error,
	CommonAstNodes.type_noreturn,
	CommonAstNodes.type_void,
	CommonAstNodes.type_bool,
	CommonAstNodes.type_null,
	CommonAstNodes.type_i8,
	CommonAstNodes.type_i16,
	CommonAstNodes.type_i32,
	CommonAstNodes.type_i64,
	CommonAstNodes.type_u8,
	CommonAstNodes.type_u16,
	CommonAstNodes.type_u32,
	CommonAstNodes.type_u64,
	CommonAstNodes.type_f32,
	CommonAstNodes.type_f64,
	CommonAstNodes.type_alias,
	CommonAstNodes.type_type,
];
private immutable AstIndex[BuiltinId.max + 1] builtinsArray = [
	CommonAstNodes.builtin_min,
	CommonAstNodes.builtin_max,
	CommonAstNodes.builtin_slice_length,
	CommonAstNodes.builtin_slice_ptr,
	CommonAstNodes.builtin_array_length,
	CommonAstNodes.builtin_array_ptr,
	CommonAstNodes.builtin_sizeof,
];
