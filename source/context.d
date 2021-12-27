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
	this(bool isICE, string file = __MODULE__, int line = __LINE__)
	{
		super(null, file, line);
		this.isICE = isICE;
	}
	/// True if Internal Compiler Error and not regular compilation error
	bool isICE;
}

///
enum IceBehavior : ubyte {
	exception,
	error,
	breakpoint
}

enum TargetOs : ubyte {
	windows,
	linux,
	macos,
}

immutable string[] TARGET_OS_STRING = [
	"windows-x64",
	"linux-x64",
	"macos-x64",
];

///
struct CompilationContext
{
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
	// TODO: replace with arena. This one uses GC
	TextSink tempBuf;

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

	HashMap!(ExtraNodeProperty, uint, ExtraNodeProperty.init) extraProperties;

	/// External modules that are provided by driver or created at runtime from @extern(module) dll functions
	HashMap!(Identifier, LinkIndex, Identifier.init) externalModules;
	/// Symbols provided by the environment (host symbols must be provided prior to compilation)
	/// Symbols imported from dll that are demanded by functions marked with @extern(module)
	HashMap!(ExternalSymbolId, LinkIndex, ExternalSymbolId.init) externalSymbols;
	/// Symbols, sections and references
	ObjectSymbolTable objSymTab;

	/// Buffer for output file generation (.exe)
	Arena!ubyte binaryBuffer;
	/// Buffer for output file generation (.har)
	Arena!ubyte bundleBuffer;

	// Stores astBuffer.length after initialize() call
	// to be reset on beginCompilation()
	private size_t initializedAstBufSize;
	private size_t initializedIrTypeBufSize; // ditto, types.buffer.length
	private size_t initializedSourceBufSize; // ditto, sourceBuffer.length
	size_t initializedTokenLocBufSize; // ditto, tokenLocationBuffer.length

	IrIndex i8PtrType;
	IrIndex i64PtrType;
	IrIndex v128Type;

	// sections
	LinkIndex[NUM_BUILTIN_SECTIONS] builtinSections;

	// modules
	LinkIndex builtinModuleIndex;

	// errors and debug

	Array!AnalysedNode analisysStack;
	// Used for error message, when crashing in the backend
	FunctionDeclNode* currentFunction;

	///
	string outputFilename = "out.exe";

	/// Set when BuildType.exe is used
	/// Set in CodeEmitter.compileFunction
	FunctionDeclNode* entryPoint;

	// build settings

	/// Target machine info
	MachineInfo* machineInfo = &mach_info_amd64;
	/// Host OS
	     version(Windows) enum TargetOs hostOS = TargetOs.windows;
	else version(linux)   enum TargetOs hostOS = TargetOs.linux;
	else version(OSX)     enum TargetOs hostOS = TargetOs.macos;
	else static assert(false, "Unhandled OS");
	/// Target OS
	TargetOs targetOs = hostOS;
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

	// Bitset of built-in version identifiers that are enabled
	uint enabledVersionIdentifiers;

	/// True if current/last pass had errors
	bool hasErrors;
	/// Text output for errors
	TextSink errorSink;
	/// Text output for errors and stack traces
	TextSink sink;
	/// If true, stack traces are added to output
	bool printTraceOnError;
	/// What happens on Internal Compiler Error
	IceBehavior iceBehavior = IceBehavior.exception;
	/// If true, every pass that generates/changes IR, performs validation
	bool validateIr = false;
	bool runTesters = true;
	/// More details in errors
	bool verboseErrors = false;
	bool conciseErrors() { return !verboseErrors; }

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

	// Don't output any files, but instead create a .har file containing all input files
	bool bundleInputs;

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

	CallConvention defaultCallConvention() {
		final switch(targetOs) {
			case TargetOs.windows: return CallConvention.win64;
			case TargetOs.linux: return CallConvention.sysv64;
			case TargetOs.macos: return CallConvention.sysv64;
		}
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

	noreturn unrecoverable_error(Args...)(TokenIndex tokIdx, string format, Args args)
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
		throw new CompilationException(false);
	}

	void assertf(Args...)(bool cond, string fmt, Args args, string file = __MODULE__, int line = __LINE__)
	{
		if (cond) return;

		auto ice_state = begin_ice(file, line);
		sink.putfln(fmt, args);
		end_ice(ice_state);
	}

	void assertf(Args...)(bool cond, TokenIndex tokIdx, string fmt, Args args, string file = __MODULE__, int line = __LINE__)
	{
		if (cond) return;

		auto ice_state = begin_ice(file, line);
		sink.putfln(fmt, args);
		end_ice(ice_state, tokIdx);
	}

	noreturn unreachable(string file = __MODULE__, int line = __LINE__)
	{
		auto ice_state = begin_ice(file, line);
		sink.putfln("Unreachable");
		end_ice(ice_state);
	}

	noreturn internal_error(Args...)(TokenIndex tokIdx, string format, Args args, string file = __MODULE__, int line = __LINE__)
	{
		auto ice_state = begin_ice(file, line);
		sink.putfln(format, args);
		end_ice(ice_state, tokIdx);
	}

	noreturn internal_error(Args...)(string format, Args args, string file = __MODULE__, int line = __LINE__)
	{
		auto ice_state = begin_ice(file, line);
		sink.putfln(format, args);
		end_ice(ice_state);
	}

	void begin_node_property_calculation(T)(T* node, NodeProperty prop)
		if (hasAstNodeType!T)
	{
		AstIndex nodeIndex = getAstNodeIndex(node);
		node.setPropertyState(prop, PropertyState.calculating);
		push_analized_node(AnalysedNode(nodeIndex, prop));
	}

	void end_node_property_calculation(T)(T* node, NodeProperty prop)
		if (hasAstNodeType!T)
	{
		node.setPropertyState(prop, PropertyState.calculated);
		pop_analized_node;
	}

	void push_analized_node(AnalysedNode item) {
		analisysStack.put(arrayArena, item);
	}

	void pop_analized_node() {
		assertf(!analisysStack.empty, "Excessive popping detected");
		analisysStack.unput(1);
	}

	noreturn circular_dependency(AstIndex nodeIndex, NodeProperty prop, string file = __MODULE__, int line = __LINE__)
	{
		push_analized_node(AnalysedNode(nodeIndex, prop));
		circular_dependency(file, line);
	}

	noreturn circular_dependency(string file = __MODULE__, int line = __LINE__)
	{
		size_t startLen = sink.data.length;
		sink.putfln("Error: Circular dependency");
		print_analysis_stack;
		if (printTraceOnError)
			try
				throw new Exception(null);
			catch (Exception e)
				sink.putf("%s", e.info);
		errorSink.put(sink.data[startLen..$]);
		hasErrors = true;
		throw new CompilationException(false, file, line);
	}

	private static struct IceState {
		size_t startLen;
		string file;
		int line;
	}

	private IceState begin_ice(string file, int line)
	{
		stdout.flush;

		size_t startLen = sink.data.length;
		sink.putf("%s:%s: ICE: ", file, line);
		return IceState(startLen, file, line);
	}

	private noreturn end_ice(IceState state, TokenIndex tokIdx = TokenIndex.init)
	{
		print_location(tokIdx);
		errorSink.put(sink.data[state.startLen..$]);
		hasErrors = true;
		handleICE(state.file, state.line);
	}

	private void print_location(TokenIndex tokIdx = TokenIndex.init)
	{
		if (tokIdx.isValid) {
			sink.putfln("- token %s", FmtSrcLoc(tokIdx, &this));
		}
		if (currentFunction) {
			sink.putfln("- module `%s`", ModuleNamePrinter(currentFunction._module.get!ModuleDeclNode(&this), &this));
			sink.putfln("- function `%s`", idString(currentFunction.id));
			sink.putfln("  - defined at %s", FmtSrcLoc(currentFunction.loc, &this));
		}
		print_analysis_stack;
	}

	private void print_analysis_stack()
	{
		if (analisysStack.empty) return;

		sink.putfln("Stack:");

		AnalysedNode top = analisysStack.back;

		while(!analisysStack.empty)
		{
			AnalysedNode currentItem = analisysStack.back;
			TokenIndex tokIdx = currentItem.nodeIndex.loc(&this);
			if (currentItem.nodeIndex == top.nodeIndex) sink.put("> ");
			else sink.put("  ");
			sink.putf("%s: node.%s %s %s ", FmtSrcLoc(tokIdx, &this), currentItem.nodeIndex.storageIndex, currentItem.prop, currentItem.nodeIndex.astType(&this));
			print_node_name(sink, currentItem.nodeIndex, &this);
			sink.putln;
			analisysStack.unput(1);
		}
	}

	void todo(Args...)(string format, Args args, string file = __MODULE__, int line = __LINE__)
	{
		if (printTodos) {
			writef("TODO(%s:%s): ", file, line);
			writefln(format, args);
		}
	}

	private noreturn handleICE(string file, int line)
	{
		final switch(iceBehavior)
		{
			case IceBehavior.exception: throw new CompilationException(true, file, line);
			case IceBehavior.error: assert(false);
			case IceBehavior.breakpoint:
				writeln(sink.text);
				stdout.flush;
				version(D_InlineAsm_X86_64) {
					asm nothrow @nogc { int 3; } // breakpoint
					assert(false);
				} else version(LDC) {
					import ldc.intrinsics: llvm_debugtrap;
					llvm_debugtrap();
				} else {
					assert(false);
				}
		}
	}

	void throwOnErrors(string file = __MODULE__, int line = __LINE__)
	{
		if (hasErrors) throw new CompilationException(false, file, line);
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
		//assert(index, "node is null");
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

	ScopeIndex appendScope() {
		uint resIndex = astBuffer.uintLength;
		Scope* obj = cast(Scope*)astBuffer.nextPtr;
		enum size_t numAllocatedSlots = divCeil(Scope.sizeof, uint.sizeof);
		astBuffer.voidPut(numAllocatedSlots);
		*obj = Scope.init;
		return ScopeIndex(resIndex);
	}

	Scope* getAstScope(ScopeIndex index) {
		if (!index) return null;
		return cast(Scope*)(&astBuffer.bufPtr[index.storageIndex]);
	}
	AstNode* getAstNode(AstIndex index) { return getAst!AstNode(index); }
	TypeNode* getAstType(AstIndex index) {
		assertf(index.isDefined, "getAstType: null index");
		TypeNode* t = getAst!TypeNode(index);
		assertf(t.isType, t.loc, "node is not a type: %s", t.astType);
		return t;
	}
	ExpressionNode* getAstExpr(AstIndex index) { return getAst!ExpressionNode(index); }

	SourceFileInfo* getFileFromToken(TokenIndex tokIndex) {
		assert(tokIndex < tokenBuffer.length, format("getFileFromToken(%s), numTokens %s", tokIndex, tokenBuffer.length));

		SourceFileInfo* lastFile;
		foreach(ref SourceFileInfo file; files.data)
		{
			// We are parsing and this module is not parsed yet, so it's previous one
			if (!file.firstTokenIndex.isValid) break;

			if (tokIndex < file.firstTokenIndex) {
				assertf(lastFile !is null,
					"Cannot find file of token %s, before first file starting at %s",
					tokIndex, file.firstTokenIndex);
				return lastFile;
			}
			lastFile = &file;
		}
		if (lastFile is null) {
			internal_error("Cannot find file of token %s, no files", tokIndex);
		}
		return lastFile;
	}

	// Checks if symbol can be accessed from code segment with 32bit signed offset
	private bool canReferenceFromCode(void* hostSym)
	{
		void* start = codeBuffer.bufPtr;
		void* end = codeBuffer.bufPtr + codeBuffer.length;
		bool reachesFromStart = (hostSym - start) == cast(int)(hostSym - start);
		bool reachesFromEnd = (hostSym - end) == cast(int)(hostSym - end);
		return reachesFromStart && reachesFromEnd;
	}

	/// Returns index of external (host or dll) module
	LinkIndex getOrCreateExternalModule(Identifier modId, ObjectModuleKind modKind) {
		LinkIndex externalModuleIndex = externalModules.get(modId);
		if (externalModuleIndex.isDefined) return externalModuleIndex;

		ObjectModule externalModule = {
			kind : modKind,
			id : modId
		};
		externalModuleIndex = objSymTab.addModule(externalModule);
		externalModules.put(arrayArena, modId, externalModuleIndex);

		return externalModuleIndex;
	}

	void addHostSymbol(LinkIndex hostModuleIndex, ExternalSymbolId externalId, void* symPtr)
	{
		LinkIndex importedSymbolIndex;

		if (canReferenceFromCode(symPtr))
		{
			ObjectSymbol importedSymbol = {
				kind : ObjectSymbolKind.isHost,
				id : externalId.symId,
				dataPtr : cast(ubyte*)symPtr,
				sectionOffset : cast(ulong)symPtr,
				sectionIndex : builtinSections[ObjectSectionType.host],
				moduleIndex : hostModuleIndex,
			};
			importedSymbolIndex = objSymTab.addSymbol(importedSymbol);
		}
		else
		{
			ulong sectionOffset = importBuffer.length;
			ulong ptr = cast(ulong)symPtr;
			importBuffer.put(*cast(ubyte[8]*)&ptr);

			ObjectSymbol importedSymbol = {
				kind : ObjectSymbolKind.isHost,
				flags : ObjectSymbolFlags.isIndirect,
				id : externalId.symId,
				sectionOffset : sectionOffset,
				sectionIndex : builtinSections[ObjectSectionType.imports],
				moduleIndex : hostModuleIndex,
			};
			importedSymbolIndex = objSymTab.addSymbol(importedSymbol);
		}
		externalSymbols.put(arrayArena, externalId, importedSymbolIndex);
	}

	LinkIndex addDllModuleSymbol(LinkIndex dllModuleIndex, ExternalSymbolId externalId)
	{
		ObjectSymbol importedSymbol = {
			kind : ObjectSymbolKind.isImported,
			flags : ObjectSymbolFlags.isIndirect,
			id : externalId.symId,
			alignmentPower : 3, // pointer size
			sectionIndex : builtinSections[ObjectSectionType.imports],
			moduleIndex : dllModuleIndex,
		};
		LinkIndex importedSymbolIndex = objSymTab.addSymbol(importedSymbol);
		externalSymbols.put(arrayArena, externalId, importedSymbolIndex);
		return importedSymbolIndex;
	}

	ModuleDeclNode* getModuleFromToken(TokenIndex tokIndex) {
		return getFileFromToken(tokIndex).mod;
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

	FunctionDeclNode* tryFindFunction(string moduleName, string funcName)
	{
		ModuleDeclNode* mod = findModule(moduleName);
		if (mod is null) return null;

		return mod.findFunction(funcName, &this);
	}

	FunctionDeclNode* findFunction(string moduleName, string funcName)
	{
		ModuleDeclNode* mod = findModule(moduleName);
		if (mod is null) internal_error("Cannot find module `%s` while searching for `%s.%s`", moduleName, moduleName, funcName);

		auto fun = mod.findFunction(funcName, &this);
		if (fun is null) internal_error("Cannot find function `%s` inside module `%s`", funcName, moduleName);
		return fun;
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

	FunctionDeclNode* getFunctionDecl(string funcName)
	{
		FunctionDeclNode* funDecl;
		foreach (ref SourceFileInfo file; files.data)
		{
			FunctionDeclNode* fun = file.mod.findFunction(funcName, &this);
			if (fun !is null)
			{
				if (funDecl !is null)
					internal_error("Function %s is found in 2 places", funcName);
				funDecl = fun;
			}
		}

		if (funDecl is null)
			internal_error("Function `%s` is not found in %s modules", funcName, files.length);

		return funDecl;
	}

	auto getFunctionPtr(ResultType, ParamTypes...)(string funcName)
	{
		return getFunctionPtr!(ResultType, ParamTypes)(getFunctionDecl(funcName));
	}

	auto getFunctionPtr(ResultType, ParamTypes...)(string moduleName, string funcName)
	{
		FunctionDeclNode* func = findFunction(moduleName, funcName);
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
		ObjectSymbol* funcSym = objSymTab.getSymbol(funcDecl.backendData.objectSymIndex);
		return cast(JittedFunc)funcSym.dataPtr;
	}

	IrIndex get_file_name_constant(AstIndex mod) {
		auto node = mod.get!ModuleDeclNode(&this);
		bool wasCreated;
		auto key = ExtraNodeProperty(ExtraProperty.fileName, mod.storageIndex);
		uint* value = extraProperties.getOrCreate(arrayArena, key, wasCreated);
		if (wasCreated) {
			IrIndex val = makeStringLiteralIrConstant(node.fileName(&this), node.objectSymIndex, &this);
			*value = val.asUint;
		}
		return IrIndex.fromUint(*value);
	}
	IrIndex get_function_name_constant(AstIndex func) {
		auto node = func.get!FunctionDeclNode(&this);
		bool wasCreated;
		auto key = ExtraNodeProperty(ExtraProperty.nodeName, func.storageIndex);
		uint* value = extraProperties.getOrCreate(arrayArena, key, wasCreated);
		if (wasCreated) {
			IrIndex val = makeStringLiteralIrConstant(idString(node.id), node._module.get!ModuleDeclNode(&this).objectSymIndex, &this);
			*value = val.asUint;
		}
		return IrIndex.fromUint(*value);
	}
	IrIndex get_module_name_constant(AstIndex mod) {
		auto node = mod.get!ModuleDeclNode(&this);
		bool wasCreated;
		auto key = ExtraNodeProperty(ExtraProperty.nodeName, mod.storageIndex);
		uint* value = extraProperties.getOrCreate(arrayArena, key, wasCreated);
		if (wasCreated) {
			IrIndex val = makeStringLiteralIrConstant(idString(node.id), node.objectSymIndex, &this);
			*value = val.asUint;
		}
		return IrIndex.fromUint(*value);
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
		printArena(irStorage.stackSlotBuffer, "IR stack slots");
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
		printArena(bundleBuffer, "bundle");

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

		// CommonAstNodes.node_root_package
		AstIndex node_root_package = appendAst!PackageDeclNode();
		assertf(node_root_package == CommonAstNodes.node_root_package, "AstIndex mismatch for node_root_package %s != %s", node_root_package, cast(AstIndex)CommonAstNodes.node_root_package);

		// add basic types
		void makeBasic(AstIndex reqIndex, uint size, ubyte alignPow, ulong minValue, ulong maxValue, BasicType basicType, int typeFlags = 0)
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
			AstIndex index = appendAst!BasicTypeNode(tokIndex, CommonAstNodes.type_type, SizeAndAlignment(size, alignPow), minValue, maxValue, basicType, cast(ubyte)typeFlags);
			assertf(index == reqIndex,
				"Result AstIndex of basic type %s (%s) is not equal to required (%s). Creation order must match CommonAstNodes order",
				basicType, index, reqIndex);
		}

		// type nodes
		makeBasic(CommonAstNodes.type_error, 0, 0, 0, 0, BasicType.t_error);
		makeBasic(CommonAstNodes.type_noreturn, 0, 0, 0, 0, BasicType.t_noreturn);
		makeBasic(CommonAstNodes.type_void,  0, 0, 0, 0, BasicType.t_void);
		makeBasic(CommonAstNodes.type_bool,  1, 0, 0, 1, BasicType.t_bool , BasicTypeFlag.isBoolean);
		makeBasic(CommonAstNodes.type_null,  8, 3, 0, 0, BasicType.t_null);

		// basic type nodes
		makeBasic(CommonAstNodes.type_i8,  1, 0, byte.min, byte.max, BasicType.t_i8, BasicTypeFlag.isInteger | BasicTypeFlag.isSigned);
		makeBasic(CommonAstNodes.type_i16, 2, 1, short.min, short.max, BasicType.t_i16, BasicTypeFlag.isInteger | BasicTypeFlag.isSigned);
		makeBasic(CommonAstNodes.type_i32, 4, 2, int.min, int.max, BasicType.t_i32, BasicTypeFlag.isInteger | BasicTypeFlag.isSigned);
		makeBasic(CommonAstNodes.type_i64, 8, 3, long.min, long.max, BasicType.t_i64, BasicTypeFlag.isInteger | BasicTypeFlag.isSigned);

		makeBasic(CommonAstNodes.type_u8,  1, 0, ubyte.min, ubyte.max, BasicType.t_u8, BasicTypeFlag.isInteger);
		makeBasic(CommonAstNodes.type_u16, 2, 1, ushort.min, ushort.max, BasicType.t_u16, BasicTypeFlag.isInteger);
		makeBasic(CommonAstNodes.type_u32, 4, 2, uint.min, uint.max, BasicType.t_u32, BasicTypeFlag.isInteger);
		makeBasic(CommonAstNodes.type_u64, 8, 3, ulong.min, ulong.max, BasicType.t_u64, BasicTypeFlag.isInteger);

		makeBasic(CommonAstNodes.type_f32, 4, 2, 0, 0, BasicType.t_f32, BasicTypeFlag.isFloat);
		makeBasic(CommonAstNodes.type_f64, 8, 3, 0, 0, BasicType.t_f64, BasicTypeFlag.isFloat);

		makeBasic(CommonAstNodes.type_alias, 4, 2, uint.min, uint.max, BasicType.t_alias);
		makeBasic(CommonAstNodes.type_type, 4, 2, uint.min, uint.max, BasicType.t_type);

		// custom types
		auto type_u8Ptr = appendAst!PtrTypeNode(TokenIndex(), CommonAstNodes.type_type, CommonAstNodes.type_u8);
		assertf(type_u8Ptr == CommonAstNodes.type_u8Ptr, "AstIndex mismatch for type_u8Ptr %s != %s", type_u8Ptr, cast(AstIndex)CommonAstNodes.type_u8Ptr);
		type_u8Ptr.gen_ir_type(&this); // we need to cache IR types too

		auto type_u8Slice = appendAst!SliceTypeNode(TokenIndex(), CommonAstNodes.type_type, CommonAstNodes.type_u8);
		assertf(type_u8Slice == CommonAstNodes.type_u8Slice, "AstIndex mismatch for type_u8Slice %s != %s", type_u8Slice, cast(AstIndex)CommonAstNodes.type_u8Slice);
		type_u8Slice.gen_ir_type(&this); // we need to cache IR types too

		auto type_aliasSlice = appendAst!SliceTypeNode(TokenIndex(), CommonAstNodes.type_type, CommonAstNodes.type_alias);
		assertf(type_aliasSlice == CommonAstNodes.type_aliasSlice, "AstIndex mismatch for type_aliasSlice %s != %s", type_aliasSlice, cast(AstIndex)CommonAstNodes.type_aliasSlice);
		type_aliasSlice.gen_ir_type(&this); // we need to cache IR types too

		// builtin nodes
		void makeBuiltin(AstIndex reqIndex, Identifier id, BuiltinId builtin) {
			AstIndex index = appendAst!BuiltinNode(TokenIndex(), id, builtin);
			assertf(index == reqIndex,
				"Result AstIndex of builtin node %s (%s) is not equal to required (%s). Creation order must match CommonAstNodes order",
				idString(id), index, reqIndex);
		}

		makeBuiltin(CommonAstNodes.builtin_min, CommonIds.id_min, BuiltinId.int_min);
		makeBuiltin(CommonAstNodes.builtin_max, CommonIds.id_max, BuiltinId.int_max);
		makeBuiltin(CommonAstNodes.builtin_slice_length, CommonIds.id_length, BuiltinId.slice_length);
		makeBuiltin(CommonAstNodes.builtin_slice_ptr, CommonIds.id_ptr, BuiltinId.slice_ptr);
		makeBuiltin(CommonAstNodes.builtin_array_length, CommonIds.id_length, BuiltinId.array_length);
		makeBuiltin(CommonAstNodes.builtin_array_ptr, CommonIds.id_ptr, BuiltinId.array_ptr);
		makeBuiltin(CommonAstNodes.builtin_sizeof, CommonIds.id_sizeof, BuiltinId.type_sizeof);
		// CommonAstNodes end

		i8PtrType = types.appendPtr(makeIrType(IrBasicType.i8));
		i64PtrType = types.appendPtr(makeIrType(IrBasicType.i64));
		v128Type = types.appendArray(makeIrType(IrBasicType.i8), 16);

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
		bundleBuffer.clear;
		irStorage.instrHeaderBuffer.clear;
		irStorage.instrPayloadBuffer.clear;
		irStorage.instrNextBuffer.clear;
		irStorage.instrPrevBuffer.clear;
		irStorage.vregBuffer.clear;
		irStorage.phiBuffer.clear;
		irStorage.basicBlockBuffer.clear;
		irStorage.arrayBuffer.clear;
		irStorage.stackSlotBuffer.clear;
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

		analisysStack = analisysStack.init;
		currentFunction = null;

		extraProperties = extraProperties.init;

		externalModules = externalModules.init;
		externalSymbols = externalSymbols.init;

		auto rootPackage = CommonAstNodes.node_root_package.get!PackageDeclNode(&this);
		*rootPackage = PackageDeclNode.init;

		// needed because all arrays are cleared
		idMap.regCommonIds(&this);

		addSections(&this);
		createBuiltinFunctions(&this);
		setVersionIds(&this);
	}
}

enum ExtraProperty : uint {
	fileName, // __FILE__
	nodeName, // __FUNCTION_NAME__, __MODULE_NAME__
	nodeFqn, // __FUNCTION_FQN__, __MODULE_FQN__
}

struct ExtraNodeProperty
{
	ExtraProperty prop;
	uint node;
}

struct AnalysedNode
{
	AstIndex nodeIndex;
	NodeProperty prop;
}

// How many 4 byte slots are required to store node in astBuffer
enum slotsPerNode(T) = divCeil(T.sizeof, uint.sizeof);

enum BASE_NODE_SLOTS = slotsPerNode!AstNode;
enum BUILTIN_FUNC_SLOTS = slotsPerNode!VariableDeclNode + slotsPerNode!FunctionSignatureNode + slotsPerNode!FunctionDeclNode; // assumes single parameter

enum CommonAstNodes : AstIndex
{
	// reserved for undefined
	undefined                = AstIndex(0),

	// error. Nodes can point to error when name resolution failed
	node_error               = AstIndex(1),
	node_root_package        = AstIndex(node_error.storageIndex+BASE_NODE_SLOTS),

	first_type               = AstIndex(node_root_package.storageIndex + slotsPerNode!PackageDeclNode),
	// basic type nodes
	// The order is the same as in TokenType enum
	// The order is the same as in BasicType enum
	type_error               = AstIndex(first_type.storageIndex +  0*NumBasicTypeNodeSlots),
	type_noreturn            = AstIndex(first_type.storageIndex +  1*NumBasicTypeNodeSlots),
	type_void                = AstIndex(first_type.storageIndex +  2*NumBasicTypeNodeSlots),
	type_bool                = AstIndex(first_type.storageIndex +  3*NumBasicTypeNodeSlots),
	type_null                = AstIndex(first_type.storageIndex +  4*NumBasicTypeNodeSlots),

	type_i8                  = AstIndex(first_type.storageIndex +  5*NumBasicTypeNodeSlots),
	type_i16                 = AstIndex(first_type.storageIndex +  6*NumBasicTypeNodeSlots),
	type_i32                 = AstIndex(first_type.storageIndex +  7*NumBasicTypeNodeSlots),
	type_i64                 = AstIndex(first_type.storageIndex +  8*NumBasicTypeNodeSlots),

	type_u8                  = AstIndex(first_type.storageIndex +  9*NumBasicTypeNodeSlots),
	type_u16                 = AstIndex(first_type.storageIndex + 10*NumBasicTypeNodeSlots),
	type_u32                 = AstIndex(first_type.storageIndex + 11*NumBasicTypeNodeSlots),
	type_u64                 = AstIndex(first_type.storageIndex + 12*NumBasicTypeNodeSlots),

	type_f32                 = AstIndex(first_type.storageIndex + 13*NumBasicTypeNodeSlots),
	type_f64                 = AstIndex(first_type.storageIndex + 14*NumBasicTypeNodeSlots),

	type_alias               = AstIndex(first_type.storageIndex + 15*NumBasicTypeNodeSlots),
	type_type                = AstIndex(first_type.storageIndex + 16*NumBasicTypeNodeSlots),
	// basic type nodes end

	first_compound           = AstIndex(first_type.storageIndex + 17*NumBasicTypeNodeSlots),

	// common custom types
	type_u8Ptr               = AstIndex(first_compound.storageIndex),
	type_u8Slice             = AstIndex(type_u8Ptr.storageIndex + slotsPerNode!PtrTypeNode),
	type_aliasSlice          = AstIndex(type_u8Slice.storageIndex + slotsPerNode!SliceTypeNode),

	first_builtin_member     = AstIndex(type_aliasSlice.storageIndex + slotsPerNode!SliceTypeNode),

	// builtin nodes
	// The order is the same as in BuiltinId enum
	builtin_min              = AstIndex(first_builtin_member.storageIndex + 0*slotsPerNode!BuiltinNode),
	builtin_max              = AstIndex(first_builtin_member.storageIndex + 1*slotsPerNode!BuiltinNode),
	builtin_slice_length     = AstIndex(first_builtin_member.storageIndex + 2*slotsPerNode!BuiltinNode),
	builtin_slice_ptr        = AstIndex(first_builtin_member.storageIndex + 3*slotsPerNode!BuiltinNode),
	builtin_array_length     = AstIndex(first_builtin_member.storageIndex + 4*slotsPerNode!BuiltinNode),
	builtin_array_ptr        = AstIndex(first_builtin_member.storageIndex + 5*slotsPerNode!BuiltinNode),
	builtin_sizeof           = AstIndex(first_builtin_member.storageIndex + 6*slotsPerNode!BuiltinNode),
	// builtin nodes end

	// builtin functions
	first_builtin_func       = AstIndex(first_builtin_member.storageIndex + 7*slotsPerNode!BuiltinNode + slotsPerNode!VariableDeclNode + slotsPerNode!FunctionSignatureNode),

	compile_error            = AstIndex(first_builtin_func.storageIndex + 0*BUILTIN_FUNC_SLOTS),
	is_slice                 = AstIndex(first_builtin_func.storageIndex + 1*BUILTIN_FUNC_SLOTS),
	is_integer               = AstIndex(first_builtin_func.storageIndex + 2*BUILTIN_FUNC_SLOTS),
	is_pointer               = AstIndex(first_builtin_func.storageIndex + 3*BUILTIN_FUNC_SLOTS),
	base_of                  = AstIndex(first_builtin_func.storageIndex + 4*BUILTIN_FUNC_SLOTS),
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
immutable AstIndex[5] builtinFuncsArray = [
	CommonAstNodes.compile_error,
	CommonAstNodes.is_slice,
	CommonAstNodes.is_integer,
	CommonAstNodes.is_pointer,
	CommonAstNodes.base_of,
];

void builtin_function_stub() {
	assert(false, "Trying to call CTFE-only function at runtime");
}

void createBuiltinFunctions(CompilationContext* c)
{
	ObjectModule builtinModule = {
		kind : ObjectModuleKind.isHost,
		id : c.idMap.getOrRegNoDup(c, ":builtin")
	};
	c.builtinModuleIndex = c.objSymTab.addModule(builtinModule);

	AstNodes params;
	ubyte numDefaultParams;
	void addParam(AstIndex type, Identifier id, AstIndex defaultValue = AstIndex())
	{
		ushort paramIndex = cast(ushort)params.length;
		AstIndex param = c.appendAst!VariableDeclNode(TokenIndex(), ScopeIndex(), type, defaultValue, id);
		auto paramNode = param.get!VariableDeclNode(c);
		paramNode.flags |= VariableFlags.isParameter;
		paramNode.scopeIndex = paramIndex;
		paramNode.state = AstNodeState.type_check_done;
		if (numDefaultParams > 0) c.assertf(defaultValue.isDefined, "default params cannot be followed by non-default");
		if (defaultValue.isDefined) ++numDefaultParams;
		params.put(c.arrayArena, param);
	}
	void make(AstIndex reqIndex, Identifier id, AstIndex retType) {
		AstIndex signature = c.appendAst!FunctionSignatureNode(TokenIndex(), retType, params, c.defaultCallConvention, numDefaultParams);
		auto sigNode = signature.get!FunctionSignatureNode(c);
		sigNode.state = AstNodeState.type_check_done;
		sigNode.flags |= FuncSignatureFlags.isCtfeOnly;
		params = AstNodes.init;
		numDefaultParams = 0;

		AstIndex func = c.appendAst!FunctionDeclNode(TokenIndex(), AstIndex(), ScopeIndex(), signature, id);
		auto funcNode = func.get!FunctionDeclNode(c);
		funcNode.state = AstNodeState.type_check_done;
		funcNode.flags |= FuncDeclFlags.isBuiltin;

		ulong sectionOffset = c.importBuffer.length;
		ulong ptr = cast(ulong)&builtin_function_stub;
		c.importBuffer.put(*cast(ubyte[8]*)&ptr);

		ObjectSymbol sym = {
			kind : ObjectSymbolKind.isHost,
			flags : ObjectSymbolFlags.isIndirect,
			id : id,
			sectionOffset : sectionOffset,
			sectionIndex : c.builtinSections[ObjectSectionType.imports],
			moduleIndex : c.builtinModuleIndex,
		};
		funcNode.backendData.objectSymIndex = c.objSymTab.addSymbol(sym);

		c.assertf(func == reqIndex,
			"Result AstIndex of builtin node %s (%s) is not equal to required (%s). Creation order must match CommonAstNodes order",
			c.idString(id), func, reqIndex);
	}

	addParam(CommonAstNodes.type_u8Slice, CommonIds.id_message);
	make(CommonAstNodes.compile_error, CommonIds.cash_compile_error, CommonAstNodes.type_noreturn);

	addParam(CommonAstNodes.type_type, CommonIds.id_type);
	make(CommonAstNodes.is_slice, CommonIds.cash_is_slice, CommonAstNodes.type_bool);

	addParam(CommonAstNodes.type_type, CommonIds.id_type);
	make(CommonAstNodes.is_integer, CommonIds.cash_is_integer, CommonAstNodes.type_bool);

	addParam(CommonAstNodes.type_type, CommonIds.id_type);
	make(CommonAstNodes.is_pointer, CommonIds.cash_is_pointer, CommonAstNodes.type_bool);

	addParam(CommonAstNodes.type_type, CommonIds.id_type);
	make(CommonAstNodes.base_of, CommonIds.cash_base_of, CommonAstNodes.type_type);

	//print_ast(CommonAstNodes.is_slice, c);
}

void addSections(CompilationContext* c)
{
	ObjectSection hostSection = {
		type : ObjectSectionType.host,
		alignmentPower : 0,
		id : c.idMap.getOrRegNoDup(c, ".host")
	};
	c.builtinSections[ObjectSectionType.host] = c.objSymTab.addSection(hostSection);

	ObjectSection importSection = {
		type : ObjectSectionType.imports,
		flags : ObjectSectionFlags.read | ObjectSectionFlags.write,
		alignmentPower : 12, // 4096
		id : c.idMap.getOrRegNoDup(c, ".idata"),
		buffer : &c.importBuffer,
	};
	c.builtinSections[ObjectSectionType.imports] = c.objSymTab.addSection(importSection);

	ObjectSection dataSection = {
		type : ObjectSectionType.rw_data,
		flags : ObjectSectionFlags.read | ObjectSectionFlags.write,
		alignmentPower : 12, // 4096
		id : c.idMap.getOrRegNoDup(c, ".data"),
		buffer : &c.staticDataBuffer,
	};
	c.builtinSections[ObjectSectionType.rw_data] = c.objSymTab.addSection(dataSection);

	ObjectSection rdataSection = {
		type : ObjectSectionType.ro_data,
		flags : ObjectSectionFlags.read,
		alignmentPower : 12, // 4096
		id : c.idMap.getOrRegNoDup(c, ".rdata"),
		buffer : &c.roStaticDataBuffer,
	};
	c.builtinSections[ObjectSectionType.ro_data] = c.objSymTab.addSection(rdataSection);

	ObjectSection textSection = {
		type : ObjectSectionType.code,
		flags : ObjectSectionFlags.execute | ObjectSectionFlags.read,
		alignmentPower : 12, // 4096
		id : c.idMap.getOrRegNoDup(c, ".text"),
		buffer : &c.codeBuffer,
	};
	c.builtinSections[ObjectSectionType.code] = c.objSymTab.addSection(textSection);
}

void setVersionIds(CompilationContext* c)
{
	final switch(c.targetOs) {
		case TargetOs.windows: c.enabledVersionIdentifiers |= 1 << VersionId.id_windows; break;
		case TargetOs.linux: c.enabledVersionIdentifiers |= 1 << VersionId.id_linux; break;
		case TargetOs.macos: c.enabledVersionIdentifiers |= 1 << VersionId.id_macos; break;
	}
}

struct Slice(T) {
	this(T[] data) {
		ptr = data.ptr;
		length = data.length;
	}
	ulong length;
	T* ptr;
	T[] slice() { return ptr[0..length]; }
	alias slice this;
}
alias SliceString = Slice!(const(char));
