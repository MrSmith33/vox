/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module driver;

import std.stdio : writeln, write, writef, writefln, stdout;
import std.path : baseName, stripExtension;

import all;

/// To compile a set of modules do following steps:
/// 1. initialize(passes)
/// 2. beginCompilation()
/// 3. addHostSymbols(hostSymbols)  --+
/// 4. addDllModules(dllModules)      | In any order
/// foreach(module; modules)          |
/// 5. addModule(module)  ------------+
/// 6. compile()
/// 7. markCodeAsExecutable() if in JIT mode
/// 8. releaseMemory()
struct Driver
{
	CompilationContext context;
	CompilePassGlobal[] passes;

	ArenaPool arenaPool;

	static void funWithAddress(){}
	void initialize(CompilePassGlobal[] passes_)
	{
		passes = passes_;

		// IrIndex can address 2^28 * 4 bytes = 1GB
		size_t BYTES_TO_RESERVE = GiB*178;
		arenaPool.reserve(BYTES_TO_RESERVE);
		//writefln("arenaPool %X .. %X", arenaPool.buffer.ptr, arenaPool.buffer.ptr+arenaPool.buffer.length);

		/// Those 3 must be allocated in this order (or in inverse order)
		/// Code must be able to use RIP-relative addressing into static data (and into import data when in JIT mode)
		context.importBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.codeBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.staticDataBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.roStaticDataBuffer.setBuffer(arenaPool.take(GiB), 0);

		context.sourceBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.files.setBuffer(arenaPool.take(GiB), 0);
		context.tokenBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.tokenLocationBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.binaryBuffer.setBuffer(arenaPool.take(GiB), 0);

		context.irStorage.instrHeaderBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.instrPayloadBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.instrNextBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.instrPrevBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.vregBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.phiBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.basicBlockBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.arrayBuffer.setBuffer(arenaPool.take(8*GiB), 0);

		context.types.buffer.setBuffer(arenaPool.take(GiB), 0);
		context.tempBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.vmBuffer.setBuffer(arenaPool.take(4*GiB), 0);
		context.objSymTab.buffer.setBuffer(arenaPool.take(GiB), 0);
		context.globals.buffer.setBuffer(arenaPool.take(GiB), 0);
		context.globals.initializerBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.constants.buffer.setBuffer(arenaPool.take(GiB), 0);
		context.constants.aggregateBuffer.setBuffer(arenaPool.take(4*GiB), 0);
		context.astBuffer.setBuffer(arenaPool.take(16*GiB), 0);
		context.arrayArena.setBuffers(
			arenaPool.take(12*4*GiB),
			arenaPool.take(16*GiB));

		context.idMap.strings.setBuffer(arenaPool.take(2*GiB), 0);
		context.idMap.stringDataBuffer.setBuffer(arenaPool.take(2*GiB), 0);

		// all arena sizes must sum up to predefined constant
		context.assertf(arenaPool.takenBytes == BYTES_TO_RESERVE,
			"%s bytes taken, %s bytes to take", arenaPool.takenBytes, BYTES_TO_RESERVE);

		context.initialize();
	}

	void releaseMemory()
	{
		arenaPool.decommitAll;
	}

	void beginCompilation()
	{
		markAsRW(context.codeBuffer.bufPtr, divCeil(context.codeBuffer.length, PAGE_SIZE));
		markAsRW(context.roStaticDataBuffer.bufPtr, divCeil(context.roStaticDataBuffer.length, PAGE_SIZE));
		context.beginCompilation;
		foreach(ref pass; passes) pass.clear;
	}

	void addModule(SourceFileInfo moduleFile)
	{
		uint fileIndex = cast(uint)context.files.length;
		context.files.put(moduleFile);
		SourceFileInfo* file = &context.files.back();

		Identifier id = context.idMap.getOrRegNoDup(&context, file.name.baseName.stripExtension);
		ObjectModule localModule = {
			kind : ObjectModuleKind.isLocal,
			id : id
		};
		auto mod = context.appendAst!ModuleDeclNode();
		file.mod = context.getAst!ModuleDeclNode(mod);
		file.mod.moduleIndex = ModuleIndex(fileIndex);
		file.mod.id = id;
		file.mod.objectSymIndex = context.objSymTab.addModule(localModule);
	}

	void addHar(string harFilename, const(char)[] harData)
	{
		void onHarFile(SourceFileInfo fileInfo) {
			addModule(fileInfo);
		}
		parseHar(context, harFilename, harData, &onHarFile);
	}

	void compile()
	{
		foreach (ref pass; passes)
		{
			auto time1 = currTime;

			// throws immediately on unrecoverable error or ICE
			pass.run(context, pass.subPasses);

			auto time2 = currTime;
			pass.duration = time2-time1;

			// throws if there were recoverable error in the pass
			context.throwOnErrors;
		}
	}

	/// Must be called after compilation is finished and before execution
	/// Effect is reverted with the call to beginCompilation
	/// Marks code pages as read-execute, and readonly data pages as read-only
	/// Clears zero-initialized data
	/// Only needed in JIT mode, not needed in AOT mode
	void markCodeAsExecutable()
	{
		markAsExecutable(context.codeBuffer.bufPtr, divCeil(context.codeBuffer.length, PAGE_SIZE));
		markAsRO(context.roStaticDataBuffer.bufPtr, divCeil(context.roStaticDataBuffer.length, PAGE_SIZE));
		// we cannot have a separate section for zeroinitialized data (would require 2 smaller arenas)
		// because it needs to occupy the same GiB as initialized data
		// to be RIP addressable in JIT mode
		context.staticDataBuffer.voidPut(context.zeroDataLength)[] = 0; // zero initialize
	}

	private bool canReferenceFromCode(void* hostSym)
	{
		void* start = context.codeBuffer.bufPtr;
		void* end = context.codeBuffer.bufPtr + context.codeBuffer.length;
		bool reachesFromStart = (hostSym - start) == cast(int)(hostSym - start);
		bool reachesFromEnd = (hostSym - end) == cast(int)(hostSym - end);
		return reachesFromStart && reachesFromEnd;
	}

	void addHostSymbols(HostSymbol[] hostSymbols)
	{
		if (hostSymbols.length > 0)
			context.assertf(context.buildType == BuildType.jit, "Can only add host symbols in JIT mode");

		ObjectModule hostModule = {
			kind : ObjectModuleKind.isHost,
			id : context.idMap.getOrRegNoDup(&context, ":host")
		};
		LinkIndex hostModuleIndex = context.objSymTab.addModule(hostModule);

		foreach (HostSymbol hostSym; hostSymbols)
		{
			Identifier symId = context.idMap.getOrRegNoDup(&context, hostSym.name);

			if (canReferenceFromCode(hostSym.ptr))
			{
				ObjectSymbol importedSymbol = {
					kind : ObjectSymbolKind.isHost,
					id : symId,
					dataPtr : cast(ubyte*)hostSym.ptr,
					sectionOffset : cast(ulong)hostSym.ptr,
					sectionIndex : context.builtinSections[ObjectSectionType.host],
					moduleIndex : hostModuleIndex,
				};
				LinkIndex importedSymbolIndex = context.objSymTab.addSymbol(importedSymbol);
				context.externalSymbols[symId] = importedSymbolIndex;
			}
			else
			{
				ulong sectionOffset = context.importBuffer.length;
				ulong ptr = cast(ulong)hostSym.ptr;
				context.importBuffer.put(*cast(ubyte[8]*)&ptr);

				ObjectSymbol importedSymbol = {
					kind : ObjectSymbolKind.isHost,
					flags : ObjectSymbolFlags.isIndirect,
					id : symId,
					sectionOffset : sectionOffset,
					sectionIndex : context.builtinSections[ObjectSectionType.imports],
					moduleIndex : hostModuleIndex,
				};
				LinkIndex importedSymbolIndex = context.objSymTab.addSymbol(importedSymbol);
				context.externalSymbols[symId] = importedSymbolIndex;
			}
		}
	}

	/// Returns index of imported module
	LinkIndex addDllModule(string libName)
	{
		ObjectModule importedModule = {
			kind : ObjectModuleKind.isImported,
			id : context.idMap.getOrRegNoDup(&context, libName)
		};
		return context.objSymTab.addModule(importedModule);
	}

	void addDllModuleSymbol(LinkIndex dllModuleIndex, string symName)
	{
		Identifier symId = context.idMap.getOrReg(&context, symName);
		ObjectSymbol importedSymbol = {
			kind : ObjectSymbolKind.isImported,
			flags : ObjectSymbolFlags.isIndirect,
			id : symId,
			alignmentPower : 3, // pointer size
			sectionIndex : context.builtinSections[ObjectSectionType.imports],
			moduleIndex : dllModuleIndex,
		};
		LinkIndex importedSymbolIndex = context.objSymTab.addSymbol(importedSymbol);
		context.externalSymbols[symId] = importedSymbolIndex;
	}

	void addDllModules(DllModule[] dllModules)
	{
		if (dllModules.length > 0)
			context.assertf(context.buildType == BuildType.exe, "Can only use dll symbols in exe mode");

		foreach (ref DllModule dllModule; dllModules)
		{
			LinkIndex importedModuleIndex = addDllModule(dllModule.libName);

			foreach (string symName; dllModule.importedSymbols)
			{
				addDllModuleSymbol(importedModuleIndex, symName);
			}
		}
	}
}
