/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.driver;

import std.stdio : writeln, write, writef, writefln, stdout;
import std.path : baseName, stripExtension;

import vox.all;

/// To compile a set of modules do following steps:
/// 1. initialize(passes)
/// 2. beginCompilation()
/// 3. addHostSymbols(hostSymbols)  --+
/// foreach(module; modules)          | In any order
/// 4. addModule(module)  ------------+
/// 5. compile()
/// 6. markCodeAsExecutable() if in JIT mode
/// 7. releaseMemory()
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
		size_t BYTES_TO_RESERVE = GiB*186;
		arenaPool.reserve(BYTES_TO_RESERVE);
		//writefln("arenaPool %X .. %X", arenaPool.buffer.ptr, arenaPool.buffer.ptr+arenaPool.buffer.length);

		/// Those 3 must be allocated in this order (or in inverse order)
		/// Code must be able to use RIP-relative addressing into static data (and into import data when in JIT mode)
		context.importBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.codeBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.staticDataBuffer.setBuffer(arenaPool.take(GiB/2), 0);
		context.roStaticDataBuffer.setBuffer(arenaPool.take(GiB/2), 0);

		context.sourceBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.files.setBuffer(arenaPool.take(GiB), 0);
		context.tokenBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.tokenLocationBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.binaryBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.bundleBuffer.setBuffer(arenaPool.take(GiB), 0);

		context.irStorage.instrHeaderBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.instrPayloadBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.instrNextBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.instrPrevBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.vregBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.phiBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.basicBlockBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.arrayBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.stackSlotBuffer.setBuffer(arenaPool.take(8*GiB), 0);

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

		context.idMap.entries.setBuffer(arenaPool.take(2*GiB), 0);
		context.idMap.stringDataBuffer.setBuffer(arenaPool.take(2*GiB), 0);

		// all arena sizes must sum up to predefined constant
		context.assertf(arenaPool.takenBytes == BYTES_TO_RESERVE,
			"%s bytes taken, %s bytes to take", arenaPool.takenBytes, BYTES_TO_RESERVE);

		context.initialize();
	}

	void releaseMemory() nothrow
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

		Identifier id = context.idMap.getOrReg(&context, file.name.baseName.stripExtension);
		ObjectModule localModule = {
			kind : ObjectModuleKind.isLocal,
			id : id
		};
		auto mod = context.appendAst!ModuleDeclNode();
		file.mod = context.getAst!ModuleDeclNode(mod);
		file.mod.moduleIndex = ModuleIndex(fileIndex);
		file.mod.fqn = id;
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

	void addHostSymbols(HostSymbol[] hostSymbols)
	{
		if (hostSymbols.length > 0)
			context.assertf(context.buildType == BuildType.jit, "Can only add host symbols in JIT mode");

		LinkIndex hostModuleIndex = context.getOrCreateExternalModule(CommonIds.id_host, ObjectModuleKind.isHost);

		foreach (HostSymbol hostSym; hostSymbols)
		{
			Identifier symId = context.idMap.getOrReg(&context, hostSym.symName);
			context.addHostSymbol(hostModuleIndex, symId, hostSym.ptr);
		}
	}
}
