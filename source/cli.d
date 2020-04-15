/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module cli;

import std.stdio;
import std.file : exists;
import std.path : absolutePath, extension, baseName, setExtension;
import all;

enum WindowsSubsystemCli : ushort {
	CUI,
	GUI
}

int runCli(string[] args)
{
	import std.getopt;

	auto time1 = currTime;
	auto startInitTime = currTime;

	Driver driver;
	WindowsSubsystemCli subSystem;

	bool printTime;
	bool printMem;
	string[] files;
	string outputFilename;
	string filterFuncName;

	bool printHelp;
	GetoptResult optResult;

	try
	{
		// GC
		optResult = getopt(
			args,
			"of", "Write output to file.", &outputFilename,
			"print-time", "Print time of compilation.", &printTime,
			"print-source", "Print source code.", &driver.context.printSource,
			"print-lexemes", "Print lexemes.", &driver.context.printLexemes,
			"print-ast-fresh", "Print AST after parsing.", &driver.context.printAstFresh,
			"print-ast-sema", "Print AST after semantic analisys.", &driver.context.printAstSema,
			"print-ir", "Print IR after AST to IR pass.", &driver.context.printIr,
			"print-ir-opt", "Print IR after optimization.", &driver.context.printIrOpt,
			"print-ir-lower", "Print IR after all lowering passes.", &driver.context.printIrLower,
			"print-ir-lower-each", "Print IR after each lowering pass.", &driver.context.printIrLowerEach,
			"print-lir", "Print Print LIR after IR to LIR pass.", &driver.context.printLir,
			"print-lir-ra", "Print LIR after register allocation pass.", &driver.context.printLirRA,
			"print-liveness", "Print liveness analisys info.", &driver.context.printLiveIntervals,
			"print-stack-layout", "Print stack layout.", &driver.context.printStackLayout,
			"print-code-hex", "Print code hex.", &driver.context.printCodeHex,
			"print-symbols", "Print symbols.", &driver.context.printSymbols,
			"print-mem", "Print memory consumtion.", &printMem,
			"print-filter", "Print only info about <function name>.", &filterFuncName,
			"print-error-trace", "Print stack trace for every error", &driver.context.printTraceOnError,
			"subsystem", "Select windows subsystem. [CUI(default), GUI].", &subSystem,
			);

		final switch (subSystem) {
			case WindowsSubsystemCli.CUI: driver.context.windowsSubsystem = WindowsSubsystem.WINDOWS_CUI; break;
			case WindowsSubsystemCli.GUI: driver.context.windowsSubsystem = WindowsSubsystem.WINDOWS_GUI; break;
		}

		args = args[1..$]; // skip program name

		if (args.length < 1) printHelp = true;
		if (optResult.helpWanted) printHelp = true;
	}
	catch(GetOptException e)
	{
		writeln(e.msg);
		printHelp = true;
	}

	if (printHelp)
	{
		writeln("Usage: tiny_jit [options]... [source|.dll|.har]...");

		size_t maxShortLength;
		size_t maxLongLength;
		foreach (it; optResult.options)
		{
			maxShortLength = max(maxShortLength, it.optShort.length);
			maxLongLength = max(maxLongLength, it.optLong.length);
		}

		foreach (it; optResult.options)
		{
			writefln("%-*s %-*s  %s", maxShortLength, it.optShort, maxLongLength, it.optLong, it.help);
		}

		return 0;
	}

	string[] filenames = args;

	driver.initialize(exePasses);
	driver.context.buildType = BuildType.exe;

	if (outputFilename) driver.context.outputFilename = outputFilename;
	else driver.context.outputFilename = filenames[0].baseName.setExtension(".exe"); // GC

	if (filterFuncName) driver.context.printOnlyFun = driver.context.idMap.getOrRegNoDup(&driver.context, filterFuncName);

	auto times = PerPassTimeMeasurements(1, driver.passes);
	auto endInitTime = currTime;

	try
	{
		driver.beginCompilation();

		foreach(filename; filenames)
		{
			string ext = filename.extension;
			switch(ext)
			{
				case ".dll":
					string libName = filename.baseName;
					LinkIndex importedModule = driver.addDllModule(libName);

					void onDllSymbol(uint ordinal, string symName) {
						driver.addDllModuleSymbol(importedModule, symName);
					}

					size_t savedLength = driver.context.sourceBuffer.length;
					scope(exit) driver.context.sourceBuffer.length = savedLength;

					if (!exists(filename))
					{
						driver.context.error("File `%s` not found", absolutePath(filename));
						break;
					}

					auto file = File(filename, "r");
					char[] sourceBuffer = driver.context.sourceBuffer.voidPut(file.size);
					char[] dllData = file.rawRead(sourceBuffer);
					file.close();

					getExportNames(driver.context, filename, cast(ubyte[])dllData, &onDllSymbol);
					break;
				case ".har":
					if (!exists(filename))
					{
						driver.context.error("File `%s` not found", absolutePath(filename));
						break;
					}

					auto file = File(filename, "r");
					char[] sourceBuffer = driver.context.sourceBuffer.voidPut(file.size);
					char[] harData = file.rawRead(sourceBuffer);
					file.close();

					driver.addHar(filename, harData);
					break;
				default:
					driver.addModule(SourceFileInfo(filename));
					break;
			}
		}

		driver.compile();
	}
	catch(CompilationException e) {
		writeln(driver.context.sink.text);
		if (e.isICE)
			writeln(e);
		return 1;
	}
	catch(Throwable t) {
		writeln(driver.context.sink.text);
		writeln(t);
		return 1;
	}

	TextSink sink;
	if (printMem) driver.context.printMemSize(sink);

	auto startReleaseTime = currTime;
		// releasing memory is not necessary when running in standalone mode
		driver.releaseMemory;
	auto endReleaseTime = currTime;

	auto time2 = currTime;
	Duration duration = time2-time1;

	times.onIteration(0, duration);

	if (printMem) write(cast(string)sink.data.data);

	if (printTime)
	{
		times.print;

		writefln("Finished in %ss, init %ss, release %ss",
			scaledNumberFmt(duration),
			scaledNumberFmt(endInitTime-startInitTime),
			scaledNumberFmt(endReleaseTime-startReleaseTime));
	}

	return 0;
}

void getExportNames(ref CompilationContext context, string filename, ubyte[] dllData, void delegate(uint, string) onExport)
{
	import be.pecoff;
	import std.string : fromStringz;

	if (dllData.length < DosHeader.sizeof) {
		context.error("`%s` is invalid .dll file. Size is %s bytes.", filename, dllData.length);
		return;
	}

	auto slicer = FileDataSlicer(dllData);
	DosHeader* dosHeader = slicer.getPtrTo!DosHeader;
	slicer.fileCursor = dosHeader.e_lfanew;
	PeSignature peSignature = *slicer.getPtrTo!PeSignature;

	if (peSignature != PeSignature.init) {
		context.error("`%s` is not a PE file", filename);
		return;
	}

	CoffFileHeader* header = slicer.getPtrTo!CoffFileHeader;
	if (header.Machine != MachineType.amd64) {
		context.error("`%s` machine type is %s", filename, header.Machine);
		return;
	}

	size_t sectionPtr = slicer.fileCursor + header.SizeOfOptionalHeader;

	OptionalHeader* opt = slicer.getPtrTo!OptionalHeader;
	if (!opt.isValidMember!"ExportTable"(header.SizeOfOptionalHeader)) return; // no export table in optional header
	if (opt.NumberOfRvaAndSizes < 1) return; // no export table in optional header

	uint exportsRVA = opt.ExportTable.VirtualAddress;

	slicer.fileCursor = sectionPtr;
	SectionHeader[] sectionHeaders = slicer.getArrayOf!SectionHeader(header.NumberOfSections);

	char[8] edataName = ".edata\0\0";
	foreach(ref section; sectionHeaders)
	{
		// export table is inside this section
		if (exportsRVA >= section.VirtualAddress && exportsRVA < section.VirtualAddress + section.SizeOfRawData)
		{
			ptrdiff_t offset = section.VirtualAddress - section.PointerToRawData;
			slicer.fileCursor = exportsRVA - offset;
			auto exportDir = slicer.getPtrTo!ExportDirectoryEntry;

			size_t nameTblPtr = exportDir.AddressOfNames - offset;
			slicer.fileCursor = nameTblPtr;
			uint[] namePointers = slicer.getArrayOf!uint(exportDir.NumberOfNames);

			foreach(size_t ordinal, uint namePtr; namePointers)
			{
				slicer.fileCursor = namePtr - offset;
				char* namez = slicer.getPtrTo!char;
				string name = cast(string)fromStringz(namez);
				onExport(cast(uint)ordinal, name);
			}
		}
	}
}
