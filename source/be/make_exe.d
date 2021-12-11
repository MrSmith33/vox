/**
Copyright: Copyright (c) 2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module be.make_exe;

import std.path;
import std.stdio;
import std.conv;

import all;
import be.pecoff;
import be.elf64;

//version = print_info;

void pass_create_executable(ref CompilationContext context, CompilePassPerModule[] subPasses)
{
	//writefln("%s", context.targetOs);
	final switch(context.targetOs) {
		case TargetOs.windows: make_pe_exe(&context); break;
		case TargetOs.linux: make_elf_exe(&context); break;
		case TargetOs.macos: context.internal_error("MacOS exe write is not implemented");
	}
}

void make_elf_exe(CompilationContext* context)
{
	enum SECTION_ALIGNMENT = 4096;
	Elf64Executable executable;
	executable.context = context;
	executable.fileHeader.file_type = ElfObjectFileType.ET_EXEC;
	executable.fileHeader.machine = ElfMachineType.x86_64;

	Elf64ProgramHeader[NUM_BUILTIN_SECTIONS] segmentBuf;
	Arena!Elf64ProgramHeader segmentArena;
	segmentArena.setBuffer(cast(ubyte[])segmentBuf);
	ObjectSection*[NUM_BUILTIN_SECTIONS] sectionBuf;
	Arena!(ObjectSection*) sectionArena;
	sectionArena.setBuffer(cast(ubyte[])sectionBuf);
	executable.assignSectionAddresses(segmentArena, sectionArena);

	// fix all references between symbols
	foreach (ref SourceFileInfo file; context.files.data) {
		linkModule(*context, file.mod.objectSymIndex);
	}

	if (context.printSymbols) context.objSymTab.dump(context);

	if (context.entryPoint is null) {
		context.unrecoverable_error(TokenIndex(), "No entry point set. Need 'main' function");
	}

	ObjectSymbol* entryPoint = context.objSymTab.getSymbol(context.entryPoint.backendData.objectSymIndex);
	ObjectSection* entryPointSection = context.objSymTab.getSection(entryPoint.sectionIndex);
	executable.fileHeader.entry = to!uint(entryPointSection.sectionAddress + entryPoint.sectionOffset);

	executable.write(context.binaryBuffer);
}

void make_pe_exe(CompilationContext* context)
{
	// Code section
	Section textSection = Section(SectionType.text, ".text");
	textSection.header.Characteristics =
		SectionFlags.SCN_CNT_CODE |
		SectionFlags.SCN_MEM_EXECUTE |
		SectionFlags.SCN_MEM_READ;

	// Import table section
	Section idataSection = Section(SectionType.idata, ".idata");
	idataSection.header.Characteristics =
		SectionFlags.SCN_CNT_INITIALIZED_DATA |
		SectionFlags.SCN_MEM_WRITE |
		SectionFlags.SCN_MEM_READ;

	// Static data section
	Section dataSection = Section(SectionType.data, ".data");
	dataSection.header.Characteristics =
		SectionFlags.SCN_MEM_WRITE |
		SectionFlags.SCN_MEM_READ;

	// Readonly static data section
	Section rdataSection = Section(SectionType.data, ".rdata");
	rdataSection.header.Characteristics =
		SectionFlags.SCN_CNT_INITIALIZED_DATA |
		SectionFlags.SCN_MEM_READ;

	// ---------------------------------------------------------

	ImportSection importSection;
	importSection.section = &idataSection;

	CoffImportSectionSize impSize = calcImportSize(context);
	ubyte[] importBuffer = context.importBuffer.voidPut(impSize.totalSectionBytes);
	auto importMapping = CoffImportSectionMapping(importBuffer, impSize);

	textSection.data = context.codeBuffer.data;
	idataSection.data = importMapping.sectionData;
	dataSection.data = context.staticDataBuffer.data;
	dataSection.setSectionZeroLength(context.zeroDataLength);
	if (dataSection.data.length > 0) dataSection.header.Characteristics |= SectionFlags.SCN_CNT_INITIALIZED_DATA;
	if (context.zeroDataLength > 0) dataSection.header.Characteristics |= SectionFlags.SCN_CNT_UNINITIALIZED_DATA;
	rdataSection.data = context.roStaticDataBuffer.data;

	// ---------------------------------------------------------

	// Exe gen
	Section*[4] sectionsBuf;
	Arena!(Section*) sections;
	sections.setBuffer(cast(ubyte[])sectionsBuf);

	void addSection(Section* section)
	{
		if (section.totalSize == 0) return;
		sections.put(section);
	}

	addSection(&textSection);
	addSection(&idataSection);
	addSection(&dataSection);
	addSection(&rdataSection);

	auto fileParams = FileParameters(DEFAULT_SECTION_ALIGNMENT, context.sectionAlignemnt);
	if (fileParams.fileAlignment < 512) fileParams.sectionAlignment = fileParams.fileAlignment;

	CoffExecutable executable = CoffExecutable(fileParams, context);
	executable.sections = sections.data;

	// fills header.VirtualAddress
	executable.windowsSubsystem = context.windowsSubsystem;
	executable.fixup();

	// fill sectionAddress, uses VirtualAddress
	context.objSymTab.getSection(context.builtinSections[ObjectSectionType.code]).sectionAddress = textSection.header.VirtualAddress;
	context.objSymTab.getSection(context.builtinSections[ObjectSectionType.imports]).sectionAddress = idataSection.header.VirtualAddress;
	context.objSymTab.getSection(context.builtinSections[ObjectSectionType.rw_data]).sectionAddress = dataSection.header.VirtualAddress;
	context.objSymTab.getSection(context.builtinSections[ObjectSectionType.ro_data]).sectionAddress = rdataSection.header.VirtualAddress;

	// uses sectionAddress
	if (importMapping.sectionData.length > 0) {
		fillImports(importMapping, context);
	}

	if (context.printSymbols) context.objSymTab.dump(context);

	// fix all references between symbols
	foreach (ref SourceFileInfo file; context.files.data) {
		linkModule(*context, file.mod.objectSymIndex);
	}

	if (context.entryPoint is null)
	{
		context.unrecoverable_error(TokenIndex(), "No entry point set. Need 'main' function");
	}

	ObjectSymbol* entryPoint = context.objSymTab.getSymbol(context.entryPoint.backendData.objectSymIndex);
	ObjectSection* entryPointSection = context.objSymTab.getSection(entryPoint.sectionIndex);
	executable.entryPointAddress = to!uint(entryPointSection.sectionAddress + entryPoint.sectionOffset);

	/*
	writeln("Code");
	printHex(textSection.data, 16);
	writeln("Imports");
	printHex(idataSection.data, 16);
	writeln("data");
	printHex(dataSection.data, 16);
	writeln;*/

	executable.write(context.binaryBuffer);
	//writeln(textSection.header);
	//writeln(idataSection.header);
	//writeln(dataSection.header);
	//printHex(sink.data, 16);
	//writefln("Writing to '%s'", context.outputFilename.absolutePath);
	//std.file.write(context.outputFilename, sink.data);

	//context.objSymTab.print_dd64_debug_info(&context);
}

// Walks all imported symbols and calculates import section size
CoffImportSectionSize calcImportSize(CompilationContext* context)
{
	CoffImportSectionSize impSize;
	LinkIndex modIndex = context.objSymTab.firstModule;
	while (modIndex.isDefined)
	{
		ObjectModule* mod = context.objSymTab.getModule(modIndex);

		if (mod.isImported)
		{
			++impSize.numLibs;
			string libName = context.idMap.get(mod.id);
			impSize.totalDllNamesBytes = alignValue(
				impSize.totalDllNamesBytes + libName.length + 1, 2);

			size_t numImportedFunctions;
			LinkIndex symIndex = mod.firstSymbol;
			while (symIndex.isDefined)
			{
				ObjectSymbol* sym = context.objSymTab.getSymbol(symIndex);

				// Only add referenced symbols to import table
				if (sym.isReferenced)
				{
					string symName = context.idString(sym.id);
					++numImportedFunctions;
					impSize.totalDllHintsBytes = alignValue(
						impSize.totalDllHintsBytes +
						HintNameEntry.Hint.sizeof +
						symName.length + 1, 2);
				}

				symIndex = sym.nextSymbol;
			}

			size_t numTableEntries = numImportedFunctions + 1; // include null entry
			impSize.totalImportEntries += numTableEntries;
		}
		modIndex = mod.nextModule;
	}
	return impSize;
}

struct CoffImportSectionSize
{
	size_t numLibs;
	size_t importDirectoryTableBytes() { return (numLibs + 1) * ImportDirectoryTableEntry.sizeof; }
	size_t totalImportEntries; // num of entries for both IAT and ILT, with null entries
	size_t totalTableBytes() { return totalImportEntries * IAT_ILT_ENTRY_BYTES; }
	size_t totalDllNamesBytes; // with leading zero and 2 byte alignment padding
	size_t totalDllHintsBytes; // with leading zero and 2 byte alignment padding
	size_t totalSectionBytes() {
		// without check importDirectoryTableBytes is adding 1 to numLibs, resulting in non-zero size for empty idata section
		if (numLibs == 0) return 0;
		return importDirectoryTableBytes + totalTableBytes * 2 + totalDllHintsBytes + totalDllNamesBytes;
	}
}

/// A set of slices on top of single memory buffer
struct CoffImportSectionMapping
{
	this(ubyte[] _sectionBuffer, CoffImportSectionSize _importSizes)
	{
		this.importSizes = _importSizes;
		this.sectionData = _sectionBuffer[0..importSizes.totalSectionBytes];
		assert(sectionData.length == importSizes.totalSectionBytes);

		if (sectionData.length == 0) return;

		size_t dirsEnd = importSizes.importDirectoryTableBytes;
		directories = cast(ImportDirectoryTableEntry[])(sectionData[0..dirsEnd]);

		ilt_rva = cast(uint)(dirsEnd);
		size_t ILT_end = cast(uint)(ilt_rva + importSizes.totalTableBytes);
		ILTs = cast(ImportLookupEntry[])(sectionData[ilt_rva..ILT_end]);

		iat_rva = cast(uint)(ILT_end);
		size_t IAT_end = cast(uint)(iat_rva + importSizes.totalTableBytes);
		IATs = cast(ImportLookupEntry[])(sectionData[iat_rva..IAT_end]);

		str_rva = cast(uint)IAT_end;
		stringData = sectionData[str_rva..$];
	}

	ubyte[] sectionData;

	// initial info
	CoffImportSectionSize importSizes;

	// dir entries
	ImportDirectoryTableEntry[] directories; // includes null entry
	// import lookup tables (ILTs)
	ImportLookupEntry[] ILTs; // includes null entry
	uint ilt_rva;
	// import address tables (IATs)
	ImportLookupEntry[] IATs; // includes null entry
	uint iat_rva;
	// list of (lib name + hint/names)
	ubyte[] stringData;
	uint str_rva;
}

void fillImports(ref CoffImportSectionMapping mapping, CompilationContext* context)
{
	ObjectSection* importSection = context.objSymTab.getSection(context.builtinSections[ObjectSectionType.imports]);
	uint sectionRVA = cast(uint)importSection.sectionAddress; // TODO check

	// here, sink already has reserved space for Directory entries and IA, IL tables
	// we will write strings and set address at the same time. Relative to section start
	size_t tableIndex;
	size_t moduleIndex;
	immutable uint str_rva = sectionRVA + mapping.str_rva;
	immutable uint ilt_rva = sectionRVA + mapping.ilt_rva;
	immutable uint iat_rva = sectionRVA + mapping.iat_rva;

	Arena!ubyte strSink;
	strSink.setBuffer(cast(ubyte[])mapping.stringData);

	LinkIndex modIndex = context.objSymTab.firstModule;
	while (modIndex.isDefined)
	{
		ObjectModule* mod = context.objSymTab.getModule(modIndex);

		if (mod.isImported)
		{
			mapping.directories[moduleIndex].importLookupTableRVA = cast(uint)(ilt_rva + tableIndex * IAT_ILT_ENTRY_BYTES);
			mapping.directories[moduleIndex].importAddressTableRVA = cast(uint)(iat_rva + tableIndex * IAT_ILT_ENTRY_BYTES);
			mapping.directories[moduleIndex].nameRVA = cast(uint)(str_rva + strSink.length);

			string libName = context.idMap.get(mod.id);
			auto pre1 = strSink.length;
			strSink.writeStringAligned2(libName);
			version(print_info) writefln("write '%s' len %s sink %s", libName, strSink.length - pre1, strSink.length);

			LinkIndex symIndex = mod.firstSymbol;
			while (symIndex.isDefined)
			{
				ObjectSymbol* sym = context.objSymTab.getSymbol(symIndex);

				// Only add referenced symbols to import table
				if (sym.isReferenced)
				{
					string symName = context.idString(sym.id);

					uint hintRVA = cast(uint)(str_rva + strSink.length);
					auto hint = ImportLookupEntry.fromHintNameTableRVA(hintRVA);

					mapping.ILTs[tableIndex] = hint;
					mapping.IATs[tableIndex] = hint;

					uint sectionOffset = cast(uint)(mapping.iat_rva + tableIndex * IAT_ILT_ENTRY_BYTES);
					sym.sectionOffset = sectionOffset;
					sym.length = IAT_ILT_ENTRY_BYTES;

					auto pre2 = strSink.length;
					HintNameEntry(0, symName).write(strSink);
					version(print_info) writefln("write '%s' len %s RVA %x", symName, strSink.length - pre2, sectionOffset);

					++tableIndex;
				}

				symIndex = sym.nextSymbol;
			}

			// account for null entry
			++tableIndex;
			++moduleIndex;
		}
		modIndex = mod.nextModule;
	}

	assert(strSink.length == mapping.stringData.length);
}

struct CoffExecutable
{
	FileParameters params;
	CompilationContext* context;
	uint entryPointAddress;
	WindowsSubsystem windowsSubsystem;

	DosHeader dosHeader;
	DosStub dosStub;
	PeSignature peSignature;
	CoffFileHeader coffFileHeader;
	OptionalHeader optionalHeader;
	Section*[] sections;

	uint unalignedHeadersSize;

	void fixup()
	{
		calculateFileSizes();
		fixupInMemorySizes();
		collectCodeInfo();
		fixupInvariants();
	}

	private void calculateFileSizes()
	{
		uint fileSize = 0;
		fileSize += DosHeader.sizeof;
		fileSize += DosStub.sizeof;
		fileSize += PeSignature.sizeof;
		fileSize += CoffFileHeader.sizeof;
		fileSize += OptionalHeader.sizeof;
		fileSize += SectionHeader.sizeof * sections.length;

		unalignedHeadersSize = fileSize;
		optionalHeader.SizeOfHeaders = alignValue(unalignedHeadersSize, params.fileAlignment);
		uint imageFileSize = optionalHeader.SizeOfHeaders;

		foreach (Section* section; sections)
		{
			// size in a file
			uint sectionFileSize = alignValue(section.initializedSize, params.fileAlignment);
			section.header.SizeOfRawData = sectionFileSize;

			// position in a file
			if (section.header.SizeOfRawData == 0)
				// "When a section contains only uninitialized data, this field should be zero."
				section.header.PointerToRawData = 0;
			else
				section.header.PointerToRawData = imageFileSize;

			imageFileSize += sectionFileSize;
		}
		version(print_info) writefln("Image file size is %s bytes", imageFileSize);
	}

	private void fixupInMemorySizes()
	{
		uint headersInMemorySize = alignValue(unalignedHeadersSize, params.sectionAlignment);
		uint imageVirtualSize = headersInMemorySize;

		foreach (Section* section; sections)
		{
			// position in memory after load
			section.header.VirtualAddress = imageVirtualSize;
			uint sectionVirtualSize = alignValue(section.totalSize, params.sectionAlignment);
			// size in memory after load, need not be aligned
			section.header.VirtualSize = section.totalSize;
			imageVirtualSize += sectionVirtualSize;
		}

		optionalHeader.SizeOfImage = imageVirtualSize;
	}

	private void collectCodeInfo()
	{
		bool codeSectionDetected = false;
		bool importSectionDetected = false;

		foreach (Section* section; sections)
		{
			if (section.isCodeSection)
			{
				if (!codeSectionDetected)
				{
					codeSectionDetected = true;
					optionalHeader.BaseOfCode = section.header.VirtualAddress;
					version(print_info) writefln("First code section. BaseOfCode is %s", optionalHeader.BaseOfCode);
				}
				optionalHeader.SizeOfCode += section.header.SizeOfRawData;
				version(print_info) writefln("Code section %s", section.header);
			}
			else if (section.isImportSection)
			{
				if (!importSectionDetected)
				{
					importSectionDetected = true;
					optionalHeader.ImportTable.VirtualAddress = section.header.VirtualAddress;
				}
			}

			optionalHeader.SizeOfInitializedData += section.header.SizeOfRawData;
		}
		version(print_info) writefln("Total code size is %sB", optionalHeader.SizeOfCode);
	}

	private void fixupInvariants()
	{
		// COFF Header
		coffFileHeader.Machine = MachineType.amd64;
		coffFileHeader.NumberOfSections = cast(ushort)sections.length;
		coffFileHeader.TimeDateStamp = 0;
		coffFileHeader.PointerToSymbolTable = 0;
		coffFileHeader.NumberOfSymbols = 0;
		coffFileHeader.Characteristics =
			CoffFlags.RELOCS_STRIPPED | // TODO. remove when relocations are implemented
			CoffFlags.EXECUTABLE_IMAGE |
			CoffFlags.LARGE_ADDRESS_AWARE;


		// Optional Header (Image Only)
		optionalHeader.MajorLinkerVersion = 1;
		optionalHeader.SizeOfUninitializedData = 0; // FIXUP
		optionalHeader.SectionAlignment = params.sectionAlignment;
		optionalHeader.FileAlignment = params.fileAlignment;
		optionalHeader.MajorOperatingSystemVersion = 6;
		optionalHeader.MinorOperatingSystemVersion = 0;
		optionalHeader.MajorImageVersion = 0;
		optionalHeader.MinorImageVersion = 0;
		optionalHeader.MajorSubsystemVersion = 6;
		optionalHeader.MinorSubsystemVersion = 0;
		optionalHeader.CheckSum = 0;
		optionalHeader.Subsystem = windowsSubsystem;
		optionalHeader.DllCharacteristics = 0;
		optionalHeader.SizeOfStackReserve = 0x100000;
		optionalHeader.SizeOfStackCommit = 0x1000;
		optionalHeader.SizeOfHeapReserve = 0x100000;
		optionalHeader.SizeOfHeapCommit = 0x1000;

		// Section headers
		foreach (section; sections)
		{
			section.header.PointerToRelocations = 0;
			section.header.NumberOfRelocations = 0;
		}
	}

	void write(ref Arena!ubyte sink)
	{
		optionalHeader.AddressOfEntryPoint = entryPointAddress;

		// DOS Header
		dosHeader.write(sink);
		// DOS Stub
		dosStub.write(sink);
		// PE signature
		peSignature.write(sink);
		// COFF Header
		coffFileHeader.write(sink);
		// Optional Header (Image Only)
		optionalHeader.write(sink);
		// Section Headers
		foreach (section; sections)
		{
			section.header.Name[0..section.name.length] = section.name;
			section.header.Name[section.name.length..$] = '\0';
			section.header.write(sink);
		}
		uint headersPadding = paddingSize(unalignedHeadersSize, params.fileAlignment);
		sink.pad(headersPadding);

		// Section Datas
		foreach (section; sections)
		{
			version(print_info) writefln("%s RVA %x\t%s len\t%s bytes", section.name, sink.length, section.header.VirtualAddress, section.data.length);
			sink.put(section.data);
			size_t sectionPadding = section.header.SizeOfRawData - section.data.length;
			sink.pad(sectionPadding);
		}
	}
}

struct Elf64Executable
{
	CompilationContext* context;
	Elf64Header fileHeader;
	// 0 address will not work without relocations
	ulong baseAddress = 0x400000;

	Elf64ProgramHeader[] segments;
	ObjectSection*[] sections;

	void assignSectionAddresses(ref Arena!Elf64ProgramHeader segmentBuf, ref Arena!(ObjectSection*) sectionBuf)
	{
		// gather non-empty sections and calc offsets
		uint fileOffset = Elf64Header.sizeof;

		foreach (LinkIndex sectionIndex; context.builtinSections)
		{
			ObjectSection* section = context.objSymTab.getSection(sectionIndex);
			if (section.totalLength == 0) continue; // skip empty sections

			Elf64ProgramHeader segment;
			segment.type = Elf64SegmentType.PT_LOAD;
			if (section.flag_read) segment.flags |= Elf64SegmentAttributes.READ;
			if (section.flag_write) segment.flags |= Elf64SegmentAttributes.WRITE;
			if (section.flag_execute) segment.flags |= Elf64SegmentAttributes.EXECUTE;

			fileOffset += Elf64ProgramHeader.sizeof;

			segmentBuf.put(segment);
			sectionBuf.put(section);

		}
		segments = segmentBuf.data;
		sections = sectionBuf.data;

		fileHeader.phnum = to!ushort(segments.length);

		ulong memoryOffset = baseAddress;

		foreach(i, ObjectSection* section; sections) {
			Elf64ProgramHeader* segment = &segments[i];
			segment.alignment = section.alignment;

			// Memory offset
			// A.1. align the data in memory
			memoryOffset = alignValue(memoryOffset, section.alignment); // add padding
			segment.vaddr = memoryOffset;
			section.sectionAddress = memoryOffset;
			// Memory size
			// A.2. then add the data (including zero-initialized data)
			segment.memsz = section.totalLength;
			memoryOffset += section.totalLength;

			if (section.initDataLength > 0)
			{
				// File offset
				// We don't want to add padding to the file if there will be no data
				// B.1. align the data in the file
				fileOffset = alignValue(fileOffset, section.alignment); // add padding
				segment.offset = fileOffset;
				// File size
				// B.2. then set the in-memory size (only initialized data)
				segment.filesz = section.initDataLength;
				fileOffset += section.initDataLength;
			}
		}
	}

	void write(ref Arena!ubyte sink)
	{
		// ELF64 header
		sink.put(fileHeader);

		// Segment table
		foreach (ref segment; segments) {
			sink.put(segment);
		}

		// Segments
		foreach (i, ref segment; segments) {
			ObjectSection* section = sections[i];
			sink.padUntilAligned(segment.alignment);
			sink.put(section.buffer.data);
		}
	}
}
