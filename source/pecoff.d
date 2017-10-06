/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module pecoff;

import std.stdio;
import std.file;
import std.datetime;

import amd64asm;
import utils;

//version = print_info;

enum DEFAULT_SECTION_ALIGNMENT = 4096;
enum DEFAULT_FILE_ALIGNMENT = 512;

alias PC = ubyte*;
struct ArraySink {
	private ubyte[] mem;
	private PC pc;
	size_t length() { return pc - mem.ptr; }

	void setBuffer(ubyte[] buf) { mem = buf; pc = mem.ptr; }
	void resetPC() { pc = mem.ptr; }
	ubyte[] data() { return mem[0..pc - mem.ptr]; }

	void put(T)(T value) {
		*(cast(T*)pc) = value;
		pc += value.sizeof;
	}
	void put(ubyte[] value) {
		pc[0..value.length] = value;
		pc += value.length;
	}
	void pad(size_t bytes) {
		pc[0..bytes] = 0;
		pc += bytes;
	}
}

enum : uint {
    STD_INPUT_HANDLE  = 0xFFFFFFF6,
    STD_OUTPUT_HANDLE = 0xFFFFFFF5,
    STD_ERROR_HANDLE  = 0xFFFFFFF4
}

enum SectionType : ubyte
{
	text,
	idata,
	data,
}

void main()
{
	MemAllocator allocator;
	auto time0 = currTime;

	ubyte[] buf = allocator.allocate(0x1000 * 20, MemType.RW);
	scope(exit) allocator.free(buf);
	ubyte[] binaryMem = buf[0..2*0x1000];// = allocator.allocate(4096 * 8, MemType.RW);
	ubyte[] importMem = buf[2*0x1000..4*0x1000];
	ubyte[] dataMem = buf[4*0x1000..6*0x1000];

	ubyte[] codeMem = buf[6*0x1000..$];

	// ---------------------------------------------------------

	// Code section
	Section textSection = Section(SectionType.text);
	textSection.header.Name = ".text";
	textSection.header.Characteristics =
		SectionFlags.SCN_CNT_CODE |
		SectionFlags.SCN_MEM_EXECUTE |
		SectionFlags.SCN_MEM_READ;

	// Import table section
	Section idataSection = Section(SectionType.idata);
	idataSection.header.Name = ".idata";
	idataSection.header.Characteristics =
		SectionFlags.SCN_CNT_INITIALIZED_DATA |
		SectionFlags.SCN_MEM_WRITE |
		SectionFlags.SCN_MEM_READ;

	// Static data section
	Section dataSection = Section(SectionType.data);
	dataSection.header.Name = ".data";
	dataSection.header.Characteristics =
		SectionFlags.SCN_CNT_INITIALIZED_DATA |
		SectionFlags.SCN_MEM_READ;

	// ---------------------------------------------------------

	// Exe gen
	Section*[3] sections;
	sections[SectionType.text] = &textSection;
	sections[SectionType.idata] = &idataSection;
	sections[SectionType.data] = &dataSection;

	// ---------------------------------------------------------

	ArraySink sink;
	sink.setBuffer(binaryMem);

	DataSection dataSectionSymbols;
	dataSectionSymbols.sink.setBuffer(dataMem);
	dataSectionSymbols.section = &dataSection;

	ImportSection importSection;
	importSection.section = &idataSection;


	// ---------------------------------------------------------

	ReferenceTable refTable;

	// Code gen
	CodeGen_x86_64 codeGen;
	codeGen.encoder.setBuffer(codeMem);

	void putFixup(SymbolRef symRef)
	{
		auto fixup = codeGen.getAddressFixup;
		refTable.put(RelativeReference(SectionType.text, fixup.fixupOffset, fixup.extraOffset, symRef));
	}

	// main
	SymbolRef msg_str = dataSectionSymbols.putString("Hello world!");
	SymbolRef ref_GetStdHandle = importSection.importLibFunction("kernel32", "GetStdHandle");
	SymbolRef ref_WriteConsoleA = importSection.importLibFunction("kernel32", "WriteConsoleA");

	codeGen.subq(Register.SP, Imm32(0x30));
	codeGen.movq(Register.CX, Imm32(STD_OUTPUT_HANDLE));
	codeGen.call(memAddrRipDisp32(0)); // call GetStdHandle
	putFixup(ref_GetStdHandle);

	codeGen.movq(Register.CX, Register.AX); // 1 in hConsoleOutput
	codeGen.leaq(Register.DX, memAddrRipDisp32(0)); // 2 in lpBuffer
	putFixup(msg_str);

	codeGen.movq(Register.R8, Imm32(dataSection.symBytes(msg_str.symbolIndex))); // 3 in nNumberOfCharsToWrite
	codeGen.leaq(Register.R9, memAddrBaseDisp8(Register.SP, 0x28)); // 4 out lpNumberOfCharsWritten
	codeGen.movq(memAddrBaseDisp8(Register.SP, 0x20), Imm32(0)); // 5 in lpReserved

	codeGen.call(memAddrRipDisp32(0)); // call WriteConsoleA
	putFixup(ref_WriteConsoleA);

	codeGen.addq(Register.SP, Imm32(0x30));
	codeGen.xorq(Register.AX, Register.AX);
	codeGen.ret();

	// ---------------------------------------------------------

	auto importMapping = ImportSectionMapping(importMem, importSection.getLibs);

	textSection.data = codeGen.encoder.code;
	idataSection.data = importMapping.sectionData;
	dataSection.data = dataSectionSymbols.sink.data;

	// ---------------------------------------------------------

	Executable executable;
	executable.params = FileParameters();
	executable.sections = sections[];
	executable.fixup();
	importMapping.createImports(sections[SectionType.idata]);
	refTable.fixupReferences(sections[]);
	executable.write(sink);

	// write exe
	auto time1 = currTime;
	string outputFilename = "out.exe";

	auto f = File(outputFilename, "wb");
	f.rawWrite(sink.data);
	f.close();

	// test exe
	import std.process;
	import std.path;
	auto time2 = currTime;
	auto result = execute("out.exe");
	auto time3 = currTime;

	writefln("out.exe exited with %s, in %ss", result.status, scaledNumberFmt(time3 - time2));
	writefln("Compile in %ss, write %ss", scaledNumberFmt(time1 - time0), scaledNumberFmt(time2 - time1));
	writeln(absolutePath(outputFilename));
	//assert(result.status == 0);
}

struct RelativeReference
{
	SectionType fromSection;
	uint fromOffset;
	uint extraOffset;
	SymbolRef referencedSymbol;
}

struct ReferenceTable
{
	RelativeReference[] relativeRefs;

	void put(RelativeReference reference)
	{
		relativeRefs ~= reference;
	}

	void fixupReferences(Section*[] sections)
	{
		foreach(reference; relativeRefs)
		{
			auto from = sections[reference.fromSection];
			auto to = sections[reference.referencedSymbol.sectionId];

			uint* fixup = cast(uint*)(from.data.ptr + reference.fromOffset);
			// 1000 + 0010 + 0004 = 1014
			uint fromOffset = from.header.VirtualAddress + reference.fromOffset + reference.extraOffset;
			// 2000 + 0040 = 2040
			uint symOffset = to.symOffset(reference.referencedSymbol.symbolIndex);
			uint toOffset = to.header.VirtualAddress + symOffset;
			uint value = toOffset - fromOffset;

			version(print_info) writefln("VA %x sym off %x", to.header.VirtualAddress, symOffset);
			version(print_info) writefln("Fix %s:%x -> %s:%x with %x", from.name, reference.fromOffset, to.name, symOffset, value);
			version(print_info) writefln("    %x -> %x", fromOffset, toOffset);

			*fixup = value;
		}
	}
}

struct DataSection
{
	ArraySink sink;
	Section* section;

	SymbolRef putString(string str)
	{
		SymbolIndex index = section.addSymbol(cast(uint)sink.length, cast(uint)str.length);
		sink.put(cast(ubyte[])str);
		sink.put!ubyte(0);
		return SymbolRef(index, SectionType.data);
	}
}

struct SymbolImport
{
	string name;
	SymbolIndex symbolIndex;
}

struct ImportSection
{
	SymbolImport[][string] importedLibs;
	Section* section;

	SymbolRef importLibFunction(string library, string functionName)
	{
		SymbolIndex funcIndex = section.addSymbol(0, 0);
		SymbolImport[] functions = importedLibs.get(library, null);
		functions ~= SymbolImport(functionName, funcIndex);
		importedLibs[library] = functions;
		return SymbolRef(funcIndex, SectionType.idata);
	}

	ImportedDlls getLibs() {
		DllImports[] libs;
		libs.length = importedLibs.length;
		size_t i;
		foreach(libName, functions; importedLibs)
			libs[i++] = DllImports(libName, functions);
		return ImportedDlls(libs);
	}
}

// register function in library
// insert external in code

enum IAT_ILT_ENTRY_BYTES = 8;

struct ImportedDlls
{
	this(DllImports[] dlls)
	{
		this.libs = dlls;
		foreach(dll; libs)
		{
			totalDllNamesBytes = alignValue(totalDllNamesBytes + dll.libName.length + 1, 2);
			foreach(func; dll.importedFunctions)
			{
				// Hint/Name length
				totalDllHintsBytes = alignValue(totalDllHintsBytes + HintNameEntry.Hint.sizeof + func.name.length + 1, 2);
			}
			totalImportEntries += dll.numTableEntries; // include null entry
		}
	}

	DllImports[] libs;
	size_t importDirectoryTableBytes() { return (libs.length + 1) * ImportDirectoryTableEntry.sizeof; }
	size_t totalImportEntries; // num of entries for both IAT and ILT, with null entries
	size_t totalTableBytes() { return totalImportEntries * IAT_ILT_ENTRY_BYTES; }
	size_t totalDllNamesBytes; // with leading zero and 2 byte alignment padding
	size_t totalDllHintsBytes; // with leading zero and 2 byte alignment padding
	size_t totalSectionBytes() { return importDirectoryTableBytes + totalTableBytes * 2 + totalDllHintsBytes + totalDllNamesBytes; }
}

struct DllImports
{
	this(string lib, SymbolImport[] funcs) {
		libName = lib;
		importedFunctions = funcs;
	}
	string libName;
	SymbolImport[] importedFunctions;
	size_t numTableEntries() { return importedFunctions.length + 1; }
	size_t totalTableBytes() { return numTableEntries * IAT_ILT_ENTRY_BYTES; }
}

struct ImportSectionMapping
{
	this(ubyte[] _sectionBuffer, ImportedDlls _importedLibs)
	{
		this.importedLibs = _importedLibs;
		this.sectionData = _sectionBuffer[0..importedLibs.totalSectionBytes];
		assert(sectionData.length == importedLibs.totalSectionBytes);

		size_t dirsEnd = importedLibs.importDirectoryTableBytes;
		directories = cast(ImportDirectoryTableEntry[])(sectionData[0..dirsEnd]);

		ilt_rva = cast(uint)(dirsEnd);
		size_t ILT_end = cast(uint)(ilt_rva + importedLibs.totalTableBytes);
		ILTs = cast(ImportLookupEntry[])(sectionData[ilt_rva..ILT_end]);

		iat_rva = cast(uint)(ILT_end);
		size_t IAT_end = cast(uint)(iat_rva + importedLibs.totalTableBytes);
		IATs = cast(ImportLookupEntry[])(sectionData[iat_rva..IAT_end]);

		str_rva = cast(uint)IAT_end;
		stringData = sectionData[str_rva..$];
	}

	ubyte[] sectionData;

	// initial info
	ImportedDlls importedLibs;

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

void createImports(ref ImportSectionMapping mapping, Section* importSection)
{
	uint sectionRVA = importSection.header.VirtualAddress;
	// here, sink already has reserved space for Directory entries and IA, IL tables
	// we will write strings and set address at the same time. Relative to section start
	size_t tableIndex;
	immutable uint str_rva = sectionRVA + mapping.str_rva;
	immutable uint ilt_rva = sectionRVA + mapping.ilt_rva;
	immutable uint iat_rva = sectionRVA + mapping.iat_rva;

	ArraySink strSink;
	strSink.setBuffer(mapping.stringData);

	foreach(i, ref DllImports dll; mapping.importedLibs.libs)
	{
		mapping.directories[i].importLookupTableRVA = cast(uint)(ilt_rva + tableIndex * IAT_ILT_ENTRY_BYTES);
		mapping.directories[i].importAddressTableRVA = cast(uint)(iat_rva + tableIndex * IAT_ILT_ENTRY_BYTES);
		mapping.directories[i].nameRVA = cast(uint)(str_rva + strSink.length);

		auto pre1 = strSink.length;
		strSink.writeStringAligned2(dll.libName);
		version(print_info) writefln("write '%s' len %s sink %s", dll.libName, strSink.length - pre1, strSink.length);

		foreach(func; dll.importedFunctions)
		{
			uint hintRVA = cast(uint)(str_rva + strSink.length);
			auto hint = ImportLookupEntry.fromHintNameTableRVA(hintRVA);

			mapping.ILTs[tableIndex] = hint;
			mapping.IATs[tableIndex] = hint;

			uint sym_rva = cast(uint)(mapping.iat_rva + tableIndex * IAT_ILT_ENTRY_BYTES);
			importSection.symbols[func.symbolIndex] = SymbolSectionInfo(sym_rva, IAT_ILT_ENTRY_BYTES);

			auto pre2 = strSink.length;
			HintNameEntry(0, func.name).write(strSink);
			version(print_info) writefln("write '%s' len %s RVA %x", func.name, strSink.length - pre2, sym_rva);

			++tableIndex;
		}

		// account for null entry
		++tableIndex;
	}

	assert(strSink.length == mapping.stringData.length);
}

struct FileParameters
{
	uint sectionAlignment = DEFAULT_SECTION_ALIGNMENT;
	uint fileAlignment = DEFAULT_FILE_ALIGNMENT;
}

struct Executable
{
	FileParameters params;

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

	void calculateFileSizes()
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

		foreach (section; sections)
		{
			section.header.PointerToRawData = imageFileSize;
			uint sectionFileSize = alignValue(section.dataSize, params.fileAlignment);
			section.header.SizeOfRawData = sectionFileSize;
			imageFileSize += sectionFileSize;
		}
		version(print_info) writefln("Image file size is %s bytes", imageFileSize);
	}

	void fixupInMemorySizes()
	{
		uint headersInMemorySize = alignValue(unalignedHeadersSize, params.sectionAlignment);
		uint imageVirtualSize = headersInMemorySize;

		foreach (section; sections)
		{
			section.header.VirtualAddress = imageVirtualSize;
			uint sectionVirtualSize = alignValue(section.dataSize, params.sectionAlignment);
			section.header.VirtualSize = sectionVirtualSize;
			imageVirtualSize += sectionVirtualSize;
		}

		optionalHeader.SizeOfImage = imageVirtualSize;
	}

	void collectCodeInfo()
	{
		bool codeSectionDetected = false;
		bool importSectionDetected = false;

		foreach (section; sections)
		{
			if (section.isCodeSection)
			{
				if (!codeSectionDetected)
				{
					codeSectionDetected = true;
					optionalHeader.BaseOfCode = section.header.VirtualAddress;
					optionalHeader.AddressOfEntryPoint = section.header.VirtualAddress;
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
					optionalHeader.ImportTable = section.header.VirtualAddress;
				}
			}

			optionalHeader.SizeOfInitializedData += section.header.SizeOfRawData;
		}
		version(print_info) writefln("Total code size is %sB", optionalHeader.SizeOfCode);
	}

	void fixupInvariants()
	{
		// COFF Header
		coffFileHeader.Machine = MachineType.amd64;
		coffFileHeader.NumberOfSections = cast(ushort)sections.length;
		coffFileHeader.TimeDateStamp = Clock.currTime().toUnixTime;
		coffFileHeader.PointerToSymbolTable = 0;
		coffFileHeader.NumberOfSymbols = 0;
		coffFileHeader.Characteristics =
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
		optionalHeader.Subsystem = 3; // CUI
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

	void write(ref ArraySink sink)
	{
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

struct DosHeader
{
	ubyte[64] hexData = [
		0x4D, 0x5A, 0x90, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00,
		0xB8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA8, 0x00, 0x00, 0x00
	];

	void write(ref ArraySink sink) {
		sink.put(hexData);
	}
}

/// MS-DOS Stub (Image Only)
///
/// The MS-DOS stub is a valid application that runs under MS-DOS. It is placed at the
/// front of the EXE image. The linker places a default stub here, which prints out the
/// message “This program cannot be run in DOS mode” when the image is run in
/// MS-DOS. The user can specify a different stub by using the /STUB linker option.
/// At location 0x3c, the stub has the file offset to the PE signature. This information
/// enables Windows to properly execute the image file, even though it has an
/// MS-DOS stub. This file offset is placed at location 0x3c during linking.
struct DosStub
{
	ubyte[104] hexData = [
		0x0E, 0x1F, 0xBA, 0x0E, 0x00, 0xB4, 0x09, 0xCD, 0x21, 0xB8, 0x01, 0x4C, 0xCD, 0x21, 0x54, 0x68,
		0x69, 0x73, 0x20, 0x70, 0x72, 0x6F, 0x67, 0x72, 0x61, 0x6D, 0x20, 0x63, 0x61, 0x6E, 0x6E, 0x6F,
		0x74, 0x20, 0x62, 0x65, 0x20, 0x72, 0x75, 0x6E, 0x20, 0x69, 0x6E, 0x20, 0x44, 0x4F, 0x53, 0x20,
		0x6D, 0x6F, 0x64, 0x65, 0x2E, 0x0D, 0x0D, 0x0A, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x39, 0xCF, 0x32, 0xDF, 0x7D, 0xAE, 0x5C, 0x8C, 0x7D, 0xAE, 0x5C, 0x8C, 0x7D, 0xAE, 0x5C, 0x8C,
		0xEA, 0xF0, 0x58, 0x8D, 0x7C, 0xAE, 0x5C, 0x8C, 0xEA, 0xF0, 0x5E, 0x8D, 0x7C, 0xAE, 0x5C, 0x8C,
		0x52, 0x69, 0x63, 0x68, 0x7D, 0xAE, 0x5C, 0x8C
	];

	void write(ref ArraySink sink) {
		sink.put(hexData);
	}
}

struct PeSignature
{
	immutable char[4] signature = "PE\0\0";
	void write(ref ArraySink sink) {
		sink.put(cast(ubyte[])signature);
	}
}
static assert(PeSignature.sizeof == 4);

/// The Machine field has one of the following values that specifies its CPU type. An
/// image file can be run only on the specified machine or on a system that emulates
/// the specified machine.
enum MachineType : ushort {
	amd64 = 0x8664, /// x64
	i386 = 0x14C, /// Intel 386 or later processors and compatible processors
	arm = 0x1C0, /// ARM little endian
	arm64 = 0xAA64, /// ARM64 little endian
}

/// The Characteristics field contains flags that indicate attributes of the object or image
/// file. The following flags are currently defined:
enum CoffFlags : ushort
{
	/// Image only, Windows CE, and Microsoft Windows NT® and later.
	/// This indicates that the file does not contain base relocations and must
	/// therefore be loaded at its preferred base address. If the base address is
	/// not available, the loader reports an error. The default behavior of the
	/// linker is to strip base relocations from executable (EXE) files.
	RELOCS_STRIPPED = 0x0001,

	/// Image only. This indicates that the image file is valid and can be run. If
	/// this flag is not set, it indicates a linker error.
	EXECUTABLE_IMAGE = 0x0002,

	//  COFF line numbers have been removed. This flag is deprecated and should be zero.
	//  LINE_NUMS_STRIPPED = 0x0004,

	//  COFF symbol table entries for local symbols have been removed. This
	//  flag is deprecated and should be zero.
	//  LOCAL_SYMS_STRIPPED = 0x0008,

	//  Obsolete. Aggressively trim working set. This flag is deprecated for
	//  Windows 2000 and later and must be zero.
	//  AGGRESSIVE_WS_TRIM = 0x0010,

	/// Application can handle > 2-GB addresses.
	LARGE_ADDRESS_AWARE = 0x0020,

	//  Little endian: the least significant bit (LSB) precedes the most significant
	//  bit (MSB) in memory. This flag is deprecated and should be zero.
	//  BYTES_REVERSED_LO = 0x0080,

	/// Machine is based on a 32-bit-word architecture.
	_32BIT_MACHINE = 0x0100,

	/// Debugging information is removed from the image file.
	DEBUG_STRIPPED = 0x0200,

	/// If the image is on removable media, fully load it and copy it to the swap file.
	REMOVABLE_RUN_FROM_SWAP = 0x0400,

	/// If the image is on network media, fully load it and copy it to the swap file.
	NET_RUN_FROM_SWAP = 0x0800,

	/// The image file is a system file, not a user program.
	SYSTEM = 0x1000,

	/// The image file is a dynamic-link library (DLL). Such files are
	/// considered executable files for almost all purposes, although they cannot be directly run.
	DLL = 0x2000,

	/// The file should be run only on a uniprocessor machine.
	UP_SYSTEM_ONLY = 0x4000,

	//  Big endian: the MSB precedes the LSB in memory. This flag is deprecated and should be zero
	//  BYTES_REVERSED_HI = 0x8000,
}

/// COFF File Header (Object and Image)
///
/// At the beginning of an object file, or immediately after the signature of an image file,
/// is a standard COFF file header in the following format. Note that the Windows
/// loader limits the number of sections to 96.
struct CoffFileHeader
{
	/// The number that identifies the type of target machine.
	MachineType Machine;

	/// The number of sections. This indicates the size of
	/// the section table, which immediately follows the headers.
	ushort NumberOfSections;

	/// The low 32 bits of the number of seconds since
	/// 00:00 January 1, 1970 (a C run-time time_t
	/// value), that indicates when the file was created.
	uint TimeDateStamp;

	/// The file offset of the COFF symbol table, or zero
	/// if no COFF symbol table is present. This value
	/// should be zero for an image because COFF
	/// debugging information is deprecated.
	uint PointerToSymbolTable;

	/// The number of entries in the symbol table. This
	/// data can be used to locate the string table, which
	/// immediately follows the symbol table. This value
	/// should be zero for an image because COFF
	/// debugging information is deprecated.
	uint NumberOfSymbols;

	/// The size of the optional header, which is required
	/// for executable files but not for object files. This
	/// value should be zero for an object file. For a
	/// description of the header format, see section 3.4,
	/// “Optional Header (Image Only).
	ushort SizeOfOptionalHeader = 240;

	/// The flags that indicate the attributes of the file.
	/// See CoffFlags.
	ushort Characteristics;

	size_t write(ref ArraySink sink) {
		auto offset = sink.length;
		sink.put(Machine);
		sink.put(NumberOfSections);
		sink.put(TimeDateStamp);
		sink.put(PointerToSymbolTable);
		sink.put(NumberOfSymbols);
		sink.put(SizeOfOptionalHeader);
		sink.put(Characteristics);
		return offset;
	}
}
static assert(CoffFileHeader.sizeof == 20);


/// Optional Header (Image Only)
///
/// Every image file has an optional header that provides information to the loader. This
/// header is optional in the sense that some files (specifically, object files) do not have
/// it. For image files, this header is required. An object file can have an optional
/// header, but generally this header has no function in an object file except to increase
/// its size.
/// Note that the size of the optional header is not fixed. The SizeOfOptionalHeader
/// field in the COFF header must be used to validate that a probe into the file for a
/// particular data directory does not go beyond SizeOfOptionalHeader. For more
/// information, see section 3.3, “COFF File Header (Object and Image).”
/// The NumberOfRvaAndSizes field of the optional header should also be used to
/// ensure that no probe for a particular data directory entry goes beyond the optional
/// header. In addition, it is important to validate the optional header magic number for
/// format compatibility.
/// The optional header magic number determines whether an image is a PE32 or PE32+ executable.
/// | Magic number + PE format
/// | 0x10b        | PE32
/// | 0x20b        | PE32+
/// PE32+ images allow for a 64-bit address space while limiting the image size to
/// 2 gigabytes. Other PE32+ modifications are addressed in their respective sections.
/// The optional header itself has three major parts
///
/// struct defines PE32+
struct OptionalHeader
{
	/// The unsigned integer that identifies the
	/// state of the image file. The most common
	/// number is 0x10B, which identifies it as a
	/// normal executable file. 0x107 identifies it as
	/// a ROM image, and 0x20B identifies it as a
	/// PE32+ executable.
	ushort Magic = 0x20B;

	/// The linker major version number.
	ubyte MajorLinkerVersion;

	/// The linker minor version number.
	ubyte MinorLinkerVersion;

	/// The size of the code (text) section, or the
	/// sum of all code sections if there are multiple
	/// sections.
	uint SizeOfCode;

	/// The size of the initialized data section, or
	/// the sum of all such sections if there are
	/// multiple data sections.
	uint SizeOfInitializedData;

	/// The size of the uninitialized data section
	/// (BSS), or the sum of all such sections if
	/// there are multiple BSS sections.
	uint SizeOfUninitializedData;

	/// The address of the entry point relative to the
	/// image base when the executable file is
	/// loaded into memory. For program images,
	/// this is the starting address. For device
	/// drivers, this is the address of the
	/// initialization function. An entry point is
	/// optional for DLLs. When no entry point is
	/// present, this field must be zero.
	uint AddressOfEntryPoint;

	/// The address that is relative to the image
	/// base of the beginning-of-code section when
	/// it is loaded into memory.
	uint BaseOfCode;

	/// The preferred address of the first
	/// byte of image when loaded into
	/// memory; must be a multiple of 64 K.
	/// The default for DLLs is 0x10000000.
	/// The default for Windows CE EXEs is
	/// 0x00010000. The default for
	/// Windows NT, Windows 2000,
	/// Windows XP, Windows 95,
	/// Windows 98, and Windows Me is
	/// 0x00400000.
	ulong ImageBase = 0x00400000;

	/// The alignment (in bytes) of sections
	/// when they are loaded into memory. It
	/// must be greater than or equal to
	/// FileAlignment. The default is the
	/// page size for the architecture.
	uint SectionAlignment = DEFAULT_SECTION_ALIGNMENT;

	/// The alignment factor (in bytes) that is
	/// used to align the raw data of sections
	/// in the image file. The value should be
	/// a power of 2 between 512 and 64 K,
	/// inclusive. The default is 512. If the
	/// SectionAlignment is less than the
	/// architecture’s page size, then
	/// FileAlignment must match
	/// SectionAlignment.
	uint FileAlignment = DEFAULT_FILE_ALIGNMENT;

	/// The major version number of the
	/// required operating system.
	ushort MajorOperatingSystemVersion;

	/// The minor version number of the
	/// required operating system.
	ushort MinorOperatingSystemVersion;

	/// The major version number of the
	/// image.
	ushort MajorImageVersion;

	/// The minor version number of the
	/// image.
	ushort MinorImageVersion;

	/// The major version number of the
	/// subsystem.
	ushort MajorSubsystemVersion;

	/// The minor version number of the
	/// subsystem.
	ushort MinorSubsystemVersion;

	/// Reserved, must be zero.
	uint Win32VersionValue = 0;

	/// The size (in bytes) of the image,
	/// including all headers, as the image is
	/// loaded in memory. It must be a
	/// multiple of SectionAlignment.
	uint SizeOfImage;

	/// The combined size of an MS-DOS
	/// stub, PE header, and section
	/// headers rounded up to a multiple of
	/// FileAlignment.
	uint SizeOfHeaders;

	/// The image file checksum. The
	/// algorithm for computing the
	/// checksum is incorporated into
	/// IMAGHELP.DLL. The following are
	/// checked for validation at load time:
	/// all drivers, any DLL loaded at boot
	/// time, and any DLL that is loaded into
	/// a critical Windows process.
	uint CheckSum;

	/// The subsystem that is required to run
	/// this image. For more information, see
	/// “Windows Subsystem” later in this
	/// specification.
	ushort Subsystem;

	/// For more information, see “DLL
	/// Characteristics” later in this
	/// specification.
	ushort DllCharacteristics;

	/// The size of the stack to reserve. Only
	/// SizeOfStackCommit is committed;
	/// the rest is made available one page
	/// at a time until the reserve size is
	/// reached.
	ulong SizeOfStackReserve;

	/// The size of the stack to commit.
	ulong SizeOfStackCommit;

	/// The size of the local heap space to
	/// reserve. Only SizeOfHeapCommit is
	/// committed; the rest is made available
	/// one page at a time until the reserve
	/// size is reached.
	ulong SizeOfHeapReserve;

	/// The size of the local heap space to
	/// commit.
	ulong SizeOfHeapCommit;

	/// Reserved, must be zero.
	uint LoaderFlags = 0;

	/// The number of data-directory entries
	/// in the remainder of the optional
	/// header. Each describes a location
	/// and size.
	uint NumberOfRvaAndSizes = 16;

	/// The export table address and size. For more
	/// information see section 6.3, “The .edata Section
	/// (Image Only).”
	ulong ExportTable;

	/// The import table address and size. For more
	/// information, see section 6.4, “The .idata
	/// Section.”
	ulong ImportTable;

	/// The resource table address and size. For more
	/// information, see section 6.9, “The .rsrc Section.”
	ulong ResourceTable;

	/// The exception table address and size. For more
	/// information, see section 6.5, “The .pdata
	/// Section.”
	ulong ExceptionTable;

	/// The attribute certificate table address and size.
	/// For more information, see section 5.7, “The
	/// Attribute Certificate Table (Image Only).”
	ulong CertificateTable;

	/// The base relocation table address and size. For
	/// more information, see section 6.6, "The .reloc
	/// Section (Image Only)."
	ulong BaseRelocationTable;

	/// The debug data starting address and size. For
	/// more information, see section 6.1, “The .debug
	/// Section.”
	ulong Debug;

	/// Reserved, must be 0
	ulong Architecture = 0;

	/// The RVA of the value to be stored in the global
	/// pointer register. The size member of this
	/// structure must be set to zero.
	ulong GlobalPtr;

	/// The thread local storage (TLS) table address
	/// and size. For more information, see section 6.7,
	/// “The .tls Section.”
	ulong TLSTable;

	/// The load configuration table address and size.
	/// For more information, see section 6.8, “The Load
	/// Configuration Structure (Image Only).”
	ulong LoadConfigTable;

	/// The bound import table address and size.
	ulong BoundImport;

	/// The import address table address and size. For
	/// more information, see section 6.4.4, “Import
	/// Address Table.”
	ulong IAT;

	/// The delay import descriptor address and size.
	/// For more information, see section 5.8, “DelayLoad Import Tables (Image Only).”
	ulong DelayImportDescriptor;

	/// The CLR runtime header address and size. For
	/// more information, see section 6.10, “The
	/// .cormeta Section (Object Only).”
	ulong CLRRuntimeHeader;

	ulong _reserved; /// Reserved, must be zero

	size_t write(ref ArraySink sink) {
		auto offset = sink.length;
		sink.put(Magic);
		sink.put(MajorLinkerVersion);
		sink.put(MinorLinkerVersion);
		sink.put(SizeOfCode);
		sink.put(SizeOfInitializedData);
		sink.put(SizeOfUninitializedData);
		sink.put(AddressOfEntryPoint);
		sink.put(BaseOfCode);
		sink.put(ImageBase);
		sink.put(SectionAlignment);
		sink.put(FileAlignment);
		sink.put(MajorOperatingSystemVersion);
		sink.put(MinorOperatingSystemVersion);
		sink.put(MajorImageVersion);
		sink.put(MinorImageVersion);
		sink.put(MajorSubsystemVersion);
		sink.put(MinorSubsystemVersion);
		sink.put(Win32VersionValue);
		sink.put(SizeOfImage);
		sink.put(SizeOfHeaders);
		sink.put(CheckSum);
		sink.put(Subsystem);
		sink.put(DllCharacteristics);
		sink.put(SizeOfStackReserve);
		sink.put(SizeOfStackCommit);
		sink.put(SizeOfHeapReserve);
		sink.put(SizeOfHeapCommit);
		sink.put(LoaderFlags);
		sink.put(NumberOfRvaAndSizes);
		sink.put(ExportTable);
		sink.put(ImportTable);
		sink.put(ResourceTable);
		sink.put(ExceptionTable);
		sink.put(CertificateTable);
		sink.put(BaseRelocationTable);
		sink.put(Debug);
		sink.put(Architecture);
		sink.put(GlobalPtr);
		sink.put(TLSTable);
		sink.put(LoadConfigTable);
		sink.put(BoundImport);
		sink.put(IAT);
		sink.put(DelayImportDescriptor);
		sink.put(CLRRuntimeHeader);
		sink.put(_reserved);
		return offset;
	}
}
static assert(OptionalHeader.sizeof == 240);

enum SectionFlags : uint
{
	/// The section contains executable code.
	SCN_CNT_CODE = 0x00000020,
	/// The section contains initialized data.
	SCN_CNT_INITIALIZED_DATA = 0x00000040,
	/// The section contains uninitialized data.
	SCN_CNT_UNINITIALIZED_DATA = 0x00000080,
	/// The section contains comments or
	/// other information. The .drectve section has this type. This is valid
	/// for object files only.
	SCN_LNK_INFO = 0x00000200,
	/// The section will not become part
	/// of the image. This is valid only for object files.
	SCN_LNK_REMOVE = 0x00000800,
	/// The section contains COMDAT data. For more information, see
	/// section 5.5.6, “COMDAT Sections (Object Only).” This is valid only for object files.
	SCN_LNK_COMDAT = 0x00001000,
	/// The section contains data referenced through the global pointer (GP).
	SCN_GPREL = 0x00008000,
	/// Align data on a    1-byte boundary. Valid only for object files.
	SCN_ALIGN_1BYTES = 0x00100000,
	/// Align data on a    2-byte boundary. Valid only for object files.
	SCN_ALIGN_2BYTES = 0x00200000,
	/// Align data on a    4-byte boundary. Valid only for object files.
	SCN_ALIGN_4BYTES = 0x00300000,
	/// Align data on a    8-byte boundary. Valid only for object files.
	SCN_ALIGN_8BYTES = 0x00400000,
	/// Align data on a   16-byte boundary. Valid only for object files.
	SCN_ALIGN_16BYTES = 0x00500000,
	/// Align data on a   32-byte boundary. Valid only for object files.
	SCN_ALIGN_32BYTES = 0x00600000,
	/// Align data on a   64-byte boundary. Valid only for object files.
	SCN_ALIGN_64BYTES = 0x00700000,
	/// Align data on a  128-byte boundary. Valid only for object files.
	SCN_ALIGN_128BYTES = 0x00800000,
	/// Align data on a  256-byte boundary. Valid only for object files.
	SCN_ALIGN_256BYTES = 0x00900000,
	/// Align data on a  512-byte boundary. Valid only for object files.
	SCN_ALIGN_512BYTES = 0x00A00000,
	/// Align data on a 1024-byte boundary. Valid only for object files.
	SCN_ALIGN_1024BYTES = 0x00B00000,
	/// Align data on a 2048-byte boundary. Valid only for object files.
	SCN_ALIGN_2048BYTES = 0x00C00000,
	/// Align data on a 4096-byte boundary. Valid only for object files.
	SCN_ALIGN_4096BYTES = 0x00D00000,
	/// Align data on a 8192-byte boundary. Valid only for object files.
	SCN_ALIGN_8192BYTES = 0x00E00000,
	/// The section contains extended relocations.
	SCN_LNK_NRELOC_OVFL = 0x01000000,
	/// The section can be discarded as needed.
	SCN_MEM_DISCARDABLE = 0x02000000,
	/// The section cannot be cached.
	SCN_MEM_NOT_CACHED = 0x04000000,
	/// The section is not pageable.
	SCN_MEM_NOT_PAGED = 0x08000000,
	/// The section can be shared in memory.
	SCN_MEM_SHARED = 0x10000000,
	/// The section can be executed as code.
	SCN_MEM_EXECUTE = 0x20000000,
	/// The section can be read.
	SCN_MEM_READ = 0x40000000,
	/// The section can be written to.
	SCN_MEM_WRITE = 0x80000000,
}

struct SectionHeader
{
	/// An 8-byte, null-padded UTF-8 encoded string. If
	/// the string is exactly 8 characters long, there is no
	/// terminating null. For longer names, this field
	/// contains a slash (/) that is followed by an ASCII
	/// representation of a decimal number that is an
	/// offset into the string table. Executable images do
	/// not use a string table and do not support section
	/// names longer than 8 characters. Long names in
	/// object files are truncated if they are emitted to an
	/// executable file.
	char[8] Name;

	/// The total size of the section when loaded into
	/// memory. If this value is greater than
	/// SizeOfRawData, the section is zero-padded. This
	/// field is valid only for executable images and
	/// should be set to zero for object files.
	uint VirtualSize;

	/// For executable images, the address of the first
	/// byte of the section relative to the image base
	/// when the section is loaded into memory. For
	/// object files, this field is the address of the first
	/// byte before relocation is applied; for simplicity,
	/// compilers should set this to zero. Otherwise, it is
	/// an arbitrary value that is subtracted from offsets
	/// during relocation.
	uint VirtualAddress;

	/// The size of the section (for object files) or the
	/// size of the initialized data on disk (for image
	/// files). For executable images, this must be a
	/// multiple of FileAlignment from the optional
	/// header. If this is less than VirtualSize, the
	/// remainder of the section is zero-filled. Because
	/// the SizeOfRawData field is rounded but the
	/// VirtualSize field is not, it is possible for
	/// SizeOfRawData to be greater than VirtualSize as
	/// well. When a section contains only uninitialized
	/// data, this field should be zero.
	uint SizeOfRawData;

	/// The file pointer to the first page of the section
	/// within the COFF file. For executable images, this
	/// must be a multiple of FileAlignment from the
	/// optional header. For object files, the value should
	/// be aligned on a 4-byte boundary for best
	/// performance. When a section contains only
	/// uninitialized data, this field should be zero.
	uint PointerToRawData;

	/// The file pointer to the beginning of relocation
	/// entries for the section. This is set to zero for
	/// executable images or if there are no relocations.
	uint PointerToRelocations;

	/// The file pointer to the beginning of line-number
	/// entries for the section. This is set to zero if there
	/// are no COFF line numbers. This value should be
	/// zero for an image because COFF debugging
	/// information is deprecated.
	uint PointerToLinenumbers = 0;

	/// The number of relocation entries for the section.
	/// This is set to zero for executable images.
	ushort NumberOfRelocations;

	/// The number of line-number entries for the
	/// section. This value should be zero for an image
	/// because COFF debugging information is
	/// deprecated.
	ushort NumberOfLinenumbers = 0;

	/// The flags that describe the characteristics of the
	/// section. See SectionFlags.
	uint Characteristics;

	size_t write(ref ArraySink sink) {
		auto offset = sink.length;
		sink.put(cast(ubyte[])Name);
		sink.put(VirtualSize);
		sink.put(VirtualAddress);
		sink.put(SizeOfRawData);
		sink.put(PointerToRawData);
		sink.put(PointerToRelocations);
		sink.put(PointerToLinenumbers);
		sink.put(NumberOfRelocations);
		sink.put(NumberOfLinenumbers);
		sink.put(Characteristics);
		return offset;
	}
}
static assert(SectionHeader.sizeof == 40);

struct ImportDirectoryTableEntry
{
	/// The RVA of the import lookup table. This table contains
	/// a name or ordinal for each import. (The name
	/// “Characteristics” is used in Winnt.h, but no longer
	/// describes this field.)
	uint importLookupTableRVA;

	/// The stamp that is set to zero until the image is bound.
	/// After the image is bound, this field is set to the
	/// time/data stamp of the DLL.
	uint timestamp;

	/// The index of the first forwarder reference.
	uint forwarderChain;

	/// The address of an ASCII string that contains the name
	/// of the DLL. This address is relative to the image base.
	uint nameRVA;

	/// The RVA of the import address table. The contents of
	/// this table are identical to the contents of the import
	/// lookup table until the image is bound.
	uint importAddressTableRVA;

	size_t write(ref ArraySink sink) {
		auto offset = sink.length;
		sink.put(this);
		return offset;
	}
}

struct ImportLookupTable
{
	ImportLookupEntry[] entries;
	size_t write(ref ArraySink sink) {
		auto offset = sink.length;
		foreach(ref entry; entries) entry.write(sink);
		sink.pad(ImportLookupEntry.sizeof); // Null Import Lookup Entry
		return offset;
	}
}

struct ImportLookupEntry
{
	enum ulong ORDINAL_FLAG = 0x8000_0000_0000_0000;
	static ImportLookupEntry fromOrdinal(ushort ordinal) {
		return ImportLookupEntry(ORDINAL_FLAG | ordinal);
	}
	static ImportLookupEntry fromHintNameTableRVA(uint hintRVA) {
		return ImportLookupEntry(hintRVA & 0x7FFF_FFFF);
	}
	ulong value;
	size_t write(ref ArraySink sink) {
		auto offset = sink.length;
		sink.put(this);
		return offset;
	}
}

struct HintNameTable
{
	HintNameEntry[] entries;

	size_t write(ref ArraySink sink) {
		auto offset = sink.length;
		foreach(ref entry; entries) entry.write(sink);
		return offset;
	}
}

struct HintNameEntry
{
	/// An index into the export name pointer table. A match is
	/// attempted first with this value. If it fails, a binary search is
	/// performed on the DLL’s export name pointer table.
	ushort Hint;

	/// An ASCII string that contains the name to import. This is the
	/// string that must be matched to the public name in the DLL.
	/// This string is case sensitive and terminated by a null byte.
	string Name;

	size_t write(ref ArraySink sink) {
		auto offset = sink.length;

		sink.put(Hint);
		sink.writeStringAligned2(Name);

		return offset;
	}
}

void writeStringAligned2(ref ArraySink sink, string str)
{
	sink.put(cast(ubyte[])str);

	// put trailing null byte and align on 2 bytes boundary
	if (sink.length % 2 == 0)
		sink.put!ushort(0); // 2 null bytes
	else
		sink.put!ubyte(0); // 1 null byte
}

struct SymbolRef
{
	SymbolIndex symbolIndex;
	uint sectionId;
}

alias SymbolIndex = uint;

struct SymbolSectionInfo
{
	uint sectionOffset;
	uint length;
}

struct Section
{
	uint sectionId;
	SectionHeader header;
	ubyte[] data;
	SymbolSectionInfo[] symbols;

	string name() {
		foreach_reverse(i, chr; header.Name)
			if (chr != '\0') return cast(string)header.Name[0..i+1];
		return null;
	}

	SymbolIndex addSymbol(uint offset, uint length)
	{
		SymbolIndex index = cast(uint)symbols.length;
		symbols ~= SymbolSectionInfo(offset, length);
		return index;
	}

	uint symOffset(SymbolIndex symbol)
	{
		return symbols[symbol].sectionOffset;
	}

	uint symBytes(SymbolIndex symbol)
	{
		return symbols[symbol].length;
	}

	uint dataSize() { return cast(uint)data.length; }

	bool isCodeSection()
	{
		return header.Name[0..5] == ".text";
	}

	bool isImportSection()
	{
		return header.Name[0..6] == ".idata";
	}
}
