/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module be.pecoff;

import std.bitmanip;
import std.stdio;
import std.conv;
import std.file;
import std.path;
import core.time;
import std.exception;
import std.string;
import std.format : formattedWrite;

import be.amd64asm;
import utils;

//version = print_info;
version = use_mmap;
//version = standalone;

version (standalone)
void main()
{
	testExeCompilation();
	//testObjLinking();
	//testObjLibDump();
}

void testObjLibDump()
{
	auto file = File("../asm/dump.txt", "w");
	//auto output = stdout.lockingTextWriter;
	auto output = file.lockingTextWriter;

	//PecoffObj.fromFile("../asm/obj/simple.obj").print(output);

	//PecoffObj.fromFile("../asm/obj/gcstub64.obj").print(output);
	//PecoffLib.fromFile("../asm/lib/phobos64.lib").objs[0].toFile("../asm/obj/zutil.obj");
	//PecoffObj.fromFile("../asm/obj/zutil.obj").print(output);

	//PecoffLib.fromFile("../asm/lib/oldnames.lib").print(output);

	auto time0 = currTime;
	auto lib = PecoffLib.fromFile("../asm/lib/phobos64.lib");
	//auto lib = PecoffLib.fromFile("../asm/lib/libcmt.lib");
	auto time1 = currTime;
	//printSectionStats(output, lib);
	//printSymbolStats(output, lib);
	auto time2 = currTime;
	lib.print(output);
	auto time3 = currTime;
	writefln("Load %ss, dump %ss", scaledNumberFmt(time1 - time0), scaledNumberFmt(time3 - time2));
	//PecoffObj.fromFile("../asm/obj/loadcfg.obj").print(output); // reallocation past the end of section

	// 1 overflow in loadcfg.obj
	//auto lib = PecoffLib.fromFile("../asm/lib/libcmt.lib");
	//printSectionStats(output, lib);
	//lib.print(output);
	//PecoffLib.fromFile("../asm/lib/libcmt.lib").objs[0x17B].toFile("../asm/obj/loadcfg.obj");

	//PecoffLib.fromFile("../asm/lib/helloc.lib").print(output);
	//PecoffObj.fromFile("../asm/obj/helloc.obj").print(output);
	//PecoffObj.fromFile("../asm/obj/extd.obj").print(output);
	//PecoffObj.fromFile("../asm/obj/extc.obj").print(output);
	//PecoffObj.fromFile("../asm/obj/test.obj").print(output);
	//PecoffObj.fromFile("../asm/obj/betterc.obj").print(output);
}

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

void testObjLinking()
{
	auto file = File("../asm/linking.txt", "w");
	auto output = file.lockingTextWriter;

	auto obj1 = PecoffObj.fromFile("../asm/obj/helloc.obj");
	auto obj2 = PecoffObj.fromFile("../asm/obj/extc.obj");
	string outname = "../asm/out.exe";
	string entryPointName = "main";

	//obj1.print(output);
	//obj2.print(output);

	LinkingContext ctx;

	ctx.setEntryPoint(entryPointName);
	ctx.put(obj1);
	ctx.put(obj2);

	// undef syms
	// def syms

	// push entry point as undef symbol
	// push inputs

	// iteratively resolve by loading undefined symbols

	// after all symbols were resolved
	// sort sections
}

struct LinkingContext
{
	SymbolName entryPointName;
	SymbolNameTable symNameTable;

	void setEntryPoint(string symName)
	{
		SymbolName name = symNameTable.put(symName);
		entryPointName = name;
	}

	void put(PecoffObj obj)
	{
		foreach (item; CoffSymbolRange(obj.symbols))
		{
			if (item.sym.isAbsolute) continue;

			string nameStr = item.sym.Name.get(obj.stringTable);
			SymbolName name = symNameTable.put(nameStr);

			string indentation = "    ";
			writeln(nameStr);

			if (item.sym.isFunction) writeln(indentation, "fun");
			else if (item.sym.isNonFunction) writeln(indentation, "non-fun");

			switch(item.sym.StorageClass)
			{
				case CoffSymClass.STATIC: writeln(indentation, "STATIC"); break;
				case CoffSymClass.LABEL: writeln(indentation, "LABEL"); break;
				case CoffSymClass.EXTERNAL: writeln(indentation, "EXTERNAL"); break;
				case CoffSymClass.WEAK_EXTERNAL: writeln(indentation, "WEAK_EXTERNAL"); break;
				default: break;
			}

			if (item.sym.isUndefined) writeln(indentation, "Undef");
			writefln("%sDAT [%(%02X %)]", indentation, cast(ubyte[])item.sym[0..1]);
			if (item.aux.length)
			{
				ubyte[] auxBytes = cast(ubyte[])item.aux;
				writefln("%sAUX [%(%02X %)]", indentation, auxBytes);
			}
		}
	}
}

enum SymbolRefType : ubyte
{
	absolute,
	rel32,
	rel64
}

struct SymbolReference
{
	SymbolIndex target;
	uint offset; // start of fixup for current symbol
	SymbolRefType type;
	long addend; // value added to relocation
}

enum SymbolType : ubyte
{
	undefined,
	absolute,
	sharedLib
}

struct Symbol
{
	SymbolType type;
	ulong address;
	SymbolReference[] references;
}

struct SymbolName
{
	uint stringIndex;
}

struct SymbolNameTable
{
	Buffer!string stringTable;
	SymbolName[string] nameToIndex;

	SymbolName put(string strName)
	{
		if (auto name = strName in nameToIndex)
		{
			return *name;
		}
		else
		{
			auto name = SymbolName(cast(typeof(SymbolName.stringIndex))stringTable.data.length);
			stringTable.put(strName);
			nameToIndex[strName] = name;
			return name;
		}
	}
}

struct SymbolTable
{
	Buffer!Symbol symbols;
	//Buffer!SymbolIndex definedSymbols;
	//Buffer!SymbolIndex undefinedSymbols;
	//Buffer!SymbolIndex sharedSymbols;
	//Buffer!SymbolIndex reachableSymbols;

	SymbolIndex put(Symbol sym)
	{
		size_t index = symbols.data.length;
		symbols.put(sym);
		return cast(SymbolIndex)index;
	}
}

void testExeCompilation()
{
	auto time0 = currTime;

	ubyte[] buf = allocate(0x1000 * 20, null, MemType.RW);
	scope(exit) deallocate(buf);
	ubyte[] binaryMem = buf[0..2*0x1000];// = allocator.allocate(4096 * 8, MemType.RW);
	ubyte[] importMem = buf[2*0x1000..4*0x1000];
	ubyte[] dataMem = buf[4*0x1000..6*0x1000];

	Arena!ubyte codeMem;
	codeMem.setBuffer(buf[6*0x1000..$]);

	// ---------------------------------------------------------

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
		SectionFlags.SCN_CNT_INITIALIZED_DATA |
		SectionFlags.SCN_MEM_READ;

	// ---------------------------------------------------------

	// Exe gen
	Section*[3] sections;
	sections[SectionType.text] = &textSection;
	sections[SectionType.idata] = &idataSection;
	sections[SectionType.data] = &dataSection;

	// ---------------------------------------------------------

	Arena!ubyte sink;
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
	codeGen.encoder.setBuffer(&codeMem);

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
	codeGen.movd(Register.CX, Imm32(STD_OUTPUT_HANDLE));
	codeGen.call(memAddrRipDisp32(0)); // RAX = GetStdHandle(STD_OUTPUT_HANDLE)
	putFixup(ref_GetStdHandle);

	codeGen.movq(Register.CX, Register.AX); // 1 in hConsoleOutput
	codeGen.leaq(Register.DX, memAddrRipDisp32(0)); // 2 in lpBuffer
	putFixup(msg_str);

	codeGen.movq(Register.R8, Imm32(dataSection.symBytes(msg_str.symbolIndex))); // 3 in nNumberOfCharsToWrite
	codeGen.leaq(Register.R9, memAddrBaseDisp8(Register.SP, 0x28)); // 4 out lpNumberOfCharsWritten
	codeGen.movq(memAddrBaseDisp8(Register.SP, 0x20), Imm32(0)); // 5 in lpReserved

	codeGen.call(memAddrRipDisp32(0)); // call WriteConsoleA(RAX, "Hello world!", "Hello world!".length, SP+0x28, 0)
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

	std.file.write(outputFilename, sink.data);

	// test exe
	import std.process;
	import std.path;
	auto time2 = currTime;
	auto result = execute("out.exe");
	auto time3 = currTime;

	writefln("%s exited with %s, in %ss", outputFilename, result.status, scaledNumberFmt(time3 - time2));
	writefln("Compile in %ss, write %ss", scaledNumberFmt(time1 - time0), scaledNumberFmt(time2 - time1));
	writeln(absolutePath(outputFilename));
	//assert(result.status == 0);
}

struct FileDataSlicer
{
	ubyte[] fileData;
	size_t fileCursor = 0;

	// Returns array of Ts of length 'length' stating from fileCursor offset in fileData
	T[] getArrayOf(T)(size_t length)
	{
		enforce(fileData.length >= fileCursor + T.sizeof * length);
		auto res = (cast(T*)(fileData.ptr + fileCursor))[0..length];
		fileCursor += T.sizeof * length;
		return res;
	}

	T* getPtrTo(T)() { return getArrayOf!T(1).ptr; }
	T parseBigEndian(T)() {
		ubyte[T.sizeof] buf = getArrayOf!ubyte(T.sizeof);
		return bigEndianToNative!T(buf);
	}

	void advanceToAlignment(size_t alignment) { fileCursor += paddingSize(fileCursor, alignment); }
}

struct CoffSymbolRangeItem
{
	size_t tableIndex;
	SymbolTableEntry* sym;
	SymbolTableEntry[] aux;
}

struct CoffSymbolRange
{
	SymbolTableEntry[] symbols;

	int opApply(scope int delegate(CoffSymbolRangeItem) dg)
	{
		size_t tableIndex = 0;
		while (tableIndex < symbols.length)
		{
			auto item = CoffSymbolRangeItem(tableIndex, &symbols[tableIndex]);
			size_t numAux = symbols[tableIndex].NumberOfAuxSymbols;
			if (numAux) item.aux = symbols[tableIndex+1..tableIndex+1+numAux];
			if (auto res = dg(item)) return res;
			tableIndex += 1 + numAux;
		}
		return 0;
	}
}

struct PecoffObj
{
	string filename;
	ubyte[] fileData;

	bool isLoaded; /// Indicates if members below are usable. Call load if not.
	CoffFileHeader* header;
	SectionHeader[] sectionHeaders;
	Section[] sections;
	SymbolTableEntry[] symbols;
	string stringTable;
	size_t totalRelocations = 0;

	static PecoffObj fromFileLazy(string filename)
	{
		enforce(exists(filename), format("%s does not exist", filename));
		enforce(extension(filename) == ".obj", format("%s must have .obj extension", filename));

		ubyte[] fileData = cast(ubyte[])std.file.read(filename);
		return fromBytesLazy(fileData, filename);
	}

	static PecoffObj fromFile(string filename)
	{
		PecoffObj obj = fromFileLazy(filename);
		obj.load();
		return obj;
	}

	/// Returns obj without actual parsing. Call load before accessing
	static PecoffObj fromBytesLazy(ubyte[] fileData, string filename = null)
	{
		PecoffObj obj;
		obj.filename = filename;
		obj.fileData = fileData;

		return obj;
	}

	static PecoffObj fromBytes(ubyte[] fileData, string filename = null)
	{
		PecoffObj obj = fromBytesLazy(fileData, filename);
		obj.load();
		return obj;
	}

	/// Performs actual processing of .obj data stored in fileData if not already loaded
	void load()
	{
		if (isLoaded) return;

		auto slicer = FileDataSlicer(fileData);

		header = slicer.getPtrTo!CoffFileHeader;
		sectionHeaders = slicer.getArrayOf!SectionHeader(header.NumberOfSections);
		sections = new Section[sectionHeaders.length];

		slicer.fileCursor = header.PointerToSymbolTable;
		symbols = slicer.getArrayOf!SymbolTableEntry(header.NumberOfSymbols);

		uint stringTableSize = *slicer.getPtrTo!uint();
		assert(stringTableSize >= 4);
		stringTable = cast(string)slicer.getArrayOf!char(stringTableSize - 4);

		// Fill sections
		foreach(size_t i, ref section; sections)
		{
			section.sectionId = cast(uint)i;
			section.header = sectionHeaders[i];
			section.name = section.header.getName(stringTable);

			size_t from = section.header.PointerToRawData;
			size_t to = from + section.header.SizeOfRawData;
			if (from < fileData.length && to <= fileData.length)
			{
				section.data = fileData[from..to];
			}
			else
			{
				section.data = null;
				writefln("ERROR %s Sect %X, SizeOfRawData %X from %X to %X len %X",
					filename, i+1, section.header.SizeOfRawData, from, to, fileData.length);
			}

			slicer.fileCursor = section.header.PointerToRelocations;
			section.relocations = slicer.getArrayOf!CoffRelocation(section.header.NumberOfRelocations);
			totalRelocations += section.relocations.length;
		}

		isLoaded = true;
	}

	void toFile(string filename)
	{
		toBytes(File(filename, "w").lockingBinaryWriter);
	}

	void toBytes(Sink)(auto ref Sink sink)
	{
		sink.put(fileData);
	}

	void print(Sink)(auto ref Sink sink)
	{
		if (!isLoaded) load(); // lazy load

		formattedWrite(sink, "%s:\n", filename);
		sink.put("\n");

		header.print(sink);
		sink.put("\n");

		printSectionTable(sink);
		sink.put("\n");

		printRelocations(sink);
		sink.put("\n");

		printSymbolTable(sink);

		printStringTable(sink);
	}

	void printSectionTable(Sink)(auto ref Sink sink)
	{
		if (!isLoaded) load(); // lazy load

		sink.put("Section table:\n");
		size_t totalRelocations = 0;
		SectionHeader.printTableHeader(sink);
		foreach(sectionIndex, ref section; sections)
		{
			totalRelocations += section.relocations.length;
			section.header.print(sink, sectionIndex+1, stringTable);
			if (section.isLinkDirective) formattedWrite(sink, "      Link directive:%s\n", cast(string)section.data);
		}
	}

	void printRelocations(Sink)(auto ref Sink sink)
	{
		if (!isLoaded) load(); // lazy load

		formattedWrite(sink, "Relocations: %X records\n", totalRelocations);

		if (totalRelocations)
		{
			formattedWrite(sink, "                                                       Symbol  Symbol\n");
			formattedWrite(sink, "Sect  Offset    Type                     Applied To     Index  Name  \n");
			formattedWrite(sink, "----  --------  ----------------  -----------------  --------  ------\n");
			foreach(sectionIndex, ref section; sections)
			{
				if (section.relocations.length)
				{
					foreach(ref reloc; section.relocations)
					{
						// Read bytes that changed by relocation and display in hex
						import std.range : repeat;
						size_t bytesToRead = divCeil(reloc.targetSize, 8);
						size_t from = reloc.VirtualAddress - section.header.VirtualAddress;
						size_t to = from + bytesToRead;
						ubyte[8] buf;
						size_t appliedTo;

						//writefln("%s section.VA %X bytesToRead %s from %X to %X len %X",
						//	reloc.typeString, section.header.VirtualAddress, bytesToRead, from, to, section.data.length);
						if (from < section.data.length && to <= section.data.length)
						{
							buf[0..bytesToRead] = section.data[from..to];
							appliedTo = *cast(size_t*)buf.ptr;
						}
						else
						{
							formattedWrite(sink, "ERROR --------  ----------------  v v  Overflow v v  --------  ------\n");
							appliedTo = 0;
						}

						formattedWrite(sink, "% 4X  %08X  % 16s  %s%0*X  % 8X  %s\n",
							sectionIndex+1,
							reloc.VirtualAddress,
							reloc.typeString,
							' '.repeat(17 - bytesToRead*2), bytesToRead*2, appliedTo,
							reloc.SymbolTableIndex,
							symbols[reloc.SymbolTableIndex].Name.get(stringTable));
					}
				}
			}
		}
	}

	void printSymbolTable(Sink)(auto ref Sink sink)
	{
		if (!isLoaded) load(); // lazy load

		sink.put("Symbol table:\n");
		sink.put("       #     Value   Section  Type      Class     Name    \n");
		sink.put("--------  --------  --------  --------  --------  --------\n");
		foreach(item; CoffSymbolRange(symbols))
		{
			// # Value
			formattedWrite(sink, "% 8X  % 8X  ",//"% 8X  % 8X  %s\n",
				item.tableIndex,
				item.sym.Value);

			// Section
			switch(item.sym.SectionNumber) {
				case -2: formattedWrite(sink, "Debug     "); break;
				case -1: formattedWrite(sink, "Absolute  "); break;
				case  0: formattedWrite(sink, "Undef     "); break;
				default: formattedWrite(sink, "% 8X  ", item.sym.SectionNumber);
			}

			// Type
			if (item.sym.isFunction) formattedWrite(sink, "function  ");
			else if (item.sym.isNonFunction) formattedWrite(sink, "not func  ");
			else formattedWrite(sink, "% 8X  ", item.sym.Type);

			// Class
			formattedWrite(sink, "% 8s  ", item.sym.StorageClass);

			// Name
			formattedWrite(sink, "%s\n", item.sym.Name.get(stringTable));

			foreach(i, ref SymbolTableEntry auxSym; item.aux)
			{
				// # Value
				formattedWrite(sink, "     AUX  % 8X  ",//"% 8X  % 8X  %s\n",
					item.tableIndex + 1 + i,
					item.sym.Value);

				// Section
				switch(item.sym.SectionNumber) {
					case -2: formattedWrite(sink, "Debug     "); break;
					case -1: formattedWrite(sink, "Absolute  "); break;
					case  0: formattedWrite(sink, "Undef     "); break;
					default: formattedWrite(sink, "% 8X  ", item.sym.SectionNumber);
				}

				// Type
				if (item.sym.isFunction) formattedWrite(sink, "function  ");
				else if (item.sym.isNonFunction) formattedWrite(sink, "          ");
				else formattedWrite(sink, "% 8X  ", item.sym.Type);

				// Class
				formattedWrite(sink, "% 8s  \n", item.sym.StorageClass);
			}
		}
	}

	void printStringTable(Sink)(auto ref Sink sink)
	{
		if (!isLoaded) load(); // lazy load

		formattedWrite(sink, "\nString table: %s bytes at %08X\n", stringTable.length+4, header.PointerToSymbolTable);
		.printStringTable(stringTable, sink, 4);
	}
}

void printStringTable(Sink)(string stringTable, auto ref Sink sink, size_t tableAddrOffset = 0)
{
	if (stringTable.length != 0)
	{
		import std.algorithm : splitter, joiner;
		size_t i = 0;
		sink.put("       #    Offset  String  \n");
		sink.put("--------  --------  --------\n");
		foreach(string sym; stringTable[0..$-1].splitter('\0'))
		{
			formattedWrite(sink, "% 8X  % 8X  ", i++, sym.ptr - stringTable.ptr + tableAddrOffset);
			sink.put(sym);
			sink.put('\n');
		}
	}
}

immutable ARCHIVE_FILE_SIGNATURE = "!<arch>\n";

struct PecoffLib
{
	version(use_mmap)
	{
		import std.mmfile;
		MmFile file;
	}

	string filename;
	ubyte[] fileData;
	uint[] memberOffsets;
	ushort[] indicies;
	string stringTable;
	PecoffObj[] objs;
	string longNames;

	static PecoffLib fromFile(string filename)
	{
		enforce(exists(filename), format("%s does not exist", filename));
		enforce(extension(filename) == ".lib", format("%s must have .lib extension", filename));

		version(use_mmap)
		{
			MmFile file = new MmFile(filename);
			ubyte[] fileData = cast(ubyte[])file[];
		}
		else
		{
			ubyte[] fileData = cast(ubyte[])std.file.read(filename);
		}

		auto lib = fromBytes(fileData, filename);

		version(use_mmap) lib.file = file;

		return lib;
	}

	static PecoffLib fromBytes(ubyte[] fileData, string filename = null)
	{
		auto slicer = FileDataSlicer(fileData);

		string signature = cast(string)slicer.getArrayOf!char(ARCHIVE_FILE_SIGNATURE.length);
		enforce(signature == ARCHIVE_FILE_SIGNATURE, format("%s has no !<arch> file signature", filename));

		PecoffLib lib;
		lib.filename = filename;
		lib.fileData = fileData;

		ParsedLibMemberHeader readMemberHeader()
		{
			LibMemberHeader* firstMemberText = slicer.getPtrTo!LibMemberHeader;
			ParsedLibMemberHeader firstMember = firstMemberText.parse;
			firstMemberText.validate();
			return firstMember;
		}

		ParsedLibMemberHeader currentMember;

		// 1st member
		{
			currentMember = readMemberHeader();
			enforce(currentMember.Name == "/", format("1st Linker Member (\"/\") expected, got \"%s\"", currentMember.Name));
			ubyte[] firstMemberData = slicer.getArrayOf!ubyte(currentMember.Size);
			slicer.advanceToAlignment(2);
			currentMember = readMemberHeader();
		}

		// 2nd member
		if (currentMember.Name == "/")
		{
			// enforce(currentMember.Name == "/", format("2nd Linker Member (\"/\") expected, got \"%s\"", currentMember.Name));
			ubyte[] secondMemberData = slicer.getArrayOf!ubyte(currentMember.Size);
			auto secondSlicer = FileDataSlicer(secondMemberData);
			uint numOfMembers = *secondSlicer.getPtrTo!uint;
			lib.objs.reserve(numOfMembers);
			lib.memberOffsets = secondSlicer.getArrayOf!uint(numOfMembers);
			uint numOfSymbols = *secondSlicer.getPtrTo!uint;
			lib.indicies = secondSlicer.getArrayOf!ushort(numOfSymbols);
			ubyte[] stringTableData = secondSlicer.fileData[secondSlicer.fileCursor..$];
			lib.stringTable = cast(string)stringTableData;
			slicer.advanceToAlignment(2);
			currentMember = readMemberHeader();
		}

		// Long names (optional)
		if (currentMember.Name == "//")
		{
			ubyte[] longNamesData = slicer.getArrayOf!ubyte(currentMember.Size);
			lib.longNames = cast(string)longNamesData;
			slicer.advanceToAlignment(2);
			currentMember = readMemberHeader();
		}

		while (true)
		{
			auto dataOffset = slicer.fileCursor;
			ubyte[] objData = slicer.getArrayOf!ubyte(currentMember.Size);
			string objName = nameFromSlashName(currentMember.Name, lib.longNames);
			//writefln("read obj %s, offset %X, length %X", objName, dataOffset, objData.length);
			lib.objs ~= PecoffObj.fromBytesLazy(objData, objName);
			slicer.advanceToAlignment(2);

			if (slicer.fileCursor >= slicer.fileData.length) break;
			currentMember = readMemberHeader();
		}

		return lib;
	}

	/// Loads all objs
	PecoffObj[] getObjs() {
		foreach (ref obj; objs) obj.load;
		return objs;
	}

	/// Ensures that obj is loaded
	ref PecoffObj getObj(size_t index) {
		objs[index].load();
		return objs[index];
	}

	void print(Sink)(auto ref Sink sink)
	{
		formattedWrite(sink, "%s:\n", filename);
		formattedWrite(sink, "  memberOffsets: %s records [%(%X, %)]\n",
			memberOffsets.length, memberOffsets);
		formattedWrite(sink, "  indicies: %s records  [%(%X, %)]\n",
			indicies.length, indicies);
		formattedWrite(sink, "\nFile table: %s files\n", objs.length);
		foreach(i, ref obj; objs) formattedWrite(sink, "% 8X  %s\n", i, obj.filename);
		sink.put("\nSymbol table:\n");
		stringTable.printStringTable(sink);
		sink.put("\n");
		foreach(i, ref obj; objs)
		{
			formattedWrite(sink, "\nOBJ# %X --------------------------------------------------\n", i);
			obj.print(sink);
		}
	}
}

struct LibMemberHeader
{
	static immutable END_OF_HEADER_VALUE = "`\n";

	align(1):
	char[16] Name;
	char[12] Date;
	char[6] UserId;
	char[6] GroupId;
	char[8] Mode;
	char[10] Size;
	char[2] EndOfHeader;

	void validate() {
		enforce(EndOfHeader == END_OF_HEADER_VALUE, format("%(%02X%) != %(%02X%)",
			cast(ubyte[])EndOfHeader[], cast(ubyte[])END_OF_HEADER_VALUE));
	}

	ParsedLibMemberHeader parse() const @safe
	{
		import std.datetime.systime : SysTime;
		ParsedLibMemberHeader res;
		res.Name = strip(Name[]);
		auto dateStripped = Date[].strip;
		if (dateStripped.length) res.Date = SysTime.fromUnixTime(to!uint(dateStripped));
		auto usedIdStripped = UserId[].strip;
		if (usedIdStripped.length) res.UserId = to!uint(usedIdStripped);
		auto groupIdStripped = UserId[].strip;
		if (groupIdStripped.length) res.GroupId = to!uint(groupIdStripped);
		auto modeStripped = Mode[].strip;
		if (modeStripped.length) res.Mode = to!uint(modeStripped, 8);
		auto sizeStripped = Size[].strip;
		res.Size = to!uint(sizeStripped);
		return res;
	}
}
static assert(LibMemberHeader.sizeof == 60);

struct ParsedLibMemberHeader
{
	import std.datetime.systime : SysTime;
	align(1):
	const(char)[] Name;
	SysTime Date;
	uint UserId;
	uint GroupId;
	uint Mode;
	uint Size;
}

struct SymbolStats
{
	static struct Key
	{
		CoffSymClass symClass;
	}

	static struct SymbolStats
	{
		size_t numSymbols;
		void visit(ref SymbolTableEntry sym)
		{
			++numSymbols;
		}
	}
	SymbolStats[Key] stats;
	size_t totalSymbols;

	void visit(ref PecoffLib lib)
	{
		foreach(ref obj; lib.getObjs()) visit(obj);
	}

	void visit(ref PecoffObj obj)
	{
		foreach (item; CoffSymbolRange(obj.symbols))
		{
			Key key = Key(item.sym.StorageClass);
			if (auto stat = key in stats) {
				stat.visit(*item.sym);
			} else {
				SymbolStats stat;
				stat.visit(*item.sym);
				stats[key] = stat;
			}
			++totalSymbols;
		}
	}

	void print(Sink)(auto ref Sink sink)
	{
		formattedWrite(sink, "Total symbols: %X\n", totalSymbols);
		sink.put("Class                   Count   \n");
		sink.put("----------------------  --------\n");
		foreach(key, stat; stats)
			formattedWrite(sink, "  % 16s(%02X)  % 8X\n", key.symClass, cast(ubyte)key.symClass, stat.numSymbols);
	}
}
void printSymbolStats(Sink)(auto ref Sink sink, ref PecoffLib lib) { SymbolStats stats; stats.visit(lib); stats.print(sink); }
void printSymbolStats(Sink)(auto ref Sink sink, ref PecoffObj obj) { SymbolStats stats; stats.visit(obj); stats.print(sink); }

struct SectionStats
{
	static struct Key
	{
		string sectionName;
		uint characteristics;
	}
	static struct SectionStats
	{
		size_t sectionsTotalSize;
		size_t numOfSections;
		void visit(ref Section section)
		{
			sectionsTotalSize += section.data.length;
			++numOfSections;
		}
	}
	SectionStats[Key] stats;
	size_t totalSymbols;

	void visit(ref PecoffLib lib)
	{
		foreach(ref obj; lib.getObjs()) visit(obj);
	}

	void visit(ref PecoffObj obj)
	{
		foreach(ref section; obj.sections) visit(section);
		totalSymbols += obj.symbols.length;
	}

	void visit(ref Section section)
	{
		Key key = Key(section.name, section.header.Characteristics);
		if (auto stat = key in stats)
		{
			stat.visit(section);
		}
		else
		{
			SectionStats stat;
			stat.visit(section);
			stats[key] = stat;
		}
	}

	void print(Sink)(auto ref Sink sink)
	{
		formattedWrite(sink, "total symbols: %X\n", totalSymbols);
		sink.put("Code|Initialized|Uninitialized|Link info|Remove\n");
		sink.put("coMdat|Gprel|Ovfl|Discardable|cacHed|Paged|Shared\n");
		sink.put("------------  ---  -----  --------  --------  --------\n");
		sink.put("    Flags                              Total          \n");
		sink.put("CIULRMGODHPS  RWX  Align     Count      Size  Name    \n");
		sink.put("------------  ---  -----  --------  --------  --------\n");
		foreach(key, stat; stats)
		{
			printSectionCharacteristicsFlags(sink, key.characteristics);
			sink.put("  ");
			printSectionCharacteristicsAlign(sink, key.characteristics);
			formattedWrite(sink, "  % 8s  % 8X  %s\n",
				stat.numOfSections, stat.sectionsTotalSize, key.sectionName);
		}
	}
}

void printSectionStats(Sink)(auto ref Sink sink, ref PecoffLib lib) { SectionStats stats; stats.visit(lib); stats.print(sink); }
void printSectionStats(Sink)(auto ref Sink sink, ref PecoffObj obj) { SectionStats stats; stats.visit(obj); stats.print(sink); }

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

/// Calculates the size of import section from DllImports
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

/// A set of slices on top of single memory buffer
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

	Arena!ubyte strSink;
	strSink.setBuffer(mapping.stringData);

	foreach(i, ref DllImports dll; mapping.importedLibs.libs)
	{
		mapping.directories[i].importLookupTableRVA = cast(uint)(ilt_rva + tableIndex * IAT_ILT_ENTRY_BYTES);
		mapping.directories[i].importAddressTableRVA = cast(uint)(iat_rva + tableIndex * IAT_ILT_ENTRY_BYTES);
		mapping.directories[i].nameRVA = cast(uint)(str_rva + strSink.length);
		//writefln("importLookupTableRVA %X", mapping.directories[i].importLookupTableRVA);
		//writefln("importAddressTableRVA %X", mapping.directories[i].importAddressTableRVA);
		//writefln("nameRVA %X", mapping.directories[i].nameRVA);

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

		foreach (Section* section; sections)
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
		coffFileHeader.TimeDateStamp = 0;
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

	void write(ref Arena!ubyte sink)
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

struct DosHeader
{
	ubyte[64] hexData = [
		0x4D, 0x5A, 0x90, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00,
		0xB8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA8, 0x00, 0x00, 0x00
	];

	void write(ref Arena!ubyte sink) {
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

	void write(ref Arena!ubyte sink) {
		sink.put(hexData);
	}
}

struct PeSignature
{
	immutable char[4] signature = "PE\0\0";
	void write(ref Arena!ubyte sink) {
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
	LINE_NUMS_STRIPPED = 0x0004,

	//  COFF symbol table entries for local symbols have been removed. This
	//  flag is deprecated and should be zero.
	LOCAL_SYMS_STRIPPED = 0x0008,

	//  Obsolete. Aggressively trim working set. This flag is deprecated for
	//  Windows 2000 and later and must be zero.
	AGGRESSIVE_WS_TRIM = 0x0010,

	/// Application can handle > 2-GB addresses.
	LARGE_ADDRESS_AWARE = 0x0020,

	//  Little endian: the least significant bit (LSB) precedes the most significant
	//  bit (MSB) in memory. This flag is deprecated and should be zero.
	BYTES_REVERSED_LO = 0x0080,

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
	BYTES_REVERSED_HI = 0x8000,
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

	size_t write(ref Arena!ubyte sink) {
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

	void print(Sink)(auto ref Sink sink)
	{
		import std.datetime.systime : SysTime;
		formattedWrite(sink, "COFF header:\n");
		formattedWrite(sink, "  Machine type: %s\n", Machine);
		formattedWrite(sink, "  Number of sections: %X\n", NumberOfSections);
		formattedWrite(sink, "  TimeDateStamp: %s\n", SysTime.fromUnixTime(TimeDateStamp));
		formattedWrite(sink, "  Pointer to symbol table: 0x%08X\n", PointerToSymbolTable);
		formattedWrite(sink, "  Number of symbols: %X\n", NumberOfSymbols);
		formattedWrite(sink, "  Size of optional header: %X\n", SizeOfOptionalHeader);
		formattedWrite(sink, "  Characteristics: 0x%04X\n", Characteristics);
		if(Characteristics) with(CoffFlags)
		{
			auto c = Characteristics;
			if(c & RELOCS_STRIPPED) formattedWrite(sink, "    Relocations stripped\n");
			if(c & EXECUTABLE_IMAGE) formattedWrite(sink, "    Executable image\n");
			if(c & LINE_NUMS_STRIPPED) formattedWrite(sink, "    Line numbers stripped\n");
			if(c & LOCAL_SYMS_STRIPPED) formattedWrite(sink, "    Local symbols stripped\n");
			if(c & AGGRESSIVE_WS_TRIM) formattedWrite(sink, "    Aggressively trim working set\n");
			if(c & LARGE_ADDRESS_AWARE) formattedWrite(sink, "    Large address aware\n");
			if(c & BYTES_REVERSED_LO) formattedWrite(sink, "    Bytes reversed low\n");
			if(c & _32BIT_MACHINE) formattedWrite(sink, "    32bit machine\n");
			if(c & DEBUG_STRIPPED) formattedWrite(sink, "    Debug stripped\n");
			if(c & REMOVABLE_RUN_FROM_SWAP) formattedWrite(sink, "    Removable run from swap\n");
			if(c & NET_RUN_FROM_SWAP) formattedWrite(sink, "    Network run from swap\n");
			if(c & SYSTEM) formattedWrite(sink, "    System\n");
			if(c & DLL) formattedWrite(sink, "    DLL\n");
			if(c & UP_SYSTEM_ONLY) formattedWrite(sink, "    Up system only\n");
			if(c & BYTES_REVERSED_HI) formattedWrite(sink, "    Bytes reversed high\n");
		}
	}
}
static assert(CoffFileHeader.sizeof == 20);

///
enum WindowsSubsystem : ushort {
	UNKNOWN = 0,                   // An unknown subsystem
	NATIVE = 1,                    // Device drivers and native Windows processes
	WINDOWS_GUI = 2,               // The Windows graphical user interface (GUI) subsystem
	WINDOWS_CUI = 3,               // The Windows character subsystem
	OS2_CUI = 5,                   // The OS/2 character subsystem
	POSIX_CUI = 7,                 // The Posix character subsystem
	NATIVE_WINDOWS = 8,            // Native Win9x driver
	WINDOWS_CE_GUI = 9,            // Windows CE
	EFI_APPLICATION = 10,          // An Extensible Firmware Interface (EFI) application
	EFI_BOOT_SERVICE_DRIVER = 11,  // An EFI driver with boot services
	EFI_RUNTIME_DRIVER = 12,       // An EFI driver with runtime services
	EFI_ROM = 13,                  // An EFI ROM image
	XBOX = 14,                     // XBOX
	WINDOWS_BOOT_APPLICATION = 16, // Windows boot application.
}


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

	size_t write(ref Arena!ubyte sink) {
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

	string getName(string stringTable)
	{
		return nameFromSlashName(Name, stringTable);
	}

	size_t write(ref Arena!ubyte sink) {
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

	static void printTableHeader(Sink)(auto ref Sink sink)
	{
		formattedWrite(sink, "Code|Initialized|Uninitialized|Link info|Remove|coMdat|Gprel|Ovfl|Discardable|cacHed|Paged|Shared\n");
		formattedWrite(sink, "----  --------  --------  --------  --------  --------  --------  -----  ------------  ---  --------\n");
		formattedWrite(sink, "      Virtual    Virtual      File      File    Relocs    Num of             Flags                  \n");
		formattedWrite(sink, "   #  Address       Size    Offset      Size    Offset    Relocs  Align  CIULRMGODHPS  RWX  Name    \n");
		formattedWrite(sink, "----  --------  --------  --------  --------  --------  --------  -----  ------------  ---  --------\n");
	}

	void print(Sink)(auto ref Sink sink, size_t index, string stringTable)
	{
		formattedWrite(sink, "% 4X  % 8X  % 8X  % 8X  % 8X  % 8X  % 8X",
			index, VirtualAddress, VirtualSize, PointerToRawData, SizeOfRawData,
			PointerToRelocations, NumberOfRelocations);
		sink.put("  ");

		// Align
		printSectionCharacteristicsAlign(sink, Characteristics);
		sink.put("  ");

		// Flags
		printSectionCharacteristicsFlags(sink, Characteristics);
		sink.put("  ");

		// Name
		formattedWrite(sink, "%s\n", getName(stringTable));
	}
}
static assert(SectionHeader.sizeof == 40);

void printSectionCharacteristicsAlign(Sink)(auto ref Sink sink, uint Characteristics)
{
	if(Characteristics & 0x00F00000) {
		size_t alignment = 1 << (((Characteristics & 0x00F00000) >> 20) - 1);
		formattedWrite(sink, "% 5s", alignment);
	} else formattedWrite(sink, "     ");
}

void printSectionCharacteristicsFlags(Sink)(auto ref Sink sink, uint Characteristics)
{
	if(Characteristics) with(SectionFlags)
	{
		void printFlag(char chr, SectionFlags flag) {
			if(Characteristics & flag) sink.put(chr);
			else sink.put(' ');
		}
		printFlag('C', SCN_CNT_CODE);
		printFlag('I', SCN_CNT_INITIALIZED_DATA);
		printFlag('U', SCN_CNT_UNINITIALIZED_DATA);
		printFlag('L', SCN_LNK_INFO);
		printFlag('R', SCN_LNK_REMOVE);
		printFlag('M', SCN_LNK_COMDAT);
		printFlag('G', SCN_GPREL);
		printFlag('O', SCN_LNK_NRELOC_OVFL);
		printFlag('D', SCN_MEM_DISCARDABLE);
		printFlag('H', SCN_MEM_NOT_CACHED);
		printFlag('P', SCN_MEM_NOT_PAGED);
		printFlag('S', SCN_MEM_SHARED);
		sink.put("  ");
		printFlag('R', SCN_MEM_READ);
		printFlag('W', SCN_MEM_WRITE);
		printFlag('X', SCN_MEM_EXECUTE);
	}
}

// Converts name that optionally refers to string table with "/n" format
string nameFromSlashName(const char[] name, string stringTable)
{
	import std.string : fromStringz;
	import std.conv : parse;
	if (name[0] == '/')
	{
		string offsetDecimalString = fromFixedString(name[1..$]);
		size_t offset = parse!size_t(offsetDecimalString);
		return fromStringz(stringTable[offset..$].ptr);
	}
	else
		return fromFixedString(name);
}

enum SymbolSectionNumber : short
{
	/// The symbol record is not yet assigned a section. A
	/// value of zero indicates that a reference to an external
	/// symbol is defined elsewhere. A value of non-zero is a
	/// common symbol with a size that is specified by the
	/// value.
	UNDEFINED = 0,

	/// The symbol has an absolute (non-relocatable) value
	/// and is not an address.
	ABSOLUTE = -1,

	/// The symbol provides general type or debugging
	/// information but does not correspond to a section.
	/// Microsoft tools use this setting along with .file
	/// records (storage class FILE).
	DEBUG = -2,
}

enum CoffSymClass : ubyte
{
	END_OF_FUNCTION = 0xFF,
	NULL = 0,
	AUTOMATIC = 1,
	EXTERNAL = 2,
	STATIC = 3,
	REGISTER = 4,
	EXTERNAL_DEF = 5,
	LABEL = 6,
	UNDEFINED_LABEL = 7,
	MEMBER_OF_STRUCT = 8,
	ARGUMENT = 9,
	STRUCT_TAG = 10,
	MEMBER_OF_UNION = 11,
	UNION_TAG = 12,
	TYPE_DEFINITION = 13,
	UNDEFINED_STATIC = 14,
	ENUM_TAG = 15,
	MEMBER_OF_ENUM = 16,
	REGISTER_PARAM = 17,
	BIT_FIELD = 18,
	BLOCK = 100,
	FUNCTION = 101,
	END_OF_STRUCT = 102,
	FILE = 103,
	SECTION = 104,
	WEAK_EXTERNAL = 105,
	CLR_TOKEN = 107,
}



/// The symbol table in this section is inherited from the traditional COFF format. It is
/// distinct from Microsoft Visual C++® debug information. A file can contain both a
/// COFF symbol table and Visual C++ debug information, and the two are kept
/// separate. Some Microsoft tools use the symbol table for limited but important
/// purposes, such as communicating COMDAT information to the linker. Section
/// names and file names, as well as code and data symbols, are listed in the symbol table.
/// The location of the symbol table is indicated in the COFF header.
/// The symbol table is an array of records, each 18 bytes long. Each record is either a
/// standard or auxiliary symbol-table record. A standard record defines a symbol or
/// name and has the following format.
struct SymbolTableEntry
{
	align(1):
	/// The name of the symbol, represented by a union
	/// of three structures. An array of 8 bytes is used if
	/// the name is not more than 8 bytes long. For more
	/// information, see section 5.4.1, “Symbol Name Representation.”
	PeSymbolName Name;

	/// The value that is associated with the symbol. The
	/// interpretation of this field depends on
	/// SectionNumber and StorageClass. A typical
	/// meaning is the relocatable address.
	uint Value;

	/// The signed integer that identifies the section,
	/// using a one-based index into the section table.
	/// Some values have special meaning, as defined in
	/// section 5.4.2, “Section Number Values.”
	short SectionNumber; /// See enum SymbolSectionNumber

	/// A number that represents type. Microsoft tools set
	/// this field to 0x20 (function) or 0x0 (not a function).
	/// For more information, see section 5.4.3, “Type
	/// Representation.”
	ushort Type;

	bool isUndefined() { return SectionNumber == SymbolSectionNumber.UNDEFINED; }
	bool isAbsolute() { return SectionNumber == SymbolSectionNumber.ABSOLUTE; }
	bool isDebug() { return SectionNumber == SymbolSectionNumber.DEBUG; }
	bool isFunction() { return Type == 0x20; }
	bool isNonFunction() { return Type == 0x0; }

	/// An enumerated value that represents storage
	/// class. For more information, see section 5.4.4,
	/// “Storage Class.”
	CoffSymClass StorageClass;

	/// The number of auxiliary symbol table entries that
	/// follow this record.
	ubyte NumberOfAuxSymbols;

	void print(Sink)(auto ref Sink sink, string stringTable)
	{
		formattedWrite(sink, "  Name: %s\n", Name.get(stringTable));
		//formattedWrite(sink, "  Name: %s %s\n", Name.Zeroes, Name.Offset);
		formattedWrite(sink, "    Value: %08X\n", Value);
		switch(SectionNumber)
		{
			case -2: formattedWrite(sink, "    Section: Debug\n"); break;
			case -1: formattedWrite(sink, "    Section: Absolute\n"); break;
			case  0: formattedWrite(sink, "    Section: Undefined\n"); break;
			default: formattedWrite(sink, "    Section: %s\n", SectionNumber);
		}
		formattedWrite(sink, "    Type: %s\n", Type);
		formattedWrite(sink, "    Storage class: %s\n", StorageClass);
		formattedWrite(sink, "    Number of aux symbols: %s\n", NumberOfAuxSymbols);
	}
}
static assert(SymbolTableEntry.sizeof == 18);

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

	size_t write(ref Arena!ubyte sink) {
		auto offset = sink.length;
		sink.put(this);
		return offset;
	}
}

struct ImportLookupTable
{
	ImportLookupEntry[] entries;
	size_t write(ref Arena!ubyte sink) {
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
	size_t write(ref Arena!ubyte sink) {
		auto offset = sink.length;
		sink.put(this);
		return offset;
	}
}

struct HintNameTable
{
	HintNameEntry[] entries;

	size_t write(ref Arena!ubyte sink) {
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

	size_t write(ref Arena!ubyte sink) {
		auto offset = sink.length;

		sink.put(Hint);
		sink.writeStringAligned2(Name);

		return offset;
	}
}

void writeStringAligned2(ref Arena!ubyte sink, string str)
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

struct PeSymbolName
{
	union
	{
		char[8] ShortName;
		struct {
			uint Zeroes;
			uint Offset; // includes 4 size bytes at the beginning of table
		}
	}

	string get(string stringTable)
	{
		import std.string : fromStringz;
		if (Zeroes == 0)
			return fromStringz(stringTable[Offset-4..$].ptr);
		else
			return fromFixedString(ShortName);
	}
}

string fromFixedString(const char[] fixedStr) {
	foreach_reverse(i, chr; fixedStr)
		if (chr != '\0') return cast(string)fixedStr[0..i+1];
	return null;
}

string[] IMAGE_REL_AMD64_names = ["ABSOLUTE","ADDR64","ADDR32","ADDR32NB","REL32","REL32_1",
	"REL32_2", "REL32_3", "REL32_4", "REL32_5", "SECTION", "SECREL", "SECREL7",
	"TOKEN", "SREL32", "PAIR", "SSPAN32"];

enum IMAGE_REL_AMD64 : ushort {
	ABSOLUTE = 0x00, /// The relocation is ignored.
	ADDR64 = 0x01, /// The 64-bit VA of the relocation target.
	ADDR32 = 0x02, /// The 32-bit VA of the relocation target.
	ADDR32NB = 0x03, /// The 32-bit address without an image base (RVA).
	REL32 = 0x04, /// The 32-bit relative address from the byte following the relocation.
	REL32_1 = 0x05, /// The 32-bit address relative to byte distance 1 from the relocation.
	REL32_2 = 0x06, /// The 32-bit address relative to byte distance 2 from the relocation.
	REL32_3 = 0x07, /// The 32-bit address relative to byte distance 3 from the relocation.
	REL32_4 = 0x08, /// The 32-bit address relative to byte distance 4 from the relocation.
	REL32_5 = 0x09, /// The 32-bit address relative to byte distance 5 from the relocation.
	SECTION = 0x0A, /// The 16-bit section index of the section that contains the target. This is used to support debugging information.
	SECREL = 0x0B, /// The 32-bit offset of the target from the beginning of its section. This is used to support debugging information and static thread local storage.
	SECREL7 = 0x0C, /// A 7-bit unsigned offset from the base of the section that contains the target.
	TOKEN = 0x0D, /// CLR tokens.
	SREL32 = 0x0E, /// A 32-bit signed span-dependent value emitted into the object.
	PAIR = 0x0F, /// A pair that must immediately follow every span-dependent value.
	SSPAN32 = 0x10 /// A 32-bit signed span-dependent value that is applied at link time.
}

ubyte[] relTargetSizeTable = [0,64,32,32,32,32,32,32,32,32,16,32,7,0,32,0,32];

struct CoffRelocation
{
	align(1):
	/// The address of the item to which relocation is
	/// applied. This is the offset from the beginning of the
	/// section, plus the value of the section’s RVA/Offset
	/// field. See section 4, “Section Table (Section
	/// Headers).” For example, if the first byte of the
	/// section has an address of 0x10, the third byte has
	/// an address of 0x12.
	uint VirtualAddress;

	/// A zero-based index into the symbol table. This
	/// symbol gives the address that is to be used for the
	/// relocation. If the specified symbol has section
	/// storage class, then the symbol’s address is the
	/// address with the first section of the same name.
	uint SymbolTableIndex;

	/// A value that indicates the kind of relocation that
	/// should be performed. Valid relocation types
	/// depend on machine type. See section 5.2.1, “Type
	/// Indicators.
	ushort Type;

	size_t targetSize()
	{
		return relTargetSizeTable[Type];
	}

	string typeString()
	{
		return IMAGE_REL_AMD64_names[Type];
	}
}
static assert(CoffRelocation.sizeof == 10);

struct Section
{
	uint sectionId; // 0 based index
	// Can be slice of SectionHeader.Name, slice of String table or separate string
	// In the latter case, header.Name needs to be set before writing to the file.
	string name;
	SectionHeader header;
	CoffRelocation[] relocations;
	ubyte[] data;
	SymbolSectionInfo[] symbols;

	// string name() { return fromFixedString(header.Name); }

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
		return name == ".text";
	}

	bool isImportSection()
	{
		return name == ".idata";
	}

	bool isLinkDirective()
	{
		return header.Characteristics & SectionFlags.SCN_LNK_INFO && name == ".drectve";
	}
}
