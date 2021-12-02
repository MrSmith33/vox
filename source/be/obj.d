/**
Copyright: Copyright (c) 2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module be.obj;

import std.bitmanip : bitfields;
import std.format;
import std.stdio;
import std.traits : getUDAs;

import all;

struct HostSymbol
{
	this(string symName, void* ptr, string modName = "host") {
		this.symName = symName;
		this.modName = modName;
		this.ptr = ptr;
	}
	string symName;
	string modName;
	void* ptr;
}

struct ExternalSymbolId
{
	Identifier modId;
	Identifier symId;
}

enum getLinkIndexKind(T) = getUDAs!(T, LinkIndexKind)[0];

enum LinkIndexKind : ubyte {
	none,
	symbol,
	section,
	module_,
	reference,
}

struct LinkIndex
{
	///
	this(uint _index, LinkIndexKind _kind)
	{
		bufferIndex = _index;
		kind = _kind;
	}

	void toString(scope void delegate(const(char)[]) sink) const {
		if (asUint == 0) {
			sink("<null>");
			return;
		}

		final switch(kind) with(LinkIndexKind) {
			case none: sink("<none>"); break;
			case symbol: sink.formattedWrite("sym.%s", bufferIndex); break;
			case section: sink.formattedWrite("sec.%s", bufferIndex); break;
			case module_: sink.formattedWrite("mod.%s", bufferIndex); break;
			case reference: sink.formattedWrite("ref.%s", bufferIndex); break;
		}
	}

	union
	{
		mixin(bitfields!(
			uint,         "bufferIndex",  28,
			LinkIndexKind,       "kind",   4,
		));

		// is 0 for undefined index
		uint asUint;
	}

	bool isDefined() { return asUint != 0; }

	bool isSymbol() { return kind == LinkIndexKind.symbol; }
	bool isSection() { return kind == LinkIndexKind.section; }
	bool isModule() { return kind == LinkIndexKind.module_; }
	bool isReference() { return kind == LinkIndexKind.reference; }
}

enum ObjectSymbolFlags : ushort {
	isMutable            = 1 << 0,
	isAllZero            = 1 << 1,
	needsZeroTermination = 1 << 2,
	/// If set calls use indirect call form
	/// symbol represents not a data but pointer to data
	isIndirect           = 1 << 3,
	/// If true, data can be printed for debug as a string
	isString             = 1 << 4,
	/// If true, data can be printed for debug as a float
	isFloat              = 1 << 5,
	/// Marked if transitively used from any root symbol (only used for imported symbols atm)
	isReferenced         = 1 << 6,
}

enum ObjectSymbolKind : ushort {
	/// We have it's contents
	isLocal,
	/// Symbol comes from dll
	isImported,
	/// Symbol comes from host
	isHost,
}

/// Final data is located at ObjectSection.sectionData + ObjectSymbol.sectionOffset
@(LinkIndexKind.symbol)
struct ObjectSymbol
{
	///
	ObjectSymbolKind kind;
	/// Set of ObjectSymbolFlags
	ushort flags;
	/// How symbol must be aligned
	ubyte alignmentPower = 0;
	///
	Identifier id;
	/// Points to initializer if it is provided. (Can be null)
	ubyte* dataPtr;
	/// Offset from the start of section. Is equal to dataPtr if host symbol
	ulong sectionOffset;
	/// Length in bytes. Doesn't include padding and zero termination
	/// Is set in setInitializer (when has initializer), or manually (when zero inited, or is external host symbol)
	uint length;
	/// Symbol is inside this module
	LinkIndex moduleIndex;
	/// Symbol is inside this section
	LinkIndex sectionIndex;
	/// List of references coming from this symbol
	LinkIndex firstRef;
	/// List of module symbols
	LinkIndex nextSymbol;

	void markReferenced() { flags |= ObjectSymbolFlags.isReferenced; }

	uint alignment() { return 1 << cast(uint)alignmentPower; }

	bool isMutable() { return cast(bool)(flags & ObjectSymbolFlags.isMutable); }
	bool isAllZero() { return cast(bool)(flags & ObjectSymbolFlags.isAllZero); }
	bool needsZeroTermination() { return cast(bool)(flags & ObjectSymbolFlags.needsZeroTermination); }
	bool isIndirect() { return cast(bool)(flags & ObjectSymbolFlags.isIndirect); }
	bool isString() { return cast(bool)(flags & ObjectSymbolFlags.isString); }
	bool isReferenced() { return cast(bool)(flags & ObjectSymbolFlags.isReferenced); }

	void setInitializer(ubyte[] data) {
		dataPtr = data.ptr;
		assert(data.length <= 1024UL*1024*1024*1, "initializer is bigger than 1GB");
		length = cast(uint)data.length;
	}
	ubyte[] initializer() {
		if (dataPtr is null) return null;
		return dataPtr[0..length];
	}
}

enum ObjectModuleKind : ubyte {
	isLocal,
	isImported,
	isHost
}

@(LinkIndexKind.module_)
struct ObjectModule
{
	///
	ObjectModuleKind kind;
	/// Set of ObjectModuleFlags
	ushort flags;
	/// Used for referencing dll modules in import table
	Identifier id;
	/// Linked list of modules
	LinkIndex nextModule;
	/// Linked list of symbols
	LinkIndex firstSymbol;

	void markReferenced() { flags |= ObjectModuleFlags.isReferenced; }

	bool isLocal() { return kind == ObjectModuleKind.isLocal; }
	bool isImported() { return kind == ObjectModuleKind.isImported; }
	bool isExternal() { return isLocal || isImported; }
}

enum ObjectModuleFlags : ushort {
	/// Marked if transitively used from any root symbol (only used for imported symbols atm)
	isReferenced = 1 << 0,
}

@(LinkIndexKind.section)
struct ObjectSection
{
	/// In JIT mode: absolute address
	/// In exe mode: offset from executable start to the section start after loading (in memory)
	ulong sectionAddress;
	/// Can be null
	/// Storage for appending data to this section
	/// In JIT mode `buffer.bufPtr` is equal to sectionAddress
	/// Points to the data of this section. Used to perform fixups
	/// Length of initialized data is in `buffer.length`
	Arena!ubyte* buffer;
	/// Length of zero-initialized data (not included into `length`)
	uint zeroDataLength;
	///
	ulong totalLength() {
		if (!buffer) return zeroDataLength;
		return buffer.length + zeroDataLength;
	}
	///
	ubyte alignmentPower;
	///
	ObjectSectionType type;
	/// set of ObjectSectionFlags
	ushort flags;
	///
	Identifier id;

	uint alignment() { return 1 << cast(uint)alignmentPower; }
	bool flag_read() { return (flags & ObjectSectionFlags.read) != 0;}
	bool flag_write() { return (flags & ObjectSectionFlags.write) != 0;}
	bool flag_execute() { return (flags & ObjectSectionFlags.execute) != 0;}
}

enum ObjectSectionType : ubyte {
	host,    // section for host symbols
	code,    // executable code
	imports, // import section
	rw_data, // rw data section
	ro_data, // r data section
}
enum NUM_BUILTIN_SECTIONS = ObjectSectionType.max+1;

enum ObjectSectionFlags : ushort {
	none = 0,
	read = 1,
	write = 2,
	execute = 4,
}

enum ObjectSymbolRefKind : ubyte {
	/// 64 bits
	absolute64,
	/// 32 bit relative offset
	relative32,
}

/// Represents numeric reference contained inside 'fromSymbol'
@(LinkIndexKind.reference)
struct ObjectSymbolReference
{
	/// TODO: not needed. We get to references through `fromSymbol` already
	LinkIndex fromSymbol;
	///
	LinkIndex referencedSymbol;
	/// link to next reference coming from 'fromSymbol'
	LinkIndex nextReference;
	/// Offset from start of 'fromSymbol' to the reference to 'referencedSymbol'
	uint refOffset;
	/// Extra offset added to the reference inside 'fromSymbol'
	/// For example on x86_64 direct RIP-relative call instruction is 0xE8 0xNN 0xNN 0xNN 0xNN
	/// where 0xNN 0xNN 0xNN 0xNN is offset between address of next instruction and callee.
	/// Fixup address is (fromSymbol_address + refOffset)
	/// Fixup offset is calculated as (referencedSymbol_address - (fromSymbol_address + refOffset + extraOffset))
	/// In call example extraOffset = 4

	// TODO: store extra offset inside memory being fixed
	short extraOffset;
	/// Describes type of reference and its size in bytes
	ObjectSymbolRefKind refKind;
}

struct ObjectSymbolTable
{
	Arena!uint buffer;
	LinkIndex firstModule;

	alias addSymbol = append!ObjectSymbol;
	alias addSection = append!ObjectSection;
	alias addModule = append!ObjectModule;
	alias addReference = append!ObjectSymbolReference;

	LinkIndex append(T)(ref T value)
	{
		LinkIndex result;
		result.bufferIndex = cast(uint)buffer.length;
		result.kind = getLinkIndexKind!T;

		enum numAllocatedSlots = divCeil(T.sizeof, uint.sizeof);
		T* item = cast(T*)buffer.voidPut(numAllocatedSlots).ptr;
		*item = value;

		static if (is(T == ObjectSymbolReference))
		{
			ObjectSymbol* sym = getSymbol(item.fromSymbol);
			item.nextReference = sym.firstRef;
			sym.firstRef = result;
			getSymbol(item.referencedSymbol).markReferenced;
		}
		else static if (is(T == ObjectModule))
		{
			item.nextModule = firstModule;
			firstModule = result;
		}
		else static if (is(T == ObjectSymbol))
		{
			ObjectModule* mod = getModule(item.moduleIndex);
			item.nextSymbol = mod.firstSymbol;
			mod.firstSymbol = result;
		}
		//writefln("add %s %s", result.kind, result.bufferIndex);

		return result;
	}

	alias getSymbol = get!ObjectSymbol;
	alias getSection = get!ObjectSection;
	alias getModule = get!ObjectModule;
	alias getReference = get!ObjectSymbolReference;

	T* get(T)(LinkIndex index)
	{
		assert(index.isDefined, "null index");
		assert(index.kind == getLinkIndexKind!T, format("%s != %s", index.kind, getLinkIndexKind!T));
		return cast(T*)(&buffer.bufPtr[index.bufferIndex]);
	}

	void dump(CompilationContext* context)
	{
		LinkIndex modIndex = firstModule;
		while (modIndex.isDefined)
		{
			ObjectModule* mod = getModule(modIndex);
			writefln("%s %s", modIndex, context.idString(mod.id));

			LinkIndex symIndex = mod.firstSymbol;
			while (symIndex.isDefined)
			{
				ObjectSymbol* sym = getSymbol(symIndex);
				writef("  %s %s %s bytes", symIndex, context.idString(sym.id), sym.length);
				if (sym.isAllZero) write(" zeroinit");
				if (sym.isString)
					writefln(` "%s"`, (cast(char*)(sym.dataPtr))[0..sym.length]);
				else writeln;

				LinkIndex symRefIndex = sym.firstRef;
				while (symRefIndex.isDefined)
				{
					ObjectSymbolReference* symRef = getReference(symRefIndex);
					writefln("    %s -> %s: off 0x%X extra %s %s",
						symRefIndex, symRef.referencedSymbol, symRef.refOffset,
						symRef.extraOffset, symRef.refKind);
					symRefIndex = symRef.nextReference;
				}
				symIndex = sym.nextSymbol;
			}
			modIndex = mod.nextModule;
		}
	}

	// prints function label JSON for .dd64 database file of x64dbg debugger
	void print_dd64_debug_info(CompilationContext* context)
	{
		LinkIndex modIndex = firstModule;
		while (modIndex.isDefined)
		{
			ObjectModule* mod = getModule(modIndex);
			if (mod.isLocal)
			{
				LinkIndex symIndex = mod.firstSymbol;
				while (symIndex.isDefined)
				{
					ObjectSymbol* sym = getSymbol(symIndex);
					ObjectSection* section = getSection(sym.sectionIndex);

					// it is a function
					if (sym.sectionIndex == context.builtinSections[ObjectSectionType.code])
					{
						writefln("  {");
						writefln("   \"module\": \"%s\",", context.outputFilename);
						writefln("   \"address\": \"0x%X\",", section.sectionAddress + sym.sectionOffset);
						writefln("   \"manual\": true,");
						//writefln("   \"text\": \"%s.%s\"", context.idString(mod.id), context.idString(sym.id));
						writefln("   \"text\": \"%s\"", context.idString(sym.id));
						writefln("  },");
						//writefln("  0x%X %s.%s", section.sectionAddress + sym.sectionOffset, context.idString(mod.id), context.idString(sym.id));
					}

					symIndex = sym.nextSymbol;
				}
			}

			modIndex = mod.nextModule;
		}
	}
}
