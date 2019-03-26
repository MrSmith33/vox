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
	string name;
	void* ptr;
}

struct DllModule
{
	string libName;
	string[] importedSymbols;
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
	isMutable = 1 << 0,
	isAllZero = 1 << 1,
	needsZeroTermination = 1 << 2,
	/// If set calls use indirect call form
	/// symbol represents not a data but pointer to data
	isIndirect  = 1 << 3,
	/// If true, data can be printed for debug as a string
	isString    = 1 << 4,
	/// Marked if transitively used from any root symbol (not yet. TODO)
	isReferenced      = 1 << 5,
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
	///
	Identifier id;
	/// Points to initializer if it is provided. (Can be null)
	ubyte* dataPtr;
	/// Offset from the start of section. Is equal to dataPtr if host symbol
	ulong sectionOffset;
	/// Length in bytes. Doesn't include padding and zero termination
	uint length;
	/// Symbol is inside this module
	LinkIndex moduleIndex;
	/// Symbol is inside this section
	LinkIndex sectionIndex;
	/// How symbol must be aligned
	uint alignment;
	/// List of references coming from this symbol
	LinkIndex firstRef;
	/// List of module symbols
	LinkIndex nextSymbol;

	void markReferenced() { flags |= ObjectSymbolFlags.isReferenced; }

	bool isMutable() { return cast(bool)(flags & ObjectSymbolFlags.isMutable); }
	bool isAllZero() { return cast(bool)(flags & ObjectSymbolFlags.isAllZero); }
	bool needsZeroTermination() { return cast(bool)(flags & ObjectSymbolFlags.needsZeroTermination); }
	bool isIndirect() { return cast(bool)(flags & ObjectSymbolFlags.isIndirect); }
	bool isString() { return cast(bool)(flags & ObjectSymbolFlags.isString); }
	bool isReferenced() { return cast(bool)(flags & ObjectSymbolFlags.isReferenced); }
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
	/// Used for referencing dll modules in import table
	Identifier id;
	/// Linked list of modules
	LinkIndex nextModule;
	/// Linked list of symbols
	LinkIndex firstSymbol;

	bool isImported() { return kind == ObjectModuleKind.isImported; }
}

@(LinkIndexKind.section)
struct ObjectSection
{
	/// In JIT mode: absolute address
	/// In exe mode: offset from executable start to the section start after loading
	ulong sectionAddress;
	/// In JIT mode is equal to sectionAddress
	/// Points to the data of this section. Used to perform fixups
	ubyte* sectionData;
	/// Total
	uint length;
	///
	uint alignment;
	///
	LinkIndex moduleIndex;
	///
	Identifier id;
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
	///
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
		T* type = cast(T*)buffer.voidPut(numAllocatedSlots).ptr;
		*type = value;

		static if (is(T == ObjectSymbolReference))
		{
			ObjectSymbol* sym = &getSymbol(type.fromSymbol);
			type.nextReference = sym.firstRef;
			sym.firstRef = result;
			getSymbol(type.referencedSymbol).markReferenced;
		}
		else static if (is(T == ObjectModule))
		{
			type.nextModule = firstModule;
			firstModule = result;
		}
		else static if (is(T == ObjectSymbol))
		{
			ObjectModule* mod = &getModule(type.moduleIndex);
			type.nextSymbol = mod.firstSymbol;
			mod.firstSymbol = result;
		}
		//writefln("add %s %s", result.kind, result.bufferIndex);

		return result;
	}

	alias getSymbol = get!ObjectSymbol;
	alias getSection = get!ObjectSection;
	alias getModule = get!ObjectModule;
	alias getReference = get!ObjectSymbolReference;

	ref T get(T)(LinkIndex index)
	{
		assert(index.isDefined, "null index");
		assert(index.kind == getLinkIndexKind!T, format("%s != %s", index.kind, getLinkIndexKind!T));
		return *cast(T*)(&buffer.bufPtr[index.bufferIndex]);
	}

	void dump(CompilationContext* context)
	{
		LinkIndex modIndex = firstModule;
		while (modIndex.isDefined)
		{
			ObjectModule* mod = &getModule(modIndex);
			writefln("%s %s", modIndex, context.idString(mod.id));

			LinkIndex symIndex = mod.firstSymbol;
			while (symIndex.isDefined)
			{
				ObjectSymbol* sym = &getSymbol(symIndex);
				writef("  %s %s", symIndex, context.idString(sym.id));
				if (sym.isString)
					writefln(` "%s"`, (cast(char*)(sym.dataPtr))[0..sym.length]);
				else writeln;

				LinkIndex symRefIndex = sym.firstRef;
				while (symRefIndex.isDefined)
				{
					ObjectSymbolReference* symRef = &getReference(symRefIndex);
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
}
