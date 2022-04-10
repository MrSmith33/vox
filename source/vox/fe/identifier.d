/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.identifier;

import std.stdio;
import vox.utils : Arena, HashMap, TextSink;
import vox.context;

// Fully qualified identifier of the form `package.module.entity_name`
// Must contain at least `entity_name`
struct Identifier {
	uint index = uint.max;
	enum uint HIGH_BIT = 0x8000_0000;

	uint strIndex() {
		assert(!hasParent);
		return index;
	}
	uint fqnIndex() {
		assert(hasParent);
		return index & ~HIGH_BIT;
	}

	bool isDefined() { return index != uint.max; }
	bool isUndefined() { return index == uint.max; }
	bool hasParent() { return cast(bool)(index & HIGH_BIT); }

	Identifier getParent(CompilationContext* c) {
		assert(isDefined);
		assert(hasParent);
		return c.idMap.entries[fqnIndex].fqn.parentId;
	}

	Identifier getSelf(CompilationContext* c) {
		assert(isDefined);
		if (hasParent) return c.idMap.entries[fqnIndex].fqn.id;
		return this;
	}

	auto pr(CompilationContext* c) {
		return IdentifierPrinter(this, c);
	}

	void visitPrint(CompilationContext* c, scope void delegate(const(char)[]) sink) {
		if (!hasParent) {
			sink(c.idMap.get(this));
			return;
		}
		if (isUndefined) {
			sink("<undefined id>");
			return;
		}
		auto entry = c.idMap.entries[fqnIndex].fqn;
		entry.parentId.visitPrint(c, sink);
		sink(".");
		sink(c.idMap.get(entry.id));
	}
}

struct IdentifierPrinter {
	Identifier id;
	CompilationContext* c;
	void toString(scope void delegate(const(char)[]) sink) { id.visitPrint(c, sink); }
}

struct IdentifierMap {
	Arena!char stringDataBuffer;
	Arena!IdMapEntry entries;
	HashMap!(StringKey, uint, StringKey.init) map;
	HashMap!(FullyQualifiedName, uint, FullyQualifiedName.init) fqnMap;

	string get(Identifier id) {
		if (id.isDefined) {
			if (id.hasParent) {
				id = entries[id.fqnIndex].fqn.id;
			}
			auto str = entries[id.strIndex].str;
			return cast(string)stringDataBuffer.bufPtr[str.offset..str.offset+str.length];
		}
		return "<undefined id>";
	}

	Identifier find(const(char)[] str) {
		assert(str.length > 0);
		auto key = StringKey(str);
		return Identifier(map.get(key, uint.max));
	}

	Identifier getOrRegFormatted(Args...)(CompilationContext* c, const(char)[] fmt, Args args) {
		assert(fmt.length > 0);
		import std.format : formattedWrite;
		auto start = stringDataBuffer.length;
		formattedWrite(stringDataBuffer, fmt, args);
		auto end = stringDataBuffer.length;

		assert(end < uint.max);
		assert(end-start < uint.max);
		auto len = cast(uint)(end-start);
		assert(len > 0);
		assert(len <= uint.max);

		const(char)[] str = stringDataBuffer.bufPtr[start..end];
		auto key = StringKey(str);
		uint id = map.get(key, uint.max);

		if (id == uint.max) {
			// can use lower 31 bits
			assert(entries.length < Identifier.HIGH_BIT, "Id map overflow");
			id = cast(uint)entries.length;
			map.put(c.arrayArena, key, id);

			entries.put(IdMapEntry(IdMapString(cast(uint)start, len)));
		} else {
			// this is old id, remove data from buffer
			stringDataBuffer.length = start;
		}

		return Identifier(id);
	}

	Identifier getOrReg(CompilationContext* c, const(char)[] str) {
		assert(str.length > 0);
		assert(str.length <= uint.max);

		auto key = StringKey(str);
		uint id = map.get(key, uint.max);

		if (id == uint.max) {
			auto start = stringDataBuffer.length;
			char[] buf = stringDataBuffer.put(str);
			auto end = stringDataBuffer.length;

			auto len = cast(uint)(end-start);
			assert(len > 0);
			assert(len <= uint.max);

			key.ptr = buf.ptr; // set new ptr so that buf data is always used for compare

			// can use lower 31 bits
			assert(entries.length < Identifier.HIGH_BIT, "Id map overflow");
			id = cast(uint)entries.length;
			map.put(c.arrayArena, key, id);
			entries.put(IdMapEntry(IdMapString(cast(uint)start, len)));
		}

		return Identifier(id);
	}

	Identifier getOrRegFqn(CompilationContext* c, FullyQualifiedName key) {
		assert(key.id.isDefined);
		if (key.parentId.isUndefined) {
			return key.id;
		}

		uint id = fqnMap.get(key, uint.max);

		if (id == uint.max) {
			assert(entries.length < Identifier.HIGH_BIT, "Id map overflow");
			id = cast(uint)entries.length;
			fqnMap.put(c.arrayArena, key, id);
			entries.put(IdMapEntry(key));
		}

		return Identifier(id | Identifier.HIGH_BIT);
	}

	// registers dot-separated identifier
	Identifier getOrRegFqn(CompilationContext* c, const(char)[] str) {
		import std.algorithm : splitter;

		assert(str.length > 0);

		Identifier parentId;

		foreach(const(char)[] part; str.splitter('.'))
		{
			Identifier partId = getOrReg(c, part);
			parentId = getOrRegFqn(c, FullyQualifiedName(parentId, partId));
		}

		return parentId;
	}

	// internal. Called in CompilationContext.initialize()
	void regCommonIds(CompilationContext* c)
	{
		assert(entries.length == 0);
		assert(map.length == 0);
		assert(stringDataBuffer.length == 0);
		foreach (size_t i, string memberName; __traits(allMembers, CommonIds))
		{
			string name = __traits(getAttributes, __traits(getMember, CommonIds, memberName))[0];
			Identifier id = getOrReg(c, name);
			assert(id == __traits(getMember, CommonIds, memberName));
		}
	}
}

enum CommonIds : Identifier
{
	@("ptr")      id_ptr      = Identifier(0),
	@("length")   id_length   = Identifier(1),
	@("min")      id_min      = Identifier(2),
	@("max")      id_max      = Identifier(3),
	@("sizeof")   id_sizeof   = Identifier(4),
	@("offsetof") id_offsetof = Identifier(5),
	@("this")     id_this     = Identifier(6),
	@("message")  id_message  = Identifier(7),
	@("type")     id_type     = Identifier(8),
	@("extern")   id_extern   = Identifier(9),
	@("static")   id_static   = Identifier(10),
	@("syscall")  id_syscall  = Identifier(11),
	@("module")   id_module   = Identifier(12),
	@("host")     id_host     = Identifier(13),
	@("main")     id_main     = Identifier(14),

	// Built-in function identifiers
	@("$compileError") cash_compile_error = Identifier(id_main.index + 1),
	@("$isSlice") cash_is_slice = Identifier(id_main.index + 2),
	@("$isInteger") cash_is_integer = Identifier(id_main.index + 3),
	@("$isPointer") cash_is_pointer = Identifier(id_main.index + 4),
	@("$baseOf") cash_base_of = Identifier(id_main.index + 5),

	// Special keywords
	@("__FILE__")          kw_file          = Identifier(cash_base_of.index + 1),
	@("__LINE__")          kw_line          = Identifier(cash_base_of.index + 2),
	@("__FUNCTION_NAME__") kw_function_name = Identifier(cash_base_of.index + 3),
	@("__MODULE_NAME__")   kw_module_name   = Identifier(cash_base_of.index + 4),

	// Conditional compilation identifiers
	@("windows")  id_windows = Identifier(kw_module_name.index + 1),
	@("linux")    id_linux   = Identifier(kw_module_name.index + 2),
	@("macos")    id_macos   = Identifier(kw_module_name.index + 3),
}

enum uint commonId_builtin_func_first = CommonIds.cash_compile_error.index;
enum uint commonId_builtin_func_last  = CommonIds.cash_base_of.index;
enum uint commonId_special_keyword_first = CommonIds.kw_file.index;
enum uint commonId_special_keyword_last  = CommonIds.kw_module_name.index;
enum uint commonId_version_id_first = CommonIds.id_windows.index;
enum uint commonId_version_id_last  = CommonIds.id_macos.index;


enum SpecialKeyword : ubyte {
	file,          // __FILE__
	line,          // __LINE__
	function_name, // __FUNCTION_NAME__
	module_name,   // __MODULE_NAME__
}

enum VersionId : ubyte {
	id_windows,
	id_linux,
	id_macos,
}


private struct StringKey
{
	const(char)* ptr;
	uint length;
	uint hash;

	this(const(char)[] str)
	{
		ptr = str.ptr;
		length = cast(uint)str.length;
		hash = fnv1a_32(cast(const(ubyte)[])str);
	}

	string data() const
	{
		return cast(string)ptr[0..length];
	}

	bool opEquals(StringKey other) const
	{
		if (hash != other.hash) return false;
		return this.data == other.data;
	}

	size_t toHash()
	{
		return hash;
	}
}

private struct IdMapString {
	uint offset;
	uint length;
}
struct FullyQualifiedName {
	Identifier parentId;
	Identifier id;
}
private union IdMapEntry {
	this(IdMapString str) { this.str = str; }
	this(FullyQualifiedName fqn) { this.fqn = fqn; }
	IdMapString str;
	FullyQualifiedName fqn;
}

// https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function
private uint fnv1a_32(const(ubyte)[] data)
{
	enum uint fnvPrime       = 0x01000193;
	enum uint fnvOffsetBasis = 0x811c9dc5;

	uint _hash = fnvOffsetBasis;

	foreach (immutable ubyte i; data)
	{
		_hash ^= i;
		_hash *= fnvPrime;
	}

	return _hash;
}
