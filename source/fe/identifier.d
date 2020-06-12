/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.identifier;

import std.stdio;
import utils : Arena, HashMap, TextSink;
import context;

struct Identifier {
	uint index = uint.max;
	bool isDefined() { return index != uint.max; }
	bool isUndefined() { return index == uint.max; }
}

struct IdentifierMap {
	Arena!char stringDataBuffer;
	Arena!(const(char)[]) strings;
	HashMap!(StringKey, uint, StringKey.init) map;

	TextSink tempBuf;

	string get(Identifier id) {
		if (id.isDefined) return strings[id.index];
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
		tempBuf.clear;
		tempBuf.putf(fmt, args);
		const(char)[] idString = tempBuf.data.data;
		return getOrReg(c, idString);
	}

	Identifier getOrReg(CompilationContext* c, const(char)[] str) {
		assert(str.length > 0);
		auto key = StringKey(str);
		uint id = map.get(key, uint.max);
		if (id == uint.max) {
			char[] buf = stringDataBuffer.voidPut(str.length);
			buf[] = str;
			string duppedKey = cast(string)buf;
			key.ptr = duppedKey.ptr; // set new ptr so that buf data is always used for compare
			// can't use .max, because it marks null ids
			assert(strings.length < uint.max, "Id map overflow");
			id = cast(uint)strings.length;
			map.put(c.arrayArena, key, id);
			//writefln("getOrReg %s %s", id, duppedKey);
			strings.put(duppedKey);
		}
		return Identifier(id);
	}

	Identifier getOrRegNoDup(CompilationContext* c, const(char)[] str) {
		assert(str.length > 0);
		auto key = StringKey(str);
		uint id = map.get(key, uint.max);
		if (id == uint.max) {
			id = cast(uint)strings.length;
			map.put(c.arrayArena, key, id);
			//writefln("getOrRegNoDup %s %s", id, str);
			strings.put(str);
		}
		return Identifier(id);
	}

	// internal. Called in CompilationContext.initialize()
	void regCommonIds(CompilationContext* c)
	{
		assert(strings.length == 0);
		assert(map.length == 0);
		assert(stringDataBuffer.length == 0);
		foreach (size_t i, string memberName; __traits(allMembers, CommonIds))
		{
			string name = __traits(getAttributes, __traits(getMember, CommonIds, memberName))[0];
			getOrRegNoDup(c, name);
		}
	}
}

enum CommonIds : Identifier
{
	@("ptr")     id_ptr     = Identifier(0),
	@("length")  id_length  = Identifier(1),
	@("min")     id_min     = Identifier(2),
	@("max")     id_max     = Identifier(3),
	@("sizeof")  id_sizeof  = Identifier(4),
	@("this")    id_this    = Identifier(5),
	@("message") id_message = Identifier(6),

	@("$compile_error") cash_compile_error = Identifier(id_message.index + 1),
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
