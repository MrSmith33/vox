/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.identifier;

import std.stdio;
import utils : Buffer, Arena;
import context;

struct Identifier {
	uint index = uint.max;
	bool isDefined() { return index != uint.max; }
	bool isUndefined() { return index == uint.max; }
}

struct IdentifierMap {
	Arena!char stringDataBuffer;
	Arena!(const(char)[]) strings;
	uint[string] map;

	Buffer!char tempBuf;

	string get(Identifier id) {
		if (id.isDefined) return strings[id.index];
		return "<undefined id>";
	}

	Identifier find(const(char)[] str) {
		assert(str.length > 0);
		return Identifier(map.get(cast(string)str, uint.max));
	}

	Identifier getOrRegWithSuffix(const(char)[] str, size_t suffix) {
		assert(str.length > 0);
		import std.format : formattedWrite;
		tempBuf.clear;
		tempBuf.put(cast(string)str);
		formattedWrite(tempBuf, "%s", suffix);
		const(char)[] idString = tempBuf.data;
		return getOrReg(idString);
	}

	Identifier getOrReg(const(char)[] str) {
		assert(str.length > 0);
		uint id = map.get(cast(string)str, uint.max);
		if (id == uint.max) {
			char[] buf = stringDataBuffer.voidPut(str.length);
			buf[] = str;
			string duppedKey = cast(string)buf;
			// can't use .max, because it marks null ids
			assert(strings.length < uint.max, "Id map overflow");
			id = cast(uint)strings.length;
			map[duppedKey] = id;
			//writefln("getOrReg %s %s", id, duppedKey);
			strings.put(duppedKey);
		}
		return Identifier(id);
	}

	Identifier getOrRegNoDup(const(char)[] str) {
		assert(str.length > 0);
		uint id = map.get(cast(string)str, uint.max);
		if (id == uint.max) {
			id = cast(uint)strings.length;
			map[str] = id;
			//writefln("getOrRegNoDup %s %s", id, str);
			strings.put(str);
		}
		return Identifier(id);
	}

	// internal. Called in CompilationContext.initialize()
	void regCommonIds()
	{
		assert(strings.length == 0);
		assert(map.length == 0);
		assert(stringDataBuffer.length == 0);
		foreach (size_t i, string memberName; __traits(allMembers, CommonIds))
		{
			string name = __traits(getAttributes, __traits(getMember, CommonIds, memberName))[0];
			getOrRegNoDup(name);
		}
	}
}

enum CommonIds : Identifier
{
	@("ptr")    id_ptr    = Identifier(0),
	@("length") id_length = Identifier(1),
	@("min")    id_min    = Identifier(2),
	@("max")    id_max    = Identifier(3),
	@("sizeof") id_sizeof = Identifier(4),
	@("this")   id_this   = Identifier(5),
}
