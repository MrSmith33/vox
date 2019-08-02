/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.identifier;

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
		return Identifier(map.get(cast(string)str, uint.max));
	}

	Identifier getOrRegWithSuffix(const(char)[] str, size_t suffix) {
		import std.format : formattedWrite;
		tempBuf.clear;
		tempBuf.put(cast(string)str);
		formattedWrite(tempBuf, "%s", suffix);
		const(char)[] idString = tempBuf.data;
		return getOrReg(idString);
	}

	Identifier getOrReg(const(char)[] str) {
		uint id = map.get(cast(string)str, uint.max);
		if (id == uint.max) {
			char[] buf = stringDataBuffer.voidPut(str.length);
			buf[] = str;
			string duppedKey = cast(string)buf;
			assert(strings.length < uint.max, "Id map overflow");
			id = cast(uint)strings.length;
			map[duppedKey] = id;
			strings.put(duppedKey);
		}
		return Identifier(id);
	}

	Identifier getOrRegNoDup(const(char)[] str) {
		uint id = map.get(cast(string)str, uint.max);
		if (id == uint.max) {
			id = cast(uint)strings.length;
			map[str] = id;
			strings.put(str);
		}
		return Identifier(id);
	}
}

struct CommonIdentifiers
{
	Identifier id_ptr;
	Identifier id_length;
	Identifier id_min;
	Identifier id_max;
}

CommonIdentifiers collectIdentifiers(ref CompilationContext context) {
	CommonIdentifiers res;
	res.id_ptr = context.idMap.getOrRegNoDup("ptr");
	res.id_length = context.idMap.getOrRegNoDup("length");
	res.id_min = context.idMap.getOrRegNoDup("min");
	res.id_max = context.idMap.getOrRegNoDup("max");
	return res;
}
