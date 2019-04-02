/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.identifier;

import utils : Buffer, Arena;

alias Identifier = uint;

struct IdentifierMap {
	Arena!char stringDataBuffer;
	Arena!(const(char)[]) strings;
	uint[string] map;

	Buffer!char tempBuf;

	string get(Identifier id) {
		return strings[id];
	}

	Identifier find(const(char)[] str) {
		return map.get(cast(string)str, uint.max);
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
			id = cast(uint)strings.length;
			map[duppedKey] = id;
			strings.put(duppedKey);
		}
		return id;
	}

	Identifier getOrRegNoDup(const(char)[] str) {
		uint id = map.get(cast(string)str, uint.max);
		if (id == uint.max) {
			id = cast(uint)strings.length;
			map[str] = id;
			strings.put(str);
		}
		return id;
	}
}
