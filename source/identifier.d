/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module identifier;

import utils : Buffer;

alias Identifier = uint;

struct IdentifierMap {
	string[] strings;
	uint[string] map;

	Buffer!char tempBuf;

	string get(Identifier id) {
		return strings[id];
	}

	Identifier find(string str) {
		return map.get(str, uint.max);
	}

	Identifier getOrRegWithSuffix(string str, size_t suffix) {
		import std.format : formattedWrite;
		tempBuf.clear;
		tempBuf.put(str);
		formattedWrite(tempBuf, "%s%s", str, suffix);
		const(char)[] idString = tempBuf.data;
		return getOrReg(idString);
	}

	Identifier getOrReg(const(char)[] str) {
		uint id = map.get(cast(string)str, uint.max);
		if (id == uint.max) {
			string duppedKey = str.idup;
			id = cast(uint)strings.length;
			map[duppedKey] = id;
			strings ~= duppedKey;
		}
		return id;
	}

	Identifier getOrRegNoDup(string str) {
		uint id = map.get(str, uint.max);
		if (id == uint.max) {
			id = cast(uint)strings.length;
			map[str] = id;
			strings ~= str;
		}
		return id;
	}
}
