/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module vox.utils.textsink;

struct IndentTextSink
{
	import std.range : repeat;

	TextSink sink;
	int indentSize = 2;
	private int indent;

	void putIndent() { sink.putf("%s", ' '.repeat(indent)); }
	void put(in char[] str) { putIndent; sink.put(str); }
	void putf(Args...)(const(char)[] fmt, Args args) { putIndent; sink.putf(fmt, args); }
	void putfln(Args...)(const(char)[] fmt, Args args) { putIndent; sink.putfln(fmt, args); }
	void putln(const(char)[] str = null) { putIndent; sink.putln(str); }

	void push() { indent += indentSize; }
	void pop() { indent -= indentSize; }
}

struct TextSink
{
	import std.format : formattedWrite;
	import std.string : stripRight;
	import vox.utils : Buffer;

	Buffer!char data;

	void clear() { data.clear(); }
	string text() { return stripRight(cast(string)data.data); }

	void put(in char[] str)
	{
		if (str.length == 0) return;
		data.put(str);
		data.stealthPut('\0');
	}

	void putf(Args...)(const(char)[] fmt, Args args) { formattedWrite(&this, fmt, args); }
	void putfln(Args...)(const(char)[] fmt, Args args) { formattedWrite(&this, fmt, args); put("\n"); }
	void putln(const(char)[] str = null) { put(str); put("\n"); }
}
