/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Parses HAR - Human Archive Format: https://github.com/marler8997/har
module utils.har;

import context : CompilationContext;
import fe.ast.source_file : SourceFileInfo;

void parseHar(
	ref CompilationContext context,
	string harFilename,
	const(char)[] harData,
	void delegate(SourceFileInfo fileInfo) onFile)
{
	import std.string : indexOf, lineSplitter;
	import std.algorithm : startsWith;
	auto lines = harData.lineSplitter;
	size_t lineNumber = 1;
	auto line = lines.front;

	auto spaceIndex = line.indexOf(' ');
	if (spaceIndex <= 0) {
		context.error("HAR file error `%s`: First line must start with delimiter ending with space", harFilename);
		return;
	}

	auto delimiter = line[0 .. spaceIndex + 1];

	outer:
	while (true)
	{
		if (line.length == 0) {
			context.error("HAR file error `%s`: Missing filename on line %s", harFilename, lineNumber);
			return;
		}

		string filename = cast(string)line[delimiter.length .. $];
		lines.popFront();

		if (lines.empty) break;
		const(char)* fileStart = lines.front.ptr;
		const(char)* fileEnd = lines.front.ptr + lines.front.length;

		while (true)
		{
			if (lines.empty) {
				onFile(SourceFileInfo(filename, fileStart[0..fileEnd-fileStart]));
				break outer;
			}

			lineNumber++;
			line = lines.front;

			if (line.startsWith(delimiter)) {
				onFile(SourceFileInfo(filename, fileStart[0..line.ptr-fileStart]));
				break;
			}

			fileEnd = lines.front.ptr + lines.front.length;
			lines.popFront();
		}
	}
}
