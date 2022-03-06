/// Copyright: Copyright (c) 2021 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.passes.read_source;

import vox.all;

void pass_read_source(ref CompilationContext ctx, CompilePassPerModule[] subPasses)
{
	size_t start = ctx.sourceBuffer.length;
	foreach(ref file; ctx.files.data)
	{
		if (file.content)
		{
			ctx.sourceBuffer.put(SOI_CHAR);
			ctx.sourceBuffer.put(file.content);
			ctx.sourceBuffer.put(EOI_CHAR);

			file.length = cast(uint)(file.content.length + 2);
			file.start = cast(uint)start;
			start += file.length;
		}
		else
		{
			import std.file : exists;
			import std.path : absolutePath;
			import std.stdio : File;

			if (!exists(file.name))
			{
				ctx.error("File `%s` not found", absolutePath(file.name));
				return;
			}

			ctx.sourceBuffer.put(SOI_CHAR);
			auto f = File(file.name, "r");
			size_t fileLength = f.size;
			char[] sourceBuffer = ctx.sourceBuffer.voidPut(fileLength);
			if (fileLength) {
				// rawRead doesn't support reading into empty sourceBuffer
				char[] result = f.rawRead(sourceBuffer);
				ctx.assertf(result.length == fileLength,
					"File read failed due to mismatch. File size is %s bytes, while read %s bytes",
					fileLength, result.length);
			}
			f.close();
			ctx.sourceBuffer.put(EOI_CHAR);

			file.content = cast(string)sourceBuffer;
			file.length = cast(uint)(fileLength + 2);
			file.start = cast(uint)start;
			start += file.length;
		}

		if (ctx.bundleInputs) {
			import std.string : stripRight;
			ctx.bundleBuffer.put("--- ");
			ctx.bundleBuffer.put(file.name);
			ctx.bundleBuffer.put("\n");
			ctx.bundleBuffer.put(file.content.stripRight);
			ctx.bundleBuffer.put("\n");
		}

		if (ctx.printSource) {
			import std.stdio : writeln, writefln;
			writefln("// Source `%s`", file.name);
			writeln(file.content);
		}
	}
}
