/**
Copyright: Copyright (c) 2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module backend.link_jit;

import std.file;
import std.stdio;

import all;
import pecoff;

void pass_link_jit(ref CompilationContext context)
{
	if (context.printSymbols) context.objSymTab.dump(&context);

	ObjectSection* dataSection = &context.objSymTab.getSection(context.dataSectionIndex);
	dataSection.sectionAddress = cast(ulong)context.staticDataBuffer.bufPtr;
	ObjectSection* textSection = &context.objSymTab.getSection(context.textSectionIndex);
	textSection.sectionAddress = cast(ulong)context.codeBuffer.ptr;

	linkModule(context);
}
