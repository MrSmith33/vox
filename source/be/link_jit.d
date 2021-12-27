/**
Copyright: Copyright (c) 2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module be.link_jit;

import std.file;
import std.stdio;

import all;

void pass_link_jit(ref CompilationContext context, CompilePassPerModule[] subPasses)
{
	context.objSymTab.getSection(context.builtinSections[ObjectSectionType.rw_data]).sectionAddress = cast(ulong)context.staticDataBuffer.bufPtr;
	context.objSymTab.getSection(context.builtinSections[ObjectSectionType.ro_data]).sectionAddress = cast(ulong)context.roStaticDataBuffer.bufPtr;
	context.objSymTab.getSection(context.builtinSections[ObjectSectionType.imports]).sectionAddress = cast(ulong)context.importBuffer.bufPtr;
	context.objSymTab.getSection(context.builtinSections[ObjectSectionType.code]).sectionAddress = cast(ulong)context.codeBuffer.bufPtr;

	foreach (ref SourceFileInfo file; context.files.data) {
		linkModule(context, file.mod.objectSymIndex);
	}

	if (context.printSymbols) context.objSymTab.dump(&context);
}
