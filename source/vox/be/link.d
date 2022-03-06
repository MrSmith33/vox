/**
Copyright: Copyright (c) 2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module vox.be.link;

import std.file;
import std.stdio;

import vox.all;
import vox.be.pecoff;

void linkModule(ref CompilationContext context, LinkIndex modIndex)
{
	ObjectModule* mod = context.objSymTab.getModule(modIndex);
	//writefln("%s %s", modIndex, context.idString(mod.id));

	LinkIndex symIndex = mod.firstSymbol;
	while (symIndex.isDefined)
	{
		ObjectSymbol* fromSymbol = context.objSymTab.getSymbol(symIndex);
		ObjectSection* fromSection = context.objSymTab.getSection(fromSymbol.sectionIndex);
		ulong fromSymAddr = fromSection.sectionAddress + fromSymbol.sectionOffset;

		//writef("  %s `%s` %X", symIndex, context.idString(fromSymbol.id), fromSymAddr);
		//if (fromSymbol.isString)
		//	writefln(` "%s"`, (cast(char*)(fromSymbol.dataPtr))[0..fromSymbol.length]);
		//else writeln;
		//writefln("    fromSection %s %X", context.idString(fromSection.id), fromSection.buffer.bufPtr);

		LinkIndex symRefIndex = fromSymbol.firstRef;
		while (symRefIndex.isDefined)
		{
			ObjectSymbolReference* symRef = context.objSymTab.getReference(symRefIndex);
			//writefln("    %s -> %s: off 0x%X extra %s %s",
			//	symRefIndex, symRef.referencedSymbol, symRef.refOffset,
			//	symRef.extraOffset, symRef.refKind);

			ObjectSymbol* toSymbol = context.objSymTab.getSymbol(symRef.referencedSymbol);
			ObjectSection* toSection = context.objSymTab.getSection(toSymbol.sectionIndex);
			//writefln("      toSection %s %X", context.idString(toSection.id), toSection.buffer.bufPtr);

			// section + symbol offset + reference offset
			ulong fromAddr = fromSymAddr + symRef.refOffset;
			ulong toAddr = toSection.sectionAddress + toSymbol.sectionOffset;
			//writefln("      refAddr %X", fromAddr);
			//writefln("      toAddr %X", toAddr);

			if (toAddr == 0 && toSection.type == ObjectSectionType.host) {
				context.internal_error("Trying to referece null host symbol %s from %s", context.idString(toSymbol.id), context.idString(fromSymbol.id));
			}

			void* fixupLocation = cast(void*)(fromSection.buffer.bufPtr + fromSymbol.sectionOffset + symRef.refOffset);

			final switch(symRef.refKind) with(ObjectSymbolRefKind)
			{
				case absolute64:
					ulong value = toAddr - symRef.extraOffset;
					ulong* fixup = cast(ulong*)fixupLocation;
					//writefln("      value %X -> %X", *fixup, value);
					*fixup = value; // write
					break;

				case relative32:
					ptrdiff_t diff = toAddr - fromAddr;
					context.assertf(diff >= int.min && diff <= int.max,
						"Cannot encode relative 32-bit offset from %s at 0x%X to %s at 0x%X, because offset is bigger than 2GiB (%s) bytes",
						context.idString(fromSymbol.id), fromAddr, context.idString(toSymbol.id), toAddr, diff);
					int value = cast(int)(toAddr - fromAddr) - symRef.extraOffset;
					int* fixup = cast(int*)fixupLocation;
					//writefln("      value %X -> %X", *fixup, value);
					*fixup = value; // write
					break;
			}

			symRefIndex = symRef.nextReference;
		}
		symIndex = fromSymbol.nextSymbol;
	}
}
