/**
Copyright: Copyright (c) 2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module be.link;

import std.file;
import std.stdio;

import all;
import be.pecoff;

void linkModule(ref CompilationContext context, LinkIndex modIndex)
{
	ObjectModule* mod = &context.objSymTab.getModule(modIndex);
	//writefln("%s %s", modIndex, context.idString(mod.id));

	LinkIndex symIndex = mod.firstSymbol;
	while (symIndex.isDefined)
	{
		ObjectSymbol* sym = &context.objSymTab.getSymbol(symIndex);
		//writef("  %s %s", symIndex, context.idString(sym.id));
		//if (sym.isString)
		//	writefln(` "%s"`, (cast(char*)(sym.dataPtr))[0..sym.length]);
		//else writeln;

		LinkIndex symRefIndex = sym.firstRef;
		while (symRefIndex.isDefined)
		{
			ObjectSymbolReference* symRef = &context.objSymTab.getReference(symRefIndex);
			//writefln("    %s -> %s: off 0x%X extra %s %s",
			//	symRefIndex, symRef.referencedSymbol, symRef.refOffset,
			//	symRef.extraOffset, symRef.refKind);

			final switch(symRef.refKind) with(ObjectSymbolRefKind)
			{
				case absolute64:
					context.unreachable; // TODO
					break;

				case relative32:
					ObjectSymbol* fromSymbol = &context.objSymTab.getSymbol(symRef.fromSymbol);
					ObjectSymbol* toSymbol = &context.objSymTab.getSymbol(symRef.referencedSymbol);
					ObjectSection* fromSection = &context.objSymTab.getSection(fromSymbol.sectionIndex);
					ObjectSection* toSection = &context.objSymTab.getSection(toSymbol.sectionIndex);

					// section + symbol offset + reference offset
					ulong fromAddr = fromSection.sectionAddress + fromSymbol.sectionOffset + symRef.refOffset;
					ulong toAddr = toSection.sectionAddress + toSymbol.sectionOffset;

					int* fixup = cast(int*)(fromSection.sectionData + fromSymbol.sectionOffset + symRef.refOffset);

					int value = cast(int)(toAddr - fromAddr) - symRef.extraOffset;
					//writefln("fromSection %s %X", context.idString(fromSection.id), fromSection.sectionData);
					//writefln("toSection %s %X", context.idString(toSection.id), toSection.sectionData);
					//writefln("fromAddr %X", fromAddr);
					//writefln("toAddr %X", toAddr);
					//writefln("value %X -> %X", *fixup, value);

					*fixup = value;

					break;
			}

			symRefIndex = symRef.nextReference;
		}
		symIndex = sym.nextSymbol;
	}
}
