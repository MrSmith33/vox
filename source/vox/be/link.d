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

void linkModule(ref CompilationContext c, LinkIndex modIndex)
{
	ObjectModule* mod = c.objSymTab.getModule(modIndex);
	//writefln("%s %s", modIndex, c.idString(mod.id));

	LinkIndex symIndex = mod.firstSymbol;
	while (symIndex.isDefined)
	{
		ObjectSymbol* fromSymbol = c.objSymTab.getSymbol(symIndex);
		ObjectSection* fromSection = c.objSymTab.getSection(fromSymbol.sectionIndex);
		ulong fromSymAddr = fromSection.sectionAddress + fromSymbol.sectionOffset;

		//writef("  %s `%s` %X", symIndex, c.idString(fromSymbol.id), fromSymAddr);
		//if (fromSymbol.isString)
		//	writefln(` "%s"`, (cast(char*)(fromSymbol.dataPtr))[0..fromSymbol.length]);
		//else writeln;
		//writefln("    fromSection %s %X", c.idString(fromSection.id), fromSection.buffer.bufPtr);

		LinkIndex symRefIndex = fromSymbol.firstRef;

		// symbols with references must be in the correct section
		if (symRefIndex.isDefined) {
			c.assertf(fromSection.buffer.bufPtr + fromSymbol.sectionOffset == fromSymbol.dataPtr, "Symbol.dataPtr does not point to the section buffer");
		}

		while (symRefIndex.isDefined)
		{
			ObjectSymbolReference* symRef = c.objSymTab.getReference(symRefIndex);
			//writefln("    %s -> %s: off 0x%X extra %s %s",
			//	symRefIndex, symRef.referencedSymbol, symRef.refOffset,
			//	symRef.extraOffset, symRef.refKind);

			ObjectSymbol* toSymbol = c.objSymTab.getSymbol(symRef.referencedSymbol);
			ObjectSection* toSection = c.objSymTab.getSection(toSymbol.sectionIndex);
			//writefln("      toSection %s %X", c.idString(toSection.id), toSection.buffer.bufPtr);

			// section + symbol offset + reference offset
			ulong fromAddr = fromSymAddr + symRef.refOffset;
			ulong toAddr = toSection.sectionAddress + toSymbol.sectionOffset;
			//writefln("      refAddr %X", fromAddr);
			//writefln("      toAddr %X", toAddr);

			if (toAddr == 0 && toSection.type == ObjectSectionType.host) {
				c.internal_error("Trying to referece null host symbol %s from %s", c.idString(toSymbol.id), c.idString(fromSymbol.id));
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
					c.assertf(diff >= int.min && diff <= int.max,
						"Cannot encode relative 32-bit offset from %s at 0x%X to %s at 0x%X, because offset is bigger than 2GiB (%s) bytes",
						c.idString(fromSymbol.id), fromAddr, c.idString(toSymbol.id), toAddr, diff);
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
