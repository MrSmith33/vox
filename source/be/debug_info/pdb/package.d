/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// .pdb reader and writer
/// PDB docs http://llvm.org/docs/PDB/index.html
/// MSF docs http://llvm.org/docs/PDB/MsfFile.html
module be.debug_info.pdb;

public import be.debug_info.pdb.symbol;

import std.bitmanip : bitsSet, bitfields;
import std.file;
import std.format : formattedWrite;
import std.path;
import std.string : fromStringz;

import utils;

/*
pdb format

MSF - Multistream File

file consists of pages of size 512, 1024, 2048 or 4096
Meta-stream stores information about all streams in .pdb file. It has following format:
	uint numStreams
	uint[numStreams] streamSizes
	uint[][numStreams] streamPages
	Note: some streams produced by MSVC have uint.max size. We interpret that as 0 size.
	It is stored in a list of pages.
	Array of indicies to those pages are stored in a page `metaStreamHeaderPage`
Free page bitmap is a page that stores a bit per page. 1 if page is free, 0 if not.
Page map...

Page 0
	MSF magic
	MSF header
Page 1  \ freePageBitmapIndex is 1 or 2
Page 2  /
Page metaStreamHeaderPage
...
Page
*/

struct PdbInfo
{
	string filename;
	ubyte[] fileData;
	MsfHeader msfHeader;

	// Those are valid if != 0
	uint linkInfoStreamIndex; // /LinkInfo
	uint headerblockStreamIndex; // /src/headerblock
	uint namesStreamIndex; // /names
	uint globalStreamIndex;
	uint publicStreamIndex;
	uint symRecordStream;

	StreamInfo[] streamInfos;
	void setStreamName(uint index, string name)
	{
		if (index == ushort.max) return; // this index is not valid
		enforce(index < streamInfos.length);
		enforce(streamInfos[index].name is null);
		streamInfos[index].name = name;
	}

	void printStreamInfos()
	{
		foreach(i, ref info; streamInfos)
		{
			writefln("%s %s %s bytes", i, info.name, info.streamBytes);
		}
	}
}

struct StreamInfo
{
	string name;
	uint streamBytes;
}

struct PdbReader
{
	PdbInfo pdb;
	string filename;
	ubyte[] fileData;

	MsfHeader* msfHeader;

	static PdbReader fromFile(string filename)
	{
		enforce(exists(filename), format("%s does not exist", filename));
		enforce(extension(filename) == ".pdb", format("%s must have .pdb extension", filename));

		ubyte[] fileData = cast(ubyte[])std.file.read(filename);
		return fromBytes(fileData, filename);
	}

	static PdbReader fromBytes(ubyte[] fileData, string filename = null)
	{
		PdbReader reader;
		reader.filename = filename;
		reader.fileData = fileData;
		reader.load();
		return reader;
	}

	/// Performs actual processing of .pdb data stored in fileData
	void load()
	{
		writefln("load %s %s bytes", filename, fileData.length);

		auto slicer = FileDataSlicer(fileData);

		ubyte[] msfMagic = slicer.getArrayOf!ubyte(MsfMagic.length);
		enforce(msfMagic == MsfMagic, "Invalid magic");

		pdb.msfHeader = *slicer.getPtrTo!MsfHeader;
		pdb.msfHeader.validate;
		enforce(pdb.msfHeader.numPages * pdb.msfHeader.pageSize == fileData.length,
			format("Invalid file size (%s) != num pages (%s) * page size (%s)",
				fileData.length, pdb.msfHeader.numPages, pdb.msfHeader.pageSize));

		writeln(pdb.msfHeader);

		slicer.fileCursor = pdb.msfHeader.metaStreamHeaderPage * pdb.msfHeader.pageSize;
		writefln("meta stream header page %s, offset 0x%X", pdb.msfHeader.metaStreamHeaderPage, slicer.fileCursor);

		uint[] metaStreamPagesIndicies = slicer.getArrayOf!uint(pdb.msfHeader.numMetaStreamPages);
		writefln("meta stream pages %s", metaStreamPagesIndicies);

		StreamReader metaStream;
		metaStream.fileData = fileData;
		metaStream.streamBytes = pdb.msfHeader.metaStreamBytes;
		metaStream.pageSize = pdb.msfHeader.pageSize;
		metaStream.pages = metaStreamPagesIndicies;

		uint numStreams = metaStream.read!uint;
		writefln("number of streams %s", numStreams);

		pdb.streamInfos = new StreamInfo[numStreams];
		pdb.setStreamName(FixedStream.old_directory, "Old directory");
		pdb.setStreamName(FixedStream.pdb_stream, "PDB stream");
		pdb.setStreamName(FixedStream.tpi_stream, "TPI stream");
		pdb.setStreamName(FixedStream.dbi_stream, "DBI stream");
		pdb.setStreamName(FixedStream.ipi_stream, "IPI stream");

		StreamReader[] streams = new StreamReader[numStreams];
		foreach(i, ref stream; streams)
		{
			stream.fileData = fileData;
			stream.streamBytes = metaStream.read!uint;
			pdb.streamInfos[i].streamBytes = stream.streamBytes;
			if (stream.streamBytes == uint.max) stream.streamBytes = 0;

			stream.pageSize = pdb.msfHeader.pageSize;
			if (stream.streamBytes)
				writefln("  stream%s size %s bytes", i, stream.streamBytes);
		}
		foreach(streamIndex, ref stream; streams)
		{
			uint numPages = divCeil(stream.streamBytes, stream.pageSize);
			stream.pages = new uint[numPages];
			metaStream.readIntoArray(stream.pages);
			foreach(page; stream.pages)
			{
				enforce(page < pdb.msfHeader.numPages,
					format("Stream %s contains page %s. Number of pages is %s",
						streamIndex, page, pdb.msfHeader.numPages));
			}
		}

		foreach(i, ref stream; streams)
		{
			writefln("%s %s", i, stream);
		}

		StreamReader* pdbStream = &streams[FixedStream.pdb_stream];
		auto pdbStreamHeader = pdbStream.read!PdbStreamHeader;
		writefln("PDB stream header %s", pdbStreamHeader);

		uint stringBufLength = pdbStream.read!uint;
		string stringBuf = new char[stringBufLength];
		pdbStream.readIntoArray(cast(char[])stringBuf);
		writefln("  string buffer %s", stringBuf);

		// size == number of present keys == number of present values
		uint hashmapSize = pdbStream.read!uint;
		// capacity == number of keys + number of values
		uint hashmapCapacity = pdbStream.read!uint / 2;
		writefln("  size %s", hashmapSize);
		writefln("  capacity %s", hashmapCapacity);

		uint presentBitmapWords = pdbStream.read!uint;
		uint[] presentBitmap = new uint[presentBitmapWords];
		pdbStream.readIntoArray(presentBitmap);

		uint deletedBitmapWords = pdbStream.read!uint;
		uint[] deletedBitmap = new uint[deletedBitmapWords];
		pdbStream.readIntoArray(deletedBitmap);

		uint[2][] hashmapBuckets = new uint[2][hashmapCapacity];
		pdbStream.readIntoArray(hashmapBuckets);

		// gather named streams
		foreach(size_t i, uint bitmapPart; presentBitmap)
		{
			if (!bitmapPart) continue; // skip all zeroes

			size_t groupIndex = i * uint.sizeof * 8;

			// iterate set bits in uint
			foreach(size_t bitSet; bitmapPart.bitsSet)
			{
				size_t bucketIndex = groupIndex + bitSet;
				if (bucketIndex >= hashmapCapacity) break; // ignore set bits out of range

				uint[2] pair = hashmapBuckets[bucketIndex];
				string name = fromStringz(stringBuf[pair[0]..$].ptr);

				switch(name)
				{
					case "/LinkInfo": pdb.linkInfoStreamIndex = pair[1]; break;
					case "/src/headerblock": pdb.headerblockStreamIndex = pair[1]; break;
					case "/names": pdb.namesStreamIndex = pair[1]; break;
					default: break; // ignore unknown
				}
			}
		}

		if (pdb.linkInfoStreamIndex) {
			pdb.setStreamName(pdb.linkInfoStreamIndex, "/LinkInfo");
			writefln("  /LinkInfo %s", pdb.linkInfoStreamIndex);
		}
		if (pdb.headerblockStreamIndex) {
			pdb.setStreamName(pdb.headerblockStreamIndex, "/src/headerblock");
			writefln("  /src/headerblock %s", pdb.headerblockStreamIndex);
		}
		if (pdb.namesStreamIndex) {
			pdb.setStreamName(pdb.namesStreamIndex, "/names");
			writefln("  /names %s", pdb.namesStreamIndex);
		}

		uint pdbFeatures;
		write("  PDB features:");
		while(!pdbStream.empty)
		{
			uint featureCode = pdbStream.read!uint;
			switch(featureCode) with(Pdb_FeatureCodeMagic)
			{
				case vc110: pdbFeatures |= Pdb_FeatureFlags.vc110; write(" vc110"); break;
				case vc140: pdbFeatures |= Pdb_FeatureFlags.vc140; write(" vc140"); break;
				case noTypeMerge: pdbFeatures |= Pdb_FeatureFlags.noTypeMerge; write(" noTypeMerge"); break;
				case minimalDebugInfo: pdbFeatures |= Pdb_FeatureFlags.minimalDebugInfo; write(" minimalDebugInfo"); break;
				default: break; // ignore unknown feature
			}
		}
		writeln;

		StreamReader* tpiStream = &streams[FixedStream.tpi_stream];
		auto tpiStreamHeader = tpiStream.read!TpiStreamHeader;
		writefln("TPI stream header %s, %s", tpiStreamHeader, tpiStream.remainingBytes);
		pdb.setStreamName(tpiStreamHeader.hashStreamIndex, "TPI Hash stream");
		pdb.setStreamName(tpiStreamHeader.hashAuxStreamIndex, "TPI Hash aux stream");

		size_t typeIndex;
		while(tpiStream.remainingBytes)
		{
			auto len = tpiStream.read!ushort;
			auto start = tpiStream.streamCursor;
			auto end = start + len;

			auto kind = tpiStream.read!ushort;
			writefln("  index %s length %s kind %s", typeIndex, len, cast(TypeRecordKind)kind);

			switch(kind) with(TypeRecordKind)
			{
				case LF_ARGLIST:
					auto argcount = tpiStream.read!uint;
					writefln("    number of parameters %s", argcount);
					foreach(index; 0..argcount)
					{
						auto arg = tpiStream.read!TypeIndex;
						writefln("    arg %s", arg);
					}
					break;
				case LF_PROCEDURE:
					auto proc = tpiStream.read!CVType_PROCEDURE;
					writefln("    return type %s", proc.returnType);
					writefln("    calling convention %s", proc.callConv);
					writefln("    number of parameters %s", proc.numParams);
					writefln("    arguments %s", proc.argList);
					break;
				default:
					tpiStream.drop(len - 2);
					break;
			}

			auto padding = end - tpiStream.streamCursor;
			assert(padding < 4);
			if (padding) writefln("  padding %s bytes", padding);

			// skip padding
			tpiStream.streamCursor = end;

			++typeIndex;
		}
		assert(typeIndex + 0x1000 == tpiStreamHeader.typeIndexEnd);

		StreamReader* ipiStream = &streams[FixedStream.ipi_stream];
		auto ipiStreamHeader = ipiStream.read!TpiStreamHeader;
		writefln("IPI stream header %s, %s", ipiStreamHeader, ipiStream.remainingBytes);
		pdb.setStreamName(ipiStreamHeader.hashStreamIndex, "IPI Hash stream");
		pdb.setStreamName(ipiStreamHeader.hashAuxStreamIndex, "IPI Hash aux stream");

		while(ipiStream.remainingBytes)
		{
			auto len = ipiStream.read!ushort;
			auto start = ipiStream.streamCursor;
			auto end = start + len;

			auto kind = ipiStream.read!ushort;
			writefln("  index %s length %s kind %s", typeIndex, len, cast(TypeRecordKind)kind);

			switch(kind) with(TypeRecordKind)
			{
				case LF_FUNC_ID:
					auto funcId = ipiStream.read!CVType_FUNC_ID;
					writefln("    scope id %s", funcId.scopeId);
					writefln("    type %s", funcId.type);
					string name = ipiStream.readNameBefore(end);
					writefln("    name `%s`", name);
					break;
				case LF_STRING_ID:
					auto stringId = ipiStream.read!CVType_STRING_ID;
					writefln("    id 0x%X", stringId.id);
					string name = ipiStream.readNameBefore(end);
					writefln("    name `%s`", name);
					break;
				case LF_SUBSTR_LIST:
					auto argcount = ipiStream.read!uint;
					writefln("    number of substrings %s", argcount);
					foreach(index; 0..argcount)
					{
						auto arg = ipiStream.read!TypeIndex;
						writefln("      substr %s", arg);
					}
					break;
				case LF_BUILDINFO:
					auto buildInfo = ipiStream.read!CVType_BUILDINFO;
					writefln("    number of args %s", buildInfo.count);
					writefln("    CurrentDirectory %s", buildInfo.args[CV_BuildInfo.CurrentDirectory]);
					writefln("    BuildTool %s", buildInfo.args[CV_BuildInfo.BuildTool]);
					writefln("    SourceFile %s", buildInfo.args[CV_BuildInfo.SourceFile]);
					writefln("    ProgramDatabaseFile %s", buildInfo.args[CV_BuildInfo.ProgramDatabaseFile]);
					writefln("    CommandArguments %s", buildInfo.args[CV_BuildInfo.CommandArguments]);
					break;
				default:
					string stringBuf2 = new char[len - 2];
					ipiStream.readIntoArray(cast(char[])stringBuf2);
					writefln("    buf `%s`", stringBuf2);
					//ipiStream.drop(len - 2);

					break;
			}

			auto padding = end - ipiStream.streamCursor;
			assert(padding < 4);
			if (padding) writefln("  padding: %s bytes", padding);

			// skip padding
			ipiStream.streamCursor = end;

			++typeIndex;
		}

		StreamReader* dbiStream = &streams[FixedStream.dbi_stream];
		auto dbiStreamHeader = dbiStream.read!DbiStreamHeader;
		writefln("DBI stream header %s, %s %s", dbiStreamHeader, DbiStreamHeader.sizeof, dbiStream.remainingBytes);
		pdb.globalStreamIndex = dbiStreamHeader.GlobalStreamIndex;
		pdb.publicStreamIndex = dbiStreamHeader.PublicStreamIndex;
		pdb.symRecordStream = dbiStreamHeader.SymRecordStream;
		pdb.setStreamName(pdb.globalStreamIndex, "Global Symbol stream");
		pdb.setStreamName(pdb.publicStreamIndex, "Public Symbol stream");
		pdb.setStreamName(pdb.symRecordStream, "Symbol record");

		uint substreamsSize = dbiStreamHeader.ModInfoSize
			+ dbiStreamHeader.SectionContributionSize
			+ dbiStreamHeader.SectionMapSize
			+ dbiStreamHeader.SourceInfoSize
			+ dbiStreamHeader.TypeServerMapSize
			+ dbiStreamHeader.OptionalDbgHeaderSize
			+ dbiStreamHeader.ECSubstreamSize;

		enforce(substreamsSize == dbiStream.remainingBytes,
			format("substream size sum (%s) != remainingBytes (%s)",
			substreamsSize, dbiStream.remainingBytes));

		StreamReader dbiModInfoStream = dbiStream.substream(dbiStreamHeader.ModInfoSize);
		writefln("  dbiModInfoStream %s", dbiModInfoStream.remainingBytes);
		StreamReader dbiSectionContributionStream = dbiStream.substream(dbiStreamHeader.SectionContributionSize);
		writefln("  dbiSectionContributionStream %s", dbiSectionContributionStream.remainingBytes);
		StreamReader dbiSectionMapStream = dbiStream.substream(dbiStreamHeader.SectionMapSize);
		writefln("  dbiSectionMapStream %s", dbiSectionMapStream.remainingBytes);
		StreamReader dbiSourceInfoStream = dbiStream.substream(dbiStreamHeader.SourceInfoSize);
		writefln("  dbiSourceInfoStream %s", dbiSourceInfoStream.remainingBytes);
		StreamReader dbiTypeServerMapStream = dbiStream.substream(dbiStreamHeader.TypeServerMapSize);
		writefln("  dbiTypeServerMapStream %s", dbiTypeServerMapStream.remainingBytes);
		StreamReader dbiECSubstreamStream = dbiStream.substream(dbiStreamHeader.ECSubstreamSize);
		writefln("  dbiECSubstreamStream %s", dbiECSubstreamStream.remainingBytes);
		StreamReader dbiOptionalDbgHeaderStream = dbiStream.substream(dbiStreamHeader.OptionalDbgHeaderSize);
		writefln("  dbiOptionalDbgHeaderStream %s", dbiOptionalDbgHeaderStream.remainingBytes);
		enforce(dbiStream.remainingBytes == 0);

		while(dbiModInfoStream.remainingBytes)
		{
			auto modInfo = dbiModInfoStream.read!ModInfo;
			writefln("modInfo %s, %s %s", modInfo, ModInfo.sizeof, dbiModInfoStream.remainingBytes);
			string moduleName = dbiModInfoStream.readZString;
			pdb.setStreamName(modInfo.ModuleSymStream, format("module sym stream: %s", moduleName));
			writefln("  moduleName %s", moduleName);
			string objFileName = dbiModInfoStream.readZString;
			writefln("  objFileName %s", objFileName);
			dbiModInfoStream.dropPadding(4);

			if (modInfo.ModuleSymStream != ushort.max)
			{
				StreamReader modSymStream = streams[modInfo.ModuleSymStream];
				writefln("  mod sym stream %s: %s bytes", modInfo.ModuleSymStream, modSymStream.remainingBytes);
				//ubyte[] buf = new ubyte[modSymStream.remainingBytes];
				//modSymStream.readIntoArray(buf);
				//printHex(buf, 16);
				auto debugMagic = modSymStream.read!uint;
				enforce(debugMagic == COFF_DEBUG_SECTION_MAGIC,
					format("Invalid magic (%s), expected %s",
						debugMagic, COFF_DEBUG_SECTION_MAGIC));

				parseSymbols(modSymStream);

				// read C13 debug subsections
				while(modSymStream.remainingBytes)
				{
					// read DebugSubsectionHeader
					auto kind = modSymStream.read!DebugSubsectionKind;
					if (kind == DebugSubsectionKind.none) break; // last entry
					auto length = modSymStream.read!uint;
					writefln("  debug subsection %s: %s bytes", kind, length);
					modSymStream.drop(length);
				}

				enforce(modSymStream.remainingBytes == 0);
			}
		}
		enforce(dbiModInfoStream.remainingBytes == 0);

		auto secContribVer = dbiSectionContributionStream.read!SectionContrSubstreamVersion;
		writefln("Section Contr Substream Version %s", secContribVer);
		enforce(secContribVer == SectionContrSubstreamVersion.Ver60, "only SectionContrSubstreamVersion.Ver60 is supported");
		while(dbiSectionContributionStream.remainingBytes) {
			auto entry = dbiSectionContributionStream.read!SectionContribEntry;
			writefln("  %s", entry);
		}
		enforce(dbiSectionContributionStream.remainingBytes == 0);

		auto sectionMapHeader = dbiSectionMapStream.read!SectionMapHeader;
		writefln("Section map header %s", sectionMapHeader);
		while(dbiSectionMapStream.remainingBytes) {
			auto entry = dbiSectionMapStream.read!SectionMapEntry;
			writefln("  %s", entry);
		}

		auto sourceInfoHeader = dbiSourceInfoStream.read!SourceInfoHeader;
		writefln("Source info %s", dbiSourceInfoStream.remainingBytes);
		writefln("  num modules %s", sourceInfoHeader.numModules);
		writefln("  num sources %s", sourceInfoHeader.numSourceFiles);
		ushort[] modIndicies = new ushort[sourceInfoHeader.numModules];
		dbiSourceInfoStream.readIntoArray(modIndicies);
		writefln("  mod indicies %s", modIndicies);
		ushort[] modFileCounts = new ushort[sourceInfoHeader.numModules];
		dbiSourceInfoStream.readIntoArray(modFileCounts);
		writefln("  mod file counts %s", modFileCounts);
		uint numSourceFiles;
		foreach(count; modFileCounts)
			numSourceFiles += count;
		uint[] fileNameOffsets = new uint[numSourceFiles];
		dbiSourceInfoStream.readIntoArray(fileNameOffsets);
		writefln("  file name offsets %s", fileNameOffsets);
		foreach(i; 0..numSourceFiles) {
			writefln("    %s %s", i, dbiSourceInfoStream.readZString);
		}
		dbiSourceInfoStream.dropPadding(4);
		enforce(dbiSourceInfoStream.remainingBytes == 0);

		// dbiTypeServerMapStream unknown purpose
		// dbiECSubstreamStream edit and continue in MSVC

		ushort[11] dbgStreamArray;
		dbgStreamArray[] = ushort.max; // init with unknown stream id if less than 11 recors present
		uint numDbgStreamIndicies = min(dbiOptionalDbgHeaderStream.remainingBytes / 2, 11); // we only understand 11 records, ignore if more present
		dbiOptionalDbgHeaderStream.readIntoArray(dbgStreamArray[0..numDbgStreamIndicies]);
		pdb.setStreamName(dbgStreamArray[0], "FPO Data");
		writefln("  FPO Data: %s", dbgStreamArray[0]);
		pdb.setStreamName(dbgStreamArray[1], "Exception Data");
		writefln("  Exception Data: %s", dbgStreamArray[1]);
		pdb.setStreamName(dbgStreamArray[2], "Fixup Data");
		writefln("  Fixup Data: %s", dbgStreamArray[2]);
		pdb.setStreamName(dbgStreamArray[3], "Omap To Src Data");
		writefln("  Omap To Src Data: %s", dbgStreamArray[3]);
		pdb.setStreamName(dbgStreamArray[4], "Omap From Src Data");
		writefln("  Omap From Src Data: %s", dbgStreamArray[4]);
		pdb.setStreamName(dbgStreamArray[5], "Section Header Data");
		writefln("  Section Header Data: %s", dbgStreamArray[5]);
		pdb.setStreamName(dbgStreamArray[6], "Token / RID Map");
		writefln("  Token / RID Map: %s", dbgStreamArray[6]);
		pdb.setStreamName(dbgStreamArray[7], "Xdata");
		writefln("  Xdata: %s", dbgStreamArray[7]);
		pdb.setStreamName(dbgStreamArray[8], "Pdata");
		writefln("  Pdata: %s", dbgStreamArray[8]);
		pdb.setStreamName(dbgStreamArray[9], "New FPO Data");
		writefln("  New FPO Data: %s", dbgStreamArray[9]);
		pdb.setStreamName(dbgStreamArray[10], "Original Section Header Data");
		writefln("  Original Section Header Data: %s", dbgStreamArray[10]);

		pdb.printStreamInfos;

		StreamReader globalStream = streams[pdb.globalStreamIndex];
		writefln("Global sym stream %s: %s bytes", pdb.globalStreamIndex, globalStream.remainingBytes);
		ubyte[] buf = new ubyte[globalStream.remainingBytes];
		globalStream.readIntoArray(buf);
		printHex(buf, 16);
			//enforce(kind == SymbolKind.S_PUB32, "Not S_PUB32 symbol in Public symbol stream");

		StreamReader tpiHashStreamIndex = streams[tpiStreamHeader.hashStreamIndex];
		writefln("TPI hash stream %s: %s bytes", tpiStreamHeader.hashStreamIndex, tpiHashStreamIndex.remainingBytes);
		ubyte[] buf2 = new ubyte[tpiHashStreamIndex.remainingBytes];
		tpiHashStreamIndex.readIntoArray(buf2);
		printHex(buf2, 16);

		StreamReader symRecordStream = streams[pdb.symRecordStream];
		writefln("Symbol record stream %s: %s bytes", pdb.symRecordStream, symRecordStream.remainingBytes);
		parseSymbols(symRecordStream);
		//ubyte[] buf2 = new ubyte[symRecordStream.remainingBytes];
		//symRecordStream.readIntoArray(buf2);
		//printHex(buf2, 16);
	}

	void parseSymbols(ref StreamReader stream)
	{
		writefln("  bytes: %s", stream.remainingBytes);
		while(stream.remainingBytes)
		{
			auto len = stream.read!ushort;
			auto start = stream.streamCursor;
			auto end = start + len;
			SymbolKind kind = stream.read!SymbolKind;

			switch(kind)
			{
				case 0:
					// * Linker * symbols do not terminate with S_END, 0x0000 follows instead
					stream.unread(4);
					return;
				case SymbolKind.S_PUB32:
					auto pubsym = stream.read!PublicSym32;
					string name = stream.readNameBefore(end);
					writefln("  %s [%04X:%08X] flags %04b %s", kind, pubsym.segment, pubsym.offset, pubsym.flags, name);
					break;
				case SymbolKind.S_GPROC32:
					auto procsym = stream.read!ProcSym;
					string name = stream.readNameBefore(end);
					writefln("  %s [%04X:%08X] flags %08b %s", kind, procsym.segment, procsym.offset, procsym.flags, name);
					writefln("    parent %s end %s next %s", procsym.parent, procsym.end, procsym.next);
					writefln("    length %s dbgStart %s dbgEnd %s type %s",
						procsym.length, procsym.dbgStart, procsym.dbgEnd, procsym.typeIndex);
					break;
				case SymbolKind.S_LOCAL:
					auto localsym = stream.read!LocalSym;
					string name = stream.readNameBefore(end);
					writefln("  %s type: %s flags: %s %s", kind, localsym.typeIndex, localsym.flags, name);
					break;
					//SymbolKind.S_COMPILE3
				case SymbolKind.S_SECTION:
					auto sectionsym = stream.read!SectionSym;
					string name = stream.readNameBefore(end);
					writefln("  %s %s align %s rva %s len %s char %s %s",
						kind, sectionsym.section, sectionsym.alignment, sectionsym.rva, sectionsym.length, sectionsym.characteristics, name);
					break;
				case SymbolKind.S_DEFRANGE_REGISTER_REL:
					auto reg = stream.read!DefRangeRegisterRelSym;
					writefln("  %s base reg: %s udt: %s offset in parent: %s base ptr offset %s %s",
						kind, reg.baseReg, reg.spilledUdtMember, reg.offsetInParent, reg.basePointerOffset, reg.range);
					break;
				case SymbolKind.S_PROCREF:
					auto procref = stream.read!ProcRefSym;
					string name = stream.readNameBefore(end);
					writefln("  %s [%04X:%08X] sum name %s %s", kind, procref.mod, procref.symOffset, procref.sumName, name);
					break;
				case SymbolKind.S_END:
					writefln("  END");
					return;
				default:
					string stringBuf2 = new char[len - 2];
					stream.readIntoArray(cast(char[])stringBuf2);
					writefln("  %s `%s`", kind, stringBuf2);
			}

			auto padding = end - stream.streamCursor;
			assert(padding < 4);
			if (padding) writefln("  padding: %s bytes", padding);

			// skip padding
			stream.streamCursor = end;
		}
	}
}

enum FixedStream
{
	old_directory,
	pdb_stream,
	tpi_stream,
	dbi_stream,
	ipi_stream,
}

struct StreamReader
{
	ubyte[] fileData;
	uint[] pages;
	uint pageSize;
	uint streamBytes;
	uint streamCursor = 0;

	void readIntoArray(T)(T[] buf)
	{
		ubyte[] byteBuf = cast(ubyte[])buf;
		while(byteBuf.length > 0)
		{
			size_t pageArrayIndex = streamCursor / pageSize;
			enforce(pageArrayIndex < pages.length, "Reading past last page");
			size_t page = pages[pageArrayIndex];
			size_t pageOffset = streamCursor % pageSize;
			size_t pageFrom = page * pageSize;
			size_t pageTo = pageFrom + pageSize;
			pageFrom += pageOffset;
			ubyte[] pageData = fileData[pageFrom..pageTo];
			size_t pageBytesToRead = min(pageData.length, byteBuf.length);
			enforce(streamCursor + pageBytesToRead <= streamBytes,
				format("attempt to read past the end of stream, %s %s %s",
					streamCursor, pageBytesToRead, streamBytes));
			byteBuf[0..pageBytesToRead] = pageData[0..pageBytesToRead];
			byteBuf.length -= pageBytesToRead;
			streamCursor += pageBytesToRead;
		}
	}

	T read(T)()
	{
		ubyte[T.sizeof] buf;
		readIntoArray(buf);
		return *cast(T*)buf.ptr;
	}

	void unread(uint numBytes)
	{
		streamCursor -= numBytes;
	}

	/// Creates stream reader that contains `byteSize` bytes since current cursor.
	/// Advances cursor of current stream.
	StreamReader substream(uint byteSize)
	{
		StreamReader sub = this;
		sub.streamBytes = streamCursor + byteSize;
		drop(byteSize);
		return sub;
	}

	void drop(size_t bytesToDrop)
	{
		enforce(streamCursor + bytesToDrop <= streamBytes,
				format("attempt to drop past the end of stream, %s %s %s",
					streamCursor, bytesToDrop, streamBytes));
		streamCursor += bytesToDrop;
	}

	uint remainingBytes() {
		return streamBytes - streamCursor;
	}

	bool empty() {
		return remainingBytes == 0;
	}

	void toString(scope void delegate(const(char)[]) sink)
	{
		sink.formattedWrite("pages %s, size %s, cursor %s", pages, streamBytes, streamCursor);
	}

	// reads zero-terminated string of unknown length including zero char. Returns string without zero char.
	string readZString()
	{
		uint cursorCopy = streamCursor;
		while (true)
		{
			char[1] buf;
			readIntoArray(buf);
			char c = buf[0];

			if (c == '\0')
			{
				uint nameLength = streamCursor - cursorCopy - 1;
				streamCursor = cursorCopy; // restore cursor
				string str = new char[nameLength];
				readIntoArray(cast(char[])str);
				drop(1); // skip 0
				return str;
			}
		}
	}

	string readNameBefore(uint end)
	{
		uint nameLength = end - streamCursor;
		string name = new char[nameLength];
		readIntoArray(cast(char[])name);
		while (name[$-1] != '\0')
			name = name[0..$-1]; // peel padding
		while (name[$-1] == '\0')
			name = name[0..$-1]; // peel zero terminator
		return name;
	}

	// alignment is PoT
	void dropPadding(uint alignment) {
		uint padding = paddingSize(streamCursor, alignment);
		drop(padding);
	}
}

ubyte[32] MsfMagic = [
	0x4D, 0x69, 0x63, 0x72, 0x6F, 0x73, 0x6F, 0x66, // Microsof
	0x74, 0x20, 0x43, 0x2F, 0x43, 0x2B, 0x2B, 0x20, // t C/C++
	0x4D, 0x53, 0x46, 0x20, 0x37, 0x2E, 0x30, 0x30, // MSF 7.00
	0x0D, 0x0A, 0x1A, 0x44, 0x53, 0x00, 0x00, 0x00, // ...DS...
];

bool isValidPageSize(uint pageSize)
{
	return pageSize ==  512 ||
		   pageSize == 1024 ||
		   pageSize == 2048 ||
		   pageSize == 4096;
}

uint[4] validPageSizes = [512, 1024, 2048, 4096];

struct MsfHeader
{
	uint pageSize;
	uint freePageBitmapIndex;
	/// Total size of MSF file is numPages * pageSize
	uint numPages;
	/// Length of stream that stores stream infos
	uint metaStreamBytes;
	uint _reserved;
	uint metaStreamHeaderPage;

	uint numMetaStreamPages() {
		return divCeil(metaStreamBytes, pageSize);
	}

	void toString(scope void delegate(const(char)[]) sink)
	{
		sink.formattedWrite("page size 0x%X\n", pageSize);
		sink.formattedWrite("number of pages %s\n", numPages);
		sink.formattedWrite("free page bitmap index %s\n", freePageBitmapIndex);
		sink.formattedWrite("page map index %s\n", metaStreamHeaderPage);
		sink.formattedWrite("meta-stream bytes %s\n", metaStreamBytes);
	}

	void validate()
	{
		enforce(isValidPageSize(pageSize),
			format("Invalid page size %s. Valid page sizes are %s",
				pageSize, validPageSizes));
		enforce(freePageBitmapIndex < numPages,
			format("Free page map index (%s) out of bounds. Number of pages is %s",
				freePageBitmapIndex, numPages));
		enforce(freePageBitmapIndex == 1 || freePageBitmapIndex == 2,
			format("Free page map index must be 1 or 2, not %s", freePageBitmapIndex));
		enforce(metaStreamHeaderPage < numPages,
			format("Page map index (%s) out of bounds. Number of pages is %s",
				metaStreamHeaderPage, numPages));
		enforce(metaStreamHeaderPage != 0, "Page map index cannot be located in page 0");

		uint directoryBytes = cast(uint)(numMetaStreamPages * uint.sizeof);
		enforce(directoryBytes <= pageSize,
			format("Array of directory pages (%s pages, %s bytes) cannot fit into single page (%s bytes)",
				numMetaStreamPages, directoryBytes, pageSize));
	}
}

struct PdbStreamHeader {
	uint ver;
	uint signature;
	uint age;
	ubyte[16] guid;
}

enum PdbStreamVersion : uint {
	VC2 = 19941610,
	VC4 = 19950623,
	VC41 = 19950814,
	VC50 = 19960307,
	VC98 = 19970604,
	VC70Dep = 19990604,
	VC70 = 20000404,
	VC80 = 20030901,
	VC110 = 20091201,
	VC140 = 20140508,
}

enum Pdb_FeatureCodeMagic : uint {
	vc110 = 20091201,
	vc140 = 20140508,
	noTypeMerge = 0x4D544F4E,
	minimalDebugInfo = 0x494E494D,
};

enum Pdb_FeatureFlags : uint {
	vc110 = 1 << 0,
	vc140 = 1 << 1,
	noTypeMerge = 1 << 2,
	minimalDebugInfo = 1 << 3,
};

struct TpiStreamHeader {
	uint ver;
	uint headerSize;
	uint typeIndexBegin;
	uint typeIndexEnd;
	uint typeRecordBytes;

	ushort hashStreamIndex;
	ushort hashAuxStreamIndex;
	uint hashKeySize;
	uint numHashBuckets;

	int hashValueBufferOffset;
	uint hashValueBufferLength;

	int indexOffsetBufferOffset;
	uint indexOffsetBufferLength;

	int hashAdjBufferOffset;
	uint hashAdjBufferLength;
}

struct DbiStreamHeader {
	int VersionSignature;
	uint VersionHeader;
	uint Age;
	ushort GlobalStreamIndex;
	ushort BuildNumber;
	ushort PublicStreamIndex;
	ushort PdbDllVersion;
	ushort SymRecordStream;
	ushort PdbDllRbld;
	int ModInfoSize;
	int SectionContributionSize;
	int SectionMapSize;
	int SourceInfoSize;
	int TypeServerMapSize;
	uint MFCTypeServerIndex;
	int OptionalDbgHeaderSize;
	int ECSubstreamSize;
	ushort Flags;
	ushort Machine;
	uint Padding;
}
static assert(DbiStreamHeader.sizeof == 64);

enum COFF_DEBUG_SECTION_MAGIC = 4;

enum DebugSubsectionKind : uint {
	none = 0,
	symbols = 0xf1,
	lines = 0xf2,
	stringTable = 0xf3,
	fileChecksums = 0xf4,
	frameData = 0xf5,
	inlineeLines = 0xf6,
	crossScopeImports = 0xf7,
	crossScopeExports = 0xf8,
	iLLines = 0xf9,
	funcMDTokenMap = 0xfa,
	typeMDTokenMap = 0xfb,
	mergedAssemblyInput = 0xfc,
	coffSymbolRVA = 0xfd,
}

struct DebugSubsectionHeader
{
	DebugSubsectionKind kind;
	uint length;
}

struct ModInfo {
	uint Unused1;
	SectionContribEntry SectionContr;
	ushort Flags;
	ushort ModuleSymStream;
	uint SymByteSize;
	uint C11ByteSize;
	uint C13ByteSize;
	ushort SourceFileCount;
	ubyte[2] Padding;
	uint Unused2;
	uint SourceFileNameIndex;
	uint PdbFilePathNameIndex;
	/// two zero-terminated strings follow. They are padded to 4 byte alignment
	//char ModuleName[];
	//char ObjFileName[];
}
static assert(ModInfo.sizeof == 64);

enum SectionContrSubstreamVersion : uint {
	Ver60 = 0xeffe0000 + 19970605,
	V2 = 0xeffe0000 + 20140516
}

struct SectionContribEntry
{
	ushort section;
	int    offset;
	int    size;
	uint   characteristics;
	ushort moduleIndex;
	uint   dataCrc;
	uint   relocCrc;
}
static assert(SectionContribEntry.sizeof == 28);

struct SectionContribution2
{
	SectionContribEntry sc;
	uint CoffSectionIndex;
}

struct SectionMapHeader
{
	ushort count;    // Number of segment descriptors
	ushort logCount; // Number of logical segment descriptors
}

struct SectionMapEntry
{
	ushort flags;         // See the SectionMapEntryFlags enum below.
	ushort ovl;           // Logical overlay number
	ushort group;         // Group index into descriptor array.
	ushort frame;
	ushort sectionName;   // Byte index of segment / group name in string table, or 0xFFFF.
	ushort className;     // Byte index of class in string table, or 0xFFFF.
	uint   offset;        // Byte offset of the logical segment within physical segment.  If group is set in flags, this is the offset of the group.
	uint   sectionLength; // Byte count of the segment or group.
}

enum SectionMapEntryFlags : ushort {
  read = 1 << 0,              // Segment is readable.
  write = 1 << 1,             // Segment is writable.
  execute = 1 << 2,           // Segment is executable.
  addressIs32Bit = 1 << 3,    // Descriptor describes a 32-bit linear address.
  isSelector = 1 << 8,        // Frame represents a selector.
  isAbsoluteAddress = 1 << 9, // Frame represents an absolute address.
  isGroup = 1 << 10           // If set, descriptor represents a group.
}

struct SourceInfoHeader {
	ushort numModules;
	ushort numSourceFiles;

	// following are variable size arrays
	// ushort ModIndices[numModules];
	// ushort ModFileCounts[numModules];
	// uint FileNameOffsets[numSourceFiles];
	// char NamesBuffer[][numSourceFiles];
};

enum PointerKind : ubyte {
	near16 = 0x00,                // 16 bit pointer
	far16 = 0x01,                 // 16:16 far pointer
	huge16 = 0x02,                // 16:16 huge pointer
	basedOnSegment = 0x03,        // based on segment
	basedOnValue = 0x04,          // based on value of base
	basedOnSegmentValue = 0x05,   // based on segment value of base
	basedOnAddress = 0x06,        // based on address of base
	basedOnSegmentAddress = 0x07, // based on segment address of base
	basedOnType = 0x08,           // based on type
	basedOnSelf = 0x09,           // based on self
	near32 = 0x0a,                // 32 bit pointer
	far32 = 0x0b,                 // 16:32 pointer
	near64 = 0x0c                 // 64 bit pointer
}
enum PointerMode : ubyte {
	pointer = 0x00,                 // "normal" pointer
	lValueReference = 0x01,         // "old" reference
	pointerToDataMember = 0x02,     // pointer to data member
	pointerToMemberFunction = 0x03, // pointer to member function
	rValueReference = 0x04          // r-value reference
}
enum PointerModifiers : ubyte {
	none = 0x00,                    // "normal" pointer
	flat32 = 0x01,                  // "flat" pointer
	volatile = 0x02,                // pointer is marked volatile
	constant = 0x04,                // pointer is marked const
	unaligned = 0x08,               // pointer is marked unaligned
	restrict = 0x10,                // pointer is marked restrict
}
enum PointerFlags : ubyte {
	winRTSmartPointer = 0x01,       // pointer is a WinRT smart pointer
	lValueRefThisPointer = 0x02,    // pointer is a 'this' pointer of a member function with ref qualifier (e.g. void X::foo() &)
	rValueRefThisPointer = 0x04     // pointer is a 'this' pointer of a member function with ref qualifier (e.g. void X::foo() &&)
}

enum TypeRecordKind : ushort {
	// Those are found in TPI stream
	LF_POINTER          = 0x1002,
	LF_MODIFIER         = 0x1001,
	LF_PROCEDURE        = 0x1008,
	LF_MFUNCTION        = 0x1009,
	LF_LABEL            = 0x000e,
	LF_ARGLIST          = 0x1201,
	LF_FIELDLIST        = 0x1203,
	LF_ARRAY            = 0x1503,
	LF_CLASS            = 0x1504,

	// Those are found in IPI stream
	LF_FUNC_ID          = 0x1601,    // global func ID
	LF_MFUNC_ID         = 0x1602,    // member func ID
	LF_BUILDINFO        = 0x1603,    // build info: tool, version, command line, src/pdb file
	LF_SUBSTR_LIST      = 0x1604,    // similar to LF_ARGLIST, for list of sub strings
	LF_STRING_ID        = 0x1605,    // string ID
}

/// http://llvm.org/docs/PDB/TpiStream.html#id4
/// 32bit index
struct TypeIndex
{
	union {
		uint asUint;
		mixin(bitfields!(
			SimpleTypeKind, "kind",            8,
			SimpleTypeMode, "mode",            4,
			uint,            "",              19,
			bool,            "isInIPIStream",  1
		));
	}

	void toString(scope void delegate(const(char)[]) sink)
	{
		TypeIndex copy = this;
		copy.isInIPIStream = false;
		if (copy.asUint >= 0x1000)
		{
			if (isInIPIStream)
			{
				sink.formattedWrite("IPI:%s", asUint - 0x1000);
			}
			else
			{
				sink.formattedWrite("TPI:%s", asUint - 0x1000);
			}
		}
		else
		{
			sink.formattedWrite("%s:%s", kind, mode);
		}
	}
}

enum SimpleTypeKind : ushort {
	None = 0x0000,          // uncharacterized type (no type)
	Void = 0x0003,          // void
	NotTranslated = 0x0007, // type not translated by cvpack
	HResult = 0x0008,       // OLE/COM HRESULT

	SignedCharacter = 0x0010,   // 8 bit signed
	UnsignedCharacter = 0x0020, // 8 bit unsigned
	NarrowCharacter = 0x0070,   // really a char
	WideCharacter = 0x0071,     // wide char
	Character16 = 0x007a,       // char16_t
	Character32 = 0x007b,       // char32_t

	SByte = 0x0068,       // 8 bit signed int
	Byte = 0x0069,        // 8 bit unsigned int
	Int16Short = 0x0011,  // 16 bit signed
	UInt16Short = 0x0021, // 16 bit unsigned
	Int16 = 0x0072,       // 16 bit signed int
	UInt16 = 0x0073,      // 16 bit unsigned int
	Int32Long = 0x0012,   // 32 bit signed
	UInt32Long = 0x0022,  // 32 bit unsigned
	Int32 = 0x0074,       // 32 bit signed int
	UInt32 = 0x0075,      // 32 bit unsigned int
	Int64Quad = 0x0013,   // 64 bit signed
	UInt64Quad = 0x0023,  // 64 bit unsigned
	Int64 = 0x0076,       // 64 bit signed int
	UInt64 = 0x0077,      // 64 bit unsigned int
	Int128Oct = 0x0014,   // 128 bit signed int
	UInt128Oct = 0x0024,  // 128 bit unsigned int
	Int128 = 0x0078,      // 128 bit signed int
	UInt128 = 0x0079,     // 128 bit unsigned int

	Float16 = 0x0046,                 // 16 bit real
	Float32 = 0x0040,                 // 32 bit real
	Float32PartialPrecision = 0x0045, // 32 bit PP real
	Float48 = 0x0044,                 // 48 bit real
	Float64 = 0x0041,                 // 64 bit real
	Float80 = 0x0042,                 // 80 bit real
	Float128 = 0x0043,                // 128 bit real

	Complex16 = 0x0056,                 // 16 bit complex
	Complex32 = 0x0050,                 // 32 bit complex
	Complex32PartialPrecision = 0x0055, // 32 bit PP complex
	Complex48 = 0x0054,                 // 48 bit complex
	Complex64 = 0x0051,                 // 64 bit complex
	Complex80 = 0x0052,                 // 80 bit complex
	Complex128 = 0x0053,                // 128 bit complex

	Boolean8 = 0x0030,   // 8 bit boolean
	Boolean16 = 0x0031,  // 16 bit boolean
	Boolean32 = 0x0032,  // 32 bit boolean
	Boolean64 = 0x0033,  // 64 bit boolean
	Boolean128 = 0x0034, // 128 bit boolean
}

enum SimpleTypeMode : ubyte {
	Direct = 0,        // Not a pointer
	NearPointer = 1,   // Near pointer
	FarPointer = 2,    // Far pointer
	HugePointer = 3,   // Huge pointer
	NearPointer32 = 4, // 32 bit near pointer
	FarPointer32 = 5,  // 32 bit far pointer
	NearPointer64 = 6, // 64 bit near pointer
	NearPointer128 = 7 // 128 bit near pointer
};



/// Leaf record types
/// 32 bit types are the same as 16 bit but have 0x1000 bit set

// LF_SKIP
struct CVType_SKIP
{
	uint type; // next valid index
}

// LF_ARGLIST, LF_SUBSTR_LIST
struct CVType_ARGLIST
{
	uint count; // number of arguments
	// next follow `count` type indicies of type TypeIndex
}

// LF_DERIVED

// LF_PROCEDURE
struct CVType_PROCEDURE
{
	TypeIndex returnType;
	CV_CallConv callConv;
	ubyte funcAttributes;
	ushort numParams;
	TypeIndex argList;
}

enum CV_CallConv : ubyte {
	NEAR_C      = 0x00, // near right to left push, caller pops stack
	FAR_C       = 0x01, // far right to left push, caller pops stack
	NEAR_PASCAL = 0x02, // near left to right push, callee pops stack
	FAR_PASCAL  = 0x03, // far left to right push, callee pops stack
	NEAR_FAST   = 0x04, // near left to right push with regs, callee pops stack
	FAR_FAST    = 0x05, // far left to right push with regs, callee pops stack
	SKIPPED     = 0x06, // skipped (unused) call index
	NEAR_STD    = 0x07, // near standard call
	FAR_STD     = 0x08, // far standard call
	NEAR_SYS    = 0x09, // near sys call
	FAR_SYS     = 0x0a, // far sys call
	THISCALL    = 0x0b, // this call (this passed in register)
	MIPSCALL    = 0x0c, // Mips call
	GENERIC     = 0x0d, // Generic call sequence
	ALPHACALL   = 0x0e, // Alpha call
	PPCCALL     = 0x0f, // PPC call
	SHCALL      = 0x10, // Hitachi SuperH call
	ARMCALL     = 0x11, // ARM call
	AM33CALL    = 0x12, // AM33 call
	TRICALL     = 0x13, // TriCore Call
	SH5CALL     = 0x14, // Hitachi SuperH-5 call
	M32RCALL    = 0x15, // M32R Call
	CLRCALL     = 0x16, // clr call
	INLINE      = 0x17, // Marker for routines always inlined and thus lacking a convention
	NEAR_VECTOR = 0x18, // near left to right push with regs, callee pops stack
	RESERVED    = 0x19  // first unused call enumeration

	// Do NOT add any more machine specific conventions.  This is to be used for
	// calling conventions in the source only (e.g. __cdecl, __stdcall).
}

// LF_FUNC_ID
struct CVType_FUNC_ID
{
	uint scopeId;   // parent scope of the ID, 0 if global
	TypeIndex type; // function type
	// zero terminated string follows
}

// LF_STRING_ID
struct CVType_STRING_ID
{
	uint id; // ID to list of sub string IDs
	// zero terminated string follows
}


// LF_BUILDINFO
struct CVType_BUILDINFO
{
    ushort count; // number of arguments
    TypeIndex[CV_BuildInfo.KNOWN] args;
}

enum CV_BuildInfo
{
    CurrentDirectory    = 0,
    BuildTool           = 1, // Cl.exe
    SourceFile          = 2, // foo.cpp
    ProgramDatabaseFile = 3, // foo.pdb
    CommandArguments    = 4, // -I etc
    KNOWN
}

// --------------------------------------------
// Symbols

/// Header of the hash tables found in the globals and publics sections.
// GSIHashHdr
struct GSIHashHeader
{
	uint verSignature = 0xFFFF_FFFF;
	uint verHdr = 0xeffe0000 + 19990810;
	uint hrSize;
	uint numBuckets;
}

// PSGSIHDR
struct PublicsStreamHeader
{
	uint symHash;
	uint addrMap;
	uint numThunks;
	uint sizeOfThunk;
	ushort isectThunkTable;
	ubyte[2] padding;
	uint offThunkTable;
	uint numSections;
}
