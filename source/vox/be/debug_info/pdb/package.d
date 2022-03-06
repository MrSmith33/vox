/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// .pdb reader and writer
/// PDB docs http://llvm.org/docs/PDB/index.html
/// MSF docs http://llvm.org/docs/PDB/MsfFile.html
/// CodeView http://pierrelib.pagesperso-orange.fr/exec_formats/MS_Symbol_Type_v1.0.pdf
/// https://github.com/mountainstorm/pdbfile
/// https://github.com/willglynn/pdb
/// https://github.com/smx-smx/PDBSharp

/// todo:
///   - S_BPREL32
///   - C13 DEBUG SUBSECTIONS: fileChecksums, line numbers, GlobalRefs
///   - TPI hash stream, IPI hash stream
module vox.be.debug_info.pdb;

public import vox.be.debug_info.pdb.symbol;
public import vox.be.debug_info.pdb.codeview;

import std.bitmanip : bitfields;
import std.file;
import std.format : formattedWrite;
import std.path;
import std.range : repeat;
import std.string : fromStringz;

import vox.utils;

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
	Array of indices to those pages are stored in a page `metaStreamHeaderPage`
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
		enforce(streamInfos[index].name is null,
			format("stream %s, already has name '%s', trying to set to '%s'",
				index, streamInfos[index].name, name));
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
	string[100] longestNames;

	void visitString(string str)
	{
		foreach(ref longStr; longestNames)
			if (str.length > longStr.length)
				swap(longStr, str);
	}

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
		writefln("Load %s %s bytes", filename, fileData.length);

		auto slicer = FileDataSlicer(fileData);

		// MSF Magic
		enforce(slicer.fileData.length >= MsfMagic.length, format("%s: Error: Invalid PDB file, MSF magic is truncated", filename));
		ubyte[] msfMagic = slicer.getArrayOf!ubyte(MsfMagic.length);
		enforce(msfMagic == MsfMagic, "Invalid magic");

		// MSF Header
		pdb.msfHeader = *slicer.getPtrTo!MsfHeader;
		pdb.msfHeader.validate;
		enforce(pdb.msfHeader.numPages * pdb.msfHeader.pageSize == fileData.length,
			format("Invalid file size (%s) != num pages (%s) * page size (%s)",
				fileData.length, pdb.msfHeader.numPages, pdb.msfHeader.pageSize));

		writeln(pdb.msfHeader);

		// Meta stream contains info about all streams
		slicer.fileCursor = pdb.msfHeader.metaStreamHeaderPage * pdb.msfHeader.pageSize;
		writefln("meta stream header page %s, offset 0x%X", pdb.msfHeader.metaStreamHeaderPage, slicer.fileCursor);

		uint[] metaStreamPagesIndices = slicer.getArrayOf!uint(pdb.msfHeader.numMetaStreamPages);
		writefln("meta stream pages %s", metaStreamPagesIndices);

		StreamReader metaStream;
		metaStream.fileData = fileData;
		metaStream.streamBytes = pdb.msfHeader.metaStreamBytes;
		metaStream.pageSize = pdb.msfHeader.pageSize;
		metaStream.pages = metaStreamPagesIndices;

		uint numStreams = metaStream.read!uint;
		writefln("Number of streams %s", numStreams);

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
		writeln;


		writefln("Streams:");
		foreach(i, ref stream; streams)
		{
			writefln("  %s %s", i, stream);
		}
		writeln;


		writefln("#   PDB stream %s", cast(uint)FixedStream.pdb_stream);
		StreamReader* pdbStream = &streams[FixedStream.pdb_stream];
		auto pdbStreamHeader = pdbStream.read!PdbStreamHeader;
		writefln("PDB stream header %s", pdbStreamHeader);

		writefln("  Named streams hashmap");
		uint stringBufLength = pdbStream.read!uint;
		string namedStreamStringBuf = cast(string)pdbStream.readArray!char(stringBufLength);
		writefln("    string buffer %s", namedStreamStringBuf);

		// size == number of present keys == number of present values
		uint hashmapSize = pdbStream.read!uint;
		// capacity == number of keys + number of values
		uint hashmapCapacity = pdbStream.read!uint;
		writefln("    size %s", hashmapSize);
		writefln("    capacity %s", hashmapCapacity);

		uint presentBitmapWords = pdbStream.read!uint;
		uint[] presentBitmap = pdbStream.readArray!uint(presentBitmapWords);
		writefln("    presentBitmap %s", presentBitmap);

		uint deletedBitmapWords = pdbStream.read!uint;
		uint[] deletedBitmap = pdbStream.readArray!uint(deletedBitmapWords);
		writefln("    deletedBitmap %s", deletedBitmap);

		writefln("    buckets:");

		// gather named streams
		// iterate set bits in uint
		// for each bit set an entry follows
		foreach(size_t bucketIndex; presentBitmap.bitsSet)
		{
			uint nameBufOffset = pdbStream.read!uint;
			uint streamIndex = pdbStream.read!uint;

			string name = namedStreamStringBuf[nameBufOffset..$].ptr.fromStringz;
			writefln("  % 4s %s", streamIndex, name);

			switch(name)
			{
				case "/LinkInfo": pdb.linkInfoStreamIndex = streamIndex; break;
				case "/src/headerblock": pdb.headerblockStreamIndex = streamIndex; break;
				case "/names": pdb.namesStreamIndex = streamIndex; break;
				default: break; // ignore unknown
			}

			pdb.setStreamName(streamIndex, name);
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
				default:
					writef(" feature(0x%x)", featureCode);
					break; // ignore unknown feature
			}
		}
		writeln;
		writeln;


		StreamReader* tpiStream = &streams[FixedStream.tpi_stream];
		writefln("#   TPI stream %s, %s bytes", cast(uint)FixedStream.tpi_stream, tpiStream.remainingBytes);
		auto tpiStreamHeader = tpiStream.read!TpiStreamHeader;
		tpiStreamHeader.print;
		pdb.setStreamName(tpiStreamHeader.hashStreamIndex, "TPI Hash stream");
		pdb.setStreamName(tpiStreamHeader.hashAuxStreamIndex, "TPI Hash aux stream");

		size_t typeIndex;
		while(tpiStream.remainingBytes)
		{
			auto len = tpiStream.read!ushort;
			auto start = tpiStream.streamCursor;
			auto end = start + len;

			auto kind = tpiStream.read!ushort;
			writefln("- TPI:%s %s %s bytes", typeIndex, cast(TypeRecordKind)kind, len);

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
		writeln;


		StreamReader* ipiStream = &streams[FixedStream.ipi_stream];
		writefln("#   IPI stream header %s, %s bytes", cast(uint)FixedStream.ipi_stream, ipiStream.remainingBytes);
		auto ipiStreamHeader = ipiStream.read!TpiStreamHeader;
		ipiStreamHeader.print;
		pdb.setStreamName(ipiStreamHeader.hashStreamIndex, "IPI Hash stream");
		pdb.setStreamName(ipiStreamHeader.hashAuxStreamIndex, "IPI Hash aux stream");

		while(ipiStream.remainingBytes)
		{
			auto start = ipiStream.streamCursor;
			auto len = ipiStream.read!ushort;
			auto end = start + len + 2;

			auto kind = ipiStream.read!ushort;
			writefln("- IPI:%s (%06X) length %s kind %s", typeIndex + 0x1000, start, len, cast(TypeRecordKind)kind);

			switch(kind) with(TypeRecordKind)
			{
				case LF_FUNC_ID:
					auto funcId = ipiStream.read!CVType_FUNC_ID;
					writefln("    scope id %s", funcId.scopeId);
					writefln("    type %s, size %s", funcId.type, CVType_FUNC_ID.sizeof);
					string name = ipiStream.readNameBefore(end);
					visitString(name);
					writefln("    name `%s`", name);
					break;

				case LF_MFUNC_ID:
					auto funcId = ipiStream.read!CVType_MFUNC_ID;
					writefln("    type %s", funcId.type);
					writefln("    parent type %s", funcId.parentType);
					string name = ipiStream.readNameBefore(end);
					visitString(name);
					writefln("    name `%s`", name);
					break;

				case LF_STRING_ID:
					auto stringId = ipiStream.read!CVType_STRING_ID;
					writefln("    id 0x%X", stringId.id);
					string name = ipiStream.readNameBefore(end);
					visitString(name);
					writefln("    name `%s`", name);
					break;

				case LF_UDT_SRC_LINE:
					auto udtSrcLine = ipiStream.read!UdtSrcLine;
					writefln("    type = %s, source file = %s, line = %s", udtSrcLine.udt, udtSrcLine.sourceFile, udtSrcLine.lineNumber);
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
					break;
			}

			auto padding = end - ipiStream.streamCursor;
			assert(padding < 4);
			if (padding) writefln("  padding: %s bytes", padding);

			// skip padding
			ipiStream.streamCursor = end;

			++typeIndex;
		}
		writeln;


		StreamReader* dbiStream = &streams[FixedStream.dbi_stream];
		auto dbiStreamHeader = dbiStream.read!DbiStreamHeader;
		writefln("#   DBI stream %s, %s bytes", cast(uint)FixedStream.dbi_stream, dbiStream.remainingBytes);
		writefln("  DbiStreamHeader %s", dbiStreamHeader);

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
		StreamReader dbiSectionContributionStream = dbiStream.substream(dbiStreamHeader.SectionContributionSize);
		StreamReader dbiSectionMapStream = dbiStream.substream(dbiStreamHeader.SectionMapSize);
		StreamReader dbiSourceInfoStream = dbiStream.substream(dbiStreamHeader.SourceInfoSize);
		StreamReader dbiTypeServerMapStream = dbiStream.substream(dbiStreamHeader.TypeServerMapSize);
		StreamReader dbiECSubstreamStream = dbiStream.substream(dbiStreamHeader.ECSubstreamSize);
		StreamReader dbiOptionalDbgHeaderStream = dbiStream.substream(dbiStreamHeader.OptionalDbgHeaderSize);
		enforce(dbiStream.remainingBytes == 0);
		writeln;


		writefln("##  dbiModInfo substream: %s bytes", dbiModInfoStream.remainingBytes);
		while(dbiModInfoStream.remainingBytes)
		{
			auto modInfoStart = dbiModInfoStream.streamCursor;
			auto modInfo = dbiModInfoStream.read!ModInfo;
			writefln("(%06X) ModInfo", modInfoStart);
			string moduleName = dbiModInfoStream.readZString;
			pdb.setStreamName(modInfo.ModuleSymStream, format("module sym stream: %s", moduleName));
			writefln("    moduleName %s", moduleName);
			string objFileName = dbiModInfoStream.readZString;
			writefln("    objFileName %s", objFileName);
			dbiModInfoStream.dropPadding(4);
			writefln("    % 8s module sym stream", modInfo.ModuleSymStream);
			writefln("    %04X Flags", modInfo.Flags);
			writefln("    SectionContr %s", modInfo.SectionContr);
			writefln("    % 8s SymByteSize", modInfo.SymByteSize);
			writefln("    % 8s C11ByteSize", modInfo.C11ByteSize);
			writefln("    % 8s C13ByteSize", modInfo.C13ByteSize);
			writefln("    % 8s SourceFileCount", modInfo.SourceFileCount);
			writefln("    % 8s SourceFileNameIndex", modInfo.SourceFileNameIndex);
			writefln("    % 8s PdbFilePathNameIndex", modInfo.PdbFilePathNameIndex);

			if (modInfo.ModuleSymStream != ushort.max)
			{
				StreamReader modSymStream = streams[modInfo.ModuleSymStream];
				writefln("#   Module symbol stream %s: %s bytes", modInfo.ModuleSymStream, modSymStream.remainingBytes);

				// SYMBOLS
				StreamReader symSubstream = modSymStream.substream(modInfo.SymByteSize);
				auto debugMagic = symSubstream.read!uint;
				enforce(debugMagic == COFF_DEBUG_SECTION_MAGIC,
					format("Invalid magic (%s), expected %s",
						debugMagic, COFF_DEBUG_SECTION_MAGIC));
				parseSymbols(symSubstream);
				enforce(symSubstream.remainingBytes == 0);
				writeln;

				// C11, legacy
				StreamReader c11Substream = modSymStream.substream(modInfo.C11ByteSize);
				if (c11Substream.remainingBytes) {
					writefln("--- C11 DEBUG SUBSECTIONS: %s bytes ---", modInfo.C11ByteSize);
					printHex(c11Substream.readArray!ubyte(c11Substream.remainingBytes), 16, PrintAscii.yes);
				}

				// C13
				StreamReader c13Substream = modSymStream.substream(modInfo.C13ByteSize);
				if (c13Substream.remainingBytes) {
					writefln("--- C13 DEBUG SUBSECTIONS: %s bytes ---", modInfo.C13ByteSize);
					// read C13 debug subsections
					while(c13Substream.remainingBytes)
					{
						// read DebugSubsectionHeader
						auto kind = c13Substream.read!DebugSubsectionKind;
						if (kind == DebugSubsectionKind.none) break; // last entry
						auto length = c13Substream.read!uint;
						writefln("  - %s: %s bytes", kind, length);
						printHex(c13Substream.readArray!ubyte(length), 16, PrintAscii.yes);
					}
					enforce(c13Substream.remainingBytes == 0);
				}

				// GlobalRefs, unknown purpose
				uint GlobalRefsSize = modSymStream.read!uint;
				if (GlobalRefsSize) {
					writefln("--- GlobalRefs: %s bytes ---", GlobalRefsSize);
					printHex(modSymStream.readArray!ubyte(GlobalRefsSize), 16, PrintAscii.yes);
				}

				enforce(modSymStream.remainingBytes == 0);
			}
			writeln;
		}
		enforce(dbiModInfoStream.remainingBytes == 0);
		writeln;


		writefln("##  dbiSectionContribution substream: %s bytes", dbiSectionContributionStream.remainingBytes);
		auto secContribVer = dbiSectionContributionStream.read!SectionContrSubstreamVersion;
		writefln("  Section Contr Substream Version %s", secContribVer);
		enforce(secContribVer == SectionContrSubstreamVersion.Ver60, "only SectionContrSubstreamVersion.Ver60 is supported");
		while(dbiSectionContributionStream.remainingBytes) {
			auto entry = dbiSectionContributionStream.read!SectionContribEntry;
			writefln("  %s", entry);
		}
		enforce(dbiSectionContributionStream.remainingBytes == 0);
		writeln;


		writefln("##  dbiSectionMap substream: %s bytes", dbiSectionMapStream.remainingBytes);
		auto sectionMapHeader = dbiSectionMapStream.read!SectionMapHeader;
		writefln("Section map header %s", sectionMapHeader);
		while(dbiSectionMapStream.remainingBytes) {
			auto entry = dbiSectionMapStream.read!SectionMapEntry;
			writefln("  %s", entry);
		}
		writeln;

		//writefln("Longest string:");
		//import std.algorithm : uniq;
		//foreach(str; longestNames[].uniq) writeln(str);

		writefln("##  dbiSourceInfo substream: %s bytes", dbiSourceInfoStream.remainingBytes);
		auto sourceInfoHeader = dbiSourceInfoStream.read!SourceInfoHeader;
		writefln("Source info %s", dbiSourceInfoStream.remainingBytes);
		writefln("  num modules %s", sourceInfoHeader.numModules);
		writefln("  num sources %s", sourceInfoHeader.numSourceFiles);

		ushort[] modIndices = new ushort[sourceInfoHeader.numModules];
		dbiSourceInfoStream.readIntoArray(modIndices);
		writefln("  mod indices %s", modIndices);

		ushort[] modFileCounts = new ushort[sourceInfoHeader.numModules];
		dbiSourceInfoStream.readIntoArray(modFileCounts);
		writefln("  mod file counts %s", modFileCounts);

		uint numSourceFiles;
		foreach(count; modFileCounts)
			numSourceFiles += count;
		uint[] fileNameOffsets = new uint[numSourceFiles];
		dbiSourceInfoStream.readIntoArray(fileNameOffsets);
		writefln("  File name offsets %s", fileNameOffsets);

		{
			writefln("  File name strings:");
			uint numStrings;
			uint stringsStart = dbiSourceInfoStream.streamCursor;
			while (!dbiSourceInfoStream.empty)
			{
				++numStrings;
				uint offset = dbiSourceInfoStream.streamCursor - stringsStart;
				writefln("    %s %s", offset, dbiSourceInfoStream.readZString);
			}
			writefln("  Num name strings: %s", numStrings);
		}
		dbiSourceInfoStream.dropPadding(4);
		enforce(dbiSourceInfoStream.remainingBytes == 0);
		writeln;


		writefln("##  dbiTypeServerMap substream: %s bytes", dbiTypeServerMapStream.remainingBytes);
		// dbiTypeServerMapStream unknown purpose
		writefln("##  dbiECSubstream subtream: %s bytes", dbiECSubstreamStream.remainingBytes);
		// dbiECSubstreamStream edit and continue in MSVC


		writefln("##  dbiOptionalDbgHeader subtream: %s bytes", dbiOptionalDbgHeaderStream.remainingBytes);
		ushort[11] dbgStreamArray;
		dbgStreamArray[] = ushort.max; // init with unknown stream id if less than 11 recors present
		uint numDbgStreamIndices = min(dbiOptionalDbgHeaderStream.remainingBytes / 2, 11); // we only understand 11 records, ignore if more present
		dbiOptionalDbgHeaderStream.readIntoArray(dbgStreamArray[0..numDbgStreamIndices]);
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
		writeln;


		writefln("Stream names:");
		pdb.printStreamInfos;
		writeln;


		StreamReader globalStream = streams[pdb.globalStreamIndex];
		writefln("#   Globals symbol stream %s: %s bytes", pdb.globalStreamIndex, globalStream.remainingBytes);
		readGSIHashTable(globalStream);
		enforce(globalStream.remainingBytes == 0);
		writeln;


		// Publics stream
		StreamReader publicStream = streams[pdb.publicStreamIndex];
		writefln("#   Publics sym stream %s: %s bytes", pdb.publicStreamIndex, publicStream.remainingBytes);
		enforce(publicStream.remainingBytes >= PublicsStreamHeader.sizeof,
			format("Cannot read PublicsStreamHeader from Publics stream, not enough bytes left (%s)",
			publicStream.remainingBytes));
		auto publicsHeader = publicStream.read!PublicsStreamHeader;
		writefln("  PublicsStreamHeader:");
		writefln("    symHash %s", publicsHeader.symHash);
		writefln("    address map bytes %s", publicsHeader.addrMap);
		writefln("    numThunks %s", publicsHeader.numThunks);
		writefln("    sizeOfThunk %s", publicsHeader.sizeOfThunk);
		writefln("    isectThunkTable %s", publicsHeader.isectThunkTable);
		writefln("    offThunkTable %s", publicsHeader.offThunkTable);
		writefln("    numSections %s", publicsHeader.numSections);
		readGSIHashTable(publicStream);
		uint[] addressMap = publicStream.readArray!uint(publicsHeader.addrMap / uint.sizeof);
		writefln("  address map %(0x%X, %)", addressMap);
		uint[] thunkMap = publicStream.readArray!uint(publicsHeader.numThunks);
		writefln("  thunk map %(0x%X, %)", thunkMap);
		SectionOffset[] sectionOffsets = publicStream.readArray!SectionOffset(publicsHeader.numSections);
		writefln("  section map:");
		foreach(sect; sectionOffsets) writefln("    isect 0x%04X offset 0x%08X", sect.isect, sect.offset);
		enforce(publicStream.remainingBytes == 0);
		writeln;


		StreamReader tpiHashStream = streams[tpiStreamHeader.hashStreamIndex];
		writefln("#   TPI hash stream %s: %s bytes", tpiStreamHeader.hashStreamIndex, tpiHashStream.remainingBytes);
		ubyte[] buf2 = tpiHashStream.readArray!ubyte(tpiHashStream.remainingBytes);
		printHex(buf2, 16, PrintAscii.yes);
		enforce(tpiHashStream.remainingBytes == 0, "Found bytes past the stream data");
		writeln;


		StreamReader ipiHashStream = streams[ipiStreamHeader.hashStreamIndex];
		writefln("#   IPI hash stream %s: %s bytes", ipiStreamHeader.hashStreamIndex, ipiHashStream.remainingBytes);
		ubyte[] buf3 = ipiHashStream.readArray!ubyte(ipiHashStream.remainingBytes);
		printHex(buf3, 16, PrintAscii.yes);
		enforce(ipiHashStream.remainingBytes == 0, "Found bytes past the stream data");
		writeln;


		StreamReader symRecordStream = streams[pdb.symRecordStream];
		writefln("#   Symbol record stream %s: %s bytes", pdb.symRecordStream, symRecordStream.remainingBytes);
		parseSymbols(symRecordStream);
		enforce(symRecordStream.remainingBytes == 0, "Found bytes past the stream data");
		writeln;

		StreamReader namesStream = streams[pdb.namesStreamIndex];
		auto stringtableHeader = namesStream.read!StringTableHeader;
		writefln("#   Names stream %s: %s bytes", pdb.namesStreamIndex, namesStream.remainingBytes);
		writeln ("  StringTableHeader:");
		writefln("    signature    0x%X", stringtableHeader.signature);
		writefln("    hashVersion  %s", stringtableHeader.hashVersion);
		writefln("    byteSize     %s", stringtableHeader.byteSize);
		enforce(stringtableHeader.hashVersion == 1 || stringtableHeader.hashVersion == 2,
			format("Unsupported hash version %s", stringtableHeader.hashVersion));

		StreamReader stringsSubstream = namesStream.substream(stringtableHeader.byteSize);
		writeln ("  String buffer:");
		size_t strOffset;
		size_t strIndex;
		while (!stringsSubstream.empty)
		{
			string str = stringsSubstream.readZString;
			writefln("    % 6s %08X %s", strIndex, strOffset, str);
			strOffset += str.length + 1;
			++strIndex;
		}

		uint numHashSlots = namesStream.read!uint;
		writefln("  Num hash slots: %s", numHashSlots);

		uint[] hashSlots = namesStream.readArray!uint(numHashSlots);
		writefln("  Slots: %s", hashSlots);

		uint numNames = namesStream.read!uint;
		writefln("  Num names: %s", numNames);

		enforce(namesStream.remainingBytes == 0);
		writeln;
	}

	void readGSIHashTable(ref StreamReader stream)
	{
		auto gsiHashHeader = stream.read!GSIHashHeader;
		enforce(gsiHashHeader.verSignature == GSIHashHeader.init.verSignature, "GSIHashHeader.verSignature is not valid");
		enforce(gsiHashHeader.verHdr == GSIHashHeader.init.verHdr, "GSIHashHeader.verHdr is not valid");
		writefln("  hrSize %s numBuckets %s bytes %s", gsiHashHeader.hrSize, gsiHashHeader.numBuckets, stream.remainingBytes);
		enforce(gsiHashHeader.hrSize + gsiHashHeader.numBuckets <= stream.remainingBytes,
			format("Not enough bytes left in the stream to read GSI hash map (needed %s + %s, while got %s)",
				gsiHashHeader.hrSize, gsiHashHeader.numBuckets, stream.remainingBytes));

		// read hash records
		enforce(gsiHashHeader.hrSize % PSHashRecord.sizeof == 0, format("GSIHashHeader.hrSize is not multiple of %s", PSHashRecord.sizeof));
		uint numHRRecords = gsiHashHeader.hrSize / PSHashRecord.sizeof;
		PSHashRecord[] hashRecords = stream.readArray!PSHashRecord(numHRRecords);
		writefln("  Hash records (%s items)", numHRRecords);
		foreach(record; hashRecords) {
			writefln("    offset: 0x%X, cref: 0x%X", record.offset, record.cref);
		}

		// read hash buckets
		// bitmap is followed by buckets
		// calculate bitmap size (it depends on verSignature and verHdr, but we only support the modern version)
		enum IPHR_HASH = 4096;
		// extra 1 slot (1 bit in bitmap) is reserved for technical reasons, and with 4 byte alignment gives extra 32 bits in addition to IPHR_HASH = 4096
		uint bitmapBits = alignValue(IPHR_HASH + 1, 32);
		uint bitmapBytes = bitmapBits / 8;
		uint bitmapUints = bitmapBytes / uint.sizeof;
		uint[] hashBitmap = stream.readArray!uint(bitmapUints);
		enforce(stream.remainingBytes % uint.sizeof == 0, format("Space after GSI hash bitmap is not multiple of 4 (%s)", stream.remainingBytes));
		writefln("  Buckets: %s", stream.remainingBytes / uint.sizeof);
		// iterate all set bits in bitmap
		// there are 1 bucket per set bit after bitmap
		foreach(size_t bitIndex; hashBitmap.bitsSet)
		{
			uint bucket = stream.read!uint;
			// max 4096 buckets
			writefln("    % 4s 0x%08X", bitIndex, bucket);
		}
	}

	void parseSymbols(ref StreamReader stream)
	{
		char[] indentation;
		int indentLevel = 0;
		void ind(char[] i = indentation) { // prints indentation
			write(i);
		}
		void indPush() {
			++indentLevel;
			indentation ~= "| ";
		}
		void indPop() {
			--indentLevel;
			indentation = indentation[0..$-2];
		}
		writefln("--- SYMBOLS %s bytes ---", stream.remainingBytes);
		while(stream.remainingBytes)
		{
			auto start = stream.streamCursor;
			auto len = stream.read!ushort;
			auto end = start + len + 2;
			SymbolKind kind = stream.read!SymbolKind;

			switch(kind)
			{
				case SymbolKind.S_UDT:
					auto udtsym = stream.read!UdtSym;
					string name = stream.readZString;
					visitString(name);
					ind; writefln("(%06X) %s: Type %s, %s", start, kind, udtsym.type, name);
					break;

				case SymbolKind.S_PUB32:
					auto pubsym = stream.read!PublicSym32;
					string name = stream.readZString;
					visitString(name);
					ind; writefln("(%06X) %s: [%04X:%08X] Flags %04b, %s", start, kind, pubsym.segment, pubsym.offset, pubsym.flags, name);
					break;

				// Those start a new level of indentation
				// then follow arguments terminated with S_ENDARG
				// then other data terminated with S_END
				case SymbolKind.S_GPROC32:
				case SymbolKind.S_LPROC32:
				case SymbolKind.S_GPROC32_ID:
				case SymbolKind.S_LPROC32_ID:
				case SymbolKind.S_LPROC32_DPC:
				case SymbolKind.S_LPROC32_DPC_ID:
					auto procsym = stream.read!ProcSym;
					string name = stream.readZString;
					visitString(name);
					ind; writef("(%06X) %s: [%04X:%08X] Flags:", start, kind, procsym.segment, procsym.offset);
					printProcSymFlags(procsym.flags);
					writefln(", %s", name);
					ind; writefln("|        Parent %06X End %06X Next %06X", procsym.parent, procsym.end, procsym.next);
					ind; writefln("|        Length %s, Dbg Start %08X, Dbg End %08X, Type %s",
						procsym.length, procsym.dbgStart, procsym.dbgEnd, procsym.typeIndex);
					indPush;
					ind; writeln;
					break;

				case SymbolKind.S_ENDARG:
					ind(indentation[0..$-2]); writefln("+-(%06X) %s", start, kind);
					break;

				case SymbolKind.S_REGREL32:
					// (0000C8) S_REGREL32: rsp+00000008, Type:    T_64PVOID(0603), hInstance
					auto regRelative = stream.read!RegRelativeSym;
					string name = stream.readZString;
					visitString(name);
					ind; writefln("(%06X) %s: %s+%08X, Type %s, %s", start, kind, regRelative.register, regRelative.offset, regRelative.type, name);
					break;

				case SymbolKind.S_LDATA32:
				case SymbolKind.S_GDATA32:
				case SymbolKind.S_LMANDATA:
				case SymbolKind.S_GMANDATA:
					// S_GDATA32: [0002:000236E8], Type:             0x1A60, RTInfoImpl
					auto dataSym = stream.read!DataSym;
					string name = stream.readZString;
					visitString(name);
					ind; writefln("(%06X) %s: [%04X:%08X] Type %s, %s", start, kind, dataSym.segment, dataSym.dataOffset, dataSym.type, name);
					break;

				case SymbolKind.S_LTHREAD32:
				case SymbolKind.S_GTHREAD32:
					// (1FCD30) S_GTHREAD32: [0006:00000270], Type:             0x168D, binaryCondStrings
					auto threadData = stream.read!ThreadDataSym;
					string name = stream.readZString;
					visitString(name);
					ind; writefln("(%06X) %s: [%04X:%08X] Type %s, %s", start, kind, threadData.segment, threadData.dataOffset, threadData.type, name);
					break;

				case SymbolKind.S_BUILDINFO:
					auto buildInfo = stream.read!BuildInfoSym;
					ind; writefln("(%06X) %s: %s", start, kind, buildInfo.buildId);
					break;

				case SymbolKind.S_INLINESITE:
					// (0002D8)  S_INLINESITE: Parent: 000001E8, End: 00000354, Inlinee:             0x259F
					//     BinaryAnnotations:    CodeLengthAndCodeOffset 29 20
					//     BinaryAnnotation Length: 4 bytes (1 bytes padding)
					auto inlineSite = stream.read!InlineSiteSym;
					ind; writefln("(%06X) %s: Parent %06X, End %06X, Inlinee %s", start, kind, inlineSite.parent, inlineSite.end, inlineSite.inlinee);

					// binary annotations stream
					StreamReader binAnnot = stream.substreamUntil(end);
					uint annotationsBytes = binAnnot.remainingBytes;
					ind; writefln("|        BinaryAnnotations: %s bytes", annotationsBytes);

					while (!binAnnot.empty)
					{
						auto instr = cast(BinaryAnnotationsOpcode)cvReadCompressedUint(binAnnot);
						enforce(instr != uint.max, "Invalid value inside binary annotations");
						if (instr == BinaryAnnotationsOpcode.invalid) {
							// only happens when first byte of an instruction is 0, which is a padding at the end of data
							binAnnot.unread(1);
							break;
						}

						ind; write("|          ");

						switch (instr) with(BinaryAnnotationsOpcode)
						{
							case codeOffset:
								uint arg = cvReadCompressedUint(binAnnot);
								writefln("code offset: %s", arg);
								break;
							case changeCodeOffsetBase:
								uint arg = cvReadCompressedUint(binAnnot);
								writefln("segment number: %s", arg);
								break;
							case changeCodeOffset:
								uint arg = cvReadCompressedUint(binAnnot);
								writefln("change code offset: %s (delta)", arg);
								break;
							case changeCodeLength:
								uint arg = cvReadCompressedUint(binAnnot);
								writefln("change code length: %s", arg);
								break;
							case changeFile:
								uint arg = cvReadCompressedUint(binAnnot);
								writefln("change file: fileId %s", arg);
								break;
							case changeLineOffset:
								uint arg = cvDecodeSignedInt32(cvReadCompressedUint(binAnnot));
								writefln("change line offset: %s (signed)", arg);
								break;
							case changeLineEndDelta:
								uint arg = cvReadCompressedUint(binAnnot);
								writefln("change line end: %s (delta)", arg);
								break;
							case changeRangeKind:
								uint arg = cvReadCompressedUint(binAnnot);
								writef("change range kind to %s", arg);
								switch(arg) {
									case 0: writeln(" (expression)"); break;
									case 1: writeln(" (statement)"); break;
									default: writeln; break;
								}
								break;
							case changeColumnStart:
								uint arg = cvReadCompressedUint(binAnnot);
								writef("change column start: %s", arg);
								if (arg == 0) writeln(" (0 means no column info)");
								else writeln;
								break;
							case changeColumnEndDelta:
								uint arg = cvDecodeSignedInt32(cvReadCompressedUint(binAnnot));
								writefln("end column number delta: %s (signed)", arg);
								break;
							case changeCodeOffsetAndLineOffset:
								uint arg = cvReadCompressedUint(binAnnot);
								writefln("end column number delta: %s (signed)", arg & 0b1111);
								ind; writefln("|          and change code offset: %s (delta)", arg >> 4);
								break;
							case changeCodeLengthAndCodeOffset:
								uint arg = cvReadCompressedUint(binAnnot);
								writefln("change code length: %s", arg);
								uint arg2 = cvReadCompressedUint(binAnnot);
								ind; writefln("|          and change code offset: %s (delta)", arg2);
								break;
							case changeColumnEnd:
								uint arg = cvReadCompressedUint(binAnnot);
								writef("end column number: %s", arg);
								break;
							default:
								writefln("??? 0x%04X", cast(uint)instr); break;
						}
					}
					ind; writefln("|        BinaryAnnotations padding: %s bytes", binAnnot.remainingBytes);

					indPush;
					break;

				case SymbolKind.S_CONSTANT:
				case SymbolKind.S_MANCONSTANT: // was not observed
					auto con = stream.read!ConstSym;
					ind; writef("(%06X) %s: Type %s, Value: ", start, kind, con.type);
					void printNum()
					{
						if (con.value < CV_TYPE.LF_NUMERIC)
						{
							// `con.value` is a value
							writef("%s", con.value);
							return;
						}

						// `con.value` is a type of value
						switch(cast(CV_TYPE)con.value)
						{
							case CV_TYPE.LF_CHAR: writef("(LF_CHAR) 0x%02X", stream.read!ubyte); break;
							case CV_TYPE.LF_SHORT: writef("(LF_SHORT) 0x%04X", stream.read!short); break;
							case CV_TYPE.LF_USHORT: writef("(LF_USHORT) 0x%04X", stream.read!ushort); break;
							case CV_TYPE.LF_LONG: writef("(LF_LONG) 0x%08X", stream.read!int); break;
							case CV_TYPE.LF_ULONG: writef("(LF_ULONG) 0x%08X", stream.read!uint); break;
							case CV_TYPE.LF_QUADWORD: writef("(LF_QUADWORD) 0x%016X", stream.read!long); break;
							case CV_TYPE.LF_UQUADWORD: writef("(LF_UQUADWORD) 0x%016X", stream.read!ulong); break;
							case CV_TYPE.LF_OCTWORD:
								writef("(LF_OCTWORD) [%(%02X %)]", stream.read!(ubyte[16]));
								break;
							case CV_TYPE.LF_UOCTWORD:
								writef("(LF_UOCTWORD) [%(%02X %)]", stream.read!(ubyte[16]));
								break;
							case CV_TYPE.LF_REAL16:
								writef("(LF_REAL16) 0x04X", stream.read!ushort);
								break;
							case CV_TYPE.LF_REAL32:
								union U {
									ubyte[4] bytes;
									float f;
								}
								U u;
								u.bytes = stream.read!(ubyte[4]);
								writef("(LF_REAL32) [%(%02X %)] %s", u.bytes, u.f);
								break;
							case CV_TYPE.LF_REAL64:
								union U2 {
									ubyte[8] bytes;
									double f;
								}
								U2 u;
								u.bytes = stream.read!(ubyte[8]);
								writef("(LF_REAL64) [%(%02X %)] %s", u.bytes, u.f);
								break;
							case CV_TYPE.LF_REAL80:
								writef("(LF_REAL80) [%(%02X %)]", stream.read!(ubyte[10]));
								break;
							case CV_TYPE.LF_REAL128:
								writef("(LF_REAL128) [%(%02X %)]", stream.read!(ubyte[16]));
								break;
							case CV_TYPE.LF_REAL48:
								writef("(LF_REAL48) [%(%02X %)]", stream.read!(ubyte[6]));
								break;
							case CV_TYPE.LF_COMPLEX32:
								writef("(LF_COMPLEX32) [%(%02X %)]", stream.read!(ubyte[8]));
								break;
							case CV_TYPE.LF_COMPLEX64:
								writef("(LF_COMPLEX64) [%(%02X %)]", stream.read!(ubyte[16]));
								break;
							case CV_TYPE.LF_COMPLEX80:
								writef("(LF_COMPLEX80) [%(%02X %)]", stream.read!(ubyte[20]));
								break;
							case CV_TYPE.LF_COMPLEX128:
								writef("(LF_COMPLEX128) [%(%02X %)]", stream.read!(ubyte[32]));
								break;
							case CV_TYPE.LF_VARSTRING:
								ushort len = stream.read!ushort;
								writef("(LF_VARSTRING) %s", stream.readArray!char(len));
								break;
							case CV_TYPE.LF_DATE:
								writef("(LF_DATE) 0x%016X", stream.read!ulong); // DATE is alias of double
								break;
							case CV_TYPE.LF_DECIMAL:
								writef("(LF_DECIMAL) [%(%02X %)]", stream.read!(ubyte[12])); // DECIMAL is 12 bytes
								break;
							case CV_TYPE.LF_UTF8STRING:
								writef("(LF_UTF8STRING) %s", stream.readZString);
								break;
							default:
								write("Invalid Numeric Leaf");
								break;
						}
					}
					printNum;
					writefln(", %s", stream.readZString);
					break;

				case SymbolKind.S_LOCAL:
					auto localsym = stream.read!LocalSym;
					string name = stream.readZString;
					visitString(name);
					ind; writef("(%06X) %s: Type %s, Flags:", start, kind, localsym.typeIndex);
					printLocalSymFlags(localsym.flags);
					writefln(", %s", name);
					break;

				case SymbolKind.S_FRAMEPROC:
					// (0000A0)  S_FRAMEPROC:
					//           Frame size = 0x00000028 bytes
					//           Pad size = 0x00000000 bytes
					//           Offset of pad in frame = 0x00000000
					//           Size of callee save registers = 0x00000000
					//           Address of exception handler = 0000:00000000
					//           Function info: invalid_pgo_counts opt_for_speed Local=rsp Param=rsp (0x00114000)
					auto frameProc = stream.read!FrameProcSym;
					ind; writefln("(%06X) %s:", start, kind);
					ind; writefln("         Frame size = 0x%08X bytes", frameProc.totalFrameBytes);
					ind; writefln("         Pad size = 0x%08X bytes", frameProc.paddingFrameBytes);
					ind; writefln("         Offset of pad in frame = 0x%08X", frameProc.offsetToPadding);
					ind; writefln("         Size of callee save registers = 0x%08X", frameProc.bytesOfCalleeSavedRegisters);
					ind; writefln("         Address of exception handler = %04X:%08X", frameProc.sectionIdOfExceptionHandler, frameProc.offsetOfExceptionHandler);
					ind; write("         Function info:");
					if (frameProc.hasAlloca) write(" alloca");
					if (frameProc.hasSetJmp) write(" setjmp");
					if (frameProc.hasLongJmp) write(" longjmp");
					if (frameProc.hasInlineAssembly) write(" inlasm");
					if (frameProc.hasExceptionHandling) write(" EH");
					if (frameProc.markedInline) write(" marked_inline");
					if (frameProc.hasStructuredExceptionHandling) write(" SEH");
					if (frameProc.naked) write(" naked");
					if (frameProc.securityChecks) write(" gschecks");
					if (frameProc.asynchronousExceptionHandling) write(" asyncEH");
					if (frameProc.noStackOrderingForSecurityChecks) write(" gs_no_stack_ordering");
					if (frameProc.inlined) write(" wasinlined");
					if (frameProc.strictSecurityChecks) write(" strict_gs_check");
					if (frameProc.safeBuffers) write(" safe_buffers");
					if (frameProc.encodedLocalBasePointer) write(" Local=%s", frameProc.encodedLocalBasePointer);
					if (frameProc.encodedParamBasePointer) write(" Param=%s", frameProc.encodedParamBasePointer);
					if (frameProc.profileGuidedOptimization) write(" pgo_on");
					if (frameProc.validProfileCounts) write(" valid_pgo_counts"); else write(" invalid_pgo_counts");
					if (frameProc.optimizedForSpeed) write(" opt_for_speed");
					if (frameProc.guardCfg) write(" guard_cfg");
					if (frameProc.guardCfw) write(" guard_cfw");
					writeln;
					break;

				case SymbolKind.S_TRAMPOLINE:
					// (000184) S_TRAMPOLINE: subtype Incremental, code size = 5 bytes
					//          Thunk address: [0001:00000005]
					//          Thunk target:  [0001:00000010]
					auto trampSym = stream.read!TrampolineSym;
					ind; writefln("(%06X) %s: Subtype %s, Code size %s bytes", start, kind, trampSym.type, trampSym.size);
					ind; writefln("         Thunk address [%04X:%08X]", trampSym.thunkSection, trampSym.thunkOffset);
					ind; writefln("         Thunk target  [%04X:%08X]", trampSym.targetSection, trampSym.targetOffset);
					ind; writeln;
					break;

				case SymbolKind.S_THUNK32:
					// (000048) S_THUNK32: [0001:004B4984], Cb: 00000006, CommandLineToArgvW
					//          Parent: 00000000, End: 00000074, Next: 00000000
					auto thunk = stream.read!ThunkSym;
					string name = stream.readZString;
					visitString(name);
					//ubyte[] variantData = stream.readArrayBefore!ubyte(end);
					ind; writefln("(%06X) %s: [%04X:%08X] Length %08X, Type %s, %s", start, kind, thunk.segment, thunk.offset, thunk.length, thunk.type, name);
					ind; writefln("|        Parent %06X, End %06X, Next %06X", thunk.parent, thunk.end, thunk.next);
					//ind; writefln("    variant data: %(%02x %)", variantData);
					indPush;
					break;

				case SymbolKind.S_COMPILE:
					auto compilesym = stream.read!CompileSym;
					string verstring = stream.readZString;
					ind; writefln("(%06X) %s:", start, kind);
					ind; writefln("         Language: %s", compilesym.sourceLanguage);
					ind; writefln("         Target processor: %s", cast(CV_CPUType)compilesym.machine);
					ind; writefln("         Floating-point precision: %s", compilesym.floatprec);
					static immutable string[4] floatPackageStrings = ["hardware", "emulator", "altmath", "???"];
					ind; writefln("         Floating-point package: %s", floatPackageStrings[compilesym.floatpkg]);
					static immutable string[4] modelStrings = ["near", "far", "huge", "???"];
					ind; writefln("         Ambient data: %s", modelStrings[compilesym.ambdata]);
					ind; writefln("         Ambient code: %s", modelStrings[compilesym.ambcode]);
					ind; writefln("         PCode present: %s", compilesym.pcode);
					ind; writefln("         mode32: %s", compilesym.mode32);
					if (compilesym.pad) {
						ind; writefln("         pad: 0x%03X", compilesym.pad);
					}
					ind; writefln("         Compiler Version: %s", verstring);
					ind; writeln;
					break;

				case SymbolKind.S_COMPILE2:
					auto compilesym = stream.read!CompileSym2;
					string verstring = stream.readZString;
					ind; writefln("(%06X) %s:", start, kind);
					ind; writefln("         Language: %s", compilesym.sourceLanguage);
					ind; writefln("         Target processor: %s", compilesym.machine);
					ind; writefln("         Compiled for edit and continue: %s", compilesym.EC);
					ind; writefln("         Compiled without debugging info: %s", compilesym.NoDbgInfo);
					ind; writefln("         Compiled with LTCG: %s", compilesym.LTCG);
					ind; writefln("         Compiled with /bzalign: %s", compilesym.NoDataAlign);
					ind; writefln("         Managed code present: %s", compilesym.ManagedPresent);
					ind; writefln("         Compiled with /GS: %s", compilesym.SecurityChecks);
					ind; writefln("         Compiled with /hotpatch: %s", compilesym.HotPatch);
					ind; writefln("         Converted by CVTCIL: %s", compilesym.CVTCIL);
					ind; writefln("         MSIL module: %s", compilesym.MSILModule);
					ind; writefln("         Pad bits = 0x%04X", compilesym.padding);
					ind; writefln("         Frontend Version: Major = %s, Minor = %s, Build = %s",
						compilesym.verFEMajor, compilesym.verFEMinor, compilesym.verFEBuild);
					ind; writefln("         Backend Version: Major = %s, Minor = %s, Build = %s",
						compilesym.verMajor, compilesym.verMinor, compilesym.verBuild);
					ind; writefln("         Version string: %s", verstring);
					ind; writefln("         Command block: %s", verstring);
					while(!stream.empty)
					{
						string cmdName = stream.readZString;
						if (cmdName is null) break; // terminated by empty string

						string cmd = stream.readZString;
						ind; writefln("         %s = '%s'", cmdName, cmd);
					}
					ind; writeln;
					break;

				case SymbolKind.S_COMPILE3:
					auto compilesym = stream.read!CompileSym3;
					string verstring = stream.readZString;
					ind; writefln("(%06X) %s:", start, kind);
					ind; writefln("         Language: %s", compilesym.sourceLanguage);
					ind; writefln("         Target processor: %s", compilesym.machine);
					ind; writefln("         Compiled for edit and continue: %s", compilesym.EC);
					ind; writefln("         Compiled without debugging info: %s", compilesym.NoDbgInfo);
					ind; writefln("         Compiled with LTCG: %s", compilesym.LTCG);
					ind; writefln("         Compiled with /bzalign: %s", compilesym.NoDataAlign);
					ind; writefln("         Managed code present: %s", compilesym.ManagedPresent);
					ind; writefln("         Compiled with /GS: %s", compilesym.SecurityChecks);
					ind; writefln("         Compiled with /hotpatch: %s", compilesym.HotPatch);
					ind; writefln("         Converted by CVTCIL: %s", compilesym.CVTCIL);
					ind; writefln("         MSIL module: %s", compilesym.MSILModule);
					ind; writefln("         Compiled with /sdl: %s", compilesym.Sdl);
					ind; writefln("         Compiled with pgo: %s", compilesym.PGO);
					ind; writefln("         .EXP module: %s", compilesym.Exp);
					ind; writefln("         Pad bits = 0x%04X", compilesym.padding);
					ind; writefln("         Frontend Version: Major = %s, Minor = %s, Build = %s, QFE = %s",
						compilesym.verFEMajor, compilesym.verFEMinor, compilesym.verFEBuild, compilesym.verFEQFE);
					ind; writefln("         Backend Version: Major = %s, Minor = %s, Build = %s, QFE = %s",
						compilesym.verMajor, compilesym.verMinor, compilesym.verBuild, compilesym.verQFE);
					ind; writefln("         Version string: %s", verstring);
					ind; writeln;
					break;

				case SymbolKind.S_LABEL32:
					auto labelSym = stream.read!LabelSym;
					string name = stream.readZString;
					visitString(name);
					ind; writef("(%06X) %s: [%04X:%08X] Flags:",
						start, kind, labelSym.segment, labelSym.offset);
					printProcSymFlags(labelSym.flags);
					writefln(", %s", name);
					break;

				case SymbolKind.S_BLOCK32:
					// (000930)  S_BLOCK32: [0001:0005AE21], Cb: 00000027,
					//           Parent: 00000118, End: 00000964
					auto blockSym = stream.read!BlockSym;
					string name = stream.readZString;
					visitString(name);
					ind; writefln("(%06X) %s: [%04X:%08X] Code size %08X bytes, %s",
						start, kind, blockSym.codeSegment, blockSym.codeOffset, blockSym.codeSize, name);
					ind; writefln("|        Parent %06X, End %06X", blockSym.parent, blockSym.end);
					assert(len == 22);
					indPush;
					break;

				case SymbolKind.S_OBJNAME:
					// (000004) S_OBJNAME: Signature: 00000000, * Linker *
					auto objname = stream.read!ObjNameSym;
					string name = stream.readZString;
					visitString(name);
					ind; writefln("(%06X) %s: Signature %08X, %s", start, kind, objname.signature, name);
					ind; writeln;
					break;

				case SymbolKind.S_ENVBLOCK:
					auto env = stream.read!EnvBlockSym;
					ind; writefln("(%06X) %s:", start, kind);
					ind; writefln("         Compiled for edit and continue: %s", env.EC);
					ind; writefln("         Command block:");

					// at least 4 bytes are needed for 2 non-empty strings
					while(!stream.empty)
					{
						string cmdName = stream.readZString;
						if (cmdName is null) break; // terminated by empty string

						string cmd = stream.readZString;
						ind; writefln("         %s = '%s'", cmdName, cmd);
					}
					ind; writeln;
					break;

				case SymbolKind.S_SECTION:
					// (000198) S_SECTION: [0001], RVA = 00001000, Cb = 00001030, Align = 00001000, Characteristics = 60000020, .text
					auto sectionsym = stream.read!SectionSym;
					string name = stream.readZString;
					visitString(name);
					ind; writefln("(%06X) %s: [%04X] RVA %08X, %08X bytes, Align %08X, Char %08X, %s", start, kind,
						sectionsym.section, sectionsym.rva, sectionsym.length,
						1 << sectionsym.alignmentPower, sectionsym.characteristics, name);
					break;

				case SymbolKind.S_COFFGROUP:
					// (0001B4) S_COFFGROUP: [0001:00000000], Cb: 00001030, Characteristics = 60000020, .text$mn
					auto coffGroupSym = stream.read!CoffGroupSym;
					string name = stream.readZString;
					visitString(name);
					ind; writefln("(%06X) %s: [%04X:%08X] %08X bytes, Char %08X, %s", start, kind,
						coffGroupSym.symbolSegment, coffGroupSym.symbolOffset,
						coffGroupSym.length, coffGroupSym.characteristics, name);
					break;

				case SymbolKind.S_EXPORT:
					auto exportSym = stream.read!ExportSym;
					string name = stream.readZString;
					visitString(name);
					ind; writef("(%06X) %s: Ordinal %s, Flags:", start, kind, exportSym.ordinal);
					if (exportSym.isConstant) write(" isConstant");
					if (exportSym.isData) write(" isData");
					if (exportSym.isPrivate) write(" isPrivate");
					if (exportSym.hasNoName) write(" hasNoName");
					if (exportSym.hasExplicitOrdinal) write(" hasExplicitOrdinal");
					if (exportSym.isForwarder) write(" isForwarder");
					if (exportSym.pad) writef(" pad = 0x%X", exportSym.pad);
					writefln(", %s", name);
					break;

				case SymbolKind.S_DEFRANGE_REGISTER:
					// (000230)  S_DEFRANGE_REGISTER: rcx
					//     Range: [0001:004A706C] - [0001:004A708C], 0 Gaps
					auto reg = stream.read!DefRangeRegisterSym;
					auto gaps = stream.readArrayBefore!LocalVariableAddrGap(end);
					ind; writefln("(%06X) %s: %s", start, kind, reg.register);
					ind; writef("         Range %s, %s Gaps", reg.range, gaps.length);
					if (gaps.length) write(" (Start offset, Length):");
					foreach(gap; gaps) writef(" (%04X, %02X)", gap.startOffset, gap.length);
					writeln;
					break;

				case SymbolKind.S_DEFRANGE_SUBFIELD_REGISTER:
					auto reg = stream.read!DefRangeSubfieldRegisterSym;
					auto gaps = stream.readArrayBefore!LocalVariableAddrGap(end);
					ind; writefln("(%06X) %s: offset at %04X: %s", start, kind, reg.offsetInParent, reg.register);
					ind; writef("         Range %s, %s Gaps", reg.range, gaps.length);
					if (gaps.length) write(" (Start offset, Length):");
					foreach(gap; gaps) writef(" (%04X, %02X)", gap.startOffset, gap.length);
					writeln;
					break;

				case SymbolKind.S_DEFRANGE_REGISTER_REL:
					auto reg = stream.read!DefRangeRegisterRelSym;
					ind; writefln("(%06X) %s: %s+%08X, UDT %s, Offset in parent %s", start, kind,
						reg.baseReg, reg.basePointerOffset, reg.spilledUdtMember, reg.offsetInParent);
					ind; writefln("         Range: %s", reg.range);
					break;

				case SymbolKind.S_DEFRANGE_FRAMEPOINTER_REL:
					auto reg = stream.read!DefRangeFramePointerRelSym;
					auto gaps = stream.readArrayBefore!LocalVariableAddrGap(end);
					ind; writefln("(%06X) %s: FrameOffset: %04X ", start, kind, reg.offFramePointer);
					ind; writef("         Range: %s, %s Gaps", reg.range, gaps.length);
					if (gaps.length) write(" (Start offset, Length):");
					foreach(gap; gaps) writef(" (%04X, %02X)", gap.startOffset, gap.length);
					writeln;
					break;

				case SymbolKind.S_DEFRANGE_FRAMEPOINTER_REL_FULL_SCOPE:
					auto reg = stream.read!DefRangeFramePointerRelFullScopeSym;
					ind; writefln("(%06X) %s: FrameOffset: %04X", start, kind, reg.offFramePointer);
					break;

				case SymbolKind.S_PROCREF:
				case SymbolKind.S_LPROCREF:
					auto procref = stream.read!ProcRefSym;
					string name = stream.readZString;
					visitString(name);
					ind; writefln("(%06X) %s: [%04X:%08X] Sum name %s, %s", start, kind, procref.mod,
						procref.symOffset, procref.sumName, name);
					break;

				case SymbolKind.S_CALLERS:
				case SymbolKind.S_CALLEES:
				case SymbolKind.S_INLINEES:
					auto funclist = stream.read!FunctionListSym;
					TypeIndex[] funcs = stream.readArray!TypeIndex(funclist.numFuncs);
					uint[] numInvocationsPerFunc = stream.readArrayBefore!uint(end);
					ind; writefln("(%06X) %s: count %s", start, kind, funclist.numFuncs);
					foreach(i, func; funcs) {
						uint numInvocations = 0;
						if (i < numInvocationsPerFunc.length)
							numInvocations = numInvocationsPerFunc[i];
						ind; writefln("         %s (%s)", func, numInvocations);
					}
					break;

				case SymbolKind.S_FRAMECOOKIE:
					auto cookie = stream.read!FrameCookieSym;
					ind; writefln("(%06X) %s: %s+%08X, Type %s, Flags 0x%02X", start, kind,
						cookie.reg, cookie.offset, cookie.type, cookie.flags);
					break;

				case SymbolKind.S_HEAPALLOCSITE:
					auto heapAlloc = stream.read!HeapAllocationSiteSym;
					ind; writefln("(%06X) %s: [%04X:%08X] Instr length %08X bytes, Type %s", start, kind,
						heapAlloc.section, heapAlloc.offset, heapAlloc.instrBytes, heapAlloc.type);
					break;

				case SymbolKind.S_CALLSITEINFO:
					auto callSite = stream.read!CallSiteInfoSym;
					ind; writefln("(%06X) %s: [%04X:%08X] Type %s", start, kind,
						callSite.section, callSite.offset, callSite.type);
					break;

				case SymbolKind.S_UNAMESPACE:
					string name = stream.readZString;
					visitString(name);
					ind; writefln("(%06X) %s: %s", start, kind, name);
					break;

				// terminates S_*PROC*, S_THUNK32, S_BLOCK32
				case SymbolKind.S_END:
				// terminates S_INLINESITE
				case SymbolKind.S_INLINESITE_END:
					enforce(indentLevel > 0, format("%s found when depth is 0", kind));
					indPop;
					ind; writefln("`-(%06X) %s", start, kind);
					if (indentLevel == 0) {
						ind; writeln;
					}
					break;

				case SymbolKind.S_FILESTATIC:
					auto fileStatic = stream.read!FileStaticSym;
					string name = stream.readZString;
					ind; writef("(%06X) %s: Mod name offset %08X, Type %s, Flags:", start, kind,
						fileStatic.modOffset, fileStatic.type);
					printLocalSymFlags(fileStatic.flags);
					writefln(", %s", name);
					break;

				default:
					string stringBuf2 = new char[len - 2];
					stream.readIntoArray(cast(char[])stringBuf2);
					ind; writefln("(%06X) %s: UNKNOWN `%s`", start, kind, stringBuf2);
					printHex(cast(ubyte[])stringBuf2, 16, PrintAscii.yes, indentation, 9);
			}

			auto padding = end - stream.streamCursor;
			if (padding >= 4) {
				ind; writefln("         padding: %s bytes", padding);
			}

			// skip padding
			stream.streamCursor = end;
		}
		enforce(indentLevel == 0, format("Wrong nesting detected, depth is %s", indentLevel));
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
			//writefln("cur %s page %s pageOffset %s read %s", streamCursor, page, pageOffset, pageBytesToRead);
			//printHex(pageData[0..pageBytesToRead], 16, PrintAscii.yes);
			enforce(streamCursor + pageBytesToRead <= streamBytes,
				format("attempt to read past the end of stream, %s %s %s",
					streamCursor, pageBytesToRead, streamBytes));
			byteBuf[0..pageBytesToRead] = pageData[0..pageBytesToRead];
			byteBuf = byteBuf[pageBytesToRead..$];
			streamCursor += pageBytesToRead;
		}
	}

	T[] readArrayBefore(T)(uint cursorAfterArray)
	{
		uint numBytes = cursorAfterArray - streamCursor;
		enforce(numBytes % T.sizeof == 0,
			format("readArrayBefore: %s is not multiple of %s.sizeof (%s)",
				numBytes, T.stringof, T.sizeof));
		uint numItems = numBytes / T.sizeof;
		return readArray!T(numItems);
	}

	T[] readArray(T)(uint size)
	{
		auto buf = new T[size];
		readIntoArray(buf);
		return buf;
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

	StreamReader substreamUntil(uint end)
	{
		StreamReader sub = this;
		sub.streamBytes = end;
		assert(end >= streamCursor);
		assert(end <= streamBytes);
		uint byteSize = end - streamCursor;
		drop(byteSize);
		return sub;
	}

	void drop(size_t bytesToDrop)
	{
		enforce(streamCursor + bytesToDrop <= streamBytes,
				format("attempt to drop past the end of stream, (stream cursor %s, bytes to drop %s, bytes left %s)",
					streamCursor, bytesToDrop, remainingBytes));
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
		while (true)
		{
			if (name.length == 0) return null; // handle empty string
			if (name[$-1] != '\0') break; // we found non-zero
			name = name[0..$-1]; // peel zero terminators
		}
		return name;
	}

	string readZNameBefore(uint end)
	{
		uint nameLength = end - streamCursor;
		string name = new char[nameLength];
		readIntoArray(cast(char[])name);
		//printHex(cast(ubyte[])name, 16, PrintAscii.yes);
		// find zero terminator first
		while (true)
		{
			if (name.length == 0) return null; // handle empty string
			if (name[$-1] == '\0') break; // we found non-zero
			name = name[0..$-1]; // peel padding
		}
		// find last char
		while (true)
		{
			if (name.length == 0) return null; // handle empty string
			if (name[$-1] != '\0') break; // we found non-zero
			name = name[0..$-1]; // peel zero terminators
		}
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
		sink.formattedWrite("MsfHeader:");
		sink.formattedWrite("  page size 0x%X\n", pageSize);
		sink.formattedWrite("  number of pages %s\n", numPages);
		sink.formattedWrite("  free page bitmap index %s\n", freePageBitmapIndex);
		sink.formattedWrite("  page map index %s\n", metaStreamHeaderPage);
		sink.formattedWrite("  meta-stream bytes %s\n", metaStreamBytes);
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

	void print()
	{
		writefln("  Stream header:");
		writefln("  % 10s ver", ver);
		writefln("  % 10s headerSize", headerSize);
		writefln("  % 10s typeIndexBegin", typeIndexBegin);
		writefln("  % 10s typeIndexEnd", typeIndexEnd);
		writefln("  % 10s typeRecordBytes", typeRecordBytes);
		writefln("  % 10s hashStreamIndex", hashStreamIndex);
		writefln("  % 10s hashAuxStreamIndex", hashAuxStreamIndex);
		writefln("  % 10s hashKeySize", hashKeySize);
		writefln("  % 10s numHashBuckets", numHashBuckets);
		writefln("  % 10s hashValueBufferOffset", hashValueBufferOffset);
		writefln("  % 10s hashValueBufferLength", hashValueBufferLength);
		writefln("  % 10s indexOffsetBufferOffset", indexOffsetBufferOffset);
		writefln("  % 10s indexOffsetBufferLength", indexOffsetBufferLength);
		writefln("  % 10s hashAdjBufferOffset", hashAdjBufferOffset);
		writefln("  % 10s hashAdjBufferLength", hashAdjBufferLength);
	}
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
	LF_STRUCTURE        = 0x1505,
	LF_UNION            = 0x1506,
	LF_ENUM             = 0x1507,
	LF_TYPESERVER2      = 0x1515,

	// Those are found in IPI stream
	LF_FUNC_ID          = 0x1601, // global func ID
	LF_MFUNC_ID         = 0x1602, // member func ID
	LF_BUILDINFO        = 0x1603, // build info: tool, version, command line, src/pdb file
	LF_SUBSTR_LIST      = 0x1604, // similar to LF_ARGLIST, for list of sub strings
	LF_STRING_ID        = 0x1605, // string ID
	LF_UDT_SRC_LINE     = 0x1606, // source and line on where an UDT is defined, only generated by compiler
	LF_UDT_MOD_SRC_LINE = 0x1607, // module, source and line on where an UDT is defined, only generated by linker
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
	// next follow `count` type indices of type TypeIndex
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



// LF_FUNC_ID
struct CVType_FUNC_ID
{
	uint scopeId;   // parent scope of the ID, 0 if global
	TypeIndex type; // function type
	// zero terminated string follows (name)
}

// LF_STRING_ID
struct CVType_STRING_ID
{
	uint id; // ID to list of sub string IDs
	// zero terminated string follows (name)
}

// struct lfUdtSrcLine
// LF_UDT_SRC_LINE
struct UdtSrcLine {
	TypeIndex udt;        // UDT's type index
	TypeIndex sourceFile; // index to LF_STRING_ID record where source file name is saved
	uint lineNumber;      // line number
}

// LF_MFUNC_ID
struct CVType_MFUNC_ID {
	TypeIndex parentType; // type index of parent
	TypeIndex type;       // function type
	// zero terminated string follows (name)
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
	// hrSize + numBuckets == remainingBytes of the stream
	uint hrSize;
	uint numBuckets;
}

// Comes first in Publics stream
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

struct PSHashRecord
{
	uint offset; // Offset in the symbol record stream
	uint cref;
}

// struct SO in langapi/include/pdb.h
struct SectionOffset
{
	uint offset;
	ushort isect;
	ubyte[2] pad;
};


// The header preceeding the /names stream.
struct StringTableHeader
{
	uint signature = 0xEFFEEFFE;
	uint hashVersion; // 1 or 2
	uint byteSize;    // Number of bytes of names buffer.

	uint stringTableHashString(const(char)[] str) {
		if (hashVersion == 1) return stringTableHashStringV1(str);
		if (hashVersion == 2) return stringTableHashStringV2(str);
		assert(false);
	}
};

// Corresponds to `Hasher::lhashPbCb` in PDB/include/misc.h.
// Used for name hash table and TPI/IPI hashes.
uint stringTableHashStringV1(const(char)[] str) {
	uint hash = 0;
	uint length = cast(uint)str.length;

	uint[] uints = (cast(uint*)str.ptr)[0..str.length / uint.sizeof];
	foreach (u; uints)
		hash ^= u;

	const(ubyte)* remainder = cast(const(ubyte)*)(uints.ptr + uints.length);
	uint remainderSize = length % 4;

	// Maximum of 3 bytes left.  Hash a 2 byte word if possible, then hash the
	// possibly remaining 1 byte.
	if (remainderSize >= 2) {
		ushort value = *cast(ushort*)(remainder);
		hash ^= value;
		remainder += 2;
		remainderSize -= 2;
	}

	// hash possible odd byte
	if (remainderSize == 1) {
		hash ^= *(remainder++);
	}

	immutable uint toLowerMask = 0x20202020;
	hash |= toLowerMask;
	hash ^= (hash >> 11);

	return hash ^ (hash >> 16);
}

// Corresponds to `HasherV2::HashULONG` in PDB/include/misc.h.
// Used for name hash table.
uint stringTableHashStringV2(const(char)[] str) {
	uint hash = 0xb170a1bf;

	// hash whole uints from the start of the string
	uint[] uints = (cast(uint*)str.ptr)[0..str.length / uint.sizeof];
	foreach (uint item; uints) {
		hash += item;
		hash += (hash << 10);
		hash ^= (hash >> 6);
	}

	// hash remaining bytes
	const(ubyte)[] remainingBytes = cast(const(ubyte)[])str[uints.length * uint.sizeof..$];
	foreach (ubyte item; remainingBytes) {
		hash += item;
		hash += (hash << 10);
		hash ^= (hash >> 6);
	}

	return hash * 1664525U + 1013904223U;
}
