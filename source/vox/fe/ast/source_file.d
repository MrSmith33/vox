/// Copyright: Copyright (c) 2021 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Source file, source location
module vox.fe.ast.source_file;

import std.format : formattedWrite;
import vox.all;

/// One struct per source file
/// File can be virtual or located in some File System
/// Stored in CompilationContext.files
struct SourceFileInfo
{
	/// File name. Must be always set.
	string name;
	/// If set, then used as a source. Otherwise is read from file `name`
	const(char)[] content;
	/// Start of file source code in CompilationContext.sourceBuffer
	uint start;
	/// Length of source code
	uint length;
	/// Tokens of all files are stored linearly inside CompilationContext.tokenBuffer
	/// and CompilationContext.tokenLocationBuffer. Token file can be found with binary search
	TokenIndex firstTokenIndex;

	/// Module declaration
	ModuleDeclNode* mod;
}

/// One per token
/// Stored in CompilationContext.tokenLocationBuffer
struct SourceLocation {
	/// Byte offset in the CompilationContext.sourceBuffer
	uint start;
	/// Byte offset in the CompilationContext.sourceBuffer (exclusive)
	uint end;
	/// Zero based line number
	uint line;
	/// Zero based column number
	uint col;

	const(char)[] getTokenString(const(char)[] input) pure const {
		return input[start..end];
	}
	const(char)[] getTokenString(TokenType type, const(char)[] input) pure const {
		switch (type) {
			case TokenType.EOI:
				return "end of input";
			default:
				return input[start..end];
		}
	}
	void toString(scope void delegate(const(char)[]) sink) const {
		sink.formattedWrite("line %s col %s start %s end %s", line+1, col+1, start, end);
	}
}

// Helper for pretty printing
struct FmtSrcLoc
{
	TokenIndex tok;
	CompilationContext* ctx;

	void toString(scope void delegate(const(char)[]) sink) {
		if (tok.index == uint.max) return;
		auto loc = ctx.tokenLoc(tok);
		if (tok.index >= ctx.initializedTokenLocBufSize) {
			SourceFileInfo* fileInfo = ctx.getFileFromToken(tok);
			sink.formattedWrite("%s:%s:%s", fileInfo.name, loc.line+1, loc.col+1);
		}
	}
}
