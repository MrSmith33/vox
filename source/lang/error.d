/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lang.error;

import std.string : format;
import lang.lex : SourceLocation;

class RuntimeException : Exception
{
	this(string msg, int line, string file) { super(msg); this.line = line; this.file = file; }
}

RuntimeException runtime_error(Args...)(string msg, Args args, int line = __LINE__, string file = __FILE__) {
	return new RuntimeException(format(msg, args), line, file);
}

class CompilationException : Exception
{
	this(SourceLocation loc, string msg) {
		this.loc = loc;
		super(msg);
	}
	SourceLocation loc;
}

class ParsingException : CompilationException {
	this(SourceLocation loc, string msg) { super(loc, msg); }
}

ParsingException syntax_error(Args...)(SourceLocation loc, string msg, Args args) {
	return new ParsingException(loc, format(msg, args));
}

class SemanticsException : CompilationException {
	this(SourceLocation loc, string msg) { super(loc, msg); }
}

SemanticsException semantics_error(Args...)(SourceLocation loc, string msg, Args args) {
	return new SemanticsException(loc, format(msg, args));
}

Error internal_error(Args...)(string msg, Args args) {
	return new Error(format(msg, args));
}
