/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// Lexer
module vox.fe.passes.lexer;

import std.format : formattedWrite;
import std.string : format;
import std.range : repeat;
import std.stdio;

import vox.all;


void pass_lexer(ref CompilationContext ctx, CompilePassPerModule[] subPasses)
{
	foreach (ref SourceFileInfo file; ctx.files.data)
	{
		file.firstTokenIndex = TokenIndex(ctx.tokenLocationBuffer.uintLength);

		Lexer lexer = Lexer(&ctx, ctx.sourceBuffer.data, &ctx.tokenBuffer, &ctx.tokenLocationBuffer);
		lexer.position = file.start;

		lexer.lex();

		ctx.numLinesLexed += lexer.line;

		if (ctx.printLexemes) {
			writefln("// Lexemes `%s`", file.name);
			Token tok = Token(TokenType.init, file.firstTokenIndex);
			do
			{
				tok.type = ctx.tokenBuffer[tok.index];
				auto loc = ctx.tokenLocationBuffer[tok.index];
				writefln("%s %s, `%s`", tok, loc, loc.getTokenString(ctx.sourceBuffer.data));
				++tok.index;
			}
			while(tok.type != TokenType.EOI);
		}
	}
}


/// Start of input
enum char SOI_CHAR = '\2';
/// End of input
enum char EOI_CHAR = '\3';

immutable string[] keyword_strings = ["auto","bool","true","false","alias","break","continue","do","else",
	"function","f32","f64","i16","i32","i64","i8","if","import","module","isize","return","struct","union","u16","u32",
	"u64","u8","usize","void","noreturn","while","for","switch","cast","enum","null"];
enum NUM_KEYWORDS = keyword_strings.length;
immutable TokenType[NUM_KEYWORDS] keyword_tokens = [TT.TYPE_AUTO,TT.TYPE_BOOL,TT.TRUE_LITERAL,TT.FALSE_LITERAL,
	TT.ALIAS_SYM, TT.BREAK_SYM,TT.CONTINUE_SYM,TT.DO_SYM,TT.ELSE_SYM,TT.FUNCTION_SYM,TT.TYPE_F32,
	TT.TYPE_F64,TT.TYPE_I16, TT.TYPE_I32,TT.TYPE_I64,TT.TYPE_I8,TT.IF_SYM,TT.IMPORT_SYM,TT.MODULE_SYM,
	TT.TYPE_ISIZE,TT.RETURN_SYM, TT.STRUCT_SYM,TT.UNION_SYM,TT.TYPE_U16,TT.TYPE_U32,TT.TYPE_U64,TT.TYPE_U8,TT.TYPE_USIZE,
	TT.TYPE_VOID,TT.TYPE_NORETURN,TT.WHILE_SYM,TT.FOR_SYM,TT.SWITCH_SYM,TT.CAST,TT.ENUM,TT.NULL];

struct Lexer
{
	CompilationContext* context;
	const(char)[] inputChars; // contains data of all files
	Arena!TokenType* outputTokens;
	Arena!SourceLocation* outputTokenLocations;

	private dchar c; // current symbol

	private uint position; // offset of 'c' in input
	private uint line; // line of 'c'
	private uint column; // column of 'c'

	private uint startPos; // offset of first token byte in input
	private uint startLine; // line of first token byte
	private uint startCol; // column of first token byte

	void lex()
	{
		while (true)
		{
			TokenType tokType = nextToken();

			outputTokens.put(tokType);
			set_loc();

			if (tokType == TokenType.EOI) return;
		}
	}

	private void prevChar()
	{
		--position;
		--column;
		c = inputChars[position];
	}

	private void nextChar()
	{
		++position;
		++column;
		c = inputChars[position];
	}

	private void set_loc()
	{
		outputTokenLocations.put(SourceLocation(startPos, position, startLine, startCol));
	}

	int opApply(scope int delegate(TokenType) dg)
	{
		TokenType tok;
		while ((tok = nextToken()) != TokenType.EOI)
			if (int res = dg(tok))
				return res;
		return 0;
	}

	TokenType nextToken()
	{
		c = inputChars[position];

		while (true)
		{
			startPos = position;
			startLine = line;
			startCol = column;

			switch(c)
			{
				case SOI_CHAR:
					// manual nextChar, because we don't want to advance column
					++position;
					c = inputChars[position];
					return TT.SOI;

				case EOI_CHAR:         return TT.EOI;
				case '\t': nextChar;   continue;
				case '\n': lex_EOLN(); continue;
				case '\r': lex_EOLR(); continue;
				case ' ' : nextChar;   continue;
				case '!' : nextChar; return lex_multi_equal2(TT.NOT, TT.NOT_EQUAL);
				case '%' : nextChar; return lex_multi_equal2(TT.PERCENT, TT.PERCENT_EQUAL);
				case '&' : nextChar; return lex_multi_equal2_3('&', TT.AND, TT.AND_EQUAL, TT.AND_AND);
				case '(' : nextChar; return TT.LPAREN;
				case ')' : nextChar; return TT.RPAREN;
				case '*' : nextChar; return lex_multi_equal2(TT.STAR, TT.STAR_EQUAL);
				case '+' : nextChar; return lex_multi_equal2_3('+', TT.PLUS, TT.PLUS_EQUAL, TT.PLUS_PLUS);
				case ',' : nextChar; return TT.COMMA;
				case '-' : nextChar; return lex_multi_equal2_3('-', TT.MINUS, TT.MINUS_EQUAL, TT.MINUS_MINUS);
				case '.' : nextChar;
					if (c == '.') { nextChar;
						if (c == '.') { nextChar;
							return TT.DOT_DOT_DOT;
						}
						return TT.DOT_DOT;
					}
					return TT.DOT;
				case '\"': nextChar; return lex_QUOTE_QUOTE();
				case '\'': nextChar; return lex_QUOTE();
				case '/' :           return lex_SLASH();
				case '0' :           return lex_ZERO();
				case '1' : ..case '9': return lex_DIGIT();
				case ':' : nextChar; return TT.COLON;
				case ';' : nextChar; return TT.SEMICOLON;
				case '<' : nextChar;
					if (c == '<') { nextChar;
						if (c == '=') { nextChar;
							return TT.LESS_LESS_EQUAL;
						}
						return TT.LESS_LESS;
					}
					if (c == '=') { nextChar;
						return TT.LESS_EQUAL;
					}
					return TT.LESS;
				case '=' : nextChar; return lex_multi_equal2(TT.EQUAL, TT.EQUAL_EQUAL);
				case '?' : nextChar; return TT.QUESTION;
				case '>' : nextChar;
					if (c == '=') { nextChar;
						return TT.MORE_EQUAL;
					}
					if (c == '>') { nextChar;
						if (c == '>') { nextChar;
							if (c == '=') { nextChar;
								return TT.MORE_MORE_MORE_EQUAL;
							}
							return TT.MORE_MORE_MORE;
						}
						if (c == '=') { nextChar;
							return TT.MORE_MORE_EQUAL;
						}
						return TT.MORE_MORE;
					}
					return TT.MORE;
				//case '?' : nextChar; return TT.QUESTION;
				case '@' : nextChar; return TT.AT;
				case '#' : nextChar; return lex_HASH();
				case '$' : nextChar; return lex_DOLLAR();
				case 'A' : ..case 'Z': return lex_LETTER();
				case '[' : nextChar; return TT.LBRACKET;
				case '\\': nextChar; return TT.BACKSLASH;
				case ']' : nextChar; return TT.RBRACKET;
				case '^' : nextChar; return lex_multi_equal2(TT.XOR, TT.XOR_EQUAL);
				case '_' : return lex_LETTER();
				case 'a' : ..case 'z': return lex_LETTER();
				case '{' : nextChar; return TT.LCURLY;
				case '|' : nextChar; return lex_multi_equal2_3('|', TT.OR, TT.OR_EQUAL, TT.OR_OR);
				case '}' : nextChar; return TT.RCURLY;
				case '~' : nextChar; return lex_multi_equal2(TT.TILDE, TT.TILDE_EQUAL);
				default  : nextChar; return TT.INVALID;
			}
		}
	}

	private void lex_EOLR() // \r[\n]
	{
		nextChar;
		if (c == '\n') nextChar;
		onNewLine;
	}

	private void lex_EOLN() // \n
	{
		nextChar;
		onNewLine;
	}

	private void onNewLine()
	{
		++line;
		column = 0;
	}

	// Lex X= tokens
	private TokenType lex_multi_equal2(TokenType single_tok, TokenType eq_tok)
	{
		if (c == '=') {
			nextChar;
			return eq_tok;
		}
		return single_tok;
	}

	private TokenType lex_multi_equal2_3(dchar chr, TokenType single_tok, TokenType eq_tok, TokenType double_tok)
	{
		if (c == chr) { nextChar;
			return double_tok;
		}
		if (c == '=') { nextChar;
			return eq_tok;
		}
		return single_tok;
	}

	private noreturn lexError(Args...)(TT type, string format, Args args) {
		outputTokens.put(type);
		set_loc();
		TokenIndex lastToken = TokenIndex(cast(uint)outputTokens.length-1);
		context.unrecoverable_error(lastToken, format, args);
	}

	private TokenType lex_SLASH() // /
	{
		nextChar;
		if (c == '/')
		{
			consumeLine();
			return TT.COMMENT;
		}
		if (c == '*')
		{
			nextChar;
			while (true)
			{
				switch(c)
				{
					case EOI_CHAR:
						lexError(TT.COMMENT, "Unterminated multiline comment");

					case '\n': lex_EOLN(); continue;
					case '\r': lex_EOLR(); continue;
					case '*':
						nextChar;
						if (c == '/') {
							nextChar;
							return TT.COMMENT;
						}
						break;
					default: break;
				}
				nextChar;
			}
		}
		if (c == '=') { nextChar;
			return TT.SLASH_EQUAL;
		}
		return TT.SLASH;
	}

	private TokenType lex_QUOTE_QUOTE() // "
	{
		while (true)
		{
			switch(c)
			{
				case EOI_CHAR:
					lexError(TT.STRING_LITERAL, "Unexpected end of input inside string literal");

				case '\\':
					nextChar;
					lexEscapeSequence();
					break;

				case '\n': lex_EOLN(); continue;
				case '\r': lex_EOLR(); continue;
				case '\"':
					nextChar; // skip "
					return TT.STRING_LITERAL;
				default: nextChar; break;
			}
		}
	}

	private void lexEscapeSequence() {
		switch(c)
		{
			case '\'':
			case '"':
			case '?':
			case '\\':
			case '0':
			case 'a':
			case 'b':
			case 'f':
			case 'n':
			case 'r':
			case 't':
			case 'v':
				nextChar;
				break;

			case 'x':
				nextChar; // skip x
				uint numChars = consumeHexadecimal;
				if (numChars < 2)
					lexError(TT.INVALID, "Invalid escape sequence");
				break;
			case 'u':
				nextChar; // skip u
				uint numChars = consumeHexadecimal;
				if (numChars < 4)
					lexError(TT.INVALID, "Invalid escape sequence");
				break;
			case 'U':
				nextChar; // skip U
				uint numChars = consumeHexadecimal;
				if (numChars < 8)
					lexError(TT.INVALID, "Invalid escape sequence");
				break;
			default:
				lexError(TT.INVALID, "Invalid escape sequence");
		}
	}

	private TokenType lex_QUOTE() // '
	{
		switch(c)
		{
			case EOI_CHAR:
				lexError(TT.CHAR_LITERAL, "Unexpected end of input inside char literal");

			case '\\':
				nextChar;
				lexEscapeSequence();
				break;

			case '\n': lex_EOLN(); break;
			case '\r': lex_EOLR(); break;
			default:
				nextChar;
				break;
		}
		if (c == '\'') {
			nextChar;
			return TT.CHAR_LITERAL;
		} else {
			lexError(TT.CHAR_LITERAL, "Invalid char literal");
		}
	}

	private TokenType lex_ZERO() // 0
	{
		nextChar;

		if (c == 'x' || c == 'X')
		{
			nextChar;
			consumeHexadecimal();
			if (c == 'i' || c == 'u') skipIntSuffix;
			return TT.INT_HEX_LITERAL;
		}
		else if (c == 'b' || c == 'B')
		{
			nextChar;
			consumeBinary();
			if (c == 'i' || c == 'u') skipIntSuffix;
			return TT.INT_BIN_LITERAL;
		}
		else
		{
			return consumeDecimal();
		}
	}

	private TokenType lex_DIGIT() // 1-9
	{
		nextChar;
		return consumeDecimal();
	}


	private TokenType lex_DOLLAR() // $
	{
		switch (c)
		{
			case 'a':
				if (match("alias")) { return TT.TYPE_ALIAS; } break;
			case 't':
				if (match("type")) { return TT.TYPE_TYPE; } break;
			default: break;
		}
		consumeId();
		return TT.CASH_IDENTIFIER;
	}

	private TokenType lex_HASH() // #
	{
		switch (c)
		{
			case 'i':
				nextChar;
				switch(c) {
					case 'f': if (match("f")) { return TT.HASH_IF; } break;
					case 'n': if (match("nline")) { return TT.HASH_INLINE; } break;
					default: break;
				}
				break;
			case 'a':
				if (match("assert")) { return TT.HASH_ASSERT; } break;
			case 'f':
				if (match("foreach")) { return TT.HASH_FOREACH; } break;
			case 'v':
				if (match("version")) { return TT.HASH_VERSION; } break;
			default: break;
		}
		lexError(TT.INVALID, "Invalid # identifier");
	}

	private TokenType lex_LETTER() // a-zA-Z_
	{
		switch (c)
		{
			case '_':
				nextChar; // skip _
				if (c != '_') break;
				nextChar; // skip _
				if (c == 'F') {
					nextChar; // skip F
					if (match("ILE__")) return TT.SPECIAL_KW; // __FILE__
					if (match("UNCTION_NAME__")) return TT.SPECIAL_KW; // __FUNCTION_NAME__
				}
				if (c == 'L' && match("LINE__")) return TT.SPECIAL_KW; // __LINE__
				if (c == 'M' && match("MODULE_NAME__")) return TT.SPECIAL_KW; // __MODULE_NAME__
				break;
			case 'a':
				nextChar; // skip a
				if (match("lias")) { return TT.ALIAS_SYM; }
				if (match("uto")) { return TT.TYPE_AUTO; }
				break;
			case 'b':
				nextChar;
				if (c == 'o' && match("ool")) { return TT.TYPE_BOOL; }
				else if (c == 'r' && match("reak")) { return TT.BREAK_SYM; }
				break;
			case 'c':
				nextChar;
				if (c == 'o' && match("ontinue")) { return TT.CONTINUE_SYM; }
				else if (c == 'a' && match("ast")) { return TT.CAST; }
				break;
			case 'd': if (match("do")) { return TT.DO_SYM; } break;
			case 'e':
				nextChar;
				if (c == 'l' && match("lse")) { return TT.ELSE_SYM; }
				else if (c == 'n' && match("num")) { return TT.ENUM; }
				break;
			case 'f':
				nextChar;
				if (c == 'a' && match("alse")) { return TT.FALSE_LITERAL; }
				if (c == '3' && match("32")) { return TT.TYPE_F32; }
				if (c == '6' && match("64")) { return TT.TYPE_F64; }
				if (c == 'o' && match("or")) { return TT.FOR_SYM; }
				if (c == 'u' && match("unction")) { return TT.FUNCTION_SYM; }
				break;
			case 'i':
				nextChar;
				switch(c) {
					case '1': if (match("16")) { return TT.TYPE_I16; } break;
					case '3': if (match("32")) { return TT.TYPE_I32; } break;
					case '6': if (match("64")) { return TT.TYPE_I64; } break;
					case '8': if (match("8"))  { return TT.TYPE_I8; }  break;
					case 's': if (match("size")) { return TT.TYPE_ISIZE; } break;
					case 'f': if (match("f")) { return TT.IF_SYM; } break;
					case 'm': if (match("mport")) { return TT.IMPORT_SYM; } break;
					default: break;
				}
				break;
			case 'n':
				nextChar;
				if (match("ull")) { return TT.NULL; }
				if (match("oreturn")) { return TT.TYPE_NORETURN; }
				break;
			case 'm': if (match("module")) { return TT.MODULE_SYM; } break;
			case 'r': if (match("return")) { return TT.RETURN_SYM; } break;
			case 's':
				nextChar;
				if (match("truct")) { return TT.STRUCT_SYM; }
				else if (match("witch")) { return TT.SWITCH_SYM; }
				break;
			case 'u':
				nextChar;
				switch(c) {
					case '1': if (match("16")) { return TT.TYPE_U16; } break;
					case '3': if (match("32")) { return TT.TYPE_U32; } break;
					case '6': if (match("64")) { return TT.TYPE_U64; } break;
					case '8': if (match("8"))  { return TT.TYPE_U8; }  break;
					case 'n': if (match("nion")){ return TT.UNION_SYM; }  break;
					case 's': if (match("size")) { return TT.TYPE_USIZE; } break;
					default: break;
				}
				break;
			case 't': if (match("true")) { return TT.TRUE_LITERAL; } break;
			case 'v': if (match("void")) { return TT.TYPE_VOID; } break;
			case 'w': if (match("while")) { return TT.WHILE_SYM; } break;
			default: break;
		}

		consumeId();
		return TT.IDENTIFIER;
	}

	// Does not reset in case of mismatch, so we continue consuming chars as regular identifier
	private bool match(string identifier)
	{
		uint index = 0;
		while (identifier[index] == c)
		{
			nextChar;
			++index;
			if (index == identifier.length)
			{
				// check that no valid symbol follow this id. ifff for if id.
				if (isIdSecond(c)) return false;
				return true;
			}
		}
		return false;
	}

	private void consumeId()
	{
		while (isIdSecond(c)) nextChar;
	}

	private TokenType consumeDecimal()
	{
		// skip initial decimal literal
		while (isNumSecond(c)) nextChar;

		bool isFloat = false;

		if (c == 'f') {
			skipFloatSuffix;
			return TT.FLOAT_DEC_LITERAL;
		} else if (c == 'i' || c == 'u') {
			skipIntSuffix;
			return TT.INT_DEC_LITERAL;
		}

		// check for "." followed by decimal
		if (c == '.') {
			nextChar; // skip .
			if (!isNumFirst(c)) {
				prevChar(); // revert .
				return TT.INT_DEC_LITERAL;
			}
			// skip second decimal literal
			while (isNumSecond(c)) nextChar;
			isFloat = true;
		}

		// check for exponent
		if (c == 'e' || c == 'E') {
			nextChar; // skip eE
			if (c == '+' || c == '-') nextChar; // skip -+
			if (!isNumFirst(c)) lexError(TT.INVALID, "Invalid char after exponent of float literal. Expected digit, got '%s'", c);
			// skip exponent
			while (isNumSecond(c)) nextChar;
			if (c == 'f') skipFloatSuffix;
			return TT.FLOAT_DEC_LITERAL;
		} else if (c == 'f') {
			skipFloatSuffix;
			return TT.FLOAT_DEC_LITERAL;
		}

		if (isFloat) return TT.FLOAT_DEC_LITERAL;
		return TT.INT_DEC_LITERAL;
	}

	private void skipFloatSuffix() {
		// f32/f64
		nextChar; // skip f
		if (c == '3') {
			nextChar; // skip 3
			if (c == '2') {
				nextChar; // skip 2
			} else lexError(TT.INVALID, "Invalid char after `f3` of float literal. Expected f32 or f64 suffix, got '%s'", c);
		} else if (c == '6') {
			nextChar; // skip 3
			if (c == '4') {
				nextChar; // skip 4
			} else lexError(TT.INVALID, "Invalid char after `f6` of float literal. Expected f32 or f64 suffix, got '%s'", c);
		}
		else lexError(TT.INVALID, "Invalid char after `f` of float literal. Expected f32 or f64 suffix, got '%s'", c);
	}

	private void skipIntSuffix() {
		// i8/i16/i32/i64/u8/u16/u32/u64
		dchar intChar = c;
		nextChar; // skip i/u
		switch(c)
		{
			case '1':
				nextChar; // skip 1
				if (c == '6') {
					nextChar; // skip 6
					return;
				} else lexError(TT.INVALID, "Invalid char after `%1$s1` of int literal. Expected %1$s16 suffix, got '%1$s1%s'", intChar, c);
			case '3':
				nextChar; // skip 3
				if (c == '2') {
					nextChar; // skip 2
					return;
				} else lexError(TT.INVALID, "Invalid char after `%1$s3` of int literal. Expected %1$s32 suffix, got '%1$s3%s'", intChar, c);
			case '6':
				nextChar; // skip 6
				if (c == '4') {
					nextChar; // skip 4
					return;
				} else lexError(TT.INVALID, "Invalid char after `%1$s6` of int literal. Expected %1$s64 suffix, got '%1$s6%s'", intChar, c);
			case '8':
				nextChar; // skip 8
				return;
			default: lexError(TT.INVALID, "Invalid char after `%1$s` of int literal. Expected %1$s8/%1$s16/%1$s32/%1$s64 suffix, got '%1$s%s'", intChar, c);
		}
	}

	private uint consumeHexadecimal()
	{
		uint count;
		while (true)
		{
			if ('0' <= c && c <= '9') {
			} else if ('a' <= c && c <= 'f') {
			} else if ('A' <= c && c <= 'F') {
			} else if (c != '_') return count;
			nextChar;
			++count;
		}
	}

	private void consumeBinary()
	{
		while (true)
		{
			if (c == '0' || c == '1') {
			} else if (c != '_') return;
			nextChar;
		}
	}

	private void consumeLine()
	{
		while (true)
		{
			switch(c)
			{
				case EOI_CHAR: return;
				case '\n': lex_EOLN(); return;
				case '\r': lex_EOLR(); return;
				default: break;
			}
			nextChar;
		}
	}
}

// strRepr is string representation of a single char, without ' around
dchar escapeToChar(const(char)[] strRepr) {
	import std.conv : to;
	switch (strRepr[0]) {
		case '\'': return '\'';
		case '"': return '\"';
		case '?': return '\?';
		case '\\': return '\\';
		case '0': return '\0';
		case 'a': return '\a';
		case 'b': return '\b';
		case 'f': return '\f';
		case 'n': return '\n';
		case 'r': return '\r';
		case 't': return '\t';
		case 'v': return '\v';
		case 'x': return strRepr[1..$].to!uint(16);
		case 'u': return strRepr[1..$].to!uint(16);
		case 'U': return strRepr[1..$].to!uint(16);
		default: assert(false, strRepr);
	}
}

dchar getCharValue(const(char)[] strRepr) {
	if (strRepr[0] == '\\') return escapeToChar(strRepr[1..$]);
	assert(strRepr.length == 1);
	return strRepr[0];
}

// Only handles valid strings
// We copy it into buffer, then run through it modifing in-place
string handleEscapedString(ref Arena!ubyte sink, const(char)[] str)
{
	import std.conv : to;
	char* dstStart = cast(char*)sink.nextPtr;
	sink.put(cast(ubyte[])str);
	sink.put(0); // we will look for this 0 to end the loop
	char* src = cast(char*)dstStart;
	char* dst = cast(char*)dstStart;

	loop:
	while(*src) // look for \0 we put into buffer
	{
		if (*src == '\\')
		{
			++src; // skip \

			// Read escaped char
			dchar c;
			switch (*src) {
				case '\'': c = '\''; break;
				case '"':  c = '\"'; break;
				case '?':  c = '\?'; break;
				case '\\': c = '\\'; break;
				case '0':  c = '\0'; break;
				case 'a':  c = '\a'; break;
				case 'b':  c = '\b'; break;
				case 'f':  c = '\f'; break;
				case 'n':  c = '\n'; break;
				case 'r':  c = '\r'; break;
				case 't':  c = '\t'; break;
				case 'v':  c = '\v'; break;
				case 'x':
					c = (cast(char[])src[1..3]).to!uint(16); src += 3;
					*dst++ = cast(ubyte)c;
					continue loop; // skip rest, as this represents byte value, not unicode char
				case 'u':  c = (cast(char[])src[1..5]).to!uint(16); src += 4; break;
				case 'U':  c = (cast(char[])src[1..9]).to!uint(16); src += 8; break;
				default: assert(false, "Invalid escape sequence");
			}
			// For bigger cases (u, U) we inrement additionally. (x) is not handled here
			++src;

			// Write utf-8 back
			if (c < 0x80) {
				*dst++ = cast(ubyte)c;
			} else if (c < 0x800) {
				*dst++ = 0xC0 | cast(ubyte)(c >> 6);
				*dst++ = 0x80 | (c & 0x3f);
			} else if (c < 0x10000) {
				*dst++ = 0xE0 | cast(ubyte)(c >> 12);
				*dst++ = 0x80 | ((c >> 6) & 0x3F);
				*dst++ = 0x80 | (c & 0x3f);
			} else if (c < 0x110000) {
				*dst++ = 0xF0 | cast(ubyte)(c >> 18);
				*dst++ = 0x80 | ((c >> 12) & 0x3F);
				*dst++ = 0x80 | ((c >> 6) & 0x3F);
				*dst++ = 0x80 | (c & 0x3f);
			}
		}
		else // non-escaped char. Just copy
		{
			*dst++ = *src++;
		}
	}
	size_t len = cast(size_t)(dst - dstStart);
	sink.unput(str.length + 1 - len); // unput \0 too
	return cast(string)dstStart[0..len];
}

private bool isIdSecond(dchar chr) pure nothrow {
	return
		'0' <= chr && chr <= '9' ||
		'a' <= chr && chr <= 'z' ||
		'A' <= chr && chr <= 'Z' ||
		chr == '_';
}

private bool isNumFirst(dchar chr) pure nothrow {
	return '0' <= chr && chr <= '9';
}

private bool isNumSecond(dchar chr) pure nothrow {
	return '0' <= chr && chr <= '9' || chr == '_';
}


unittest
{
	CompilationContext ctx;
	ubyte[64] tokenBuffer;
	ubyte[64] locs;

	Lexer makeLexer(string input) {
		ctx.tokenBuffer.setBuffer(tokenBuffer[], 64);
		ctx.tokenLocationBuffer.setBuffer(locs[], 64);
		return Lexer(&ctx, input~EOI_CHAR, &ctx.tokenBuffer, &ctx.tokenLocationBuffer);
	}

	foreach(i, string keyword; keyword_strings)
	{
		Lexer lexer = makeLexer(keyword);
		TokenType token = lexer.nextToken;
		assert(token == keyword_tokens[i],
			format("For %s expected %s got %s", keyword, keyword_tokens[i], token));
	}

	foreach(i, string keyword; keyword_strings)
	{
		Lexer lexer = makeLexer(keyword~"A");
		TokenType token = lexer.nextToken;
		assert(token == TT.IDENTIFIER);
	}

	{
		string[] ops = ["&","&&","&=","@","\\",":",",",".","..","...",
			"=","==",">",">=",">>",">>=",">>>",">>>=","<","<=","<<","<<=","-",
			"-=","--","!","!=","|","|=","||","%","%=","+","+=","++","?",";","/",
			"/=","*","*=","~","~=","^","^=","(",")","[","]","{","}",];
		TokenType[] tokens_ops = [TT.AND,TT.AND_AND,TT.AND_EQUAL,TT.AT,TT.BACKSLASH,
			TT.COLON,TT.COMMA,TT.DOT,TT.DOT_DOT,TT.DOT_DOT_DOT,TT.EQUAL,
			TT.EQUAL_EQUAL,TT.MORE,TT.MORE_EQUAL,TT.MORE_MORE,
			TT.MORE_MORE_EQUAL,TT.MORE_MORE_MORE,TT.MORE_MORE_MORE_EQUAL,
			TT.LESS,TT.LESS_EQUAL,TT.LESS_LESS,TT.LESS_LESS_EQUAL,TT.MINUS,
			TT.MINUS_EQUAL,TT.MINUS_MINUS,TT.NOT,TT.NOT_EQUAL,TT.OR,TT.OR_EQUAL,
			TT.OR_OR,TT.PERCENT,TT.PERCENT_EQUAL,TT.PLUS,TT.PLUS_EQUAL,TT.PLUS_PLUS,
			TT.QUESTION,TT.SEMICOLON,TT.SLASH,TT.SLASH_EQUAL,TT.STAR,TT.STAR_EQUAL,
			TT.TILDE,TT.TILDE_EQUAL,TT.XOR,TT.XOR_EQUAL,TT.LPAREN,TT.RPAREN,
			TT.LBRACKET,TT.RBRACKET, TT.LCURLY,TT.RCURLY,];
		foreach(i, string op; ops)
		{
			Lexer lexer = makeLexer(op);
			TokenType token = lexer.nextToken;
			assert(token == tokens_ops[i],
				format("For %s expected %s got %s", op, tokens_ops[i], token));
		}
	}

	void testNumeric(string input, TokenType tokType)
	{
		Lexer lexer = makeLexer(input);
		assert(lexer.nextToken == tokType);
	}

	assert(makeLexer("_10").nextToken == TT.IDENTIFIER);
	testNumeric("10", TT.INT_DEC_LITERAL);
	testNumeric("1_0", TT.INT_DEC_LITERAL);
	testNumeric("10_", TT.INT_DEC_LITERAL);
	testNumeric("10.0", TT.FLOAT_DEC_LITERAL);
	testNumeric("10.0e0", TT.FLOAT_DEC_LITERAL);
	testNumeric("10.0e+0", TT.FLOAT_DEC_LITERAL);
	testNumeric("10.0e-0", TT.FLOAT_DEC_LITERAL);
	testNumeric("10.0E+0", TT.FLOAT_DEC_LITERAL);
	testNumeric("10.0E-0", TT.FLOAT_DEC_LITERAL);
	testNumeric("10e0", TT.FLOAT_DEC_LITERAL);
	testNumeric("10E0", TT.FLOAT_DEC_LITERAL);
	testNumeric("10e+0", TT.FLOAT_DEC_LITERAL);
	testNumeric("10e-0", TT.FLOAT_DEC_LITERAL);
	testNumeric("10E+0", TT.FLOAT_DEC_LITERAL);
	testNumeric("10E-0", TT.FLOAT_DEC_LITERAL);
	testNumeric("0xFF", TT.INT_HEX_LITERAL);
	testNumeric("0XABCDEF0123456789", TT.INT_HEX_LITERAL);
	testNumeric("0x1_0", TT.INT_HEX_LITERAL);
	testNumeric("0b10", TT.INT_BIN_LITERAL);
	testNumeric("0B10", TT.INT_BIN_LITERAL);
	testNumeric("0b1_0", TT.INT_BIN_LITERAL);

	{
		string source = "/*\n*/test";
		Lexer lexer = makeLexer(source);
		lexer.lex;
		assert(tokenBuffer[0] == TT.COMMENT);
		assert(ctx.tokenLocationBuffer[0].getTokenString(source) == "/*\n*/", format("%s", ctx.tokenLocationBuffer[0]));
		assert(tokenBuffer[1] == TT.IDENTIFIER);
		assert(ctx.tokenLocationBuffer[1].getTokenString(source) == "test");
	}
	{
		string source = "//test\nhello";
		Lexer lexer = makeLexer(source);
		lexer.lex;
		assert(tokenBuffer[0] == TT.COMMENT);
		assert(ctx.tokenLocationBuffer[0].getTokenString(source) == "//test\n");
		assert(tokenBuffer[1] == TT.IDENTIFIER);
		assert(ctx.tokenLocationBuffer[1].getTokenString(source) == "hello");
	}
	{
		string source = `"literal"`;
		Lexer lexer = makeLexer(source);
		lexer.lex;
		assert(tokenBuffer[0] == TT.STRING_LITERAL);
		assert(ctx.tokenLocationBuffer[0].getTokenString(source) == `"literal"`, format("%s", tokenBuffer[0]));
	}
	{
		string source = `'@'`;
		Lexer lexer = makeLexer(source);
		lexer.lex;
		assert(tokenBuffer[0] == TT.CHAR_LITERAL);
		assert(ctx.tokenLocationBuffer[0].getTokenString(source) == `'@'`, format("%s", tokenBuffer[0]));
	}
}
