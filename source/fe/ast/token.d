/// Copyright: Copyright (c) 2021 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.token;

import utils : gatherEnumStrings;

struct Token {
	TokenType type;
	TokenIndex index;
}

struct TokenIndex
{
	uint index = uint.max;
	alias index this;
	bool isValid() { return index != uint.max; }
}

immutable string[] tokStrings = gatherEnumStrings!TokenType();

enum TokenType TYPE_TOKEN_FIRST = TokenType.TYPE_NORETURN;
enum TokenType TYPE_TOKEN_LAST = TokenType.TYPE_TYPE;

alias TT = TokenType;
enum TokenType : ubyte {
	@("#soi")  SOI,
	@("#eoi")  EOI,
	@(null)   INVALID,

	@("&")    AND,
	@("&&")   AND_AND,
	@("&=")   AND_EQUAL,
	@("@")    AT,
	@("\\")   BACKSLASH,
	@(":")    COLON,
	@(",")    COMMA,
	@(".")    DOT,
	@("..")   DOT_DOT,
	@("...")  DOT_DOT_DOT,
	@("=")    EQUAL,
	@("==")   EQUAL_EQUAL,
	@(">")    MORE,
	@(">=")   MORE_EQUAL,
	@(">>")   MORE_MORE,
	@(">>=")  MORE_MORE_EQUAL,
	@(">>>")  MORE_MORE_MORE,
	@(">>>=") MORE_MORE_MORE_EQUAL,
	@("<")    LESS,
	@("<=")   LESS_EQUAL,
	@("<<")   LESS_LESS,
	@("<<=")  LESS_LESS_EQUAL,
	@("-")    MINUS,
	@("-=")   MINUS_EQUAL,
	@("--")   MINUS_MINUS,
	@("!")    NOT,
	@("!=")   NOT_EQUAL,
	@("|")    OR,
	@("|=")   OR_EQUAL,
	@("||")   OR_OR,
	@("%")    PERCENT,
	@("%=")   PERCENT_EQUAL,
	@("+")    PLUS,
	@("+=")   PLUS_EQUAL,
	@("++")   PLUS_PLUS,
	@("?")    QUESTION,
	@(";")    SEMICOLON,
	@("/")    SLASH,
	@("/=")   SLASH_EQUAL,
	@("*")    STAR,
	@("*=")   STAR_EQUAL,
	@("~")    TILDE,
	@("~=")   TILDE_EQUAL,
	@("^")    XOR,
	@("^=")   XOR_EQUAL,

	@("(")    LPAREN,
	@(")")    RPAREN,
	@("[")    LBRACKET,
	@("]")    RBRACKET,
	@("{")    LCURLY,
	@("}")    RCURLY,


	@("#if")      HASH_IF,
	@("#version") HASH_VERSION,
	@("#inline")  HASH_INLINE,
	@("#assert")  HASH_ASSERT,
	@("#foreach") HASH_FOREACH,

	@("alias")    ALIAS_SYM,
	@("break")    BREAK_SYM,
	@("continue") CONTINUE_SYM,
	@("do")       DO_SYM,
	@("else")     ELSE_SYM,
	@("function") FUNCTION_SYM,
	@("if")       IF_SYM,
	@("import")   IMPORT_SYM,
	@("module")   MODULE_SYM,
	@("return")   RETURN_SYM,
	@("struct")   STRUCT_SYM,
	@("union")    UNION_SYM,
	@("while")    WHILE_SYM,
	@("for")      FOR_SYM,
	@("switch")   SWITCH_SYM,
	@("cast")     CAST,                 // cast(T)
	@("enum")     ENUM,

	@("#id")      IDENTIFIER,           // [a-zA-Z_] [a-zA-Z_0-9]*
	@("$id")      CASH_IDENTIFIER,      // $ [a-zA-Z_0-9]*

	// ----------------------------------------
	// list of basic types. The order is the same as in `enum BasicType`


	@("noreturn") TYPE_NORETURN,        // noreturn
	@("void") TYPE_VOID,                // void
	@("bool") TYPE_BOOL,                // bool
	@("null") NULL,                     // null
	@("i8")   TYPE_I8,                  // i8
	@("i16")  TYPE_I16,                 // i16
	@("i32")  TYPE_I32,                 // i32
	@("i64")  TYPE_I64,                 // i64

	@("u8")   TYPE_U8,                  // u8
	@("u16")  TYPE_U16,                 // u16
	@("u32")  TYPE_U32,                 // u32
	@("u64")  TYPE_U64,                 // u64

	@("f32")  TYPE_F32,                 // f32
	@("f64")  TYPE_F64,                 // f64

	@("$alias") TYPE_ALIAS,             // $alias
	@("$type")  TYPE_TYPE,              // $type
	// ----------------------------------------

	@("isize") TYPE_ISIZE,              // isize
	@("usize") TYPE_USIZE,              // usize

	@("true")  TRUE_LITERAL,            // true
	@("false") FALSE_LITERAL,           // false
	@("#int_dec_lit") INT_DEC_LITERAL,
	@("#int_hex_lit") INT_HEX_LITERAL,
	@("#int_bin_lit") INT_BIN_LITERAL,
	@("#float_dec_lit") FLOAT_DEC_LITERAL,
	@("#str_lit") STRING_LITERAL,
	@("#char_lit") CHAR_LITERAL,
	//@(null) DECIMAL_LITERAL,          // 0|[1-9][0-9_]*
	//@(null) BINARY_LITERAL,           // ("0b"|"0B")[01_]+
	//@(null) HEX_LITERAL,              // ("0x"|"0X")[0-9A-Fa-f_]+

	@("#comm") COMMENT,                 // // /*
}
