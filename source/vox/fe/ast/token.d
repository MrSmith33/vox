/// Copyright: Copyright (c) 2021 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.token;

import vox.utils : gatherEnumStrings;

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
	@("#soi") SOI,
	@("#eoi") EOI,
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


	@("#if")      HASH_IF,              // #if
	@("#version") HASH_VERSION,         // #version
	@("#inline")  HASH_INLINE,          // #inline
	@("#assert")  HASH_ASSERT,          // #assert
	@("#foreach") HASH_FOREACH,         // #foreach

	@("alias")    ALIAS_SYM,            // alias
	@("break")    BREAK_SYM,            // break
	@("continue") CONTINUE_SYM,         // continue
	@("do")       DO_SYM,               // do
	@("else")     ELSE_SYM,             // else
	@("function") FUNCTION_SYM,         // function
	@("if")       IF_SYM,               // if
	@("import")   IMPORT_SYM,           // import
	@("module")   MODULE_SYM,           // module
	@("return")   RETURN_SYM,           // return
	@("struct")   STRUCT_SYM,           // struct
	@("union")    UNION_SYM,            // union
	@("while")    WHILE_SYM,            // while
	@("for")      FOR_SYM,              // for
	@("switch")   SWITCH_SYM,           // switch
	@("cast")     CAST,                 // cast
	@("enum")     ENUM,                 // enum

	@("#special_kw") SPECIAL_KW,        // __FILE__, __LINE__, __FUNCTION_NAME__, __MODULE_NAME__

	@("#id")      IDENTIFIER,           // [a-zA-Z_] [a-zA-Z_0-9]*
	@("$id")      CASH_IDENTIFIER,      // $ [a-zA-Z_0-9]*

	// ----------------------------------------
	// list of basic types. The order is the same as in `enum BasicType`

	@("auto") TYPE_AUTO,                // auto
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
	@("#int_dec_lit") INT_DEC_LITERAL,  // 0|([1-9][0-9_]*)
	@("#int_hex_lit") INT_HEX_LITERAL,  // ("0x"|"0X")[0-9A-Fa-f_]+
	@("#int_bin_lit") INT_BIN_LITERAL,  // ("0b"|"0B")[01_]+
	@("#float_dec_lit") FLOAT_DEC_LITERAL,
	@("#str_lit") STRING_LITERAL,
	@("#char_lit") CHAR_LITERAL,

	@("#comm") COMMENT,                 // // /*
}
