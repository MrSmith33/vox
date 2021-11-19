/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.ast.basictype;

import std.string : format;
import all;

enum IrArgSize SIZET_SIZE = IrArgSize.size64;

// The order is the same as in TokenType enum
// The order is the same as in CommonAstNodes enum
enum BasicType : ubyte {
	t_error,
	t_noreturn,
	t_void,
	t_bool,
	t_null,

	t_i8,
	t_i16,
	t_i32,
	t_i64,

	t_u8,
	t_u16,
	t_u32,
	t_u64,

	t_f32,
	t_f64,

	// meta types
	t_alias,
	t_type,
	//t_value,
}

bool isInteger(BasicType b) {
	return b >= BasicType.t_i8 && b <= BasicType.t_u64;
}
ubyte integerSize(BasicType b) {
	switch(b) with(BasicType) {
		case t_i8, t_u8: return 1;
		case t_i16, t_u16: return 2;
		case t_i32, t_u32: return 4;
		case t_i64, t_u64: return 8;
		default: return 0;
	}
}
bool isSignedInteger(BasicType b) {
	return b >= BasicType.t_i8 && b <= BasicType.t_i64;
}
bool isUnsignedInteger(BasicType b) {
	return b >= BasicType.t_u8 && b <= BasicType.t_u64;
}
bool isFloat(BasicType b) {
	return b == BasicType.t_f32 || b == BasicType.t_f64;
}

// 0b_000_0
//        ^
//        | 0 e 0 explicit conversion only
//        ` 1 i 1 explicit and implicit conversion allowed
enum TypeConvResKind : ubyte {
	fail = 0b_000_0, // cannot convert
	no_e = 0b_001_0, // noop, explicit conversion only
	no_i = 0b_001_1, // noop, explicit and implicit conversion allowed

	ii_e = 0b_010_0, // i to i explicit conversion only
	ii_i = 0b_010_1, // i to i explicit and implicit conversion allowed
	if_e = 0b_011_0, // i to f explicit conversion only
	if_i = 0b_011_1, // i to f explicit and implicit conversion allowed
	ff_e = 0b_100_0, // f to f explicit conversion only
	ff_i = 0b_100_1, // f to f explicit and implicit conversion allowed
	fi_e = 0b_101_0, // f to i explicit conversion only
	fi_i = 0b_101_1, // f to i explicit and implicit conversion allowed

	override_expr_type_e = 0b_110_0,
	override_expr_type_i = 0b_110_1,

	string_literal_to_u8_ptr = 0b_111_0,
	array_literal_to_slice = 0b_111_1,
}

bool isNoop(TypeConvResKind kind) { return (kind & 0b_111_0) == TypeConvResKind.no_e; }
bool successful(TypeConvResKind kind) { return kind != TypeConvResKind.fail; }
bool canConvertImplicitly(TypeConvResKind kind) {
	return (kind & 1) == 1 || kind >= TypeConvResKind.string_literal_to_u8_ptr; }
TypeConvResKind allowExplicitOnly(TypeConvResKind kind) {
	if (kind >= TypeConvResKind.string_literal_to_u8_ptr) return kind;
	return cast(TypeConvResKind)(kind & 0b_111_0);
}

// usage basicConversionKind[from][to]
immutable TypeConvResKind[BasicType.max + 1][BasicType.max + 1] basicConversionKind = (){ with(TypeConvResKind){ return [
	//err noret void bool null   i8  i16  i32  i64   u8  u16  u32  u64  f32  f64 $alias $type  // to
	[no_i, no_i,no_i,no_i,no_i,no_i,no_i,no_i,no_i,no_i,no_i,no_i,no_i,no_i,no_i, no_i, no_i], // from error
	[no_i, fail,no_i,no_i,no_i,no_i,no_i,no_i,no_i,no_i,no_i,no_i,no_i,no_i,no_i, fail, fail], // from noreturn
	[no_i, fail,no_i,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail, fail, fail], // from void
	[no_i, fail,fail,no_i,fail,ii_i,ii_i,ii_i,ii_i,ii_i,ii_i,ii_i,ii_i,if_i,if_i, fail, fail], // from bool
	[no_i, fail,fail,ii_i,no_i,ii_e,ii_e,ii_e,ii_e,ii_e,ii_e,ii_e,ii_e,fail,fail, fail, fail], // from null
	[no_i, fail,fail,ii_i,fail,no_i,ii_i,ii_i,ii_i,ii_e,ii_e,ii_e,ii_e,if_i,if_i, fail, fail], // from i8
	[no_i, fail,fail,ii_i,fail,ii_e,no_i,ii_i,ii_i,ii_e,ii_e,ii_e,ii_e,if_i,if_i, fail, fail], // from i16
	[no_i, fail,fail,ii_i,fail,ii_e,ii_e,no_i,ii_i,ii_e,ii_e,ii_e,ii_e,if_i,if_i, fail, fail], // from i32
	[no_i, fail,fail,ii_i,fail,ii_e,ii_e,ii_e,no_i,ii_e,ii_e,ii_e,ii_e,if_i,if_i, fail, fail], // from i64
	[no_i, fail,fail,ii_i,fail,ii_e,ii_i,ii_i,ii_i,no_i,ii_i,ii_i,ii_i,if_i,if_i, fail, fail], // from u8
	[no_i, fail,fail,ii_i,fail,ii_e,ii_e,ii_i,ii_i,ii_e,no_i,ii_i,ii_i,if_i,if_i, fail, fail], // from u16
	[no_i, fail,fail,ii_i,fail,ii_e,ii_e,ii_e,ii_i,ii_e,ii_e,no_i,ii_i,if_i,if_i, fail, fail], // from u32
	[no_i, fail,fail,ii_i,fail,ii_e,ii_e,ii_e,ii_e,ii_e,ii_e,ii_e,no_i,if_i,if_i, fail, fail], // from u64
	[no_i, fail,fail,ii_i,fail,fi_i,fi_i,fi_i,fi_i,fi_i,fi_i,fi_i,fi_i,no_i,ff_i, fail, fail], // from f32
	[no_i, fail,fail,ii_i,fail,fi_i,fi_i,fi_i,fi_i,fi_i,fi_i,fi_i,fi_i,ff_e,no_i, fail, fail], // from f64
	[no_i, fail,fail,ii_i,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail, no_i, fail], // from $alias
	[no_i, fail,fail,ii_i,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail, ii_i, no_i], // from $type
]; }
}();

immutable BasicType[BasicType.max + 1][BasicType.max + 1] commonBasicType = (){ with(BasicType){ return [
	// error  noreturn    void     bool     null       i8      i16      i32      i64       u8      u16      u32      u64      f32      f64   $alias    $type
	[t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error], // error
	[t_error,t_noreturn,t_error,t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error], // noreturn
	[t_error, t_error, t_void,  t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_alias, t_type ], // void
	[t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_alias, t_type ], // bool
	[t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_alias, t_type ], // null
	[t_error, t_error, t_error, t_error, t_error, t_i8,    t_i16,   t_i32,   t_i64,   t_i8,    t_i16,   t_i32,   t_i64,   t_f32,   t_f64,   t_alias, t_type ], // i8
	[t_error, t_error, t_error, t_error, t_error, t_i16,   t_i16,   t_i32,   t_i64,   t_i16,   t_i16,   t_i32,   t_i64,   t_f32,   t_f64,   t_alias, t_type ], // i16
	[t_error, t_error, t_error, t_error, t_error, t_i32,   t_i32,   t_i32,   t_i64,   t_i32,   t_i32,   t_i32,   t_i64,   t_f32,   t_f64,   t_alias, t_type ], // i32
	[t_error, t_error, t_error, t_error, t_error, t_i64,   t_i64,   t_i64,   t_i64,   t_i64,   t_i64,   t_i64,   t_i64,   t_f32,   t_f64,   t_alias, t_type ], // i64
	[t_error, t_error, t_error, t_error, t_error, t_i8,    t_i16,   t_i32,   t_i64,   t_u8,    t_u16,   t_u32,   t_u64,   t_f32,   t_f64,   t_alias, t_type ], // u8
	[t_error, t_error, t_error, t_error, t_error, t_i16,   t_i16,   t_i32,   t_i64,   t_u16,   t_u16,   t_u32,   t_u64,   t_f32,   t_f64,   t_alias, t_type ], // u16
	[t_error, t_error, t_error, t_error, t_error, t_i32,   t_i32,   t_i32,   t_i64,   t_u32,   t_u32,   t_u32,   t_u64,   t_f32,   t_f64,   t_alias, t_type ], // u32
	[t_error, t_error, t_error, t_error, t_error, t_i64,   t_i64,   t_i64,   t_i64,   t_u64,   t_u64,   t_u64,   t_u64,   t_f32,   t_f64,   t_alias, t_type ], // u64
	[t_error, t_error, t_error, t_error, t_error, t_f32,   t_f32,   t_f32,   t_f32,   t_f32,   t_f32,   t_f32,   t_f32,   t_f32,   t_f64,   t_alias, t_type ], // f32
	[t_error, t_error, t_error, t_error, t_error, t_f64,   t_f64,   t_f64,   t_f64,   t_f64,   t_f64,   t_f64,   t_f64,   t_f64,   t_f64,   t_alias, t_type ], // f64
	[t_error, t_error, t_alias, t_alias, t_alias, t_alias, t_alias, t_alias, t_alias, t_alias, t_alias, t_alias, t_alias, t_alias, t_alias, t_alias, t_alias], // $alias
	[t_error, t_error, t_type,  t_type,  t_type,  t_type,  t_type,  t_type,  t_type,  t_type,  t_type,  t_type,  t_type,  t_type,  t_type,  t_alias, t_type ], // $type
]; }
}();

string[BasicType.max + 1] basicTypeNames = ["error", "noreturn", "void", "bool", "typeof(null)", "i8", "i16", "i32",
"i64", "u8", "u16", "u32", "u64", "f32", "f64", "$alias", "$type"];

bool isBasicTypeToken(TokenType tt) {
	return tt >= TYPE_TOKEN_FIRST && tt <= TYPE_TOKEN_LAST;
}

BasicType tokenTypeToBasicType(TokenType tt) {
	return cast(BasicType)(tt - TYPE_TOKEN_FIRST + BasicType.t_noreturn);
}

ubyte numSignedBytesForInt(long value) {
	if (cast(byte)(value & 0xFF) == value)
		return 1;
	else if (cast(short)(value & 0xFFFF) == value)
		return 2;
	else if (cast(int)(value & 0xFFFF_FFFF) == value)
		return 4;
	else
		return 8;
}

ubyte numUnsignedBytesForInt(ulong value) {
	if (cast(ubyte)(value & 0xFF) == value)
		return 1;
	else if (cast(ushort)(value & 0xFFFF) == value)
		return 2;
	else if (cast(uint)(value & 0xFFFF_FFFF) == value)
		return 4;
	else
		return 8;
}

IrArgSize argSizeIntSigned(long value) {
	if (cast(byte)(value & 0xFF) == value)
		return IrArgSize.size8;
	else if (cast(short)(value & 0xFFFF) == value)
		return IrArgSize.size16;
	else if (cast(int)(value & 0xFFFF_FFFF) == value)
		return IrArgSize.size32;
	else
		return IrArgSize.size64;
}

IrArgSize argSizeIntUnsigned(ulong value) {
	if (cast(ubyte)(value & 0xFF) == value)
		return IrArgSize.size8;
	else if (cast(ushort)(value & 0xFFFF) == value)
		return IrArgSize.size16;
	else if (cast(uint)(value & 0xFFFF_FFFF) == value)
		return IrArgSize.size32;
	else
		return IrArgSize.size64;
}

BasicType minUnsignedIntType(ulong value) {
	if (cast(ubyte)(value & 0xFF) == value)
		return BasicType.t_u8;
	else if (cast(ushort)(value & 0xFFFF) == value)
		return BasicType.t_u16;
	else if (cast(uint)(value & 0xFFFF_FFFF) == value)
		return BasicType.t_u32;
	else
		return BasicType.t_u64;
}

BasicType minSignedIntType(long value) {
	if (cast(long)cast(byte)(value & 0xFF) == value)
		return BasicType.t_i8;
	else if (cast(long)cast(short)(value & 0xFFFF) == value)
		return BasicType.t_i16;
	else if (cast(int)(value & 0xFFFF_FFFF) == value)
		return BasicType.t_i32;
	else
		return BasicType.t_i64;
}
