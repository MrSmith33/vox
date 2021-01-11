/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.basictype;

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

// usage isAutoConvertibleFromToBasic[from][to]
immutable bool[BasicType.max + 1][BasicType.max + 1] isAutoConvertibleFromToBasic = [
	//err noreturn void bool null i8 i16 i32 i64  u8 u16 u32 u64 f32 f64 $alias $type // to
	[   0,       0,   0,   0,   0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,     0,    0], // from error
	[   0,       0,   0,   0,   0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,     0,    0], // from noreturn
	[   0,       0,   0,   0,   0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,     0,    0], // from void
	[   0,       0,   0,   0,   0, 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,     0,    0], // from bool
	[   0,       0,   0,   1,   0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,     0,    0], // from null
	[   0,       0,   0,   1,   0, 0,  1,  1,  1,  0,  0,  0,  0,  1,  1,     0,    0], // from i8
	[   0,       0,   0,   1,   0, 0,  0,  1,  1,  0,  0,  0,  0,  1,  1,     0,    0], // from i16
	[   0,       0,   0,   1,   0, 0,  0,  0,  1,  0,  0,  0,  0,  1,  1,     0,    0], // from i32
	[   0,       0,   0,   1,   0, 0,  0,  0,  0,  0,  0,  0,  0,  1,  1,     0,    0], // from i64
	[   0,       0,   0,   1,   0, 0,  1,  1,  1,  0,  1,  1,  1,  1,  1,     0,    0], // from u8
	[   0,       0,   0,   1,   0, 0,  0,  1,  1,  0,  0,  1,  1,  1,  1,     0,    0], // from u16
	[   0,       0,   0,   1,   0, 0,  0,  0,  1,  0,  0,  0,  1,  1,  1,     0,    0], // from u32
	[   0,       0,   0,   1,   0, 0,  0,  0,  0,  0,  0,  0,  0,  1,  1,     0,    0], // from u64
	[   0,       0,   0,   1,   0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  1,     0,    0], // from f32
	[   0,       0,   0,   1,   0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,     0,    0], // from f64
	[   0,       0,   0,   1,   0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,     0,    0], // from $alias
	[   0,       0,   0,   1,   0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,     1,    0], // from $type
];

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

string[BasicType.max + 1] basicTypeNames = ["error", "noreturn", "void", "bool", "null", "i8", "i16", "i32",
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
