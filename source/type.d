/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module type;

import std.string : format;
import std.typecons : Flag, Yes, No;
import all;

// The order is the same as in TokenType enum
enum BasicType : ubyte {
	t_error,
	t_void,
	t_bool,

	t_i8,
	t_i16,
	t_i32,
	t_i64,
	//t_isize,

	t_u8,
	t_u16,
	t_u32,
	t_u64,
	//t_usize,

	t_f32,
	t_f64,
}

bool isTypeImplemented(BasicType t) {
	final switch(t) {
		case BasicType.t_error: return false;
		case BasicType.t_void: return false;
		case BasicType.t_bool: return false;
		case BasicType.t_i8: return false;
		case BasicType.t_i16: return false;
		case BasicType.t_i32: return false;
		case BasicType.t_i64: return false;
		case BasicType.t_u8: return false;
		case BasicType.t_u16: return false;
		case BasicType.t_u32: return false;
		case BasicType.t_u64: return false;
		case BasicType.t_f32: return false;
		case BasicType.t_f64: return false;
	}
}

// usage isAutoConvertibleFromToBasic[from][to]
immutable bool[13][13] isAutoConvertibleFromToBasic = [
	//err  void bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64  // to
	[   0,   0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0], // from error
	[   0,   0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0], // from void
	[   0,   0,  0,  1,  1,  1,  1,  1,  1,  1,  1, 1,  1], // from bool
	[   0,   0,  1,  0,  1,  1,  1,  0,  0,  0,  0, 1,  1], // from i8
	[   0,   0,  1,  0,  0,  1,  1,  0,  0,  0,  0, 1,  1], // from i16
	[   0,   0,  1,  0,  0,  0,  1,  0,  0,  0,  0, 1,  1], // from i32
	[   0,   0,  1,  0,  0,  0,  0,  0,  0,  0,  0, 1,  1], // from i64
	[   0,   0,  1,  0,  1,  1,  1,  0,  1,  1,  1, 1,  1], // from u8
	[   0,   0,  1,  0,  0,  1,  1,  0,  0,  1,  1, 1,  1], // from u16
	[   0,   0,  1,  0,  0,  0,  1,  0,  0,  0,  1, 1,  1], // from u32
	[   0,   0,  1,  0,  0,  0,  0,  0,  0,  0,  0, 1,  1], // from u64
	[   0,   0,  1,  0,  0,  0,  0,  0,  0,  0,  0, 0,  1], // from f32
	[   0,   0,  1,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0], // from f64
];

immutable BasicType[13][13] commonBasicType = (){ with(BasicType){ return [
	// error     void     bool       i8      i16      i32      i64       u8      u16      u32      u64      f32      f64
	[t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error], // error
	[t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error], // void
	[t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error], // bool
	[t_error, t_error, t_error, t_i8,    t_i16,   t_i32,   t_i64,   t_i8,    t_i16,   t_i32,   t_i64,   t_f32,   t_f64  ], // i8
	[t_error, t_error, t_error, t_i16,   t_i16,   t_i32,   t_i64,   t_i16,   t_i16,   t_i32,   t_i64,   t_f32,   t_f64  ], // i16
	[t_error, t_error, t_error, t_i32,   t_i32,   t_i32,   t_i64,   t_i32,   t_i32,   t_i32,   t_i64,   t_f32,   t_f64  ], // i32
	[t_error, t_error, t_error, t_i64,   t_i64,   t_i64,   t_i64,   t_i64,   t_i64,   t_i64,   t_i64,   t_f32,   t_f64  ], // i64
	[t_error, t_error, t_error, t_i8,    t_i16,   t_i32,   t_i64,   t_u8,    t_u16,   t_u32,   t_u64,   t_f32,   t_f64  ], // u8
	[t_error, t_error, t_error, t_i16,   t_i16,   t_i32,   t_i64,   t_u16,   t_u16,   t_u32,   t_u64,   t_f32,   t_f64  ], // u16
	[t_error, t_error, t_error, t_i32,   t_i32,   t_i32,   t_i64,   t_u32,   t_u32,   t_u32,   t_u64,   t_f32,   t_f64  ], // u32
	[t_error, t_error, t_error, t_i64,   t_i64,   t_i64,   t_i64,   t_u64,   t_u64,   t_u64,   t_u64,   t_f32,   t_f64  ], // u64
	[t_error, t_error, t_error, t_f32,   t_f32,   t_f32,   t_f32,   t_f32,   t_f32,   t_f32,   t_f32,   t_f32,   t_f64  ], // f32
	[t_error, t_error, t_error, t_f64,   t_f64,   t_f64,   t_f64,   t_f64,   t_f64,   t_f64,   t_f64,   t_f64,   t_f64  ], // f64
]; }
}();

string[13] basicTypeNames = ["error", "void", "bool", "i8", "i16", "i32",
"i64", "u8", "u16", "u32", "u64", "f32", "f64"];

bool isBasicTypeToken(TokenType tt) {
	return tt >= TYPE_TOKEN_FIRST && tt <= TYPE_TOKEN_LAST;
}

BasicType tokenTypeToBasicType(TokenType tt) {
	return cast(BasicType)(tt - TYPE_TOKEN_FIRST + BasicType.t_void);
}
