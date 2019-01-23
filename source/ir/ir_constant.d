/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// IR constant
module ir.ir_constant;

import std.string : format;
import all;

/// Stores numeric constant data
/// Type is implicitly the smallest signed int type. TODO more types of constants
@(IrValueKind.constant)
struct IrConstant
{
	this(long value, IrIndex type) {
		this.i64 = value;
		this.type = type;
	}
	this(long value) {
		this.i64 = value;
		switch(numSignedBytes) {
			case 1: type = makeBasicTypeIndex(IrValueType.i8); break;
			case 2: type = makeBasicTypeIndex(IrValueType.i16); break;
			case 4: type = makeBasicTypeIndex(IrValueType.i32); break;
			case 8: type = makeBasicTypeIndex(IrValueType.i64); break;
			default: assert(false);
		}
	}

	ubyte numSignedBytes() {
		if (cast(byte)(i64 & 0xFF) == i64)
			return 1;
		else if (cast(short)(i64 & 0xFFFF) == i64)
			return 2;
		else if (cast(int)(i64 & 0xFFFF_FFFF) == i64)
			return 4;
		else
			return 8;
	}

	ubyte numUnsignedBytes() {
		if (cast(ubyte)(i64 & 0xFF) == i64)
			return 1;
		else if (cast(ushort)(i64 & 0xFFFF) == i64)
			return 2;
		else if (cast(uint)(i64 & 0xFFFF_FFFF) == i64)
			return 4;
		else
			return 8;
	}

	union {
		bool i1;
		byte i8;
		short i16;
		int i32;
		long i64;
	}
	IrIndex type;
}

///
struct IrConstantStorage
{
	IrConstant[] array;

	///
	IrIndex add(IrConstant con)
	{
		IrIndex conIndex = IrIndex(cast(uint)array.length, IrValueKind.constant);
		array ~= con;
		return conIndex;
	}

	///
	ref IrConstant get(IrIndex index)
	{
		assert(index.kind == IrValueKind.constant, format("Not a constant (%s)", index));
		assert(index.storageUintIndex < array.length, "Not in bounds");
		return array[index.storageUintIndex];
	}
}
