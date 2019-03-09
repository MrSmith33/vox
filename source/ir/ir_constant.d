/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// IR constant
module ir.ir_constant;

import std.string : format;
import all;

///
enum IrConstantKind : ubyte {
	/// Unsigned integer constant. Up to 24 bits. Stored directly in IrIndex.
	intUnsignedSmall,
	/// Signed integer constant. Up to 24 bits. Stored directly in IrIndex.
	intSignedSmall,
	/// Unsigned integer constant. Stored in constants buffer.
	intUnsignedBig,
	/// Signed integer constant. Stored in constants buffer.
	intSignedBig,
	/// Complex type initializer. Stored as IrIndex[].
	aggregate
}

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
	Arena!IrConstant buffer;

	///
	IrIndex add(IrConstant con)
	{
		IrIndex conIndex = IrIndex(cast(uint)buffer.length, IrValueKind.constant);
		buffer.put(con);
		return conIndex;
	}

	///
	ref IrConstant get(IrIndex index)
	{
		assert(index.kind == IrValueKind.constant, format("Not a constant (%s)", index));
		assert(index.storageUintIndex < buffer.length, "Not in bounds");
		return buffer[index.storageUintIndex];
	}
}
