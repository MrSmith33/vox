/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// IR type info
module ir.ir_type;

import std.format : format;
import all;

enum IrValueType : ubyte
{
	void_t,
	i8,
	i16,
	i32,
	i64,
	//f32,
	//f64,
}

enum IrTypeKind : ubyte
{
	basic,
	pointer,
	array,
	struct_t
}

@(IrValueKind.type)
struct IrTypeHeader
{
	IrIndex prevType; // null for first type
	IrIndex nextType; // null for last type
}

@(IrValueKind.type)
align(8)
struct IrTypePointer
{
	IrTypeHeader header;
	IrIndex baseType;
}

@(IrValueKind.type)
align(8)
struct IrTypeArray
{
	IrTypeHeader header;
	IrIndex elemType;
	uint size;
}

@(IrValueKind.type)
align(8)
struct IrTypeStruct
{
	IrTypeHeader header;
	uint size;
	uint numMembers;
	IrTypeStructMember[0] members_payload;
	/// This must be called on the value in the buffer, not stack-local value
	IrTypeStructMember[] members() { return members_payload[0..numMembers];}
}

align(8)
struct IrTypeStructMember
{
	IrIndex type;
	uint offset;
}

///
struct IrTypeStorage
{
	FixedBuffer!ulong buffer;
	IrIndex firstType;
	IrIndex lastType;

	///
	IrIndex append(T)()
	{
		static assert(T.alignof == 8, "Can only store types aligned to 8 bytes");
		static assert(getIrValueKind!T == IrValueKind.type, "Can only add types");

		IrIndex typeIndex = IrIndex(cast(uint)buffer.length, getIrValueKind!T);

		enum allocSize = divCeil(T.sizeof, ulong.sizeof);
		T* type = cast(T*)buffer.voidPut(allocSize).ptr;
		*type = T.init;

		if (lastType.isDefined)
		{
			get!IrTypeHeader(lastType).nextType = typeIndex;
			type.header.prevType = lastType;
		}
		else
		{
			firstType = typeIndex;
		}
		lastType = typeIndex;

		return typeIndex;
	}

	///
	ref T get(T)(IrIndex index)
	{
		assert(index.kind != IrValueKind.none, "null index");
		assert(index.kind == getIrValueKind!T, format("%s != %s", index.kind, getIrValueKind!T));
		return *cast(T*)(&buffer.bufPtr[index.storageUintIndex]);
	}
}
