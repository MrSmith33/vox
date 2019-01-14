/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// IR type info
module ir.ir_type;

import std.format : format;
import std.traits : getUDAs;
import all;

/// Integers are both signed two-complement and unsigned
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

///
enum IrTypeKind : ubyte
{
	basic,
	pointer,
	array,
	struct_t
}

enum getIrTypeKind(T) = getUDAs!(T, IrTypeKind)[0];

IrIndex makeBasicTypeIndex(IrValueType t) {
	IrIndex result;
	result.kind = IrValueKind.type;
	result.typeKind = IrTypeKind.basic;
	result.typeIndex = t;
	return result;
}

///
@(IrValueKind.type)
struct IrTypeHeader
{
	IrIndex prevType; // null for first type
	IrIndex nextType; // null for last type
}

///
@(IrValueKind.type, IrTypeKind.pointer)
align(8)
struct IrTypePointer
{
	IrTypeHeader header;
	IrIndex baseType;
}

///
@(IrValueKind.type, IrTypeKind.array)
align(8)
struct IrTypeArray
{
	IrTypeHeader header;
	IrIndex elemType;
	uint size;
}

/// variadic type, members follow the struct in memory
@(IrValueKind.type, IrTypeKind.struct_t)
align(8)
struct IrTypeStruct
{
	IrTypeHeader header;
	uint size;
	uint alignment;
	uint numMembers;
	IrTypeStructMember[0] members_payload;
	/// This must be called on the value in the buffer, not stack-local value
	IrTypeStructMember[] members() { return members_payload.ptr[0..numMembers];}
}

///
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

	IrIndex appendStruct(uint numMembers)
	{
		IrIndex result = append!IrTypeStruct;

		{ // add member slots
			enum allocSize = divCeil(IrTypeStructMember.sizeof, ulong.sizeof);
			size_t numAllocatedSlots = allocSize * numMembers;
			ulong[] members = buffer.voidPut(numAllocatedSlots);
			members[] = 0;
		}

		get!IrTypeStruct(result).numMembers = numMembers;
		return result;
	}

	IrIndex appendPtr(IrIndex baseType)
	{
		IrIndex result = append!IrTypePointer;
		get!IrTypePointer(result).baseType = baseType;
		return result;
	}

	IrIndex appendArray(IrIndex elemType, uint size)
	{
		IrIndex result = append!IrTypeArray;
		IrTypeArray* array = &get!IrTypeArray(result);
		array.elemType = elemType;
		array.size = size;
		return result;
	}

	///
	IrIndex append(T)(uint howMany = 1)
	{
		static assert(T.alignof == 8, "Can only store types aligned to 8 bytes");
		static assert(getIrValueKind!T == IrValueKind.type, "Can only add types");
		enum typeKind = getIrTypeKind!T;

		IrIndex typeIndex;
		typeIndex.typeIndex = cast(uint)buffer.length;
		typeIndex.typeKind = typeKind;
		typeIndex.kind = getIrValueKind!T;

		enum allocSize = divCeil(T.sizeof, ulong.sizeof);
		size_t numAllocatedSlots = allocSize * howMany;
		T* type = cast(T*)buffer.voidPut(numAllocatedSlots).ptr;
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
		assert(index.isDefined, "null index");
		assert(index.kind == getIrValueKind!T, format("%s != %s", index.kind, getIrValueKind!T));
		static if (!is(T == IrTypeHeader))
			assert(index.typeKind == getIrTypeKind!T, format("%s != %s", index.typeKind, getIrTypeKind!T));
		return *cast(T*)(&buffer.bufPtr[index.storageUintIndex]);
	}

	bool isVoid(IrIndex type)
	{
		assert(type.isDefined, "null index");
		assert(type.isType, "not a type");
		return type.typeKind == IrTypeKind.basic && type.typeIndex == IrValueType.void_t;
	}

	uint typeSize(IrIndex type) {
		final switch (type.typeKind) {
			case IrTypeKind.basic:
			final switch (cast(IrValueType)type.typeIndex) {
				case IrValueType.void_t: return 0;
				case IrValueType.i8: return 1;
				case IrValueType.i16: return 2;
				case IrValueType.i32: return 4;
				case IrValueType.i64: return 8;
			}
			case IrTypeKind.pointer:
				return 8;
			case IrTypeKind.array:
				IrTypeArray* array = &get!IrTypeArray(type);
				uint elemSize = typeSize(array.elemType);
				return elemSize * array.size;
			case IrTypeKind.struct_t:
				return get!IrTypeStruct(type).size;
		}
	}

	uint typeAlignment(IrIndex type) {
		//writefln("TODO: type alignment");
		return 4;
	}

	IrIndex getPointerBaseType(IrIndex ptrType)
	{
		return get!IrTypePointer(ptrType).baseType;
	}

	IrIndex getArrayElementType(IrIndex arrayType)
	{
		return get!IrTypeArray(arrayType).elemType;
	}
}

/// Returns type of value
IrIndex getValueType(IrIndex value, ref IrFunction ir, ref CompilationContext context)
	out (res; res.isType, "Not a type")
{
	switch(value.kind) with(IrValueKind)
	{
		case constant:
			context.todo("getValueType %s", value.kind);
			assert(false);
		case global:
			context.todo("getValueType %s", value.kind);
			assert(false);
		case stackSlot:
			return ir.backendData.stackLayout[value].type;
		case virtualRegister:
			context.todo("getValueType %s", value.kind);
			assert(false);
		default:
			context.internal_error("Cannot get type of %s", value.kind);
			assert(false);
	}
}
