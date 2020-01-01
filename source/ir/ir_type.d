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
	struct_t,
	func_t
}

enum getIrTypeKind(T) = getUDAs!(T, IrTypeKind)[0];

///
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

	// Prevent type from copying because members will not be copied. Need to use ptr.
	@disable this(this);

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

/// variadic type, members follow the struct in memory
@(IrValueKind.type, IrTypeKind.func_t)
align(8)
struct IrTypeFunction
{
	IrTypeHeader header;
	uint numResults;
	uint numParameters;
	CallConvention callConv;

	// Prevent type from copying because members will not be copied. Need to use ptr.
	@disable this(this);

	IrIndex[0] payload; // result types followed by paramter types
	/// This must be called on the value in the buffer, not stack-local value
	IrIndex[] resultTypes() { return payload.ptr[0..numResults];}
	IrIndex[] parameterTypes() { return payload.ptr[numResults..numResults+numParameters];}
}

///
struct IrTypeStorage
{
	Arena!ulong buffer;
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

	IrIndex appendFuncSignature(uint numResults, uint numParameters, CallConvention callConv)
	{
		IrIndex result = append!IrTypeFunction;

		{ // add slots for results and paramters
			uint numIndicies = numResults + numParameters;
			size_t numSlots = divCeil(IrIndex.sizeof * numIndicies, ulong.sizeof);
			ulong[] data = buffer.voidPut(numSlots);
			data[] = 0;
		}

		auto func = &get!IrTypeFunction(result);
		func.numResults = numResults;
		func.numParameters = numParameters;
		func.callConv = callConv;
		return result;
	}

	IrIndex appendPtr(IrIndex baseType)
	{
		assert(baseType.isDefined);
		IrIndex result = append!IrTypePointer;
		get!IrTypePointer(result).baseType = baseType;
		return result;
	}

	IrIndex appendArray(IrIndex elemType, uint size)
	{
		assert(elemType.isDefined);
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

		//uint len = buffer.uintLength;
		IrIndex typeIndex;
		typeIndex.typeIndex = buffer.uintLength;
		typeIndex.typeKind = typeKind;
		typeIndex.kind = getIrValueKind!T;

		enum allocSize = divCeil(T.sizeof, ulong.sizeof);
		size_t numAllocatedSlots = allocSize * howMany;
		T* type = cast(T*)buffer.voidPut(numAllocatedSlots).ptr;
		*type = T.init;
		//writefln("append %s %s->%s %s", T.stringof, len, buffer.uintLength, typeIndex);

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
		return *cast(T*)(&buffer.bufPtr[index.typeIndex]);
	}

	uint typeSize(IrIndex type) {
		assert(type.isType, format("not a type (%s)", type));
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
			case IrTypeKind.func_t:
				return 0;
		}
	}

	uint typeAlignment(IrIndex type) {
		assert(type.isType, format("not a type (%s)", type));
		final switch (type.typeKind) {
			case IrTypeKind.basic:
			final switch (cast(IrValueType)type.typeIndex) {
				case IrValueType.void_t: return 1;
				case IrValueType.i8: return 1;
				case IrValueType.i16: return 2;
				case IrValueType.i32: return 4;
				case IrValueType.i64: return 8;
			}
			case IrTypeKind.pointer:
				return 8;
			case IrTypeKind.array:
				IrTypeArray* array = &get!IrTypeArray(type);
				return typeAlignment(array.elemType);
			case IrTypeKind.struct_t:
				return get!IrTypeStruct(type).alignment;
			case IrTypeKind.func_t:
				return 0;
		}
	}

	IrIndex getPointerBaseType(IrIndex ptrType)
	{
		return get!IrTypePointer(ptrType).baseType;
	}

	IrIndex getArrayElementType(IrIndex arrayType)
	{
		return get!IrTypeArray(arrayType).elemType;
	}

	IrIndex getStructMemberType(IrIndex structType, uint memberIndex, ref CompilationContext context)
	{
		IrTypeStructMember[] members = get!IrTypeStruct(structType).members;

		context.assertf(memberIndex < members.length,
			"Indexing member %s of %s-member struct",
			memberIndex, members.length);

		IrTypeStructMember member = members[memberIndex];
		return member.type;
	}

	IrIndex getReturnType(IrIndex funcSigType, ref CompilationContext c)
	{
		auto func = &get!IrTypeFunction(funcSigType);
		if (func.numResults == 0)
			return makeBasicTypeIndex(IrValueType.void_t);
		c.assertf(func.numResults == 1, "getFuncSignatureReturnType on func with %s results", func.numResults);
		return func.resultTypes[0];
	}

	CallConv* getCalleeCallConv(IrIndex callee, IrFunction* ir, CompilationContext* c)
	{
		if (callee.isFunction)
		{
			return c.getFunction(callee).backendData.getCallConv(c);
		}
		else
		{
			IrIndex type = getValueType(callee, ir, c);
			if (type.isTypePointer)
			{
				IrIndex base = getPointerBaseType(type);
				if (base.isTypeFunction)
				{
					CallConvention callConv = get!IrTypeFunction(base).callConv;
					return callConventions[callConv];
				}
			}
		}
		c.internal_error("cannot get call convention %s", callee);
		assert(false);
	}

	bool isSameType(IrIndex a, IrIndex b) {
		//writefln("isSameType %s %s", a, b);
		assert(a.isType, format("not a type (%s)", a));
		assert(b.isType, format("not a type (%s)", b));

		if (a == b) return true;

		final switch (a.typeKind) {
			case IrTypeKind.basic:
				return b.typeKind == IrTypeKind.basic && a.typeIndex == b.typeIndex;
			case IrTypeKind.pointer:
				if (b.typeKind != IrTypeKind.pointer) return false;
				auto baseA = getPointerBaseType(a);
				auto baseB = getPointerBaseType(b);
				//writefln("  ptr %s %s", baseA, baseB);
				return isSameType(baseA, baseB);
			case IrTypeKind.array:
				if (b.typeKind != IrTypeKind.array) return false;
				IrTypeArray* arrayA = &get!IrTypeArray(a);
				IrTypeArray* arrayB = &get!IrTypeArray(b);
				return isSameType(arrayA.elemType, arrayB.elemType) && arrayA.size == arrayB.size;
			case IrTypeKind.struct_t:
				if (b.typeKind != IrTypeKind.struct_t) return false;
				IrTypeStructMember[] membersA = get!IrTypeStruct(a).members;
				IrTypeStructMember[] membersB = get!IrTypeStruct(b).members;
				foreach(i, IrTypeStructMember memA; membersA) {
					if (!isSameType(memA.type, membersB[i].type)) return false;
					if (memA.offset != membersB[i].offset) return false;
				}
				return true;
			case IrTypeKind.func_t:
				return a == b;
		}
	}
}

/// Returns type of value
IrIndex getValueType(IrIndex value, IrFunction* ir, CompilationContext* context)
	out (res; res.isType, format("Not a type %s -> %s", value, res))
{
	switch(value.kind) with(IrValueKind)
	{
		case constant:
			return context.constants.get(value).type(value);
		case constantAggregate:
			return context.constants.getAggregate(value).type;
		case constantZero:
			value.kind = IrValueKind.type;
			return value;
		case global:
			IrGlobal* global = &context.globals.get(value);
			context.assertf(global.type.isDefined, "Global has no type");
			return global.type;
		case stackSlot:
			return ir.backendData.stackLayout[value].type;
		case virtualRegister:
			return ir.getVirtReg(value).type;
		case func:
			return context.types.appendPtr(context.getFunction(value).signature.get!FunctionSignatureNode(context).irType);
		default:
			context.internal_error("Cannot get type of %s", value.kind);
			assert(false);
	}
}

IrArgSize getValueTypeArgSize(IrIndex value, IrFunction* ir, CompilationContext* context)
{
	if (value.isPhysReg) return cast(IrArgSize)value.physRegSize;
	IrIndex type = getValueType(value, ir, context);
	return sizeToIrArgSize(context.types.typeSize(type), context);
}

IrArgSize getTypeArgSize(IrIndex type, IrFunction* ir, CompilationContext* context)
{
	return sizeToIrArgSize(context.types.typeSize(type), context);
}
