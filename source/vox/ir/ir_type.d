/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// IR type info
module vox.ir.ir_type;

import std.format : format;
import std.traits : getUDAs;
import vox.all;

/// Integers are two-complement of unknown signedness
enum IrBasicType : ubyte
{
	noreturn_t,
	void_t,
	i8,
	i16,
	i32,
	i64,
	f32,
	f64,
}

enum CovertionKind : ubyte {
	itoi, // int to int
	itof, // int to float
	ftoi, // float to int
	ftof, // float to float
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

struct SizeAndAlignment {
	uint size;
	ubyte alignmentPower;
	uint alignment() { return 1 << cast(uint)alignmentPower; }
}

///
IrIndex makeIrType(IrBasicType t) pure {
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
	uint numElements;
}

/// variadic type, members follow the struct in memory
@(IrValueKind.type, IrTypeKind.struct_t)
align(8)
struct IrTypeStruct
{
	IrTypeHeader header;
	uint size;
	ubyte alignmentPower;
	uint alignment() { return 1 << cast(uint)alignmentPower; }
	SizeAndAlignment sizealign() { return SizeAndAlignment(size, alignmentPower); }
	void sizealign(SizeAndAlignment sa) { size = sa.size; alignmentPower = sa.alignmentPower; }
	// all members have offset of 0
	// alignment is max alignment of members
	// IrAggregateConstant contains index of member in slot 0, and constant for that member in slot 1
	bool isUnion;
	uint numMembers;

	// Prevent type from copying because members will not be copied. Need to use ptr.
	@disable this(this);

	IrTypeStructMember[0] members_payload;
	/// This must be called on the value in the buffer, not stack-local value
	IrTypeStructMember[] members() return { return members_payload.ptr[0..numMembers];}
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
	ushort syscallNumber;

	// Prevent type from copying because members will not be copied. Need to use ptr.
	@disable this(this);

	IrIndex[0] payload; // result types followed by parameter types
	/// This must be called on the value in the buffer, not stack-local value
	IrIndex[] resultTypes() return { return payload.ptr[0..numResults];}
	IrIndex[] parameterTypes() return { return payload.ptr[numResults..numResults+numParameters];}
}

///
struct IrTypeStorage
{
	Arena!ulong buffer;
	IrIndex firstType;
	IrIndex lastType;

	IrIndex appendStruct(uint numMembers, bool isUnion = false)
	{
		IrIndex result = append!IrTypeStruct;

		{ // add member slots
			enum allocSize = divCeil(IrTypeStructMember.sizeof, ulong.sizeof);
			size_t numAllocatedSlots = allocSize * numMembers;
			ulong[] members = buffer.voidPut(numAllocatedSlots);
			members[] = 0;
		}

		get!IrTypeStruct(result).numMembers = numMembers;
		get!IrTypeStruct(result).isUnion = isUnion;
		return result;
	}

	IrIndex appendFuncSignature(uint numResults, uint numParameters, CallConvention callConv)
	{
		IrIndex result = append!IrTypeFunction;

		{ // add slots for results and parameters
			uint numIndices = numResults + numParameters;
			size_t numSlots = divCeil(IrIndex.sizeof * numIndices, ulong.sizeof);
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

	IrIndex appendArray(IrIndex elemType, uint numElements)
	{
		assert(elemType.isDefined);
		IrIndex result = append!IrTypeArray;
		IrTypeArray* array = &get!IrTypeArray(result);
		array.elemType = elemType;
		array.numElements = numElements;
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
		if (buffer.uintLength >= (1 << IrIndex.TYPE_INDEX_BITS)) {
			throw new Exception("Out of index space in IrIndex.typeIndex in IrTypeStorage");
		}
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
		assert(index.isDefined,  format("get!%s null index", T.stringof));
		assert(index.kind == getIrValueKind!T, format("%s != %s", index.kind, getIrValueKind!T));
		static if (!is(T == IrTypeHeader))
			assert(index.typeKind == getIrTypeKind!T, format("%s != %s", index.typeKind, getIrTypeKind!T));
		return *cast(T*)(&buffer.bufPtr[index.typeIndex]);
	}

	uint typeSize(IrIndex type) {
		return typeSizeAndAlignment(type).size;
	}

	SizeAndAlignment typeSizeAndAlignment(IrIndex type) {
		assert(type.isType, format("not a type (%s)", type));
		final switch (type.typeKind) {
			case IrTypeKind.basic:
			final switch (cast(IrBasicType)type.typeIndex) {
				case IrBasicType.noreturn_t: return SizeAndAlignment(0, 0);
				case IrBasicType.void_t: return SizeAndAlignment(0, 0);
				case IrBasicType.i8: return SizeAndAlignment(1, 0);
				case IrBasicType.i16: return SizeAndAlignment(2, 1);
				case IrBasicType.i32: return SizeAndAlignment(4, 2);
				case IrBasicType.i64: return SizeAndAlignment(8, 3);
				case IrBasicType.f32: return SizeAndAlignment(4, 2);
				case IrBasicType.f64: return SizeAndAlignment(8, 3);
			}
			case IrTypeKind.pointer:
				return SizeAndAlignment(8, 3);
			case IrTypeKind.array:
				IrTypeArray* array = &get!IrTypeArray(type);
				SizeAndAlignment elemInfo = typeSizeAndAlignment(array.elemType);
				return SizeAndAlignment(elemInfo.size * array.numElements, elemInfo.alignmentPower);
			case IrTypeKind.struct_t:
				auto s = &get!IrTypeStruct(type);
				return s.sizealign;
			case IrTypeKind.func_t:
				return SizeAndAlignment(0, 0);
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

	/// Returns offset + type of member indicated by indices
	IrTypeStructMember getAggregateMember(IrIndex aggrType, CompilationContext* c, IrIndex[] indices...)
	{
		ulong offset = 0;
		foreach(IrIndex memberIndex; indices)
		{
			c.assertf(memberIndex.isSimpleConstant, "Aggregates can only be indexed with constants, not with %s", memberIndex);
			ulong indexVal = c.constants.get(memberIndex).i64;
			switch(aggrType.typeKind)
			{
				case IrTypeKind.array:
					IrIndex elemType = getArrayElementType(aggrType);
					offset += indexVal * typeSize(elemType);
					aggrType = elemType;
					break;

				case IrTypeKind.struct_t:
					IrTypeStructMember[] members = get!IrTypeStruct(aggrType).members;
					c.assertf(indexVal < members.length,
						"Indexing member %s of %s-member struct",
						indexVal, members.length);
					IrTypeStructMember member = members[indexVal];
					offset += member.offset;
					aggrType = member.type;
					break;

				default:
					c.internal_error("Cannot index into %s", aggrType.typeKind);
			}
		}
		return IrTypeStructMember(aggrType, cast(uint)offset);
	}

	IrIndex getReturnType(IrIndex funcSigType, CompilationContext* c)
	{
		auto func = &get!IrTypeFunction(funcSigType);
		if (func.numResults == 0)
			return makeIrType(IrBasicType.void_t);
		c.assertf(func.numResults == 1, "getFuncSignatureReturnType on func with %s results", func.numResults);
		return func.resultTypes[0];
	}

	CallConv* getCalleeCallConv(IrIndex callee, IrFunction* ir, CompilationContext* c)
	{
		if (callee.isFunction)
		{
			return ir.getCallConv(c);
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
				return isSameType(arrayA.elemType, arrayB.elemType) && arrayA.numElements == arrayB.numElements;
			case IrTypeKind.struct_t:
				if (b.typeKind != IrTypeKind.struct_t) return false;
				IrTypeStructMember[] membersA = get!IrTypeStruct(a).members;
				IrTypeStructMember[] membersB = get!IrTypeStruct(b).members;
				if (membersA.length != membersB.length) return false;
				foreach(i, IrTypeStructMember memA; membersA) {
					if (!isSameType(memA.type, membersB[i].type)) return false;
					if (memA.offset != membersB[i].offset) return false;
				}
				return true;
			case IrTypeKind.func_t:
				if (b.typeKind != IrTypeKind.func_t) return false;
				IrTypeFunction* funcA = &get!IrTypeFunction(a);
				IrTypeFunction* funcB = &get!IrTypeFunction(b);
				if (funcA.numResults != funcB.numResults) return false;
				if (funcA.numParameters != funcB.numParameters) return false;
				foreach(i, IrIndex typeA; funcA.payload.ptr[0..funcA.numParameters+funcA.numParameters]) {
					if (!isSameType(typeA, funcB.payload.ptr[i])) return false;
				}
				return true;
		}
	}
}

/// Returns type of value
IrIndex getValueType(IrIndex value, IrFunction* ir, CompilationContext* c)
	out (res; res.isType, format("Not a type %s -> %s", value, res))
{
	switch(value.kind) with(IrValueKind)
	{
		case constant:
			return c.constants.get(value).type;
		case constantAggregate:
			return c.constants.getAggregate(value).type;
		case constantZero:
			value.kind = IrValueKind.type;
			return value;
		case global:
			IrGlobal* global = c.globals.get(value);
			c.assertf(global.type.isDefined, "Global has no type");
			return global.type;
		case stackSlot:
			return ir.getStackSlot(value).type;
		case virtualRegister:
			return ir.getVirtReg(value).type;
		case func:
			return c.types.appendPtr(c.getFunction(value).signature.get!FunctionSignatureNode(c).getIrType(c));
		default:
			c.internal_error("Cannot get type of %s", value.kind);
	}
}

IrArgSize getValueTypeArgSize(IrIndex value, IrFunction* ir, CompilationContext* context)
{
	if (value.isPhysReg) return cast(IrArgSize)value.physRegSize;
	IrIndex type = getValueType(value, ir, context);
	return sizeToIrArgSize(context.types.typeSize(type), context);
}

IrArgSize getTypeArgSize(IrIndex type, CompilationContext* context)
{
	return sizeToIrArgSize(context.types.typeSize(type), context);
}
