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
}

/// Stores numeric constant data
/// Type is implicitly the smallest signed int type. TODO more types of constants
@(IrValueKind.constant)
struct IrConstant
{
	this(long value) {
		this.i64 = value;
	}

	static IrIndex type(IrIndex index) {
		final switch(index.constantSize) with(IrArgSize) {
			case size8: return makeBasicTypeIndex(IrValueType.i8); break;
			case size16: return makeBasicTypeIndex(IrValueType.i16); break;
			case size32: return makeBasicTypeIndex(IrValueType.i32); break;
			case size64: return makeBasicTypeIndex(IrValueType.i64); break;
		}
	}

	IrArgSize payloadSize(IrIndex index) {
		if (index.isSignedConstant)
			return argSizeIntSigned(i64);
		else
			return argSizeIntUnsigned(i64);
	}

	union {
		bool i1;
		byte i8;
		short i16;
		int i32;
		long i64;
	}
}

enum IsSigned : bool {
	no = false,
	yes = true,
}

enum ulong MASK_24_BITS = (1 << 24) - 1;

@(IrValueKind.constantAggregate)
struct IrAggregateConstant
{
	IrIndex type;
	uint numMembers;

	// Prevent type from copying because members will not be copied. Need to use ptr.
	@disable this(this);

	IrIndex[0] _memberPayload;
	IrIndex[] members() {
		return _memberPayload.ptr[0..numMembers];
	}
}

///
struct IrConstantStorage
{
	Arena!IrConstant buffer;
	Arena!uint aggregateBuffer;

	///
	IrIndex add(ulong value, IsSigned signed)
	{
		if (signed)
			return add(value, signed, argSizeIntSigned(value));
		else
			return add(value, signed, argSizeIntUnsigned(value));
	}

	IrIndex add(ulong value, IsSigned signed, IrArgSize constantSize)
	{
		IrIndex result;

		if (value == 0) {
			final switch(constantSize) with(IrArgSize) {
				case size8: return makeBasicTypeIndex(IrValueType.i8).typeZeroConstant;
				case size16: return makeBasicTypeIndex(IrValueType.i16).typeZeroConstant;
				case size32: return makeBasicTypeIndex(IrValueType.i32).typeZeroConstant;
				case size64: return makeBasicTypeIndex(IrValueType.i64).typeZeroConstant;
			}
		}

		if (signed) {
			bool fitsInSmallInt = ((value << 40) >> 40) == value;
			if (fitsInSmallInt) {
				result.constantIndex = cast(uint)(value & MASK_24_BITS);
				result.constantKind = IrConstantKind.intSignedSmall;
			} else {
				result.constantIndex = cast(uint)buffer.length;
				result.constantKind = IrConstantKind.intSignedBig;
				buffer.put(IrConstant(value));
			}
		} else {
			bool fitsInSmallInt = (value & MASK_24_BITS) == value;
			if (fitsInSmallInt) {
				result.constantIndex = cast(uint)(value & MASK_24_BITS);
				result.constantKind = IrConstantKind.intUnsignedSmall;
			} else {
				result.constantIndex = cast(uint)buffer.length;
				result.constantKind = IrConstantKind.intUnsignedBig;
				buffer.put(IrConstant(value));
			}
		}
		result.constantSize = constantSize;
		result.kind = IrValueKind.constant;
		return result;
	}

	/// Creates aggrecate constant without initializing members
	IrIndex addAggrecateConstant(IrIndex type, uint numMembers)
	{
		assert (type.isTypeStruct || type.isTypeArray);
		IrIndex resultIndex = IrIndex(cast(uint)aggregateBuffer.length, IrValueKind.constantAggregate);
		uint allocSize = cast(uint)divCeil(IrAggregateConstant.sizeof, uint.sizeof) + numMembers;
		aggregateBuffer.voidPut(allocSize);
		IrAggregateConstant* agg = &getAggregate(resultIndex);
		agg.type = type;
		agg.numMembers = numMembers;
		return resultIndex;
	}

	///
	IrIndex addAggrecateConstant(IrIndex type, IrIndex[] members...) {
		IrIndex resultIndex = addAggrecateConstant(type, cast(uint)members.length);
		IrAggregateConstant* agg = &getAggregate(resultIndex);
		agg.members[] = members;
		return resultIndex;
	}

	static IrIndex addZeroConstant(IrIndex type)
	{
		type.kind = IrValueKind.constantZero;
		return type;
	}

	///
	ref IrAggregateConstant getAggregate(IrIndex index) {
		assert(index.kind == IrValueKind.constantAggregate, format("Not a constantAggregate (%s)", index));
		return *cast(IrAggregateConstant*)(&aggregateBuffer[index.storageUintIndex]);
	}

	///
	IrIndex getAggregateMember(IrIndex index, uint memberIndex) {
		return getAggregate(index).members[memberIndex];
	}

	///
	IrConstant get(IrIndex index)
	{
		if (index.kind == IrValueKind.constant)
		{
			final switch(index.constantKind) with(IrConstantKind) {
				case intUnsignedSmall: return IrConstant(index.constantIndex);
				case intSignedSmall: return IrConstant((cast(int)index.constantIndex << 8) >> 8);
				case intUnsignedBig, intSignedBig:
					assert(index.constantIndex < buffer.length,
						format("Not in bounds: index.constantIndex(%s) < buffer.length(%s)",
							index.constantIndex, buffer.length));
					return buffer[index.constantIndex];
			}
		}
		else if (index.kind == IrValueKind.constantZero)
		{
			return IrConstant(0);
		}
		else
			assert(false, format("Not a constant (%s)", index));
	}

	enum IrIndex ZERO = makeConst(0, IrConstantKind.intSignedSmall);
	enum IrIndex ONE = makeConst(1, IrConstantKind.intSignedSmall);
}

private IrIndex makeConst(uint val, IrConstantKind kind) {
	IrIndex result;
	result.storageUintIndex = val | kind << 24;
	result.kind = IrValueKind.constant;
	return result;
}

/// Stores constant into buffer
alias UnknownValueHandler = void delegate(ubyte[] buffer, IrIndex index, CompilationContext* c);
void constantToMem(ubyte[] buffer, IrIndex index, CompilationContext* c, UnknownValueHandler handler = null)
{
	if (index.isConstant)
	{
		IrConstant con = c.constants.get(index);
		switch(buffer.length)
		{
			case 1:
				if (index.constantSize > IrArgSize.size8) goto default;
				buffer[0] = con.i8;
				break;
			case 2:
				if (index.constantSize > IrArgSize.size16) goto default;
				*(cast(short*)buffer.ptr) = con.i16;
				break;
			case 4:
				if (index.constantSize > IrArgSize.size32) goto default;
				*(cast(int*)buffer.ptr) = con.i32;
				break;
			case 8:
				*(cast(long*)buffer.ptr) = con.i64;
				break;
			default:
				c.internal_error("Cannot store constant of size %s, into memory of size %s bytes",
					index.constantSize, buffer.length);
		}
	}
	else if (index.isConstantZero)
	{
		uint typeSize = c.types.typeSize(index.constantZeroType);
		c.assertf(typeSize == buffer.length,
			"Cannot store zero constant of size %s, into memory of size %s bytes",
			typeSize, buffer.length);
		buffer[] = 0;
	}
	else if (index.isConstantAggregate)
	{
		IrAggregateConstant* con = &c.constants.getAggregate(index);

		switch(con.type.typeKind) with(IrTypeKind) {
			case struct_t:
				IrTypeStruct* structType = &c.types.get!IrTypeStruct(con.type);
				c.assertf(structType.size == buffer.length,
					"Cannot store struct constant of size %s, into memory of size %s bytes",
					structType.size, buffer.length);
				IrIndex[] args = con.members;
				foreach (i, IrTypeStructMember member; structType.members)
				{
					uint memberOffset = member.offset;
					uint memberSize = c.types.typeSize(member.type);
					constantToMem(buffer[memberOffset..memberOffset+memberSize], args[i], c, handler);
				}
				break;
			case array:
				IrTypeArray* arrayType = &c.types.get!IrTypeArray(con.type);
				uint elemSize = c.types.typeSize(arrayType.elemType);
				uint typeSize = arrayType.size * elemSize;
				c.assertf(typeSize == buffer.length,
					"Cannot store array constant of size %s, into memory of size %s bytes",
					typeSize, buffer.length);
				IrIndex[] args = con.members;
				foreach (i; 0..arrayType.size)
				{
					uint memberOffset = i * elemSize;
					constantToMem(buffer[memberOffset..memberOffset+elemSize], args[i], c, handler);
				}
				break;
			default: assert(false);
		}
	}
	else
	{
		if (handler) handler(buffer, index, c);
		else c.internal_error("%s is not a constant", index);
	}
}

IrIndex memToConstant(ubyte[] buffer, IrIndex type, CompilationContext* c, IsSigned signed)
{
	c.assertf(type.isTypeBasic, "%s", type);

	ulong value;
	IrArgSize constSize;
	switch(type.typeIndex)
	{
		case IrValueType.i8:
			c.assertf(1 == buffer.length,
				"Cannot load i8 constant from memory of size %s bytes", buffer.length);
			value = *(cast(byte*)buffer.ptr);
			constSize = IrArgSize.size8;
			break;
		case IrValueType.i16:
			c.assertf(2 == buffer.length,
				"Cannot load i8 constant from memory of size %s bytes", buffer.length);
			value = *(cast(short*)buffer.ptr);
			constSize = IrArgSize.size16;
			break;
		case IrValueType.i32:
			c.assertf(4 == buffer.length,
				"Cannot load i8 constant from memory of size %s bytes", buffer.length);
			value = *(cast(int*)buffer.ptr);
			constSize = IrArgSize.size32;
			break;
		case IrValueType.i64:
			c.assertf(8 == buffer.length,
				"Cannot load i8 constant from memory of size %s bytes", buffer.length);
			value = *(cast(long*)buffer.ptr);
			constSize = IrArgSize.size64;
			break;
		default:
			c.internal_error("memToConstant %s", cast(IrValueType)type.typeIndex);
			assert(false);
	}
	return c.constants.add(value, signed, constSize);
}
