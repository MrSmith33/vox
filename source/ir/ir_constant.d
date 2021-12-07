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
	/// Sign-extended integer constant. Up to 24 bits. Stored directly in IrIndex.
	smallSx,
	/// Zero-extended integer constant. Up to 24 bits. Stored directly in IrIndex.
	smallZx,
	/// integer or float constant. Stored in constants buffer. Stores precise type.
	big,
}

/// Stores numeric constant data
/// Type is implicitly the smallest signed int type. TODO more types of constants
@(IrValueKind.constant)
struct IrConstant
{
	this(IrIndex type, ulong value) {
		this.type = type;
		this.u64 = value;
	}

	this(float value) {
		this.type = makeIrType(IrBasicType.f32);
		this.f32 = value;
	}

	this(double value) {
		this.type = makeIrType(IrBasicType.f64);
		this.f64 = value;
	}

	bool intFitsIn32Bits() {
		return u32 == u64 || i32 == i64;
	}

	IrConstVal value;
	IrIndex type;

	alias value this;
}

union IrConstVal {
	ulong u64;
	long i64;
	bool i1;
	byte i8;
	ubyte u8;
	short i16;
	ushort u16;
	int i32;
	struct {
		uint u32;
		uint u32_top;
	}
	float f32;
	double f64;
}

enum IsSigned : bool {
	no = false,
	yes = true,
}

enum IsSignExtended : bool {
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
	IrIndex[] members() return {
		return _memberPayload.ptr[0..numMembers];
	}
}

///
struct IrConstantStorage
{
	Arena!IrConstant buffer;
	Arena!uint aggregateBuffer;

	///
	IrIndex add(float value)
	{
		if (value == 0) return makeIrType(IrBasicType.f32).zeroConstantOfType;
		IrIndex result;
		result.kind = IrValueKind.constant;
		result.constantIndex = cast(uint)buffer.length;
		result.constantKind = IrConstantKind.big;
		buffer.put(IrConstant(value));
		return result;
	}

	IrIndex add(double value)
	{
		if (value == 0) return makeIrType(IrBasicType.f64).zeroConstantOfType;
		IrIndex result;
		result.kind = IrValueKind.constant;
		result.constantIndex = cast(uint)buffer.length;
		result.constantKind = IrConstantKind.big;
		buffer.put(IrConstant(value));
		return result;
	}

	///
	IrIndex add(IrIndex type, long value)
	{
		if (value == 0) return type.zeroConstantOfType;

		IrIndex result;
		result.kind = IrValueKind.constant;

		if (type.isTypeInteger) {
			bool fitsInSmallIntWithSx = ((value << 40) >> 40) == value;
			if (fitsInSmallIntWithSx) {
				result.constantSize = cast(IrArgSize)(type.typeIndex - IrBasicType.i8);
				result.constantIndex = cast(uint)(value & MASK_24_BITS);
				result.constantKind = IrConstantKind.smallSx;
				return result;
			}

			bool fitsInSmallIntWithZx = (value & MASK_24_BITS) == value;
			if (fitsInSmallIntWithZx) {
				result.constantSize = cast(IrArgSize)(type.typeIndex - IrBasicType.i8);
				result.constantIndex = cast(uint)(value & MASK_24_BITS);
				result.constantKind = IrConstantKind.smallZx;
				return result;
			}
		}
		result.constantIndex = cast(uint)buffer.length;
		result.constantKind = IrConstantKind.big;
		buffer.put(IrConstant(type, value));
		return result;
	}

	/// Creates aggrecate constant without initializing members
	IrIndex addAggrecateConstant(IrIndex type, uint numMembers)
	{
		assert(type.isTypeAggregate);
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
		assert(index.isConstantAggregate, format("Not a constantAggregate (%s)", index));
		return *cast(IrAggregateConstant*)(&aggregateBuffer[index.storageUintIndex]);
	}

	/// memberIndex must be an integer constant
	// IrIndex aggrType, CompilationContext* c, IrIndex[] indices...
	IrIndex getAggregateMember(IrIndex aggrValue, IrIndex memberIndex, CompilationContext* c) {
		if (aggrValue.isConstantAggregate) {
			uint memberIndexVal = get(memberIndex).i32;
			return getAggregate(aggrValue).members[memberIndexVal];
		} else if (aggrValue.isConstantZero) {
			IrIndex type = aggrValue.typeOfConstantZero;
			IrTypeStructMember member = c.types.getAggregateMember(type, c, memberIndex);
			return member.type.zeroConstantOfType;
		}
		else {
			c.unreachable;
		}
	}

	///
	IrConstant get(IrIndex index)
	{
		if (index.kind == IrValueKind.constant)
		{
			final switch(index.constantKind) with(IrConstantKind) {
				case smallZx: return IrConstant(makeIrType(cast(IrBasicType)(IrBasicType.i8 + index.constantSize)), index.constantIndex);
				case smallSx: return IrConstant(makeIrType(cast(IrBasicType)(IrBasicType.i8 + index.constantSize)), (cast(int)index.constantIndex << 8) >> 8);
				case big:
					assert(index.constantIndex < buffer.length,
						format("Not in bounds: index.constantIndex(%s) < buffer.length(%s)",
							index.constantIndex, buffer.length));
					return buffer[index.constantIndex];
			}
		}
		else if (index.kind == IrValueKind.constantZero)
		{
			return IrConstant(index.typeOfConstantZero, 0);
		}
		else
			assert(false, format("Not a constant (%s)", index));
	}
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
				if (c.types.typeSize(con.type) > 1) goto default;
				buffer[0] = con.i8;
				break;
			case 2:
				if (c.types.typeSize(con.type) > 2) goto default;
				*(cast(short*)buffer.ptr) = con.i16;
				break;
			case 4:
				if (c.types.typeSize(con.type) > 4) goto default;
				*(cast(int*)buffer.ptr) = con.i32;
				break;
			case 8:
				*(cast(long*)buffer.ptr) = con.i64;
				break;
			default:
				c.internal_error("Cannot store constant %s of size %s, into memory of size %s bytes",
					con.i64, c.types.typeSize(con.type), buffer.length);
		}
	}
	else if (index.isConstantZero)
	{
		uint typeSize = c.types.typeSize(index.typeOfConstantZero);
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
				c.assertf(structType.sizealign.size == buffer.length,
					"Cannot store struct constant of size %s, into memory of size %s bytes",
					structType.sizealign.size, buffer.length);
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
				uint typeSize = arrayType.numElements * elemSize;
				c.assertf(typeSize == buffer.length,
					"Cannot store array constant of size %s, into memory of size %s bytes",
					typeSize, buffer.length);
				IrIndex[] args = con.members;
				foreach (i; 0..arrayType.numElements)
				{
					uint memberOffset = i * elemSize;
					constantToMem(buffer[memberOffset..memberOffset+elemSize], args[i], c, handler);
				}
				break;
			default: c.internal_error("%s", con.type.typeKind);
		}
	}
	else
	{
		if (handler) handler(buffer, index, c);
		else c.internal_error("%s is not a constant", index);
	}
}

IrIndex memToConstant(ubyte[] buffer, IrIndex type, CompilationContext* c)
{
	switch(type.basicType(c))
	{
		case IrBasicType.i8:
			c.assertf(1 == buffer.length,
				"Cannot load i8 constant from memory of size %s bytes", buffer.length);
			return c.constants.add(type, *(cast(byte*)buffer.ptr));
		case IrBasicType.i16:
			c.assertf(2 == buffer.length,
				"Cannot load i8 constant from memory of size %s bytes", buffer.length);
			return c.constants.add(type, *(cast(short*)buffer.ptr));
		case IrBasicType.i32:
			c.assertf(4 == buffer.length,
				"Cannot load i8 constant from memory of size %s bytes", buffer.length);
			return c.constants.add(type, *(cast(int*)buffer.ptr));
		case IrBasicType.i64:
			c.assertf(8 == buffer.length,
				"Cannot load i8 constant from memory of size %s bytes", buffer.length);
			return c.constants.add(type, *(cast(long*)buffer.ptr));
		default:
			c.internal_error("memToConstant %s", cast(IrBasicType)type.typeIndex);
	}
}

// vm may be null, in which case only constants can be parsed
T irValueToNative(T)(IrVm* vm, IrIndex value, CompilationContext* c)
{
	static union Repr {
		T native;
		ubyte[T.sizeof] buf;
	}
	Repr repr;

	if (value.isVirtReg)
	{
		c.assertf(vm !is null, "Cannot read vreg without VM");
		repr.buf[] = vm.slotToSlice(vm.vregSlot(value));
		return repr.native;
	}

	void onGlobal(ubyte[] subbuffer, IrIndex index, CompilationContext* c)
	{
		c.assertf(index.isGlobal, "%s is not a global", index);

		IrGlobal* global = c.globals.get(index);
		ObjectSymbol* globalSym = c.objSymTab.getSymbol(global.objectSymIndex);
		if (globalSym.isMutable) c.internal_error("%s is not a constant", index);

		assert(globalSym.dataPtr.sizeof == subbuffer.length);
		subbuffer[] = *cast(ubyte[8]*)&globalSym.dataPtr;
	}
	constantToMem(repr.buf, value, c, &onGlobal);
	return repr.native;
}

string stringFromIrValue(IrVm* vm, IrIndex value, CompilationContext* c) {
	return cast(string)irValueToNative!SliceString(vm, value, c).slice;
}

AstIndex astIndexFromIrValue(IrVm* vm, IrIndex value, CompilationContext* c) {
	return irValueToNative!AstIndex(vm, value, c);
}
