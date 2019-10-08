/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// IR Index. Points to any entity in function's IR
module ir.ir_index;

import std.format : formattedWrite;
import std.bitmanip : bitfields;

import all;

/// Represent index of any IR entity inside function's ir array
@(IrValueKind.none)
struct IrIndex
{
	///
	this(uint _storageUintIndex, IrValueKind _kind)
	{
		storageUintIndex = _storageUintIndex;
		kind = _kind;
	}

	/// Constructor for physicalRegister
	this(uint index, uint regSize, uint regClass)
	{
		physRegIndex = index;
		physRegSize = regSize;
		physRegClass = regClass;
		kind = IrValueKind.physicalRegister;
	}

	static IrIndex fromUint(uint data)
	{
		IrIndex res;
		res.asUint = data;
		return res;
	}

	union
	{
		mixin(bitfields!(
			uint,        "storageUintIndex", 28, // may be 0 for defined index
			IrValueKind, "kind",              4  // is never 0 for defined index
		));

		// used when kind == IrValueKind.constant
		mixin(bitfields!(
			// Big constants use constantIndex as index into IrConstantStorage
			// Small constants store data directly in constantIndex.
			uint,            "constantIndex", 24,
			IrArgSize,       "constantSize",   2,
			// kind of constant
			IrConstantKind,  "constantKind",   2,
			IrValueKind,     "",               4  // index kind
		));

		// used when kind == IrValueKind.type
		// types are stored in 8-byte chunked buffer
		mixin(bitfields!(
			// if typeKind is basic, then typeIndex contains IrValueType
			uint,        "typeIndex",        24, // may be 0 for defined index
			IrTypeKind,  "typeKind",          4, // type kind
			IrValueKind, "",                  4  // index kind
		));

		// used when kind == IrValueKind.physicalRegister
		mixin(bitfields!(
			// machine-specific index
			uint,        "physRegIndex",     12,
			// physical register size
			// Not in bytes, but a machine-specific enum value
			uint,        "physRegSize",       8,
			// physical register class
			uint,        "physRegClass",      8,
			IrValueKind, "",                  4  // index `kind`
		));

		// is 0 for undefined index
		uint asUint;
	}
	static assert(IrValueKind.max <= 0b1111, "4 bits are reserved");
	bool isDefined() { return asUint != 0; }
	bool isUndefined() { return asUint == 0; }

	void toString(scope void delegate(const(char)[]) sink) const {
		if (asUint == 0) {
			sink("<null>");
			return;
		}

		switch(kind) with(IrValueKind) {
			default: sink.formattedWrite("0x%X", asUint); break;
			case listItem: sink.formattedWrite("l.%s", storageUintIndex); break;
			case instruction: sink.formattedWrite("i.%s", storageUintIndex); break;
			case basicBlock: sink.formattedWrite("@%s", storageUintIndex); break;
			case constant:
				final switch(constantKind) with(IrConstantKind) {
					case intUnsignedSmall: sink.formattedWrite("%s", constantIndex); break;
					case intSignedSmall: sink.formattedWrite("%s", (cast(int)constantIndex << 8) >> 8); break;
					case intUnsignedBig: sink.formattedWrite("cu.%s", constantIndex); break;
					case intSignedBig: sink.formattedWrite("cs.%s", constantIndex); break;
				}
				break;

			case constantAggregate: sink.formattedWrite("caggr.%s", storageUintIndex); break;
			case global: sink.formattedWrite("g%s", storageUintIndex); break;
			case phi: sink.formattedWrite("phi%s", storageUintIndex); break;
			case stackSlot: sink.formattedWrite("s%s", storageUintIndex); break;
			case virtualRegister: sink.formattedWrite("v%s", storageUintIndex); break;
			case physicalRegister: sink.formattedWrite("r<c:%s i:%s s:%s>", physRegClass, physRegIndex, physRegSize); break;
			case type: sink.formattedWrite("type.%s.%s", typeKind, typeIndex); break;
			case variable: sink.formattedWrite("var%s", storageUintIndex); break;
			case func: sink.formattedWrite("f%s", storageUintIndex); break;
		}
	}

	/// When this index represents index of 0's array item, produces
	/// index of this array items. Calling with 0 returns itself.
	IrIndex indexOf(T)(size_t offset)
	{
		static assert(T.alignof == 4, "Can only point to types aligned to 4 bytes");
		IrIndex result = this;
		result.storageUintIndex = cast(uint)(storageUintIndex + divCeil(T.sizeof, uint.sizeof) * offset);
		return result;
	}

	const:

	bool isInstruction() { return kind == IrValueKind.instruction; }
	bool isBasicBlock() { return kind == IrValueKind.basicBlock; }
	bool isPhi() { return kind == IrValueKind.phi; }
	bool isConstant() { return kind == IrValueKind.constant; }
	bool isConstantAggregate() { return kind == IrValueKind.constantAggregate; }
	bool isGlobal() { return kind == IrValueKind.global; }
	bool isVirtReg() { return kind == IrValueKind.virtualRegister; }
	bool isPhysReg() { return kind == IrValueKind.physicalRegister; }
	bool isSomeReg() {
		return kind == IrValueKind.virtualRegister ||
			kind == IrValueKind.physicalRegister;
	}
	bool isStackSlot() { return kind == IrValueKind.stackSlot; }
	bool isType() { return kind == IrValueKind.type; }
	bool isVariable() { return kind == IrValueKind.variable; }
	bool isFunction() { return kind == IrValueKind.func; }

	bool isTypeBasic() { return kind == IrValueKind.type && typeKind == IrTypeKind.basic; }
	bool isTypePointer() { return kind == IrValueKind.type && typeKind == IrTypeKind.pointer; }
	bool isTypeArray() { return kind == IrValueKind.type && typeKind == IrTypeKind.array; }
	bool isTypeStruct() { return kind == IrValueKind.type && typeKind == IrTypeKind.struct_t; }
	bool isTypeFunction() { return kind == IrValueKind.type && typeKind == IrTypeKind.func_t; }
	bool isTypeVoid()
	{
		return kind == IrValueKind.type && typeKind == IrTypeKind.basic && typeIndex == IrValueType.void_t;
	}

	bool isSignedConstant() {
		return kind == IrValueKind.constant &&
			(constantKind == IrConstantKind.intSignedSmall ||
			constantKind == IrConstantKind.intSignedBig);
	}
}

// compares physical registers size agnostically
// if not physical register compares as usual
bool sameIndexOrPhysReg(IrIndex a, IrIndex b) pure @nogc
{
	if (a.asUint == b.asUint) return true;
	if (a.kind == IrValueKind.physicalRegister)
	{
		a.physRegSize = 0;
		b.physRegSize = 0;
		return a.asUint == b.asUint;
	}
	return false;
}
