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
	this(uint _storageUintIndex, IrValueKind _kind)
	{
		storageUintIndex = _storageUintIndex;
		kind = _kind;
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
		// types are stored in 8-byte chunked buffer
		mixin(bitfields!(
			// if typeKind is basic, then typeIndex contains IrValueType
			uint,        "typeIndex",        24, // may be 0 for defined index
			IrTypeKind,  "typeKind",          4, // type kind
			IrValueKind, "",                  4  // index kind
		));
		uint asUint; // is 0 for undefined index
	}
	static assert(IrValueKind.max <= 0b1111, "4 bits are reserved");
	bool isDefined() { return asUint != 0; }

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
			case constant: sink.formattedWrite("c.%s", storageUintIndex); break;
			case phi: sink.formattedWrite("phi.%s", storageUintIndex); break;
			case memoryAddress: sink.formattedWrite("m.%s", storageUintIndex); break;
			case stackSlot: sink.formattedWrite("s.%s", storageUintIndex); break;
			case virtualRegister: sink.formattedWrite("v.%s", storageUintIndex); break;
			case physicalRegister: sink.formattedWrite("p.%s", storageUintIndex); break;
			case type: sink.formattedWrite("type.%s", storageUintIndex); break;
			case variable: sink.formattedWrite("var.%s", storageUintIndex); break;
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

	bool isTypeBasic() { return kind == IrValueKind.type && typeKind == IrTypeKind.basic; }
	bool isTypePointer() { return kind == IrValueKind.type && typeKind == IrTypeKind.pointer; }
	bool isTypeArray() { return kind == IrValueKind.type && typeKind == IrTypeKind.array; }
	bool isTypeStruct() { return kind == IrValueKind.type && typeKind == IrTypeKind.struct_t; }
}
