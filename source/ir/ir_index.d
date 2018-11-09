/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module ir.ir_index;

import std.format : formattedWrite;
import std.bitmanip : bitfields;

import all;

/// Represent index of any IR entity inside function's ir array
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
		uint asUint; // is 0 for undefined index
	}
	static assert(IrValueKind.max <= 0b1111, "4 bits are reserved");
	bool isDefined() { return asUint != 0; }

	void toString(scope void delegate(const(char)[]) sink) const {
		final switch(kind) with(IrValueKind) {
			case none: sink("<null>"); break;
			case listItem: sink.formattedWrite("l.%s", storageUintIndex); break;
			case instruction: sink.formattedWrite("i.%s", storageUintIndex); break;
			case basicBlock: sink.formattedWrite("@%s", storageUintIndex); break;
			case constant: sink.formattedWrite("c.%s", storageUintIndex); break;
			case phi: sink.formattedWrite("phi.%s", storageUintIndex); break;
			case memoryAddress: sink.formattedWrite("m.%s", storageUintIndex); break;
			case stackSlot: sink.formattedWrite("s.%s", storageUintIndex); break;
			case virtualRegister: sink.formattedWrite("v.%s", storageUintIndex); break;
			case physicalRegister: sink.formattedWrite("p.%s", storageUintIndex); break;
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

	bool isConstant() { return kind == IrValueKind.constant; }
	bool isVirtReg() { return kind == IrValueKind.virtualRegister; }
	bool isPhysReg() { return kind == IrValueKind.physicalRegister; }
	bool isSomeReg() {
		return kind == IrValueKind.virtualRegister ||
			kind == IrValueKind.physicalRegister;
	}
}
