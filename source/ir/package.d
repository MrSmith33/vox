/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// IR that implements idea of storing everything (almost)
/// inside a single array;
/// Stores instructions in a linked list
module ir;

public import ir.dump;
public import ir.ir_basic_block;
public import ir.ir_builder;
public import ir.ir_function;
public import ir.ir_index;
public import ir.ir_instructions;
public import ir.ir_module;
public import ir.ir_phi;
public import ir.ir_value_kind;
public import ir.ir_virt_reg;
public import ir.small_vector;

import all;


/// Convenience struct for Id + num suffix
struct IrName
{
	Identifier id;
	uint suffix;
}

struct IrLabel
{
	/// If numPredecessors == 0, is null
	/// If numPredecessors == 1, points to first predecessor
	/// If numPredecessors > 1,  points to a new block
	IrIndex blockIndex;
	///
	uint numPredecessors;
}

/// Stores numeric constant data
@(IrValueKind.constant)
struct IrConstant
{
	this(long value) {
		this.i64 = value;
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
}


struct IrVarId { uint id; alias id this; }
struct IrVar { Identifier name; IrVarId id; IrValueType type; }

enum IrValueType : ubyte
{
	void_t,
	i32,
	i64,
	//f32,
	//f64,

	ptr,
}

struct BlockVarPair
{
	IrIndex blockId;
	IrVarId varId;
}
