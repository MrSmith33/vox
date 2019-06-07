/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
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
public import ir.ir_constant;
public import ir.ir_function;
public import ir.ir_global;
public import ir.ir_index;
public import ir.ir_instructions;
public import ir.ir_module;
public import ir.ir_phi;
public import ir.ir_type;
public import ir.ir_value_kind;
public import ir.ir_variable;
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
	/// If isAllocated
	///   blockIndex points to new block
	/// else
	///   If numPredecessors == 0, blockIndex points to currentBlock at
	//      scope start
	///   If numPredecessors == 1, blockIndex points to first predecessor
	/// If numPredecessors > 1, blockIndex points to a new block and isAllocated must be true
	IrIndex blockIndex;
	///
	bool isAllocated;
	///
	uint numPredecessors;
}

struct BlockVarPair
{
	IrIndex blockId;
	IrIndex var;
}
