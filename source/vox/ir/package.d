/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// IR is stored in a set of arenas. One per item type.
/// Stores instructions in a linked list
module vox.ir;

public import vox.ir.ir_dump;
public import vox.ir.ir_basic_block;
public import vox.ir.ir_builder;
public import vox.ir.ir_constant;
public import vox.ir.ir_function;
public import vox.ir.ir_global;
public import vox.ir.ir_index;
public import vox.ir.ir_inline;
public import vox.ir.ir_instructions;
public import vox.ir.ir_mirror;
public import vox.ir.ir_module;
public import vox.ir.ir_phi;
public import vox.ir.ir_small_array;
public import vox.ir.ir_small_set;
public import vox.ir.ir_stack_slot;
public import vox.ir.ir_type;
public import vox.ir.ir_validation;
public import vox.ir.ir_value_kind;
public import vox.ir.ir_variable;
public import vox.ir.ir_virt_reg;
public import vox.ir.ir_vm;
