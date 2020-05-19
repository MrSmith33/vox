/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
///
module ir.ir_value_kind;

/// Describes what IrIndex is pointing at
/// Is used as UDA on IR entities
enum IrValueKind : ubyte
{
	none, /// Used for undefined indicies
	instruction,
	basicBlock,
	constant,
	constantAggregate, /// Cannot appear in LIR
	constantZero, /// Zero inits value of any type. Type is embedded (same layout as IrValueKind.type)
	global, /// Index of global var or const (or literal data)
	phi,
	stackSlot,
	virtualRegister,
	physicalRegister,
	type,
	variable, /// Represents variable index while converting into SSA-form
	// temporarily IrIndex.storageUintIndex contains AstIndex of FunctionDeclNode
	func,
	array, /// Indicates reference to big array in IrSmallArray
}
