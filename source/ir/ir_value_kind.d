/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
///
module ir.ir_value_kind;

/// Describes what IrIndex is pointing at
/// Is used as UDA on instructions
enum IrValueKind : ubyte
{
	none, /// Used for undefined indicies
	listItem, /// Indicates items of linked list in SmallVector
	instruction,
	basicBlock,
	constant,
	global, /// Index of global var or const (or literal data)
	phi,
	memoryAddress,
	stackSlot,
	virtualRegister,
	physicalRegister,
	type,
	variable /// Represents variable index while converting into SSA-form
}
