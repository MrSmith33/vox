/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// IR Types
module ir.ir_type;

import all;

enum IrValueType : ubyte
{
	void_t,
	i8,
	i16,
	i32,
	i64,
	//f32,
	//f64,
}

enum IrTypeKind : ubyte
{
	basic,
	pointer,
	array,
	struct_t
}

@(IrValueKind.type)
align(8)
struct IrTypePointer
{
	IrIndex baseType;
}

@(IrValueKind.type)
align(8)
struct IrTypeArray
{
	IrIndex elemType;
	uint size;
}

@(IrValueKind.type)
align(8)
struct IrTypeStruct
{
	uint size;
	uint numMembers;
	IrTypeStructMember[0] members_payload;
	/// This must be called on the value in the buffer, not stack-local value
	IrTypeStructMember[] members() { return members_payload[0..numMembers];}
}

align(8)
struct IrTypeStructMember
{
	IrIndex type;
	uint offset;
}
