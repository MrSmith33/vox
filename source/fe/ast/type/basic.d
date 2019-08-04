/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.type.basic;

import all;

struct BasicTypeNode {
	mixin AstNodeData!(AstType.type_basic, AstFlags.isType, AstNodeState.type_check_done);
	uint size;
	uint alignment() { return size; }
	ulong minValue;
	ulong maxValue;
	BasicType basicType;
	ubyte typeFlags;
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	string strId() { return basicTypeNames[basicType]; }

	bool isFloat() { return cast(bool)(typeFlags & BasicTypeFlag.isFloat); }
	bool isInteger() { return cast(bool)(typeFlags & BasicTypeFlag.isInteger); }
	bool isUnsigned() { return cast(bool)(typeFlags & BasicTypeFlag.isUnsigned); }
	IsSigned isSigned() { if (isUnsigned) return IsSigned.no; else return IsSigned.yes; }
	bool isBoolean() { return cast(bool)(typeFlags & BasicTypeFlag.isBoolean); }
}

BasicTypeNode basicTypeNode(uint size, ulong minValue, ulong maxValue, BasicType basicType, int typeFlags = 0)
{
	return BasicTypeNode(TokenIndex(), size, minValue, maxValue, basicType, cast(ubyte)typeFlags);
}

enum BasicTypeFlag : ubyte {
	isFloat    = 1 << 0,
	isInteger  = 1 << 1,
	isUnsigned = 1 << 2,
	isBoolean  = 1 << 3,
}

IrIndex gen_ir_type_basic(BasicTypeNode* t, CompilationContext* context)
	out(res; res.isTypeBasic, "Not a basic type")
{
	switch(t.basicType)
	{
		case BasicType.t_void: return makeBasicTypeIndex(IrValueType.void_t);
		case BasicType.t_bool: return makeBasicTypeIndex(IrValueType.i8);
		case BasicType.t_u8: return makeBasicTypeIndex(IrValueType.i8);
		case BasicType.t_i8: return makeBasicTypeIndex(IrValueType.i8);
		case BasicType.t_i16: return makeBasicTypeIndex(IrValueType.i16);
		case BasicType.t_u16: return makeBasicTypeIndex(IrValueType.i16);
		case BasicType.t_i32: return makeBasicTypeIndex(IrValueType.i32);
		case BasicType.t_u32: return makeBasicTypeIndex(IrValueType.i32);
		case BasicType.t_i64: return makeBasicTypeIndex(IrValueType.i64);
		case BasicType.t_u64: return makeBasicTypeIndex(IrValueType.i64);
		default:
			context.internal_error(t.loc, "Cannot convert %s to IrIndex", t.basicType);
			assert(false);
	}
}
