/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.type.basic;

import all;

@(AstType.type_basic)
struct BasicTypeNode {
	mixin AstNodeData!(AstType.type_basic, AstFlags.isType, AstNodeState.type_check_done);
	AstIndex type = CommonAstNodes.type_type;
	SizeAndAlignment sizealign;
	ulong minValue;
	ulong maxValue;
	BasicType basicType;
	ubyte typeFlags;
	TypeNode* typeNode() return { return cast(TypeNode*)&this; }
	string strId() { return basicTypeNames[basicType]; }

	bool isFloat() { return cast(bool)(typeFlags & BasicTypeFlag.isFloat); }
	bool isInteger() { return cast(bool)(typeFlags & BasicTypeFlag.isInteger); }
	IsSigned isSigned() { return cast(IsSigned)cast(bool)(typeFlags & BasicTypeFlag.isSigned); }
	bool isBoolean() { return cast(bool)(typeFlags & BasicTypeFlag.isBoolean); }
	bool isAlias() { return basicType == BasicType.t_alias; }
	bool isType() { return basicType == BasicType.t_type; }

	IrIndex gen_init_value(CompilationContext* c)
	{
		switch(basicType)
		{
			case BasicType.t_error: return c.constants.add(0, IsSigned.no, IrArgSize.size8);
			case BasicType.t_bool: return c.constants.add(0, IsSigned.no, IrArgSize.size8);
			case BasicType.t_u8: return c.constants.add(0, IsSigned.no, IrArgSize.size8);
			case BasicType.t_i8: return c.constants.add(0, IsSigned.no, IrArgSize.size8);
			case BasicType.t_i16: return c.constants.add(0, IsSigned.no, IrArgSize.size16);
			case BasicType.t_u16: return c.constants.add(0, IsSigned.no, IrArgSize.size16);
			case BasicType.t_i32: return c.constants.add(0, IsSigned.no, IrArgSize.size32);
			case BasicType.t_u32: return c.constants.add(0, IsSigned.no, IrArgSize.size32);
			case BasicType.t_i64: return c.constants.add(0, IsSigned.no, IrArgSize.size64);
			case BasicType.t_u64: return c.constants.add(0, IsSigned.no, IrArgSize.size64);
			case BasicType.t_f32: return c.constants.add(0, IsSigned.no, IrArgSize.size32);
			case BasicType.t_f64: return c.constants.add(0, IsSigned.no, IrArgSize.size64);
			case BasicType.t_alias: return c.constants.add(0, IsSigned.no, IrArgSize.size32);
			case BasicType.t_type: return c.constants.add(0, IsSigned.no, IrArgSize.size32);
			default:
				c.internal_error(loc, "Cannot convert %s to IrIndex", basicType);
				assert(false);
		}
	}
}

enum NumBasicTypeNodeSlots = 12;

enum BasicTypeFlag : ubyte {
	isFloat    = 1 << 0,
	isInteger  = 1 << 1,
	isSigned   = 1 << 2,
	isBoolean  = 1 << 3,
}

void print_type_basic(BasicTypeNode* node, ref AstPrintState state)
{
	state.print("TYPE ", node.typeNode.printer(state.context));
}

IrIndex gen_ir_type_basic(BasicTypeNode* t, CompilationContext* context)
	out(res; res.isTypeBasic, "Not a basic type")
{
	switch(t.basicType)
	{
		case BasicType.t_noreturn: return makeBasicTypeIndex(IrValueType.noreturn_t);
		case BasicType.t_void: return makeBasicTypeIndex(IrValueType.void_t);
		case BasicType.t_bool: return makeBasicTypeIndex(IrValueType.i8);
		case BasicType.t_null: return makeBasicTypeIndex(IrValueType.i64);
		case BasicType.t_u8: return makeBasicTypeIndex(IrValueType.i8);
		case BasicType.t_i8: return makeBasicTypeIndex(IrValueType.i8);
		case BasicType.t_i16: return makeBasicTypeIndex(IrValueType.i16);
		case BasicType.t_u16: return makeBasicTypeIndex(IrValueType.i16);
		case BasicType.t_i32: return makeBasicTypeIndex(IrValueType.i32);
		case BasicType.t_u32: return makeBasicTypeIndex(IrValueType.i32);
		case BasicType.t_i64: return makeBasicTypeIndex(IrValueType.i64);
		case BasicType.t_u64: return makeBasicTypeIndex(IrValueType.i64);
		case BasicType.t_f32: return makeBasicTypeIndex(IrValueType.f32);
		case BasicType.t_f64: return makeBasicTypeIndex(IrValueType.f64);
		case BasicType.t_alias: return makeBasicTypeIndex(IrValueType.i32);
		case BasicType.t_type: return makeBasicTypeIndex(IrValueType.i32);
		default:
			context.internal_error(t.loc, "Cannot convert %s to IrIndex", t.basicType);
			assert(false);
	}
}

CommonTypeResult common_type_basic(BasicTypeNode* node, AstIndex typeBIndex, CompilationContext* c)
{
	BasicType basicA = node.basicType;
	TypeNode* typeB = typeBIndex.get_type(c);
	switch(typeB.astType) with(AstType)
	{
		case type_basic:
			BasicType basicB = typeB.as_basic.basicType;
			BasicType commonType = commonBasicType[basicA][basicB];
			TypeConvResKind kindA = basicConversionKind[basicA][commonType];
			TypeConvResKind kindB = basicConversionKind[basicB][commonType];
			return CommonTypeResult(c.basicTypeNodes(commonType), kindA, kindB);
		case type_ptr:
			if (basicA == BasicType.t_null) {
				return CommonTypeResult(typeBIndex, TypeConvResKind.override_expr_type_i, TypeConvResKind.no_i);
			}
			return CommonTypeResult(CommonAstNodes.type_error);
		default: assert(false);
	}
}

TypeConvResKind type_conv_basic(BasicTypeNode* node, AstIndex typeBIndex, ref AstIndex expr, CompilationContext* c)
{
	BasicType fromTypeBasic = node.basicType;
	TypeNode* typeB = typeBIndex.get_type(c);

	switch(typeB.astType) with(AstType)
	{
		case type_basic:
			auto res = basicConversionKind[fromTypeBasic][typeB.as_basic.basicType];
			auto exprNode = expr.get_node(c);
			switch(exprNode.astType) {
				case AstType.literal_int:
					if (res.canConvertImplicitly) return TypeConvResKind.override_expr_type_i;
					if (typeB.isInteger)
					{
						ubyte toSize = integerSize(typeB.as_basic.basicType);
						auto lit = exprNode.as!IntLiteralExprNode(c);
						if (lit.isSigned) {
							if (numSignedBytesForInt(lit.value) <= toSize) return TypeConvResKind.override_expr_type_i;
						} else {
							if (numUnsignedBytesForInt(lit.value) <= toSize) return TypeConvResKind.override_expr_type_i;
						}
					}
					return res;
				case AstType.literal_float:
					if (res.canConvertImplicitly) return TypeConvResKind.override_expr_type_i;
					if (typeB.isFloat) return TypeConvResKind.override_expr_type_i;
					return res;
				default: return res;
			}
		case type_ptr:
			if (fromTypeBasic == BasicType.t_null) return TypeConvResKind.override_expr_type_i;
			if (fromTypeBasic.isInteger) return TypeConvResKind.ii_e;
			return TypeConvResKind.fail;
		case type_slice:
			if (fromTypeBasic == BasicType.t_null) return TypeConvResKind.override_expr_type_i;
			return TypeConvResKind.fail;
		case decl_enum:
			auto res1 = type_conv_basic(node, typeB.as_enum.memberType, expr, c);
			auto res2 = res1.allowExplicitOnly;
			//writefln("type_conv_basic %s %s %s %s", fromTypeBasic, printer(typeB.as_enum.memberType, c), res1, res2);
			return res2;
		default: return TypeConvResKind.fail;
	}
}
