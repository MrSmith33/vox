/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.type_conv;

import all;


@(AstType.expr_type_conv)
struct TypeConvExprNode {
	mixin ExpressionNodeData!(AstType.expr_type_conv);
	AstIndex expr;
	TypeConvResKind convKind() { return cast(TypeConvResKind)subType; }
}

void print_type_conv(TypeConvExprNode* node, ref AstPrintState state)
{
	state.print("CAST to ", node.type.printer(state.context));
	print_ast(node.expr, state);
}

void post_clone_type_conv(TypeConvExprNode* node, ref CloneState state)
{
	state.fixAstIndex(node.type);
	state.fixAstIndex(node.expr);
}

void name_register_nested_type_conv(TypeConvExprNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.expr, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_type_conv(TypeConvExprNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.type, state);
	require_name_resolve(node.expr, state);
	node.state = AstNodeState.name_resolve_done;
}

// Checks cast() parsed from code
// Casts inserted by the compiler skip this step
void type_check_type_conv(TypeConvExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;
	node.state = AstNodeState.type_check;
	require_type_check(node.expr, state);
	TypeConvResKind kind = checkTypeConversion(node.expr.get_expr_type(c), node.type, node.expr, c);
	if (!kind.successful)
	{
		c.error(node.loc,
			"Cannot convert expression of type `%s` to `%s`",
			node.expr.get_expr_type(c).printer(c),
			node.type.printer(c));
	}
	node.subType = kind;
	// TODO: handling is different for user inserted casts and for implicit casts.
	// because implicit casts can be omitted/replaced with other nodes.
	node.state = AstNodeState.type_check_done;
}

ExprValue ir_gen_expr_type_conv(ref IrGenState gen, IrIndex currentBlock, ref IrLabel nextStmt, TypeConvExprNode* t)
{
	CompilationContext* c = gen.context;

	if (t.type == CommonAstNodes.type_alias || t.type == CommonAstNodes.type_type) {
		AstIndex aliasedNode = get_effective_node(t.expr, c);
		if (t.type == CommonAstNodes.type_type)
			if (!aliasedNode.isType(c))
				c.error(t.loc,
					"Cannot convert expression of type `%s` to `%s`",
					t.expr.get_expr_type(c).printer(c),
					t.type.printer(c));
		IrIndex result = c.constants.add(makeIrType(IrBasicType.i32), aliasedNode.storageIndex);
		gen.builder.addJumpToLabel(currentBlock, nextStmt);
		return ExprValue(result);
	}

	IrLabel afterExpr = IrLabel(currentBlock);
	ExprValue lval = ir_gen_expr(gen, t.expr, currentBlock, afterExpr);
	currentBlock = afterExpr.blockIndex;
	IrIndex rval = lval.rvalue(gen, t.loc, currentBlock);

	ExpressionNode* childExpr = t.expr.get_expr(c);
	TypeNode* sourceType = childExpr.type.get_type(c);
	TypeNode* targetType = t.type.get_type(c);

	// fold the alias in the form of enum type
	if (sourceType.astType == AstType.decl_enum) {
		sourceType = sourceType.as_enum.memberType.get_type(c);
	}
	if (targetType.astType == AstType.decl_enum) {
		targetType = targetType.as_enum.memberType.get_type(c);
	}

	IrIndex typeFrom = sourceType.gen_ir_type(c);
	IrIndex typeTo = targetType.gen_ir_type(c);
	uint typeSizeFrom = c.types.typeSize(typeFrom);
	uint typeSizeTo = c.types.typeSize(typeTo);
	//writefln("cast %s %s -> %s", t.convKind, IrTypeDump(typeFrom, *c), IrTypeDump(typeTo, *c));

	IrIndex result;

	if (rval.isSimpleConstant) {
		//writefln("%s", FmtSrcLoc(t.loc, c));
		result = eval_type_conv(t, rval, c);
	}
	else final switch (t.convKind) with(TypeConvResKind) {
		case fail: c.internal_error(t.loc, "IR gen of failed cast");
		case no_e, no_i: result = rval; break;
		case ii_e, ii_i, override_expr_type_e, override_expr_type_i:
			if (targetType.isBool) {
				ExtraInstrArgs extra = { type : typeTo, cond : IrUnaryCondition.not_zero };
				result = gen.builder.emitInstr!(IrOpcode.set_unary_cond)(currentBlock, extra, rval).result;
			} else {
				ExtraInstrArgs extra = { type : typeTo, argSize : sizeToIrArgSize(typeSizeTo, c) };
				if (typeSizeFrom == typeSizeTo) {
					// bitcast
					// needed in case of ptr conversion. Later we may need to look into pointed type
					result = gen.builder.emitInstr!(IrOpcode.conv)(currentBlock, extra, rval).result;
				} else if (typeSizeTo < typeSizeFrom) {
					// trunc
					result = gen.builder.emitInstr!(IrOpcode.trunc)(currentBlock, extra, rval).result;
				} else {
					// sext/zext
					if (sourceType.isSigned && targetType.isSigned)
						result = gen.builder.emitInstr!(IrOpcode.sext)(currentBlock, extra, rval).result;
					else
						result = gen.builder.emitInstr!(IrOpcode.zext)(currentBlock, extra, rval).result;
				}
			}
			break;
		case if_e, if_i:
			ExtraInstrArgs extra = { type : typeTo, argSize : sizeToIrArgSize(typeSizeTo, c) };
			assert(sourceType.as_basic);
			if (sourceType.as_basic.isSigned)
				result = gen.builder.emitInstr!(IrOpcode.sitofp)(currentBlock, extra, rval).result;
			else
				result = gen.builder.emitInstr!(IrOpcode.uitofp)(currentBlock, extra, rval).result;
			break;

		case ff_e, ff_i:
			ExtraInstrArgs extra = { type : typeTo, argSize : sizeToIrArgSize(typeSizeTo, c) };
			if (typeSizeFrom == typeSizeTo)
				result = rval;
			else if (typeSizeTo < typeSizeFrom)
				result = gen.builder.emitInstr!(IrOpcode.fptrunc)(currentBlock, extra, rval).result;
			else
				result = gen.builder.emitInstr!(IrOpcode.fpext)(currentBlock, extra, rval).result;
			break;

		case fi_e, fi_i:
			ExtraInstrArgs extra = { type : typeTo, argSize : sizeToIrArgSize(typeSizeTo, c) };
			assert(sourceType.as_basic);
			if (targetType.as_basic.isSigned)
				result = gen.builder.emitInstr!(IrOpcode.fptosi)(currentBlock, extra, rval).result;
			else
				result = gen.builder.emitInstr!(IrOpcode.fptoui)(currentBlock, extra, rval).result;
			break;

		case string_literal_to_u8_ptr:
			result = c.constants.getAggregateMember(rval, c.constants.add(makeIrType(IrBasicType.i32), 1), c);
			break;
		case array_literal_to_slice:
			assert(false, to!string(t.convKind));
	}
	gen.builder.addJumpToLabel(currentBlock, nextStmt);
	return ExprValue(result);
}

IrIndex eval_type_conv(TypeConvExprNode* node, IrIndex rval, CompilationContext* c)
{
	c.assertf(rval.isSomeConstant, node.loc, "%s must be a constant", rval);

	TypeNode* sourceType = node.expr.get_expr(c).type.get_type(c);
	if (sourceType.astType == AstType.decl_enum) {
		sourceType = sourceType.as_enum.memberType.get_type(c);
	}
	TypeNode* targetType = node.type.get_type(c);
	if (targetType.astType == AstType.decl_enum) {
		targetType = targetType.as_enum.memberType.get_type(c);
	}
	IrIndex typeFrom = sourceType.gen_ir_type(c);
	IrIndex typeTo = targetType.gen_ir_type(c);
	uint typeSizeTo = c.types.typeSize(typeTo);

	final switch (node.convKind) with(TypeConvResKind) {
		case fail: c.internal_error(node.loc, "eval of failed cast");
		case no_e, no_i: return rval;
		case ii_e, ii_i, override_expr_type_e, override_expr_type_i:
			if (rval.isConstantZero) {
				return typeTo.zeroConstantOfType;
			}
			auto con = c.constants.get(rval);
			if (typeTo.isTypeBasic) {
				IrBasicType basicType = typeTo.basicType(c);
				switch(basicType) with(IrBasicType) {
					case i8:  return c.constants.add(typeTo, con.i64 & 0xFF);
					case i16: return c.constants.add(typeTo, con.i64 & 0xFFFF);
					case i32: return c.constants.add(typeTo, con.i64 & 0xFFFF_FFFF);
					case i64: return c.constants.add(typeTo, con.i64);
					default: c.internal_error("Invalid constant type %s", basicType);
				}
			}
			return c.constants.add(typeTo, con.i64); // ptr

		case if_e, if_i:
			c.assertf(typeFrom.isTypeInteger, "from %s", typeFrom);
			c.assertf(typeTo.isTypeFloat, "from %s", typeTo);
			auto con = c.constants.get(rval);
			if (sourceType.as_basic.isSigned) {
				IrBasicType basicTypeFrom = typeFrom.basicType(c);
				long val;
				switch(basicTypeFrom) with(IrBasicType) {
					case i8:  val = con.i8;  break;
					case i16: val = con.i16; break;
					case i32: val = con.i32; break;
					case i64: val = con.i64; break;
					default: c.internal_error("Invalid constant type %s", basicTypeFrom);
				}
				IrBasicType basicTypeTo = typeTo.basicType(c);
				switch(basicTypeTo) {
					case IrBasicType.f32: return c.constants.add(cast(float)val);
					case IrBasicType.f64: return c.constants.add(cast(double)val);
					default: c.internal_error("Invalid target type %s", basicTypeTo);
				}
			} else {
				IrBasicType basicTypeFrom = typeFrom.basicType(c);
				ulong val;
				switch(basicTypeFrom) with(IrBasicType) {
					case i8:  val = con.u8;  break;
					case i16: val = con.u16; break;
					case i32: val = con.u32; break;
					case i64: val = con.u64; break;
					default: c.internal_error("Invalid constant type %s", basicTypeFrom);
				}
				IrBasicType basicTypeTo = typeTo.basicType(c);
				switch(basicTypeTo) {
					case IrBasicType.f32: return c.constants.add(cast(float)val);
					case IrBasicType.f64: return c.constants.add(cast(double)val);
					default: c.internal_error("Invalid target type %s", basicTypeTo);
				}
			}

		case ff_e, ff_i:
			c.assertf(typeFrom.isTypeFloat, "from %s", typeFrom);
			c.assertf(typeTo.isTypeFloat, "from %s", typeTo);
			switch(typeFrom.basicType(c)) {
				case IrBasicType.f32:
					c.assertf(typeTo.basicType(c) == IrBasicType.f64, "from f32 to %s", typeTo);
					double f64 = cast(double)c.constants.get(rval).f32;
					return c.constants.add(f64);
				case IrBasicType.f64:
					c.assertf(typeTo.basicType(c) == IrBasicType.f32, "from f64 to %s", typeTo);
					float f32 = cast(float)c.constants.get(rval).f64;
					return c.constants.add(f32);
				default: c.unreachable;
			}

		case fi_e, fi_i:
			c.assertf(typeFrom.isTypeFloat, "from %s", typeFrom);
			c.assertf(typeTo.isTypeInteger, "from %s", typeTo);
			auto con = c.constants.get(rval);
			IrBasicType basicTypeFrom = typeFrom.basicType(c);
			double val;
			switch(basicTypeFrom) {
				case IrBasicType.f32: val = con.f32; break;
				case IrBasicType.f64: val = con.f64; break;
				default: c.internal_error("Invalid target type %s", basicTypeFrom);
			}
			if (targetType.as_basic.isSigned) {
				IrBasicType basicTypeTo = typeTo.basicType(c);
				switch(basicTypeTo) with(IrBasicType) {
					case i8:  return c.constants.add(typeTo, cast(byte)val);
					case i16: return c.constants.add(typeTo, cast(short)val);
					case i32: return c.constants.add(typeTo, cast(int)val);
					case i64: return c.constants.add(typeTo, cast(long)val);
					default: c.internal_error("Invalid constant type %s", basicTypeTo);
				}
			} else {
				IrBasicType basicTypeTo = typeTo.basicType(c);
				switch(basicTypeTo) with(IrBasicType) {
					case i8:  return c.constants.add(typeTo, cast(ubyte)val);
					case i16: return c.constants.add(typeTo, cast(ushort)val);
					case i32: return c.constants.add(typeTo, cast(uint)val);
					case i64: return c.constants.add(typeTo, cast(ulong)val);
					default: c.internal_error("Invalid constant type %s", basicTypeTo);
				}
			}

		case string_literal_to_u8_ptr:
			IrIndex ONE = c.constants.add(makeIrType(IrBasicType.i32), 1);
			return c.constants.getAggregateMember(rval, ONE, c);

		case array_literal_to_slice:
			c.internal_error("TODO %s", node.convKind);
	}
}

void ir_gen_branch_type_conv(ref IrGenState gen, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit, TypeConvExprNode* t)
{
	CompilationContext* c = gen.context;
	IrLabel afterExpr = IrLabel(currentBlock);
	ExprValue lval = ir_gen_expr(gen, t.expr, currentBlock, afterExpr);
	currentBlock = afterExpr.blockIndex;
	IrIndex rval = lval.rvalue(gen, t.loc, currentBlock);

	ExpressionNode* childExpr = t.expr.get_expr(c);
	IrIndex from = childExpr.type.gen_ir_type(c);

	if (rval.isSimpleConstant ||
		from == makeIrType(IrBasicType.i8) ||
		from == makeIrType(IrBasicType.i16) ||
		from == makeIrType(IrBasicType.i32) ||
		from == makeIrType(IrBasicType.i64))
	{
		addUnaryBranch(gen, rval, currentBlock, trueExit, falseExit);
		return;
	}
	else
	{
		//result = builder.emitInstr1(IrOpcode.o_conv, to, rval);
		c.internal_error(t.loc, "%s to %s", childExpr.type.printer(c), t.type.printer(c));
	}
}
