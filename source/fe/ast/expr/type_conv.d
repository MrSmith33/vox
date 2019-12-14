/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.type_conv;

import all;


@(AstType.expr_type_conv)
struct TypeConvExprNode {
	mixin ExpressionNodeData!(AstType.expr_type_conv);
	AstIndex expr;
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

void type_check_type_conv(TypeConvExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	require_type_check(node.expr, state);
	if (!isConvertibleTo(node.expr.expr_type(c), node.type, c))
	{
		c.error(node.loc,
			"Cannot auto-convert expression of type `%s` to `%s`",
			node.expr.expr_type(c).printer(c),
			node.type.printer(c));
	}
	node.state = AstNodeState.type_check_done;
}

ExprValue ir_gen_expr_type_conv(ref IrGenState gen, IrIndex currentBlock, ref IrLabel nextStmt, TypeConvExprNode* t)
{
	CompilationContext* c = gen.context;
	IrLabel afterExpr = IrLabel(currentBlock);
	ExprValue lval = ir_gen_expr(gen, t.expr, currentBlock, afterExpr);
	currentBlock = afterExpr.blockIndex;
	IrIndex rval = getRvalue(gen, t.loc, currentBlock, lval);

	ExpressionNode* childExpr = t.expr.get_expr(c);
	TypeNode* sourceType = childExpr.type.get_type(c);
	TypeNode* targetType = t.type.get_type(c);
	c.assertf(sourceType.as_basic || sourceType.as_ptr, t.loc, "Source must have basic/ptr type, not %s", sourceType.printer(c));
	c.assertf(targetType.as_basic || targetType.as_ptr, t.loc, "Target must have basic/ptr type, not %s", targetType.printer(c));
	IrIndex typeTo = t.type.gen_ir_type(c);
	IrIndex typeFrom = childExpr.type.gen_ir_type(c);
	uint typeSizeFrom = c.types.typeSize(typeFrom);
	uint typeSizeTo = c.types.typeSize(typeTo);

	IrIndex result;
	if (rval.isConstant)
	{
		result = rval;
	}
	else if (targetType.isBool)
	{
		ExtraInstrArgs extra = { type : typeTo, cond : IrUnaryCondition.not_zero };
		result = gen.builder.emitInstr!(IrOpcode.set_unary_cond)(currentBlock, extra, rval).result;
	}
	else
	{
		// bitcast
		if (typeSizeFrom == typeSizeTo)
		{
			ExtraInstrArgs extra = { type : typeTo };
			result = gen.builder.emitInstr!(IrOpcode.conv)(currentBlock, extra, rval).result;
		}
		// trunc
		else if (typeSizeTo < typeSizeFrom)
		{
			ExtraInstrArgs extra = { type : typeTo, argSize : targetType.argSize(c) };
			result = gen.builder.emitInstr!(IrOpcode.trunc)(currentBlock, extra, rval).result;
		}
		// sext/zext
		else
		{
			c.assertf(sourceType.as_basic !is null, t.loc, "Source must have basic type, not %s", sourceType.printer(c));
			c.assertf(targetType.as_basic !is null, t.loc, "Target must have basic type, not %s", targetType.printer(c));
			if (sourceType.as_basic.isSigned && targetType.as_basic.isSigned)
			{
				ExtraInstrArgs extra = { type : typeTo, argSize : targetType.argSize(c) };
				result = gen.builder.emitInstr!(IrOpcode.sext)(currentBlock, extra, rval).result;
			}
			else
			{
				ExtraInstrArgs extra = { type : typeTo, argSize : targetType.argSize(c) };
				result = gen.builder.emitInstr!(IrOpcode.zext)(currentBlock, extra, rval).result;
			}
		}
	}
	gen.builder.addJumpToLabel(currentBlock, nextStmt);
	return ExprValue(result);
}

void ir_gen_branch_type_conv(ref IrGenState gen, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit, TypeConvExprNode* t)
{
	CompilationContext* c = gen.context;
	IrLabel afterExpr = IrLabel(currentBlock);
	ExprValue lval = ir_gen_expr(gen, t.expr, currentBlock, afterExpr);
	currentBlock = afterExpr.blockIndex;
	IrIndex rval = getRvalue(gen, t.loc, currentBlock, lval);

	ExpressionNode* childExpr = t.expr.get_expr(c);
	IrIndex from = childExpr.type.gen_ir_type(c);

	if (rval.isConstant ||
		from == makeBasicTypeIndex(IrValueType.i8) ||
		from == makeBasicTypeIndex(IrValueType.i16) ||
		from == makeBasicTypeIndex(IrValueType.i32) ||
		from == makeBasicTypeIndex(IrValueType.i64))
	{
		addUnaryBranch(gen, rval, currentBlock, trueExit, falseExit);
		return;
	}
	else
	{
		//result = builder.emitInstr1(IrOpcode.o_conv, to, rval);
		c.internal_error(t.loc, "%s to %s", childExpr.type.printer(c), t.type.printer(c));
	}
	c.unreachable;
}
