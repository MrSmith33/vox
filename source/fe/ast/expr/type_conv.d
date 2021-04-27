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
		IrIndex result = c.constants.add(aliasedNode.storageIndex, IsSigned.no, IrArgSize.size32);
		gen.builder.addJumpToLabel(currentBlock, nextStmt);
		return ExprValue(result);
	}

	IrLabel afterExpr = IrLabel(currentBlock);
	ExprValue lval = ir_gen_expr(gen, t.expr, currentBlock, afterExpr);
	currentBlock = afterExpr.blockIndex;
	IrIndex rval = getRvalue(gen, t.loc, currentBlock, lval);

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
		case fail: c.internal_error(t.loc, "IR gen of failed cast"); assert(false);
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
			result = c.constants.getAggregateMember(rval, 1);
			break;
		case array_literal_to_slice:
			assert(false, to!string(t.convKind));
	}
	gen.builder.addJumpToLabel(currentBlock, nextStmt);
	return ExprValue(result);
}

IrIndex eval_type_conv(TypeConvExprNode* node, IrIndex rval, CompilationContext* c)
{
	c.assertf(rval.isSomeConstant, node.loc, "Must be constant");
	IrIndex result;

	TypeNode* targetType = node.type.get_type(c);
	if (targetType.astType == AstType.decl_enum) {
		targetType = targetType.as_enum.memberType.get_type(c);
	}
	IrIndex typeTo = targetType.gen_ir_type(c);
	uint typeSizeTo = c.types.typeSize(typeTo);

	final switch (node.convKind) with(TypeConvResKind) {
		case fail: c.internal_error(node.loc, "eval of failed cast"); assert(false);
		case no_e, no_i: result = rval; break;
		case ii_e, ii_i, override_expr_type_e, override_expr_type_i:
			if (rval.isConstantZero) {
				return typeTo.zeroConstantOfType;
			}
			result = rval;
			result.constantSize = sizeToIrArgSize(typeSizeTo, c);
			break;
		case if_e, if_i:
			assert(false, "TODO");
			break;

		case ff_e, ff_i:
			assert(false, "TODO");
			break;

		case fi_e, fi_i:
			assert(false, "TODO");
			break;

		case string_literal_to_u8_ptr:
			result = c.constants.getAggregateMember(rval, 1);
			break;

		case array_literal_to_slice:
			assert(false, to!string(node.convKind));
			assert(false, "TODO");
	}
	return result;
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

	if (rval.isSimpleConstant ||
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
