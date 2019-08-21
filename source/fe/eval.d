/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Constant folding and Compile-time function evaluation (CTFE)
/// Requires nodes that are evaluated to be type checked
module fe.eval;

import all;

/// Eval only literals for now
/// Returns integer constant
IrIndex eval_static_expr(AstIndex nodeIndex, CompilationContext* context)
{
	AstNode* node = context.getAstNode(nodeIndex);

	switch(node.state) with(AstNodeState)
	{
		case name_resolve_done:
			// perform type checking of forward referenced node
			require_type_check(nodeIndex, context);
			break;
		case type_check_done: break; // all requirement are done
		default: context.internal_error(node.loc, "Node %s in %s state", node.astType, node.state);
	}

	context.assertf(node !is null, "null node");

	switch (node.astType) with(AstType)
	{
		case decl_enum_member: return eval_static_expr_enum_member(cast(EnumMemberDecl*)node, context);
		case expr_name_use: return eval_static_expr_name_use(cast(NameUseExprNode*)node, context);
		case expr_member:
			if (node.subType == MemberSubType.static_array_member)
				return eval_static_expr_static_array_member(cast(MemberExprNode*)node, context);
			goto default;
		case expr_bin_op: return eval_static_expr_bin_op(cast(BinaryExprNode*)node, context);
		case expr_type_conv: return eval_static_expr_type_conv(cast(TypeConvExprNode*)node, context);
		case literal_int: return eval_static_expr_literal_int(cast(IntLiteralExprNode*)node, context);
		case literal_string: return eval_static_expr_literal_string(cast(StringLiteralExprNode*)node, context);
		case literal_null: return eval_static_expr_literal_null(cast(NullLiteralExprNode*)node, context);
		case literal_bool: return eval_static_expr_literal_bool(cast(BoolLiteralExprNode*)node, context);
		default:
			context.internal_error(node.loc, "Cannot evaluate static expression %s", node.astType);
			assert(false);
	}
}

IrIndex eval_static_expr_enum_member(EnumMemberDecl* node, CompilationContext* context)
{
	return eval_static_expr(node.initializer, context);
}

IrIndex eval_static_expr_name_use(NameUseExprNode* node, CompilationContext* context)
{
	return eval_static_expr(node.entity, context);
}

IrIndex eval_static_expr_static_array_member(MemberExprNode* node, CompilationContext* context)
{
	if (!node.irValue.isDefined)
	{
		if (node.memberIndex == BuiltinMemberIndex.MEMBER_LENGTH) // length
		{
			AstIndex arr = node.aggregate.get_node_type(context);
			require_type_check(arr, context);
			node.irValue = context.constants.add(arr.cast_type_static_array(context).length, IsSigned.no, IrArgSize.size64);
		}
		else if (node.memberIndex == BuiltinMemberIndex.MEMBER_PTR) // ptr
		{
			context.unrecoverable_error(node.loc, "Cannot access static array .ptr member while in CTFE. Not implemented");
		}
		else
			context.unreachable;
	}
	return node.irValue;
}

IrIndex eval_static_expr_bin_op(BinaryExprNode* node, CompilationContext* context)
{
	if (!node.irValue.isDefined)
	{
		IrIndex leftVal = eval_static_expr(node.left, context);
		IrIndex rightVal = eval_static_expr(node.right, context);
		node.irValue = calcBinOp(node.op, leftVal, rightVal, node.type.typeArgSize(context), context);
	}
	return node.irValue;
}

IrIndex eval_static_expr_type_conv(TypeConvExprNode* node, CompilationContext* context)
{
	if (!node.irValue.isDefined)
	{
		node.irValue = eval_static_expr(node.expr, context);
	}
	return node.irValue;
}

IrIndex eval_static_expr_literal_int(IntLiteralExprNode* node, CompilationContext* context)
{
	if (!node.irValue.isDefined)
	{
		node.irValue = context.constants.add(node.value, node.isSigned, node.type.typeArgSize(context));
	}
	return node.irValue;
}

IrIndex eval_static_expr_literal_string(StringLiteralExprNode* node, CompilationContext* context)
{
	return node.irValue;
}

IrIndex eval_static_expr_literal_null(NullLiteralExprNode* node, CompilationContext* context)
{
	if (!node.irValue.isDefined)
	{
		if (node.type.get_type(context).isPointer)
		{
			node.irValue = context.constants.add(0, IsSigned.no, SIZET_SIZE);
		}
		else if (node.type.get_type(context).isSlice)
		{
			IrIndex irValue = context.constants.add(0, IsSigned.no, SIZET_SIZE); // ptr and length
			node.irValue = context.constants.addAggrecateConstant(node.type.gen_ir_type(context), irValue, irValue);
		}
		else
		{
			context.internal_error(node.loc, "%s", node.type.printer(context));
		}
	}
	return node.irValue;
}

IrIndex eval_static_expr_literal_bool(BoolLiteralExprNode* node, CompilationContext* context)
{
	if (!node.irValue.isDefined)
	{
		if (node.value)
			node.irValue = context.constants.add(1, IsSigned.no, node.type.typeArgSize(context));
		else
			node.irValue = context.constants.add(0, IsSigned.no, node.type.typeArgSize(context));
	}
	return node.irValue;
}
