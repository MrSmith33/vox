/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Constant folding and Compile-time function evaluation (CTFE)
/// Requires nodes that are evaluated to be type checked
module fe.eval;

import all;

/// Eval only literals for now
/// Returns integer constant
IrIndex eval_static_expr(AstNode* node, CompilationContext* context)
{
	switch(node.state) with(AstNodeState)
	{
		case name_resolve_done:
			// perform type checking of forward referenced node
			require_type_check(node, context);
			break;
		case type_check_done: break; // all requirement are done
		default: context.internal_error(node.loc, "Node in %s state", node.state);
	}

	context.assertf(node !is null, "null node");

	switch (node.astType) with(AstType)
	{
		case decl_enum_member: return eval_static_expr_enum_member(cast(EnumMemberDecl*)node, context);
		case expr_var_name_use: return eval_static_expr_var_name_use(cast(NameUseExprNode*)node, context);
		case expr_static_array_member: return eval_static_expr_static_array_member(cast(MemberExprNode*)node, context);
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
	return eval_static_expr(node.initializer.as_node, context);
}

IrIndex eval_static_expr_var_name_use(NameUseExprNode* node, CompilationContext* context)
{
	return eval_static_expr(node.entity, context);
}

IrIndex eval_static_expr_static_array_member(MemberExprNode* node, CompilationContext* context)
{
	if (!node.irValue.isDefined)
	{
		if (node.memberIndex == BuiltinMemberIndex.MEMBER_LENGTH) // length
		{
			StaticArrayTypeNode* arr = node.aggregate.as_node.get_node_type.as_static_array;
			require_type_check(arr.as_node, context);
			node.irValue = context.constants.add(arr.length, IsSigned.no, IrArgSize.size64);
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
		IrIndex leftVal = eval_static_expr(node.left.as_node, context);
		IrIndex rightVal = eval_static_expr(node.right.as_node, context);
		node.irValue = calcBinOp(node.op, leftVal, rightVal, node.type.argSize(context), context);
	}
	return node.irValue;
}

IrIndex eval_static_expr_type_conv(TypeConvExprNode* node, CompilationContext* context)
{
	if (!node.irValue.isDefined)
	{
		node.irValue = eval_static_expr(node.expr.as_node, context);
	}
	return node.irValue;
}

IrIndex eval_static_expr_literal_int(IntLiteralExprNode* node, CompilationContext* context)
{
	if (!node.irValue.isDefined)
	{
		node.irValue = context.constants.add(node.value, node.isSigned, node.type.argSize(context));
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
		if (node.type.isPointer)
		{
			node.irValue = context.constants.add(0, IsSigned.no, SIZET_SIZE);
		}
		else if (node.type.isSlice)
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
			node.irValue = context.constants.add(1, IsSigned.no, node.type.argSize(context));
		else
			node.irValue = context.constants.add(0, IsSigned.no, node.type.argSize(context));
	}
	return node.irValue;
}
