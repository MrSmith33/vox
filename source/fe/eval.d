/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Constant folding and Compile-time function evaluation (CTFE)
/// Requires nodes that are evaluated to be type checked
module fe.eval;

import all;

/// Eval only expressions now. No CTFE yet
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
		case expr_member: return eval_static_expr_member(cast(MemberExprNode*)node, context);
		case expr_bin_op: return eval_static_expr_bin_op(cast(BinaryExprNode*)node, context);
		case expr_un_op: return eval_static_expr_un_op(cast(UnaryExprNode*)node, context);
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

IrIndex eval_static_expr_member(MemberExprNode* node, CompilationContext* c)
{
	if (!node.irValue.isDefined)
	{
		switch(node.subType) with(MemberSubType)
		{
			case enum_member:
				node.irValue = eval_static_expr(node.member(c), c);
				break;
			case builtin_member:
				node.irValue = eval_builtin(node.member(c).get!BuiltinNode(c).builtin, node.aggregate, node.loc, c);
				break;
			default:
				AstIndex nodeIndex = get_ast_index(node, c);
				c.unrecoverable_error(node.loc,
					"Cannot access .%s member of %s while in CTFE",
					get_node_id(nodeIndex, c),
					get_node_kind_name(nodeIndex, c));
		}
	}
	return node.irValue;
}

IrIndex eval_builtin(BuiltinId builtin, AstIndex obj, TokenIndex loc, CompilationContext* c)
{
	AstIndex objType = obj.get_node_type(c);
	switch(builtin) with(BuiltinId)
	{
		case int_min:
			auto b = objType.get!BasicTypeNode(c);
			return c.constants.add(b.minValue, b.isSigned, objType.get_type(c).argSize(c));
		case int_max:
			auto b = objType.get!BasicTypeNode(c);
			return c.constants.add(b.maxValue, b.isSigned, objType.get_type(c).argSize(c));
		case array_length:
			require_type_check(objType, c);
			return c.constants.add(objType.get!StaticArrayTypeNode(c).length, IsSigned.no, IrArgSize.size64);
		case type_sizeof:
			require_type_check(objType, c);
			return c.constants.add(objType.get_type(c).size(c), IsSigned.no, IrArgSize.size64);
		default:
			c.unrecoverable_error(loc,
				"Cannot access .%s member of %s while in CTFE",
				builtinIdStrings[builtin],
				get_node_kind_name(objType, c));
			assert(false);
	}
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

IrIndex eval_static_expr_un_op(UnaryExprNode* node, CompilationContext* c)
{
	if (!node.irValue.isDefined)
	{
		ExpressionNode* child = node.child.get_expr(c);
		switch (node.op) with(UnOp)
		{
			case addrOf:
				switch(child.astType)
				{
					case AstType.expr_name_use:
						AstNode* entity = child.as!NameUseExprNode(c).entity.get_node(c);

						switch (entity.astType)
						{
							// TODO: force IR gen for global var when address is taken
							case AstType.decl_function:
								// type is not pointer to function sig, but sig itself
								node.irValue = entity.as!FunctionDeclNode(c).getIrIndex(c);
								break;
							default:
								c.internal_error(node.loc, "Cannot take address of %s while in CTFE", entity.astType);
						}
						break;
					default:
						c.internal_error(node.loc, "Cannot take address of %s while in CTFE", child.astType);
				}
				break;
			default:
				c.internal_error(node.loc, "%s not implemented in CTFE", node.op);
		}
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
