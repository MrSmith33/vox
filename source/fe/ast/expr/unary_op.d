/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.unary_op;

import all;


@(AstType.expr_un_op)
struct UnaryExprNode {
	mixin ExpressionNodeData!(AstType.expr_un_op);
	UnOp op;
	AstIndex child;
}

void name_register_nested_unary_op(UnaryExprNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.child, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_unary_op(UnaryExprNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.child, state);
	switch(node.op) with(UnOp)
	{
		case addrOf:
			AstNode* child = node.child.get_node(state.context);
			child.flags |= AstFlags.isLvalue;
			if (child.astType == AstType.expr_name_use)
			{
				child.flags |= NameUseFlags.isAddressTaken;
			}
			break;
		default:
			break;
	}
	node.state = AstNodeState.name_resolve_done;
}

void type_check_unary_op(UnaryExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	require_type_check(node.child, state);
	ExpressionNode* child = node.child.get_expr(c);
	assert(child.type, format("child(%s).type: is null", child.astType));

	if (child.type.isErrorType(c))
	{
		node.type = child.type;
		node.state = AstNodeState.type_check_done;
		return;
	}

	switch(node.op) with(UnOp)
	{
		case addrOf:
			// make sure that variable gets stored in memory
			switch(child.astType)
			{
				case AstType.expr_name_use:
					AstNode* entity = child.as!NameUseExprNode(c).entity.get_node(c);

					switch (entity.astType)
					{
						case AstType.decl_var:
							entity.flags |= VariableFlags.isAddressTaken; // mark variable
							node.type = c.appendAst!PtrTypeNode(node.child.loc(c), node.child.expr_type(c));
							break;
						case AstType.decl_function:
							node.type = c.appendAst!PtrTypeNode(node.child.loc(c), node.child.expr_type(c));
							break;
						default:
							c.internal_error(node.loc, "Cannot take address of %s", entity.astType);
					}
					break;
				case AstType.expr_index:
					node.type = c.appendAst!PtrTypeNode(node.child.loc(c), node.child.expr_type(c));
					break;
				default:
					c.internal_error(node.loc, "Cannot take address of %s", child.astType);
			}
			break;
		case bitwiseNot:
			node.type = child.type;
			break;
		case logicalNot:
			autoconvToBool(node.child, c);
			node.type = c.basicTypeNodes(BasicType.t_bool);
			break;
		case minus:
			node.type = child.type;
			break;
		case preIncrement, postIncrement, preDecrement, postDecrement:
			node.type = child.type;
			break;
		case deref:
			if (child.type.isErrorType(c)) {
				node.type = child.type;
				break;
			}
			if (!child.type.isPointerType(c)) {
				c.unrecoverable_error(node.loc, "Cannot dereference %s", child.type.printer(c));
			}
			node.type = child.type.get_type(c).as_ptr.base;
			break;
		default:
			c.internal_error("un op %s not implemented", node.op);
			node.type = node.child.expr_type(c);
	}
	node.state = AstNodeState.type_check_done;
}

enum UnOp : ubyte {
	plus, // +
	minus, // -
	logicalNot, // !
	bitwiseNot, // ~
	deref, // *
	addrOf, // &
	preIncrement, // ++x
	preDecrement, // --x
	postIncrement, // x++
	postDecrement, // x--
	staticArrayToSlice,
}
