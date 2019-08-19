/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.index;

import all;


struct IndexExprNode {
	mixin ExpressionNodeData!(AstType.expr_index);
	AstIndex array;
	AstIndex index;
}

void name_resolve_index(IndexExprNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.array, state);
	require_name_resolve(node.index, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_index(IndexExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	require_type_check(node.array, state);
	require_type_check(node.index, state);
	autoconvTo(node.index, c.basicTypeNodes(BasicType.t_i64), c);
	switch (node.array.expr_type(c).astType(c)) with(AstType)
	{
		case type_ptr, type_static_array, type_slice: break; // valid
		default: c.internal_error("Cannot index value of type `%s`", node.array.expr_type(c).printer(c));
	}
	node.type = node.array.expr_type(c).get_type(c).getElementType(c);
	node.state = AstNodeState.type_check_done;
}
