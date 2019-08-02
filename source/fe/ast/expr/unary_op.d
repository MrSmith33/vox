/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.unary_op;

import all;


struct UnaryExprNode {
	mixin ExpressionNodeData!(AstType.expr_un_op);
	UnOp op;
	ExpressionNode* child;
}

void name_resolve_unary_op(UnaryExprNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.child.as_node, state);
	node.state = AstNodeState.name_resolve_done;
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
