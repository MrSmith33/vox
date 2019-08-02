/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.index;

import all;


struct IndexExprNode {
	mixin ExpressionNodeData!(AstType.expr_index);
	ExpressionNode* array;
	ExpressionNode* index;
}

void name_resolve_index(IndexExprNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.array.as_node, state);
	require_name_resolve(node.index.as_node, state);
	node.state = AstNodeState.name_resolve_done;
}
