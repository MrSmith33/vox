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
