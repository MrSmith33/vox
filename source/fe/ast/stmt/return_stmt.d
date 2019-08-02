/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.stmt.return_stmt;

import all;


struct ReturnStmtNode {
	mixin AstNodeData!(AstType.stmt_return, AstFlags.isStatement, AstNodeState.name_register_done);
	ExpressionNode* expression; // Nullable
}

void name_resolve_return(ReturnStmtNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	if (node.expression) require_name_resolve(node.expression.as_node, state);
	node.state = AstNodeState.name_resolve_done;
}
