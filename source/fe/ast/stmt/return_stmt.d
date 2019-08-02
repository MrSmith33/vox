/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.stmt.return_stmt;

import all;


struct ReturnStmtNode {
	mixin AstNodeData!(AstType.stmt_return, AstFlags.isStatement, AstNodeState.name_register_done);
	ExpressionNode* expression; // Nullable
}
