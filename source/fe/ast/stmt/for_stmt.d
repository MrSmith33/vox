/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.stmt.for_stmt;

import all;


struct ForStmtNode {
	mixin AstNodeData!(AstType.stmt_for, AstFlags.isStatement);
	Array!(AstNode*) init_statements;
	ExpressionNode* condition; // Nullable
	Array!(AstNode*) increment_statements;
	AstNode* statement;
	Scope* _scope;
}
