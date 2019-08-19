/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.stmt.while_stmt;

import all;


struct WhileStmtNode {
	mixin AstNodeData!(AstType.stmt_while, AstFlags.isStatement);
	AstIndex condition;
	AstIndex statement;
	AstIndex _scope;
}

void name_register_while(WhileStmtNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	require_name_register(node.condition, state);
	node._scope = state.pushScope("While", Yes.ordered);
	require_name_register(node.statement, state);
	state.popScope;
	node.state = AstNodeState.name_register_done;
}

void name_resolve_while(WhileStmtNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.condition, state);
	state.pushScope(node._scope);
	require_name_resolve(node.statement, state);
	state.popScope;
	node.state = AstNodeState.name_resolve_done;
}
