/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.stmt.if_stmt;

import all;


struct IfStmtNode {
	mixin AstNodeData!(AstType.stmt_if, AstFlags.isStatement);
	AstIndex condition;
	AstIndex thenStatement;
	AstIndex elseStatement; // Nullable
	AstIndex then_scope;
	AstIndex else_scope;
}

void name_register_if(IfStmtNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	require_name_register(node.condition, state);
	node.then_scope = state.pushScope("Then", Yes.ordered);
	require_name_register(node.thenStatement, state);
	state.popScope;
	if (node.elseStatement) {
		node.else_scope = state.pushScope("Else", Yes.ordered);
		require_name_register(node.elseStatement, state);
		state.popScope;
	}
	node.state = AstNodeState.name_register_done;
}

void name_resolve_if(IfStmtNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.condition, state);
	state.pushScope(node.then_scope);
	require_name_resolve(node.thenStatement, state);
	state.popScope;
	if (node.elseStatement) {
		state.pushScope(node.else_scope);
		require_name_resolve(node.elseStatement, state);
		state.popScope;
	}
	node.state = AstNodeState.name_resolve_done;
}
