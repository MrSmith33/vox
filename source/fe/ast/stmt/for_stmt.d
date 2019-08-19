/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.stmt.for_stmt;

import all;


struct ForStmtNode {
	mixin AstNodeData!(AstType.stmt_for, AstFlags.isStatement);
	Array!AstIndex init_statements;
	AstIndex condition; // Nullable
	Array!AstIndex increment_statements;
	AstIndex statement;
	AstIndex _scope;
}

void name_register_for(ForStmtNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	node._scope = state.pushScope("For", Yes.ordered);
	foreach(stmt; node.init_statements) require_name_register(stmt, state);
	if (node.condition) require_name_register(node.condition, state);
	foreach(stmt; node.increment_statements) require_name_register(stmt, state);
	require_name_register(node.statement, state);
	state.popScope;
	node.state = AstNodeState.name_register_done;
}

void name_resolve_for(ForStmtNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	state.pushScope(node._scope);
	foreach(stmt; node.init_statements) require_name_resolve(stmt, state);
	if (node.condition) require_name_resolve(node.condition, state);
	foreach(stmt; node.increment_statements) require_name_resolve(stmt, state);
	require_name_resolve(node.statement, state);
	state.popScope;
	node.state = AstNodeState.name_resolve_done;
}
