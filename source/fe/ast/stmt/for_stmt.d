/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.stmt.for_stmt;

import all;


@(AstType.stmt_for)
struct ForStmtNode {
	mixin AstNodeData!(AstType.stmt_for, AstFlags.isStatement, AstNodeState.name_register_self_done);
	AstNodes init_statements;
	AstIndex condition; // Nullable
	AstNodes increment_statements;
	AstNodes body_statements;
}

void name_register_nested_for(ForStmtNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.init_statements, state);
	if (node.condition) require_name_register(node.condition, state);
	require_name_register(node.increment_statements, state);
	require_name_register(node.body_statements, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_for(ForStmtNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.init_statements, state);
	if (node.condition) require_name_resolve(node.condition, state);
	require_name_resolve(node.increment_statements, state);
	require_name_resolve(node.body_statements, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_for(ForStmtNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.init_statements, state);
	if (node.condition) {
		require_type_check(node.condition, state);
		autoconvToBool(node.condition, state.context);
	}
	require_type_check(node.increment_statements, state);
	require_type_check(node.body_statements, state);
	node.state = AstNodeState.type_check_done;
}
