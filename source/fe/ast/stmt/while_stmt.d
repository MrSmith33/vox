/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.stmt.while_stmt;

import all;


@(AstType.stmt_while)
struct WhileStmtNode {
	mixin AstNodeData!(AstType.stmt_while, AstFlags.isStatement, AstNodeState.name_register_self_done);
	AstIndex condition;
	AstNodes statements;
}

void name_register_nested_while(WhileStmtNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.condition, state);
	require_name_register(node.statements, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_while(WhileStmtNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.condition, state);
	require_name_resolve(node.statements, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_while(WhileStmtNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.condition, state);
	autoconvToBool(node.condition, state.context);
	require_type_check(node.statements, state);
	node.state = AstNodeState.type_check_done;
}
