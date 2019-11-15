/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.stmt.if_stmt;

import all;


@(AstType.stmt_if)
struct IfStmtNode
{
	mixin AstNodeData!(AstType.stmt_if, AstFlags.isStatement, AstNodeState.name_register_self_done);
	AstIndex condition;
	AstNodes thenStatements; // can be empty
	AstNodes elseStatements; // can be empty
}

void name_register_nested_if(IfStmtNode* node, ref NameRegisterState state)
{
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.condition, state);
	require_name_register(node.thenStatements, state);
	require_name_register(node.elseStatements, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_if(IfStmtNode* node, ref NameResolveState state)
{
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.condition, state);
	require_name_resolve(node.thenStatements, state);
	require_name_resolve(node.elseStatements, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_if(IfStmtNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.condition, state);
	autoconvToBool(node.condition, state.context);
	require_type_check(node.thenStatements, state);
	require_type_check(node.elseStatements, state);
	node.state = AstNodeState.type_check_done;
}
