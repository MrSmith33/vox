/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.stmt.block_stmt;

import all;


struct BlockStmtNode {
	mixin AstNodeData!(AstType.stmt_block, AstFlags.isStatement);
	/// Each node can be expression, declaration or expression
	Array!(AstNode*) statements;
	Scope* _scope;
}

void name_register_block(BlockStmtNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	node._scope = state.pushScope("Block", Yes.ordered);
	foreach(stmt; node.statements) require_name_register(stmt, state);
	state.popScope;
	node.state = AstNodeState.name_register_done;
}

void name_resolve_block(BlockStmtNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	state.pushScope(node._scope);
	foreach(stmt; node.statements) require_name_resolve(stmt, state);
	state.popScope;
	node.state = AstNodeState.name_resolve_done;
}
