/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.stmt.do_stmt;

import all;


@(AstType.stmt_do_while)
struct DoWhileStmtNode {
	mixin AstNodeData!(AstType.stmt_do_while, 0, AstNodeState.name_register_self_done);
	AstIndex condition;
	AstNodes statements;
}

void print_do(DoWhileStmtNode* node, ref AstPrintState state)
{
	state.print("DO");
	print_ast(node.condition, state);
	state.print("WHILE");
	print_ast(node.statements, state);
}

void post_clone_do(DoWhileStmtNode* node, ref CloneState state)
{
	state.fixAstIndex(node.condition);
	state.fixAstNodes(node.statements);
}

void name_register_nested_do(DoWhileStmtNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.statements, state);
	require_name_register(node.condition, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_do(DoWhileStmtNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.statements, state);
	require_name_resolve(node.condition, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_do(DoWhileStmtNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.statements, state);
	require_type_check(node.condition, state);
	autoconvToBool(node.condition, state.context);
	node.state = AstNodeState.type_check_done;
}

void ir_gen_do(ref IrGenState gen, IrIndex currentBlock, ref IrLabel nextStmt, DoWhileStmtNode* d)
{
	gen.context.unreachable;
}
