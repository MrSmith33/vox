/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.stmt.block_stmt;

import vox.all;


@(AstType.stmt_block)
struct BlockStmtNode {
	mixin AstNodeData!(AstType.stmt_block, 0, AstNodeState.name_register_self_done);
	/// Each node can be expression, declaration or expression
	AstNodes statements;
}

void print_block(BlockStmtNode* node, ref AstPrintState state)
{
	state.print("BLOCK");
	print_ast(node.statements, state);
}

void post_clone_block(BlockStmtNode* node, ref CloneState state)
{
	state.fixAstNodes(node.statements);
}

void name_register_nested_block(BlockStmtNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.statements, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_block(BlockStmtNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.statements, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_block(BlockStmtNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.statements, state);
	node.state = AstNodeState.type_check_done;
}

void ir_gen_block(ref IrGenState gen, IrIndex curBlock, ref IrLabel nextStmt, BlockStmtNode* b)
{
	genBlock(gen, b.as!AstNode(gen.context), b.statements, curBlock, nextStmt);
}
