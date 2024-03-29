/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.stmt.while_stmt;

import vox.all;


@(AstType.stmt_while)
struct WhileStmtNode {
	mixin AstNodeData!(AstType.stmt_while, 0, AstNodeState.name_register_self_done);
	AstIndex condition;
	AstNodes statements;
}

void print_while(WhileStmtNode* node, ref AstPrintState state)
{
	state.print("WHILE");
	print_ast(node.condition, state);
	print_ast(node.statements, state);
}

void post_clone_while(WhileStmtNode* node, ref CloneState state)
{
	state.fixAstIndex(node.condition);
	state.fixAstNodes(node.statements);
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

void ir_gen_while(ref IrGenState gen, IrIndex currentBlock, ref IrLabel nextStmt, WhileStmtNode* n)
{
	// loop header
	IrLabel loopHeaderLabel = IrLabel(currentBlock);

	IrLabel* prevLoopHeader = gen.currentLoopHeader; // save continue label
	gen.currentLoopHeader = &loopHeaderLabel;
	scope(exit) gen.currentLoopHeader = prevLoopHeader; // restore continue label

	IrLabel* prevLoopEnd = gen.currentLoopEnd; // save break label
	gen.currentLoopEnd = &nextStmt;
	scope(exit) gen.currentLoopEnd = prevLoopEnd; // restore break label

	gen.builder.addJumpToLabel(currentBlock, loopHeaderLabel);

	// we need loop header in a separate block because it will
	// have 2 predecessors: currentBlock and loop body
	gen.builder.forceAllocLabelBlock(loopHeaderLabel);
	IrIndex loopHeaderBlock = loopHeaderLabel.blockIndex;
	gen.ir.getBlock(loopHeaderBlock).preventSeal = true;
	currentBlock = loopHeaderBlock;

	IrLabel bodyLabel = IrLabel(currentBlock);

	// will force allocate body block
	ir_gen_branch(gen, n.condition, loopHeaderBlock, bodyLabel, nextStmt);

	// body
	if (bodyLabel.numPredecessors > 0)
	{
		currentBlock = bodyLabel.blockIndex;
		gen.builder.sealBlock(currentBlock);

		IrBasicBlock* block = gen.ir.getBlock(currentBlock);
		assert(!block.isFinished);
		genBlock(gen, n.as!AstNode(gen.context), n.statements, currentBlock, loopHeaderLabel);
	}

	if (loopHeaderLabel.numPredecessors > 1)
		gen.ir.getBlock(loopHeaderBlock).isLoopHeader = true;

	assert(!gen.ir.getBlock(loopHeaderBlock).isSealed);
	gen.builder.sealBlock(loopHeaderBlock, true);
}
