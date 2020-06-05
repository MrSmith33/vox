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

void print_for(ForStmtNode* node, ref AstPrintState state)
{
	state.print("FOR");
	print_ast(node.init_statements, state);
	state.print("COND");
	if (node.condition)
		print_ast(node.condition, state);
	state.print("INC");
	print_ast(node.increment_statements, state);
	print_ast(node.body_statements, state);
}

void post_clone_for(ForStmtNode* node, ref CloneState state)
{
	state.fixAstIndex(node.condition);
	state.fixAstNodes(node.init_statements);
	state.fixAstNodes(node.increment_statements);
	state.fixAstNodes(node.body_statements);
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

void ir_gen_for(ref IrGenState gen, IrIndex currentBlock, ref IrLabel nextStmt, ForStmtNode* n)
{
	CompilationContext* c = gen.context;
	// init statements
	IrLabel afterInitLabel = IrLabel(currentBlock);
	genBlock(gen, n.as!AstNode(c), n.init_statements, currentBlock, afterInitLabel);
	currentBlock = afterInitLabel.blockIndex;

	// loop header
	IrLabel loopHeaderLabel = IrLabel(currentBlock);
	// increment section of body
	IrLabel incrementLabel = IrLabel(currentBlock);

	// continue label
	IrLabel* prevLoopHeader = gen.currentLoopHeader; // save continue label
	gen.currentLoopHeader = &incrementLabel;
	scope(exit) gen.currentLoopHeader = prevLoopHeader; // restore continue label

	// break label
	IrLabel* prevLoopEnd = gen.currentLoopEnd; // save break label
	gen.currentLoopEnd = &nextStmt;
	scope(exit) gen.currentLoopEnd = prevLoopEnd; // restore break label

	gen.builder.addJumpToLabel(currentBlock, loopHeaderLabel);

	// we need loop header in a separate block because it will
	// have 2 predecessors: currentBlock and loop body
	gen.builder.forceAllocLabelBlock(loopHeaderLabel);
	IrIndex loopHeaderBlock = loopHeaderLabel.blockIndex;
	currentBlock = loopHeaderBlock;

	IrLabel bodyLabel = IrLabel(currentBlock);

	// will force allocate body block
	if (n.condition)
		ir_gen_branch(gen, n.condition, loopHeaderBlock, bodyLabel, nextStmt);
	else
		gen.builder.addJumpToLabel(loopHeaderBlock, bodyLabel);

	// body
	if (bodyLabel.numPredecessors > 0)
	{
		currentBlock = bodyLabel.blockIndex;
		gen.builder.sealBlock(currentBlock);

		IrBasicBlock* block = gen.ir.getBlock(currentBlock);
		assert(!block.isFinished);
		genBlock(gen, n.as!AstNode(c), n.body_statements, currentBlock, incrementLabel);

		if (incrementLabel.numPredecessors > 0)
		{
			gen.builder.sealBlock(incrementLabel.blockIndex);
			genBlock(gen, n.as!AstNode(c), n.increment_statements, incrementLabel.blockIndex, loopHeaderLabel);
		}
	}

	if (loopHeaderLabel.numPredecessors > 1)
		gen.ir.getBlock(loopHeaderBlock).isLoopHeader = true;

	gen.builder.sealBlock(loopHeaderBlock);
}
