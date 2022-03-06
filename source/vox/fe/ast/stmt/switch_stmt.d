/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.stmt.switch_stmt;

import vox.all;


@(AstType.stmt_switch)
struct SwitchStmtNode
{
	mixin AstNodeData!(AstType.stmt_switch, 0, AstNodeState.name_register_self_done);
	AstIndex condition;
	AstIndex elseStmt; // else block stmt, nullable
	Array!SwitchCase cases;
	IrIndex[] argsValues;
}

struct SwitchCase
{
	AstIndex expr; // case constant expr
	AstIndex stmt; // block stmt
}

void print_switch(SwitchStmtNode* node, ref AstPrintState state)
{
	state.print("SWITCH");
	print_ast(node.condition, state);
	foreach (SwitchCase c; node.cases)
	{
		state.print("CASE");
		print_ast(c.expr, state);
		print_ast(c.stmt, state);
	}
	if (node.elseStmt)
	{
		state.print("ELSE");
		print_ast(node.elseStmt, state);
	}
}

void post_clone_switch(SwitchStmtNode* node, ref CloneState state)
{
	state.fixAstIndex(node.condition);
	state.fixAstIndex(node.elseStmt);
	node.cases = node.cases.dup(state.context.arrayArena);
	foreach(ref SwitchCase c; node.cases) {
		state.fixAstIndex(c.expr);
		state.fixAstIndex(c.stmt);
	}
}

void name_register_nested_switch(SwitchStmtNode* node, ref NameRegisterState state)
{
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.condition, state);
	if (node.elseStmt.isDefined) require_name_register(node.elseStmt, state);
	foreach(ref SwitchCase c; node.cases) {
		require_name_register(c.expr, state);
		require_name_register(c.stmt, state);
	}
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_switch(SwitchStmtNode* node, ref NameResolveState state)
{
	require_name_resolve(node.condition, state);
	if (node.elseStmt.isDefined) require_name_resolve(node.elseStmt, state);
	foreach(ref SwitchCase c; node.cases) {
		require_name_resolve(c.expr, state);
		require_name_resolve(c.stmt, state);
	}
}

void type_check_switch(SwitchStmtNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;
	require_type_check(node.condition, state);
	if (node.elseStmt.isDefined) require_type_check(node.elseStmt, state);
	// Args: value + N integer constants
	node.argsValues = c.allocateTempArray!IrIndex(node.cases.length + 1);
	foreach(i, ref SwitchCase caseNode; node.cases) {
		require_type_check(caseNode.expr, state);
		require_type_check(caseNode.stmt, state);
		node.argsValues[i+1] = eval_static_expr(caseNode.expr, c);
	}
}

bool isSwitchableType(CompilationContext* c, AstIndex type)
{
	return false;
}

void ir_gen_switch(ref IrGenState gen, IrIndex currentBlock, ref IrLabel nextStmt, SwitchStmtNode* node)
{
	CompilationContext* c = gen.context;

	IrLabel afterExpr = IrLabel(currentBlock);
	ExprValue lval = ir_gen_expr(gen, node.condition, currentBlock, afterExpr);
	currentBlock = afterExpr.blockIndex;
	IrIndex rval = lval.rvalue(gen, node.loc, currentBlock);

	node.argsValues[0] = rval;
	gen.builder.emitInstr!(IrOpcode.branch_switch)(currentBlock, node.argsValues);

	IrBasicBlock* block = gen.ir.getBlock(currentBlock);
	assert(!block.isFinished);
	block.isFinished = true;

	// default case
	IrIndex defaultBlock = gen.builder.addBasicBlock;
	gen.builder.addBlockTarget(currentBlock, defaultBlock);
	gen.builder.sealBlock(defaultBlock);
	if (node.elseStmt.isDefined) {
		ir_gen_stmt(gen, node.elseStmt, defaultBlock, nextStmt);
	} else {
		gen.builder.addUnreachable(defaultBlock);
	}

	// cases
	foreach(i, ref SwitchCase switchCase; node.cases) {
		IrIndex caseBlock = gen.builder.addBasicBlock;
		gen.builder.addBlockTarget(currentBlock, caseBlock);
		gen.builder.sealBlock(caseBlock);
		ir_gen_stmt(gen, switchCase.stmt, caseBlock, nextStmt);
	}
}
