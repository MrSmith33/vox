/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.stmt.if_stmt;

import all;


@(AstType.stmt_if)
struct IfStmtNode
{
	mixin AstNodeData!(AstType.stmt_if, 0, AstNodeState.name_register_self_done);
	AstIndex condition;
	AstNodes thenStatements; // can be empty
	AstNodes elseStatements; // can be empty
}

void print_if(IfStmtNode* node, ref AstPrintState state)
{
	state.print("IF");
	print_ast(node.condition, state);
	state.print("THEN");
	print_ast(node.thenStatements, state);
	if (!node.elseStatements.empty)
	{
		state.print("ELSE");
		print_ast(node.elseStatements, state);
	}
}

void post_clone_if(IfStmtNode* node, ref CloneState state)
{
	state.fixAstIndex(node.condition);
	state.fixAstNodes(node.thenStatements);
	state.fixAstNodes(node.elseStatements);
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

void ir_gen_if(ref IrGenState gen, IrIndex currentBlock, ref IrLabel nextStmt, IfStmtNode* i)
{
	CompilationContext* c = gen.context;
	if (!i.elseStatements.empty) // if then else
	{
		IrLabel trueLabel = IrLabel(currentBlock);
		IrLabel falseLabel = IrLabel(currentBlock);
		ir_gen_branch(gen, i.condition, currentBlock, trueLabel, falseLabel);

		if (trueLabel.numPredecessors != 0)
		{
			IrIndex thenBlock = trueLabel.blockIndex;
			gen.builder.sealBlock(thenBlock);
			genBlock(gen, i.as!AstNode(c), i.thenStatements, thenBlock, nextStmt);
		}

		if (falseLabel.numPredecessors != 0)
		{
			IrIndex elseBlock = falseLabel.blockIndex;
			gen.builder.sealBlock(elseBlock);
			genBlock(gen, i.as!AstNode(c), i.elseStatements, elseBlock, nextStmt);
		}
	}
	else // if then
	{
		IrLabel trueLabel = IrLabel(currentBlock);
		ir_gen_branch(gen, i.condition, currentBlock, trueLabel, nextStmt);

		if (trueLabel.numPredecessors != 0)
		{
			IrIndex thenBlock = trueLabel.blockIndex;
			gen.builder.sealBlock(thenBlock);
			genBlock(gen, i.as!AstNode(c), i.thenStatements, thenBlock, nextStmt);
		}
	}
}
