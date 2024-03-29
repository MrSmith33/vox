/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.stmt.return_stmt;

import vox.all;


@(AstType.stmt_return)
struct ReturnStmtNode {
	mixin AstNodeData!(AstType.stmt_return, 0, AstNodeState.name_register_nested_done);
	AstIndex parentFunction;
	AstIndex expression; // Nullable
}

void print_return(ReturnStmtNode* node, ref AstPrintState state)
{
	state.print("RETURN");
	if (node.expression) print_ast(node.expression, state);
}

void post_clone_return(ReturnStmtNode* node, ref CloneState state)
{
	state.fixAstIndex(node.parentFunction);
	state.fixAstIndex(node.expression);
}

void name_resolve_return(ReturnStmtNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	if (node.expression) require_name_resolve(node.expression, state);
	node.state = AstNodeState.name_resolve_done;
}

// Check return type and function return type
void type_check_return(ReturnStmtNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;

	AstIndex retTypeIndex = node.parentFunction.get!FunctionDeclNode(c).signature.get!FunctionSignatureNode(c).returnType;
	bool isVoidFunc = retTypeIndex.isVoidType(c);

	if (node.expression)
	{
		require_type_check_expr(retTypeIndex, node.expression, state);

		bool success = autoconvTo(node.expression, retTypeIndex, c);
		if (!success)
			c.error(node.loc,
				"Cannot implicitly convert expression of type `%s` to `%s`",
				node.expression.get_expr_type(c).printer(c),
				retTypeIndex.printer(c));
	}
	else
	{
		if (!isVoidFunc)
			c.error(node.loc,
				"Cannot return void from non-void function");
	}
	node.state = AstNodeState.type_check_done;
}

void ir_gen_return(ref IrGenState gen, IrIndex currentBlock, ref IrLabel nextStmt, ReturnStmtNode* node)
{
	CompilationContext* c = gen.context;
	if (node.expression)
	{
		IrLabel afterExpr = IrLabel(currentBlock);
		ExprValue lval = ir_gen_expr(gen, node.expression, currentBlock, afterExpr);
		currentBlock = afterExpr.blockIndex;
		if (node.expression.get_expr_type(c).isVoidType(c)) {
			gen.builder.addReturn(currentBlock);
			return;
		}
		IrIndex rval = lval.rvalue(gen, node.loc, currentBlock);
		gen.builder.addReturn(currentBlock, rval);
	}
	else gen.builder.addReturn(currentBlock);
}
