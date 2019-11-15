/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.stmt.return_stmt;

import all;


@(AstType.stmt_return)
struct ReturnStmtNode {
	mixin AstNodeData!(AstType.stmt_return, AstFlags.isStatement, AstNodeState.name_register_nested_done);
	AstIndex expression; // Nullable
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
	if (!state.curFunc)
	{
		c.error(node.loc,
			"Return statement is not inside function");
		return;
	}

	AstIndex retTypeIndex = state.curFunc.signature.get!FunctionSignatureNode(c).returnType;
	bool isVoidFunc = retTypeIndex.isVoidType(c);

	if (node.expression)
	{
		require_type_check(node.expression, state);
		if (isVoidFunc)
		{
			c.error(node.expression.get_expr(c).loc,
				"Cannot return expression of type `%s` from void function",
				node.expression.get_expr(c).type.typeName(c));
		}
		else
		{
			autoconvTo(node.expression, retTypeIndex, c);
		}
	}
	else
	{
		if (!isVoidFunc)
			c.error(node.loc,
				"Cannot return void from non-void function",
				node.expression.get_expr(c).type.typeName(c));
	}
	node.state = AstNodeState.type_check_done;
}
