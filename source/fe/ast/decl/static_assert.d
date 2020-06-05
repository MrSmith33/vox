/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.static_assert;

import all;


@(AstType.decl_static_assert)
struct StaticAssertDeclNode
{
	mixin AstNodeData!(AstType.decl_static_assert, AstFlags.isDeclaration, AstNodeState.name_register_self_done);
	AstIndex condition;
	AstIndex message; // for now only single string is allowed
}

void print_static_assert(StaticAssertDeclNode* node, ref AstPrintState state)
{
	state.print("#ASSERT");
	print_ast(node.condition, state);
	print_ast(node.message, state);
}

void post_clone_static_assert(StaticAssertDeclNode* node, ref CloneState state)
{
	state.fixAstIndex(node.condition);
	state.fixAstIndex(node.message);
}

void name_register_nested_static_assert(StaticAssertDeclNode* node, ref NameRegisterState state)
{
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.condition, state);
	require_name_register(node.message, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_static_assert(StaticAssertDeclNode* node, ref NameResolveState state)
{
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.condition, state);
	require_name_resolve(node.message, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_static_assert(StaticAssertDeclNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	scope(exit) node.state = AstNodeState.type_check_done;

	if (node.message.astType(c) != AstType.literal_string) {
		c.error(node.loc, "Error: static assert only supports string literal as a message");
		return;
	}

	IrIndex val = eval_static_expr(node.condition, c);
	if (!c.constants.get(val).i64)
	{
		c.error(node.loc, "static assert: \"%s\"", node.message.get!StringLiteralExprNode(c).value);
	}
}
