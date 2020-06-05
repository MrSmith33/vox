/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.static_if;

import all;


@(AstType.decl_static_if)
struct StaticIfDeclNode
{
	mixin AstNodeData!(AstType.decl_static_if, AstFlags.isDeclaration);
	AstIndex condition;
	AstNodes thenItems; // can be empty
	AstNodes elseItems; // can be empty
	AstIndex next; // Next static if. Used during expansion
	AstIndex prev; // Prev static if. Used during expansion
	uint arrayIndex; // Index into AstNodes of the parent node, where items are to be inserted
}

void print_static_if(StaticIfDeclNode* node, ref AstPrintState state)
{
	state.print("#IF");
	print_ast(node.condition, state);
	state.print("#THEN");
	print_ast(node.thenItems, state);
	state.print("#ELSE");
	print_ast(node.elseItems, state);
}

void post_clone_static_if(StaticIfDeclNode* node, ref CloneState state)
{
	state.fixAstIndex(node.condition);
	state.fixAstNodes(node.thenItems);
	state.fixAstNodes(node.elseItems);
}

void name_register_nested_static_if(AstIndex nodeIndex, StaticIfDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.condition, state);
	node.state = AstNodeState.name_register_nested_done;
}
