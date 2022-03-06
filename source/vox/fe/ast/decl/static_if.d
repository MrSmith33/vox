/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.decl.static_if;

import vox.all;


mixin template ConditionalDeclNodeData(AstType _astType, int default_flags = 0, AstNodeState _init_state = AstNodeState.parse_done) {
	mixin AstNodeData!(_astType, default_flags, _init_state);
	AstIndex next; // Next conditional declaration. Used during expansion
	AstIndex prev; // Prev conditional declaration. Used during expansion
	uint arrayIndex; // Index into AstNodes of the parent node, where items are to be inserted
}

// Abstract node, must not be instantiated
struct ConditionalDeclNode
{
	mixin ConditionalDeclNodeData!(AstType.abstract_node);
}


@(AstType.decl_static_if)
struct StaticIfDeclNode
{
	mixin ConditionalDeclNodeData!(AstType.decl_static_if);
	AstIndex condition;
	AstNodes thenItems; // can be empty
	AstNodes elseItems; // can be empty
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


@(AstType.decl_static_version)
struct StaticVersionDeclNode
{
	mixin ConditionalDeclNodeData!(AstType.decl_static_version);
	Identifier versionId;
	AstNodes thenItems; // can be empty
	AstNodes elseItems; // can be empty
}

void print_static_version(StaticVersionDeclNode* node, ref AstPrintState state)
{
	state.print("#VERSION", state.context.idString(node.versionId));
	state.print("#THEN");
	print_ast(node.thenItems, state);
	state.print("#ELSE");
	print_ast(node.elseItems, state);
}

void post_clone_static_version(StaticVersionDeclNode* node, ref CloneState state)
{
	state.fixAstNodes(node.thenItems);
	state.fixAstNodes(node.elseItems);
}
