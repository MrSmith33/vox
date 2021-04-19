/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.static_foreach;

import all;


@(AstType.decl_static_foreach)
struct StaticForeachDeclNode
{
	mixin ConditionalDeclNodeData!(AstType.decl_static_foreach);
	AstIndex parentScope;
	Identifier keyId; // optional
	Identifier valueId;
	/// What will be iterated
	AstIndex iterableExpr;
	/// statements / declaration
	/// has at least 1 item
	AstNodes body;
	/// Points to the first index that needs to be copied
	AstIndex body_start;
	/// Points to the next index after body data
	AstIndex after_body;
}

void print_static_foreach(StaticForeachDeclNode* node, ref AstPrintState state)
{
	state.print("#FOREACH");
	print_ast(node.iterableExpr, state);
	print_ast(node.body, state);
}

void post_clone_static_foreach(StaticForeachDeclNode* node, ref CloneState state)
{
	state.fixScope(node.parentScope);
	state.fixAstIndex(node.iterableExpr);
	state.fixAstNodes(node.body);

	node.body_start.storageIndex += state.offset;
	node.after_body.storageIndex += state.offset;
}
