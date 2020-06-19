/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.alias_;

import all;

/// Aliases are super simple in implementation
/// Since they can only be used by name (via NameUseExprNode node),
/// we simply replace NameUseExprNode with aliased entity when name is resolved
@(AstType.decl_alias)
struct AliasDeclNode
{
	mixin AstNodeData!(AstType.decl_alias, AstFlags.isDeclaration | AstFlags.isStatement);
	AstIndex parentScope;
	Identifier id;
	AstIndex initializer;
}

void print_alias(AliasDeclNode* node, ref AstPrintState state)
{
	state.print("ALIAS ", state.context.idString(node.id));
	print_ast(node.initializer, state);
}

void post_clone_alias(AliasDeclNode* node, ref CloneState state)
{
	state.fixScope(node.parentScope);
	state.fixAstIndex(node.initializer);
}

void name_register_self_alias(AstIndex nodeIndex, AliasDeclNode* node, ref NameRegisterState state)
{
	node.state = AstNodeState.name_register_self;
	node.parentScope.insert_scope(node.id, nodeIndex, state.context);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_alias(AliasDeclNode* node, ref NameResolveState state)
{
	CompilationContext* c = state.context;
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.initializer, state);
	AstNode* initializer = node.initializer.get_node(c);
	if (initializer.isType) {
		// ok
	} else if (initializer.astType == AstType.expr_name_use) {
		// ok
		initializer.flags |= NameUseFlags.forbidParenthesesFreeCall;
	}
	else if (initializer.astType == AstType.expr_call)
	{
		// CTFE function that returns $alias
		node.initializer = eval_static_expr_alias(node.initializer, state.context);
		initializer = node.initializer.get_node(c);
	}
	else
	{
		c.error(node.loc, "Cannot create alias of %s", get_node_kind_name(node.initializer, c));
	}
	if (initializer.isType)
		node.flags |= AstFlags.isType;
	node.state = AstNodeState.name_resolve_done;
}

void type_check_alias(AliasDeclNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	// user of NameUseExprNode will require type check of initializer
	node.state = AstNodeState.type_check_done;
}


@(AstType.decl_alias_array)
struct AliasArrayDeclNode {
	mixin AstNodeData!(AstType.decl_alias_array, 0, AstNodeState.type_check_done);
	AstNodes items;
}

void print_alias_array(AliasArrayDeclNode* node, ref AstPrintState state)
{
	state.print("alias array ", node.items);
}
