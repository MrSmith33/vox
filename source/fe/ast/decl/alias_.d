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
	Identifier id;
	AstIndex initializer;
}

void name_register_alias(AstIndex nodeIndex, AliasDeclNode* node, ref NameRegisterState state)
{
	node.state = AstNodeState.name_register;
	state.insert(node.id, nodeIndex);
	node.state = AstNodeState.name_register_done;
}

void name_resolve_alias(AliasDeclNode* node, ref NameResolveState state)
{
	CompilationContext* c = state.context;
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.initializer, state);
	AstNode* initializer = node.initializer.get_node(c);
	if (initializer.isType || initializer.astType == AstType.expr_name_use)
	{
		// ok
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
