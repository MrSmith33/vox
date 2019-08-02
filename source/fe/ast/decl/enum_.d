/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.enum_;

import all;

struct EnumDeclaration
{
	mixin ScopeDeclNodeData!(AstType.decl_enum);
	TypeNode* memberType;
	Scope* _scope;
	Identifier id;
	TypeNode* typeNode() { return cast(TypeNode*)&this; }

	this(TokenIndex loc, Array!(AstNode*) members, TypeNode* memberType, Identifier id)
	{
		this.loc = loc;
		this.astType = AstType.decl_enum;
		this.flags = AstFlags.isScope | AstFlags.isDeclaration;
		this.declarations = members;
		this.memberType = memberType;
		this.id = id;
	}

	/// Anonymous
	this(TokenIndex loc, Array!(AstNode*) members, TypeNode* memberType)
	{
		this.loc = loc;
		this.astType = AstType.decl_enum;
		this.flags = AstFlags.isScope | AstFlags.isDeclaration | AstFlags.user1;
		this.declarations = members;
		this.memberType = memberType;
	}

	bool isAnonymous() { return cast(bool)(flags & AstFlags.user1); }
}

void name_register_enum(EnumDeclaration* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	if (node.isAnonymous)
	{
		foreach (decl; node.declarations) require_name_register(decl, state);
	}
	else
	{
		state.insert(node.id, node.as_node);
		node._scope = state.pushScope(state.context.idString(node.id), No.ordered);
		foreach (decl; node.declarations) require_name_register(decl, state);
		state.popScope;
	}
	node.state = AstNodeState.name_register_done;
}

void name_resolve_enum(EnumDeclaration* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	if (node.isAnonymous)
	{
		foreach (decl; node.declarations) require_name_resolve(decl, state);
	}
	else
	{
		state.pushScope(node._scope);
		foreach (decl; node.declarations) require_name_resolve(decl, state);
		state.popScope;
	}
	node.state = AstNodeState.name_resolve_done;
}


struct EnumMemberDecl
{
	mixin AstNodeData!(AstType.decl_enum_member, AstFlags.isDeclaration | AstFlags.isStatement);
	TypeNode* type;
	ExpressionNode* initializer;
	Identifier id;
	ushort scopeIndex;
}

void name_register_enum_member(EnumMemberDecl* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	state.insert(node.id, node.as_node);
	if (node.initializer) require_name_register(node.initializer.as_node, state);
	node.state = AstNodeState.name_register_done;
}

void name_resolve_enum_member(EnumMemberDecl* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.type.as_node, state);
	if (node.initializer) require_name_resolve(node.initializer.as_node, state);
	node.state = AstNodeState.name_resolve_done;
}
