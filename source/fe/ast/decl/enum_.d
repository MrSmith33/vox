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

struct EnumMemberDecl
{
	mixin AstNodeData!(AstType.decl_enum_member, AstFlags.isDeclaration | AstFlags.isStatement);
	TypeNode* type;
	ExpressionNode* initializer;
	Identifier id;
	ushort scopeIndex;
}
