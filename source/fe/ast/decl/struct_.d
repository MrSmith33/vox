/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.struct_;

import all;

struct StructDeclNode {
	mixin ScopeDeclNodeData!(AstType.decl_struct, AstFlags.isType);
	Identifier id;
	IrIndex irType;
	Scope* _scope;
	uint size = 1;
	uint alignment = 1;

	this(TokenIndex loc, Array!(AstNode*) members, Identifier id, bool _isOpaque)
	{
		this.loc = loc;
		this.astType = AstType.decl_struct;
		this.flags = AstFlags.isScope | AstFlags.isDeclaration | AstFlags.isType;
		this.declarations = members;
		this.id = id;
		if (_isOpaque) flags |= AstFlags.user1;
	}

	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	bool isOpaque() { return cast(bool)(flags & AstFlags.user1); }
}
