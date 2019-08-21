/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.struct_;

import all;

struct StructDeclNode {
	mixin ScopeDeclNodeData!(AstType.decl_struct, AstFlags.isType);
	Identifier id;
	IrIndex irType;
	AstIndex _scope;
	uint size = 1;
	uint alignment = 1;

	private enum Flags
	{
		isOpaque = AstFlags.userFlag
	}

	this(TokenIndex loc, Array!AstIndex members, Identifier id, bool _isOpaque)
	{
		this.loc = loc;
		this.astType = AstType.decl_struct;
		this.flags = AstFlags.isScope | AstFlags.isDeclaration | AstFlags.isType;
		this.declarations = members;
		this.id = id;
		if (_isOpaque) flags |= Flags.isOpaque;
	}

	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	bool isOpaque() { return cast(bool)(flags & Flags.isOpaque); }
}

void name_register_struct(AstIndex nodeIndex, StructDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	state.insert(node.id, nodeIndex);
	node._scope = state.pushScope(state.context.idString(node.id), No.ordered);
	foreach (decl; node.declarations) require_name_register(decl, state);
	state.popScope;
	node.state = AstNodeState.name_register_done;
}

void name_resolve_struct(StructDeclNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	state.pushScope(node._scope);
	foreach (decl; node.declarations) require_name_resolve(decl, state);
	state.popScope;
	node.state = AstNodeState.name_resolve_done;
}

IrIndex gen_ir_type_struct(StructDeclNode* s, CompilationContext* context)
	out(res; res.isTypeStruct, "Not a struct type")
{
	if (s.irType.isDefined) return s.irType;

	uint numFields = 0;
	foreach(AstIndex memberIndex; s.declarations)
	{
		AstNode* member = context.getAstNode(memberIndex);
		if (member.astType == AstType.decl_var)
			++numFields;
	}

	s.irType = context.types.appendStruct(numFields);
	IrTypeStruct* structType = &context.types.get!IrTypeStruct(s.irType);
	IrTypeStructMember[] members = structType.members;

	uint memberIndex;
	uint memberOffset;
	uint maxAlignment = 1;
	foreach(AstIndex memberAstIndex; s.declarations)
	{
		AstNode* member = context.getAstNode(memberAstIndex);
		if (member.astType == AstType.decl_var)
		{
			IrIndex type = (cast(VariableDeclNode*)member).type.gen_ir_type(context);
			uint memberSize = context.types.typeSize(type);
			uint memberAlignment = context.types.typeAlignment(type);
			maxAlignment = max(maxAlignment, memberAlignment);
			memberOffset = alignValue!uint(memberOffset, memberAlignment);
			members[memberIndex++] = IrTypeStructMember(type, memberOffset);
			memberOffset += memberSize;
		}
	}

	memberOffset = alignValue!uint(memberOffset, maxAlignment);
	structType.size = memberOffset;
	structType.alignment = maxAlignment;
	return s.irType;
}

void type_check_struct(StructDeclNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	foreach (decl; node.declarations) require_type_check(decl, state);
	node.state = AstNodeState.type_check_done;
}
