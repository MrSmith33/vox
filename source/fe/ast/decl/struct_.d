/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.struct_;

import all;

enum StructFlags
{
	isOpaque = AstFlags.userFlag
}

@(AstType.decl_struct)
struct StructDeclNode {
	mixin ScopeDeclNodeData!(AstType.decl_struct, AstFlags.isType);
	AstIndex parentScope;
	AstIndex memberScope;
	Identifier id;
	IrIndex irType;
	IrIndex defaultVal;

	this(TokenIndex loc, AstIndex parentScope, AstIndex memberScope, Identifier id)
	{
		this.loc = loc;
		this.astType = AstType.decl_struct;
		this.flags = AstFlags.isScope | AstFlags.isDeclaration | AstFlags.isType;
		this.parentScope = parentScope;
		this.memberScope = memberScope;
		this.id = id;
	}

	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	bool isOpaque() { return cast(bool)(flags & StructFlags.isOpaque); }
	uint size(CompilationContext* c) {
		c.assertf(state >= AstNodeState.type_check_done, loc, "size is unknown in %s state. Must be semantically analized", state);
		IrTypeStruct* structType = &c.types.get!IrTypeStruct(irType);
		return structType.size;
	}
	uint alignment(CompilationContext* c) {
		c.assertf(state >= AstNodeState.type_check_done, loc, "alignment is unknown in %s state. Must be semantically analized", state);
		IrTypeStruct* structType = &c.types.get!IrTypeStruct(irType);
		return structType.alignment;
	}
}

void print_struct(StructDeclNode* node, ref AstPrintState state)
{
	state.print("STRUCT ", state.context.idString(node.id));
	print_ast(node.declarations, state);
}

void post_clone_struct(StructDeclNode* node, ref CloneState state)
{
	state.fixScope(node.parentScope);
	state.fixScope(node.memberScope);
	state.fixAstNodes(node.declarations);
}

void name_register_self_struct(AstIndex nodeIndex, StructDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_self;
	node.parentScope.insert_scope(node.id, nodeIndex, state.context);
	node.state = AstNodeState.name_register_self_done;
}

void name_register_nested_struct(AstIndex nodeIndex, StructDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.declarations, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_struct(StructDeclNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.declarations, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_struct(StructDeclNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.declarations, state);
	gen_ir_type_struct(node, state.context);
	node.state = AstNodeState.type_check_done;
}

IrIndex gen_default_value_struct(StructDeclNode* node, CompilationContext* c)
{
	if (node.defaultVal.isDefined) return node.defaultVal;

	IrIndex structType = node.gen_ir_type_struct(c);
	uint numStructMembers = c.types.get!IrTypeStruct(structType).numMembers;
	IrIndex[] args = c.allocateTempArray!IrIndex(numStructMembers);
	scope(exit) c.freeTempArray(args);

	uint memberIndex;
	bool allZeroes = true;
	foreach(AstIndex member; node.declarations)
	{
		AstNode* memberVarNode = member.get_node(c);
		if (memberVarNode.astType != AstType.decl_var) continue;
		VariableDeclNode* memberVar = memberVarNode.as!VariableDeclNode(c);
		IrIndex memberValue = memberVar.gen_default_value_var(c);
		args[memberIndex] = memberValue;
		++memberIndex;

		if (!memberValue.isConstantZero) allZeroes = false;
	}
	if (allZeroes)
		node.defaultVal = c.constants.addZeroConstant(structType);
	else
		node.defaultVal = c.constants.addAggrecateConstant(structType, args);

	return node.defaultVal;
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
