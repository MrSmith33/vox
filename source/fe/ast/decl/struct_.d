/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.struct_;

import all;

enum TypeFlags : ushort
{
	type_size_mask      = 1 << 0 | 1 << 1, // used for reading value
	size_not_calculated = 0 << 1,          // used for setting flags
	size_is_calculating = 1 << 1,          // used for setting flags
	size_is_calculated  = 2 << 1,          // used for setting flags

	userFlag            = 1 << 2,
}

enum StructFlags : ushort
{
	isOpaque   = TypeFlags.userFlag << 0,
	// Set if struct contains meta type member variables or methods
	isCtfeOnly = TypeFlags.userFlag << 1,
	isUnion    = TypeFlags.userFlag << 2,
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
		this.flags = AstFlags.isType;
		this.parentScope = parentScope;
		this.memberScope = memberScope;
		this.id = id;
	}

	TypeNode* typeNode() return { return cast(TypeNode*)&this; }
	bool isOpaque() { return cast(bool)(flags & StructFlags.isOpaque); }
	bool isCtfeOnly() { return cast(bool)(flags & StructFlags.isCtfeOnly); }
	bool isUnion() { return cast(bool)(flags & StructFlags.isUnion); }
	SizeAndAlignment sizealign(CompilationContext* c) {
		c.assertf(state >= AstNodeState.type_check_done, loc, "size is unknown in %s state. Must be semantically analized", state);
		IrTypeStruct* structType = &c.types.get!IrTypeStruct(irType);
		return structType.sizealign;
	}
}

void print_struct(StructDeclNode* node, ref AstPrintState state)
{
	state.print(node.isUnion ? "UNION " : "STRUCT ", state.context.idString(node.id), node.isCtfeOnly ? " #ctfe" : null);
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

TypeConvResKind type_conv_struct(StructDeclNode* node, AstIndex typeBIndex, ref AstIndex expr, CompilationContext* c)
{
	TypeNode* typeB = typeBIndex.get_type(c);

	switch(typeB.astType) with(AstType)
	{
		case decl_enum:
			if (c.getAstNodeIndex(node) == typeB.as_enum.memberType.get_node_type(c))
				return TypeConvResKind.no_e;
			goto default;
		default: return TypeConvResKind.fail;
	}
}

IrIndex gen_init_value_struct(StructDeclNode* node, CompilationContext* c)
{
	if (node.defaultVal.isDefined) return node.defaultVal;

	IrIndex structType = node.gen_ir_type_struct(c);
	uint numStructMembers = c.types.get!IrTypeStruct(structType).numMembers;
	uint numArgSlots = node.isUnion ? 2 : numStructMembers;
	IrIndex[] args = c.allocateTempArray!IrIndex(numArgSlots);
	scope(exit) c.freeTempArray(args);

	uint memberIndex;
	bool allZeroes = true;
	foreach(AstIndex member; node.declarations)
	{
		AstNode* memberVarNode = member.get_node(c);
		if (memberVarNode.astType != AstType.decl_var) continue;
		VariableDeclNode* memberVar = memberVarNode.as!VariableDeclNode(c);
		IrIndex memberValue = memberVar.gen_init_value_var(c);
		if (!memberValue.isConstantZero) allZeroes = false;
		if (node.isUnion) {
			args[0] = c.constants.ZERO; // member index
			args[1] = memberValue; // value
			break;
		}
		args[memberIndex] = memberValue;
		++memberIndex;
	}
	if (allZeroes)
		node.defaultVal = c.constants.addZeroConstant(structType);
	else
		node.defaultVal = c.constants.addAggrecateConstant(structType, args);

	return node.defaultVal;
}

IrIndex gen_ir_type_struct(StructDeclNode* s, CompilationContext* c)
	out(res; res.isTypeStruct, "Not a struct type")
{
	if (s.irType.isDefined) return s.irType;

	uint numFields = 0;
	foreach(AstIndex memberIndex; s.declarations)
	{
		AstNode* member = c.getAstNode(memberIndex);
		if (member.astType == AstType.decl_var)
			++numFields;
	}

	s.irType = c.types.appendStruct(numFields, s.isUnion);
	IrTypeStruct* structType = &c.types.get!IrTypeStruct(s.irType);
	IrTypeStructMember[] members = structType.members;

	uint memberIndex;
	uint memberOffset;
	uint maxMemberSize;
	ubyte maxAlignmentPower = 0;
	foreach(AstIndex memberAstIndex; s.declarations)
	{
		AstNode* member = c.getAstNode(memberAstIndex);
		if (member.astType == AstType.decl_var)
		{
			auto var = member.as!(VariableDeclNode)(c);
			IrIndex type = var.type.gen_ir_type(c);
			SizeAndAlignment memberInfo = c.types.typeSizeAndAlignment(type);
			maxAlignmentPower = max(maxAlignmentPower, memberInfo.alignmentPower);
			memberOffset = alignValue!uint(memberOffset, 1 << memberInfo.alignmentPower);
			if (s.isUnion)
				members[memberIndex++] = IrTypeStructMember(type, 0);
			else
				members[memberIndex++] = IrTypeStructMember(type, memberOffset);
			memberOffset += memberInfo.size;
			maxMemberSize = max(maxMemberSize, memberInfo.size);

			if (var.type.isMetaType(c)) {
				s.flags |= StructFlags.isCtfeOnly;
			}
		} else if (member.astType == AstType.decl_function) {
			if (member.as!(FunctionDeclNode)(c).isCtfeOnly(c)) {
				s.flags |= StructFlags.isCtfeOnly;
			}
		}
	}

	memberOffset = alignValue!uint(memberOffset, 1 << maxAlignmentPower);
	if (s.isUnion)
		structType.sizealign = SizeAndAlignment(maxMemberSize, maxAlignmentPower);
	else
		structType.sizealign = SizeAndAlignment(memberOffset, maxAlignmentPower);
	return s.irType;
}
