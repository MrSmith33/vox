/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.type.slice;

import all;

@(AstType.type_slice)
struct SliceTypeNode {
	mixin AstNodeData!(AstType.type_slice, AstFlags.isType, AstNodeState.name_register_self_done);
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	AstIndex base;
	IrIndex irType;

	uint size() { return POINTER_SIZE * 2; }
	uint alignment() { return POINTER_SIZE; }
}

void name_register_nested_slice(SliceTypeNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.base, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_slice(SliceTypeNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.base, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_slice(SliceTypeNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.base, state);
	node.state = AstNodeState.type_check_done;
}

bool same_type_slice(SliceTypeNode* t1, SliceTypeNode* t2, CompilationContext* context)
{
	return same_type(t1.base, t2.base, context);
}

// slice is lowered into struct with two members
IrIndex gen_ir_type_slice(SliceTypeNode* t, CompilationContext* context)
	out(res; res.isTypeStruct, "Not a struct type")
{
	if (t.irType.isDefined) return t.irType;

	t.irType = context.types.appendStruct(2);
	IrTypeStruct* structType = &context.types.get!IrTypeStruct(t.irType);
	IrIndex baseType = t.base.gen_ir_type(context);
	// length
	structType.members[0] = IrTypeStructMember(makeBasicTypeIndex(IrValueType.i64), 0);
	// ptr
	structType.members[1] = IrTypeStructMember(context.types.appendPtr(baseType), POINTER_SIZE);
	structType.size = t.size;
	structType.alignment = POINTER_SIZE;
	return t.irType;
}
