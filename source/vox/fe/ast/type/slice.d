/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.type.slice;

import vox.all;

@(AstType.type_slice)
struct SliceTypeNode {
	mixin AstNodeData!(AstType.type_slice, AstFlags.isType, AstNodeState.name_register_self_done);
	AstIndex type = CommonAstNodes.type_type;
	TypeNode* typeNode() return { return cast(TypeNode*)&this; }
	AstIndex base;
	IrIndex irType;
	IrIndex defaultVal;

	enum sizealign = SizeAndAlignment(POINTER_SIZE * 2, POINTER_ALIGN_POW);
}

void print_slice(SliceTypeNode* node, ref AstPrintState state)
{
	state.print("TYPE ", node.typeNode.printer(state.context));
}

void post_clone_slice(SliceTypeNode* node, ref CloneState state)
{
	state.fixAstIndex(node.base);
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
	CompilationContext* c = state.context;
	node.state = AstNodeState.type_check;
	require_type_check(node.base, state);
	if (!node.base.isType(c))
		c.error(node.loc, "Slice base type is not a type, it is %s", node.base.astType(c));

	node.state = AstNodeState.type_check_done;
}

bool same_type_slice(SliceTypeNode* t1, SliceTypeNode* t2, CompilationContext* context)
{
	return same_type(t1.base, t2.base, context);
}

IrIndex gen_init_value_slice(SliceTypeNode* node, CompilationContext* c)
{
	if (node.defaultVal.isDefined) return node.defaultVal;
	node.defaultVal = c.constants.addZeroConstant(gen_ir_type_slice(node, c));
	return node.defaultVal;
}

// slice is lowered into struct with two members
IrIndex gen_ir_type_slice(SliceTypeNode* t, CompilationContext* context)
	out(res; res.isTypeStruct, "Not a struct type")
{
	if (t.irType.isDefined) return t.irType;

	t.irType = context.types.appendStruct(2);
	IrTypeStruct* structType = &context.types.get!IrTypeStruct(t.irType);
	IrIndex baseType = t.base.gen_ir_type(context, AllowHeaderOnly.yes);
	// length
	structType.members[0] = IrTypeStructMember(makeIrType(IrBasicType.i64), 0);
	// ptr
	structType.members[1] = IrTypeStructMember(context.types.appendPtr(baseType), POINTER_SIZE);
	structType.sizealign = t.sizealign;
	return t.irType;
}

TypeConvResKind type_conv_slice(SliceTypeNode* node, AstIndex typeBIndex, ref AstIndex expr, CompilationContext* c)
{
	TypeNode* typeB = typeBIndex.get_type(c);

	switch(typeB.astType) with(AstType) {
		case type_ptr:
			if (expr.astType(c) == AstType.literal_string) {
				if (typeB.astType == AstType.type_ptr) {
					TypeNode* ptrBaseType = typeB.as_ptr.base.get_type(c);
					if (ptrBaseType.astType == AstType.type_basic &&
						ptrBaseType.as_basic.basicType == BasicType.t_u8)
					{
						return TypeConvResKind.string_literal_to_u8_ptr;
					}
				}
			}
			return TypeConvResKind.fail;
		default: return TypeConvResKind.fail;
	}
}
