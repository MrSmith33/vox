/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.type.ptr;

import vox.all;

@(AstType.type_ptr)
struct PtrTypeNode {
	mixin AstNodeData!(AstType.type_ptr, AstFlags.isType, AstNodeState.name_register_self_done);
	AstIndex type = CommonAstNodes.type_type;
	TypeNode* typeNode() return { return cast(TypeNode*)&this; }
	AstIndex base;
	IrIndex irType;
	enum sizealign = SizeAndAlignment(POINTER_SIZE, POINTER_ALIGN_POW);
	bool isVoidPtr(CompilationContext* context) {
		return context.getAstType(base).isVoid;
	}

	IrIndex gen_init_value(CompilationContext* c)
	{
		return c.constants.addZeroConstant(gen_ir_type_ptr(&this, c));
	}
}

void print_ptr(PtrTypeNode* node, ref AstPrintState state)
{
	state.print("TYPE ", node.typeNode.printer(state.context));
}

void post_clone_ptr(PtrTypeNode* node, ref CloneState state)
{
	state.fixAstIndex(node.base);
}

void name_register_nested_ptr(PtrTypeNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.base, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_ptr(PtrTypeNode* node, ref NameResolveState state)
{
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.base, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_ptr(PtrTypeNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.base, state);
	node.state = AstNodeState.type_check_done;
}

bool same_type_ptr(PtrTypeNode* t1, PtrTypeNode* t2, CompilationContext* context)
{
	return same_type(t1.base, t2.base, context);
}

CommonTypeResult common_type_ptr(PtrTypeNode* node, AstIndex typeBIndex, CompilationContext* c)
{
	TypeNode* typeB = typeBIndex.get_type(c);
	switch(typeB.astType) with(AstType)
	{
		case type_basic:
			BasicType basicB = typeB.as_basic.basicType;
			if (basicB == BasicType.t_null) {
				return CommonTypeResult(c.getAstNodeIndex(node), TypeConvResKind.no_i, TypeConvResKind.override_expr_type_i);
			}
			goto default;
		case type_ptr:
			if (typeB.as_ptr.base == CommonAstNodes.type_void) {
				return CommonTypeResult(c.getAstNodeIndex(node), TypeConvResKind.ii_i, TypeConvResKind.no_i);
			}
			goto default;
		default: return CommonTypeResult(CommonAstNodes.type_error);
	}
}

TypeConvResKind type_conv_ptr(PtrTypeNode* node, AstIndex typeBIndex, ref AstIndex expr, CompilationContext* c)
{
	TypeNode* typeB = typeBIndex.get_type(c);
	switch(typeB.astType) with(AstType) {
		case type_basic:
			auto toBasic = typeB.as_basic.basicType;
			if (toBasic.isInteger) return TypeConvResKind.ii_e;
			return TypeConvResKind.fail;
		case type_ptr:
			if (typeB.as_ptr.base == CommonAstNodes.type_void) return TypeConvResKind.ii_i;
			return TypeConvResKind.ii_e;
		default: return TypeConvResKind.fail;
	}
}

IrIndex gen_ir_type_ptr(PtrTypeNode* t, CompilationContext* context)
	out(res; res.isTypePointer, "Not a pointer type")
{
	if (t.irType.isDefined) return t.irType;
	IrIndex baseType = t.base.gen_ir_type(context, AllowHeaderOnly.yes);
	t.irType = context.types.appendPtr(baseType);
	return t.irType;
}
