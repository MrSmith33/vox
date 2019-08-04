/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.type.ptr;

import all;

struct PtrTypeNode {
	mixin AstNodeData!(AstType.type_ptr, AstFlags.isType, AstNodeState.name_register_done);
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	TypeNode* base;
	IrIndex irType;
	uint size() { return POINTER_SIZE; }
	uint alignment() { return POINTER_SIZE; }
	bool isVoidPtr() { return base.isVoid; }
}

void name_resolve_ptr(PtrTypeNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.base.as_node, state);
	node.state = AstNodeState.name_resolve_done;
}

bool same_type_ptr(PtrTypeNode* t1, PtrTypeNode* t2)
{
	return same_type(t1.base, t2.base);
}

IrIndex gen_ir_type_ptr(PtrTypeNode* t, CompilationContext* context)
	out(res; res.isTypePointer, "Not a pointer type")
{
	if (t.irType.isDefined) return t.irType;
	t.irType = context.types.appendPtr(t.base.gen_ir_type(context));
	return t.irType;
}
