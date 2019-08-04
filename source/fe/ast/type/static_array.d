/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.type.static_array;

import all;

struct StaticArrayTypeNode {
	mixin AstNodeData!(AstType.type_static_array, AstFlags.isType, AstNodeState.name_register_done);
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	TypeNode* base;
	ExpressionNode* length_expr;
	uint length;
	IrIndex irType;
	uint size() { return cast(uint)(base.size * length); } // TODO check overflow
	uint alignment() { return base.alignment; }
}

void name_resolve_static_array(StaticArrayTypeNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.base.as_node, state);
	require_name_resolve(node.length_expr.as_node, state);
	node.state = AstNodeState.name_resolve_done;
}

bool same_type_static_array(StaticArrayTypeNode* t1, StaticArrayTypeNode* t2)
{
	return same_type(t1.base, t2.base) && (t1.length == t2.length);
}

IrIndex gen_ir_type_static_array(StaticArrayTypeNode* t, CompilationContext* context)
	out(res; res.isTypeArray, "Not a array type")
{
	if (t.irType.isDefined) return t.irType;
	t.irType = context.types.appendArray(t.base.gen_ir_type(context), t.length);
	return t.irType;
}
