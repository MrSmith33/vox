/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.type.static_array;

import all;

@(AstType.type_static_array)
struct StaticArrayTypeNode {
	mixin AstNodeData!(AstType.type_static_array, AstFlags.isType, AstNodeState.name_register_self_done);
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	AstIndex base;
	AstIndex length_expr;
	uint length;
	IrIndex irType;
	uint size(CompilationContext* context) { return cast(uint)(base.typeSize(context) * length); } // TODO check overflow
	uint alignment(CompilationContext* context) { return base.typeAlignment(context); }
}

void post_clone_static_array(StaticArrayTypeNode* node, ref CloneState state)
{
	state.fixAstIndex(node.base);
	state.fixAstIndex(node.length_expr);
}

void name_register_nested_static_array(StaticArrayTypeNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.base, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_static_array(StaticArrayTypeNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.base, state);
	require_name_resolve(node.length_expr, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_static_array(StaticArrayTypeNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	require_type_check(node.base, state);
	IrIndex val = eval_static_expr(node.length_expr, c);
	node.length = c.constants.get(val).i64.to!uint;
	node.state = AstNodeState.type_check_done;
}

bool same_type_static_array(StaticArrayTypeNode* t1, StaticArrayTypeNode* t2, CompilationContext* context)
{
	return (t1.length == t2.length) && same_type(t1.base, t2.base, context);
}

IrIndex gen_ir_type_static_array(StaticArrayTypeNode* t, CompilationContext* context)
	out(res; res.isTypeArray, "Not a array type")
{
	if (t.irType.isDefined) return t.irType;
	t.irType = context.types.appendArray(t.base.gen_ir_type(context), t.length);
	return t.irType;
}
