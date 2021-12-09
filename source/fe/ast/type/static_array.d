/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.type.static_array;

import all;

@(AstType.type_static_array)
struct StaticArrayTypeNode {
	mixin AstNodeData!(AstType.type_static_array, AstFlags.isType, AstNodeState.name_register_self_done);
	AstIndex type = CommonAstNodes.type_type;
	TypeNode* typeNode() return { return cast(TypeNode*)&this; }
	AstIndex base;
	AstIndex length_expr;
	uint length;
	IrIndex irType;
	IrIndex defaultVal;
	SizeAndAlignment sizealign(CompilationContext* c) {
		gen_ir_type_static_array(&this, c);
		return c.types.typeSizeAndAlignment(irType);
	}
}

void print_static_array(StaticArrayTypeNode* node, ref AstPrintState state)
{
	state.print("TYPE ", node.typeNode.printer(state.context));
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
	calc_length_static_array(node, c);
	node.state = AstNodeState.type_check_done;
}

TypeConvResKind type_conv_static_array(StaticArrayTypeNode* node, AstIndex typeBIndex, ref AstIndex expr, CompilationContext* c)
{
	TypeNode* typeB = typeBIndex.get_type(c);
	switch(typeB.astType) with(AstType)
	{
		case type_slice:
			if (same_type(node.base, typeB.as_slice.base, c)) {
				return TypeConvResKind.array_literal_to_slice;
			}
			return TypeConvResKind.fail;
		default: return TypeConvResKind.fail;
	}
}

bool same_type_static_array(StaticArrayTypeNode* t1, StaticArrayTypeNode* t2, CompilationContext* context)
{
	return (t1.length == t2.length) && same_type(t1.base, t2.base, context);
}

IrIndex gen_init_value_static_array(StaticArrayTypeNode* node, CompilationContext* c)
{
	if (node.defaultVal.isDefined) return node.defaultVal;

	IrIndex arrayType = node.gen_ir_type_static_array(c);
	uint numElements = c.types.get!IrTypeArray(arrayType).numElements;

	IrIndex elemDefault = node.base.get_type(c).gen_init_value(c);

	if (elemDefault.isConstantZero)
	{
		node.defaultVal = c.constants.addZeroConstant(arrayType);
	}
	else
	{
		node.defaultVal = c.constants.addAggrecateConstant(arrayType, numElements);
		IrAggregateConstant* agg = &c.constants.getAggregate(node.defaultVal);
		agg.members[] = elemDefault;
	}

	return node.defaultVal;
}

uint calc_length_static_array(StaticArrayTypeNode* node, CompilationContext* c)
{
	final switch(node.getPropertyState(NodeProperty.init_value)) {
		case PropertyState.not_calculated: break;
		case PropertyState.calculating: c.circular_dependency;
		case PropertyState.calculated: return node.length;
	}

	c.begin_node_property_calculation(node, NodeProperty.init_value);
	scope(exit) c.end_node_property_calculation(node, NodeProperty.init_value);

	IrIndex val = eval_static_expr(node.length_expr, c);
	node.length = c.constants.get(val).i64.to!uint;

	return node.length;
}

IrIndex gen_ir_type_static_array(StaticArrayTypeNode* node, CompilationContext* c)
	out(res; res.isTypeArray, "Not a array type")
{
	final switch(node.getPropertyState(NodeProperty.ir_body)) {
		case PropertyState.not_calculated: break;
		case PropertyState.calculating: c.circular_dependency;
		case PropertyState.calculated: return node.irType;
	}

	// dependencies
	uint array_length = calc_length_static_array(node, c);
	IrIndex baseType = node.base.gen_ir_type(c);

	c.begin_node_property_calculation(node, NodeProperty.ir_body);
	scope(exit) c.end_node_property_calculation(node, NodeProperty.ir_body);

	node.irType = c.types.appendArray(baseType, array_length);

	return node.irType;
}
