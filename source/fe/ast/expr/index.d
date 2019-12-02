/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.index;

import all;


@(AstType.expr_index)
struct IndexExprNode {
	mixin ExpressionNodeData!(AstType.expr_index);
	AstIndex array;
	AstIndex index;
	IrIndex _padding;
}

void name_register_nested_index(IndexExprNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.array, state);
	require_name_register(node.index, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_index(IndexExprNode* node, ref NameResolveState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.array, state);
	require_name_resolve(node.index, state);

	if (node.array.isType(c))
	{
		// convert to static array type inplace

		// if this fires allocate new node instead of repurposing this one
		static assert(IndexExprNode.sizeof == StaticArrayTypeNode.sizeof, "IndexExprNode.sizeof != StaticArrayTypeNode.sizeof");

		IndexExprNode copy = *node;
		auto arrayType = cast(StaticArrayTypeNode*)node;
		*arrayType = StaticArrayTypeNode(copy.loc, copy.array, copy.index);
	}
	node.state = AstNodeState.name_resolve_done;
}

void type_check_index(IndexExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	node.array.flags(c) |= AstFlags.isLvalue;
	require_type_check(node.array, state);
	require_type_check(node.index, state);
	autoconvTo(node.index, c.basicTypeNodes(BasicType.t_i64), c);
	switch (node.array.expr_type(c).astType(c)) with(AstType)
	{
		case type_ptr, type_static_array, type_slice: break; // valid
		default: c.internal_error("Cannot index value of type `%s`", node.array.expr_type(c).printer(c));
	}
	node.type = node.array.expr_type(c).get_type(c).getElementType(c);
	node.state = AstNodeState.type_check_done;
}

ExprValue ir_gen_index(ref IrGenState gen, IrIndex curBlock, ref IrLabel nextStmt, IndexExprNode* node)
{
	CompilationContext* c = gen.context;
	ExpressionNode* arrayExpr = node.array.get_expr(c);
	ExpressionNode* indexExpr = node.index.get_expr(c);

	IrLabel afterRight = IrLabel(curBlock);
	ExprValue indexLvalue = ir_gen_expr(gen, node.index, curBlock, afterRight);
	curBlock = afterRight.blockIndex;
	IrIndex indexRvalue = getRvalue(gen, node.loc, curBlock, indexLvalue);

	IrLabel afterIndex = IrLabel(curBlock);
	ExprValue arrayLvalue = ir_gen_expr(gen, node.array, curBlock, afterIndex);
	curBlock = afterIndex.blockIndex;

	IrIndex aggregateIndex = c.constants.ZERO;
	IrIndex slicePtrIndex = c.constants.ONE;

	switch (arrayExpr.type.get_type(c).astType) with(AstType)
	{
		case type_ptr:
			IrIndex ptrRvalue = getRvalue(gen, node.loc, curBlock, arrayLvalue);
			IrIndex result = buildGEP(gen, node.loc, curBlock, ptrRvalue, indexRvalue);
			gen.builder.addJumpToLabel(curBlock, nextStmt);
			return ExprValue(result, ExprValueKind.ptr_to_data, IsLvalue.yes);

		case type_static_array:
			ExprValue result = getAggregateMember(gen, node.loc, curBlock, arrayLvalue, indexRvalue);
			gen.builder.addJumpToLabel(curBlock, nextStmt);
			return result;

		case type_slice:
			ExprValue ptrLvalue = getAggregateMember(gen, node.loc, curBlock, arrayLvalue, slicePtrIndex);
			IrIndex ptr = getRvalue(gen, node.loc, curBlock, ptrLvalue);
			IrIndex result = buildGEP(gen, node.loc, curBlock, ptr, indexRvalue);
			gen.builder.addJumpToLabel(curBlock, nextStmt);
			return ExprValue(result, ExprValueKind.ptr_to_data, IsLvalue.yes);

		default:
			c.internal_error("Cannot index %s", arrayExpr.type.printer(c));
			assert(false);
	}
}
