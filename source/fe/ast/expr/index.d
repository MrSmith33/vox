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

void ir_gen_index(ref IrGenState gen, IrIndex currentBlock, ref IrLabel nextStmt, IndexExprNode* i)
{
	CompilationContext* c = gen.context;
	ExpressionNode* arrayExpr = i.array.get_expr(c);
	ExpressionNode* indexExpr = i.index.get_expr(c);

	IrLabel afterIndex = IrLabel(currentBlock);
	arrayExpr.flags |= AstFlags.isLvalue;
	ir_gen_expr(gen, i.array, currentBlock, afterIndex);
	currentBlock = afterIndex.blockIndex;

	IrLabel afterRight = IrLabel(currentBlock);
	ir_gen_expr(gen, i.index, currentBlock, afterRight);
	currentBlock = afterRight.blockIndex;

	IrIndex aggregateIndex = c.constants.ZERO;
	IrIndex slicePtrIndex = c.constants.ONE;

	switch (arrayExpr.type.get_type(c).astType) with(AstType)
	{
		case type_ptr:
			i.irValue = buildGEP(gen, i.loc, currentBlock, arrayExpr.irValue, indexExpr.irValue);
			break;
		case type_static_array:
			IrIndex type = gen.ir.getValueType(c, arrayExpr.irValue);
			if (type.isTypePointer)
				i.irValue = buildGEP(gen, i.loc, currentBlock, arrayExpr.irValue, aggregateIndex, indexExpr.irValue);
			else {
				c.assertf(type.isTypeArray, "%s", IrIndexDump(type, c, gen.ir));
				i.irValue = buildGEP(gen, i.loc, currentBlock, arrayExpr.irValue, indexExpr.irValue);
			}
			break;
		case type_slice:
			IrIndex ptrPtr = buildGEP(gen, i.loc, currentBlock, arrayExpr.irValue, aggregateIndex, slicePtrIndex);
			IrIndex ptr = load(gen, currentBlock, ptrPtr);
			i.irValue = buildGEP(gen, i.loc, currentBlock, ptr, indexExpr.irValue);
			break;
		default:
			c.internal_error("Cannot index %s", arrayExpr.type.printer(c));
			break;
	}

	if (!i.isLvalue) {
		i.irValue = load(gen, currentBlock, i.irValue);
	}

	gen.builder.addJumpToLabel(currentBlock, nextStmt);
}
