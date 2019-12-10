/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.slice;

import all;


@(AstType.expr_slice)
struct SliceExprNode {
	mixin ExpressionNodeData!(AstType.expr_slice);
	AstIndex array;
	AstIndex fromIndex;
	AstIndex toIndex;
}

void name_register_nested_expr_slice(SliceExprNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.array, state);
	require_name_register(node.fromIndex, state);
	require_name_register(node.toIndex, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_expr_slice(SliceExprNode* node, ref NameResolveState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.array, state);
	require_name_resolve(node.fromIndex, state);
	require_name_resolve(node.toIndex, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_expr_slice(SliceExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	node.array.flags(c) |= AstFlags.isLvalue;
	require_type_check(node.array, state);
	require_type_check(node.fromIndex, state);
	autoconvTo(node.fromIndex, c.basicTypeNodes(BasicType.t_i64), c);
	require_type_check(node.toIndex, state);
	autoconvTo(node.toIndex, c.basicTypeNodes(BasicType.t_i64), c);
	switch (node.array.expr_type(c).astType(c)) with(AstType)
	{
		case type_ptr, type_static_array:
			AstIndex elemType = node.array.expr_type(c).get_type(c).getElementType(c);
			node.type = c.appendAst!SliceTypeNode(node.loc, elemType);
			break;
		case type_slice:
			node.type = node.array.expr_type(c);
			break;
		default: c.internal_error("Cannot slice value of type `%s`", node.array.expr_type(c).printer(c));
	}
	node.state = AstNodeState.type_check_done;
}

ExprValue ir_gen_expr_slice(ref IrGenState gen, IrIndex curBlock, ref IrLabel nextStmt, SliceExprNode* node)
{
	CompilationContext* c = gen.context;
	ExpressionNode* arrayExpr = node.array.get_expr(c);

	IrLabel afterFrom = IrLabel(curBlock);
	ExprValue fromIndexLvalue = ir_gen_expr(gen, node.fromIndex, curBlock, afterFrom);
	curBlock = afterFrom.blockIndex;
	IrIndex fromIndexRvalue = getRvalue(gen, node.loc, curBlock, fromIndexLvalue);

	IrLabel afterTo = IrLabel(curBlock);
	ExprValue toIndexLvalue = ir_gen_expr(gen, node.toIndex, curBlock, afterTo);
	curBlock = afterTo.blockIndex;
	IrIndex toIndexRvalue = getRvalue(gen, node.loc, curBlock, toIndexLvalue);

	IrLabel afterArray = IrLabel(curBlock);
	ExprValue arrayLvalue = ir_gen_expr(gen, node.array, curBlock, afterArray);
	curBlock = afterArray.blockIndex;

	IrIndex aggregateIndex = c.constants.ZERO;
	IrIndex slicePtrIndex = c.constants.ONE;

	AstType astType = arrayExpr.type.get_type(c).astType;
	IrIndex ptr; // pointer to first element
	IrIndex length; // slice length
	if (fromIndexRvalue.isConstant && c.constants.get(fromIndexRvalue).i64 == 0)
	{
		// special case for array[0..to];
		ptr = arrayLvalue.irValue;
		switch (astType) with(AstType)
		{
			case type_ptr:
				// read pointer variable for T*
				if (ptr.isVariable) ptr = gen.builder.readVariable(curBlock, ptr);
				break;
			case type_static_array:
				// need to convert [n x T]* into T* for static arrays
				ptr = buildGEP(gen, node.loc, curBlock, arrayLvalue.irValue, c.constants.ZERO, c.constants.ZERO);
				break;
			case type_slice:
				ExprValue ptrLvalue = getAggregateMember(gen, node.loc, curBlock, arrayLvalue, slicePtrIndex);
				ptr = getRvalue(gen, node.loc, curBlock, ptrLvalue);
				break;
			default: assert(false);
		}
		length = toIndexRvalue;
	}
	else
	{
		switch (astType) with(AstType)
		{
			case type_ptr:
				ptr = buildGEP(gen, node.loc, curBlock, arrayLvalue.irValue, fromIndexRvalue);
				break;
			case type_static_array:
				ptr = buildGEP(gen, node.loc, curBlock, arrayLvalue.irValue, c.constants.ZERO, fromIndexRvalue);
				break;
			case type_slice:
				ExprValue ptrLvalue = getAggregateMember(gen, node.loc, curBlock, arrayLvalue, slicePtrIndex);
				ptr = getRvalue(gen, node.loc, curBlock, ptrLvalue);
				ptr = buildGEP(gen, node.loc, curBlock, ptr, fromIndexRvalue);
				break;
			default: assert(false);
		}

		ExtraInstrArgs extra1 = {
			type : makeBasicTypeIndex(IrValueType.i64),
			argSize : IrArgSize.size64
		};
		length = gen.builder.emitInstr!(IrOpcode.sub)(curBlock, extra1, toIndexRvalue, fromIndexRvalue).result;
	}
	//writefln("len %s, ptr %s", length, ptr);
	// combine into slice {u64, T*}
	IrIndex resType = node.type.gen_ir_type(c);
	ExtraInstrArgs extra = { type : resType };
	InstrWithResult res = gen.builder.emitInstr!(IrOpcode.create_aggregate)(curBlock, extra, length, ptr);
	gen.builder.addJumpToLabel(curBlock, nextStmt);
	return ExprValue(res.result);
}
