/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.expr.slice;

import vox.all;


@(AstType.expr_slice)
struct SliceExprNode {
	mixin ExpressionNodeData!(AstType.expr_slice);
	AstIndex array;
	AstIndex fromIndex;
	AstIndex toIndex;
}

void print_expr_slice(SliceExprNode* node, ref AstPrintState state)
{
	state.print("SLICE");
	print_ast(node.array, state);
	print_ast(node.fromIndex, state);
	print_ast(node.toIndex, state);
}

void post_clone_expr_slice(SliceExprNode* node, ref CloneState state)
{
	state.fixAstIndex(node.array);
	state.fixAstIndex(node.fromIndex);
	state.fixAstIndex(node.toIndex);
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
	autoconvTo(node.fromIndex, CommonAstNodes.type_i64, c);
	require_type_check(node.toIndex, state);
	autoconvTo(node.toIndex, CommonAstNodes.type_i64, c);
	switch (node.array.get_expr_type(c).astType(c)) with(AstType)
	{
		case type_ptr, type_static_array:
			AstIndex elemType = node.array.get_expr_type(c).get_type(c).getElementType(c);
			node.type = c.appendAst!SliceTypeNode(node.loc, CommonAstNodes.type_type, elemType);
			break;
		case type_slice:
			node.type = node.array.get_expr_type(c);
			break;
		default: c.internal_error("Cannot slice value of type `%s`", node.array.get_expr_type(c).printer(c));
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
	IrIndex fromIndexRvalue = fromIndexLvalue.rvalue(gen, node.loc, curBlock);

	IrLabel afterTo = IrLabel(curBlock);
	ExprValue toIndexLvalue = ir_gen_expr(gen, node.toIndex, curBlock, afterTo);
	curBlock = afterTo.blockIndex;
	IrIndex toIndexRvalue = toIndexLvalue.rvalue(gen, node.loc, curBlock);

	IrLabel afterArray = IrLabel(curBlock);
	ExprValue arrayLvalue = ir_gen_expr(gen, node.array, curBlock, afterArray);
	curBlock = afterArray.blockIndex;

	IrIndex aggregateIndex = c.constants.addZeroConstant(makeIrType(IrBasicType.i32));
	IrIndex slicePtrIndex = c.constants.add(makeIrType(IrBasicType.i32), 1);

	AstType astType = arrayExpr.type.get_type(c).astType;
	IrIndex ptr; // pointer to first element
	IrIndex length; // slice length

	if (fromIndexRvalue.isSimpleConstant && c.constants.get(fromIndexRvalue).i64 == 0)
	{
		// special case for array[0..to];
		ptr = arrayLvalue.irValue;
		switch (astType) with(AstType)
		{
			case type_ptr:
				// read pointer variable for T*
				ptr = arrayLvalue.rvalue(gen, node.loc, curBlock);
				break;
			case type_static_array:
				// need to convert [n x T]* into T* for static arrays
				IrIndex ZERO = c.constants.addZeroConstant(makeIrType(IrBasicType.i32));
				ptr = buildGEP(gen, node.loc, curBlock, ptr, ZERO, ZERO);
				break;
			case type_slice:
				ExprValue ptrLvalue = arrayLvalue.member(gen, node.loc, curBlock, slicePtrIndex);
				ptr = ptrLvalue.rvalue(gen, node.loc, curBlock);
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
				IrIndex ZERO = c.constants.addZeroConstant(makeIrType(IrBasicType.i32));
				ptr = buildGEP(gen, node.loc, curBlock, arrayLvalue.irValue, ZERO, fromIndexRvalue);
				break;
			case type_slice:
				ExprValue ptrLvalue = arrayLvalue.member(gen, node.loc, curBlock, slicePtrIndex);
				ptr = ptrLvalue.rvalue(gen, node.loc, curBlock);
				ptr = buildGEP(gen, node.loc, curBlock, ptr, fromIndexRvalue);
				break;
			default: assert(false);
		}

		ExtraInstrArgs extra1 = {
			type : makeIrType(IrBasicType.i64),
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
