/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.index;

import all;


@(AstType.expr_index)
struct IndexExprNode {
	mixin ExpressionNodeData!(AstType.expr_index);
	AstIndex array;
	AstNodes indicies;
}

void print_index(IndexExprNode* node, ref AstPrintState state)
{
	state.print("INDEX");
	print_ast(node.array, state);
	print_ast(node.indicies, state);
}

void post_clone_index(IndexExprNode* node, ref CloneState state)
{
	state.fixAstIndex(node.array);
	state.fixAstNodes(node.indicies);
}

void name_register_nested_index(IndexExprNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.array, state);
	require_name_register(node.indicies, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_index(ref AstIndex nodeIndex, IndexExprNode* node, ref NameResolveState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.name_resolve;
	AstIndex arrayCopy = node.array; // link to node before it replaces itself
	require_name_resolve(node.array, state);
	require_name_resolve(node.indicies, state);

	AstIndex effective_array = node.array.get_effective_node(c);

	if (effective_array.isType(c))
	{
		// convert to static array type inplace

		// if this fires allocate new node instead of repurposing this one
		static assert(IndexExprNode.sizeof >= StaticArrayTypeNode.sizeof, "IndexExprNode.sizeof < StaticArrayTypeNode.sizeof");
		static assert(IndexExprNode.sizeof >= SliceTypeNode.sizeof, "IndexExprNode.sizeof < SliceTypeNode.sizeof");
		IndexExprNode copy = *node;
		if (copy.indicies.length == 0)
		{
			// in the future this can also be slice expression
			auto sliceType = cast(SliceTypeNode*)node;
			*sliceType = SliceTypeNode(copy.loc, CommonAstNodes.type_type, copy.array);
		}
		else if (copy.indicies.length == 1)
		{
			auto arrayType = cast(StaticArrayTypeNode*)node;
			*arrayType = StaticArrayTypeNode(copy.loc, CommonAstNodes.type_type, copy.array, copy.indicies[0]);
		}
		else
		{
			c.error(node.loc, "Invalid number of indicies: %s", copy.indicies.length);
			node.type = CommonAstNodes.type_error;
		}
	}
	else if (effective_array.astType(c) == AstType.decl_template)
	{
		// must be template instantiation. Copy isType
		if (effective_array.get!TemplateDeclNode(c).body.isType(c))
			node.flags |= AstFlags.isType;
	}
	else if (effective_array.astType(c) == AstType.decl_alias_array && node.indicies.length == 1)
	{
		if (arrayCopy.astType(c) == AstType.expr_name_use)
		{
			auto nameUse = arrayCopy.get!NameUseExprNode(c);
			// replace current node with aliased entity
			// reuse name_use
			IrIndex indexVal = eval_static_expr(node.indicies[0], c);
			AstIndex item = effective_array.get!AliasArrayDeclNode(c).items[c.constants.get(indexVal).i64];
			if (item.get_effective_node(c).isType(c))
				nameUse.flags |= AstFlags.isType;
			nameUse.resolve(item, c);
			nodeIndex = arrayCopy;
		}
	}
	node.state = AstNodeState.name_resolve_done;
}

void type_check_index(ref AstIndex nodeIndex, IndexExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	scope(exit) node.state = AstNodeState.type_check_done;

	node.array.flags(c) |= AstFlags.isLvalue;
	require_type_check(node.array, state);
	require_type_check(node.indicies, state);

	AstIndex effective_array = node.array.get_effective_node(c);
	AstType array_ast_type = effective_array.astType(c);

	if (array_ast_type == AstType.decl_template)
	{
		// template instantiation
		nodeIndex = get_template_instance(effective_array, node.loc, node.indicies, state);
	}
	else
	{
		if (node.indicies.length != 1)
		{
			c.error(node.loc, "Array indexing only supports single index, not %s", node.indicies.length);
			if (node.indicies.length < 1) return;
			// if > 1 continue with 0-th index
		}

		// array/ptr/slice indexing
		autoconvTo(node.indicies[0], CommonAstNodes.type_i64, c);
		switch (node.array.get_expr_type(c).astType(c)) with(AstType)
		{
			case type_ptr, type_static_array, type_slice: break; // valid
			default: c.internal_error("Cannot index value of type `%s`", node.array.get_expr_type(c).printer(c));
		}
		node.type = node.array.get_expr_type(c).get_type(c).getElementType(c);
	}
}

ExprValue ir_gen_index(ref IrGenState gen, IrIndex curBlock, ref IrLabel nextStmt, IndexExprNode* node)
{
	CompilationContext* c = gen.context;

	c.assertf(node.indicies.length == 1, "%s", node.indicies.length);

	IrLabel afterRight = IrLabel(curBlock);
	ExprValue indexLvalue = ir_gen_expr(gen, node.indicies[0], curBlock, afterRight);
	curBlock = afterRight.blockIndex;
	IrIndex indexRvalue = indexLvalue.rvalue(gen, node.loc, curBlock);

	IrLabel afterIndex = IrLabel(curBlock);
	ExprValue arrayLvalue = ir_gen_expr(gen, node.array, curBlock, afterIndex);
	curBlock = afterIndex.blockIndex;

	IrIndex aggregateIndex = c.constants.ZERO;
	IrIndex slicePtrIndex = c.constants.ONE;

	ExpressionNode* arrayExpr = node.array.get_expr(c);
	switch (arrayExpr.type.get_type(c).astType) with(AstType)
	{
		case type_ptr:
			IrIndex ptrRvalue = arrayLvalue.rvalue(gen, node.loc, curBlock);
			IrIndex result = buildGEP(gen, node.loc, curBlock, ptrRvalue, indexRvalue);
			gen.builder.addJumpToLabel(curBlock, nextStmt);
			return ExprValue(result, ExprValueKind.ptr_to_data, IsLvalue.yes);

		case type_static_array:
			ExprValue result = arrayLvalue.member(gen, node.loc, curBlock, indexRvalue);
			gen.builder.addJumpToLabel(curBlock, nextStmt);
			return result;

		case type_slice:
			ExprValue ptrLvalue = arrayLvalue.member(gen, node.loc, curBlock, slicePtrIndex);
			IrIndex ptr = ptrLvalue.rvalue(gen, node.loc, curBlock);
			IrIndex result = buildGEP(gen, node.loc, curBlock, ptr, indexRvalue);
			gen.builder.addJumpToLabel(curBlock, nextStmt);
			return ExprValue(result, ExprValueKind.ptr_to_data, IsLvalue.yes);

		default:
			c.internal_error("Cannot index %s", arrayExpr.type.printer(c));
			assert(false);
	}
}
