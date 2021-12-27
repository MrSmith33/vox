/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.index;

import all;


@(AstType.expr_index)
struct IndexExprNode {
	mixin ExpressionNodeData!(AstType.expr_index);
	ScopeIndex parentScope; // needed to resolve `this` pointer in member access
	AstIndex array;
	AstNodes indices;
}

void print_index(IndexExprNode* node, ref AstPrintState state)
{
	state.print("INDEX");
	print_ast(node.array, state);
	print_ast(node.indices, state);
}

void post_clone_index(IndexExprNode* node, ref CloneState state)
{
	state.fixScope(node.parentScope);
	state.fixAstIndex(node.array);
	state.fixAstNodes(node.indices);
}

void name_register_nested_index(IndexExprNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.array, state);
	require_name_register(node.indices, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_index(ref AstIndex nodeIndex, IndexExprNode* node, ref NameResolveState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.name_resolve;
	AstIndex arrayCopy = node.array; // link to node before it replaces itself
	require_name_resolve(node.array, state);
	require_name_resolve(node.indices, state);

	AstIndex effective_array = node.array.get_effective_node(c);

	if (effective_array.isType(c))
	{
		// convert to static array type inplace

		// if this fires allocate new node instead of repurposing this one
		static assert(IndexExprNode.sizeof >= StaticArrayTypeNode.sizeof, "IndexExprNode.sizeof < StaticArrayTypeNode.sizeof");
		static assert(IndexExprNode.sizeof >= SliceTypeNode.sizeof, "IndexExprNode.sizeof < SliceTypeNode.sizeof");
		IndexExprNode copy = *node;
		if (copy.indices.length == 0)
		{
			// in the future this can also be slice expression
			auto sliceType = cast(SliceTypeNode*)node;
			*sliceType = SliceTypeNode(copy.loc, CommonAstNodes.type_type, copy.array);
		}
		else if (copy.indices.length == 1)
		{
			auto arrayType = cast(StaticArrayTypeNode*)node;
			*arrayType = StaticArrayTypeNode(copy.loc, CommonAstNodes.type_type, copy.array, copy.indices[0]);
		}
		else
		{
			c.error(node.loc, "Invalid number of indices: %s", copy.indices.length);
			node.type = CommonAstNodes.type_error;
		}
	}
	else if (effective_array.astType(c) == AstType.decl_template)
	{
		// must be template instantiation. Copy isType
		if (effective_array.get!TemplateDeclNode(c).body.isType(c))
			node.flags |= AstFlags.isType;
	}
	else if (effective_array.astType(c) == AstType.decl_alias_array && node.indices.length == 1)
	{
		if (arrayCopy.astType(c) == AstType.expr_name_use)
		{
			auto nameUse = arrayCopy.get!NameUseExprNode(c);
			// replace current node with aliased entity
			// reuse name_use
			IrIndex indexVal = eval_static_expr(node.indices[0], c);
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

	AstIndex effective_array = node.array.get_effective_node(c);
	AstType array_ast_type = effective_array.astType(c);

	if (array_ast_type == AstType.decl_template)
	{
		require_type_check(node.indices, state);
		// template instantiation
		nodeIndex = get_template_instance(effective_array, node.loc, node.indices, state);
		if (nodeIndex == CommonAstNodes.node_error) {
			node.type = CommonAstNodes.type_error;
			return;
		}

		if (nodeIndex.astType(c) == AstType.decl_function)
		{
			nodeIndex = c.appendAst!CallExprNode(node.loc, AstIndex(), node.parentScope, nodeIndex);
			nodeIndex.setState(c, AstNodeState.name_resolve_done);
		}

		require_type_check(nodeIndex, state);
		return;
	}

	if (array_ast_type == AstType.expr_member)
	{
		// template instantiation
		MemberExprNode* memberExpr = effective_array.get!MemberExprNode(c);
		LookupResult res = lookupMember(effective_array, memberExpr, state);

		if (res == LookupResult.success) {
			AstIndex memberIndex = memberExpr.member(c);

			if (memberExpr.subType == MemberSubType.struct_templ_method) {
				// rewrite as call
				nodeIndex = c.appendAst!CallExprNode(node.loc, AstIndex(), node.parentScope, nodeIndex);
				nodeIndex.setState(c, AstNodeState.name_resolve_done);
				require_type_check(nodeIndex, state);
				return;
			}
		}
	}

	node.array.flags(c) |= AstFlags.isLvalue;
	require_type_check(node.array, state);
	require_type_check(node.indices, state);

	{
		if (node.indices.length != 1)
		{
			c.error(node.loc, "Array indexing only supports single index, not %s", node.indices.length);
			if (node.indices.length < 1) return;
			// if > 1 continue with 0-th index
		}

		// array/ptr/slice indexing
		autoconvTo(node.indices[0], CommonAstNodes.type_i64, c);
		switch (node.array.get_expr_type(c).astType(c)) with(AstType)
		{
			case type_ptr, type_static_array, type_slice:
				// valid
				node.type = node.array.get_expr_type(c).get_type(c).getElementType(c);
				break;
			default:
				node.type = CommonAstNodes.type_error;
				c.error(node.loc, "Cannot index value of type `%s`", node.array.get_expr_type(c).printer(c));
				break;
		}
	}
}

ExprValue ir_gen_index(ref IrGenState gen, IrIndex curBlock, ref IrLabel nextStmt, IndexExprNode* node)
{
	CompilationContext* c = gen.context;

	c.assertf(node.indices.length == 1, "%s", node.indices.length);

	IrLabel afterRight = IrLabel(curBlock);
	ExprValue indexLvalue = ir_gen_expr(gen, node.indices[0], curBlock, afterRight);
	curBlock = afterRight.blockIndex;
	IrIndex indexRvalue = indexLvalue.rvalue(gen, node.loc, curBlock);

	IrLabel afterIndex = IrLabel(curBlock);
	ExprValue arrayLvalue = ir_gen_expr(gen, node.array, curBlock, afterIndex);
	curBlock = afterIndex.blockIndex;

	IrIndex aggregateIndex = c.constants.addZeroConstant(makeIrType(IrBasicType.i32));
	IrIndex slicePtrIndex = c.constants.add(makeIrType(IrBasicType.i32), 1);

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
	}
}
