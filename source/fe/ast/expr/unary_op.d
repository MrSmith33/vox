/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.unary_op;

import all;


@(AstType.expr_un_op)
struct UnaryExprNode {
	mixin ExpressionNodeData!(AstType.expr_un_op);
	UnOp op;
	AstIndex child;
}

void print_unary_op(UnaryExprNode* node, ref AstPrintState state)
{
	if (node.type) state.print("UNOP ", node.type.printer(state.context), " ", node.op);
	else state.print("UNOP ", node.op);
	print_ast(node.child, state);
}

void post_clone_unary_op(UnaryExprNode* node, ref CloneState state)
{
	state.fixAstIndex(node.child);
}

void name_register_nested_unary_op(UnaryExprNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.child, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_unary_op(UnaryExprNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.child, state);
	switch(node.op) with(UnOp)
	{
		case addrOf:
			AstNode* child = node.child.get_node(state.context);
			child.flags |= AstFlags.isLvalue;
			if (child.astType == AstType.expr_name_use)
			{
				child.flags |= NameUseFlags.forbidParenthesesFreeCall;
			}
			break;
		default:
			break;
	}
	node.state = AstNodeState.name_resolve_done;
}

void type_check_unary_op(UnaryExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	require_type_check(node.child, state);
	ExpressionNode* child = node.child.get_expr(c);
	assert(child.type, format("child(%s).type: is null", child.astType));

	if (child.type.isErrorType)
	{
		node.type = child.type;
		node.state = AstNodeState.type_check_done;
		return;
	}

	switch(node.op) with(UnOp)
	{
		case addrOf:
			// make sure that variable gets stored in memory
			//writefln("addrOf %s", child.astType);
			switch(child.astType)
			{
				case AstType.expr_name_use:
					AstNode* entity = child.as!NameUseExprNode(c).entity.get_node(c);

					switch (entity.astType)
					{
						case AstType.decl_var:
							// TODO: fix test 205. We do not detect all cases where pointer of local var is taken
							// For example taking address of some member is not detected, only direct address obtaining.
							entity.flags |= VariableFlags.isAddressTaken; // mark variable
							node.type = c.appendAst!PtrTypeNode(node.child.loc(c), CommonAstNodes.type_type, node.child.get_expr_type(c));
							break;
						case AstType.decl_function:
							node.type = c.appendAst!PtrTypeNode(node.child.loc(c), CommonAstNodes.type_type, node.child.get_expr_type(c));
							break;
						default:
							c.internal_error(node.loc, "Cannot take address of %s", entity.astType);
					}
					break;
				case AstType.expr_index:
					node.type = c.appendAst!PtrTypeNode(node.child.loc(c), CommonAstNodes.type_type, node.child.get_expr_type(c));
					break;
				case AstType.expr_member:
					node.type = c.appendAst!PtrTypeNode(node.child.loc(c), CommonAstNodes.type_type, node.child.get_expr_type(c));
					break;
				default:
					c.internal_error(node.loc, "Cannot take address of %s", child.astType);
			}
			break;
		case bitwiseNot:
			node.type = child.type;
			break;
		case logicalNot:
			autoconvToBool(node.child, c);
			node.type = CommonAstNodes.type_bool;
			break;
		case minus:
			node.type = child.type;
			break;
		case preIncrement, postIncrement, preDecrement, postDecrement:
			child.flags |= AstFlags.isLvalue;
			node.type = child.type;
			break;
		case deref:
			if (child.type.isErrorType) {
				node.type = child.type;
				break;
			}
			if (!child.type.isPointerType(c)) {
				c.unrecoverable_error(node.loc, "Cannot dereference %s", child.type.printer(c));
			}
			node.type = child.type.get_type(c).as_ptr.base;
			break;
		default:
			c.internal_error("un op %s not implemented", node.op);
			node.type = node.child.get_expr_type(c);
	}
	node.state = AstNodeState.type_check_done;
}

ExprValue ir_gen_expr_unary_op(ref IrGenState gen, IrIndex currentBlock, ref IrLabel nextStmt, UnaryExprNode* u)
{
	CompilationContext* c = gen.context;

	switch(u.op) with(UnOp)
	{
		case addrOf:
			IrLabel afterChild = IrLabel(currentBlock);
			ExprValue lval = ir_gen_expr(gen, u.child, currentBlock, afterChild);
			currentBlock = afterChild.blockIndex;
			ExprValue res = lval.addrOf(gen, u.loc, currentBlock);
			gen.builder.addJumpToLabel(currentBlock, nextStmt);
			return res;

		case bitwiseNot:
			IrLabel afterChild = IrLabel(currentBlock);
			ExprValue lval = ir_gen_expr(gen, u.child, currentBlock, afterChild);
			currentBlock = afterChild.blockIndex;
			IrIndex rval = lval.rvalue(gen, u.loc, currentBlock);
			ExtraInstrArgs extra = {type : u.type.gen_ir_type(c), argSize : u.type.typeArgSize(c) };
			IrIndex result = gen.builder.emitInstr!(IrOpcode.not)(currentBlock, extra, rval).result;
			gen.builder.addJumpToLabel(currentBlock, nextStmt);
			return ExprValue(result);

		case logicalNot:
			IrLabel afterChild = IrLabel(currentBlock);
			IrIndex result = makeBoolValue(gen, cast(ExpressionNode*)u, currentBlock, afterChild);
			currentBlock = afterChild.blockIndex;
			gen.builder.addJumpToLabel(currentBlock, nextStmt);
			return ExprValue(result);

		case minus:
			IrLabel afterChild = IrLabel(currentBlock);
			ExprValue lval = ir_gen_expr(gen, u.child, currentBlock, afterChild);
			currentBlock = afterChild.blockIndex;
			IrIndex rval = lval.rvalue(gen, u.loc, currentBlock);
			ExtraInstrArgs extra = {type : u.type.gen_ir_type(c), argSize : u.type.typeArgSize(c) };
			IrIndex result = gen.builder.emitInstr!(IrOpcode.neg)(currentBlock, extra, rval).result;
			gen.builder.addJumpToLabel(currentBlock, nextStmt);
			return ExprValue(result);

		case preIncrement, postIncrement, preDecrement, postDecrement:
			IrOpcode opcode = IrOpcode.sub;
			if (u.op == preIncrement || u.op == postIncrement) opcode = IrOpcode.add;

			ExpressionNode* childExpr = u.child.get_expr(c);

			IrLabel afterChild = IrLabel(currentBlock);
			ExprValue lval = ir_gen_expr(gen, u.child, currentBlock, afterChild);
			currentBlock = afterChild.blockIndex;

			IrIndex increment = c.constants.ONE; // integers increment by 1
			TypeNode* childType = childExpr.type.get_type(c);
			if (childType.isPointer) { // pointers increment by size of element
				uint size = childType.as_ptr.base.require_type_size(c).size;
				increment = c.constants.add(size, IsSigned.no);
			}

			IrArgSize argSize = childType.argSize(c);
			IrIndex rval = lval.rvalue(gen, u.loc, currentBlock);
			ExtraInstrArgs extra = {
				opcode : opcode,
				type : childType.gen_ir_type(c),
				argSize : argSize
			};
			IrIndex opResult = gen.builder.emitInstr!(IrOpcode.generic_binary)(
				currentBlock, extra, rval, increment).result;
			lval.store(gen, u.loc, currentBlock, opResult);

			gen.builder.addJumpToLabel(currentBlock, nextStmt);

			if (u.op == preIncrement || u.op == preDecrement)
				return ExprValue(opResult);
			else
				return ExprValue(rval);

		case deref:
			IrLabel afterChild = IrLabel(currentBlock);
			ExprValue lval = ir_gen_expr(gen, u.child, currentBlock, afterChild);
			currentBlock = afterChild.blockIndex;
			ExprValue res = lval.deref(gen, u.loc, currentBlock);
			gen.builder.addJumpToLabel(currentBlock, nextStmt);
			return res;

		case staticArrayToSlice:
			IrLabel afterChild = IrLabel(currentBlock);
			ExpressionNode* childExpr = u.child.get_expr(c);
			ExprValue lval = ir_gen_expr(gen, u.child, currentBlock, afterChild);
			currentBlock = afterChild.blockIndex;

			// pointer to first element
			IrIndex ptr = buildGEPEx(gen, u.loc, currentBlock, lval, c.constants.ZERO, c.constants.ZERO);
			// array length
			IrIndex length = c.constants.add(childExpr.type.get_type(c).as_static_array.length, IsSigned.no, IrArgSize.size64);

			// combine into slice {u64, T*}
			IrIndex resType = u.type.gen_ir_type(c);
			ExtraInstrArgs extra = { type : resType };
			InstrWithResult res = gen.builder.emitInstr!(IrOpcode.create_aggregate)(currentBlock, extra, length, ptr);
			return ExprValue(res.result);

		default:
			c.internal_error(u.loc, "un op %s not implemented", u.op);
			assert(false);
	}
}

void ir_gen_branch_unary_op(ref IrGenState gen, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit, UnaryExprNode* u)
{
	CompilationContext* c = gen.context;
	switch(u.op) with(UnOp)
	{
		case logicalNot:
			ir_gen_branch(gen, u.child, currentBlock, falseExit, trueExit);
			break;

		default: c.internal_error(u.loc, "Opcode `%s` is not implemented", u.op); break;
	}
}

enum UnOp : ubyte {
	plus, // +
	minus, // -
	logicalNot, // !
	bitwiseNot, // ~
	deref, // *
	addrOf, // &
	preIncrement, // ++x
	preDecrement, // --x
	postIncrement, // x++
	postDecrement, // x--
	staticArrayToSlice,
}

IrIndex calcUnOp(UnOp op, IrIndex child, IrArgSize argSize, CompilationContext* c)
{
	IrConstant childCon = c.constants.get(child);

	switch(op)
	{
		case UnOp.bitwiseNot:
			return c.constants.add(~childCon.i64, cast(IsSigned)child.isSignedConstant, argSize);

		default:
			c.internal_error("Opcode `%s` is not implemented", op);
			assert(false);
	}
}
