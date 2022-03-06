/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.expr.binary_op;

import vox.all;

enum BinaryOpFlags : ushort
{
	isAssignment = AstFlags.userFlag << 0,
}

@(AstType.expr_bin_op)
struct BinaryExprNode {
	mixin ExpressionNodeData!(AstType.expr_bin_op, 0);
	BinOp op;
	AstIndex left;
	AstIndex right;

	bool isAssignment() { return cast(bool)(flags & BinaryOpFlags.isAssignment); }
}

void print_binary_op(BinaryExprNode* node, ref AstPrintState state)
{
	if (node.type) state.print("BINOP ", node.type.printer(state.context), " ", node.op);
	else state.print("BINOP ", node.op);
	print_ast(node.left, state);
	print_ast(node.right, state);
}

void post_clone_binary_op(BinaryExprNode* node, ref CloneState state)
{
	state.fixAstIndex(node.left);
	state.fixAstIndex(node.right);
}

void name_register_nested_binary_op(BinaryExprNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.left, state);
	require_name_register(node.right, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_binary_op(BinaryExprNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.left, state);
	require_name_resolve(node.right, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_binary_op(BinaryExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	require_type_check(node.left, state);
	require_type_check(node.right, state);
	assert(node.left.get_expr_type(c), format("left(%s).type: is null", node.left.astType(c)));
	assert(node.right.get_expr_type(c), format("right(%s).type: is null", node.right.astType(c)));

	setResultType(node, c);
	TypeNode* leftType = node.left.get_expr(c).type.get_type(c);
	node.op = selectTypedOpcode(node.op, leftType.isSigned, leftType.isFloat);
	node.state = AstNodeState.type_check_done;
}

ExprValue ir_gen_expr_binary_op(ref IrGenState gen, IrIndex currentBlock, ref IrLabel nextStmt, BinaryExprNode* b)
{
	CompilationContext* c = gen.context;

	if (b.op == BinOp.LOGIC_AND || b.op == BinOp.LOGIC_OR)
	{
		IrLabel afterChild = IrLabel(currentBlock);
		IrIndex irValue = makeBoolValue(gen, cast(ExpressionNode*)b, currentBlock, afterChild);
		currentBlock = afterChild.blockIndex;
		gen.builder.addJumpToLabel(currentBlock, nextStmt);
		return ExprValue(irValue);
	}

	if (b.isAssignment)
	{
		IrLabel afterLeft = IrLabel(currentBlock);
		ExprValue leftLvalue = ir_gen_expr(gen, b.left, currentBlock, afterLeft);
		currentBlock = afterLeft.blockIndex;

		IrLabel afterRight = IrLabel(currentBlock);
		ExprValue rightLvalue = ir_gen_expr(gen, b.right, currentBlock, afterRight);
		currentBlock = afterRight.blockIndex;
		IrIndex rightRvalue = rightLvalue.rvalue(gen, b.loc, currentBlock);

		ExpressionNode* leftExpr = b.left.get_expr(c);
		ExpressionNode* rightExpr = b.right.get_expr(c);

		c.assertf(leftLvalue.irValue.isDefined, leftExpr.loc, "%s null IR val", leftExpr.astType);
		c.assertf(rightLvalue.irValue.isDefined, rightExpr.loc, "%s null IR val", rightExpr.astType);

		if (b.op == BinOp.ASSIGN) {
			leftLvalue.store(gen, b.loc, currentBlock, rightRvalue);
			gen.builder.addJumpToLabel(currentBlock, nextStmt);
			return rightLvalue;
		}
		else if (b.op == BinOp.PTR_PLUS_INT_ASSIGN)
		{
			IrIndex leftRvalue = leftLvalue.rvalue(gen, b.loc, currentBlock);
			assert(leftExpr.type.get_type(c).isPointer && rightExpr.type.get_type(c).isInteger);
			IrIndex irValue = buildGEP(gen, b.loc, currentBlock, leftRvalue, rightRvalue);
			leftLvalue.store(gen, b.loc, currentBlock, irValue);
			gen.builder.addJumpToLabel(currentBlock, nextStmt);
			return ExprValue(irValue);
		}
		else
		{
			IrIndex leftRvalue = leftLvalue.rvalue(gen, b.loc, currentBlock);
			IrIndex irValue;

			if (leftRvalue.isSimpleConstant && rightRvalue.isSimpleConstant)
			{
				irValue = calcBinOp(binOpAssignToRegularOp(b.op), leftRvalue, rightRvalue, c);
			}
			else
			{
				ExtraInstrArgs extra = {
					opcode : binOpcode(b.op, b.loc, c),
					type : leftExpr.type.gen_ir_type(c),
					argSize : leftExpr.type.typeArgSize(c)
				};
				irValue = gen.builder.emitInstr!(IrOpcode.generic_binary)(
					currentBlock, extra, leftRvalue, rightRvalue).result;
			}

			leftLvalue.store(gen, b.loc, currentBlock, irValue);
			gen.builder.addJumpToLabel(currentBlock, nextStmt);
			return ExprValue(irValue);
		}
	}
	else
	{
		IrLabel fake;
		IrIndex irValue = visitBinOpImpl!true(gen, currentBlock, nextStmt, fake, b);
		assert(fake.numPredecessors == 0);
		return ExprValue(irValue);
	}
}

void ir_gen_branch_binary_op(ref IrGenState gen, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit, BinaryExprNode* b)
{
	CompilationContext* c = gen.context;
	if (b.isAssignment)
	{
		c.error(b.loc, "Cannot assign inside condition");
	}
	if (b.op == BinOp.LOGIC_AND)
	{
		IrLabel cond2Label = IrLabel(currentBlock);
		ir_gen_branch(gen, b.left, currentBlock, cond2Label, falseExit);

		if (cond2Label.numPredecessors != 0)
		{
			IrIndex cond2Block = cond2Label.blockIndex;
			gen.builder.sealBlock(cond2Block);
			ir_gen_branch(gen, b.right, cond2Block, trueExit, falseExit);
		}

		return;
	}
	else if (b.op == BinOp.LOGIC_OR)
	{
		IrLabel cond2Label = IrLabel(currentBlock);
		ir_gen_branch(gen, b.left, currentBlock, trueExit, cond2Label);

		if (cond2Label.numPredecessors != 0)
		{
			IrIndex cond2Block = cond2Label.blockIndex;
			gen.builder.sealBlock(cond2Block);
			ir_gen_branch(gen, b.right, cond2Block, trueExit, falseExit);
		}

		return;
	}
	visitBinOpImpl!false(gen, currentBlock, trueExit, falseExit, b);
}

// In value mode only uses trueExit as nextStmt
IrIndex visitBinOpImpl(bool forValue)(ref IrGenState gen, ref IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit, BinaryExprNode* b)
{
	CompilationContext* c = gen.context;

	IrLabel afterLeft = IrLabel(currentBlock);
	ExprValue leftLvalue = ir_gen_expr(gen, b.left, currentBlock, afterLeft);
	currentBlock = afterLeft.blockIndex;

	IrLabel afterRight = IrLabel(currentBlock);
	ExprValue rightLvalue = ir_gen_expr(gen, b.right, currentBlock, afterRight);
	currentBlock = afterRight.blockIndex;

	ExpressionNode* leftExpr = b.left.get_expr(c);
	ExpressionNode* rightExpr = b.right.get_expr(c);

	c.assertf(leftLvalue.irValue.isDefined, leftExpr.loc, "%s null IR val", leftExpr.astType);
	c.assertf(rightLvalue.irValue.isDefined, rightExpr.loc, "%s null IR val", rightExpr.astType);

	auto leftValue = leftLvalue.rvalue(gen, b.loc, currentBlock);
	auto rightValue = rightLvalue.rvalue(gen, b.loc, currentBlock);

	// constant folding
	if (leftValue.isSimpleConstant && rightValue.isSimpleConstant)
	{
		IrIndex value = calcBinOp(b.op, leftValue, rightValue, c);
		static if (forValue)
		{
			c.assertf(value.isDefined, b.loc, "%s null IR val", b.astType);
			return value;
		}
		else
		{
			if (c.constants.get(value).i8)
				gen.builder.addJumpToLabel(currentBlock, trueExit);
			else
				gen.builder.addJumpToLabel(currentBlock, falseExit);
			return IrIndex();
		}
	}

	static if (forValue)
	{
		IrIndex resType = b.type.gen_ir_type(c);
		IrIndex irValue;
		switch(b.op) with(BinOp)
		{
			case EQUAL, NOT_EQUAL:
			case SGT, SGE, SLT, SLE:
			case UGT, UGE, ULT, ULE:
			case FGT, FGE, FLT, FLE:
				ExtraInstrArgs extra = { cond : convertBinOpToIrCond(b.op), type : resType };
				irValue = gen.builder.emitInstr!(IrOpcode.set_binary_cond)(currentBlock, extra, leftValue, rightValue).result;
				break;

			case PTR_PLUS_INT:
				assert(leftExpr.type.get_type(c).isPointer && rightExpr.type.get_type(c).isInteger);
				irValue = buildGEP(gen, b.loc, currentBlock, leftValue, rightValue);
				break;

			case PTR_DIFF:
				assert(leftExpr.type.get_type(c).isPointer && rightExpr.type.get_type(c).isPointer);

				ExtraInstrArgs extra = { type : resType, argSize : leftExpr.type.typeArgSize(c) };
				irValue = gen.builder.emitInstr!(IrOpcode.sub)(currentBlock, extra, leftValue, rightValue).result;

				// divide by elem size
				TypeNode* baseType = leftExpr.type.get_type(c).as_ptr.base.get_type(c);
				uint elemSize = baseType.sizealign(c).size;
				if (elemSize == 1 || baseType.isVoid) break;

				ExtraInstrArgs extra2 = { type : resType, argSize : leftExpr.type.typeArgSize(c) };
				IrIndex elemSizeValue = c.constants.add(resType, elemSize);
				irValue = gen.builder.emitInstr!(IrOpcode.sdiv)(currentBlock, extra2, irValue, elemSizeValue).result;
				break;

			case INT_PLUS, INT_MINUS, INT_SMUL, INT_UMUL, INT_SDIV, INT_UDIV, INT_SREM, INT_UREM:
			case FLT_PLUS, FLT_MINUS, FLT_MUL, FLT_DIV:
			case SHL, SHR, ASHR, XOR, BITWISE_AND, BITWISE_OR:
				ExtraInstrArgs extra = {
					opcode : binOpcode(b.op, b.loc, c),
					type : resType,
					argSize : b.type.typeArgSize(c)
				};
				irValue = gen.builder.emitInstr!(IrOpcode.generic_binary)(
					currentBlock, extra, leftValue, rightValue).result;
				break;
			default: c.internal_error(b.loc, "Opcode `%s` is not implemented", b.op);
		}
		c.assertf(irValue.isDefined, b.loc, "%s null IR val", b.astType);
		gen.builder.addJumpToLabel(currentBlock, trueExit);
		return irValue;
	}
	else // branch
	{
		switch(b.op) with(BinOp)
		{
			case EQUAL, NOT_EQUAL:
			case SGT, SGE, SLT, SLE:
			case UGT, UGE, ULT, ULE:
			case FGT, FGE, FLT, FLE:
				auto branch = gen.builder.addBinBranch(
					currentBlock, convertBinOpToIrCond(b.op), leftExpr.type.typeArgSize(c),
					leftValue, rightValue, trueExit, falseExit);
				break;
			default: c.internal_error(b.loc, "Opcode `%s` is not implemented", b.op);
		}
		return IrIndex();
	}
}

IrOpcode binOpcode(BinOp binop, TokenIndex loc, CompilationContext* context)
{
	switch(binop) with(BinOp)
	{
		case INT_PLUS, INT_PLUS_ASSIGN:   return IrOpcode.add;
		case INT_MINUS, INT_MINUS_ASSIGN: return IrOpcode.sub;
		case INT_SMUL, INT_SMUL_ASSIGN:   return IrOpcode.smul;
		case INT_UMUL, INT_UMUL_ASSIGN:   return IrOpcode.umul;
		case INT_SDIV, INT_SDIV_ASSIGN:   return IrOpcode.sdiv;
		case INT_UDIV, INT_UDIV_ASSIGN:   return IrOpcode.udiv;
		case INT_SREM, INT_SREM_ASSIGN:   return IrOpcode.srem;
		case INT_UREM, INT_UREM_ASSIGN:   return IrOpcode.urem;

		case FLT_PLUS, FLT_PLUS_ASSIGN:   return IrOpcode.fadd;
		case FLT_MINUS, FLT_MINUS_ASSIGN: return IrOpcode.fsub;
		case FLT_DIV, FLT_DIV_ASSIGN:     return IrOpcode.fdiv;
		case FLT_MUL, FLT_MUL_ASSIGN:     return IrOpcode.fmul;

		case SHL, SHL_ASSIGN: return IrOpcode.shl;
		case SHR, SHR_ASSIGN: return IrOpcode.lshr;
		case ASHR, ASHR_ASSIGN: return IrOpcode.ashr;
		case XOR, XOR_ASSIGN: return IrOpcode.xor;
		case BITWISE_AND, BITWISE_AND_ASSIGN: return IrOpcode.and;
		case BITWISE_OR, BITWISE_OR_ASSIGN: return IrOpcode.or;
		default:
			context.internal_error(loc, "assign op %s not implemented", binop);
	}
}

IrIndex calcBinOp(BinOp op, IrIndex left, IrIndex right, CompilationContext* c)
{
	c.assertf(left.isSimpleConstant, "%s is not a constant", left);
	c.assertf(right.isSimpleConstant, "%s is not a constant", right);

	IrConstant leftCon = c.constants.get(left);
	IrConstant rightCon = c.constants.get(right);

	bool is_f64() { return leftCon.type == makeIrType(IrBasicType.f64); }

	switch(op)
	{
		case BinOp.EQUAL:     return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.i64 == rightCon.i64));
		case BinOp.NOT_EQUAL: return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.i64 != rightCon.i64));

		case BinOp.SGT: return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.i64 >  rightCon.i64));
		case BinOp.SGE: return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.i64 >= rightCon.i64));
		case BinOp.SLT: return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.i64 <  rightCon.i64));
		case BinOp.SLE: return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.i64 <= rightCon.i64));

		case BinOp.UGT: return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.u64 >  rightCon.u64));
		case BinOp.UGE: return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.u64 >= rightCon.u64));
		case BinOp.ULT: return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.u64 <  rightCon.u64));
		case BinOp.ULE: return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.u64 <= rightCon.u64));

		case BinOp.FGT: if (is_f64) return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.f64 >  rightCon.f64)); return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.f32 >  rightCon.f32));
		case BinOp.FGE: if (is_f64) return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.f64 >= rightCon.f64)); return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.f32 >= rightCon.f32));
		case BinOp.FLT: if (is_f64) return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.f64 <  rightCon.f64)); return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.f32 <  rightCon.f32));
		case BinOp.FLE: if (is_f64) return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.f64 <= rightCon.f64)); return c.constants.add(makeIrType(IrBasicType.i8), cast(ubyte)(leftCon.f32 <= rightCon.f32));

		case BinOp.INT_PLUS: return c.constants.add(leftCon.type, leftCon.i64 + rightCon.i64);
		case BinOp.FLT_PLUS:
			if (is_f64) return c.constants.add(leftCon.f64 + rightCon.f64);
			return c.constants.add(leftCon.f32 + rightCon.f32);
		case BinOp.INT_MINUS:
			return c.constants.add(leftCon.type, leftCon.i64 - rightCon.i64);
		case BinOp.FLT_MINUS:
			if (is_f64) return c.constants.add(leftCon.f64 - rightCon.f64);
			return c.constants.add(leftCon.f32 - rightCon.f32);
		case BinOp.INT_SMUL: return c.constants.add(leftCon.type, leftCon.i64 * rightCon.i64);
		case BinOp.INT_UMUL: return c.constants.add(leftCon.type, leftCon.u64 * rightCon.u64);
		case BinOp.FLT_MUL:
			if (is_f64) return c.constants.add(leftCon.f64 * rightCon.f64);
			return c.constants.add(leftCon.f32 * rightCon.f32);
		case BinOp.INT_SDIV: return c.constants.add(leftCon.type, leftCon.i64 / rightCon.i64);
		case BinOp.INT_UDIV: return c.constants.add(leftCon.type, leftCon.u64 / rightCon.u64);
		case BinOp.FLT_DIV:
			if (is_f64) return c.constants.add(leftCon.f64 / rightCon.f64);
			return c.constants.add(leftCon.f32 / rightCon.f32);

		case BinOp.INT_SREM: return c.constants.add(leftCon.type, leftCon.i64 % rightCon.i64);
		case BinOp.INT_UREM: return c.constants.add(leftCon.type, leftCon.u64 % rightCon.u64);

		case BinOp.SHL:
			IrBasicType basicType = leftCon.type.basicType(c);
			ulong result;
			switch(basicType) with(IrBasicType) {
				case i8:  result = leftCon.i32 << rightCon.i64; break; // (leftCon.i8  << rightCon.i64 &    0b111)) & 0xFF; break;
				case i16: result = leftCon.i32 << rightCon.i64; break; // (leftCon.i16 << rightCon.i64 &   0b1111)) & 0xFFFF; break;
				case i32: result = leftCon.i32 << rightCon.i64; break; // (leftCon.i32 << rightCon.i64 &  0b11111)) & 0xFFFF_FFFF; break;
				case i64: result = leftCon.i64 << rightCon.i64; break; // (leftCon.i64 << rightCon.i64 & 0b111111)) & 0xFFFF_FFFF_FFFF_FFFF; break;
				default: c.internal_error("Invalid constant type %s", basicType);
			}
			return c.constants.add(leftCon.type, result);
		case BinOp.SHR:
			IrBasicType basicType = leftCon.type.basicType(c);
			ulong result;
			switch(basicType) with(IrBasicType) {
				case i8:  result = leftCon.i32 >>> rightCon.i64; break; // (leftCon.i8  >>> (rightCon.i64 &    0b111)) & 0xFF; break;
				case i16: result = leftCon.i32 >>> rightCon.i64; break; // (leftCon.i16 >>> (rightCon.i64 &   0b1111)) & 0xFFFF; break;
				case i32: result = leftCon.i32 >>> rightCon.i64; break; // (leftCon.i32 >>> (rightCon.i64 &  0b11111)) & 0xFFFF_FFFF; break;
				case i64: result = leftCon.i64 >>> rightCon.i64; break; // (leftCon.i64 >>> (rightCon.i64 & 0b111111)) & 0xFFFF_FFFF_FFFF_FFFF; break;
				default: c.internal_error("Invalid constant type %s", basicType);
			}
			return c.constants.add(leftCon.type, result);
		case BinOp.ASHR:
			IrBasicType basicType = leftCon.type.basicType(c);
			ulong result;
			switch(basicType) with(IrBasicType) {
				case i8:  result = leftCon.i32 >> rightCon.i64; break; // (leftCon.i8  >> (rightCon.i64 &    0b111)) & 0xFF; break;
				case i16: result = leftCon.i32 >> rightCon.i64; break; // (leftCon.i16 >> (rightCon.i64 &   0b1111)) & 0xFFFF; break;
				case i32: result = leftCon.i32 >> rightCon.i64; break; // (leftCon.i32 >> (rightCon.i64 &  0b11111)) & 0xFFFF_FFFF; break;
				case i64: result = leftCon.i64 >> rightCon.i64; break; // (leftCon.i64 >> (rightCon.i64 & 0b111111)) & 0xFFFF_FFFF_FFFF_FFFF; break;
				default: c.internal_error("Invalid constant type %s", basicType);
			}
			return c.constants.add(leftCon.type, result);

		case BinOp.BITWISE_OR:    return c.constants.add(leftCon.type, leftCon.i64 | rightCon.i64);
		case BinOp.BITWISE_AND:   return c.constants.add(leftCon.type, leftCon.i64 & rightCon.i64);
		case BinOp.XOR:           return c.constants.add(leftCon.type, leftCon.i64 ^ rightCon.i64);

		default: c.internal_error("Opcode `%s` is not implemented", op);
	}
}

IrBinaryCondition convertBinOpToIrCond(BinOp op)
{
	switch(op) with(BinOp) with(IrBinaryCondition)
	{
		case EQUAL:         return eq;
		case NOT_EQUAL:     return ne;
		case SGT:           return sgt;
		case SGE:           return sge;
		case SLT:           return slt;
		case SLE:           return sle;
		case UGT:           return ugt;
		case UGE:           return uge;
		case ULT:           return ult;
		case ULE:           return ule;
		case FGT:           return fgt;
		case FGE:           return fge;
		case FLT:           return flt;
		case FLE:           return fle;
		default: assert(false, "Unexpected BinOp");
	}
}

void setResultType(BinaryExprNode* b, CompilationContext* c)
{
	AstIndex resType = CommonAstNodes.type_error;
	b.type = resType;
	AstIndex leftTypeIndex = b.left.get_expr_type(c);
	TypeNode* leftType = leftTypeIndex.get_type(c);
	AstIndex rightTypeIndex = b.right.get_expr_type(c);
	TypeNode* rightType = rightTypeIndex.get_type(c);

	if (leftType.astType == AstType.decl_enum) {
		leftTypeIndex = leftType.as_enum.memberType;
		leftType = leftTypeIndex.get_type(c);
	}
	if (rightType.astType == AstType.decl_enum) {
		rightTypeIndex = rightType.as_enum.memberType;
		rightType = rightTypeIndex.get_type(c);
	}

	if (leftTypeIndex.isErrorType || rightTypeIndex.isErrorType) return;

	switch(b.op) with(BinOp)
	{
		// logic ops. Requires both operands to be bool
		case LOGIC_AND, LOGIC_OR:
			autoconvToBool(b.left, c);
			autoconvToBool(b.right, c);
			resType = CommonAstNodes.type_bool;
			break;
		// logic ops. Requires both operands to be of the same type
		case EQUAL, NOT_EQUAL, GENERIC_GREATER, GENERIC_GREATER_EQUAL, GENERIC_LESS, GENERIC_LESS_EQUAL:
			if (leftType.isPointer && rightType.isPointer)
			{
				if (
					same_type(leftTypeIndex, rightTypeIndex, c) ||
					leftType.as_ptr.isVoidPtr(c) ||
					rightType.as_ptr.isVoidPtr(c))
				{
					resType = CommonAstNodes.type_bool;
					break;
				}
			}

			if (autoconvToCommonType(b.left, b.right, c)) {
				resType = CommonAstNodes.type_bool;
			}
			else
				c.error(b.loc, "Cannot compare `%s` and `%s`",
					leftType.typeName(c),
					rightType.typeName(c));
			break;

		case GENERIC_MINUS:
			if (leftType.isPointer && rightType.isPointer) // handle ptr - ptr
			{
				if (same_type(leftTypeIndex, rightTypeIndex, c))
				{
					b.op = BinOp.PTR_DIFF;
					resType = CommonAstNodes.type_i64;
					break;
				}
				else
				{
					c.error(b.loc, "cannot subtract pointers to different types: `%s` and `%s`",
						leftType.printer(c), rightType.printer(c));
					break;
				}
			} else if (leftType.isPointer && rightType.isInteger) { // handle ptr - int
				b.op = BinOp.PTR_PLUS_INT;
				(cast(IntLiteralExprNode*)b.right.get_node(c)).negate(b.loc, *c);
				resType = leftTypeIndex;
				break;
			}
			goto case GENERIC_DIV;

		case GENERIC_PLUS:
			// handle int + ptr and ptr + int
			if (leftType.isPointer && rightType.isInteger) {
				b.op = BinOp.PTR_PLUS_INT;
				resType = leftTypeIndex;
				break;
			} else if (leftType.isInteger && rightType.isPointer) {
				b.op = BinOp.PTR_PLUS_INT;
				// canonicalize
				swap(b.left, b.right);
				resType = leftTypeIndex;
				break;
			}

			goto case GENERIC_DIV;

		// arithmetic op int float
		case GENERIC_DIV, GENERIC_MUL:
			if (autoconvToCommonType(b.left, b.right, c))
			{
				resType = b.left.get_expr_type(c);
			}
			else
			{
				c.error(b.loc, "Cannot perform `%s` %s `%s` operation",
					leftType.typeName(c), binOpStrings[b.op],
					rightType.typeName(c));
			}
			break;

		// integer only
		case GENERIC_INT_REM, SHL, SHR, ASHR, BITWISE_AND, BITWISE_OR, XOR:
			if (leftType.isInteger && rightType.isInteger && autoconvToCommonType(b.left, b.right, c))
			{
				resType = b.left.get_expr_type(c);
			}
			else
			{
				c.error(b.loc, "Cannot perform `%s` %s `%s` operation",
					leftType.typeName(c), binOpStrings[b.op],
					rightType.typeName(c));
			}
			break;

		case GENERIC_MINUS_ASSIGN:
			if (leftType.isPointer && rightType.isInteger) {
				b.op = BinOp.PTR_PLUS_INT_ASSIGN;
				(cast(IntLiteralExprNode*)b.right.get_node(c)).negate(b.loc, *c);
				resType = leftTypeIndex;
				break;
			}
			goto case BITWISE_AND_ASSIGN;

		case GENERIC_PLUS_ASSIGN:
			if (leftType.isPointer && rightType.isInteger) {
				b.op = BinOp.PTR_PLUS_INT_ASSIGN;
				resType = leftTypeIndex;
				break;
			}
			goto case BITWISE_AND_ASSIGN;

		case GENERIC_DIV_ASSIGN, GENERIC_MUL_ASSIGN:
			bool success = autoconvTo(b.right, leftTypeIndex, c);
			if (!success)
				c.error(b.loc, "Cannot perform `%s` %s `%s` operation",
					leftType.typeName(c), binOpStrings[b.op],
					rightType.typeName(c));
			resType = CommonAstNodes.type_void;
			break;

		case BITWISE_AND_ASSIGN, BITWISE_OR_ASSIGN, GENERIC_INT_REM_ASSIGN,
			SHL_ASSIGN, SHR_ASSIGN, ASHR_ASSIGN, XOR_ASSIGN:
			bool success = leftType.isInteger && rightType.isInteger && autoconvTo(b.right, leftTypeIndex, c);
			if (!success)
				c.error(b.loc, "Cannot perform `%s` %s `%s` operation",
					leftType.typeName(c), binOpStrings[b.op],
					rightType.typeName(c));
			resType = CommonAstNodes.type_void;
			break;

		case ASSIGN:
			bool success = autoconvTo(b.right, leftTypeIndex, c);
			if (!success)
				c.error(b.loc, "Cannot perform `%s` %s `%s` operation",
					leftType.typeName(c), binOpStrings[b.op],
					rightType.typeName(c));
			resType = CommonAstNodes.type_void;
			break;

		default:
			c.internal_error(b.loc, "Unimplemented op %s", b.op);
	}
	assert(resType.isDefined);
	b.type = resType;
}

BinOp selectTypedOpcode(BinOp op, bool isSigned, bool isFloat)
{
	switch(op) with(BinOp) {
		case GENERIC_GREATER:
			if (isFloat) return FGT;
			if (isSigned) return SGT;
			return UGT;
		case GENERIC_GREATER_EQUAL:
			if (isFloat) return FGE;
			if (isSigned) return SGE;
			return UGE;
		case GENERIC_LESS:
			if (isFloat) return FLT;
			if (isSigned) return SLT;
			return ULT;
		case GENERIC_LESS_EQUAL:
			if (isFloat) return FLE;
			if (isSigned) return SLE;
			return ULE;
		case GENERIC_INT_REM:
			if (isSigned) return INT_SREM;
			return INT_UREM;
		case GENERIC_PLUS:
			if (isFloat) return FLT_PLUS;
			return INT_PLUS;
		case GENERIC_MINUS:
			if (isFloat) return FLT_MINUS;
			return INT_MINUS;
		case GENERIC_MUL:
			if (isFloat) return FLT_MUL;
			if (isSigned) return INT_SMUL;
			return INT_UMUL;
		case GENERIC_DIV:
			if (isFloat) return FLT_DIV;
			if (isSigned) return INT_SDIV;
			return INT_UDIV;
		case GENERIC_PLUS_ASSIGN:
			if (isFloat) return FLT_PLUS_ASSIGN;
			return INT_PLUS_ASSIGN;
		case GENERIC_MINUS_ASSIGN:
			if (isFloat) return FLT_MINUS_ASSIGN;
			return INT_MINUS_ASSIGN;
		case GENERIC_MUL_ASSIGN:
			if (isFloat) return FLT_MUL_ASSIGN;
			if (isSigned) return INT_SMUL_ASSIGN;
			return INT_UMUL_ASSIGN;
		case GENERIC_DIV_ASSIGN:
			if (isFloat) return FLT_DIV_ASSIGN;
			if (isSigned) return INT_SDIV_ASSIGN;
			return INT_UDIV_ASSIGN;
		case GENERIC_INT_REM_ASSIGN:
			if (isSigned) return INT_SREM_ASSIGN;
			return INT_UREM_ASSIGN;
		default: return op;
	}
}

BinOp binOpAssignToRegularOp(BinOp op) {
	switch(op) with(BinOp) {
		case BITWISE_AND_ASSIGN: return BITWISE_AND;
		case BITWISE_OR_ASSIGN: return BITWISE_OR;
		case XOR_ASSIGN: return XOR;
		case SHL_ASSIGN: return SHL;
		case SHR_ASSIGN: return SHR;
		case ASHR_ASSIGN: return ASHR;
		case INT_PLUS_ASSIGN: return INT_PLUS;
		case INT_MINUS_ASSIGN: return INT_MINUS;
		case INT_SMUL_ASSIGN: return INT_SMUL;
		case INT_UMUL_ASSIGN: return INT_UMUL;
		case INT_SDIV_ASSIGN: return INT_SDIV;
		case INT_UDIV_ASSIGN: return INT_UDIV;
		case INT_SREM_ASSIGN: return INT_SREM;
		case INT_UREM_ASSIGN: return INT_UREM;
		case FLT_PLUS_ASSIGN: return FLT_PLUS;
		case FLT_MINUS_ASSIGN: return FLT_MINUS;
		case FLT_MUL_ASSIGN: return FLT_MUL;
		case FLT_DIV_ASSIGN: return FLT_DIV;
		case PTR_PLUS_INT_ASSIGN: return PTR_PLUS_INT;
		default: assert(false);
	}
}


///
string[] binOpStrings = gatherStrings!BinOp;

enum BinOp : ubyte {
	// logic ops
	@("&&") LOGIC_AND,         // &&
	@("||") LOGIC_OR,          // ||

	// comparisons are converted into IrBinaryCondition, order is important
	@("==") EQUAL,             // ==
	@("!=") NOT_EQUAL,         // !=

	// generic compare
	@(">")  GENERIC_GREATER,       // >
	@(">=") GENERIC_GREATER_EQUAL, // >=
	@("<")  GENERIC_LESS,          // <
	@("<=") GENERIC_LESS_EQUAL,    // <=

	// integer compare
	// signed
	@("s>")  SGT,              // >
	@("s>=") SGE,              // >=
	@("s<")  SLT,              // <
	@("s<=") SLE,              // <=

	// unsigned
	@("u>")  UGT,              // >
	@("u>=") UGE,              // >=
	@("u<")  ULT,              // <
	@("u<=") ULE,              // <=

	// float compare
	@("f>")  FGT,              // >
	@("f>=") FGE,              // >=
	@("f<")  FLT,              // <
	@("f<=") FLE,              // <=

	// arithmetic ops
	@("&") BITWISE_AND,        // &
	@("|") BITWISE_OR,         // |
	@("^") XOR,                // ^

	@("<<") SHL,               // <<
	@(">>") SHR,               // >>
	@(">>>") ASHR,             // >>>

	@("+") GENERIC_PLUS,       // +
	@("-") GENERIC_MINUS,      // -
	@("*") GENERIC_MUL,        // *
	@("/") GENERIC_DIV,        // /
	@("%") GENERIC_INT_REM,    // %

	@("+") FLT_PLUS,           // +
	@("-") FLT_MINUS,          // -
	@("*") FLT_MUL,            // *
	@("/") FLT_DIV,            // /

	@("+") INT_PLUS,           // +
	@("-") INT_MINUS,          // -
	@("*") INT_SMUL,           // *
	@("*") INT_UMUL,           // *
	@("/") INT_SDIV,           // /
	@("/") INT_UDIV,           // /
	@("%") INT_SREM,           // %
	@("%") INT_UREM,           // %

	@("-") PTR_DIFF,           // ptr - ptr
	@("+") PTR_PLUS_INT,       // ptr + int and ptr - int

	@("=")   ASSIGN,           // =

	@("&=")  BITWISE_AND_ASSIGN, // &=
	@("|=")  BITWISE_OR_ASSIGN,  // |=
	@("^=")  XOR_ASSIGN,         // ^=

	@("<<=") SHL_ASSIGN,         // <<=
	@(">>=") SHR_ASSIGN,         // >>=
	@(">>>=") ASHR_ASSIGN,       // >>>=


	@("+=")  GENERIC_PLUS_ASSIGN,    // +=
	@("-=")  GENERIC_MINUS_ASSIGN,   // -=
	@("*=")  GENERIC_MUL_ASSIGN,     // *=
	@("/=")  GENERIC_DIV_ASSIGN,     // /=
	@("%=")  GENERIC_INT_REM_ASSIGN, // %=

	@("+=")  INT_PLUS_ASSIGN,    // +=
	@("-=")  INT_MINUS_ASSIGN,   // -=
	@("*=")  INT_SMUL_ASSIGN,    // *=
	@("*=")  INT_UMUL_ASSIGN,    // *=
	@("/=")  INT_SDIV_ASSIGN,    // /=
	@("/=")  INT_UDIV_ASSIGN,    // /=
	@("%=")  INT_SREM_ASSIGN,    // %=
	@("%=")  INT_UREM_ASSIGN,    // %=

	@("+=")  FLT_PLUS_ASSIGN,    // +=
	@("-=")  FLT_MINUS_ASSIGN,   // -=
	@("*=")  FLT_MUL_ASSIGN,     // *=
	@("/=")  FLT_DIV_ASSIGN,     // /=

	@("+")   PTR_PLUS_INT_ASSIGN,// ptr -= / += int

	// member access
	@(".") DOT,                // .
}

private string[] gatherStrings(alias _enum)()
{
	string[] res = new string[__traits(allMembers, _enum).length];
	foreach (i, m; __traits(allMembers, _enum))
	{
		res[i] = __traits(getAttributes, __traits(getMember, _enum, m))[0];
	}
	return res;
}
