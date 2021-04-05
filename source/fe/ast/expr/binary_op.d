/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.binary_op;

import all;

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
		IrIndex rightRvalue = getRvalue(gen, b.loc, currentBlock, rightLvalue);

		ExpressionNode* leftExpr = b.left.get_expr(c);
		ExpressionNode* rightExpr = b.right.get_expr(c);

		c.assertf(leftLvalue.irValue.isDefined, leftExpr.loc, "%s null IR val", leftExpr.astType);
		c.assertf(rightLvalue.irValue.isDefined, rightExpr.loc, "%s null IR val", rightExpr.astType);

		if (b.op == BinOp.ASSIGN) {
			store(gen, b.loc, currentBlock, leftLvalue, rightRvalue);
			gen.builder.addJumpToLabel(currentBlock, nextStmt);
			return rightLvalue;
		}
		else if (b.op == BinOp.PTR_PLUS_INT_ASSIGN)
		{
			IrIndex leftRvalue = getRvalue(gen, b.loc, currentBlock, leftLvalue);
			assert(leftExpr.type.get_type(c).isPointer && rightExpr.type.get_type(c).isInteger);
			IrIndex irValue = buildGEP(gen, b.loc, currentBlock, leftRvalue, rightRvalue);
			store(gen, b.loc, currentBlock, leftLvalue, irValue);
			gen.builder.addJumpToLabel(currentBlock, nextStmt);
			return ExprValue(irValue);
		}
		else
		{
			IrIndex leftRvalue = getRvalue(gen, b.loc, currentBlock, leftLvalue);
			IrIndex irValue;

			if (leftRvalue.isSimpleConstant && rightRvalue.isSimpleConstant)
			{
				irValue = calcBinOp(binOpAssignToRegularOp(b.op), leftRvalue, rightRvalue, leftExpr.type.typeArgSize(c), c);
			}
			else
			{
				auto leftType = leftExpr.type.get_type(c);
				ExtraInstrArgs extra = {
					opcode : binOpcode(b.op, leftType.isSigned, leftType.isFloat, b.loc, c),
					type : leftExpr.type.gen_ir_type(c),
					argSize : leftExpr.type.typeArgSize(c)
				};
				irValue = gen.builder.emitInstr!(IrOpcode.generic_binary)(
					currentBlock, extra, leftRvalue, rightRvalue).result;
			}

			store(gen, b.loc, currentBlock, leftLvalue, irValue);
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

	auto leftValue = getRvalue(gen, b.loc, currentBlock, leftLvalue);
	auto rightValue = getRvalue(gen, b.loc, currentBlock, rightLvalue);

	// constant folding
	if (leftValue.isSimpleConstant && rightValue.isSimpleConstant)
	{
		IrIndex value = calcBinOp(b.op, leftValue, rightValue, b.type.typeArgSize(c), c);
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
		IrIndex irValue;
		switch(b.op) with(BinOp)
		{
			case EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL:
				auto leftType = leftExpr.type.get_type(c);
				ExtraInstrArgs extra = {
					cond : convertBinOpToIrCond(b.op, leftExpr.type.isSigned(c), leftType.isFloat),
					type : b.type.gen_ir_type(c),
					argSize : leftExpr.type.typeArgSize(c)
				};
				irValue = gen.builder.emitInstr!(IrOpcode.set_binary_cond)(
					currentBlock, extra, leftValue, rightValue).result;
				break;

			case PTR_PLUS_INT:
				assert(leftExpr.type.get_type(c).isPointer && rightExpr.type.get_type(c).isInteger);
				irValue = buildGEP(gen, b.loc, currentBlock, leftValue, rightValue);
				break;

			case PTR_DIFF:
				assert(leftExpr.type.get_type(c).isPointer && rightExpr.type.get_type(c).isPointer);

				ExtraInstrArgs extra = { type : b.type.gen_ir_type(c), argSize : leftExpr.type.typeArgSize(c) };
				irValue = gen.builder.emitInstr!(IrOpcode.sub)(currentBlock, extra, leftValue, rightValue).result;

				// divide by elem size
				TypeNode* baseType = leftExpr.type.get_type(c).as_ptr.base.get_type(c);
				uint elemSize = baseType.sizealign(c).size;
				if (elemSize == 1 || baseType.isVoid) break;

				ExtraInstrArgs extra2 = { type : makeBasicTypeIndex(IrValueType.i64), argSize : leftExpr.type.typeArgSize(c) };
				IrIndex elemSizeValue = c.constants.add(elemSize, IsSigned.no, leftExpr.type.typeArgSize(c));
				irValue = gen.builder.emitInstr!(IrOpcode.sdiv)(currentBlock, extra, irValue, elemSizeValue).result;
				break;

			case PLUS, MINUS, DIV, REMAINDER, MULT, SHL, SHR, ASHR, XOR, BITWISE_AND, BITWISE_OR:
				auto leftType = leftExpr.type.get_type(c);
				ExtraInstrArgs extra = {
					opcode : binOpcode(b.op, leftType.isSigned, leftType.isFloat, b.loc, c),
					type : b.type.gen_ir_type(c),
					argSize : b.type.typeArgSize(c)
				};
				irValue = gen.builder.emitInstr!(IrOpcode.generic_binary)(
					currentBlock, extra, leftValue, rightValue).result;
				break;
			default: c.internal_error(b.loc, "Opcode `%s` is not implemented", b.op); assert(false);
		}
		c.assertf(irValue.isDefined, b.loc, "%s null IR val", b.astType);
		gen.builder.addJumpToLabel(currentBlock, trueExit);
		return irValue;
	}
	else // branch
	{
		switch(b.op) with(BinOp)
		{
			case EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL:
				auto branch = gen.builder.addBinBranch(
					currentBlock, convertBinOpToIrCond(b.op, leftExpr.type.isSigned(c), leftExpr.type.isFloat(c)), leftExpr.type.typeArgSize(c),
					leftValue, rightValue, trueExit, falseExit);
				break;
			default: c.internal_error(b.loc, "Opcode `%s` is not implemented", b.op); break;
		}
		return IrIndex();
	}
}

IrOpcode binOpcode(BinOp binop, IsSigned isSigned, bool isFloat, TokenIndex loc, CompilationContext* context)
{
	switch(binop) with(BinOp)
	{
		case PLUS, PLUS_ASSIGN:
			if (isFloat) return IrOpcode.fadd;
			return IrOpcode.add;
		case MINUS, MINUS_ASSIGN:
			if (isFloat) return IrOpcode.fsub;
			return IrOpcode.sub;
		case DIV, DIV_ASSIGN:
			if (isFloat) return IrOpcode.fdiv;
			if (isSigned) return IrOpcode.sdiv;
			return IrOpcode.udiv;
		case REMAINDER, REMAINDER_ASSIGN:
			if (isSigned) return IrOpcode.srem;
			return IrOpcode.urem;
		case MULT, MULT_ASSIGN:
			if (isFloat) return IrOpcode.fmul;
			if (isSigned) return IrOpcode.smul;
			return IrOpcode.umul;
		case SHL, SHL_ASSIGN: return IrOpcode.shl;
		case SHR, SHR_ASSIGN: return IrOpcode.lshr;
		case ASHR, ASHR_ASSIGN: return IrOpcode.ashr;
		case XOR, XOR_ASSIGN: return IrOpcode.xor;
		case BITWISE_AND, BITWISE_AND_ASSIGN: return IrOpcode.and;
		case BITWISE_OR, BITWISE_OR_ASSIGN: return IrOpcode.or;
		default:
			context.internal_error(loc, "assign op %s not implemented", binop);
			assert(false);
	}
}

IrIndex calcBinOp(BinOp op, IrIndex left, IrIndex right, IrArgSize argSize, CompilationContext* context)
{
	IrConstant leftCon = context.constants.get(left);
	IrConstant rightCon = context.constants.get(right);

	bool isAnySigned = left.isSignedConstant || right.isSignedConstant;

	switch(op)
	{
		case BinOp.EQUAL:         return context.constants.add(cast(ubyte)(leftCon.i64 == rightCon.i64), IsSigned.no, argSize);
		case BinOp.NOT_EQUAL:     return context.constants.add(cast(ubyte)(leftCon.i64 != rightCon.i64), IsSigned.no, argSize);
		case BinOp.GREATER:       return context.constants.add(cast(ubyte)(leftCon.i64 >  rightCon.i64), IsSigned.no, argSize);
		case BinOp.GREATER_EQUAL: return context.constants.add(cast(ubyte)(leftCon.i64 >= rightCon.i64), IsSigned.no, argSize);
		case BinOp.LESS:          return context.constants.add(cast(ubyte)(leftCon.i64 <  rightCon.i64), IsSigned.no, argSize);
		case BinOp.LESS_EQUAL:    return context.constants.add(cast(ubyte)(leftCon.i64 <= rightCon.i64), IsSigned.no, argSize);
		case BinOp.PLUS:          return context.constants.add(leftCon.i64 + rightCon.i64, cast(IsSigned)isAnySigned, argSize);
		case BinOp.MINUS:         return context.constants.add(leftCon.i64 - rightCon.i64, cast(IsSigned)isAnySigned, argSize);
		case BinOp.MULT:          return context.constants.add(leftCon.i64 * rightCon.i64, cast(IsSigned)isAnySigned, argSize);
		case BinOp.DIV:           return context.constants.add(leftCon.i64 / rightCon.i64, cast(IsSigned)isAnySigned, argSize);
		case BinOp.REMAINDER:     return context.constants.add(leftCon.i64 % rightCon.i64, cast(IsSigned)isAnySigned, argSize);

		// TODO: we need type info here, to correctly mask the shift size
		case BinOp.SHL:           return context.constants.add(leftCon.i64 << rightCon.i64, cast(IsSigned)left.isSignedConstant, argSize);
		case BinOp.SHR:
			ulong result;
			final switch(left.constantSize) with(IrArgSize)
			{
				case size8:  result = leftCon.i8 >>> rightCon.i64; break;
				case size16: result = leftCon.i16 >>> rightCon.i64; break;
				case size32: result = leftCon.i32 >>> rightCon.i64; break;
				case size64: result = leftCon.i64 >>> rightCon.i64; break;
				case size128, size256, size512: context.internal_error("Invalid constant size %s", left.constantSize);
			}
			return context.constants.add(result, cast(IsSigned)left.isSignedConstant, argSize);
		case BinOp.ASHR:
			ulong result;
			final switch(left.constantSize) with(IrArgSize)
			{
				case size8:  result = leftCon.i8 >> rightCon.i64; break;
				case size16: result = leftCon.i16 >> rightCon.i64; break;
				case size32: result = leftCon.i32 >> rightCon.i64; break;
				case size64: result = leftCon.i64 >> rightCon.i64; break;
				case size128, size256, size512: context.internal_error("Invalid constant size %s", left.constantSize);
			}
			return context.constants.add(result, cast(IsSigned)left.isSignedConstant, argSize);
		case BinOp.BITWISE_OR:    return context.constants.add(leftCon.i64 | rightCon.i64, cast(IsSigned)isAnySigned, argSize);
		case BinOp.BITWISE_AND:   return context.constants.add(leftCon.i64 & rightCon.i64, cast(IsSigned)isAnySigned, argSize);
		case BinOp.XOR:           return context.constants.add(leftCon.i64 ^ rightCon.i64, cast(IsSigned)isAnySigned, argSize);

		default:
			context.internal_error("Opcode `%s` is not implemented", op);
			assert(false);
	}
}

IrBinaryCondition convertBinOpToIrCond(BinOp op, bool isSigned, bool isFloat)
{
	switch(op) with(BinOp) with(IrBinaryCondition)
	{
		case EQUAL:         return eq;
		case NOT_EQUAL:     return ne;
		case GREATER:       return cast(IrBinaryCondition)(ugt + cast(ubyte)isSigned * 4); // ugt or sgt
		case GREATER_EQUAL: return cast(IrBinaryCondition)(uge + cast(ubyte)isSigned * 4); // uge or sge
		case LESS:          return cast(IrBinaryCondition)(ult + cast(ubyte)isSigned * 4); // ult or slt
		case LESS_EQUAL:    return cast(IrBinaryCondition)(ule + cast(ubyte)isSigned * 4); // ule or sle
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

	if (leftType.isError || rightType.isError) return;

	switch(b.op) with(BinOp)
	{
		// logic ops. Requires both operands to be bool
		case LOGIC_AND, LOGIC_OR:
			autoconvToBool(b.left, c);
			autoconvToBool(b.right, c);
			resType = CommonAstNodes.type_bool;
			break;
		// logic ops. Requires both operands to be of the same type
		case EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL:
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

			if (autoconvToCommonType(b.left, b.right, c))
				resType = CommonAstNodes.type_bool;
			else
				c.error(b.left.get_node(c).loc, "Cannot compare `%s` and `%s`",
					leftType.typeName(c),
					rightType.typeName(c));
			break;

		case MINUS:
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
			goto case DIV;

		case PLUS:
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

			goto case DIV;

		// arithmetic op int float
		case DIV, MULT:
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
		case REMAINDER, SHL, SHR, ASHR, BITWISE_AND, BITWISE_OR, XOR:
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

		case MINUS_ASSIGN:
			if (leftType.isPointer && rightType.isInteger) {
				b.op = BinOp.PTR_PLUS_INT_ASSIGN;
				(cast(IntLiteralExprNode*)b.right.get_node(c)).negate(b.loc, *c);
				resType = leftTypeIndex;
				break;
			}
			goto case BITWISE_AND_ASSIGN;

		case PLUS_ASSIGN:
			if (leftType.isPointer && rightType.isInteger) {
				b.op = BinOp.PTR_PLUS_INT_ASSIGN;
				resType = leftTypeIndex;
				break;
			}
			goto case BITWISE_AND_ASSIGN;

		case DIV_ASSIGN, MULT_ASSIGN:
			bool success = autoconvTo(b.right, leftTypeIndex, c);
			if (!success)
				c.error(b.loc, "Cannot perform `%s` %s `%s` operation",
					leftType.typeName(c), binOpStrings[b.op],
					rightType.typeName(c));
			break;

		case BITWISE_AND_ASSIGN, BITWISE_OR_ASSIGN, REMAINDER_ASSIGN,
			SHL_ASSIGN, SHR_ASSIGN, ASHR_ASSIGN, XOR_ASSIGN:
			bool success = leftType.isInteger && rightType.isInteger && autoconvTo(b.right, leftTypeIndex, c);
			if (!success)
				c.error(b.loc, "Cannot perform `%s` %s `%s` operation",
					leftType.typeName(c), binOpStrings[b.op],
					rightType.typeName(c));
			break;

		case ASSIGN:
			bool success = autoconvTo(b.right, leftTypeIndex, c);
			if (!success)
				c.error(b.loc, "Cannot perform `%s` %s `%s` operation",
					leftType.typeName(c), binOpStrings[b.op],
					rightType.typeName(c));
			break;

		default:
			c.internal_error(b.loc, "Unimplemented op %s", b.op);
			assert(false);
	}
	assert(resType.isDefined);
	b.type = resType;
}

BinOp binOpAssignToRegularOp(BinOp op) {
	switch(op) with(BinOp) {
		case BITWISE_AND_ASSIGN: return BITWISE_AND;
		case BITWISE_OR_ASSIGN: return BITWISE_OR;
		case REMAINDER_ASSIGN: return REMAINDER;
		case SHL_ASSIGN: return SHL;
		case SHR_ASSIGN: return SHR;
		case ASHR_ASSIGN: return ASHR;
		case MINUS_ASSIGN: return MINUS;
		case PLUS_ASSIGN: return PLUS;
		case DIV_ASSIGN: return DIV;
		case MULT_ASSIGN: return MULT;
		case XOR_ASSIGN: return XOR;
		default: assert(false);
	}
}


///
string[] binOpStrings = gatherStrings!BinOp;

enum BinOp BIN_OP_LOGIC_FIRST = BinOp.EQUAL;
enum BinOp BIN_OP_LOGIC_LAST = BinOp.LESS_EQUAL;
enum BinOp BIN_OP_ARITH_FIRST = BinOp.MINUS;
enum BinOp BIN_OP_ARITH_LAST = BinOp.MULT;

enum BinOp : ubyte {
	// logic ops
	@("&&") LOGIC_AND,          // &&
	@("||") LOGIC_OR,           // ||

	// comparisons are converted into IrBinaryCondition, order is important
	@("==") EQUAL,              // ==
	@("!=") NOT_EQUAL,          // !=
	@(">")  GREATER,            // >
	@(">=") GREATER_EQUAL,      // >=
	@("<")  LESS,               // <
	@("<=") LESS_EQUAL,         // <=

	// arithmetic ops
	@("&") BITWISE_AND,        // &
	@("|") BITWISE_OR,         // |
	@("%") REMAINDER,          // %
	@("<<") SHL,               // <<
	@(">>") SHR,               // >>
	@(">>>") ASHR,             // >>>
	@("-") MINUS,              // -
	@("+") PLUS,               // +
	@("/") DIV,                // /
	@("*") MULT,               // *
	@("^") XOR,                // ^

	@("-") PTR_DIFF,           // ptr - ptr
	@("+") PTR_PLUS_INT,       // ptr + int and ptr - int

	@("=")   ASSIGN,             // =

	// arithmetic opOpAssing, same order as above (to convert)
	@("&=")  BITWISE_AND_ASSIGN, // &=
	@("|=")  BITWISE_OR_ASSIGN,  // |=
	@("%=")  REMAINDER_ASSIGN,   // %=
	@("<<=") SHL_ASSIGN,         // <<=
	@(">>=") SHR_ASSIGN,         // >>=
	@(">>>=") ASHR_ASSIGN,       // >>>=
	@("-=")  MINUS_ASSIGN,       // -=
	@("+=")  PLUS_ASSIGN,        // +=
	@("/=")  DIV_ASSIGN,         // /=
	@("*=")  MULT_ASSIGN,        // *=
	@("^=")  XOR_ASSIGN,         // ^=

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
