/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.literal;

import all;

@(AstType.literal_int)
struct IntLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_int, 0, AstNodeState.name_resolve_done);
	ulong value;

	private enum Flags : ushort
	{
		isNegative = AstFlags.userFlag
	}
	bool isNegative() { return cast(bool)(flags & Flags.isNegative); }
	IsSigned isSigned() { return cast(IsSigned)isNegative; }

	void negate(TokenIndex pos, ref CompilationContext context) {
		if (isNegative) {
			value = -(cast(long)value);
			flags &= ~cast(int)Flags.isNegative;
		} else {
			if (value <= 0x8000_0000_0000_0000) {
				value = -(cast(long)value);
				flags |= Flags.isNegative;
			}
			else {
				context.error(pos, "`-%s` results in signed integer overflow", value);
			}
		}
	}
}

void print_literal_int(IntLiteralExprNode* node, ref AstPrintState state)
{
	if (node.isSigned)
		state.print("LITERAL int ", node.type.printer(state.context), " ", cast(long)node.value);
	else
		state.print("LITERAL int ", node.type.printer(state.context), " ", node.value);
}

void type_check_literal_int(IntLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	if (node.isSigned) {
		BasicType t = minSignedIntType(node.value);
		t = max(t, BasicType.t_i32);
		node.type = state.context.basicTypeNodes(t);
	} else {
		BasicType t = minUnsignedIntType(node.value);
		if (cast(uint)(node.value & 0x7FFF_FFFF) == node.value) t = BasicType.t_i32;
		node.type = state.context.basicTypeNodes(t);
	}
	node.state = AstNodeState.type_check_done;
}

IrIndex ir_gen_literal_int(CompilationContext* context, IntLiteralExprNode* n)
{
	CompilationContext* c = context;
	return c.constants.add(n.type.gen_ir_type(c), n.value);
}


@(AstType.literal_float)
struct FloatLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_float, 0, AstNodeState.name_resolve_done);
	double value;
	void negate(TokenIndex pos, ref CompilationContext context) {
		value = -value;
	}
}

void print_literal_float(FloatLiteralExprNode* node, ref AstPrintState state)
{
	state.print("LITERAL float ", node.type.printer(state.context), " ", node.value);
}

void type_check_literal_float(FloatLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	node.type = CommonAstNodes.type_f64;
	node.state = AstNodeState.type_check_done;
}

IrIndex ir_gen_literal_float(CompilationContext* context, FloatLiteralExprNode* node)
{
	if (node.type == CommonAstNodes.type_f32) {
		return context.constants.add(float(node.value));
	} else {
		assert(node.type == CommonAstNodes.type_f64);
		return context.constants.add(double(node.value));
	}
}


@(AstType.literal_null)
struct NullLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_null, 0, AstNodeState.name_resolve_done);
}

void print_literal_null(NullLiteralExprNode* node, ref AstPrintState state)
{
	state.print("LITERAL null");
}

void type_check_literal_null(NullLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	node.type = CommonAstNodes.type_null;
	node.state = AstNodeState.type_check_done;
}

IrIndex ir_gen_literal_null(CompilationContext* context, NullLiteralExprNode* n)
{
	CompilationContext* c = context;
	if (n.type.get_type(c).isPointer) {
		return c.constants.addZeroConstant(makeIrType(IrBasicType.i64));
	} else if (n.type.get_type(c).isSlice) {
		return c.constants.addZeroConstant(n.type.gen_ir_type(c));
	} else if (n.type.get_type(c).isTypeofNull) {
		return c.constants.addZeroConstant(makeIrType(IrBasicType.i64));
	} else c.internal_error(n.loc, "%s", n.type.printer(c));
}


@(AstType.literal_bool)
struct BoolLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_bool, 0, AstNodeState.name_resolve_done);
	bool value;
}

void print_literal_bool(BoolLiteralExprNode* node, ref AstPrintState state)
{
	if (node.value) state.print("TRUE ", node.type.printer(state.context));
	else state.print("FALSE ", node.type.printer(state.context));
}

void type_check_literal_bool(BoolLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	node.type = CommonAstNodes.type_bool;
	node.state = AstNodeState.type_check_done;
}

IrIndex ir_gen_literal_bool(CompilationContext* context, BoolLiteralExprNode* n)
{
	CompilationContext* c = context;
	return c.constants.add(makeIrType(IrBasicType.i8), (n.value != 0));
}

void ir_gen_branch_literal_bool(ref IrGenState gen, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit, BoolLiteralExprNode* n)
{
	if (n.value)
		gen.builder.addJumpToLabel(currentBlock, trueExit);
	else
		gen.builder.addJumpToLabel(currentBlock, falseExit);
}


@(AstType.literal_string)
struct StringLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_string, 0, AstNodeState.name_resolve_done);
	IrIndex irValue;
	string value;
}

void print_literal_string(StringLiteralExprNode* node, ref AstPrintState state)
{
	state.print("LITERAL string ", node.type.printer(state.context), " ", node.value);
}

void type_check_literal_string(StringLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	// done in parser
	node.state = AstNodeState.type_check_done;
}

IrIndex ir_gen_literal_string(CompilationContext* context, StringLiteralExprNode* n)
{
	return n.irValue;
}


@(AstType.literal_array)
struct ArrayLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_array, 0, AstNodeState.name_resolve_done);
	AstNodes items;
	IrIndex irValue;
}

void print_literal_array(ArrayLiteralExprNode* node, ref AstPrintState state)
{
	state.print("LITERAL array ", node.type.printer(state.context), " ", node.items);
}

void type_check_literal_array(ArrayLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.items, state);
	node.state = AstNodeState.type_check_done;
}
