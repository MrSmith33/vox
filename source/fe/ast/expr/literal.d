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

void type_check_literal_int(IntLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	if (node.isSigned)
		node.type = state.context.basicTypeNodes(minSignedIntType(node.value));
	else
		node.type = state.context.basicTypeNodes(minUnsignedIntType(node.value));
	node.state = AstNodeState.type_check_done;
}

IrIndex ir_gen_literal_int(CompilationContext* context, IntLiteralExprNode* n)
{
	CompilationContext* c = context;
	return c.constants.add(n.value, n.isSigned, n.type.typeArgSize(c));
}

@(AstType.literal_null)
struct NullLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_null, 0, AstNodeState.name_resolve_done);
}

void type_check_literal_null(NullLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	node.type = state.context.basicTypeNodes(BasicType.t_null);
	node.state = AstNodeState.type_check_done;
}

IrIndex ir_gen_literal_null(CompilationContext* context, NullLiteralExprNode* n)
{
	CompilationContext* c = context;
	if (n.type.get_type(c).isPointer) {
		return c.constants.add(0, IsSigned.no, SIZET_SIZE);
	} else if (n.type.get_type(c).isSlice) {
		return c.constants.addZeroConstant(n.type.gen_ir_type(c));
	} else c.internal_error(n.loc, "%s", n.type.printer(c));
	assert(false);
}

@(AstType.literal_bool)
struct BoolLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_bool, 0, AstNodeState.name_resolve_done);
	bool value;
}

void type_check_literal_bool(BoolLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	node.type = state.context.basicTypeNodes(BasicType.t_bool);
	node.state = AstNodeState.type_check_done;
}

IrIndex ir_gen_literal_bool(CompilationContext* context, BoolLiteralExprNode* n)
{
	CompilationContext* c = context;
	if (n.value)
		return c.constants.add(1, IsSigned.no, n.type.typeArgSize(c));
	else
		return c.constants.add(0, IsSigned.no, n.type.typeArgSize(c));
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
