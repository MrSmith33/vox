/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.literal;

import all;

struct IntLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_int, AstFlags.isLiteral, AstNodeState.name_resolve_done);
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

struct NullLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_null, AstFlags.isLiteral, AstNodeState.name_resolve_done);
}

void type_check_literal_null(NullLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	node.type = state.context.basicTypeNodes(BasicType.t_null);
	node.state = AstNodeState.type_check_done;
}

struct BoolLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_bool, AstFlags.isLiteral, AstNodeState.name_resolve_done);
	bool value;
}

void type_check_literal_bool(BoolLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	node.type = state.context.basicTypeNodes(BasicType.t_bool);
	node.state = AstNodeState.type_check_done;
}

struct StringLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_string, AstFlags.isLiteral, AstNodeState.name_resolve_done);
	string value;
}

void type_check_literal_string(StringLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	// done in parser
	node.state = AstNodeState.type_check_done;
}
