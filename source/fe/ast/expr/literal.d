/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.literal;

import all;

struct IntLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_int, AstFlags.isLiteral, AstNodeState.name_resolve);
	ulong value;
	bool isNegative() { return cast(bool)(flags & AstFlags.user1); }
	IsSigned isSigned() { return cast(IsSigned)isNegative; }
	void negate(TokenIndex pos, ref CompilationContext context) {
		if (isNegative) {
			value = -(cast(long)value);
			flags &= ~AstFlags.user1;
		} else {
			if (value <= 0x8000_0000_0000_0000) {
				value = -(cast(long)value);
				flags |= AstFlags.user1;
			}
			else {
				context.error(pos, "`-%s` results in signed integer overflow", value);
			}
		}
	}
}

struct NullLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_null, AstFlags.isLiteral, AstNodeState.name_resolve);
}

struct BoolLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_bool, AstFlags.isLiteral, AstNodeState.name_resolve);
	bool value;
}

struct StringLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_string, AstFlags.isLiteral, AstNodeState.name_resolve);
	string value;
}
