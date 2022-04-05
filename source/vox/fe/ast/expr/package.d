/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.expr;

public import vox.fe.ast.expr.binary_op;
public import vox.fe.ast.expr.call;
public import vox.fe.ast.expr.index;
public import vox.fe.ast.expr.literal;
public import vox.fe.ast.expr.member_access;
public import vox.fe.ast.expr.name_use;
public import vox.fe.ast.expr.named_argument;
public import vox.fe.ast.expr.slice;
public import vox.fe.ast.expr.type_conv;
public import vox.fe.ast.expr.unary_op;

import vox.all;

mixin template ExpressionNodeData(AstType _astType, int default_flags = 0, AstNodeState _init_state = AstNodeState.name_register_self_done) {
	mixin AstNodeData!(_astType, default_flags, _init_state);
	AstIndex type;
}

// Abstract node, must not be instantiated
struct ExpressionNode {
	mixin ExpressionNodeData!(AstType.abstract_node);

	StringLiteralExprNode* isStringLiteral() return {
		if (astType == AstType.literal_string) return cast(StringLiteralExprNode*)&this;
		return null;
	}
}
