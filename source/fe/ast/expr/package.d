/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr;

public import fe.ast.expr.binary_op;
public import fe.ast.expr.call;
public import fe.ast.expr.index;
public import fe.ast.expr.literal;
public import fe.ast.expr.member_access;
public import fe.ast.expr.name_use;
public import fe.ast.expr.type_conv;
public import fe.ast.expr.unary_op;

import all;

NameUseExprNode* cast_expr_name_use(AstNode* t) {
	if (t.astType == AstType.expr_name_use) {
		return cast(NameUseExprNode*)t;
	}
	return null;
}

NameUseExprNode* get_expr_name_use(AstIndex index, CompilationContext* context) {
	return cast_expr_name_use(context.getAstNode(index));
}

mixin template ExpressionNodeData(AstType _astType, int default_flags = 0, AstNodeState _init_state = AstNodeState.name_register_done) {
	mixin AstNodeData!(_astType, default_flags | AstFlags.isExpression, _init_state);
	AstIndex type;
	// can be stack slot, global, variable, virtualRegister, constant, constantAggregate
	IrIndex irValue;

	AstNode* as_node() { return cast(AstNode*)&this; }
	NameUseExprNode* as_name_use() { return cast_expr_name_use(cast(AstNode*)&this); }
}

// Abstract node, must not be instantiated
struct ExpressionNode {
	mixin ExpressionNodeData!(AstType.abstract_node);

	StringLiteralExprNode* isStringLiteral() {
		if (astType == AstType.literal_string) return cast(StringLiteralExprNode*)&this;
		return null;
	}
}
