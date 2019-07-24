/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.call;

import all;

struct CallExprNode {
	mixin ExpressionNodeData!(AstType.expr_call);
	ExpressionNode* callee;
	Array!(ExpressionNode*) args;
}
