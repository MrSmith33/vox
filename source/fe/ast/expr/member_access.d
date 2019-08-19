/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.member_access;

import all;


// member access of aggregate.member form
struct MemberExprNode {
	mixin ExpressionNodeData!(AstType.expr_member);
	AstIndex aggregate;
	AstIndex member; // member name (NameUseExprNode)
	uint memberIndex; // resolved index of member being accessed
	AstIndex curScope; // set in name resolve pass
}

void name_resolve_member(MemberExprNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	node.curScope = state.context.getAstNodeIndex(state.currentScope);
	// name resolution is done in type check pass
	require_name_resolve(node.aggregate, state);
	node.state = AstNodeState.name_resolve_done;
}
