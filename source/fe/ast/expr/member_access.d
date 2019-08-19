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

void type_check_member(ref AstIndex nodeIndex, MemberExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;
	node.state = AstNodeState.type_check;

	// try member
	NameUseExprNode* member = node.member.get_expr_name_use(c);
	require_type_check(node.aggregate, state);
	LookupResult res = lookupMember(node, c);
	if (res == LookupResult.success) {
		node.state = AstNodeState.type_check_done;
		return;
	}

	// try UFCS
	AstIndex ufcsNodeIndex = lookupScopeIdRecursive(node.curScope.get_scope(c), member.id(c), node.loc, c);
	if (ufcsNodeIndex)
	{
		AstNode* ufcsNode = c.getAstNode(ufcsNodeIndex);
		if (ufcsNode.astType == AstType.decl_function)
		{
			// rewrite as call
			member.resolve = ufcsNodeIndex;
			member.astType = AstType.expr_func_name_use;
			Array!AstIndex args;
			args.put(c.arrayArena, node.aggregate);
			nodeIndex = c.appendAst!CallExprNode(member.loc, AstIndex(), IrIndex(), node.member, args);
			nodeIndex.get_node(c).state = AstNodeState.name_resolve_done;
			// type check call
			require_type_check(nodeIndex, state);
			return;
		}
	}

	// nothing found
	node.type = c.basicTypeNodes(BasicType.t_error);
	AstIndex objType = node.aggregate.get_node_type(c);
	c.error(node.loc, "`%s` has no member `%s`", objType.printer(c), c.idString(member.id(c)));

	node.state = AstNodeState.type_check_done;
}
