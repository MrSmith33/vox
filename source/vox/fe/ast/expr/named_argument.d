/// Copyright: Copyright (c) 2022 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.expr.named_argument;

import vox.all;

enum NamedArgumentFlags : ushort
{
	isSymResolved = AstFlags.userFlag << 0,
}

@(AstType.expr_named_argument)
struct NamedArgumenExprNode {
	mixin AstNodeData!(AstType.expr_named_argument, 0, AstNodeState.name_register_self_done);
	union
	{
		private Identifier _id;    // used when not yet resolved
		private ushort _paramIndex; // used when resolved
	}
	AstIndex expr;

	bool isSymResolved() { return cast(bool)(flags & NamedArgumentFlags.isSymResolved); }

	this(TokenIndex loc, Identifier id, AstIndex expr)
	{
		this.loc = loc;
		this.astType = AstType.expr_named_argument;
		this.state = AstNodeState.name_register_self_done;
		this._id = id;
		this.expr = expr;
	}

	Identifier getId(CompilationContext* c) {
		c.assertf(!isSymResolved, loc, "Getting id of resolved NamedArgumenExprNode");
		return _id;
	}

	ushort getParamIndex(CompilationContext* c) {
		c.assertf(isSymResolved, loc, "Getting paramIndex of unresolved NamedArgumenExprNode");
		return _paramIndex;
	}

	void resolve(ushort paramIndex, CompilationContext* c) {
		c.assertf(!isSymResolved, loc, "Already resolved");
		this._paramIndex = paramIndex;
		this.flags |= NamedArgumentFlags.isSymResolved;
	}
}

void print_named_argument(NamedArgumenExprNode* node, ref AstPrintState state)
{
	if (node.isSymResolved)
		state.print(node.getParamIndex(state.context), ": ");
	else
		state.print(state.context.idString(node._id), ": ");
	print_ast(node.expr, state);
}

void post_clone_named_argument(NamedArgumenExprNode* node, ref CloneState state)
{
	state.fixAstIndex(node.expr);
}

void name_register_nested_named_argument(NamedArgumenExprNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.expr, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_named_argument(NamedArgumenExprNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.expr, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_named_argument(NamedArgumenExprNode* node, ref TypeCheckState state) {
	node.state = AstNodeState.type_check;
	require_type_check(node.expr, state);
	node.state = AstNodeState.type_check_done;
}
