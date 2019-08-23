/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.call;

import all;

@(AstType.expr_call)
struct CallExprNode {
	mixin ExpressionNodeData!(AstType.expr_call);
	AstIndex callee;
	Array!AstIndex args;
}

void name_resolve_call(CallExprNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.callee, state);
	foreach (ref arg; node.args) require_name_resolve(arg, state);
	node.state = AstNodeState.name_resolve_done;
}

// Get type from function declaration
void type_check_call(ref AstIndex callIndex, CallExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	AstNode* callee = node.callee.get_effective_node(c).get_node(c);
	// TODO: support more than plain func() calls. Such as func_array[42](), (*func_ptr)() etc
	switch (callee.astType)
	{
		// static function call
		case AstType.decl_function: return type_check_func_call(node, callee.as!FunctionDeclNode(c), state);
		case AstType.decl_struct: return type_check_constructor_call(node, callee.as!StructDeclNode(c), state);
		case AstType.expr_member:
			// UFCS call
			MemberExprNode* member = callee.as!MemberExprNode(c);
			Identifier calleeName = member.memberId(c);
			LookupResult ufcsRes = tryUFCSCall(callIndex, member, state);
			if (ufcsRes == LookupResult.failure) {
				AstIndex objType = member.aggregate.get_node_type(c);
				node.type = c.basicTypeNodes(BasicType.t_error);
				c.error(node.loc, "`%s` has no member `%s`", objType.printer(c), c.idString(calleeName));
				return;
			}
			break;
		default:
			node.type = c.basicTypeNodes(BasicType.t_error);
			c.error(node.loc, "Cannot call %s", callee.astType);
			c.internal_error(node.loc,
				"Only direct function calls are supported right now");

	}
	node.state = AstNodeState.type_check_done;
}


void type_check_func_call(CallExprNode* node, FunctionDeclNode* funcDecl, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.type = c.basicTypeNodes(BasicType.t_error);

	Array!AstIndex params = funcDecl.parameters;
	auto numParams = params.length;
	auto numArgs = node.args.length;

	if (numArgs < numParams) {
		c.error(node.loc, "Insufficient parameters to '%s', got %s, expected %s",
			c.idString(funcDecl.id), numArgs, numParams);
		return;
	}
	else if (numArgs > numParams) {
		c.error(node.loc, "Too much parameters to '%s', got %s, expected %s",
			c.idString(funcDecl.id), numArgs, numParams);
		return;
	}

	foreach (i, ref AstIndex arg; node.args)
	{
		VariableDeclNode* param = c.getAst!VariableDeclNode(params[i]);

		require_type_check(param.type, state);
		require_type_check(arg, state);
		bool success = autoconvTo(arg, param.type, c);
		if (!success)
			c.error(arg.loc(c),
				"Argument %s, must have type %s, not %s", i+1,
				param.type.printer(c),
				arg.expr_type(c).printer(c));
	}
	node.type = funcDecl.returnType;
}

void type_check_constructor_call(CallExprNode* node, StructDeclNode* s, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	size_t numStructMembers;
	foreach(AstIndex memberIndex; s.declarations)
	{
		AstNode* member = memberIndex.get_node(c);
		if (member.astType != AstType.decl_var) continue;

		ExpressionNode* initializer;
		if (node.args.length > numStructMembers) { // init from constructor argument
			require_type_check(node.args[numStructMembers], state);
			autoconvTo(node.args[numStructMembers], member.as!VariableDeclNode(c).type, c);
		} else { // init with initializer from struct definition
			c.internal_error(node.loc, "Not implemented");
		}
		++numStructMembers;
	}
	node.type = c.getAstNodeIndex(s);
}
