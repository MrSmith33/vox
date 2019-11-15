/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

// inside struct method supported:
// foo;
// foo();
// this.foo;
// this.foo();
// in static function:
// s.foo;
// s.foo();
module fe.ast.expr.call;

import all;

@(AstType.expr_call)
struct CallExprNode {
	mixin ExpressionNodeData!(AstType.expr_call);
	AstIndex callee;
	AstNodes args;
}

void name_register_nested_call(CallExprNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.callee, state);
	require_name_register(node.args, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_call(CallExprNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.callee, state);
	require_name_resolve(node.args, state);
	node.state = AstNodeState.name_resolve_done;
}

// Get type from function declaration
void type_check_call(ref AstIndex callIndex, CallExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	scope(exit) node.state = AstNodeState.type_check_done;

	AstIndex callee = node.callee.get_effective_node(c);

	switch (callee.astType(c))
	{
		// static function call
		case AstType.decl_function:
			auto func = callee.get!FunctionDeclNode(c);
			auto signature = func.signature.get!FunctionSignatureNode(c);
			return type_check_func_call(node, signature, func.id, state);
		case AstType.decl_struct:
			return type_check_constructor_call(node, callee.get!StructDeclNode(c), state);
		case AstType.expr_member:
			MemberExprNode* member = callee.get!MemberExprNode(c);
			// Method call
			LookupResult res = lookupMember(member, state);
			if (res == LookupResult.success) {
				node.callee = member.member(c);
				auto signature = node.callee.get_type(c).as_func_sig;
				lowerThisArgument(signature, member.aggregate, member.loc, c);
				node.args.putFront(c.arrayArena, member.aggregate);
				return type_check_func_call(node, signature, member.memberId(c), state);
			}
			// UFCS call
			Identifier calleeName = member.memberId(c);
			LookupResult ufcsRes = tryUFCSCall(callIndex, member, state);
			if (ufcsRes == LookupResult.failure) {
				AstIndex objType = member.aggregate.get_node_type(c);
				node.type = c.basicTypeNodes(BasicType.t_error);
				c.error(node.loc, "`%s` has no member `%s`", objType.printer(c), c.idString(calleeName));
				return;
			}
			break;
		case AstType.decl_var, AstType.decl_enum_member:
			// check if func ptr
			TypeNode* varType = callee.get_node_type(c).get_type(c);
			if (varType.isPointer)
			{
				TypeNode* base = varType.as_ptr.base.get_type(c);
				if (base.isFuncSignature)
				{
					auto signature = base.as_func_sig;
					return type_check_func_call(node, signature, callee.get_node_id(c), state);
				}
			}
			goto default;
		default:
			node.type = c.basicTypeNodes(BasicType.t_error);
			c.error(node.loc, "Cannot call %s", callee.astType(c));
			c.internal_error(node.loc,
				"Only direct function calls are supported right now");
	}
}


void type_check_func_call(CallExprNode* node, FunctionSignatureNode* signature, Identifier id, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.type = signature.returnType;

	Array!AstIndex params = signature.parameters;
	auto numParams = params.length;
	auto numArgs = node.args.length;

	if (numArgs < numParams) {
		c.error(node.loc, "Insufficient parameters to '%s', got %s, expected %s",
			c.idString(id), numArgs, numParams);
		return;
	}
	else if (numArgs > numParams) {
		c.error(node.loc, "Too much parameters to '%s', got %s, expected %s",
			c.idString(id), numArgs, numParams);
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
