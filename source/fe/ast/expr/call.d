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

void post_clone_call(CallExprNode* node, ref CloneState state)
{
	state.fixAstIndex(node.callee);
	state.fixAstNodes(node.args);
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
			require_type_check(callee, state);
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
		case AstType.expr_index:
			require_type_check(callee, state);
			if (callee.astType(c) != AstType.decl_function)
				goto case AstType.decl_var;
			node.callee = callee;
			goto case AstType.decl_function;
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

	AstIndex signatureIndex = c.getAstNodeIndex(signature);
	require_type_check(signatureIndex, state);
	node.type = signature.returnType;

	AstNodes params = signature.parameters;
	auto numParams = params.length;
	auto numDefaultArgs = signature.numDefaultArgs;
	c.assertf(numParams >= numDefaultArgs, node.loc,
		"numParams(%s) < numDefaultArgs(%s)", numParams, numDefaultArgs);
	auto numRequiredArgs = numParams - numDefaultArgs;
	auto numArgs = node.args.length;

	if (numArgs < numRequiredArgs) {
		if (numDefaultArgs == 0)
			c.error(node.loc, "Insufficient arguments to `%s`, got %s, expected %s",
				c.idString(id), numArgs, numParams);
		else
			c.error(node.loc, "Insufficient arguments to `%s`, got %s, expected %s-%s",
				c.idString(id), numArgs, numRequiredArgs, numParams);
		return;
	}
	else if (numArgs > numParams) {
		if (numDefaultArgs == 0)
			c.error(node.loc, "Too much arguments to `%s`, got %s, expected %s",
				c.idString(id), numArgs, numParams);
		else
			c.error(node.loc, "Too much arguments to `%s`, got %s, expected %s-%s",
				c.idString(id), numArgs, numRequiredArgs, numParams);

		return;
	}

	// check arguments
	// default arguments do not require checking here
	foreach (i, ref AstIndex arg; node.args)
	{
		VariableDeclNode* param = c.getAst!VariableDeclNode(params[i]);

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
	foreach(AstIndex arg; s.declarations)
	{
		AstNode* member = arg.get_node(c);
		if (member.astType != AstType.decl_var) continue;

		ExpressionNode* initializer;
		if (node.args.length > numStructMembers) { // init from constructor argument
			require_type_check(node.args[numStructMembers], state);
			AstIndex memberType = member.as!VariableDeclNode(c).type;
			bool success = autoconvTo(node.args[numStructMembers], memberType, c);
			if (!success) {
				c.error(node.args[numStructMembers].loc(c),
					"Argument %s, must have type %s, not %s", numStructMembers+1,
					memberType.printer(c),
					node.args[numStructMembers].expr_type(c).printer(c));
			}
		} else { // init with initializer from struct definition
			// skip
		}
		++numStructMembers;
	}
	node.type = c.getAstNodeIndex(s);
}

ExprValue ir_gen_call(ref IrGenState gen, IrIndex currentBlock, ref IrLabel nextStmt, CallExprNode* node)
{
	CompilationContext* c = gen.context;

	AstIndex callee = node.callee.get_effective_node(c);

	switch (callee.astType(c))
	{
		case AstType.decl_function:
			auto func = callee.get!FunctionDeclNode(c);
			AstIndex returnType = func.signature.get!FunctionSignatureNode(c).returnType;
			IrIndex irIndex = func.getIrIndex(c);
			return visitCall(gen, func.signature, irIndex, currentBlock, nextStmt, node);
		case AstType.decl_struct: return visitConstructor(gen, callee.get!StructDeclNode(c), currentBlock, nextStmt, node);
		case AstType.decl_enum_member:
			// TODO: clean up
			TypeNode* varType = callee.get_node_type(c).get_type(c);
			if (!varType.isPointer) goto default;
			TypeNode* base = varType.as_ptr.base.get_type(c);
			if (!base.isFuncSignature) goto default;
			IrIndex irIndex = eval_static_expr(callee, c);
			return visitCall(gen, c.getAstNodeIndex(base), irIndex, currentBlock, nextStmt, node);
		case AstType.decl_var:
			VariableDeclNode* var = callee.get!VariableDeclNode(c);
			TypeNode* varType = var.type.get_type(c);
			if (!varType.isPointer) goto default;
			TypeNode* base = varType.as_ptr.base.get_type(c);
			if (!base.isFuncSignature) goto default;
			IrIndex irIndex = getRvalue(gen, node.loc, currentBlock, var.irValue);
			return visitCall(gen, c.getAstNodeIndex(base), irIndex, currentBlock, nextStmt, node);
			goto default;
		default:
			c.internal_error(node.loc, "Cannot call %s", callee.get_node_type(c).get_type(c).printer(c));
			assert(false);
	}
}

ExprValue visitCall(ref IrGenState gen, AstIndex signatureIndex, IrIndex callee, IrIndex currentBlock, ref IrLabel nextStmt, CallExprNode* n)
{
	CompilationContext* c = gen.context;
	auto signature = signatureIndex.get!FunctionSignatureNode(c);
	uint numArgs = n.args.length;
	uint numParams = signature.parameters.length;
	c.assertf(numArgs+1 <= IrInstrHeader.MAX_ARGS,
		"Cannot generate a call with %s arguments, max args is %s",
		numArgs, IrInstrHeader.MAX_ARGS-1);

	// We need to allocate for each call, because calls can be nested
	IrIndex[] args = c.allocateTempArray!IrIndex(numParams + 1); // first argument is callee
	scope(exit) c.freeTempArray(args);

	args[0] = callee;

	if (callee.kind == IrValueKind.func)
	{
		// force creation of function type for external functions
		gen_ir_type(signatureIndex, c);
	}

	foreach (i, AstIndex arg; n.args)
	{
		IrLabel afterArg = IrLabel(currentBlock);
		ExpressionNode* node = arg.get_expr(c);
		ExprValue lval = ir_gen_expr(gen, arg, currentBlock, afterArg);
		currentBlock = afterArg.blockIndex;
		args[i+1] = getRvalue(gen, n.loc, currentBlock, lval); // account for callee in 0th index
		debug c.assertf(node.irValue.isDefined, "Arg %s %s (%s) is undefined", i+1, node.astType, c.tokenLoc(node.loc));
	}

	foreach(i; numArgs..numParams)
	{
		// use default argument value
		VariableDeclNode* param = c.getAst!VariableDeclNode(signature.parameters[i]);
		c.assertf(param.initializer.isDefined, param.loc, "Undefined default arg %s", c.idString(param.id));
		args[i+1] = param.gen_default_value_var(c); // account for callee in 0th index
	}

	// TODO: support more than plain func() calls. Such as func_array[42](), (*func_ptr)() etc
	// need handling of function pointers

	if (n.isLvalue) {
		c.internal_error(n.loc, "Call cannot be an l-value");
	}

	if (signature.returnType.isVoidType(c))
	{
		InstrWithResult res = gen.builder.emitInstr!(IrOpcode.call)(currentBlock, args);
		c.assertf(!res.result.isDefined, "Call has result");
		gen.builder.addJumpToLabel(currentBlock, nextStmt);
		return ExprValue();
	}
	else
	{
		IrIndex callResultType = signature.returnType.gen_ir_type(c);

		ExtraInstrArgs extra = { hasResult : true, type : callResultType };
		InstrWithResult res = gen.builder.emitInstr!(IrOpcode.call)(currentBlock, extra, args);
		gen.builder.addJumpToLabel(currentBlock, nextStmt);
		return ExprValue(res.result);
	}
}

ExprValue visitConstructor(ref IrGenState gen, StructDeclNode* s, IrIndex currentBlock, ref IrLabel nextStmt, CallExprNode* n)
{
	CompilationContext* c = gen.context;
	IrIndex structType = s.gen_ir_type_struct(c);
	uint numStructMembers = c.types.get!IrTypeStruct(structType).numMembers;
	IrIndex[] args = c.allocateTempArray!IrIndex(numStructMembers);
	scope(exit) c.freeTempArray(args);

	uint memberIndex;
	foreach(AstIndex member; s.declarations)
	{
		AstNode* memberVarNode = member.get_node(c);
		if (memberVarNode.astType != AstType.decl_var) continue;
		VariableDeclNode* memberVar = memberVarNode.as!VariableDeclNode(c);

		AstIndex initializer;
		if (n.args.length > memberIndex) // init from constructor argument
		{
			IrLabel afterArg = IrLabel(currentBlock);
			ExprValue lval = ir_gen_expr(gen, n.args[memberIndex], currentBlock, afterArg);
			currentBlock = afterArg.blockIndex;
			args[memberIndex] = getRvalue(gen, n.loc, currentBlock, lval);
		}
		else // init with initializer from struct definition
		{
			args[memberIndex] = memberVar.gen_default_value_var(c);
		}

		++memberIndex;
	}

	if (n.isLvalue) {
		c.internal_error(n.loc, "Constructor cannot be an l-value");
	}

	ExtraInstrArgs extra = { type : structType };
	InstrWithResult res = gen.builder.emitInstr!(IrOpcode.create_aggregate)(currentBlock, extra, args);
	return ExprValue(res.result);
}
