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
	IrIndex[] argsValues;
}

void print_call(CallExprNode* node, ref AstPrintState state)
{
	if (node.callee && node.callee.astType(state.context) == AstType.decl_function)
	{
		state.print("CALL ", state.context.idString(node.callee.get_node_id(state.context)));
	}
	else
	{
		state.print("CALL");
		print_ast(node.callee, state);
	}
	print_ast(node.args, state);
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
			MemberExprNode* memberNode = callee.get!MemberExprNode(c);
			// Method call
			LookupResult res = lookupMember(callee, memberNode, state);
			if (res == LookupResult.success) {
				AstIndex memberIndex = memberNode.member(c);
				auto calleeType = memberIndex.get_type(c);
				if (calleeType.isPointer)
				{
					TypeNode* base = calleeType.as_ptr.base.get_type(c);
					if (base.isFuncSignature)
					{
						auto signature = base.as_func_sig;
						return type_check_func_call(node, signature, memberNode.memberId(c), state);
					}
				}
				node.callee = memberIndex;
				auto signature = calleeType.as_func_sig;
				assert(signature);
				lowerThisArgument(signature, memberNode.aggregate, memberNode.loc, c);
				node.args.putFront(c.arrayArena, memberNode.aggregate);
				return type_check_func_call(node, signature, memberNode.memberId(c), state);
			}
			// UFCS call
			Identifier calleeName = memberNode.memberId(c);
			LookupResult ufcsRes = tryUFCSCall(callIndex, memberNode, state);
			if (ufcsRes == LookupResult.failure) {
				AstIndex objType = memberNode.aggregate.get_node_type(c);
				node.type = CommonAstNodes.type_error;
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
			if (varType.isAlias)
			{
				if (callee.astType(c) == AstType.decl_var) goto default;
				auto enumMember = callee.get!EnumMemberDecl(c);
				node.callee = eval_static_expr_alias(enumMember.initializer, c);
				callee = node.callee; // used in case decl_function
				goto case AstType.decl_function;
			}
			goto default;
		case AstType.expr_index:
			require_type_check(callee, state);
			if (callee.astType(c) != AstType.decl_function)
				goto case AstType.decl_var;
			node.callee = callee;
			goto case AstType.decl_function;
		case AstType.decl_template:
			auto templ = callee.get!TemplateDeclNode(c);
			if (templ.body.astType(c) != AstType.decl_function) {
				c.unrecoverable_error(node.loc, "Cannot call template of %s", templ.body.astType(c));
			}
			auto func = templ.body.get!FunctionDeclNode(c);
			auto signature = func.signature.get!FunctionSignatureNode(c);
			bool success;
			AstNodes types = doIfti(templ, signature, node.args, success, state);
			if (!success)
				c.unrecoverable_error(node.loc, "Cannot infer template arguments from runtime arguments");
			callee = get_template_instance(callee, node.loc, types, state);
			node.callee = callee;
			if (callee == CommonAstNodes.node_error) {
				node.type = CommonAstNodes.type_error;
				return;
			}
			goto case AstType.decl_function;
		default:
			// unknown / unimplemented case
			node.type = CommonAstNodes.type_error;
			c.error(node.loc, "Cannot call %s", callee.astType(c));
			c.internal_error(node.loc,
				"Only direct function calls are supported right now");
	}
}


// Returns inferred template parameters
AstNodes doIfti(TemplateDeclNode* templ, FunctionSignatureNode* sig, AstNodes args, out bool success, ref TypeCheckState state)
{
	// - run through all provided runtime args
	// - get type of runtime parameter, if it is template type arg (T or T... here), then
	//   - if it is variadic template parameter T..., this and all subsequent variadic runtime argument types are appended
	//     after non-variadic template parameters, then continue walking non-variadics
	//   - otherwise it is type parameter T
	//     - if no type is given yet, assign runtime arg type
	//     - otherwise find common type between two
	// - at the end run through all template args, and error if any arg is not inferred.

	CompilationContext* c = state.context;

	AstNodes ct_params = templ.parameters;
	AstNodes rt_params = sig.parameters;
	auto numRtParams = rt_params.length;
	auto numCtParams = ct_params.length;
	auto numRtArgs = args.length;

	AstNodes result;
	success = true;
	if (numCtParams == 0) return result;

	HashMap!(Identifier, AstIndex, Identifier.init) paramNames;

	result.reserve(c.arrayArena, max(args.length, ct_params.length));

	// only 0 or 1 variadic CT parameters allowed
	// number of type ct params before variadic ct param
	bool hasVariadic = templ.numParamsBeforeVariadic < numCtParams;

	foreach(uint i; 0..ct_params.length)
	{
		auto ctParam = ct_params[i].get!TemplateParamDeclNode(c);
		paramNames.put(c.arrayArena, ctParam.id, ct_params[i]);
		if (ctParam.isVariadic) break; // skip variadic and the rest of params
		result.put(c.arrayArena, AstIndex());
	}

	void processTemplatedArg(AstIndex argTypeIndex, TemplateParamDeclNode* ctParam)
	{
		//writefln("ct_arg%s %s <- rt_arg %s",
		//	ctParam.index, c.idString(ctParam.id),
		//	argTypeIndex.printer(c));

		if (result[ctParam.index].isUndefined)
		{
			// we found first use of template param. Assign type
			result[ctParam.index] = argTypeIndex;
		}
		else if (same_type(result[ctParam.index], argTypeIndex, c))
		{
			// ok
		}
		else
		{
			// calculate common type
			CommonTypeResult res = calcCommonType(result[ctParam.index], argTypeIndex, c);
			if (res.commonType.isErrorType) {
				c.error("Cannot infer template parameter %s, from argument types %s and %s",
					c.idString(ctParam.id),
					result[ctParam.index].printer(c),
					argTypeIndex.printer(c),
					);
				success = false;
			}
			result[ctParam.index] = res.commonType;
		}
	}

	void processVariadicRtArgs(size_t group1_size)
	{
		uint group3_size = cast(uint)(numRtParams - group1_size - 1 - sig.numDefaultArgs);
		uint rt_group12_size = cast(uint)(numRtArgs - group3_size);
		uint rt_group2_size = cast(uint)(numRtArgs - group1_size - group3_size);
		//writefln("groups 1 %s 2 %s 3 %s 4 %s", group1_size, rt_group2_size, group3_size, sig.numDefaultArgs);

		// group 2
		foreach(size_t i; group1_size..rt_group12_size)
		{
			AstIndex arg = args[i];
			require_type_check(arg, state);
			AstIndex argTypeIndex = arg.get_expr_type(c);
			result.put(c.arrayArena, argTypeIndex);
		}

		// group 3
		foreach(size_t i; rt_group12_size..numRtArgs)
		{
			AstIndex arg = args[i];
			require_type_check(arg, state);
			AstIndex argTypeIndex = arg.get_expr_type(c);
			uint rtParamIndex = cast(uint)(i - rt_group2_size + 1); // 1 skips variadic param

			AstNode* rtType = rt_params[rtParamIndex].get!VariableDeclNode(c).type.get_node(c);

			if (rtType.astType == AstType.expr_name_use)
			{
				auto typeName = rtType.as!NameUseExprNode(c);
				Identifier typeId = typeName.id(c);
				AstIndex symIndex = paramNames.get(typeId, AstIndex.init);

				if (symIndex)
				{
					// we got template parameter
					auto ctParam = symIndex.get!TemplateParamDeclNode(c);
					assert(!ctParam.isVariadic);
					processTemplatedArg(argTypeIndex, ctParam);
				}
			}
		}
	}

	// group 1
	foreach(size_t i, ref AstIndex arg; args)
	{
		require_type_check(arg, state);
		AstIndex argTypeIndex = arg.get_expr_type(c);
		AstNode* rtType = rt_params[i].get!VariableDeclNode(c).type.get_node(c);
		if (rtType.astType == AstType.expr_name_use)
		{
			auto typeName = rtType.as!NameUseExprNode(c);
			Identifier typeId = typeName.id(c);
			AstIndex symIndex = paramNames.get(typeId, AstIndex.init);
			if (symIndex)
			{
				// we got template parameter
				auto ctParam = symIndex.get!TemplateParamDeclNode(c);

				if (ctParam.isVariadic) {
					// this and the rest of runtime args are going into CT variadic param
					processVariadicRtArgs(i);
					break;
				}

				processTemplatedArg(argTypeIndex, ctParam);
			}
		}
	}

	// check that all parameters before variadic were inferred
	bool reported;
	foreach(uint i; 0..templ.numParamsBeforeVariadic) {
		AstIndex param = result[i];
		if (param.isDefined) continue;

		auto ctParam = ct_params[i].get!TemplateParamDeclNode(c);
		c.error("Cannot infer template parameter %s", c.idString(ctParam.id));
		success = false;
	}

	return result;
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

	// We need to allocate for each call, because calls can be nested
	// We allocate here instead of in IR gen, because eval cannot happen at IR gen,
	// and we need to call gen_default_value_var for default args, which may need to eval
	node.argsValues = c.allocateTempArray!IrIndex(numParams + 1); // first argument is callee

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
				arg.get_expr_type(c).printer(c));
	}

	foreach(i; numArgs..numParams)
	{
		// eval default argument values
		VariableDeclNode* param = c.getAst!VariableDeclNode(params[i]);
		c.assertf(param.initializer.isDefined, param.loc, "Undefined default arg %s", c.idString(param.id));
		node.argsValues[i+1] = param.gen_default_value_var(c);
	}
}

void type_check_constructor_call(CallExprNode* node, StructDeclNode* s, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	IrIndex structType = s.gen_ir_type_struct(c);
	uint numStructMembers = c.types.get!IrTypeStruct(structType).numMembers;
	node.argsValues = c.allocateTempArray!IrIndex(numStructMembers);

	if (s.isUnion && node.args.length > 1) {
		c.error(node.loc, "Union constructor can only have single argument, not %s", node.args.length);
	}

	size_t memberIndex;
	foreach(AstIndex arg; s.declarations)
	{
		AstNode* member = arg.get_node(c);
		if (member.astType != AstType.decl_var) continue;
		VariableDeclNode* memberVar = member.as!VariableDeclNode(c);

		if (node.args.length > memberIndex) { // init from constructor argument
			require_type_check(node.args[memberIndex], state);
			AstIndex memberType = memberVar.type;
			bool success = autoconvTo(node.args[memberIndex], memberType, c);
			if (!success) {
				c.error(node.args[memberIndex].loc(c),
					"Argument %s, must have type %s, not %s", memberIndex+1,
					memberType.printer(c),
					node.args[memberIndex].get_expr_type(c).printer(c));
			}
		} else { // init with initializer from struct definition
			node.argsValues[memberIndex] = memberVar.gen_default_value_var(c);
		}
		++memberIndex;
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
			IrIndex irIndex = func.getIrIndex(c);
			return visitCall(gen, func.signature, irIndex, currentBlock, nextStmt, node);
		case AstType.decl_struct:
			return visitConstructor(gen, callee.get!StructDeclNode(c), currentBlock, nextStmt, node);
		case AstType.decl_enum_member:
			// TODO: clean up
			TypeNode* varType = callee.get_node_type(c).get_type(c);
			if (!varType.isPointer) goto default;
			TypeNode* base = varType.as_ptr.base.get_type(c);
			if (!base.isFuncSignature) goto default;
			IrIndex irIndex = callee.get!EnumMemberDecl(c).getInitVal(c);
			return visitCall(gen, c.getAstNodeIndex(base), irIndex, currentBlock, nextStmt, node);
		case AstType.decl_var:
			VariableDeclNode* var = callee.get!VariableDeclNode(c);
			TypeNode* varType = var.type.get_type(c);
			if (!varType.isPointer) goto default;
			TypeNode* base = varType.as_ptr.base.get_type(c);
			if (!base.isFuncSignature) goto default;

			IrIndex irIndex = getRvalue(gen, node.loc, currentBlock, var.irValue);
			return visitCall(gen, c.getAstNodeIndex(base), irIndex, currentBlock, nextStmt, node);
		case AstType.expr_member:
			// Can probably fold other cases into this one
			TypeNode* exprType = callee.get_expr_type(c).get_type(c);
			if (!exprType.isPointer) goto default;
			TypeNode* base = exprType.as_ptr.base.get_type(c);
			if (!base.isFuncSignature) goto default;

			IrLabel afterCallee = IrLabel(currentBlock);
			ExprValue calleeLval = ir_gen_expr(gen, callee, currentBlock, afterCallee);
			currentBlock = afterCallee.blockIndex;
			IrIndex calleeRval = getRvalue(gen, node.loc, currentBlock, calleeLval);

			return visitCall(gen, c.getAstNodeIndex(base), calleeRval, currentBlock, nextStmt, node);
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
	c.assertf(callee.isDefined, n.loc, "Undefined callee");
	c.assertf(numArgs+1 <= IrInstrHeader.MAX_ARGS, n.loc,
		"Cannot generate a call with %s arguments, max args is %s",
		numArgs, IrInstrHeader.MAX_ARGS-1);

	n.argsValues[0] = callee;
	bool alwaysInline;

	if (callee.kind == IrValueKind.func)
	{
		// force creation of function type for external functions
		gen_ir_type(signatureIndex, c);
		FunctionDeclNode* calleeFunc = c.getFunction(callee);
		alwaysInline = calleeFunc.isInline;
	}

	foreach (i, AstIndex arg; n.args)
	{
		IrLabel afterArg = IrLabel(currentBlock);
		ExpressionNode* nodeArg = arg.get_expr(c);
		ExprValue lval = ir_gen_expr(gen, arg, currentBlock, afterArg);
		currentBlock = afterArg.blockIndex;
		n.argsValues[i+1] = getRvalue(gen, n.loc, currentBlock, lval); // account for callee in 0th index
		debug c.assertf(n.argsValues[i+1].isDefined, "Arg %s %s (%s) is undefined", i+1, n.astType, c.tokenLoc(n.loc));
	}

	// default args are populated in type check

	// TODO: support more than plain func() calls. Such as func_array[42](), (*func_ptr)() etc
	// need handling of function pointers

	if (n.isLvalue) {
		c.internal_error(n.loc, "Call cannot be an l-value");
	}

	if (signature.returnType.isVoidType(c))
	{
		InstrWithResult res = gen.builder.emitInstr!(IrOpcode.call)(currentBlock, n.argsValues);
		gen.builder.ir.getInstr(res.instruction).alwaysInline = alwaysInline;
		c.assertf(!res.result.isDefined, "Call has result");
		gen.builder.addJumpToLabel(currentBlock, nextStmt);
		return ExprValue();
	}
	else if (signature.returnType.isNoreturnType(c))
	{
		InstrWithResult res = gen.builder.emitInstr!(IrOpcode.call)(currentBlock, n.argsValues);
		gen.builder.addUnreachable(currentBlock);
		gen.builder.ir.getInstr(res.instruction).alwaysInline = alwaysInline;
		c.assertf(!res.result.isDefined, "Call has result");
		return ExprValue();
	}
	else
	{
		IrIndex callResultType = signature.returnType.gen_ir_type(c);

		ExtraInstrArgs extra = { hasResult : true, type : callResultType };
		InstrWithResult res = gen.builder.emitInstr!(IrOpcode.call)(currentBlock, extra, n.argsValues);
		gen.builder.ir.getInstr(res.instruction).alwaysInline = alwaysInline;
		gen.builder.addJumpToLabel(currentBlock, nextStmt);
		return ExprValue(res.result);
	}
}

ExprValue visitConstructor(ref IrGenState gen, StructDeclNode* s, IrIndex currentBlock, ref IrLabel nextStmt, CallExprNode* n)
{
	CompilationContext* c = gen.context;

	uint numArgs = n.args.length;
	uint numMembers = cast(uint)n.argsValues.length;

	if (numArgs == 0)
	{
		return ExprValue(s.gen_default_value_struct(c));
	}

	bool allConstants = true;
	bool allZeroes = true;
	uint memberIndex;
	foreach(AstIndex member; s.declarations)
	{
		AstNode* memberVarNode = member.get_node(c);
		if (memberVarNode.astType != AstType.decl_var) continue;
		VariableDeclNode* memberVar = memberVarNode.as!VariableDeclNode(c);

		if (numArgs == memberIndex) break; // no more args provided, others are default inited

		IrLabel afterArg = IrLabel(currentBlock);
		ExprValue lval = ir_gen_expr(gen, n.args[memberIndex], currentBlock, afterArg);
		currentBlock = afterArg.blockIndex;
		IrIndex memberValue = getRvalue(gen, n.loc, currentBlock, lval);
		if (memberValue.isVirtReg) allConstants = false;
		if (!memberValue.isConstantZero) allZeroes = false;
		n.argsValues[memberIndex] = memberValue;

		++memberIndex;
	}

	// check the rest of args for all zeroes
	if (allConstants)
	foreach (uint i; numArgs..numMembers) {
		if (!n.argsValues[i].isConstantZero) {
			allZeroes = false;
			break;
		}
	}

	if (n.isLvalue) {
		c.internal_error(n.loc, "Constructor cannot be an l-value");
	}
	assert(s.irType.isDefined);

	if (allZeroes)
	{
		return ExprValue(c.constants.addZeroConstant(s.irType));
	}
	else if (allConstants)
	{
		return ExprValue(c.constants.addAggrecateConstant(s.irType, n.argsValues));
	}
	else
	{
		ExtraInstrArgs extra = { type : s.irType };
		InstrWithResult res = gen.builder.emitInstr!(IrOpcode.create_aggregate)(currentBlock, extra, n.argsValues);
		return ExprValue(res.result);
	}
}
