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
module vox.fe.ast.expr.call;

import vox.all;

enum CallExprFlags : ushort
{
	hasNamedArgs = AstFlags.userFlag << 0,
}

@(AstType.expr_call)
struct CallExprNode {
	mixin ExpressionNodeData!(AstType.expr_call);
	ScopeIndex parentScope; // needed to resolve `this` pointer in member access
	AstIndex callee;
	AstNodes args;
	IrIndex[] argsValues;

	bool hasNamedArgs() { return cast(bool)(flags & CallExprFlags.hasNamedArgs); }
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
	state.fixScope(node.parentScope);
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

	AstNodes templateArgs;
	AstIndex callee = node.callee.get_effective_node(c);

	start: switch (callee.astType(c))
	{
		// static function call
		case AstType.decl_function:
			auto func = callee.get!FunctionDeclNode(c);
			if (func.isMember) {
				// rewrite as method[](args) as method[](this, args)
				// let member_access handle everything else
				AstIndex thisName = c.appendAst!NameUseExprNode(node.loc, node.parentScope, CommonIds.id_this);
				require_name_resolve(thisName, c);
				node.args.putFront(c.arrayArena, thisName);
			}
			auto signature = func.signature.get!FunctionSignatureNode(c);
			return type_check_func_call(node, signature, func.id, state);

		case AstType.decl_struct:
			require_type_check(callee, state, IsNested.no);
			return type_check_constructor_call(node, callee.get!StructDeclNode(c), state);

		case AstType.expr_member:
			MemberExprNode* memberNode = callee.get!MemberExprNode(c);
			// Method call
			LookupResult res = lookupMember(callee, memberNode, state);
			if (res == LookupResult.success) {
				AstIndex memberIndex = memberNode.member(c);

				if (memberNode.subType == MemberSubType.struct_templ_method) {
					memberIndex = get_template_instance(memberIndex, node.loc, templateArgs, state);

					if (memberIndex == CommonAstNodes.node_error) {
						node.type = CommonAstNodes.type_error;
						return;
					}
				}

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
				if (memberNode.member(c).get_node(c).isMember) {
					lowerThisArgument(signature, memberNode.aggregate, memberNode.loc, c);
					node.args.putFront(c.arrayArena, memberNode.aggregate);
				}
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
			auto index = callee.get!IndexExprNode(c);
			AstIndex effective_array = index.array.get_effective_node(c);
			AstType array_ast_type = effective_array.astType(c);

			require_type_check(index.indices, state);

			if (array_ast_type == AstType.decl_template)
			{
				// template instantiation
				callee = get_template_instance(effective_array, node.loc, index.indices, state);
				if (callee == CommonAstNodes.node_error) {
					node.type = CommonAstNodes.type_error;
					return;
				}
				node.callee = callee;

				switch(callee.astType(c)) {
					case AstType.decl_function:
					case AstType.decl_struct:
						goto start;
					default:
						c.internal_error(node.loc, "Unexpected template type %s", callee.astType(c));
				}
			}

			if (array_ast_type == AstType.expr_member)
			{
				templateArgs = index.indices;
				callee = effective_array;
				node.callee = callee;
				goto case AstType.expr_member;
			}

			goto case AstType.decl_var;
		case AstType.decl_template:
			auto templ = callee.get!TemplateDeclNode(c);

			//writefln("%s %s", templ.isMember, templ.body.get_node(c).isMember);

			if (templ.body.astType(c) != AstType.decl_function) {
				c.unrecoverable_error(node.loc, "Cannot call template of %s", templ.body.astType(c));
			}

			c.assertf(!node.hasNamedArgs, node.loc, "Named arguments with variadics are not implemented");

			auto templFunc = templ.body.get!FunctionDeclNode(c);
			auto templSignature = templFunc.signature.get!FunctionSignatureNode(c);

			if (templ.isMember) {
				// rewrite as method[](args) as method[](this, args)
				// let member_access handle everything else
				AstIndex thisName = c.appendAst!NameUseExprNode(node.loc, node.parentScope, CommonIds.id_this);
				require_name_resolve(thisName, c);
				node.args.putFront(c.arrayArena, thisName);
			}

			bool success;
			AstNodes types = doIfti(node.loc, templ, templSignature, node.args, success, state);
			if (!success)
				c.unrecoverable_error(node.loc, "Cannot infer template arguments from runtime arguments");

			callee = get_template_instance(callee, node.loc, types, state);
			node.callee = callee;

			if (callee == CommonAstNodes.node_error) {
				node.type = CommonAstNodes.type_error;
				return;
			}

			auto func = callee.get!FunctionDeclNode(c);
			auto signature = func.signature.get!FunctionSignatureNode(c);
			return type_check_func_call(node, signature, func.id, state);
		default:
			// unknown / unimplemented case
			node.type = CommonAstNodes.type_error;
			c.error(node.loc, "Cannot call %s", get_node_kind_name(callee, c));
			//c.internal_error(node.loc,
			//	"Only direct function calls are supported right now");
			return;
	}
}


// Returns inferred template parameters
// args needs to be ref, because type-check needs it to be ref
AstNodes doIfti(TokenIndex loc, TemplateDeclNode* templ, FunctionSignatureNode* sig, ref AstNodes rt_args, out bool success, ref TypeCheckState state)
{
	// - run through all provided runtime args
	// - get type of runtime parameter, if it is template type arg (T or T... here), then
	//   - if it is variadic template parameter T..., this and all subsequent variadic runtime argument types are appended
	//     after non-variadic template parameters, then continue walking non-variadics
	//   - otherwise it is type parameter T
	//     - if no type is given yet, assign runtime arg type
	//     - otherwise find common type between two
	// - at the end run through all template args, and error if any arg is not inferred.

	// signature has 0 or 1 expanded parameter
	// template has 0 or 1 variadic parameter

	CompilationContext* c = state.context;

	AstNodes ct_params = templ.parameters;
	AstNodes rt_params = sig.parameters;
	auto numRtParams = rt_params.length;
	auto numCtParams = ct_params.length;
	auto numRtArgs = rt_args.length;

	//writefln("numRtParams %s, numCtParams %s, numRtArgs %s, sig.hasExpandedParam %s, templ.hasVariadic %s",
	//		numRtParams, numCtParams, numRtArgs, sig.hasExpandedParam, templ.hasVariadic);

	// array of inferred types (TODO: not only types)
	AstNodes inferredTemplArgs;
	success = true;
	if (numCtParams == 0) return inferredTemplArgs;

	HashMap!(Identifier, AstIndex, Identifier.init) paramNames;

	inferredTemplArgs.reserve(c.arrayArena, max(rt_args.length, ct_params.length));

	foreach(uint i; 0..ct_params.length)
	{
		auto ctParam = ct_params[i].get!TemplateParamDeclNode(c);
		paramNames.put(c.arrayArena, ctParam.id, ct_params[i]);
		if (ctParam.isVariadic) break; // skip variadic and the rest of params
		inferredTemplArgs.put(c.arrayArena, AstIndex());
	}

	void reportCorrectExpandedParam(TokenIndex loc, Identifier rt_param_id)
	{
		if (templ.hasVariadic) {
			uint variadicIndex = templ.numParamsBeforeVariadic;
			auto ctParam = ct_params[variadicIndex].get!TemplateParamDeclNode(c);
			c.error(loc, "Should be `%s... %s`", c.idString(ctParam.id), c.idString(rt_param_id));
		}
	}

	if (templ.hasVariadic)
		c.assertf(templ.numParamsBeforeVariadic+1 == numCtParams, loc, "TODO: parameters after variadic template parameter");

	if (sig.hasExpandedParam)
	{
		if (!templ.hasVariadic) {
			uint variadicIndex = sig.numParamsBeforeVariadic;
			auto param = sig.parameters[variadicIndex].get!VariableDeclNode(c);
			c.unrecoverable_error(param.loc, "Variadic parameters are not implemented");
		}
	}
	else
	{
		if (numRtParams != numRtArgs)
		{
			foreach(AstIndex idx; rt_params) {
				VariableDeclNode* rt_param = idx.get!VariableDeclNode(c);
				AstNode* rt_type = rt_param.type.get_node(c);
				if (rt_type.astType != AstType.expr_name_use) continue;
				auto typeName = rt_type.as!NameUseExprNode(c);
				Identifier typeId = typeName.id(c);
				AstIndex symIndex = paramNames.get(typeId, AstIndex.init);
				if (!symIndex) continue;
				auto ctParam = symIndex.get!TemplateParamDeclNode(c);
				if (ctParam.isVariadic) {
					reportCorrectExpandedParam(rt_type.loc, rt_param.id);
					break;
				}
			}

			c.unrecoverable_error(loc,
				"Cannot infer template parameters. Number of runtime arguments (%s) does not match number of function parameters (%s)",
				numRtArgs, numRtParams);
		}
	}

	void processTemplatedArg(AstIndex argTypeIndex, TemplateParamDeclNode* ctParam)
	{
		//writefln("ct_arg%s %s <- rt_arg %s",
		//	ctParam.index, c.idString(ctParam.id),
		//	argTypeIndex.printer(c));

		if (inferredTemplArgs[ctParam.index].isUndefined)
		{
			// we found first use of template param. Assign type
			inferredTemplArgs[ctParam.index] = argTypeIndex;
		}
		else if (same_type(inferredTemplArgs[ctParam.index], argTypeIndex, c))
		{
			// ok
		}
		else
		{
			// calculate common type
			CommonTypeResult res = calcCommonType(inferredTemplArgs[ctParam.index], argTypeIndex, AstIndex(), AstIndex(), c);
			if (res.commonType.isErrorType) {
				c.error(loc, "Cannot infer template parameter %s, from argument types %s and %s",
					c.idString(ctParam.id),
					inferredTemplArgs[ctParam.index].printer(c),
					argTypeIndex.printer(c),
					);
				success = false;
			}
			inferredTemplArgs[ctParam.index] = res.commonType;
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
			require_type_check(rt_args[i], state); // do not cache args, because require_type_check may modify it
			AstIndex argTypeIndex = rt_args[i].get_expr_type(c);
			inferredTemplArgs.put(c.arrayArena, argTypeIndex);
		}

		// group 3
		foreach(size_t i; rt_group12_size..numRtArgs)
		{
			require_type_check(rt_args[i], state); // do not cache args, because require_type_check may modify it
			AstIndex argTypeIndex = rt_args[i].get_expr_type(c);
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
	foreach(size_t i, ref AstIndex arg; rt_args)
	{
		require_type_check(arg, state); // do not cache arg, because require_type_check may modify it
		AstIndex argTypeIndex = arg.get_expr_type(c);
		assert(i < numRtParams);

		VariableDeclNode* rt_param = rt_params[i].get!VariableDeclNode(c);
		AstNode* rt_type = rt_param.type.get_node(c);

		if (rt_type.astType == AstType.expr_name_use)
		{
			auto typeName = rt_type.as!NameUseExprNode(c);
			Identifier typeId = typeName.id(c);
			AstIndex symIndex = paramNames.get(typeId, AstIndex.init);
			if (symIndex)
			{
				// we got template parameter
				auto ctParam = symIndex.get!TemplateParamDeclNode(c);

				if (ctParam.isVariadic) {
					if (!rt_param.isVariadicParam) {
						reportCorrectExpandedParam(rt_param.loc, rt_param.id);
						c.unrecoverable_error(rt_param.loc, "Cannot expand non-variadic template parameter %s", c.idString(typeId));
					}
					// this and the rest of runtime args are going into CT variadic param
					processVariadicRtArgs(i);
					break;
				}

				if (rt_param.isVariadicParam) {
					reportCorrectExpandedParam(rt_param.loc, rt_param.id);
					c.unrecoverable_error(rt_param.loc, "Cannot expand non-variadic template parameter %s", c.idString(typeId));
				}

				processTemplatedArg(argTypeIndex, ctParam);
			}
			else if (rt_param.isVariadicParam)
			{
				reportCorrectExpandedParam(rt_param.loc, rt_param.id);
				c.unrecoverable_error(rt_param.loc, "Cannot expand parameter, type name is not found among template parameters");
			}
		}
		else if (rt_param.isVariadicParam)
		{
			reportCorrectExpandedParam(rt_param.loc, rt_param.id);
			c.unrecoverable_error(rt_param.loc, "Cannot expand parameter");
		}
	}

	// check that all parameters before variadic were inferred
	foreach(uint i; 0..templ.numParamsBeforeVariadic) {
		AstIndex param = inferredTemplArgs[i];
		if (param.isDefined) continue;

		auto ctParam = ct_params[i].get!TemplateParamDeclNode(c);
		c.error(loc, "Cannot infer template parameter %s", c.idString(ctParam.id));
		success = false;
	}

	return inferredTemplArgs;
}

void type_check_func_call(CallExprNode* node, FunctionSignatureNode* signature, Identifier funcId, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	AstIndex signatureIndex = c.getAstNodeIndex(signature);
	require_type_check(signatureIndex, state, IsNested.no);
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
			c.error(node.loc, "Too few arguments to `%s`, got %s, expected %s",
				c.idString(funcId), numArgs, numParams);
		else
			c.error(node.loc, "Too few arguments to `%s`, got %s, expected %s-%s",
				c.idString(funcId), numArgs, numRequiredArgs, numParams);
		return;
	}
	else if (numArgs > numParams) {
		if (numDefaultArgs == 0)
			c.error(node.loc, "Too many arguments to `%s`, got %s, expected %s",
				c.idString(funcId), numArgs, numParams);
		else
			c.error(node.loc, "Too many arguments to `%s`, got %s, expected %s-%s",
				c.idString(funcId), numArgs, numRequiredArgs, numParams);

		return;
	}

	// We need to allocate for each call, because calls can be nested
	// We allocate here instead of in IR gen, because eval cannot happen at IR gen,
	// and we need to call gen_init_value_var for default args, which may need to eval
	node.argsValues = c.allocateTempArray!IrIndex(numParams + 1); // first argument is callee

	// For named arguments
	Scope* parameterScope;
	if (node.hasNamedArgs && numParams > 0) {
		parameterScope = params[0].get!VariableDeclNode(c).parentScope.get_scope(c);
	}

	uint parameterIndex = 0;
	bool skipPositionalArgs = false;

	// check arguments
	// default arguments do not require checking here
	foreach (i, ref AstIndex arg; node.args)
	{
		require_type_check(arg, state);

		AstNode* argNode = arg.get_node(c);

		if (argNode.astType == AstType.expr_named_argument)
		{
			NamedArgumenExprNode* namedArg = argNode.as!NamedArgumenExprNode(c);
			Identifier argId = namedArg.getId(c);
			AstIndex paramIndex = parameterScope.symbols.get(argId, AstIndex.init);
			if (paramIndex.isUndefined) {
				c.error(namedArg.loc, "Function `%s` has no parameter named `%s`", c.idString(funcId), c.idString(argId));
				// we don't know the index of the next parameter, so we cannot tell which parameter is missing
				skipPositionalArgs = true;
				continue;
			}
			// restore reporting after we found a correct named parameters, since we know indicies after it
			skipPositionalArgs = false;
			VariableDeclNode* param = c.getAst!VariableDeclNode(paramIndex);
			// remember the index for IR generation
			namedArg.resolve(param.scopeIndex, c);
			parameterIndex = param.scopeIndex;

			bool success = autoconvTo(namedArg.expr, param.type, c);
			if (!success)
				c.error(namedArg.expr.loc(c),
					"Named argument %s, must have type %s, not %s", c.idString(argId),
					param.type.printer(c),
					namedArg.expr.get_expr_type(c).printer(c));
			if (node.argsValues[parameterIndex+1] == IrIndex(1, IrValueKind.none)) {
				c.error(namedArg.loc, "Parameter `%s` provided several times", c.idString(argId));
			}
			node.argsValues[parameterIndex+1] = IrIndex(1, IrValueKind.none); // mark argument as set
		}
		else
		{
			if (skipPositionalArgs) continue;

			if (parameterIndex >= numParams) {
				c.error(arg.loc(c), "Trying to provide parameter %s, while `%s` has %s parameters", parameterIndex+1, c.idString(funcId), numParams);
				++parameterIndex;
				continue;
			}
			VariableDeclNode* param = c.getAst!VariableDeclNode(params[parameterIndex]);

			bool success = autoconvTo(arg, param.type, c);
			if (!success)
				c.error(arg.loc(c),
					"Argument %s, must have type %s, not %s", parameterIndex+1,
					param.type.printer(c),
					arg.get_expr_type(c).printer(c));
			if (node.argsValues[parameterIndex+1] == IrIndex(1, IrValueKind.none)) {
				c.error(argNode.loc, "Parameter `%s` was provided several times", c.idString(param.id));
			}
			node.argsValues[parameterIndex+1] = IrIndex(1, IrValueKind.none); // mark argument as set
		}
		++parameterIndex;
	}

	foreach(i; 0..numParams)
	{
		// eval default argument values
		VariableDeclNode* param = c.getAst!VariableDeclNode(params[i]);
		if (node.argsValues[i+1].isDefined) continue;
		if (param.initializer.isUndefined) {
			if (param.isAnonymous)
				c.error(node.loc, "Missing argument for anonymous parameter %s", i+1);
			else
				c.error(node.loc, "Missing argument for parameter %s: `%s`", i+1, c.idString(param.id));
			continue;
		}

		AstNode* initializer = param.initializer.get_node(c);
		if (initializer.astType == AstType.literal_special) {
			node.argsValues[i+1] = eval_literal_special(cast(SpecialKeyword)initializer.subType, node.loc, node.parentScope, c);
		} else {
			node.argsValues[i+1] = param.gen_init_value_var(c);
		}
	}
}

void type_check_constructor_call(CallExprNode* node, StructDeclNode* s, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	IrIndex structType = s.gen_ir_type_struct(c);
	uint numStructMembers = c.types.get!IrTypeStruct(structType).numMembers;
	node.argsValues = c.allocateTempArray!IrIndex(numStructMembers);

	if (s.isUnion) {
		if (node.args.length > 1)
			c.error(node.loc, "union constructor must have a single argument, not %s", node.args.length);
	} else if (node.args.length > numStructMembers) {
		c.error(node.loc, "cannot initialize struct `%s` with %s arguments, it has %s members",
			c.idString(s.id), node.args.length, numStructMembers);
	}

	void checkArgument(VariableDeclNode* memberVar, ref AstIndex arg)
	{
		AstIndex memberType = memberVar.type;
		bool success = autoconvTo(arg, memberType, c);
		if (!success) {
			c.error(arg.loc(c),
				"argument for member `%s`, must have type %s, not %s", c.idString(memberVar.id),
				memberType.printer(c),
				arg.get_expr_type(c).printer(c));
		}
	}

	uint memberIndex;
	bool reportedMixed;

	// We expect args to be either all positional or all named
	if (node.hasNamedArgs)
	{
		foreach (i, ref AstIndex arg; node.args)
		{
			require_type_check(arg, state);

			AstNode* argNode = arg.get_node(c);

			if (argNode.astType != AstType.expr_named_argument) {
				if (!reportedMixed)
					c.error(argNode.loc, "named and positional arguments cannot be mixed in struct constructor");
				reportedMixed = true;
				continue;
			}

			NamedArgumenExprNode* namedArg = argNode.as!NamedArgumenExprNode(c);
			Identifier argId = namedArg.getId(c);

			AstIndex memberNodeIndex = s.memberScope.lookup_scope(argId, c);
			if (memberNodeIndex.isUndefined) {
				c.error(namedArg.loc, "%s `%s` has no member named `%s`", s.structOrUnionString, c.idString(s.id), c.idString(argId));
				continue;
			}

			if (!isDynamicStructMember(memberNodeIndex, c)) {
				c.error(namedArg.loc, "cannot initialize %s `%s` of %s `%s`", get_node_kind_name(memberNodeIndex, c), c.idString(argId), s.structOrUnionString, c.idString(s.id));
				continue;
			}

			VariableDeclNode* memberVar = memberNodeIndex.get!VariableDeclNode(c);
			namedArg.resolve(memberVar.scopeIndex, c);
			checkArgument(memberVar, namedArg.expr);
		}
	}
	else
	{
		foreach(uint i, AstIndex member; StructDynMemberIterator(s, c))
		{
			if (node.args.length == memberIndex) break;

			VariableDeclNode* memberVar = member.get!VariableDeclNode(c);
			require_type_check(node.args[memberIndex], state);

			checkArgument(memberVar, node.args[memberIndex]);
			++memberIndex;
		}
	}

	// will iterate the rest of members when positional or all members when named
	foreach(ref AstIndex member; s.declarations[memberIndex..$])
	{
		if (!isDynamicStructMember(member, c)) continue;
		VariableDeclNode* memberVar = member.get!VariableDeclNode(c);
		// init with initializer from struct definition
		node.argsValues[memberIndex] = memberVar.gen_init_value_var(c);
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
			IrIndex irIndex = callee.get!EnumMemberDecl(c).gen_init_value_enum_member(c);
			return visitCall(gen, c.getAstNodeIndex(base), irIndex, currentBlock, nextStmt, node);
		case AstType.decl_var:
			VariableDeclNode* var = callee.get!VariableDeclNode(c);
			TypeNode* varType = var.type.get_type(c);
			if (!varType.isPointer) goto default;
			TypeNode* base = varType.as_ptr.base.get_type(c);
			if (!base.isFuncSignature) goto default;

			IrIndex irIndex = var.irValue.rvalue(gen, node.loc, currentBlock);
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
			IrIndex calleeRval = calleeLval.rvalue(gen, node.loc, currentBlock);

			return visitCall(gen, c.getAstNodeIndex(base), calleeRval, currentBlock, nextStmt, node);
		default:
			c.internal_error(node.loc, "Cannot call %s", callee.get_node_type(c).get_type(c).printer(c));
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

	uint parameterIndex = 0;

	foreach (i, AstIndex arg; n.args)
	{
		AstNode* argNode = arg.get_node(c);
		if (argNode.astType == AstType.expr_named_argument)
		{
			NamedArgumenExprNode* namedArg = argNode.as!NamedArgumenExprNode(c);
			arg = namedArg.expr;
			parameterIndex = namedArg.getParamIndex(c);
			IrLabel afterArg = IrLabel(currentBlock);
			ExprValue lval = ir_gen_expr(gen, namedArg.expr, currentBlock, afterArg);
			currentBlock = afterArg.blockIndex;
			n.argsValues[parameterIndex+1] = lval.rvalue(gen, n.loc, currentBlock); // account for callee in 0th index
		}
		else
		{
			IrLabel afterArg = IrLabel(currentBlock);
			ExprValue lval = ir_gen_expr(gen, arg, currentBlock, afterArg);
			currentBlock = afterArg.blockIndex;
			n.argsValues[parameterIndex+1] = lval.rvalue(gen, n.loc, currentBlock); // account for callee in 0th index
		}
		debug c.assertf(n.argsValues[parameterIndex+1].isDefined, "Arg %s %s (%s) is undefined", parameterIndex+1, n.astType, c.tokenLoc(n.loc));
		++parameterIndex;
	}

	// default args are populated in type check

	// TODO: support more than plain func() calls. Such as func_array[42](), (*func_ptr)() etc
	// need handling of function pointers

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

ExprValue visitConstructor(ref IrGenState gen, StructDeclNode* s, IrIndex currentBlock, ref IrLabel nextStmt, CallExprNode* node)
{
	CompilationContext* c = gen.context;

	uint numArgs = node.args.length;
	uint numMembers = cast(uint)node.argsValues.length;

	if (numArgs == 0)
	{
		return ExprValue(s.gen_init_value_struct(c));
	}

	bool allConstants = true;
	bool allZeroes = true;

	if (node.hasNamedArgs)
	{
		foreach (i, ref AstIndex arg; node.args)
		{
			NamedArgumenExprNode* namedArg = arg.get!NamedArgumenExprNode(c);

			IrLabel afterArg = IrLabel(currentBlock);
			ExprValue lval = ir_gen_expr(gen, namedArg.expr, currentBlock, afterArg);
			currentBlock = afterArg.blockIndex;
			IrIndex memberValue = lval.rvalue(gen, node.loc, currentBlock);

			if (memberValue.isVirtReg) allConstants = false;
			if (!memberValue.isConstantZero) allZeroes = false;
			node.argsValues[namedArg.getParamIndex(c)] = memberValue;
		}
	}
	else
	{
		uint memberIndex;
		foreach(AstIndex member; s.declarations)
		{
			if (!isDynamicStructMember(member, c)) continue;
			if (numArgs == memberIndex) break; // no more positional args provided, others are default inited

			IrLabel afterArg = IrLabel(currentBlock);
			ExprValue lval = ir_gen_expr(gen, node.args[memberIndex], currentBlock, afterArg);
			currentBlock = afterArg.blockIndex;
			IrIndex memberValue = lval.rvalue(gen, node.loc, currentBlock);

			if (memberValue.isVirtReg) allConstants = false;
			if (!memberValue.isConstantZero) allZeroes = false;
			node.argsValues[memberIndex] = memberValue;

			++memberIndex;
		}
	}

	// check the rest of args for all zeroes
	if (allConstants)
	foreach (uint i; numArgs..numMembers) {
		if (!node.argsValues[i].isConstantZero) {
			allZeroes = false;
			break;
		}
	}

	if (node.isLvalue) {
		c.internal_error(node.loc, "Constructor cannot be an l-value");
	}
	assert(s.irType.isDefined);

	if (allZeroes)
	{
		return ExprValue(c.constants.addZeroConstant(s.irType));
	}
	else if (allConstants)
	{
		return ExprValue(c.constants.addAggrecateConstant(s.irType, node.argsValues));
	}
	else
	{
		ExtraInstrArgs extra = { type : s.irType };
		InstrWithResult res = gen.builder.emitInstr!(IrOpcode.create_aggregate)(currentBlock, extra, node.argsValues);
		return ExprValue(res.result);
	}
}
