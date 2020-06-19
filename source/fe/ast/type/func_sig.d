/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.type.func_sig;

import all;

enum FuncSignatureFlags : ushort
{
	// Set if at least one of return or parameter types is meta type
	isCtfeOnly = AstFlags.userFlag << 0,
}

@(AstType.type_func_sig)
struct FunctionSignatureNode {
	mixin AstNodeData!(AstType.type_func_sig, AstFlags.isType, AstNodeState.name_register_self_done);
	AstIndex returnType;
	// parameters are owned by the function declaration or
	// if it is part of function type literal then there is no owner
	AstNodes parameters; // array of var declarations
	ubyte numDefaultArgs;
	CallConvention callConvention = CallConvention.win64; // hardcoded for now
	IrIndex irType; /// Index of function type
	TypeNode* typeNode() { return cast(TypeNode*)&this; }

	bool isCtfeOnly() { return cast(bool)(flags & FuncSignatureFlags.isCtfeOnly); }
}

void print_func_sig(FunctionSignatureNode* node, ref AstPrintState state)
{
	state.print("TYPE ", node.typeNode.printer(state.context), node.isCtfeOnly ? " #ctfe" : null);
}

void post_clone_func_sig(FunctionSignatureNode* node, ref CloneState state)
{
	state.fixAstIndex(node.returnType);
	state.fixAstNodes(node.parameters);
}

// only occurs when signature is a part of function declaration
void name_register_nested_func_sig(FunctionSignatureNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.parameters, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_func_sig(FunctionSignatureNode* node, ref NameResolveState state)
{
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.returnType, state);
	uint variadicIndex;
	bool hasVariadic = false;
	foreach(size_t i, ref AstIndex paramIndex; node.parameters)
	{
		require_name_resolve(paramIndex, state);

		auto param = paramIndex.get!VariableDeclNode(state.context);
		if (param.isVariadicParam) {
			hasVariadic = true;
			variadicIndex = cast(uint)i;
		}
	}
	if (hasVariadic) expandVariadicParam(node, variadicIndex, state.context);
	node.state = AstNodeState.name_resolve_done;
}

void expandVariadicParam(FunctionSignatureNode* node, uint variadicIndex, CompilationContext* c)
{
	auto param = node.parameters[variadicIndex].get!VariableDeclNode(c);
	auto types = param.type.get_effective_node(c).get!AliasArrayDeclNode(c);

	uint numVariadicParams = types.items.length;
	node.parameters.replaceAt(c.arrayArena, variadicIndex, 1, types.items);

	AstNodes vars;
	vars.reserve(c.arrayArena, numVariadicParams);

	foreach(size_t i, AstIndex type; types.items)
	{
		AstIndex newParamIndex = c.appendAst!VariableDeclNode(param.loc, AstIndex.init, type, AstIndex.init, param.id, cast(ushort)(param.scopeIndex + i));
		auto newParam = newParamIndex.get!VariableDeclNode(c);
		newParam.flags |= VariableFlags.isParameter;
		newParam.state = AstNodeState.name_resolve_done;
		node.parameters[variadicIndex + i] = newParamIndex;
		vars.put(c.arrayArena, newParamIndex);
	}

	// update indicies of other params
	foreach(size_t i; variadicIndex + numVariadicParams..node.parameters.length)
	{
		auto rtParam = node.parameters[i].get!VariableDeclNode(c);
		rtParam.scopeIndex = cast(ushort)(rtParam.scopeIndex + numVariadicParams - 1);
	}

	// rewrite variadic parameter as array literal in-place
	static assert(VariableDeclNode.sizeof >= AliasArrayDeclNode.sizeof,
		"VariableDeclNode.sizeof < AliasArrayDeclNode.sizeof");
	auto arrayNode = cast(AliasArrayDeclNode*)param;
	*arrayNode = AliasArrayDeclNode(param.loc, vars);
}

// Parameters consist of 4 groups:
// 1) 0+, non-variadic, non-default
// 2) 0+, variadic
// 3) 0+, non-variadic, non-default
// 4) 0+, non-variadic, default
void type_check_func_sig(FunctionSignatureNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;

	require_type_check(node.returnType, state);
	check_is_type(node.returnType, c);

	TypeNode* returnType = node.returnType.get_type(c);
	if (returnType.isOpaqueStruct(c)) {
		c.error(node.loc,
			"function cannot return opaque type `%s`",
			returnType.printer(c));
	}

	require_type_check(node.parameters, state);

	if (caclIsCtfeOnly(node, c)) node.flags |= FuncSignatureFlags.isCtfeOnly;

	node.state = AstNodeState.type_check_done;
}

private bool caclIsCtfeOnly(FunctionSignatureNode* node, CompilationContext* c) {
	if (node.returnType.get_node_type(c).isMetaType(c)) return true;
	foreach (AstIndex param; node.parameters) {
		if (param.get_node_type(c).isMetaType(c)) return true;
	}
	return false;
}

bool same_type_func_sig(FunctionSignatureNode* t1, FunctionSignatureNode* t2, CompilationContext* c)
{
	if (!same_type(t1.returnType, t2.returnType, c)) return false;
	if (t1.parameters.length != t2.parameters.length) return false;
	foreach (i, AstIndex paramA; t1.parameters)
	{
		AstIndex paramB = t2.parameters[i];
		if (!same_type(paramA.get_node_type(c), paramB.get_node_type(c), c)) return false;
	}
	return true;
}

IrIndex gen_ir_type_func_sig(FunctionSignatureNode* node, CompilationContext* c)
	out(res; res.isTypeFunction, "Not a function type")
{
	if (node.irType.isDefined) return node.irType;

	uint numResults = 0;

	if (!c.getAst!TypeNode(node.returnType).isVoid) numResults = 1;

	node.irType = c.types.appendFuncSignature(numResults, node.parameters.length, node.callConvention);
	auto funcType = &c.types.get!IrTypeFunction(node.irType);

	if (numResults == 1) {
		IrIndex returnType = node.returnType.gen_ir_type(c);
		funcType.resultTypes[0] = returnType;
	}

	IrIndex[] parameterTypes = funcType.parameterTypes;
	foreach(i, AstIndex parameter; node.parameters) {
		parameterTypes[i] = parameter.get_node_type(c).gen_ir_type(c);
	}

	return node.irType;
}
