/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.type.func_sig;

import all;

@(AstType.type_func_sig)
struct FunctionSignatureNode {
	mixin AstNodeData!(AstType.type_func_sig, AstFlags.isType, AstNodeState.parse_done);
	AstIndex returnType;
	// parameters are owned by the function declaration or
	// if it is part of function type literal then there is no owner
	Array!AstIndex parameters; // array of var declarations
	IrIndex irType; /// Index of function type
	CallConvention callConvention = CallConvention.win64; // hardcoded for now
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
}

// only occurs when signature is a part of function declaration
void name_register_func_sig(FunctionSignatureNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	foreach (ref param; node.parameters) require_name_register(param, state);
	node.state = AstNodeState.name_register_done;
}

void name_resolve_func_sig(FunctionSignatureNode* node, ref NameResolveState state)
{
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.returnType, state);
	foreach(ref param; node.parameters)
		require_name_resolve(param, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_func_sig(FunctionSignatureNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;

	require_type_check(node.returnType, state);
	TypeNode* returnType = node.returnType.get_type(c);
	if (returnType.isOpaqueStruct(c)) {
		c.error(node.loc,
			"function cannot return opaque type `%s`",
			returnType.printer(c));
	}

	foreach(ref AstIndex param; node.parameters)
		require_type_check(param, state);

	node.state = AstNodeState.type_check_done;
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

IrIndex gen_ir_type_func_sig(FunctionSignatureNode* node, CompilationContext* context)
	out(res; res.isTypeFunction, "Not a function type")
{
	if (node.irType.isDefined) return node.irType;

	uint numResults = 0;
	if (!context.getAst!TypeNode(node.returnType).isVoid) numResults = 1;

	node.irType = context.types.appendFuncSignature(numResults, node.parameters.length, node.callConvention);
	auto funcType = &context.types.get!IrTypeFunction(node.irType);

	if (numResults == 1) {
		IrIndex returnType = node.returnType.gen_ir_type(context);
		funcType.resultTypes[0] = returnType;
	}

	IrIndex[] parameterTypes = funcType.parameterTypes;
	foreach(i, AstIndex parameter; node.parameters) {
		parameterTypes[i] = parameter.get_node_type(context).gen_ir_type(context);
	}

	return node.irType;
}
