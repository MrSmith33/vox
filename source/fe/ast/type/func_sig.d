/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.type.func_sig;

import all;

enum FuncSignatureFlags : ushort
{
	// Set if at least one of return or parameter types is meta type
	isCtfeOnly = AstFlags.userFlag << 0,
	// Function has parameter with expanded type
	hasExpandedParam = AstFlags.userFlag << 1,
	attachedToFunctionWithBody = AstFlags.userFlag << 2,
}

@(AstType.type_func_sig)
struct FunctionSignatureNode {
	mixin AstNodeData!(AstType.type_func_sig, AstFlags.isType, AstNodeState.name_register_self_done);
	AstIndex returnType;
	// parameters are owned by the function declaration or
	// if it is part of function type literal then there is no owner
	AstNodes parameters; // array of var declarations
	CallConvention callConvention; // Is set in the parser
	ubyte numDefaultArgs;
	/// Number of parameters before variadic parameter
	/// Is equal to parameters.length when no variadic is present
	ushort numParamsBeforeVariadic;

	private IrIndex irType; /// Index of function type
	TypeNode* typeNode() return { return cast(TypeNode*)&this; }

	IrIndex getIrType(CompilationContext* c) {
		gen_ir_type_func_sig(&this, c); // calculate if needed
		return irType;
	}

	bool isCtfeOnly() { return cast(bool)(flags & FuncSignatureFlags.isCtfeOnly); }
	bool hasExpandedParam() { return cast(bool)(flags & FuncSignatureFlags.hasExpandedParam); }
	bool attachedToFunctionWithBody() { return cast(bool)(flags & FuncSignatureFlags.attachedToFunctionWithBody); }
	bool isExternal() { return hasAttributes && attributeInfo.isExternal; }
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
	if (node.hasExpandedParam) expandVariadicParam(node, state.context);
}

void name_resolve_func_sig(FunctionSignatureNode* node, ref NameResolveState state)
{
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.returnType, state);
	require_name_resolve(node.parameters, state);
	node.state = AstNodeState.name_resolve_done;
}

// This happend when we are name resolving function signature inside template instance,
// so we already have final values for variadic template argument
void expandVariadicParam(FunctionSignatureNode* node, CompilationContext* c)
{
	uint variadicIndex = node.numParamsBeforeVariadic;
	auto param = node.parameters[variadicIndex].get!VariableDeclNode(c);
	// we are still in name register pass, but we need to get to the alias array
	require_name_resolve(param.type, c);

	auto types = param.type.get_effective_node(c).get!AliasArrayDeclNode(c);

	uint numVariadicParams = types.items.length;
	node.parameters.replaceAt(c.arrayArena, variadicIndex, 1, types.items[]);

	AstNodes vars;
	vars.reserve(c.arrayArena, numVariadicParams);

	foreach(size_t i, AstIndex type; types.items)
	{
		string originalId = c.idString(param.id);
		Identifier paramId = c.idMap.getOrRegFormatted(c, "__%s_%s", originalId, i);
		AstIndex newParamIndex = c.appendAst!VariableDeclNode(param.loc, AstIndex.init, type, AstIndex.init, paramId, cast(ushort)(param.scopeIndex + i));
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

	// ir_header
	gen_ir_header_func_sig(node, c);

	// Process @extern attribute
	process_externs(node, c);

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

// Validate @extern attributes, bind externals and update calling conventions
// depends on ir_header
void process_externs(FunctionSignatureNode* node, CompilationContext* c)
{
	// Check that function `isExternal` matches `isExternal` of signature
	if (!node.attachedToFunctionWithBody && !node.isExternal) {
		// allow until @extern(module) is implemented
		// TODO: we don't know atm if we are attached to function without body or it is free function type
	}

	if (!node.isExternal) return; // skip if no extern properties are bound to this node

	AstIndex lastExternAttrib;

	foreach(AstIndex attrib; node.attributeInfo.attributes)
	{
		auto attribNode = attrib.get_node(c);

		if (attribNode.astType != AstType.decl_builtin_attribute) continue;

		void onExternAttrib() {
			// check for duplicates
			if (lastExternAttrib.isDefined) {
				// check if previous @extern attribute is broadcasted
				// NOTE: all broadcasted attributes are located before direct attributes
				//       so we can detect multiple directly applied @extern attributes by detecting 2 consecutive @extern attributes
				if (!attribNode.as!BuiltinAttribNode(c).isBroadcasted)
				{
					if (lastExternAttrib.get!BuiltinAttribNode(c).isBroadcasted) {
						// allow duplicates in cases when only one @extern attribute is directly applied
						// and others are broadcast applied (like `@extern(...):` and `@extern(...){...}`)
					} else {
						c.error(attribNode.loc, "Duplicate @extern attribute");
					}
				}
			}
			lastExternAttrib = attrib; // save last @extern attribute
		}

		final switch(cast(BuiltinAttribSubType)attribNode.subType) {
			case BuiltinAttribSubType.extern_syscall:
				onExternAttrib();
				uint syscall_number = attribNode.as!BuiltinAttribNode(c).data;

				if (syscall_number > ushort.max) {
					c.error(attribNode.loc, "Max supported syscall number is 64k, got %s", syscall_number);
					return;
				}

				if (c.targetOs != TargetOs.linux) {
					c.error(attribNode.loc, "@extern(syscall) attribute is only implemented on linux");
					return;
				}
				break;

			case BuiltinAttribSubType.extern_module:
				onExternAttrib();
				break;
		}
	}

	// Apply the last @extern attribute
	auto attribNode = lastExternAttrib.get_node(c);
	final switch(cast(BuiltinAttribSubType)attribNode.subType) {
		case BuiltinAttribSubType.extern_syscall:
			uint syscall_number = attribNode.as!BuiltinAttribNode(c).data;
			auto funcType = &c.types.get!IrTypeFunction(node.irType);
			funcType.callConv = CallConvention.sysv64_syscall;
			funcType.syscallNumber = cast(ushort)syscall_number;
			break;

		case BuiltinAttribSubType.extern_module:
			// TODO: lookup by the name in external host module, or create import entry for dll symbols
			break;
	}

	if (node.attachedToFunctionWithBody) {
		if (attribNode.as!BuiltinAttribNode(c).isBroadcasted) {
			// allow broadcasted @extern attribute on function with body
		} else {
			c.error(node.loc, "External function cannot have a body");
		}
	}
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

TypeConvResKind type_conv_func_sig(FunctionSignatureNode* node, AstIndex typeBIndex, ref AstIndex expr, CompilationContext* c)
{
	if (typeBIndex.get_type(c).isAlias) return TypeConvResKind.ii_i;
	return TypeConvResKind.fail;
}

void gen_ir_header_func_sig(FunctionSignatureNode* node, CompilationContext* c)
{
	final switch(node.getPropertyState(NodeProperty.ir_header)) {
		case PropertyState.not_calculated: break;
		case PropertyState.calculating: assert(false);
		case PropertyState.calculated: return;
	}

	uint numResults = node.returnType.isTypeVoid ? 0 : 1;
	node.irType = c.types.appendFuncSignature(numResults, node.parameters.length, node.callConvention);
	node.setPropertyState(NodeProperty.ir_header, PropertyState.calculated);
}

// depends on ir_header
IrIndex gen_ir_type_func_sig(FunctionSignatureNode* node, CompilationContext* c)
	out(res; res.isTypeFunction, "Not a function type")
{
	final switch(node.getPropertyState(NodeProperty.ir_body)) {
		case PropertyState.not_calculated: break;
		case PropertyState.calculating: assert(false);
		case PropertyState.calculated: return node.irType;
	}

	gen_ir_header_func_sig(node, c);

	node.setPropertyState(NodeProperty.ir_body, PropertyState.calculating);
	scope(exit) node.setPropertyState(NodeProperty.ir_body, PropertyState.calculated);

	auto funcType = &c.types.get!IrTypeFunction(node.irType);

	if (funcType.numResults == 1) {
		IrIndex returnType = node.returnType.gen_ir_type(c);
		funcType.resultTypes[0] = returnType;
	}

	IrIndex[] parameterTypes = funcType.parameterTypes;
	foreach(i, AstIndex parameter; node.parameters) {
		parameterTypes[i] = parameter.get_node_type(c).gen_ir_type(c);
	}

	return node.irType;
}
