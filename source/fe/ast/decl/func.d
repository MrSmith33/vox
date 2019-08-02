/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.func;

import all;

/// Refers to a function inside a module
struct FunctionIndex
{
	/// Index into ModuleDeclNode.functions
	uint functionIndex;
	ModuleIndex moduleIndex;
}

struct FunctionBackendData
{
	/// Machine-independent IR
	IrFunction* irData;
	/// Machine-level IR
	IrFunction* lirData;
	///
	FunctionLiveIntervals liveIntervals;
	/// Executable machine-code bytes
	ubyte[] code;
	/// Position in buffer or in memory
	void* funcPtr;
	///
	StackLayout stackLayout;
	///
	CallConv* callingConvention;
	/// Callers will use this index to call this function.
	FunctionIndex index;
	/// Index of IrValueKind.type kind
	IrIndex returnType; // TODO: remove in favor of `type`
	/// Index of function type
	IrIndex irType;
	///
	Identifier name;
	///
	LinkIndex objectSymIndex;
}

struct FunctionDeclNode {
	mixin AstNodeData!(AstType.decl_function, AstFlags.isDeclaration);
	TypeNode* returnType;
	Array!(VariableDeclNode*) parameters;
	BlockStmtNode* block_stmt; // null if external
	Scope* _scope;
	FunctionBackendData backendData;

	this(TokenIndex loc, TypeNode* retType, Array!(VariableDeclNode*) parameters, BlockStmtNode* block, Identifier id)
	{
		this.loc = loc;
		this.astType = AstType.decl_function;
		this.flags = AstFlags.isDeclaration;
		this.returnType = retType;
		this.parameters = parameters;
		this.block_stmt = block;
		this.backendData.name = id;
	}

	/// External functions have no body
	bool isExternal() { return block_stmt is null; }
	ref Identifier id() { return backendData.name; }
}

void name_register_func(FunctionDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	state.insert(node.id, node.as_node);
	node._scope = state.pushScope(state.context.idString(node.id), Yes.ordered);
	foreach (param; node.parameters) require_name_register(param.as_node, state);
	if (node.block_stmt) require_name_register(node.block_stmt.as_node, state);
	state.popScope;
	node.state = AstNodeState.name_register_done;
}

void name_resolve_func(FunctionDeclNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	state.pushScope(node._scope);
	require_name_resolve(node.returnType.as_node, state);
	foreach (param; node.parameters) require_name_resolve(param.as_node, state);
	if (node.block_stmt) require_name_resolve(node.block_stmt.as_node, state);
	state.popScope;
	node.state = AstNodeState.name_resolve_done;
}
