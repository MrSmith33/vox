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
	AstIndex irData; // IrFunction
	/// Machine-level IR
	AstIndex lirData; // IrFunction
	// TODO: move into IrFunction
	StackLayout stackLayout;
	///
	Identifier name;
	///
	LinkIndex objectSymIndex;

	// Copy of FunctionDeclNode.signature. TODO
	AstIndex signature;

	CallConv* getCallConv(CompilationContext* c) {
		return callConventions[signature.get!FunctionSignatureNode(c).callConvention];
	}
}

enum FunctionAndVarFlags : ushort {
	isMember = AstFlags.userFlag << 0,
}

enum FunctionFlags : ushort {
	isMember = FunctionAndVarFlags.isMember,
}

@(AstType.decl_function)
struct FunctionDeclNode {
	mixin AstNodeData!(AstType.decl_function, AstFlags.isDeclaration);
	AstIndex _module;
	AstIndex parentScope;
	AstIndex signature; // FunctionSignatureNode
	AstIndex block_stmt; // null if external
	FunctionBackendData backendData;

	this(TokenIndex loc, AstIndex _module, AstIndex parentScope, AstIndex signature, Identifier id)
	{
		this.loc = loc;
		this.astType = AstType.decl_function;
		this.flags = AstFlags.isDeclaration;
		this._module = _module;
		this.parentScope = parentScope;
		this.signature = signature;
		this.backendData.name = id;
		this.backendData.signature = signature;
	}

	IrIndex getIrIndex(CompilationContext* c) {
		AstIndex index = get_ast_index(&this, c);
		return IrIndex(index.storageIndex, IrValueKind.func);
	}

	/// External functions have no body
	bool isExternal() { return block_stmt.isUndefined; }
	bool isMember() { return cast(bool)(flags & FunctionFlags.isMember); }
	ref Identifier id() { return backendData.name; }
}

void name_register_self_func(AstIndex nodeIndex, FunctionDeclNode* node, ref NameRegisterState state) {
	auto c = state.context;
	node.state = AstNodeState.name_register_self;

	// can't be done at parse time because of conditional compilation
	node.parentScope.insert_scope(node.id, nodeIndex, c);
	node._module.get!ModuleDeclNode(c).addFunction(nodeIndex, c);

	node.state = AstNodeState.name_register_self_done;
}

void name_register_nested_func(AstIndex nodeIndex, FunctionDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.signature, state);
	if (node.block_stmt)
	{
		// TODO: we don't need to register parameters on function without body
		require_name_register(node.block_stmt, state);
	}
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_func(FunctionDeclNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	// TODO: parameters don't need to see each other (including default param value expr)
	require_name_resolve(node.signature, state);
	if (node.block_stmt)
	{
		// TODO: we don't need to register parameters on function without body
		require_name_resolve(node.block_stmt, state);
	}
	node.state = AstNodeState.name_resolve_done;
}

void type_check_func(FunctionDeclNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	auto prevFunc = state.curFunc;
	state.curFunc = node;

	require_type_check(node.signature, state);

	if (node.block_stmt)
	{
		require_type_check(node.block_stmt, state);
	}
	state.curFunc = prevFunc;
	node.state = AstNodeState.type_check_done;
}
