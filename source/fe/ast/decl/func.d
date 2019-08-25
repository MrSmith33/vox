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
	///
	FunctionLiveIntervals liveIntervals;

	// TODO: use objectSymIndex -> ObjectSymbol to get the code ptr for JIT
	/// Executable machine-code bytes
	ubyte[] code;
	/// Position in buffer or in memory
	void* funcPtr;

	// TODO: move into IrFunction
	StackLayout stackLayout;
	/// obsolete. Use FunctionDeclNode.getIrIndex
	FunctionIndex index;
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

@(AstType.decl_function)
struct FunctionDeclNode {
	mixin AstNodeData!(AstType.decl_function, AstFlags.isDeclaration);
	AstIndex signature; // FunctionSignatureNode
	AstIndex block_stmt; // null if external
	AstIndex _scope;
	FunctionBackendData backendData;

	this(TokenIndex loc, AstIndex signature, AstIndex block, Identifier id)
	{
		this.loc = loc;
		this.astType = AstType.decl_function;
		this.flags = AstFlags.isDeclaration;
		this.signature = signature;
		this.block_stmt = block;
		this.backendData.name = id;
		this.backendData.signature = signature;
	}

	IrIndex getIrIndex(CompilationContext* c) {
		AstIndex index = get_ast_index(&this, c);
		return IrIndex(index.storageIndex, IrValueKind.func);
	}

	/// External functions have no body
	bool isExternal() { return block_stmt.isUndefined; }
	ref Identifier id() { return backendData.name; }
}

void name_register_func(AstIndex nodeIndex, FunctionDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	state.insert(node.id, nodeIndex);
	node._scope = state.pushScope(state.context.idString(node.id), Yes.ordered);
	require_name_register(node.signature, state);
	if (node.block_stmt)
	{
		// TODO: we don't need to register parameters on function without body
		require_name_register(node.block_stmt, state);
	}
	state.popScope;
	node.state = AstNodeState.name_register_done;
}

void name_resolve_func(FunctionDeclNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	// TODO: parameters don't need to see each other (including default param value expr)
	state.pushScope(node._scope);
	require_name_resolve(node.signature, state);
	if (node.block_stmt)
	{
		// TODO: we don't need to register parameters on function without body
		require_name_resolve(node.block_stmt, state);
	}
	state.popScope;
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
