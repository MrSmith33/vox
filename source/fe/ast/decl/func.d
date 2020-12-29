/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.func;

import all;

enum FuncDeclFlags : ushort
{
	isInline = AstFlags.userFlag << 0,
	isBuiltin = AstFlags.userFlag << 1,
}

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
	AstIndex optimizedIrData; // IrFunction
	AstIndex loweredIrData; // IrFunction
	/// Machine-level IR
	AstIndex lirData; // IrFunction
	///
	LinkIndex objectSymIndex;
}

@(AstType.decl_function)
struct FunctionDeclNode {
	mixin AstNodeData!(AstType.decl_function, AstFlags.isDeclaration);
	AstIndex _module;
	AstIndex parentScope;
	AstIndex signature; // FunctionSignatureNode
	AstIndex block_stmt; // null if external
	Identifier id;
	FunctionBackendData backendData;

	bool isInline() { return cast(bool)(flags & FuncDeclFlags.isInline); }
	bool isBuiltin() { return cast(bool)(flags & FuncDeclFlags.isBuiltin); }
	bool isCtfeOnly(CompilationContext* c) {
		return signature.get!FunctionSignatureNode(c).isCtfeOnly;
	}

	this(TokenIndex loc, AstIndex _module, AstIndex parentScope, AstIndex signature, Identifier id)
	{
		this.loc = loc;
		this.astType = AstType.decl_function;
		this.flags = AstFlags.isDeclaration;
		this._module = _module;
		this.parentScope = parentScope;
		this.signature = signature;
		this.id = id;
	}

	IrIndex getIrIndex(CompilationContext* c) {
		AstIndex index = get_ast_index(&this, c);
		return IrIndex(index.storageIndex, IrValueKind.func);
	}

	/// External functions have no body
	bool isExternal() { return block_stmt.isUndefined; }
}

void print_func(FunctionDeclNode* node, ref AstPrintState state)
{
	state.print("FUNC ", state.context.idString(node.id),
		node.isBuiltin ? " #builtin" : null,
		node.isInline ? " #inline" : null);
	print_ast(node.signature, state);
	if (node.block_stmt) print_ast(node.block_stmt, state);
}

void post_clone_func(FunctionDeclNode* node, ref CloneState state)
{
	state.fixScope(node.parentScope);
	state.fixAstIndex(node._module);
	state.fixAstIndex(node.signature);
	state.fixAstIndex(node.block_stmt);
}

void name_register_self_func(AstIndex nodeIndex, FunctionDeclNode* node, ref NameRegisterState state) {
	auto c = state.context;
	node.state = AstNodeState.name_register_self;

	// Template instance shouldn't register itself
	// They are discovered with template instantiation syntax
	// Instance is wrapped in special scope, which shouldn't have function name inserted, only template args
	if (!node.isTemplateInstance)
	{
		// can't be done at parse time because of conditional compilation
		node.parentScope.insert_scope(node.id, nodeIndex, c);
	}
	auto mod = node._module.get!ModuleDeclNode(c);
	mod.addFunction(nodeIndex, c);

	// Create link object
	{
		LinkIndex symbolIndex;

		if (node.isExternal)
		{
			// When JIT-compiling, host can provide a set of external functions
			// we will use provided function pointer
			symbolIndex = c.externalSymbols.get(node.id, LinkIndex());

			if (!symbolIndex.isDefined) {
				// Allowed if it is marked with @extern(syscall)
				auto sig = node.signature.get!FunctionSignatureNode(c);
				if (!sig.findExternSyscallAttrib(c)) {
					c.error(node.loc, "Unresolved external function %s", c.idString(node.id));
				}
			}
		}
		else
		{
			ObjectSymbol sym = {
				kind : ObjectSymbolKind.isLocal,
				sectionIndex : c.builtinSections[ObjectSectionType.code],
				moduleIndex : mod.objectSymIndex,
				alignmentPower : 0,
				id : node.id,
			};
			symbolIndex = c.objSymTab.addSymbol(sym);
		}

		// TODO: check that parameters match
		node.backendData.objectSymIndex = symbolIndex;
	}

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

// ModuleDeclNode.functions are processed sequentially. No nesting can occur.
void ir_gen_function(ref IrGenState gen, FunctionDeclNode* f)
{
	CompilationContext* c = gen.context;
	IrBuilder* builder = &gen.builder;

	f.state = AstNodeState.ir_gen;
	scope(exit) f.state = AstNodeState.ir_gen_done;

	// skip external functions, they don't have a body
	if (f.isExternal) return;

	gen.fun = f;
	scope(exit) gen.fun = null;

	// create new function
	AstIndex irIndex = c.appendAst!IrFunction;
	f.backendData.irData = irIndex;
	gen.ir = c.getAst!IrFunction(irIndex);
	IrFunction* ir = gen.ir;
	ir.name = f.id;

	auto signature = f.signature.get!FunctionSignatureNode(c);
	ir.type = f.signature.gen_ir_type(c);
	ir.instructionSet = IrInstructionSet.ir;

	builder.begin(ir, c);

	foreach (AstIndex param; signature.parameters)
	{
		IrLabel dummy;
		ir_gen_stmt(gen, param, ir.entryBasicBlock, dummy);
	}

	builder.addJump(ir.entryBasicBlock);

	IrIndex body_block = builder.addBasicBlock();
	builder.addBlockTarget(ir.entryBasicBlock, body_block);
	builder.sealBlock(body_block);

	// label at the end of body
	IrLabel bodyExitLabel = IrLabel(body_block);

	// compile body
	ir_gen_stmt(gen, f.block_stmt, body_block, bodyExitLabel);

	IrIndex currentBlock = bodyExitLabel.blockIndex;
	// In case new block was created, no new predecessors will be added
	builder.sealBlock(currentBlock);

	if (!signature.returnType.isVoidType(c))
	{
		// currentBlock must be finished with retVal
		if (!ir.getBlock(currentBlock).isFinished)
		{
			c.error(f.loc,
				"function `%s` has no return statement, but is expected to return a value of type %s",
				c.idString(f.id), signature.returnType.typeName(c));
		}

		auto exitBlock = ir.getBlock(ir.exitBasicBlock);
		if (exitBlock.predecessors.empty)
		{
			// control flow doesn't reach exit block. Remove return var phi function.
			removeAllPhis(*exitBlock);
			removeAllInstrs(*exitBlock);
			builder.emitInstr!(IrOpcode.unreachable)(ir.exitBasicBlock);
		}
	}
	else
	{
		// currentBlock must be finished with ret or, not finished
		if (!ir.getBlock(currentBlock).isFinished)
		{
			builder.addReturn(currentBlock);
		}
	}

	//dumpFunction(c, ir, "IR gen end");

	// all blocks with return (exit's predecessors) already connected, seal exit block
	builder.sealBlock(ir.exitBasicBlock);

	builder.finalizeIr;
}
