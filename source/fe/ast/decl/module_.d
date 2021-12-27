/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.module_;

import all;

/// Index into CompilationContext.files
struct ModuleIndex
{
	uint fileIndex;
}

/// If `module` declaration is found in the module points to that token
@(AstType.decl_module)
struct ModuleDeclNode {
	mixin ScopeDeclNodeData!(AstType.decl_module, 0, AstNodeState.name_register_self_done);
	ScopeIndex memberScope;
	/// Linear list of all functions of a module (including nested and methods and externals)
	/// Order may be different from declaration order, because conditionaly compiled functions are added later
	AstNodes functions;
	IrModule irModule;
	IrModule lirModule;
	LinkIndex objectSymIndex;
	ModuleIndex moduleIndex;
	/// module identifier. Used by import declaration.
	Identifier id;
	/// Points to PackageDeclNode
	AstIndex parentPackage = CommonAstNodes.node_root_package;

	bool isTopLevel() { return parentPackage == CommonAstNodes.node_root_package; }

	void addFunction(AstIndex func, CompilationContext* context) {
		functions.put(context.arrayArena, func);
	}

	string fileName(CompilationContext* c) {
		return c.files[moduleIndex.fileIndex].name;
	}

	FunctionDeclNode* findFunction(string idStr, CompilationContext* context) {
		Identifier id = context.idMap.find(idStr);
		if (id.isUndefined) return null;
		return findFunction(id, context);
	}
	FunctionDeclNode* findFunction(Identifier id, CompilationContext* context) {
		AstIndex sym = memberScope.lookup_scope(id, context);
		if (sym.isUndefined) return null;
		return sym.get!FunctionDeclNode(context);
	}
}


struct ModuleNamePrinter {
	ModuleDeclNode* mod;
	CompilationContext* c;

	void toString(scope void delegate(const(char)[]) sink) {
		if (!mod.isTopLevel) {
			printPackageName(mod.parentPackage, sink, c);
			sink(".");
		}
		sink(c.idString(mod.id));
	}
}

void print_module(ModuleDeclNode* node, ref AstPrintState state)
{
	state.print("MODULE ", state.context.files[node.moduleIndex.fileIndex].name,
		" ", ModuleNamePrinter(node, state.context));
	print_ast(node.declarations, state);
}

void name_register_nested_module(ModuleDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.declarations, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_module(ModuleDeclNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.declarations, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_module(ModuleDeclNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.declarations, state);
	node.state = AstNodeState.type_check_done;
}

void ir_gen_module_globals(ref IrGenState gen, ModuleDeclNode* m)
{
	CompilationContext* c = gen.context;

	foreach (AstIndex decl; m.declarations)
	{
		ir_gen_decl(gen, decl);
	}
}

void ir_gen_module_func(ref IrGenState gen, ModuleDeclNode* m)
{
	CompilationContext* c = gen.context;

	foreach (AstIndex decl; m.functions)
	{
		FunctionDeclNode* func = c.getAst!FunctionDeclNode(decl);
		ir_gen_function(gen, func);

		IrFunction* irData = c.getAst!IrFunction(func.backendData.irData);
		// irData can be null if function is external
		if (irData) m.irModule.addFunction(c, irData);
	}
}
