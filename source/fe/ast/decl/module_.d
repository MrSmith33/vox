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

///
struct ModuleDeclNode {
	mixin ScopeDeclNodeData!(AstType.decl_module);
	AstIndex _scope;
	/// Linear list of all functions of a module (including nested and methods and externals)
	Array!AstIndex functions;
	IrModule irModule;
	IrModule lirModule;
	LinkIndex objectSymIndex;
	ModuleIndex moduleIndex;
	/// module identifier. Used by import declaration.
	Identifier id;

	void addFunction(AstIndex func, CompilationContext* context) {
		context.getAst!FunctionDeclNode(func).backendData.index = FunctionIndex(cast(uint)functions.length, moduleIndex);
		functions.put(context.arrayArena, func);
	}

	FunctionDeclNode* findFunction(string idStr, CompilationContext* context) {
		Identifier id = context.idMap.find(idStr);
		if (id.isUndefined) return null;
		return findFunction(id, context);
	}
	FunctionDeclNode* findFunction(Identifier id, CompilationContext* context) {
		AstIndex sym = context.getAst!Scope(_scope).symbols.get(id, AstIndex.init);
		if (sym.isUndefined) return null;
		return context.getAstNode(sym).cast_decl_function;
	}
}

void name_register_module(ModuleDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	node._scope = state.pushScope("Module", No.ordered);
	foreach (decl; node.declarations) require_name_register(decl, state);
	state.popScope;
	node.state = AstNodeState.name_register_done;
}

void name_resolve_module(ModuleDeclNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	state.pushScope(node._scope);
	foreach (decl; node.declarations) require_name_resolve(decl, state);
	state.popScope;
	node.state = AstNodeState.name_resolve_done;
}

void type_check_module(ModuleDeclNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	foreach (ref AstIndex decl; node.declarations) require_type_check(decl, state);
	node.state = AstNodeState.type_check_done;
}
