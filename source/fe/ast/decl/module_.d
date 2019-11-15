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
@(AstType.decl_module)
struct ModuleDeclNode {
	mixin ScopeDeclNodeData!(AstType.decl_module, 0, AstNodeState.name_register_self_done);
	AstIndex memberScope;
	/// Linear list of all functions of a module (including nested and methods and externals)
	Array!AstIndex functions;
	IrModule irModule;
	IrModule lirModule;
	LinkIndex objectSymIndex;
	ModuleIndex moduleIndex;
	/// module identifier. Used by import declaration.
	Identifier id;

	void addFunction(AstIndex func, CompilationContext* context) {
		functions.put(context.arrayArena, func);
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
