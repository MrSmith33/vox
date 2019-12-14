/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.import_;

import all;

@(AstType.decl_import)
struct ImportDeclNode
{
	mixin AstNodeData!(AstType.decl_import, AstFlags.isDeclaration);
	AstIndex parentScope;
	Identifier id;
}

void post_clone_import(ImportDeclNode* node, ref CloneState state)
{
	state.fixScope(node.parentScope);
}

void name_register_self_import(ImportDeclNode* node, ref NameRegisterState state) {
	CompilationContext* c = state.context;
	node.state = AstNodeState.name_register_self;
	ModuleDeclNode* m = c.findModule(node.id);
	if (m is null)
		c.error(node.loc, "Cannot find module `%s`", c.idString(node.id));
	else
		node.parentScope.get_scope(c).imports.put(c.arrayArena, c.getAstNodeIndex(m));
	node.state = AstNodeState.type_check_done;
}
