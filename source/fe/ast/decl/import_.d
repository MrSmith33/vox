/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.import_;

import all;

@(AstType.decl_import)
struct ImportDeclNode
{
	mixin AstNodeData!(AstType.decl_import, AstFlags.isDeclaration);
	Identifier id;
}

void name_register_import(ImportDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	ModuleDeclNode* m = state.context.findModule(node.id);
	if (m is null)
		state.context.error(node.loc, "Cannot find module `%s`", state.context.idString(node.id));
	else
		state.currentScope.imports.put(state.context.arrayArena, state.context.getAstNodeIndex(m));
	node.state = AstNodeState.type_check_done;
}
