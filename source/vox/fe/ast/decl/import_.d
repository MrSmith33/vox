/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.decl.import_;

import std.algorithm : joiner;
import vox.all;

@(AstType.decl_import)
struct ImportDeclNode
{
	mixin AstNodeData!(AstType.decl_import);
	ScopeIndex parentScope;
	Identifier id;
}


void print_import(ImportDeclNode* node, ref AstPrintState state)
{
	state.print("IMPORT ", node.id.pr(state.context));
}

void post_clone_import(ImportDeclNode* node, ref CloneState state)
{
	state.fixScope(node.parentScope);
}

void name_register_self_import(ImportDeclNode* node, ref NameRegisterState state) {
	CompilationContext* c = state.context;
	node.state = AstNodeState.name_register_self;
	scope(exit) node.state = AstNodeState.type_check_done;

	AstIndex index = c.modules.get(node.id);
	if (index.isUndefined) {
		// check if we can find a module with common prefix. Report as a typo
		Identifier id = node.id;
		while(id.hasParent) {
			id = id.getParent(c);
			AstIndex index2 = c.modules.get(id);
			if (index2.isDefined) {
				c.error(node.loc, "Cannot find module `%s`. But there is module with name `%s`", node.id.pr(c), id.pr(c));
				return;
			}
		}
		c.error(node.loc, "Cannot find module `%s`", node.id.pr(c));
	} else if (index.astType(c) == AstType.decl_package) {
		c.error(node.loc, "Cannot import package `%s`", node.id.pr(c));
	} else {
		// TODO: check that we do not import ourselves
		node.parentScope.get_scope(c).imports.put(c.arrayArena, index);
	}
}
