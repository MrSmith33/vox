/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.import_;

import std.algorithm : joiner;
import all;

@(AstType.decl_import)
struct ImportDeclNode
{
	mixin AstNodeData!(AstType.decl_import);
	AstIndex parentScope;
	Array!Identifier ids;
}

struct IdListPrinter
{
	Identifier[] ids;
	CompilationContext* c;
	void toString(scope void delegate(const(char)[]) sink) {
		foreach(i, Identifier id; ids) {
			if (i > 0) sink(".");
			sink(c.idString(id));
		}
	}
}

void print_import(ImportDeclNode* node, ref AstPrintState state)
{
	state.print("IMPORT ", IdListPrinter(node.ids[], state.context));
}

void post_clone_import(ImportDeclNode* node, ref CloneState state)
{
	state.fixScope(node.parentScope);
}

void name_register_self_import(ImportDeclNode* node, ref NameRegisterState state) {
	CompilationContext* c = state.context;
	node.state = AstNodeState.name_register_self;
	auto pack = CommonAstNodes.node_root_package.get!PackageDeclNode(c);
	foreach(i, Identifier id; node.ids) {
		AstIndex index = pack.declarations.get(id);
		if (index.isUndefined) {
			c.error(node.loc, "Cannot find module `%s`", IdListPrinter(node.ids[], c));
			break;
		}
		if (index.astType(c) == AstType.decl_package) {
			if (i+1 == node.ids.length) {
				c.error(node.loc, "Cannot import package `%s`", IdListPrinter(node.ids[], c));
			} else {
				pack = index.get!PackageDeclNode(c);
			}
		} else {
			// it is a module
			if (i+1 == node.ids.length) {
				// TODO: check that we do not import ourselves
				node.parentScope.get_scope(c).imports.put(c.arrayArena, index);
			} else {
				// otherwise id is incorrect. We found module, but id is not ended yet
				c.error(node.loc, "Cannot find module `%s`. But there is module with name `%s`", IdListPrinter(node.ids[], c), IdListPrinter(node.ids[0..i+1], c));
			}
		}
	}
	node.state = AstNodeState.type_check_done;
}
