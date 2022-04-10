/// Copyright: Copyright (c) 2021 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.decl.package_;

import vox.all;

/// Does not correspond to any token in the source code
/// All packages and modules have parent package. Top-level package is root package (CommonAstNodes.node_root_package)
@(AstType.decl_package)
struct PackageDeclNode {
	mixin AstNodeData!(AstType.decl_package, 0, AstNodeState.ir_gen_done);
	AstNodeMap declarations; // nested modules/packages. Key is self name (non-fqn)
	Identifier fqn;
	/// Points to PackageDeclNode
	AstIndex parentPackage = CommonAstNodes.node_root_package;

	bool isTopLevel() { return parentPackage == CommonAstNodes.node_root_package; }

	void addModule(TokenIndex loc, Identifier modId, AstIndex modIndex, ref AstIndex conflictingModPack, CompilationContext* c) {
		bool wasCreated;
		c.assertf(modIndex.astType(c) == AstType.decl_module, "Must be module");
		AstIndex value = *declarations.getOrCreate(c.arrayArena, modId.getSelf(c), wasCreated, modIndex);
		if (!wasCreated) {
			if (conflictingModPack.isUndefined) conflictingModPack = value;
		}
	}

	AstIndex lookup(Identifier subpackageId, CompilationContext* c) {
		AstIndex index = declarations.get(subpackageId, AstIndex.init);
		assert(index.isModOrPack(c));
		return index;
	}

	AstIndex getOrCreateSubpackage(TokenIndex loc, Identifier subpackageId, ref AstIndex conflictingModule, CompilationContext* c) {
		AstIndex index = declarations.get(subpackageId.getSelf(c), AstIndex.init);

		void makePack() {
			index = c.appendAst!PackageDeclNode(TokenIndex.init, AstNodeMap.init, subpackageId, c.getAstNodeIndex(&this));
			c.modules.put(c.arrayArena, subpackageId, index);
			declarations.put(c.arrayArena, subpackageId.getSelf(c), index);
		}

		if (index.isUndefined) makePack(); // create new package

		AstNode* node = index.get_node(c);

		if (node.astType == AstType.decl_module) {
			if (conflictingModule.isUndefined) conflictingModule = index;
			// Create new package and replace existing, so that we can finish parsing module declaration
			// Calling code will check conflictingModule in the end, and generate an error
			makePack();
		}
		return index;
	}

	// Needed to report file name that defines this package
	void visitModules(scope void delegate(ref AstIndex mod) visitor, CompilationContext* c) {
		foreach(i, ref AstIndex sub; declarations) {
			auto node = sub.get_node(c);
			if (node.astType == AstType.decl_module) visitor(sub);
			else node.as!PackageDeclNode(c).visitModules(visitor, c);
		}
	}
}


struct PackageFilesPrinter
{
	PackageDeclNode* pack;
	CompilationContext* c;
	void toString(scope void delegate(const(char)[]) sink) {
		uint filesVisited = 0;
		enum FILE_PRINT_LIMIT = 2;

		// TODO: need to gather names in array, sort it and then print. Otherwise tests depend on hashmap order
		void onModule(ref AstIndex modIndex) {
			if (filesVisited < FILE_PRINT_LIMIT || c.verboseErrors) {
				if (filesVisited > 0) sink(", ");
				sink(modIndex.get!ModuleDeclNode(c).fileName(c));
			}
			++filesVisited;
		}

		pack.visitModules(&onModule, c);
		if (filesVisited > FILE_PRINT_LIMIT && c.conciseErrors) sink.formattedWrite(" and %s more", filesVisited - FILE_PRINT_LIMIT);
	}
}

void print_package(PackageDeclNode* node, ref AstPrintState state)
{
	state.print("PACKAGE ", node.fqn.pr(state.context));
}
