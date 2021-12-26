/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.scope_;

import all;

enum ScopeKind : ubyte {
	no_scope, // scope created by @attr{} or by #version or #if or #foreach
	local,  // function
	member, // struct, enum
	global, // module
}

///
struct Scope
{
	///
	AstNodeMap symbols;
	/// Imported modules
	AstNodes imports;
	///
	AstIndex parentScope;
	/// This node is owner of all the definitions inside of this scope
	AstIndex owner;
	///
	ScopeKind kind;
	///
	string debugName;

	/// Constructs and inserts symbol with id
	void insert(Identifier id, AstIndex nodeIndex, CompilationContext* c)
	{
		AstNode* node = nodeIndex.get_node(c);
		if (auto s = symbols.get(id, AstIndex.init))
		{
			c.error(node.loc,
				"declaration `%s` is already defined at %s",
				c.idString(id), FmtSrcLoc(s.get_node(c).loc, c));
		}
		symbols.put(c.arrayArena, id, nodeIndex);
	}
}

void post_clone_scope(Scope* node, ref CloneState state)
{
	// Scope.symbols/imports dont need fixing, because no symbols are registered at this point
	state.fixScope(node.parentScope);
	state.fixAstIndex(node.owner);
}

mixin template ScopeDeclNodeData(AstType _astType, int default_flags = 0, AstNodeState _init_state = AstNodeState.parse_done) {
	mixin AstNodeData!(_astType, default_flags, _init_state);
	/// Each node can be struct, function or variable
	AstNodes declarations;
}
