/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.scope_;

import all;

///
struct Scope
{
	///
	HashMap!(Identifier, AstIndex, Identifier.init) symbols;
	/// Imported modules
	Array!AstIndex imports;
	///
	AstIndex parentScope;
	///
	string debugName;
	/// Ordered scope is in function body, requires declaration above use
	/// Unordered scope is in struct, module
	IsOrdered isOrdered;

	/// Constructs and inserts symbol with id
	void insert(Identifier id, AstIndex nodeIndex, CompilationContext* c)
	{
		AstNode* node = nodeIndex.get_node(c);
		if (isOrdered) node.flags |= AstFlags.isInOrderedScope;
		if (auto s = symbols.get(id, AstIndex.init))
		{
			c.error(node.loc,
				"declaration `%s` is already defined at %s",
				c.idString(id), FmtSrcLoc(s.get_node(c).loc, c));
		}
		symbols.put(c.arrayArena, id, nodeIndex);
	}
}

enum IsOrdered : bool {
	no = false,
	yes = true,
}

mixin template ScopeDeclNodeData(AstType _astType, int default_flags = 0, AstNodeState _init_state = AstNodeState.parse_done) {
	mixin AstNodeData!(_astType, default_flags | AstFlags.isScope | AstFlags.isDeclaration, _init_state);
	/// Each node can be struct, function or variable
	AstNodes declarations;
}
