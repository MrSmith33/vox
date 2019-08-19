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
	bool isOrdered;
}

mixin template ScopeDeclNodeData(AstType _astType, int default_flags = 0) {
	mixin AstNodeData!(_astType, default_flags | AstFlags.isScope | AstFlags.isDeclaration);
	/// Each node can be struct, function or variable
	Array!AstIndex declarations;
}
