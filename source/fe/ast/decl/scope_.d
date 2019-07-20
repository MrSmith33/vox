/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.decl.scope_;

import all;

///
struct Scope
{
	///
	HashMap!(Identifier, AstNode*, Identifier.init) symbols;
	/// Imported modules
	Array!(ModuleDeclNode*) imports;
	///
	Scope* parentScope;
	///
	string debugName;
	/// Ordered scope is in function body, requires declaration above use
	/// Unordered scope is in struct, module
	bool isOrdered;
}
