/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.builtin;

import all;

enum BuiltinId : ubyte {
	int_min,
	int_max,
	slice_length,
	slice_ptr,
	array_length,
	array_ptr,
	type_sizeof
}

//struct BuiltinNode
//{
//	mixin AstNodeData!(AstType.decl_builtin, AstFlags.isDeclaration);
//	BuiltinId id;
//}
