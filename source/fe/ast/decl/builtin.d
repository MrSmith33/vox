/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.builtin;

import all;

// The order is the same as in CommonAstNodes enum
enum BuiltinId : ubyte {
	@("min")    int_min,
	@("max")    int_max,
	@("length") slice_length,
	@("ptr")    slice_ptr,
	@("length") array_length,
	@("ptr")    array_ptr,
	@("sizeof") type_sizeof
}

immutable string[] builtinIdStrings = gatherEnumStrings!BuiltinId();


@(AstType.decl_builtin)
struct BuiltinNode
{
	mixin AstNodeData!(AstType.decl_builtin, AstFlags.isDeclaration, AstNodeState.type_check_done);
	Identifier id;
	BuiltinId builtin;
}
