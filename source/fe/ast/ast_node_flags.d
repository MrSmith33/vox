/// Copyright: Copyright (c) 2021 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast_node_flags;

enum AstFlags : ushort
{
	/// Before AST node AttributeInfo struct allocated
	hasAttributes      = 1 <<  0,

	isType             = 1 <<  1,
	/// Is added to expression nodes that are being assigned to
	isLvalue           = 1 <<  2,
	/// stores ScopeKind
	scopeKindMask      = 1 <<  3 | 1 << 4, // used for reading value
	isLocal            = 0 <<  3,          // used for setting flags
	isGlobal           = 1 <<  3,          // used for setting flags
	isMember           = 2 <<  3,          // used for setting flags

	isTemplateInstance = 1 <<  5,
	// used for node specific flags
	userFlag           = 1 <<  6,
}
