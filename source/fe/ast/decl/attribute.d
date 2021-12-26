/// Copyright: Copyright (c) 2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.attribute;

import all;

/// If AstNode has any attributes attached at parse time then AttributeInfo is allocated
/// in memory before such AstNode and node receives AstFlags.hasAttributes flag.
/// All broadcasted nodes are located before direct attributes.
struct AttributeInfo
{
	AstNodes attributes;
	uint flags;

	// Returns pointer to the node this struct is attached to
	AstNode* node() return {
		return cast(AstNode*)(cast(void*)(&this) + AttributeInfo.sizeof);
	}

	bool isExternal() { return cast(bool)(flags & AttribInfoFlags.isExternal); }
}

void print_attributes(AttributeInfo* attribs, ref AstPrintState state) {
	print_ast(attribs.attributes, state);
}

void post_clone_attributes(AttributeInfo* attribs, ref CloneState state) {
	state.fixAstNodes(attribs.attributes);
}

void name_register_nested_attributes(AttributeInfo* attribs, ref NameRegisterState state) {
	require_name_register(attribs.attributes, state);
}

void name_resolve_attributes(AttributeInfo* attribs, ref NameResolveState state) {
	require_name_resolve(attribs.attributes, state);
}

void type_check_attributes(AttributeInfo* attribs, ref TypeCheckState state) {
	require_type_check(attribs.attributes, state);
}

enum AttribInfoFlags : uint {
	isExternal = 1 <<  0, // set if AttributeInfo contains some @extern attribute
}

enum BuiltinFlagAttrib : ushort {
	isStatic = 1 <<  0, // @static
}

enum BuiltinAttribSubType : ubyte {
	extern_syscall,
	extern_module
}

immutable uint[BuiltinAttribSubType.max+1] builtinAttribFlags = [
	AttribInfoFlags.isExternal, // extern_syscall
	AttribInfoFlags.isExternal, // extern_module
];

uint calcAttribFlags(AstIndex attrib, CompilationContext* c) {
	auto attribNode = attrib.get_node(c);
	if (attribNode.astType != AstType.decl_builtin_attribute) return 0;
	return builtinAttribFlags[attribNode.subType];
}

enum AnyAttributeFlags : ushort
{
	// Set if attribute was applied to multiple nodes at once via `@attr{}` or `@attr:` syntax
	// If not set, attribute was applied directly
	isBroadcasted = AstFlags.userFlag << 0,
}

@(AstType.decl_builtin_attribute)
struct BuiltinAttribNode
{
	mixin AstNodeData!(AstType.decl_builtin_attribute, 0, AstNodeState.type_check_done);
	uint data;

	bool isBroadcasted() { return cast(bool)(flags & AnyAttributeFlags.isBroadcasted); }

	this(TokenIndex loc, BuiltinAttribSubType subType, uint data)
	{
		this.loc = loc;
		this.astType = AstType.decl_builtin_attribute;
		this.flags = 0;
		if (subType == BuiltinAttribSubType.extern_syscall)
			this.state = AstNodeState.name_resolve_done;
		else
			this.state = AstNodeState.type_check_done;
		this.subType = subType;
		this.data = data;
	}
}

void print_builtin_attribute(BuiltinAttribNode* node, ref AstPrintState state)
{
	final switch(cast(BuiltinAttribSubType)node.subType) {
		case BuiltinAttribSubType.extern_syscall:
			state.print("ATTRIB @extern(syscall, ", node.data, ")", AttributeFlagPrinter(node.flags));
			break;
		case BuiltinAttribSubType.extern_module:
			state.print("ATTRIB @extern(module, ", state.context.idString(Identifier(node.data)), ")", AttributeFlagPrinter(node.flags));
			break;
	}
}

struct AttributeFlagPrinter {
	ushort flags;
	void toString(scope void delegate(const(char)[]) sink) {
		if (flags == 0) return;
		if (flags & AnyAttributeFlags.isBroadcasted) sink(" /broadcasted");
	}
}

void type_check_builtin_attribute(BuiltinAttribNode* node, ref TypeCheckState state)
{
	if (node.subType == BuiltinAttribSubType.extern_syscall && node.isBroadcasted)
	{
		// forbid broadcasting @extern(syscall) as it makes no sense, since it carries data, which should be different for each function
		state.context.error(node.loc, "Broadcasting @extern(syscall) attribute is forbidden");
	}
}
