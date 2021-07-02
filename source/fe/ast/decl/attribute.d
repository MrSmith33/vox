/// Copyright: Copyright (c) 2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.attribute;

import all;

/// If AstNode has any attributes attached at parse time then AttributeInfo is allocated
/// in memory before such AstNode and node receives AstFlags.hasAttributes flag.
struct AttributeInfo
{
	AstNodes attributes;
	ulong flags;

	// Returns pointer to the node this struct is attached to
	AstNode* node() return {
		return cast(AstNode*)(cast(void*)(&this) + AttributeInfo.sizeof);
	}
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

enum BuiltinAttribSubType : ubyte {
	extern_syscall
}

@(AstType.decl_builtin_attribute)
struct BuiltinAttribNode
{
	mixin AstNodeData!(AstType.decl_builtin_attribute, 0, AstNodeState.type_check_done);
	uint data;

	this(TokenIndex loc, BuiltinAttribSubType subType, uint data)
	{
		this.loc = loc;
		this.astType = AstType.decl_builtin_attribute;
		this.flags = 0;
		this.state = AstNodeState.type_check_done;
		this.subType = subType;
		this.data = data;
	}
}

void print_builtin_attribute(BuiltinAttribNode* node, ref AstPrintState state)
{
	final switch(cast(BuiltinAttribSubType)node.subType) {
		case BuiltinAttribSubType.extern_syscall:
			state.print("ATTRIB @extern(syscall, ", node.data, ")");
			break;
	}
}

