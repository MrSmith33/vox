/// Copyright: Copyright (c) 2021 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast_node;

import std.traits : getUDAs;
import all;

mixin template AstNodeData(AstType _astType = AstType.abstract_node, int default_flags = 0, AstNodeState _init_state = AstNodeState.parse_done)
{
	import std.bitmanip : bitfields;

	// how many fields this mixin template has
	private enum NUM_BASE_FIELDS = 5;

	this(Args...)(TokenIndex loc, Args args) {
		this(loc);
		enum len = this.tupleof.length - NUM_BASE_FIELDS;
		enum numDefault = len - args.length;
		static assert(args.length <= len, "Too many args");
		this.tupleof[NUM_BASE_FIELDS..$-numDefault] = args;
	}

	this(TokenIndex loc) {
		this.loc = loc;
		this.astType = _astType;
		this.flags = cast(ushort)default_flags;
		this.state = _init_state;
	}

	TokenIndex loc;
	AstType astType = _astType;

	mixin(bitfields!(
		AstNodeState,  "state",     4,
		uint,          "subType",   4,
	));
	ushort flags = cast(ushort)default_flags;

	// Stores one PropertyState per NodeProperty for up to 16 properties
	uint propertyStates;

	PropertyState getPropertyState(NodeProperty prop) {
		return cast(PropertyState)((propertyStates >> (prop * 2)) & 0b11);
	}

	void setPropertyState(NodeProperty prop, PropertyState state) {
		uint mask = ~(uint(0b11) << (prop * 2));
		propertyStates = (propertyStates & mask) | (uint(state) << (prop * 2));
	}

	T* as(T)(CompilationContext* c) {
		static if (hasAstNodeType!T)
		{
			c.assertf(astType == getAstNodeType!T, "as(%s) got %s", T.stringof, astType);
		}
		return cast(T*)&this;
	}

	TypeNode* as_type(CompilationContext* c) return {
		c.assertf(isType, loc, "as_type(%s)", astType);
		return cast(TypeNode*)&this;
	}

	// Returns pointer to attributes. Only valid if hasAttributes flag is set
	AttributeInfo* attributeInfo() return {
		assert(hasAttributes);
		return cast(AttributeInfo*)(cast(void*)(&this) - AttributeInfo.sizeof);
	}

	bool hasExternAttrib() {
		if (!hasAttributes) return false;
		return attributeInfo.isExternal;
	}

	// Returns last @extern attribute attached to the node
	AstNode* getExternAttrib(CompilationContext* c) {
		if (!hasAttributes) return null;
		foreach_reverse(AstIndex attrib; attributeInfo.attributes) {
			auto attribNode = attrib.get_node(c);
			if (attribNode.astType != AstType.decl_builtin_attribute) continue;
			if (attribNode.subType == BuiltinAttribSubType.extern_syscall) return attribNode;
			if (attribNode.subType == BuiltinAttribSubType.extern_module) return attribNode;
		}
		return null;
	}

	bool isType()       { return cast(bool)(flags & AstFlags.isType); }
	bool isLvalue()     { return cast(bool)(flags & AstFlags.isLvalue); }
	bool hasAttributes(){ return cast(bool)(flags & AstFlags.hasAttributes); }
	bool isTemplateInstance() { return cast(bool)(flags & AstFlags.isTemplateInstance); }
	bool isGlobal()     { return (flags & AstFlags.scopeKindMask) == AstFlags.isGlobal; }
	bool isMember()     { return (flags & AstFlags.scopeKindMask) == AstFlags.isMember; }
	bool isLocal()      { return (flags & AstFlags.scopeKindMask) == AstFlags.isLocal;  }
}

struct AstNode
{
	mixin AstNodeData;
}

@(AstType.error)
struct ErrorAstNode
{
	mixin AstNodeData!(AstType.error);
}

alias AstNodes = Array!AstIndex;
alias AstNodeMap = HashMap!(Identifier, AstIndex, Identifier.init);

enum hasAstNodeType(T) = getUDAs!(T, AstType).length > 0;
enum getAstNodeType(T) = getUDAs!(T, AstType)[0];

AstNodePrinter printer(AstIndex nodeIndex, CompilationContext* context)
{
	return AstNodePrinter(nodeIndex, context);
}

struct AstNodePrinter
{
	AstIndex nodeIndex;
	CompilationContext* context;

	void toString(scope void delegate(const(char)[]) sink) {
		if (!nodeIndex) {
			sink("<null>");
			return;
		}
		AstNode* node = context.getAstNode(nodeIndex);
		if (node.isType) node.as_type(context).printType(sink, context);
	}
}

void print_node_name(ref TextSink sink, AstIndex nodeIndex, CompilationContext* c)
{
	AstNode* node = c.getAstNode(nodeIndex);
	switch(node.astType) with(AstType)
	{
		case decl_alias: sink.put(c.idString(node.as!AliasDeclNode(c).id)); break;
		case decl_builtin: sink.put(c.idString(node.as!BuiltinNode(c).id)); break;
		case decl_module: sink.put(c.idString(node.as!ModuleDeclNode(c).id)); break;
		case decl_struct: sink.put(c.idString(node.as!StructDeclNode(c).id)); break;
		case decl_function: sink.put(c.idString(node.as!FunctionDeclNode(c).id)); break;
		case decl_var: sink.put(c.idString(node.as!VariableDeclNode(c).id)); break;
		case decl_enum: sink.put(c.idString(node.as!EnumDeclaration(c).id)); break;
		case decl_enum_member: sink.put(c.idString(node.as!EnumMemberDecl(c).id)); break;
		case expr_name_use: sink.put(c.idString(node.as!NameUseExprNode(c).id(c))); break;
		case expr_member: sink.put(c.idString(node.as!MemberExprNode(c).memberId(c))); break;
		case decl_template: sink.put(c.idString(node.as!TemplateDeclNode(c).id)); break;
		case type_basic: sink.put(basicTypeNames[node.as!BasicTypeNode(c).basicType]); break;
		case type_ptr:
			print_node_name(sink, node.as!PtrTypeNode(c).base, c);
			sink.put("*");
			break;
		case type_slice:
			print_node_name(sink, node.as!SliceTypeNode(c).base, c);
			sink.put("[]");
			break;
		case type_static_array:
			auto arr = node.as!StaticArrayTypeNode(c);
			print_node_name(sink, arr.base, c);
			sink.putf("[%s]", arr.length);
			break;
		case type_func_sig:
			auto sig = node.as!FunctionSignatureNode(c);
			print_node_name(sink, sig.returnType, c);
			sink.put(" function(");
			foreach(i, AstIndex param; sig.parameters)
			{
				if (i > 0) sink.put(", ");
				print_node_name(sink, param, c);
			}
			sink.put(")");
			break;
		case expr_call:
			print_node_name(sink, node.as!CallExprNode(c).callee, c);
			break;
		default: sink.formattedWrite("%s", node.astType);
	}
}

void check_is_type(ref AstIndex nodeIndex, CompilationContext* c)
{
	AstNode* node = c.getAstNode(nodeIndex);
	if (!node.isType) {
		c.error(node.loc, "%s is not a type", get_node_kind_name(nodeIndex, c));
		nodeIndex = CommonAstNodes.type_error;
	}
}

ref Identifier get_node_id(AstIndex nodeIndex, CompilationContext* c)
{
	AstNode* node = c.getAstNode(nodeIndex);
	switch(node.astType) with(AstType)
	{
		case decl_alias: return node.as!AliasDeclNode(c).id;
		case decl_builtin: return node.as!BuiltinNode(c).id;
		case decl_module: return node.as!ModuleDeclNode(c).id;
		case decl_struct: return node.as!StructDeclNode(c).id;
		case decl_function: return node.as!FunctionDeclNode(c).id;
		case decl_var: return node.as!VariableDeclNode(c).id;
		case decl_enum: return node.as!EnumDeclaration(c).id;
		case decl_enum_member: return node.as!EnumMemberDecl(c).id;
		case expr_name_use: return node.as!NameUseExprNode(c).id(c);
		case expr_member: return node.as!MemberExprNode(c).memberId(c);
		case decl_template: return node.as!TemplateDeclNode(c).id;
		default: assert(false, format("got %s", node.astType));
	}
}

AstIndex get_node_type(AstIndex nodeIndex, CompilationContext* c)
{
	if (nodeIndex.isUndefined) return nodeIndex;

	AstNode* node = c.getAstNode(nodeIndex);
	//c.assertf(node.state >= AstNodeState.type_check_done, "get_node_type on node %s in state %s", node.astType, node.state);

	switch(node.astType) with(AstType)
	{
		case decl_alias: return node.as!AliasDeclNode(c).initializer.get_node_type(c);
		case decl_struct: return nodeIndex;
		case decl_function: return node.as!FunctionDeclNode(c).signature.get_node_type(c);
		case decl_var: return node.as!VariableDeclNode(c).type.get_node_type(c);
		case decl_enum: return nodeIndex;
		case decl_enum_member: return node.as!EnumMemberDecl(c).type.get_node_type(c);
		case type_basic, type_func_sig, type_ptr, type_slice, type_static_array: return nodeIndex;
		case expr_name_use: return node.as!NameUseExprNode(c).entity.get_node_type(c);
		case error, literal_null, literal_bool, literal_int, literal_float, literal_string, literal_array, literal_special, expr_call, expr_index, expr_slice, expr_bin_op, expr_un_op, expr_type_conv, expr_member:
			return node.as!ExpressionNode(c).type.get_node_type(c);

		default: assert(false, format("get_node_type used on %s", node.astType));
	}
}

AstIndex get_node_alias(AstIndex nodeIndex, CompilationContext* c)
{
	if (nodeIndex.isUndefined) return nodeIndex;

	AstNode* node = c.getAstNode(nodeIndex);
	//c.assertf(node.state >= AstNodeState.type_check_done, "get_node_type on node in state %s", node.state);

	switch(node.astType) with(AstType)
	{
		case decl_alias: return node.as!AliasDeclNode(c).initializer.get_node_alias(c);
		case decl_struct: return CommonAstNodes.type_type;
		case decl_function: return CommonAstNodes.type_type;
		case decl_var: return CommonAstNodes.type_alias;
		case decl_enum: return CommonAstNodes.type_type;
		case decl_enum_member: return CommonAstNodes.type_alias;
		case type_basic, type_func_sig, type_ptr, type_slice, type_static_array: return CommonAstNodes.type_type;
		case expr_name_use: return node.as!NameUseExprNode(c).entity.get_node_alias(c);
		case error, literal_null, literal_bool, literal_int, literal_float, literal_string, literal_array, literal_special, expr_call, expr_index, expr_slice, expr_bin_op, expr_un_op, expr_type_conv, expr_member:
			return CommonAstNodes.type_alias;

		default: assert(false, format("get_node_alias used on %s", node.astType));
	}
}

AstIndex get_expr_type(AstIndex nodeIndex, CompilationContext* c)
	out(res; res.isDefined, "null result")
{
	assert(nodeIndex, "get_expr_type nodeIndex is null");

	AstNode* node = c.getAstNode(nodeIndex);
	//c.assertf(node.state >= AstNodeState.type_check_done, "get_expr_type on node in state %s", node.state);

	switch(node.astType) with(AstType)
	{
		case decl_alias: return node.as!AliasDeclNode(c).initializer.get_expr_type(c);
		case decl_struct: return CommonAstNodes.type_type;
		case decl_function: return node.as!FunctionDeclNode(c).signature.get_node_type(c);
		case decl_var: return node.as!VariableDeclNode(c).type.get_node_type(c);
		case decl_enum: return CommonAstNodes.type_type;
		case decl_enum_member: return node.as!EnumMemberDecl(c).type.get_node_type(c);
		case type_basic, type_func_sig, type_ptr, type_slice, type_static_array: return CommonAstNodes.type_type;
		case expr_name_use: return node.as!NameUseExprNode(c).type;
		case error, literal_null, literal_bool, literal_int, literal_float, literal_string, literal_array, literal_special, expr_call, expr_index, expr_slice, expr_bin_op, expr_un_op, expr_type_conv, expr_member:
			return node.as!ExpressionNode(c).type.get_node_type(c);

		default: assert(false, format("get_expr_type used on %s", node.astType));
	}
}

AstIndex get_effective_node(AstIndex nodeIndex, CompilationContext* c)
{
	if (nodeIndex.isUndefined) return nodeIndex;

	AstNode* node = c.getAstNode(nodeIndex);
	//c.assertf(node.state >= AstNodeState.type_check_done, "get_effective_node on node in state %s", node.state);

	switch(node.astType) with(AstType)
	{
		case decl_alias: return node.as!AliasDeclNode(c).initializer.get_effective_node(c);
		case decl_alias_array: return nodeIndex;
		case decl_template: return nodeIndex;
		case decl_struct: return nodeIndex;
		case decl_builtin: return nodeIndex;
		case decl_function: return nodeIndex;
		case decl_var: return nodeIndex;
		case decl_enum: return nodeIndex;
		case decl_enum_member: return nodeIndex;
		case type_basic, type_ptr, type_slice, type_static_array: return nodeIndex;
		case expr_name_use: return node.as!NameUseExprNode(c).entity.get_effective_node(c);
		case error, literal_int, literal_float, literal_string, literal_array, literal_special, expr_call, expr_index, expr_slice, expr_bin_op, expr_un_op, expr_type_conv, expr_member:
			return nodeIndex;

		default: assert(false, format("get_effective_node used on %s", node.astType));
	}
}

AstIndex get_ast_index(T)(T* node, CompilationContext* context)
{
	return context.getAstNodeIndex(node);
}

string get_node_kind_name(AstIndex nodeIndex, CompilationContext* c)
{
	AstNode* node = c.getAstNode(nodeIndex);

	switch(node.astType) with(AstType)
	{
		case decl_alias: return "alias";
		case decl_struct: return "struct";
		case decl_function: return "function";
		case decl_var: return "variable";
		case decl_enum: return "enum";
		case decl_enum_member: return "enum member";
		case type_basic: return "basic type";
		case type_ptr: return "pointer type";
		case type_slice: return "slice type";
		case type_static_array: return "static array type";
		case expr_name_use: return node.as!NameUseExprNode(c).entity.get_node_kind_name(c);
		case literal_int: return "int literal";
		case literal_float: return "float literal";
		case literal_string: return "string literal";
		case literal_array: return "array literal";
		case literal_special: return "special literal";
		case expr_call: return "call expression";
		case expr_index: return "index expression";
		case expr_slice: return "slice expression";
		case expr_bin_op: return "binary expression";
		case expr_un_op: return "unary expression";
		case expr_type_conv: return "type conversion expression";
		case expr_member: return "member access expression";

		default: return node.astType.to!string;
	}
}

AstIndex find_innermost_owner(AstIndex parentScope, AstType ownerType, CompilationContext* c)
{
	c.assertf(ownerType == AstType.decl_struct ||
		ownerType == AstType.decl_module ||
		ownerType == AstType.decl_function,
		"Invalid owner type requested (%s)", ownerType);

	while(true)
	{
		c.assertf(parentScope.isDefined, "Undefined scope");

		AstIndex owner = parentScope.get_scope(c).owner;
		c.assertf(parentScope.isDefined, "Undefined owner");

		AstNode* ownerNode = owner.get_node(c);

		if (ownerNode.astType == ownerType) return owner;

		switch(ownerNode.astType) {
			case AstType.decl_module:
				return AstIndex(0); // didn't find the requested owner

			case AstType.decl_struct:
				parentScope = ownerNode.as!StructDeclNode(c).parentScope;
				continue;

			case AstType.decl_function:
				parentScope = ownerNode.as!FunctionDeclNode(c).parentScope;
				continue;

			default: c.internal_error("Invalid owner type found (%s)", ownerNode.astType);
		}
	}
}
