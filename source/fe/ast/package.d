/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.ast;

import std.traits : getUDAs;
import std.stdio;

import all;

public import fe.ast.ast_index;
public import fe.ast.decl;
public import fe.ast.dump;
public import fe.ast.expr;
public import fe.ast.lexer;
public import fe.ast.parser;
public import fe.ast.stmt;
public import fe.ast.type;
public import fe.ast.visitor;

enum AstType : ubyte
{
	error,
	abstract_node,

	decl_alias,
	decl_builtin,
	decl_module,
	decl_import,
	decl_function,
	decl_var,
	decl_struct,
	decl_enum,
	decl_enum_member,
	decl_static_if,

	stmt_block,
	stmt_if,
	stmt_while,
	stmt_do_while,
	stmt_for,
	stmt_return,
	stmt_break,
	stmt_continue,

	expr_name_use,
	expr_member,
	expr_call,
	expr_index,
	expr_bin_op,
	expr_un_op,
	expr_type_conv,

	literal_int,
	literal_string,
	literal_null,
	literal_bool,

	type_basic,
	type_ptr,
	type_static_array,
	type_slice,
	type_func_sig,
}

enum AstFlags : ushort
{
	isDeclaration    = 1 <<  0,
	isScope          = 1 <<  1,
	isExpression     = 1 <<  2,
	/// Can be applied to expression if it is in place of stmt
	isStatement      = 1 <<  3,
	isType           = 1 <<  4,
	/// Is added to expression nodes that are being assigned to
	isLvalue         = 1 <<  5,
	isLiteral        = 1 <<  6, // unused
	isAssignment     = 1 <<  7,
	/// Marks expression that is used as func argument.
	/// Needed to handle calling conventions properly.
	isArgument       = 1 <<  8,
	/// Declaration at module level
	isGlobal         = 1 <<  9,
	isInOrderedScope = 1 << 10,
	// used for node specific flags
	userFlag         = 1 << 11,
}

enum hasAstNodeType(T) = getUDAs!(T, AstType).length > 0;
enum getAstNodeType(T) = getUDAs!(T, AstType)[0];

/// Invariant: child.state >= parent.state
enum AstNodeState : ubyte
{
	// initial state
	parse_done,
	// is set after scope gathered all named entities
	name_register_self,
	name_register_self_done,
	// is set after scope gathered all named entities
	name_register_nested,
	name_register_nested_done,
	// is set after name uses resolved identifier
	name_resolve,
	name_resolve_done,
	// is set after type checking
	type_check,
	type_check_done,
	// is set after IR representation was created
	ir_gen,
	ir_gen_done,
}
static assert(AstNodeState.max <= 15, "Assumed to fit in 4 bits");

mixin template AstNodeData(AstType _astType = AstType.abstract_node, int default_flags = 0, AstNodeState _init_state = AstNodeState.parse_done)
{
	import std.bitmanip : bitfields;

	this(Args...)(TokenIndex loc, Args args) {
		this(loc);
		enum len = this.tupleof.length - 4;
		enum numDefault = len - args.length;
		static assert(args.length <= len, "Too many args");
		this.tupleof[4..$-numDefault] = args;
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

	T* as(T)(CompilationContext* c) {
		static if (hasAstNodeType!T)
		{
			c.assertf(astType == getAstNodeType!T, "as(%s) got %s", T.stringof, astType);
		}
		return cast(T*)&this;
	}

	TypeNode* as_type(CompilationContext* c) {
		c.assertf(isType, loc, "as_type(%s)", astType);
		return cast(TypeNode*)&this;
	}

	bool isDeclaration() { return cast(bool)(flags & AstFlags.isDeclaration); }
	bool isScope() { return cast(bool)(flags & AstFlags.isScope); }
	bool isExpression() { return cast(bool)(flags & AstFlags.isExpression); }
	bool isStatement() { return cast(bool)(flags & AstFlags.isStatement); }
	bool isType() { return cast(bool)(flags & AstFlags.isType); }
	bool isLvalue() { return cast(bool)(flags & AstFlags.isLvalue); }
	bool isLiteral() { return cast(bool)(flags & AstFlags.isLiteral); }
	bool isAssignment() { return cast(bool)(flags & AstFlags.isAssignment); }
	bool isArgument() { return cast(bool)(flags & AstFlags.isArgument); }
	bool isGlobal() { return cast(bool)(flags & AstFlags.isGlobal); }
	bool isInOrderedScope() { return cast(bool)(flags & AstFlags.isInOrderedScope); }
}

Identifier get_node_id(AstIndex nodeIndex, CompilationContext* c)
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
		default: assert(false, format("got %s", node.astType));
	}
}

AstIndex get_node_type(AstIndex nodeIndex, CompilationContext* c)
{
	if (nodeIndex.isUndefined) return nodeIndex;

	AstNode* node = c.getAstNode(nodeIndex);

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
		case literal_int, literal_string, expr_call, expr_index, expr_bin_op, expr_un_op, expr_type_conv, expr_member:
			return node.as!ExpressionNode(c).type.get_node_type(c);

		default: assert(false, format("get_node_type used on %s", node.astType));
	}
}

AstIndex get_effective_node(AstIndex nodeIndex, CompilationContext* c)
{
	if (nodeIndex.isUndefined) return nodeIndex;

	AstNode* node = c.getAstNode(nodeIndex);

	switch(node.astType) with(AstType)
	{
		case decl_alias: return node.as!AliasDeclNode(c).initializer.get_effective_node(c);
		case decl_struct: return nodeIndex;
		case decl_function: return nodeIndex;
		case decl_var: return nodeIndex;
		case decl_enum: return nodeIndex;
		case decl_enum_member: return nodeIndex;
		case type_basic, type_ptr, type_slice, type_static_array: return nodeIndex;
		case expr_name_use: return node.as!NameUseExprNode(c).entity.get_effective_node(c);
		case literal_int, literal_string, expr_call, expr_index, expr_bin_op, expr_un_op, expr_type_conv, expr_member:
			return nodeIndex;

		default: assert(false, format("get_node_type used on %s", node.astType));
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
		case literal_string: return "string literal";
		case expr_call: return "call expression";
		case expr_index: return "index expression";
		case expr_bin_op: return "binary expression";
		case expr_un_op: return "unary expression";
		case expr_type_conv: return "type conversion expression";
		case expr_member: return "member access expression";

		default: return node.astType.to!string;
	}
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
