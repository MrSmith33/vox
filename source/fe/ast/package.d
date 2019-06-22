/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.ast;

import std.stdio;

import all;

public import fe.ast.declaration;
public import fe.ast.dump;
public import fe.ast.expression;
public import fe.ast.statement;
public import fe.ast.type;
public import fe.ast.visitor;

enum AstType : ubyte
{
	error,
	abstract_node,

	decl_module,
	decl_import,
	decl_function,
	decl_var,
	decl_struct,
	decl_enum,
	decl_enum_member,

	stmt_block,
	stmt_if,
	stmt_while,
	stmt_do_while,
	stmt_return,
	stmt_break,
	stmt_continue,

	expr_name_use,
	expr_var_name_use, // var name
	expr_func_name_use, // function name
	expr_member_name_use, // struct member name
	expr_type_name_use, // type name

	expr_member,
	expr_struct_member,
	expr_enum_member,
	expr_slice_member,

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
}

enum AstFlags
{
	isDeclaration = 1 <<  0,
	isScope       = 1 <<  1,
	isExpression  = 1 <<  2,
	/// Can be applied to expression if it is in place of stmt
	isStatement   = 1 <<  3,
	isType        = 1 <<  4,
	isSymResolved = 1 <<  5,
	/// Is added to expression nodes that are being assigned to
	isLvalue      = 1 <<  6,
	isLiteral     = 1 <<  7,
	isAssignment  = 1 <<  8,
	/// Marks expression that is used as func argument.
	/// Needed to handle calling conventions properly.
	isArgument    = 1 <<  9,
	/// Declaration at module level
	isGlobal      = 1 << 10,
	isInOrderedScope = 1 << 11,
	user1         = 1 << 12,
	user2         = 1 << 13,
}

mixin template AstNodeData(AstType _astType = AstType.abstract_node, int default_flags = 0)
{
	this(Args...)(TokenIndex loc, Args args) {
		this(loc);
		enum len = this.tupleof.length - 3;
		enum numDefault = len - args.length;
		static assert(args.length <= len, "Too many args");
		this.tupleof[3..$-numDefault] = args;
	}

	this(TokenIndex loc) {
		this.loc = loc; this.astType = _astType; this.flags = cast(ushort)default_flags; }
	/*this(TokenIndex loc, int flags) {
		this.loc = loc; this.astType = _astType; this.flags = cast(ushort)(default_flags|flags); }
	this(TokenIndex loc, AstType astType) {
		this.loc = loc; this.astType = astType; this.flags = cast(ushort)default_flags; }
	this(TokenIndex loc, AstType astType, int flags) {
		this.loc = loc; this.astType = astType; this.flags = cast(ushort)(default_flags|flags); }*/
	TokenIndex loc;
	AstType astType = _astType;
	ushort flags;

	AstNode* as_node() {
		return cast(AstNode*)&this;
	}
	ExpressionNode* as_expr() {
		if (isExpression) return cast(ExpressionNode*)&this;
		return null;
	}

	bool isDeclaration() { return cast(bool)(flags & AstFlags.isDeclaration); }
	bool isScope() { return cast(bool)(flags & AstFlags.isScope); }
	bool isExpression() { return cast(bool)(flags & AstFlags.isExpression); }
	bool isStatement() { return cast(bool)(flags & AstFlags.isStatement); }
	bool isType() { return cast(bool)(flags & AstFlags.isType); }
	bool isSymResolved() { return cast(bool)(flags & AstFlags.isSymResolved); }
	bool isLvalue() { return cast(bool)(flags & AstFlags.isLvalue); }
	bool isLiteral() { return cast(bool)(flags & AstFlags.isLiteral); }
	bool isAssignment() { return cast(bool)(flags & AstFlags.isAssignment); }
	bool isArgument() { return cast(bool)(flags & AstFlags.isArgument); }
	bool isGlobal() { return cast(bool)(flags & AstFlags.isGlobal); }
	bool isInOrderedScope() { return cast(bool)(flags & AstFlags.isInOrderedScope); }
}

Identifier get_node_id(AstNode* n) {
	switch(n.astType) with(AstType)
	{
		case decl_module: return n.cast_decl_module.id;
		case decl_struct: return n.cast_decl_struct.id;
		case decl_function: return n.cast_decl_function.id;
		case decl_var: return n.cast_decl_var.id;
		case decl_enum: return n.cast_decl_enum.id;
		case decl_enum_member: return n.cast_decl_enum_member.id;
		case expr_name_use, expr_var_name_use, expr_func_name_use, expr_member_name_use, expr_type_name_use:
			return n.cast_expr_name_use.id;
		default: assert(false, format("got %s", n.astType));
	}
}

TypeNode* get_node_type(AstNode* n) {
	AstNode* type;
	switch(n.astType) with(AstType)
	{
		case decl_struct: type = n; break;
		case decl_function: type = n.cast_decl_function.returnType.as_node; break;
		case decl_var:
			type = n.cast_decl_var.type.as_node; break;
		case decl_enum: type = n; break;
		case decl_enum_member: type = n.cast_decl_enum_member.type.as_node; break;
		case type_basic: type = n; break;
		case expr_var_name_use: type = n.cast_expr_name_use.entity.cast_decl_var.type.as_node; break;
		case expr_type_name_use:
			type = n.cast_expr_name_use.entity;
			break;

		case literal_int, literal_string, expr_call, expr_index, expr_bin_op, expr_un_op, expr_type_conv:
		case expr_member, expr_struct_member, expr_enum_member, expr_slice_member:
			type = n.as_expr.type.as_node;
			break;

		default: assert(false, format("get_node_type used on %s", n.astType));
	}
	if (type.astType == AstType.expr_type_name_use)
	{
		type = type.cast_expr_name_use.entity;
		return type.cast_type_node;
	}
	return cast(TypeNode*)type;
}

struct AstNode
{
	mixin AstNodeData;
}

struct ErrorAstNode
{
	mixin AstNodeData!(AstType.error);
}

alias AstNodes = Array!(AstNode*);
