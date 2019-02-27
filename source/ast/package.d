/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module ast;

import std.stdio;

import all;

public import ast.declaration;
public import ast.dump;
public import ast.expression;
public import ast.statement;
public import ast.type;
public import ast.visitor;

enum AstType : ubyte
{
	error,
	abstract_node,

	decl_module,
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
	expr_member,
	expr_call,
	expr_index,
	expr_bin_op,
	expr_un_op,
	expr_type_conv,

	literal_int,
	literal_string,

	type_basic,
	type_ptr,
	type_static_array,
	type_slice,
	type_struct,
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
	user1         = 1 << 11,
	user2         = 1 << 12,
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
}

mixin template SymRefNodeData()
{
	SymbolRef symRef;

	string strId(CompilationContext* context) { return context.idString(symRef.id(isSymResolved)); }
	Identifier id() { return symRef.id(isSymResolved); }
	void resolveSymbol(Symbol* symbol) {
		symRef._symbol = symbol;
		flags |= AstFlags.isSymResolved;
	}
	Symbol* getSym() { assert(isSymResolved, "Unresolved symbol"); return symRef._symbol; }
}

struct AstNode
{
	mixin AstNodeData;
}
