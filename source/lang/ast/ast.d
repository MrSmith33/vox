/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lang.ast.ast;

import lang.lex : SourceLocation, TokenType;
import lang.identifier;
public import std.stdio;

// The order is the same as in TokenType enum
enum BasicType
{
	t_void,
	t_bool,

	t_i8,
	t_i16,
	t_i32,
	t_i64,
	t_isize,

	t_u8,
	t_u16,
	t_u32,
	t_u64,
	t_usize,

	t_f32,
	t_f64,
}

string[] basicTypeNames = ["void", "bool", "i8", "i16", "i32",
"i64", "isize", "u8", "u16", "u32", "u64", "usize", "f32", "f64"];

BasicType tokenTypeToBasicType(TokenType tt)
{
	return cast(BasicType)(tt - TokenType.TYPE_LIST_START);
}

abstract class AstVisitor
{
	void visit(Module){}
	void visit(FunctionDeclaration){}
	void visit(VarDeclaration){}
	void visit(StructDeclaration){}
	void visit(BlockStatement){}
	void visit(IfStatement){}
	void visit(IfElseStatement){}
	void visit(WhileStatement){}
	void visit(DoWhileStatement){}
	void visit(ReturnStatement){}
	void visit(ExpressionStatement){}
	void visit(DeclarationStatement){}
	void visit(VariableExpression){}
	void visit(ConstExpression){}
	void visit(BinaryExpression){}
	void visit(CallExpression){}
	void visit(TypeAstNode){}
	void visit(ParameterAstNode){}
}


class FunctionVisitor : AstVisitor
{
	override void visit(FunctionDeclaration f) { foreach(p; f.parameters) p.accept(this); f.statements.accept(this); }
	override void visit(IfStatement n) { n.condition.accept(this); n.thenStatement.accept(this); }
	override void visit(IfElseStatement n) { n.condition.accept(this); n.thenStatement.accept(this); n.elseStatement.accept(this); }
	override void visit(WhileStatement w) { w.condition.accept(this); w.statement.accept(this); }
	override void visit(DoWhileStatement w) { w.condition.accept(this); w.statement.accept(this); }
	override void visit(ReturnStatement r) { if (r.expression) r.expression.accept(this); }
	override void visit(BlockStatement b) { foreach(s; b.statements) s.accept(this); }
	override void visit(ExpressionStatement e) { e.expression.accept(this); }
	override void visit(DeclarationStatement d){ d.declaration.accept(this); }
	override void visit(BinaryExpression b) { b.left.accept(this); b.right.accept(this); }
	override void visit(CallExpression c) { foreach(a; c.args) a.accept(this); }
}


abstract class AstNode {
	void accept(AstVisitor);
	SourceLocation loc;
}

class TypeAstNode : AstNode {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	BasicType basicType;
}

abstract class Declaration : AstNode {

}

abstract class ScopeDeclaration : Declaration {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	FunctionDeclaration[] functions;
	VarDeclaration[] variables;
	StructDeclaration[] structs;
	FunctionDeclaration getFunction(Identifier id)
	{
		foreach(fun; functions)
			if (fun.id == id) return fun;
		throw new Error("Invalid function id");
	}
}

class Module : ScopeDeclaration {
	this(SourceLocation loc, typeof(ScopeDeclaration.tupleof) args) { super(loc, args); }
	override void accept(AstVisitor v) { v.visit(this); }
}

class FunctionDeclaration : Declaration {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Identifier id;
	TypeAstNode returnType;
	ParameterAstNode[] parameters;
	BlockStatement statements;
}

class VarDeclaration : Declaration {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Identifier id;
	TypeAstNode type;
}

// WIP
class StructDeclaration : ScopeDeclaration {
	this(SourceLocation loc, typeof(ScopeDeclaration.tupleof) argsScope, typeof(this.tupleof) args)
	{ super(loc, argsScope); this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Identifier id;
}

class ParameterAstNode : AstNode {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Identifier id;
	TypeAstNode type;
}


abstract class Statement : AstNode {

}

class IfStatement : Statement {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Expression condition;
	Statement thenStatement;
}

class IfElseStatement : Statement {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Expression condition;
	Statement thenStatement;
	Statement elseStatement;
}

class WhileStatement : Statement {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Expression condition;
	Statement statement;
}

class DoWhileStatement : Statement {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Expression condition;
	Statement statement;
}

class ReturnStatement : Statement {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Expression expression; // can be null
}

class BlockStatement : Statement {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Statement[] statements;
}

class ExpressionStatement : Statement {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Expression expression;
}

class DeclarationStatement : Statement {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Declaration declaration;
}


abstract class Expression : AstNode {
	TypeAstNode type;
}

class VariableExpression : Expression {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Identifier id;
}

class ConstExpression : Expression {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	long value;
}

enum BinOp { ADD, SUB, MUL, DIV, MOD, SHL, SHR, ASHR, AND, OR, ANDAND, OROR, LT, GT, LE, GE, EQUAL, NOTEQUAL, ASSIGN }

class BinaryExpression : Expression {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	BinOp op;
	Expression left;
	Expression right;
}

class CallExpression : Expression {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Identifier calleeId;
	Expression[] args;
}
