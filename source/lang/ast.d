/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lang.ast;

import lang.lex2 : SourceLocation;

alias Identifier = uint;

class IdentifierMap {
	string[] strings;
	uint[string] map;

	string get(Identifier id) {
		return strings[id];
	}

	Identifier find(string str) {
		return map.get(str, uint.max);
	}

	Identifier getOrReg(string str) {
		uint id = map.get(str, uint.max);
		if (id == uint.max) {
			id = cast(uint)strings.length;
			map[str] = id;
			strings ~= str;
		}
		return id;
	}
}

abstract class AstVisitor
{
	void visit(Module){}
	void visit(FunctionDeclaration){}
	void visit(BlockStatement){}
	void visit(IfStatement){}
	void visit(IfElseStatement){}
	void visit(WhileStatement){}
	void visit(DoWhileStatement){}
	void visit(ReturnStatement){}
	void visit(ExpressionStatement){}
	void visit(VariableExpression){}
	void visit(ConstExpression){}
	void visit(BinaryExpression){}
	void visit(CallExpression){}
}

class DepthAstVisitor : AstVisitor
{
	override void visit(Module m) { foreach(f; m.functions) f.accept(this); }
	override void visit(FunctionDeclaration f) { foreach(p; f.parameters) p.accept(this); f.statements.accept(this); }
	override void visit(IfStatement n) { n.condition.accept(this); n.thenStatement.accept(this); }
	override void visit(IfElseStatement n) { n.condition.accept(this); n.thenStatement.accept(this); n.elseStatement.accept(this); }
	override void visit(WhileStatement w) { w.condition.accept(this); w.statement.accept(this); }
	override void visit(DoWhileStatement w) { w.condition.accept(this); w.statement.accept(this); }
	override void visit(ReturnStatement r) { if (r.expression) r.expression.accept(this); }
	override void visit(BlockStatement b) { foreach(s; b.statements) s.accept(this); }
	override void visit(ExpressionStatement e) { e.expression.accept(this); }
	override void visit(BinaryExpression b) { b.left.accept(this); b.right.accept(this); }
	override void visit(CallExpression c) { foreach(a; c.args) a.accept(this); }
}

abstract class AstNode {
	void accept(AstVisitor);
	SourceLocation loc;
}

abstract class Declaration : AstNode {

}

class Module : Declaration {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	FunctionDeclaration[] functions;
	FunctionDeclaration getFunction(Identifier id)
	{
		foreach(fun; functions)
			if (fun.id == id) return fun;
		throw new Error("Invalid function id");
	}
}

class FunctionDeclaration : Declaration {
	this(SourceLocation loc, typeof(this.tupleof) args) { this.loc = loc; this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Identifier id;
	VariableExpression[] parameters;
	BlockStatement statements;
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



abstract class Expression : AstNode {

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
