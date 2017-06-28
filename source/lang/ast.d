/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lang.ast;

alias Identifier = uint;

class IdentifierMap {
	string[] strings;
	uint[string] map;

	string get(Identifier id) {
		return strings[id];
	}

	Identifier get(string str) {
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
}

abstract class AstNode {
	void accept(AstVisitor);
}

abstract class Declaration : AstNode {

}

class Module : Declaration {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	FunctionDeclaration[] functions;
}

class FunctionDeclaration : Declaration {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Identifier id;
	BlockStatement statements;
}



abstract class Statement : AstNode {

}

class IfStatement : Statement {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Expression condition;
	Statement thenStatement;
}

class IfElseStatement : Statement {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Expression condition;
	Statement thenStatement;
	Statement elseStatement;
}

class WhileStatement : Statement {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Expression condition;
	Statement statement;
}

class DoWhileStatement : Statement {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Expression condition;
	Statement statement;
}

class ReturnStatement : Statement {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Expression expression; // can be null
}

class BlockStatement : Statement {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Statement[] statements;
}

class ExpressionStatement : Statement {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Expression expression;
}



abstract class Expression : AstNode {

}

class VariableExpression : Expression {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	Identifier id;

}

class ConstExpression : Expression {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	int value;
}

enum BinOp { ADD, SUB, MUL, DIV, MOD, SHL, SHR, ASHR, AND, OR, ANDAND, OROR, LT, GT, LE, GE, EQUAL, NOTEQUAL, ASSIGN }

class BinaryExpression : Expression {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	override void accept(AstVisitor v) { v.visit(this); }
	BinOp op;
	Expression left;
	Expression right;
}
