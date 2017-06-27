/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
import ast;

alias Identifier = uint;

struct IdentifierMap {
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

class AstNode {

}

class Declaration : AstNode {

}

class Module : Declaration {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	FunctionDeclaration[] functions;
}

class FunctionDeclaration : Declaration {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	Identifier id;
	BlockStatement statements;
}



class Statement : AstNode {

}

class IfStatement : Statement {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	Expression condition;
	Statement thenStatement;
}

class IfElseStatement : Statement {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	Expression condition;
	Statement thenStatement;
	Statement elseStatement;
}

class WhileStatement : Statement {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	Expression condition;
	Statement statement;
}

class DoWhileStatement : Statement {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	Expression condition;
	Statement statement;
}

class ReturnStatement : Statement {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	Expression expression; // can be null
}

class BlockStatement : Statement {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	Statement[] statements;
}

class ExpressionStatement : Statement {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	Expression expression;
}



class Expression : AstNode {

}

class VariableExpression : Expression {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	Identifier id;
}

class ConstExpression : Expression {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	int value;
}

enum BinOp { ADD, SUB, MUL, DIV, MOD, SHL, SHR, ASHR, AND, OR, ANDAND, OROR, LT, GT, LE, GE, EQUAL, NOTEQUAL, ASSIGN }

class BinaryExpression : Expression {
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	BinOp op;
	Expression left;
	Expression right;
}
