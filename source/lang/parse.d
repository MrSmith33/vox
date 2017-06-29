/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lang.parse;

import lang.lex2;
import lang.ast;

import std.stdio;

/*  <module> ::= <declaration>*
 *  <declaration> ::= <func_declaration>
 *  <func_decl> ::= "func" <id> "(" <id_list> ")" <block_statement>
 *  <block_statement> ::= "{" <statement>* "}"
 *  <statement> ::= "if" <paren_expr> <statement> /
 *                  "if" <paren_expr> <statement> "else" <statement> /
 *                  "while" <paren_expr> <statement> /
 *                  "do" <statement> "while" <paren_expr> ";" /
 *                  "return" <expr>? ";" /
 *                  <block_statement> /
 *                  <expr> ";" /
 *                  ";"
 *  <id_list> ::= (<id> ",")*
 *  <paren_expr> ::= "(" <expr> ")"
 *  <expr> ::= <test> | <id> "=" <expr>
 *  <test> ::= <sum> | <sum> ("=="|"!="|"<"|">"|"<="|"<=") <sum>
 *  <sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term>
 *  <term> ::= <id> | <int> | <paren_expr>
 *  <id> ::= "a" | "b" | "c" | "d" | ... | "z"
 *  <int> ::= <an_unsigned_decimal_integer>
 */

/*
string input = q{
	func main(){ return 1; return 2+a; return (result < 3); }
	func sub1(){ return 42; a=b=c; }
	func sub2(){ if (a<b) c=10; }
	func sub3(){ if (a<b) c=10; else c=20; }
	func sub4(){ while(i<100){a=a+i;i=i+1;} }
	func sub5(){ do{a=a+i;i=i+1;}while(i<100); }
};
void main()
{
	auto idMap = new IdentifierMap();
	Lexer2 lexer = Lexer2(input);

	Parser parser = Parser(&lexer, idMap);
	try
	{
		auto root = parser.parseModule();
		printAST(root, &idMap);
	}
	catch(ParsingException e)
	{
		writefln("%s: [ERROR] %s", e.token.start.pos, e.msg);
		writeln(e);
	}
}
*/

void printAST(AstNode n, IdentifierMap idMap, int indentSize = 1)
{
	import std.range : repeat;
	if (!n) return;

	int indent = -indentSize;
	void print(Args...)(Args args) {
		auto i = ' '.repeat(indent);
		writeln(i, args);
	}

	AstVisitor visitor;

	void pr_node(AstNode node) // print node
	{
		indent += indentSize;
		node.accept(visitor);
		indent -= indentSize;
	}

	visitor = new class AstVisitor {
		override void visit(Module m) { print("MODULE"); foreach(f; m.functions) pr_node(f); }
		override void visit(FunctionDeclaration f) {
			print("FUNC ", idMap.get(f.id));
			indent += indentSize;
			foreach(p; f.parameters)
				print("PARAM ", idMap.get(p.id));
			indent -= indentSize;
			pr_node(f.statements);
		}
		override void visit(IfStatement n) { print("IF"); pr_node(n.condition); pr_node(n.thenStatement); }
		override void visit(IfElseStatement n) { print("IF"); pr_node(n.condition); pr_node(n.thenStatement); pr_node(n.elseStatement); }
		override void visit(WhileStatement w) { print("WHILE"); pr_node(w.condition); pr_node(w.statement); }
		override void visit(DoWhileStatement w) { print("DO"); pr_node(w.condition); pr_node(w.statement); }
		override void visit(ReturnStatement r) { print("RETURN"); if (r.expression) pr_node(r.expression); }
		override void visit(BlockStatement b) { print("BLOCK"); foreach(s; b.statements) pr_node(s); }
		override void visit(ExpressionStatement e) { print("EXPR"); pr_node(e.expression); }
		override void visit(VariableExpression v) { print("VAR ", idMap.get(v.id)); }
		override void visit(ConstExpression c) { print("CONST ", c.value); }
		override void visit(BinaryExpression b) { print("BINOP ", b.op); pr_node(b.left); pr_node(b.right); }
	};

	pr_node(n);
}

class ParsingException : Exception
{
	this(Args...)(Token token, string msg, Args args) {
		this.token = token;
		import std.string : format;
		super(format(msg, args));
	}
	Token token;
}

//version = Mmap_mem;
struct Parser
{
	Lexer2* lexer;
	IdentifierMap idMap;

	Token tok;

	version(Mmap_mem) {
		ubyte[] mem;
		void* pc;
		void setup() {
			import utils;
			if (mem.length == 0) mem = allocate(PAGE_SIZE * 64, false);
			pc = mem.ptr;
		}
		T make(T, Args...)(Args args) {
			import std.conv : emplace;
			enum size = __traits(classInstanceSize, T);
			T t = emplace!T(pc[0..size], args);
			pc += size;
			return t;
		}
	} else {
		void setup() {}
		T make(T, Args...)(Args args) { return new T(tok.loc, args); }
	}

	void nextToken() {
		do {
			tok = lexer.nextToken();
		}
		while (tok.type == TokenType.COMMENT);
	}

	void syntax_error(Args...)(Args args) {
		throw new ParsingException(tok, args);
	}

	Token expectAndConsume(TokenType type) {
		if (tok.type != type) {
			string tokenString = lexer.getTokenString(tok);
			syntax_error("Expected '%s', while got '%s'", type, tokenString);
		}
		scope(exit) nextToken();
		return tok;
	}


	Module parseModule() { // <module> ::= <declaration>*
		FunctionDeclaration[] functions;
		nextToken();
		while (tok.type != TokenType.EOI)
		{
			functions ~= func_declaration();
		}
		return make!Module(functions);
	}

	FunctionDeclaration func_declaration() // <declaration> ::= <func_declaration>
	{
		expectAndConsume(TokenType.FUNC_SYM); // <func_decl> ::= "func" <id> "(" (<id> ",")* ")" <compound_statement>
		Token funcNameTok = expectAndConsume(TokenType.ID);
		string funcName = lexer.getTokenString(funcNameTok);
		auto funcId = idMap.get(funcName);
		expectAndConsume(TokenType.LPAREN);
		VariableExpression[] params;
		while (tok.type != TokenType.RPAREN)
		{
			Token paramTok = expectAndConsume(TokenType.ID);
			string paramName = lexer.getTokenString(paramTok);
			Identifier paramId = idMap.get(paramName);
			params ~= make!VariableExpression(paramId);
			if (tok.type == TokenType.COMMA) nextToken();
			else break;
		}
		expectAndConsume(TokenType.RPAREN);
		auto statements = block_stmt();
		return make!FunctionDeclaration(funcId, params, statements);
	}

	BlockStatement block_stmt() // <compound_statement> ::= "{" <statement>* "}"
	{
		Statement[] statements;
		expectAndConsume(TokenType.LCURLY);
		while (tok.type != TokenType.RCURLY)
		{
			statements ~= statement();
		}
		expectAndConsume(TokenType.RCURLY);
		return make!BlockStatement(statements);
	}

	Statement statement()
	{
		switch (tok.type)
		{
			case TokenType.IF_SYM: /* "if" <paren_expr> <statement> */
				nextToken();
				Expression condition = paren_expr();
				Statement thenStatement = statement();
				if (tok.type == TokenType.ELSE_SYM) { /* ... "else" <statement> */
					nextToken();
					Statement elseStatement = statement();
					return make!IfElseStatement(condition, thenStatement, elseStatement);
				}
				else return make!IfStatement(condition, thenStatement);
			case TokenType.WHILE_SYM:  /* "while" <paren_expr> <statement> */
				nextToken();
				Expression condition = paren_expr();
				Statement statement = statement();
				return make!WhileStatement(condition, statement);
			case TokenType.DO_SYM:  /* "do" <statement> "while" <paren_expr> ";" */
				nextToken();
				Statement statement = statement();
				expectAndConsume(TokenType.WHILE_SYM);
				Expression condition = paren_expr();
				expectAndConsume(TokenType.SEMICOLON);
				return make!DoWhileStatement(condition, statement);
			case TokenType.RET_SYM:  /* return <expr> */
				nextToken();
				Expression expression = tok.type != TokenType.SEMICOLON ? expr() : null;
				expectAndConsume(TokenType.SEMICOLON);
				return make!ReturnStatement(expression);
			case TokenType.SEMICOLON:  /* ";" */
				nextToken();
				return make!BlockStatement(null); // TODO: make this error
			case TokenType.LCURLY:  /* "{" { <statement> } "}" */
				return block_stmt();
			default:  /* <expr> ";" */
				Expression expression = expr();
				expectAndConsume(TokenType.SEMICOLON);
				return make!ExpressionStatement(expression);
		}
	}

	Expression paren_expr() { /* <paren_expr> ::= "(" <expr> ")" */
		expectAndConsume(TokenType.LPAREN);
		auto res = expr();
		expectAndConsume(TokenType.RPAREN);
		return res;
	}

	Expression expr() { /* <expr> ::= <test> | <id> "=" <expr> */
		Expression t, n;
		if (tok.type != TokenType.ID) return test();
		n = test();
		if (cast(VariableExpression)n && tok.type == TokenType.EQ)
		{
			nextToken();
			t = n;
			n = make!BinaryExpression(BinOp.ASSIGN, t, expr());
		}
		return n;
	}

	Expression test() { /* <test> ::= <sum> | <sum> "<" <sum> */
		Expression t, n = sum();
		if (tok.type == TokenType.LT)
		{
			nextToken();
			t = n;
			n = make!BinaryExpression(BinOp.LT, t, sum());
		}
		return n;
	}

	Expression sum() { /* <sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term> */
		Expression n = term();
		Expression t;
		loop: while (true)
		{
			BinOp op;
			switch(tok.type) {
				case TokenType.PLUS : op = BinOp.ADD; break;
				case TokenType.MINUS: op = BinOp.SUB; break;
				default: break loop;
			}
			nextToken();
			t = n;
			n = make!BinaryExpression(op, t, term());
		}
		return n;
	}

	Expression term() { /* <term> ::= <id> | <int> | <paren_expr> */
		if (tok.type == TokenType.ID) {
			string name = lexer.getTokenString(tok);
			Identifier id = idMap.get(name);
			nextToken();
			return make!VariableExpression(id);
		}
		else if (tok.type == TokenType.DECIMAL_NUM) {
			string num = lexer.getTokenString(tok);
			import std.conv : to;
			int value=to!int(num);
			nextToken();
			return make!ConstExpression(value);
		}
		else return paren_expr();
	}
}
