/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lang.parse;

import lang.lex;
import lang.ast;

import std.stdio;

/*  <module> ::= <declaration>*
 *  <declaration> ::= <func_declaration>
 *  <func_decl> ::= "func" <id> "(" ")" <block_statement>
 *  <block_statement> ::= "{" <statement>* "}"
 *  <statement> ::= "if" <paren_expr> <statement> /
 *                  "if" <paren_expr> <statement> "else" <statement> /
 *                  "while" <paren_expr> <statement> /
 *                  "do" <statement> "while" <paren_expr> ";" /
 *                  "return" <expr>? ";" /
 *                  <block_statement> /
 *                  <expr> ";" /
 *                  ";"
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
	auto stream = CharStream!string(input);
	StringLexer lexer = StringLexer(stream);

	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!'{', TokenType.LCURLY);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!'}', TokenType.RCURLY);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!'(', TokenType.LPAREN);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!')', TokenType.RPAREN);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!'+', TokenType.PLUS);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!'-', TokenType.MINUS);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!'<', TokenType.LT);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!';', TokenType.SEMICOLON);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!'=', TokenType.ASSIGN);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!',', TokenType.COMMA);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchId!"func", TokenType.FUNC_SYM);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchId!"return", TokenType.RET_SYM);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchId!"while", TokenType.WHILE_SYM);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchId!"if", TokenType.IF_SYM);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchId!"else", TokenType.ELSE_SYM);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchId!"do", TokenType.DO_SYM);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchIdent, TokenType.ID);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchDecimalNumber, TokenType.DECIMAL_NUM);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchHexNumber, TokenType.HEX_NUM);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchComment, TokenType.COMMENT);

	Parser parser = Parser(&lexer, &idMap);
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

	void pr_node(AstNode node) // print node
	{
		indent += indentSize;
		// Declarations
		if (auto m = cast(Module)node) { print("MODULE"); foreach(f; m.functions) pr_node(f); }
		else if (auto f = cast(FunctionDeclaration)node) { print("FUNC ", idMap.get(f.id)); pr_node(f.statements); }
		// Statements
		else if (auto b = cast(BlockStatement)node) { print("BLOCK"); foreach(s; b.statements) pr_node(s); }
		else if (auto n = cast(IfStatement)node) { print("IF"); pr_node(n.condition); pr_node(n.thenStatement); }
		else if (auto n = cast(IfElseStatement)node) { print("IF"); pr_node(n.condition); pr_node(n.thenStatement); pr_node(n.elseStatement); }
		else if (auto w = cast(WhileStatement)node) { print("WHILE"); pr_node(w.condition); pr_node(w.statement); }
		else if (auto w = cast(DoWhileStatement)node) { print("DO"); pr_node(w.statement); print("WHILE"); pr_node(w.condition); }
		else if (auto r = cast(ReturnStatement)node) { print("RETURN"); pr_node(r.expression); }
		else if (auto e = cast(ExpressionStatement)node) { print("EXPR"); pr_node(e.expression); }
		// Expressions
		else if (auto v = cast(VariableExpression)node) { print("VAR ", idMap.get(v.id)); }
		else if (auto c = cast(ConstExpression)node) { print("CONST ", c.value); }
		else if (auto b = cast(BinaryExpression)node) { print("BINOP ", b.op); pr_node(b.left); pr_node(b.right);}
		indent -= indentSize;
	}
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

struct Parser
{
	StringLexer* lexer;
	IdentifierMap idMap;

	Token tok() { return lexer.current; }

	T make(T, Args...)(Args args) { return new T(args); }

	void nextToken() {
		do {
			lexer.next();
		}
		while (tok.type == TokenType.COMMENT);
	}

	void syntax_error(Args...)(Args args) {
		throw new ParsingException(tok, args);
	}

	Token expectAndConsume(TokenType type) {
		if (tok.type != type) {
			syntax_error("Expected %s, while got %s", type, tok.type);
		}
		scope(exit) nextToken();
		return tok;
	}


	Module parseModule() { // <module> ::= <declaration>*
		FunctionDeclaration[] functions;
		expectAndConsume(TokenType.SOI);
		while (tok.type != TokenType.EOI)
		{
			functions ~= func_declaration();
		}
		return make!Module(functions);
	}

	FunctionDeclaration func_declaration() // <declaration> ::= <func_declaration>
	{
		expectAndConsume(TokenType.FUNC_SYM); // <func_decl> ::= "func" <id> "(" ")" <compound_statement>
		Token funcName = expectAndConsume(TokenType.ID);
		string name = lexer.getTokenString(funcName);
		auto id = idMap.get(name);
		expectAndConsume(TokenType.LPAREN);
		expectAndConsume(TokenType.RPAREN);
		auto statements = block_stmt();
		return make!FunctionDeclaration(id, statements);
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
		if (cast(VariableExpression)n && tok.type == TokenType.ASSIGN)
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
