/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lang.parse;

import lang.lex;
import lang.ast.ast;
import lang.error;
import lang.identifier;

import std.stdio;

/*  <module> ::= <declaration>*
 *  <declaration> ::= <func_declaration>
 *  <func_declaration> ::= "func" <id> "(" <id_list> ")" <block_statement>
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
 *  <expr_list> ::= (<expr> ",")*
 *  <paren_expr> ::= "(" <expr> ")"
 *  <expr> ::= <test> | <id> "=" <expr>
 *  <test> ::= <sum> | <sum> ("=="|"!="|"<"|">"|"<="|"<=") <sum>
 *  <sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term>
 *  <term> ::= <id> | <id> "(" <expr_list> ")" | <int> | <paren_expr>
 *  <id> ::= [a-zA-Z_] (a-zA-Z_0-9)*
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
		override void visit(Module m) {
			print("MODULE");
			foreach(f; m.functions) pr_node(f);
			foreach(v; m.variables) pr_node(v);
			foreach(s; m.structs)   pr_node(s);
		}
		override void visit(FunctionDeclaration f) {
			print("FUNC ", idMap.get(f.id));
			indent += indentSize;
			foreach(p; f.parameters)
				print("PARAM ", basicTypeNames[p.type.basicType], " ", idMap.get(p.id));
			indent -= indentSize;
			pr_node(f.statements);
		}
		override void visit(VarDeclaration v) {
			print("VAR ", basicTypeNames[v.type.basicType], " ", idMap.get(v.id));
		}
		override void visit(StructDeclaration s) {
			print("STRUCT ", idMap.get(s.id));
		}
		override void visit(BlockStatement b) { print("BLOCK"); foreach(s; b.statements) pr_node(s); }
		override void visit(IfStatement n) { print("IF"); pr_node(n.condition); pr_node(n.thenStatement); }
		override void visit(IfElseStatement n) { print("IF"); pr_node(n.condition); pr_node(n.thenStatement); pr_node(n.elseStatement); }
		override void visit(WhileStatement w) { print("WHILE"); pr_node(w.condition); pr_node(w.statement); }
		override void visit(DoWhileStatement w) { print("DO"); pr_node(w.condition); pr_node(w.statement); }
		override void visit(ReturnStatement r) { print("RETURN"); if (r.expression) pr_node(r.expression); }
		override void visit(ExpressionStatement e) { print("EXPR"); pr_node(e.expression); }
		override void visit(DeclarationStatement d) { pr_node(d.declaration); }
		override void visit(VariableExpression v) { print("VAR_USE ", idMap.get(v.id)); }
		override void visit(ConstExpression c) { print("CONST ", c.value); }
		override void visit(BinaryExpression b) { print("BINOP ", b.op); pr_node(b.left); pr_node(b.right); }
		override void visit(CallExpression c) { print("CALL ", idMap.get(c.calleeId)); foreach(a; c.args) pr_node(a); }
	};

	pr_node(n);
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
			T t = emplace!T(pc[0..size], tok.loc, args);
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

	T enforceNode(T)(T t)
	{
		if (t is null)
		{
			string tokenString = lexer.getTokenString(tok);
			throw syntax_error(tok.loc, "Expected %s while got %s token '%s'", T.stringof, tok.type, tokenString);
		}
		return t;
	}

	Token expectAndConsume(TokenType type) {
		if (tok.type != type) {
			string tokenString = lexer.getTokenString(tok);
			throw syntax_error(tok.loc, "Expected %s token, while got %s token '%s'", type, tok.type, tokenString);
		}
		scope(exit) nextToken();
		return tok;
	}

	Identifier expectIdentifier()
	{
		Token nameTok = expectAndConsume(TokenType.ID);
		string name = lexer.getTokenString(nameTok);
		return idMap.getOrReg(name);
	}

	Module parseModule() { // <module> ::= <declaration>*
		FunctionDeclaration[] functions;
		VarDeclaration[] vars;
		StructDeclaration[] structs;
		nextToken();
		while (tok.type != TokenType.EOI)
		{
			auto decl = enforceNode(parse_declaration);
			if (auto funcDecl = cast(FunctionDeclaration)decl)
				functions ~= funcDecl;
			else if (auto varDecl = cast(VarDeclaration)decl)
				vars ~= varDecl;
			else if (auto structDecl = cast(StructDeclaration)decl)
				structs ~= structDecl;
		}
		return make!Module(functions, vars, structs);
	}

	/// Can return null
	Declaration parse_declaration() // <declaration> ::= <func_declaration> / <var_declaration> / <struct_declaration>
	{
		if (tok.type == TokenType.STRUCT_SYM) // <struct_declaration> ::= "struct" <id> "{" <declaration>* "}"
		{
			FunctionDeclaration[] functions;
			VarDeclaration[] vars;
			StructDeclaration[] structs;
			nextToken();
			Identifier structId = expectIdentifier();
			expectAndConsume(TokenType.LCURLY);
			expectAndConsume(TokenType.RCURLY);
			return make!StructDeclaration(functions, vars, structs, structId);
		}
		else // <func_declaration> / <var_declaration>
		{
			TypeAstNode type = parse_type();
			if (type is null) return null;
			Identifier declarationId = expectIdentifier();

			if (tok.type == TokenType.SEMICOLON) // <var_declaration> ::= <type> <id> ";"
			{
				nextToken();
				return make!VarDeclaration(declarationId, type);
			}
			else if (tok.type == TokenType.LPAREN) // <func_declaration> ::= <type> <id> "(" <param_list> ")" <block_statement>
			{
				expectAndConsume(TokenType.LPAREN);
				ParameterAstNode[] params;
				while (tok.type != TokenType.RPAREN)
				{
					TypeAstNode paramType = parse_type_excpected();
					Identifier paramId = expectIdentifier();
					params ~= make!ParameterAstNode(paramId, paramType);
					if (tok.type == TokenType.COMMA) nextToken();
					else break;
				}
				expectAndConsume(TokenType.RPAREN);
				BlockStatement statements = block_stmt();
				return make!FunctionDeclaration(declarationId, type, params, statements);
			}
			else
			{
				throw syntax_error(tok.loc, "Expected '(' or ';', while got '%s'", lexer.getTokenString(tok));
			}
		}
	}

	/// Can return null
	TypeAstNode parse_type_excpected()
	{
		auto type = parse_type();
		if (type is null) throw syntax_error(tok.loc, "Expected basic type, while got '%s'", lexer.getTokenString(tok));
		return type;
	}

	TypeAstNode parse_type()
	{
		if (!isBasicTypeToken(tok.type)) return null;
		return make!TypeAstNode(parse_basic_type());
	}

	BasicType parse_basic_type()
	{
		if (isBasicTypeToken(tok.type))
		{
			auto res = tokenTypeToBasicType(tok.type);
			nextToken();
			return res;
		}
		throw syntax_error(tok.loc, "Expected basic type, while got '%s'", lexer.getTokenString(tok));
	}

	BlockStatement block_stmt() // <block_statement> ::= "{" <statement>* "}"
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
			{
				Declaration decl = parse_declaration;
				if (decl) return make!DeclarationStatement(decl);

				Expression expression = expr();
				expectAndConsume(TokenType.SEMICOLON);
				return make!ExpressionStatement(expression);
			}
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

	Expression term() /* <term> ::= <id> | <id> "(" <expr_list> ")" | <int> | <paren_expr> */
	{
		if (tok.type == TokenType.ID)
		{
			string name = lexer.getTokenString(tok);
			Identifier id = idMap.getOrReg(name);
			nextToken();
			if (tok.type == TokenType.LPAREN) // <term> ::= <id> "(" <expr_list> ")"
			{
				expectAndConsume(TokenType.LPAREN);
				Expression[] args = expr_list();
				expectAndConsume(TokenType.RPAREN);
				return make!CallExpression(id, args);
			}
			return make!VariableExpression(id);
		}
		else if (tok.type == TokenType.DECIMAL_NUM)
		{
			long value = lexer.getTokenNumber();
			nextToken();
			return make!ConstExpression(value);
		}
		else return paren_expr();
	}

	Expression[] expr_list() // <expr_list> ::= (<expr> ",")*
	{
		Expression[] expressions;
		while (tok.type != TokenType.RPAREN)
		{
			expressions ~= expr();
			if (tok.type == TokenType.COMMA) nextToken();
			else break;
		}
		return expressions;
	}
}
