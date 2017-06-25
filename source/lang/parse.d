/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
import lex;

import std.stdio;

/*  <module> ::= <declaration>*
 *  <declaration> ::= <func_declaration>
 *  <func_decl> ::= "func" <id> "(" ")" <compound_statement>
 *  <compound_statement> ::= "{" <statement>* "}"
 *  <statement> ::= "if" <paren_expr> <statement> /
 *                  "if" <paren_expr> <statement> "else" <statement> /
 *                  "while" <paren_expr> <statement> /
 *                  "do" <statement> "while" <paren_expr> ";" /
 *                  "return" <expr>? ";" /
 *                  <compound_statement> /
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

string input =
q{
func main(){ return 1; return 2+a; return (result < 3); }
func sub1(){ return 42; }
func sub2(){ if (a<b) c=10; }
func sub3(){ if (a<b) c=10; else c=20; }
func sub4(){ while(i<100){a=a+i;i=i+1;} }
func sub5(){ do{a=a+i;i=i+1;}while(i<100); }
};

void main()
{
	StringMap stringMap;
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

	Parser parser = Parser(&lexer, &stringMap);
	try
	{
		auto root = parser.parseModule();
		printAST(root, &stringMap);
	}
	catch(ParsingException e)
	{
		writefln("%s: [ERROR] %s", e.token.start.pos, e.msg);
		writeln(e);
	}
}

enum NodeT { MODULE, FUNC, VAR, CONST, BINOP, IF1, IF2, WHILE, DO, EMPTY, SEQ, EXPR, RETURN }
enum BinOp { ADD, SUB, MUL, DIV, MOD, SHL, SHR, ASHR, AND, OR, ANDAND, OROR, LT, GT, LE, GE, EQUAL, NOTEQUAL, ASSIGN }

BinOp tokToBinOp(Token token) {
	import std.string : format;
	switch (token.type) {
		case TokenType.PLUS: return BinOp.ADD;
		case TokenType.MINUS: return BinOp.SUB;
		case TokenType.MUL: return BinOp.MUL;
		case TokenType.DIV: return BinOp.DIV;
		case TokenType.MOD: return BinOp.MOD;
		case TokenType.SHL: return BinOp.SHL;
		case TokenType.SHR: return BinOp.SHR;
		case TokenType.ASHR: return BinOp.ASHR;
		case TokenType.AND: return BinOp.AND;
		case TokenType.ANDAND: return BinOp.ANDAND;
		case TokenType.OR: return BinOp.OR;
		case TokenType.OROR: return BinOp.OROR;
		case TokenType.ASSIGN: return BinOp.ASSIGN;
		case TokenType.LT: return BinOp.LT;
		case TokenType.GT: return BinOp.GT;
		case TokenType.LE: return BinOp.LE;
		case TokenType.GE: return BinOp.GE;
		case TokenType.EQUAL: return BinOp.EQUAL;
		case TokenType.NOTEQUAL: return BinOp.NOTEQUAL;
		default: assert(false, format("%s is not a binary op token", token.type));
	}
}

void printAST(Node* n, StringMap* stringMap)
{
	import std.range : repeat;
	if (!n) return;

	void printer(Node* n, uint indent)
	{
		switch (n.type)
		{
			case NodeT.VAR   : writeln(' '.repeat(indent), "VAR ", stringMap.get(n.varData.id));    break;
			case NodeT.BINOP   : writeln(' '.repeat(indent), "BINOP ", n.binExpr.op); printer(n.binExpr.left, indent+2); printer(n.binExpr.right, indent+2); break;
			case NodeT.IF1   : writeln(' '.repeat(indent), "IF1");
				printer(n.ifStmt.paren_expr, indent+2);
				printer(n.ifStmt.then_stmt, indent+2); break;
			case NodeT.IF2   : writeln(' '.repeat(indent), "IF2");
				printer(n.ifStmt.paren_expr, indent+2);
				printer(n.ifStmt.then_stmt, indent+2);
				printer(n.ifStmt.else_stmt, indent+2); break;
			case NodeT.WHILE : writeln(' '.repeat(indent), "WHILE");  printer(n.whileStmt.paren_expr, indent+2); printer(n.whileStmt.body_statement, indent+2); break;
			case NodeT.DO    : writeln(' '.repeat(indent), "DO");     printer(n.doStatement.body_statement, indent+2); printer(n.doStatement.paren_expr, indent+2); break;
			case NodeT.EMPTY : writeln(' '.repeat(indent), "EMPTY");  break;
			case NodeT.MODULE: writeln(' '.repeat(indent), "MODULE"); foreach (funcDef; n.moduleData.functions) { printer(funcDef, indent+2); } break;
			case NodeT.FUNC  : writeln(' '.repeat(indent), "FUNC ", stringMap.get(n.funcData.id)); printer(n.funcData.statement, indent+2); break;
			case NodeT.SEQ   : writeln(' '.repeat(indent), "SEQ");
				Node* seq = n;
				while(true) {
					printer(seq.seqStmt.thisStatement, indent+2);
					if (seq.seqStmt.nextSeq)
						seq = seq.seqStmt.nextSeq;
					else break;
				}
				break;
			case NodeT.RETURN: writeln(' '.repeat(indent), "RETURN"); printer(n.returnData.expression, indent+2); break;
			case NodeT.EXPR  : writeln(' '.repeat(indent), "EXPR");   printer(n.binExpr.left, indent+2); break;
			case NodeT.CONST : writeln(' '.repeat(indent), "CONST ", n.constData.value);  break;
			default: break;
		}
	}
	printer(n, 0);
}

struct Node
{
	NodeT type;
	union
	{
		ModuleData moduleData;
		FuncData funcData;
		SeqStmtData seqStmt;
		IfStmtData ifStmt;
		WhileStmtData whileStmt;
		DoStmtData doStatement;
		RetStmtData returnData;
		BinExprData binExpr;
		VarData varData;
		ConstData constData;
	}
}

alias Identifier = uint;

struct ModuleData {
	Node*[] functions; // FuncData
}
struct FuncData {
	Identifier id;
	Node* statement; // SeqStmtData
}
struct SeqStmtData {
	Node* nextSeq; // Statement*
	Node* thisStatement; // Statement*
}
struct IfStmtData {
	Node* paren_expr;
	Node* then_stmt;
	Node* else_stmt;
}
struct WhileStmtData {
	Node* paren_expr;
	Node* body_statement;
}
struct DoStmtData {
	Node* body_statement;
	Node* paren_expr;
}
struct RetStmtData {
	Node* expression;
}
struct BinExprData {
	BinOp op;
	Node* left;
	Node* right;
}
struct VarData {
	Identifier id;
}
struct ConstData {
	int value;
}

struct StringMap {
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
	StringMap* stringMap;

	Token tok() {
		return lexer.current;
	}

	Node* new_Node(NodeT type, BinExprData data) { auto n = new Node(type); n.binExpr = data; return n; }
	Node* new_Node(NodeT type) { return new Node(type); }

	void nextToken() {
		do {
			lexer.next();
		}
		while (tok.type == TokenType.COMMENT);
	}

	void error(Args...)(Args args) {
		throw new ParsingException(tok, args);
	}

	Token expectAndConsume(TokenType type) {
		if (tok.type != type) {
			error("Expected %s, while got %s", type, tok.type);
		}
		scope(exit) nextToken();
		return tok;
	}

	Node* parseModule() // <module> ::= <declaration>*
	{
		auto n = new_Node(NodeT.MODULE);
		expectAndConsume(TokenType.SOI);
		while (tok.type != TokenType.EOI)
		{
			auto decl = declaration();
			if (decl.type == NodeT.FUNC)
			{
				n.moduleData.functions ~= decl;
			}
		}
		return n;
	}

	Node* declaration() // <declaration> ::= <func_declaration>
	{
		Node* n;

		if (tok.type == TokenType.FUNC_SYM) // <func_decl> ::= "func" <id> "(" ")" <compound_statement>
		{
			n = new_Node(NodeT.FUNC);
			nextToken();
			Token funcName = expectAndConsume(TokenType.ID);
			string name = lexer.getTokenString(funcName);
			n.funcData.id = stringMap.get(name);
			expectAndConsume(TokenType.LPAREN);
			expectAndConsume(TokenType.RPAREN);
			n.funcData.statement = seqStmt();
		}
		else
		{
			error("Expected function, while got %s", tok);
		}

		return n;
	}

	Node* seqStmt() // <compound_statement> ::= "{" <statement>* "}"
	{
		Node* first;
		Node* last;
		expectAndConsume(TokenType.LCURLY);
		while (tok.type != TokenType.RCURLY)
		{
			Node* seq = new_Node(NodeT.SEQ);
			seq.seqStmt.thisStatement = statement();
			if (last) last.seqStmt.nextSeq = seq;
			last = seq;
			if (!first) first = seq;
		}
		expectAndConsume(TokenType.RCURLY);
		if (first) return first;
		else return new_Node(NodeT.EMPTY);
	}

	Node* statement()
	{
		Node* n;

		if (tok.type == TokenType.IF_SYM)  /* "if" <paren_expr> <statement> */
		{
			n = new_Node(NodeT.IF1);
			nextToken();
			n.ifStmt.paren_expr = paren_expr();
			n.ifStmt.then_stmt = statement();
			if (tok.type == TokenType.ELSE_SYM)  /* ... "else" <statement> */
			{
				n.type = NodeT.IF2;
				nextToken();
				n.ifStmt.else_stmt = statement();
			}
		}
		else if (tok.type == TokenType.WHILE_SYM)  /* "while" <paren_expr> <statement> */
		{
			n = new_Node(NodeT.WHILE);
			nextToken();
			n.whileStmt.paren_expr = paren_expr();
			n.whileStmt.body_statement = statement();
		}
		else if (tok.type == TokenType.DO_SYM)  /* "do" <statement> "while" <paren_expr> ";" */
		{
			n = new_Node(NodeT.DO);
			nextToken();
			n.doStatement.body_statement = statement();
			expectAndConsume(TokenType.WHILE_SYM);
			n.doStatement.paren_expr = paren_expr();
			expectAndConsume(TokenType.SEMICOLON);
		}
		else if (tok.type == TokenType.RET_SYM)  /* return <expr> */
		{
			n = new_Node(NodeT.RETURN);
			nextToken();
			if (tok.type != TokenType.SEMICOLON)
				n.returnData.expression = expr();
			expectAndConsume(TokenType.SEMICOLON);
		}
		else if (tok.type == TokenType.SEMICOLON)  /* ";" */
		{
			n = new_Node(NodeT.EMPTY);
			nextToken();
		}
		else if (tok.type == TokenType.LCURLY)  /* "{" { <statement> } "}" */
		{
			return seqStmt();
		}
		else  /* <expr> ";" */
		{
			n = new_Node(NodeT.EXPR);
			n.binExpr.left = expr();
			expectAndConsume(TokenType.SEMICOLON);
		}
		return n;
	}

	Node* term()  /* <term> ::= <id> | <int> | <paren_expr> */
	{
		Node* n;
		if (tok.type == TokenType.ID) {
			n = new_Node(NodeT.VAR);
			string name = lexer.getTokenString(tok);
			n.varData.id = stringMap.get(name);
			nextToken();
		}
		else if (tok.type == TokenType.DECIMAL_NUM) {
			n=new_Node(NodeT.CONST);
			string num = lexer.getTokenString(tok);
			import std.conv : to;
			n.constData.value=to!int(num);
			nextToken();
		}
		else n = paren_expr();
		return n;
	}

	Node* sum()  /* <sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term> */
	{
		Node* t, n = term();
		while (tok.type == TokenType.PLUS || tok.type == TokenType.MINUS)
		{
			t=n; n=new_Node(NodeT.BINOP, BinExprData(tokToBinOp(tok), t)); nextToken(); n.binExpr.right = term();
		}
		return n;
	}

	Node* test()  /* <test> ::= <sum> | <sum> "<" <sum> */
	{
		Node* t, n = sum();
		if (tok.type == TokenType.LT)
		{
			nextToken(); t=n; n=new_Node(NodeT.BINOP, BinExprData(BinOp.LT, t, sum()));
		}
		return n;
	}

	Node* expr()  /* <expr> ::= <test> | <id> "=" <expr> */
	{
		Node* t, n;
		if (tok.type != TokenType.ID) return test();
		n = test();
		if (n.type == NodeT.VAR && tok.type == TokenType.ASSIGN)
		{
			nextToken(); t=n; n=new_Node(NodeT.BINOP, BinExprData(BinOp.ASSIGN, t, expr()));
		}
		return n;
	}

	Node* paren_expr()  /* <paren_expr> ::= "(" <expr> ")" */
	{
		Node* n;
		expectAndConsume(TokenType.LPAREN);
		n = expr();
		expectAndConsume(TokenType.RPAREN);
		return n;
	}
}
