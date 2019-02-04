import std.array;
import std.stdio;
import std.range;
import std.algorithm;
import std.string;
import std.format;
import std.conv;

/// Port of https://github.com/bourguet/operator_precedence_parsing/blob/master/pratt_tdop_parser.py by Jean-Marc Bourguet
/// to D
void main()
{
	auto strs = [
		"id1 , id2 , id3",
		"id * id . id ( id , id , ) * id * id * id ++ id id",
		"id * id * id * id * id ;",
		"id [ id ] <<= id * id * id * id ;",
		"id1 + id2 * id3 >> id4 && id5",
		"id1 . id2 . id3 ( id4 )",
	];

	Parser parser = cexp_parser();

	foreach(text; strs)
	{
		Token[] toks = text.splitter(' ').map!strToTok.array;
		Node* exp = parser.parse(toks);
		print(exp);
		write(" | ", tokStrings[parser.token.type]);
		foreach(Token t; parser.input)
		{
			if (t.type == Tok.id) write(" ", t.id);
			else write(" ", tokStrings[t.type]);
		}
		writeln;
	}
}

private bool isIdFirst(dchar chr) pure nothrow {
	return
		'a' <= chr && chr <= 'z' ||
		'A' <= chr && chr <= 'Z' ||
		chr == '_';
}

Token strToTok(string str)
{
	ptrdiff_t pos = countUntil(tokStrings, str);
	if (pos == -1 && str[0].isIdFirst) return Token(Tok.id, str);
	assert(pos != -1, str);
	return Token(cast(Tok)pos, str);
}

void print(Node* node)
{
	if (node is null) {
		write("?");
		return;
	}

	final switch (node.astType)
	{
		case AstType.expr_bin_op:
			write("(");
			write(tokStrings[node.token.type], ' ');
			print(node.lhs);
			write(" ");
			print(node.rhs);
			write(")");
			break;
		case AstType.expr_pre_un_op:
			write("(");
			write(tokStrings[node.token.type], ' ');
			print(node.lhs);
			write(")");
			break;
		case AstType.expr_post_un_op:
			write("(post");
			write(tokStrings[node.token.type], ' ');
			print(node.lhs);
			write(")");
			break;
		case AstType.expr_id:
			write(node.token.id);
			break;
		case AstType.expr_call:
			write("(call ");
			print(node.args[0]);
			write(" (");
			foreach(i, arg; node.args[1..$]) {
				if (i > 0) write(" ");
				print(arg);
			}
			write("))");
			break;
	}
}

enum AstType : ubyte {
	expr_bin_op,
	expr_pre_un_op,
	expr_post_un_op,
	expr_id,
	expr_call,
}

struct Token {
	Tok type;
	string id; // when type is IDENTIFIER
}

enum Tok : ubyte {
	@("eoi") eoi,
	@("++") plus_plus,
	@("--") minus_minus,
	@("*") star,
	@("/") div,
	@("+") plus,
	@("-") minus,
	@("^") xor,
	@(".") dot,
	@("~") tilde,
	@("!") bang,
	@("%") percent,
	@("<<") less_less,
	@(">>") more_more,
	@(">>>") more_more_more,
	@("<") less,
	@(">") more,
	@("<=") less_eq,
	@(">=") more_eq,
	@("==") eq_eq,
	@("!=") not_eq,
	@("&") and,
	@("&&") and_and,
	@("|") or,
	@("||") or_or,

	@("=") eq,
	@("+=") plus_eq,
	@("-=") minus_eq,
	@("*=") star_eq,
	@("/=") div_eq,
	@("%=") percent_eq,
	@("<<=") less_less_eq,
	@(">>=") more_more_eq,
	@(">>>=") more_more_more_eq,
	@("&=") and_eq,
	@("|=") or_eq,
	@("^=") xor_eq,

	@("(") lpar,
	@(")") rpar,
	@("]") rbra,
	@("[") lbra,

	@(null) id,
	@("[]") index,
	@("[]") slice,
	@(null) num,
	@(",") comma,
	@(";") semicolon,
	@(":") colon,
	@(null) call,
}

immutable string[] tokStrings = gatherInfos();

string[] gatherInfos()
{
	string[] res = new string[__traits(allMembers, Tok).length];
	foreach (i, m; __traits(allMembers, Tok))
	{
		res[i] = __traits(getAttributes, mixin("Tok."~m))[0];
	}
	return res;
}

enum NFLG : ubyte {
	parenthesis,
}

struct Node {
	this(AstType astType, Token token, Node* lhs = null, Node* rhs = null)
	{
		this.astType = astType;
		this.token = token;
		this.lhs = lhs;
		this.rhs = rhs;
	}
	this(AstType astType, Token token, Node*[] args)
	{
		this.astType = astType;
		this.token = token;
		this.args = args;
	}
	AstType astType;
	ubyte flags;
	Token token; // for binary and unary op node
	union
	{
		struct
		{
			Node* lhs, rhs;
		}
		Node*[] args;
	}
}

/// min and max binding powers
enum MIN_BP = 0;
enum MAX_BP = 10000;
enum COMMA_PREC = 10;

alias LeftParser = Node* function(ref Parser p, Token token, int rbp, Node* left);
alias NullParser = Node* function(ref Parser p, Token token, int rbp);

Node* left_error_parser(ref Parser p, Token token, int rbp, Node* left)
{
	throw new Exception(format("%s can't be used in infix position", token));
}

Node* null_error_parser(ref Parser p, Token token, int rbp)
{
	throw new Exception(format("%s can't be used in prefix position", token));
}

struct LeftInfo
{
	LeftParser parser_left = &left_error_parser;
	int lbp = MIN_BP;
	int rbp = MIN_BP;
	int nbp = MIN_BP;
}

struct NullInfo
{
	NullParser parser_null = &null_error_parser;
	int lbp = MIN_BP;
	int rbp = MIN_BP;
	int nbp = MIN_BP;
}

/// Recursive TDOP parser
struct Parser
{
	Token[] input;
	Token token;
	LeftInfo[Tok.max+1] left_lookup;
	NullInfo[Tok.max+1] null_lookup;

	void expect(Tok expected) {
		if (token.type != expected) {
			throw new Exception(format("Expected `%s` token, while got `%s`", expected, token.type));
		}
	}

	/// Move to the next token
	void nextToken() {
		if (input.empty) {
			token = Token(Tok.eoi);
			return;
		}
		token = input.front;
		input.popFront;
	}

	/// Assert the value of the current token, then move to the next token
	void expectAndConsume(Tok expected) {
		expect(expected);
		nextToken;
	}

	// Examples:
	// If we see 1*2+ , rbp = 27 and lbp = 25, so stop.
	// If we see 1+2+ , rbp = 25 and lbp = 25, so stop.
	// If we see 1**2**, rbp = 26 and lbp = 27, so keep going.
	/// Parse to the right, eating tokens until we encounter a token with binding power LESS THAN OR EQUAL TO rbp
	Node* ParseUntil(int rbp)
	{
		if (token.type == Tok.eoi) throw new Exception("Unexpected end of input");
		Token t = token;
		nextToken;

		NullInfo null_info = null_lookup[t.type];
		Node* node = null_info.parser_null(this, t, null_info.rbp);
		int nbp = null_info.nbp; // next bp
		int lbp = left_lookup[token.type].lbp;

		while (rbp < lbp && lbp < nbp)
		{
			t = token;
			nextToken;
			LeftInfo left_info = left_lookup[t.type];
			node = left_info.parser_left(this, t, left_info.rbp, node);
			nbp = left_info.nbp; // next bp
			lbp = left_lookup[token.type].lbp;
		}

		return node;
	}

	Node* parse(Token[] input)
	{
		this.input = input;
		nextToken;
		Node* r = ParseUntil(0);
		return r;
	}
}

//
// Null Denotations -- tokens that take nothing on the left
//

// Name or number
Node* nullLiteral(ref Parser p, Token token, int rbp) {
	return new Node(AstType.expr_id, token);
}

// Arithmetic grouping
Node* nullParen(ref Parser p, Token token, int rbp) {
	Node* r = p.ParseUntil(rbp);
	p.expectAndConsume(Tok.rpar);
	r.flags |= NFLG.parenthesis;
	return r;
}

Node* nullPrefixOp(ref Parser p, Token token, int rbp) {
	// Prefix operator
	// Low precedence:  return, raise, etc.
	//   return x+y is return (x+y), not (return x) + y
	// High precedence: logical negation, bitwise complement, etc.
	//   !x && y is (!x) && y, not !(x && y)

	Node* right = p.ParseUntil(rbp);
	return new Node(AstType.expr_pre_un_op, token, right);
}

// ++x or ++x[1]
Node* nullIncDec(ref Parser p, Token token, int rbp) {
	Node* right = p.ParseUntil(rbp);
	//if (right.token not in ('ID', 'get') and (
	//		right.token is Token and right.token not in ('ID', 'get')))
	//	raise ParseError("Can't assign to %r (%s)" % (right, right.token))
	return new Node(AstType.expr_pre_un_op, token, right);
}

//
// Left Denotations -- tokens that take an expression on the left
//

// i++ and i--
Node* leftIncDec(ref Parser p, Token token, int rbp, Node* left) {
	// if left.token not in ('ID', 'get'):
	//  raise tdop.ParseError("Can't assign to %r (%s)" % (left, left.token))
	return new Node(AstType.expr_post_un_op, token, left);
}

// index f[x+1] or f[x][y]
Node* leftIndex(ref Parser p, Token token, int rbp, Node* left) {
	Node* index = p.ParseUntil(0);
	p.expectAndConsume(Tok.rbra);
	token = Token(Tok.index);
	return new Node(AstType.expr_bin_op, token, left, index);
}

/*
// e.g. a > 1 ? x : y
Node* leftTernaryOp(ref Parser p, Token token, int rbp, Node* left) {
	// 0 binding power since any operators allowed until ':'.  See:
	//
	// http://en.cppreference.com/w/c/language/operator_precedence#cite_note-2
	//
	// "The expression in the middle of the conditional operator (between ?  and
	// :) is parsed as if parenthesized: its precedence relative to ?: is
	// ignored."
	true_expr = p.ParseUntil(0)
	p.Eat(':')
	false_expr = p.ParseUntil(rbp)
	children = [left, true_expr, false_expr]
	return CompositeNode(token, children)
}*/

// Normal binary operator like 1+2 or 2*3, etc
Node* leftBinaryOp(ref Parser p, Token token, int rbp, Node* left) {
	return new Node(AstType.expr_bin_op, token, left, p.ParseUntil(rbp));
}

// Binary assignment operator like x += 1, or a[i] += 1
Node* leftAssignOp(ref Parser p, Token token, int rbp, Node* left) {
	return new Node(AstType.expr_bin_op, token, left, p.ParseUntil(rbp));
}

/*
// foo, bar, baz - Could be sequencing operator, or tuple without parens
Node* leftComma(ref Parser p, Token token, int rbp, Node* left) {
	r = p.ParseUntil(rbp)
	if not left.parenthesis and left.token == ',':  // Keep adding more children
		left.children.append(r)
		return left
	children = [left, r]
	return new Node(AstType.expr_bin_op, token, children)
}*/

Node* leftFuncCall(ref Parser p, Token token, int unused_rbp, Node* left) {
    // Function call f(a, b)
    Node*[] children = [left];
    // f(x) or f[i](x)
    while (p.token.type != Tok.rpar) {
        // We don't want to grab the comma, e.g. it is NOT a sequence operator.
        children ~= p.ParseUntil(COMMA_PREC);
        if (p.token.type == Tok.comma)
            p.nextToken;
    }
    p.expectAndConsume(Tok.rpar);
    token = Token(Tok.call);
    return new Node(AstType.expr_call, token, children);
}

Parser cexp_parser()
{
	auto parser = Parser();

	void _RegisterNull(int lbp, int rbp, int nbp, NullParser p, string[] tokens...) {
		foreach (string token; tokens) parser.null_lookup[strToTok(token).type] = NullInfo(p, lbp, rbp, nbp);
	}

	void _RegisterLeft(int lbp, int rbp, int nbp, LeftParser p, string[] tokens...) {
		foreach (string token; tokens) parser.left_lookup[strToTok(token).type] = LeftInfo(p, lbp, rbp, nbp);
	}

	void nilfix(int bp, NullParser nud, string[] tokens...) {
		_RegisterNull(MIN_BP, MIN_BP, MAX_BP, nud, tokens);
	}

	void prefix(int bp, NullParser nud, string[] tokens...) {
		_RegisterNull(MIN_BP, bp, MAX_BP, nud, tokens);
	}

	void suffix(int bp, LeftParser led, string[] tokens...) {
		_RegisterLeft(bp, MIN_BP, MAX_BP, led, tokens);
	}

	void infixL(int bp, LeftParser led, string[] tokens...) {
		_RegisterLeft(bp, bp, bp + 1, led, tokens);
	}

	void infixR(int bp, LeftParser led, string[] tokens...) {
		_RegisterLeft(bp, bp - 1, bp + 1, led, tokens);
	}

	void infixN(int bp, LeftParser led, string[] tokens...) {
		_RegisterLeft(bp, bp, bp, led, tokens);
	}

	// Compare the code below with this table of C operator precedence:
	// http://en.cppreference.com/w/c/language/operator_precedence

	suffix(310, &leftIncDec, ["++", "--"]);
	infixL(310, &leftFuncCall, "(");
	infixL(310, &leftIndex, "[");
	infixL(310, &leftBinaryOp, ".");
	//infixL(310, &leftBinaryOp, "->");

	// 29 -- binds to everything except function call, indexing, postfix ops
	prefix(290, &nullIncDec, ["++", "--"]);
	prefix(290, &nullPrefixOp, ["+", "-", "!", "~", "*", "&"]);

	// Right associative: 2 ** 3 ** 2 == 2 ** (3 ** 2)
	//infixR(270, &leftBinaryOp, "**");

	infixL(250, &leftBinaryOp, ["*", "/", "%"]);

	infixL(230, &leftBinaryOp, ["+", "-"]);
	infixL(210, &leftBinaryOp, ["<<", ">>"]);
	infixL(190, &leftBinaryOp, ["<", ">", "<=", ">="]);
	infixL(170, &leftBinaryOp, ["!=", "=="]);

	infixL(150, &leftBinaryOp, "&");
	infixL(130, &leftBinaryOp, "^");
	infixL(110, &leftBinaryOp, "|");
	infixL(90, &leftBinaryOp, "&&");
	infixL(70, &leftBinaryOp, "||");

	//infixR(50, &leftTernaryOp, "?");

	// Right associative: a = b = 2 is a = (b = 2)
	infixR(
		30, &leftAssignOp, [
			"=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "^=", "|="]);

	//infixL(COMMA_PREC, leftComma, ",");

	// 0 precedence -- doesn"t bind until )
	prefix(0, &nullParen, "("); // for grouping

	// 0 precedence -- never used
	nilfix(0, &nullLiteral, ["id"]);
	nilfix(0, &null_error_parser, [")", "]", ":", "eoi", ";"]);
	return parser;
}
