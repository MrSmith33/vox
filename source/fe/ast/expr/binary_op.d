/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.binary_op;

import all;


struct BinaryExprNode {
	mixin ExpressionNodeData!(AstType.expr_bin_op, 0, AstNodeState.name_register_done);
	BinOp op;
	AstIndex left;
	AstIndex right;
}

void name_resolve_binary_op(BinaryExprNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.left, state);
	require_name_resolve(node.right, state);
	node.state = AstNodeState.name_resolve_done;
}

BinOp binOpAssignToRegularOp(BinOp op) {
	switch(op) with(BinOp) {
		case BITWISE_AND_ASSIGN: return BITWISE_AND;
		case BITWISE_OR_ASSIGN: return BITWISE_OR;
		case REMAINDER_ASSIGN: return REMAINDER;
		case SHL_ASSIGN: return SHL;
		case SHR_ASSIGN: return SHR;
		case ASHR_ASSIGN: return ASHR;
		case MINUS_ASSIGN: return MINUS;
		case PLUS_ASSIGN: return PLUS;
		case DIV_ASSIGN: return DIV;
		case MULT_ASSIGN: return MULT;
		case XOR_ASSIGN: return XOR;
		default: assert(false);
	}
}


///
string[] binOpStrings = gatherStrings!BinOp;

enum BinOp BIN_OP_LOGIC_FIRST = BinOp.EQUAL;
enum BinOp BIN_OP_LOGIC_LAST = BinOp.LESS_EQUAL;
enum BinOp BIN_OP_ARITH_FIRST = BinOp.MINUS;
enum BinOp BIN_OP_ARITH_LAST = BinOp.MULT;

enum BinOp : ubyte {
	// logic ops
	@("&&") LOGIC_AND,          // &&
	@("||") LOGIC_OR,           // ||

	// comparisons are converted into IrBinaryCondition, order is important
	@("==") EQUAL,              // ==
	@("!=") NOT_EQUAL,          // !=
	@(">")  GREATER,            // >
	@(">=") GREATER_EQUAL,      // >=
	@("<")  LESS,               // <
	@("<=") LESS_EQUAL,         // <=

	// arithmetic ops
	@("&") BITWISE_AND,        // &
	@("|") BITWISE_OR,         // |
	@("%") REMAINDER,          // %
	@("<<") SHL,               // <<
	@(">>") SHR,               // >>
	@(">>>") ASHR,             // >>>
	@("-") MINUS,              // -
	@("+") PLUS,               // +
	@("/") DIV,                // /
	@("*") MULT,               // *
	@("^") XOR,                // ^

	@("-") PTR_DIFF,           // ptr - ptr
	@("+") PTR_PLUS_INT,       // ptr + int and ptr - int

	@("=")   ASSIGN,             // =

	// arithmetic opOpAssing, same order as above (to convert)
	@("&=")  BITWISE_AND_ASSIGN, // &=
	@("|=")  BITWISE_OR_ASSIGN,  // |=
	@("%=")  REMAINDER_ASSIGN,   // %=
	@("<<=") SHL_ASSIGN,         // <<=
	@(">>=") SHR_ASSIGN,         // >>=
	@(">>>=") ASHR_ASSIGN,       // >>>=
	@("-=")  MINUS_ASSIGN,       // -=
	@("+=")  PLUS_ASSIGN,        // +=
	@("/=")  DIV_ASSIGN,         // /=
	@("*=")  MULT_ASSIGN,        // *=
	@("^=")  XOR_ASSIGN,         // ^=

	@("+")   PTR_PLUS_INT_ASSIGN,// ptr -= / += int

	// member access
	@(".") DOT,                // .
}

private string[] gatherStrings(alias _enum)()
{
	string[] res = new string[__traits(allMembers, _enum).length];
	foreach (i, m; __traits(allMembers, _enum))
	{
		res[i] = __traits(getAttributes, __traits(getMember, _enum, m))[0];
	}
	return res;
}
