/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.ast.expression;

import all;

mixin template ExpressionNodeData(AstType _astType, int default_flags = 0) {
	mixin AstNodeData!(_astType, default_flags | AstFlags.isExpression);
	TypeNode* type;
	// can be stack slot, global, variable, virtualRegister, constant
	IrIndex irValue;
}

// Abstract node, must not be instantiated
struct ExpressionNode {
	mixin ExpressionNodeData!(AstType.abstract_node);

	StringLiteralExprNode* isStringLiteral() {
		if (astType == AstType.literal_string) return cast(StringLiteralExprNode*)&this;
		return null;
	}
}

struct NameUseExprNode {
	mixin ExpressionNodeData!(AstType.expr_name_use);
	mixin SymRefNodeData;
}

struct IntLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_int, AstFlags.isLiteral);
	long value;
}

struct StringLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_string, AstFlags.isLiteral);
	string value;
	IrIndex irValueLength; // irValue stores ptr
}

enum BinOp : ubyte {
	// logic ops
	LOGIC_AND,          // &&
	LOGIC_OR,           // ||

	// comparisons are converted into IrBinaryCondition, order is important
	EQUAL,              // ==
	NOT_EQUAL,          // !=
	GREATER,            // >
	GREATER_EQUAL,      // >=
	LESS,               // <
	LESS_EQUAL,         // <=

	// arithmetic ops
	BITWISE_AND,        // &
	BITWISE_OR,         // |
	REMAINDER,          // %
	SHL,                // <<
	SHR,                // >>
	ASHR,               // >>>
	MINUS,              // -
	PLUS,               // +
	DIV,                // /
	MULT,               // *
	XOR,                // ^

	// arithmetic opEquals
	ASSIGN,             // =
	BITWISE_AND_ASSIGN, // &=
	BITWISE_OR_ASSIGN,  // |=
	REMAINDER_ASSIGN,   // %=
	SHL_ASSIGN,         // <<=
	SHR_ASSIGN,         // >>=
	ASHR_ASSIGN,        // >>>=
	MINUS_ASSIGN,       // -=
	PLUS_ASSIGN,        // +=
	DIV_ASSIGN,         // /=
	MULT_ASSIGN,        // *=
	XOR_ASSIGN,         // ^=

	// member access
	DOT,                // .
}

enum BinOp BIN_OP_LOGIC_FIRST = BinOp.EQUAL;
enum BinOp BIN_OP_LOGIC_LAST = BinOp.LESS_EQUAL;
enum BinOp BIN_OP_ARITH_FIRST = BinOp.MINUS;
enum BinOp BIN_OP_ARITH_LAST = BinOp.MULT;
//enum BinOp BIN_OP_ARITH_EQUALS_FIRST = BinOp.AND_EQUAL;
//enum BinOp BIN_OP_ARITH_EQUALS_LAST = BinOp.XOR_EQUAL;

struct BinaryExprNode {
	mixin ExpressionNodeData!(AstType.expr_bin_op);
	BinOp op;
	ExpressionNode* left;
	ExpressionNode* right;
}

enum UnOp : ubyte {
	plus, // +
	minus, // -
	logicalNot, // !
	bitwiseNot, // ~
	deref, // *
	addrOf, // &
	preIncrement, // ++x
	preDecrement, // --x
	postIncrement, // x++
	postDecrement, // x--
}

struct UnaryExprNode {
	mixin ExpressionNodeData!(AstType.expr_un_op);
	UnOp op;
	ExpressionNode* child;
}

// member access of aggregate.member form
struct MemberExprNode {
	mixin ExpressionNodeData!(AstType.expr_member);
	ExpressionNode* aggregate;
	NameUseExprNode* member; // member name
	uint memberIndex; // resolved index of member being accessed
}

struct TypeConvExprNode {
	mixin ExpressionNodeData!(AstType.expr_type_conv);
	ExpressionNode* expr;
}

struct CallExprNode {
	mixin ExpressionNodeData!(AstType.expr_call);
	ExpressionNode* callee;
	ExpressionNode*[] args;
}

struct IndexExprNode {
	mixin ExpressionNodeData!(AstType.expr_index);
	ExpressionNode* array;
	ExpressionNode* index;
}
