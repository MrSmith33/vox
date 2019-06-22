/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.ast.expression;

import all;

NameUseExprNode* cast_expr_name_use(AstNode* t) {
	switch(t.astType) with(AstType) {
		case expr_name_use, expr_var_name_use, expr_func_name_use, expr_member_name_use, expr_type_name_use:
			return cast(NameUseExprNode*)t;
		default: return null;
	}
}

mixin template ExpressionNodeData(AstType _astType, int default_flags = 0) {
	mixin AstNodeData!(_astType, default_flags | AstFlags.isExpression);
	TypeNode* type;
	// can be stack slot, global, variable, virtualRegister, constant
	IrIndex irValue;

	AstNode* as_base() { return cast(AstNode*)&this; }
	NameUseExprNode* as_name_use() { return cast_expr_name_use(cast(AstNode*)&this); }
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
	union
	{
		private AstNode* _entity; // used when resolved, node contains Identifier internally
		private Identifier _id; // used when not yet resolved
	}

	this(TokenIndex loc, Identifier id, TypeNode* type = null, IrIndex irValue = IrIndex.init)
	{
		this.loc = loc;
		this.astType = AstType.expr_name_use;
		this.flags = AstFlags.isExpression;
		this._id = id;
		this.type = type;
		this.irValue = irValue;
	}

	void resolve(AstNode* n) {
		assert(n);
		_entity = n;
		flags |= AstFlags.isSymResolved;
	}
	AstNode* entity() { return isSymResolved ? _entity : null; }
	Identifier id() { return isSymResolved ? _entity.get_node_id : _id; }

	T* get(T, AstType _astType)() {
		assert(isSymResolved);
		assert(_entity.astType == _astType, format("%s used on %s", _astType, _entity.astType));
		return cast(T*)_entity;
	}

	alias varDecl = get!(VariableDeclNode, AstType.decl_var);
	alias funcDecl = get!(FunctionDeclNode, AstType.decl_function);
	alias structDecl = get!(StructDeclNode, AstType.decl_struct);
	alias enumDecl = get!(EnumDeclaration, AstType.decl_enum);
	alias enumMember = get!(EnumMemberDecl, AstType.decl_enum_member);
}

struct IntLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_int, AstFlags.isLiteral);
	ulong value;
	bool isNegative() { return cast(bool)(flags & AstFlags.user1); }
	IsSigned isSigned() { return cast(IsSigned)isNegative; }
	void negate(TokenIndex pos, ref CompilationContext context) {
		if (isNegative) {
			value = -(cast(long)value);
			flags &= ~AstFlags.user1;
		} else {
			if (value <= 0x8000_0000_0000_0000) {
				value = -(cast(long)value);
				flags |= AstFlags.user1;
			}
			else {
				context.error(pos, "`-%s` results in signed integer overflow", value);
			}
		}
	}
}

struct NullLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_null, AstFlags.isLiteral);
}

struct BoolLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_bool, AstFlags.isLiteral);
	bool value;
}

struct StringLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_string, AstFlags.isLiteral);
	string value;
	IrIndex irValueLength; // ExpressionNodeData.irValue stores ptr
}

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

	// arithmetic opEquals
	@("=")   ASSIGN,             // =
	@("&=")  BITWISE_AND_ASSIGN, // &=
	@("|=")  BITWISE_OR_ASSIGN,  // |=
	@("%=")  REMAINDER_ASSIGN,   // %=
	@("<<=") SHL_ASSIGN,         // <<=
	@(">>=") SHR_ASSIGN,         // >>=
	@(">>>=") ASHR_ASSIGN,       // >>>=
	@("+")   PTR_PLUS_INT_ASSIGN,// ptr -= / += int
	@("-=")  MINUS_ASSIGN,       // -=
	@("+=")  PLUS_ASSIGN,        // +=
	@("/=")  DIV_ASSIGN,         // /=
	@("*=")  MULT_ASSIGN,        // *=
	@("^=")  XOR_ASSIGN,         // ^=

	// member access
	@(".") DOT,                // .
}
string[] binOpStrings = gatherStrings!BinOp;

string[] gatherStrings(alias _enum)()
{
	string[] res = new string[__traits(allMembers, _enum).length];
	foreach (i, m; __traits(allMembers, _enum))
	{
		res[i] = __traits(getAttributes, __traits(getMember, _enum, m))[0];
	}
	return res;
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
	Array!(ExpressionNode*) args;
}

struct IndexExprNode {
	mixin ExpressionNodeData!(AstType.expr_index);
	ExpressionNode* array;
	ExpressionNode* index;
}
