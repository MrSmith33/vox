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

void type_check_binary_op(BinaryExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	require_type_check(node.left, state);
	require_type_check(node.right, state);
	assert(node.left.expr_type(c), format("left(%s).type: is null", node.left.astType(c)));
	assert(node.right.expr_type(c), format("right(%s).type: is null", node.right.astType(c)));

	setResultType(node, c);
	node.state = AstNodeState.type_check_done;
}

void setResultType(BinaryExprNode* b, CompilationContext* context)
{
	AstIndex resRype = context.basicTypeNodes(BasicType.t_error);
	AstIndex leftTypeIndex = b.left.get_expr(context).type;
	TypeNode* leftType = leftTypeIndex.get_type(context);
	AstIndex rightTypeIndex = b.right.get_expr(context).type;
	TypeNode* rightType = rightTypeIndex.get_type(context);

	if (leftType.isError || rightType.isError) return;

	switch(b.op) with(BinOp)
	{
		// logic ops. Requires both operands to be bool
		case LOGIC_AND, LOGIC_OR:
			autoconvToBool(b.left, context);
			autoconvToBool(b.right, context);
			resRype = context.basicTypeNodes(BasicType.t_bool);
			break;
		// logic ops. Requires both operands to be of the same type
		case EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL:
			if (leftType.isPointer && rightType.isPointer)
			{
				if (
					same_type(leftTypeIndex, rightTypeIndex, context) ||
					leftType.as_ptr.isVoidPtr(context) ||
					rightType.as_ptr.isVoidPtr(context))
				{
					resRype = context.basicTypeNodes(BasicType.t_bool);
					break;
				}
			}

			if (autoconvToCommonType(b.left, b.right, context))
				resRype = context.basicTypeNodes(BasicType.t_bool);
			else
				context.error(b.left.get_node(context).loc, "Cannot compare `%s` and `%s`",
					leftType.typeName(context),
					rightType.typeName(context));
			break;

		case MINUS:
			if (leftType.isPointer && rightType.isPointer) // handle ptr - ptr
			{
				if (same_type(leftTypeIndex, rightTypeIndex, context))
				{
					b.op = BinOp.PTR_DIFF;
					resRype = context.basicTypeNodes(BasicType.t_i64);
					break;
				}
				else
				{
					context.error(b.loc, "cannot subtract pointers to different types: `%s` and `%s`",
						leftType.printer(context), rightType.printer(context));
					break;
				}
			} else if (leftType.isPointer && rightType.isInteger) { // handle ptr - int
				b.op = BinOp.PTR_PLUS_INT;
				(cast(IntLiteralExprNode*)b.right.get_node(context)).negate(b.loc, *context);
				resRype = leftTypeIndex;
				break;
			}
			goto case DIV;

		case PLUS:
			// handle int + ptr and ptr + int
			if (leftType.isPointer && rightType.isInteger) {
				b.op = BinOp.PTR_PLUS_INT;
				resRype = leftTypeIndex;
				break;
			} else if (leftType.isInteger && rightType.isPointer) {
				b.op = BinOp.PTR_PLUS_INT;
				// canonicalize
				swap(b.left, b.right);
				resRype = leftTypeIndex;
				break;
			}

			goto case DIV;

		// arithmetic op int float
		case DIV, REMAINDER, MULT, SHL, SHR, ASHR, BITWISE_AND, BITWISE_OR, XOR:
			if (autoconvToCommonType(b.left, b.right, context))
			{
				resRype = b.left.get_node_type(context);
			}
			else
			{
				context.error(b.loc, "Cannot perform `%s` %s `%s` operation",
					leftType.typeName(context), binOpStrings[b.op],
					rightType.typeName(context));
			}
			break;

		case MINUS_ASSIGN:
			if (leftType.isPointer && rightType.isInteger) {
				b.op = BinOp.PTR_PLUS_INT_ASSIGN;
				(cast(IntLiteralExprNode*)b.right.get_node(context)).negate(b.loc, *context);
				resRype = leftTypeIndex;
				break;
			}
			goto case BITWISE_AND_ASSIGN;

		case PLUS_ASSIGN:
			if (leftType.isPointer && rightType.isInteger) {
				b.op = BinOp.PTR_PLUS_INT_ASSIGN;
				resRype = leftTypeIndex;
				break;
			}
			goto case BITWISE_AND_ASSIGN;

		case BITWISE_AND_ASSIGN, BITWISE_OR_ASSIGN, REMAINDER_ASSIGN, SHL_ASSIGN, SHR_ASSIGN,
			ASHR_ASSIGN, DIV_ASSIGN, MULT_ASSIGN, XOR_ASSIGN, ASSIGN:
			bool success = autoconvTo(b.right, leftTypeIndex, context);
			if (!success)
				context.error(b.loc, "Cannot perform `%s` %s `%s` operation",
					leftType.typeName(context), binOpStrings[b.op],
					rightType.typeName(context));
			break;

		default:
			context.internal_error(b.loc, "Unimplemented op %s", b.op);
			assert(false);
	}
	b.type = resRype;
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
