/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.semantics;

import std.stdio;
import std.string : format;
import std.typecons : Flag, Yes, No;
import all;

void pass_type_check(ref CompilationContext ctx, CompilePassPerModule[] subPasses)
{
	auto state = TypeCheckState(&ctx);

	foreach (ref SourceFileInfo file; ctx.files.data) {
		require_type_check(file.mod.as_node, state);

		if (ctx.printAstSema && file.mod !is null) {
			auto astPrinter = AstPrinter(&ctx, 2);
			writefln("// AST typed `%s`", file.name);
			astPrinter.printAst(cast(AstNode*)file.mod);
		}
	}
}

struct TypeCheckState
{
	CompilationContext* context;
	FunctionDeclNode* curFunc;
}

/// Type checking for static context
void require_type_check(AstNode* node, CompilationContext* context)
{
	auto state = TypeCheckState(context);
	require_type_check(node, state);
}

/// Annotates all expression nodes with their type
/// Type checking, casting
void require_type_check(AstNode* node, ref TypeCheckState state)
{
	switch(node.state) with(AstNodeState)
	{
		case name_register, name_resolve, type_check: state.context.unrecoverable_error(node.loc, "Circular dependency"); return;
		case name_resolve_done: break; // all requirement are done
		case type_check_done: return; // already type checked
		default: state.context.internal_error(node.loc, "Node %s in %s state", node.astType, node.state);
	}

	final switch(node.astType) with(AstType)
	{
		case error: state.context.internal_error(node.loc, "Visiting error node"); break;
		case abstract_node: state.context.internal_error(node.loc, "Visiting abstract node"); break;

		case decl_module: type_check_module(cast(ModuleDeclNode*)node, state); break;
		case decl_import: assert(false);
		case decl_function: type_check_func(cast(FunctionDeclNode*)node, state); break;
		case decl_var: type_check_var(cast(VariableDeclNode*)node, state); break;
		case decl_struct: type_check_struct(cast(StructDeclNode*)node, state); break;
		case decl_enum: type_check_enum(cast(EnumDeclaration*)node, state); break;
		case decl_enum_member: type_check_enum_member(cast(EnumMemberDecl*)node, state); break;

		case stmt_block: type_check_block(cast(BlockStmtNode*)node, state); break;
		case stmt_if: type_check_if(cast(IfStmtNode*)node, state); break;
		case stmt_while: type_check_while(cast(WhileStmtNode*)node, state); break;
		case stmt_do_while: type_check_do(cast(DoWhileStmtNode*)node, state); break;
		case stmt_for: type_check_for(cast(ForStmtNode*)node, state); break;
		case stmt_return: type_check_return(cast(ReturnStmtNode*)node, state); break;
		case stmt_break: assert(false);
		case stmt_continue: assert(false);

		case expr_name_use, expr_var_name_use, expr_func_name_use, expr_member_name_use, expr_type_name_use:
			type_check_name_use(cast(NameUseExprNode*)node, state); break;
		case expr_member, expr_struct_member, expr_enum_member, expr_slice_member, expr_static_array_member:
			type_check_member(cast(MemberExprNode*)node, state); break;
		case expr_bin_op: type_check_binary_op(cast(BinaryExprNode*)node, state); break;
		case expr_un_op: type_check_unary_op(cast(UnaryExprNode*)node, state); break;
		case expr_call: type_check_call(cast(CallExprNode*)node, state); break;
		case expr_index: type_check_index(cast(IndexExprNode*)node, state); break;
		case expr_type_conv: type_check_type_conv(cast(TypeConvExprNode*)node, state); break;

		case literal_int: type_check_literal_int(cast(IntLiteralExprNode*)node, state); break;
		case literal_string: type_check_literal_string(cast(StringLiteralExprNode*)node, state); break;
		case literal_null: type_check_literal_null(cast(NullLiteralExprNode*)node, state); break;
		case literal_bool: type_check_literal_bool(cast(BoolLiteralExprNode*)node, state); break;

		case type_basic: assert(false);
		case type_ptr: type_check_ptr(cast(PtrTypeNode*)node, state); break;
		case type_static_array: type_check_static_array(cast(StaticArrayTypeNode*)node, state); break;
		case type_slice: type_check_slice(cast(SliceTypeNode*)node, state); break;
	}
}

bool isBool(TypeNode* type)
{
	return
		type.astType == AstType.type_basic &&
		type.as_basic.basicType == BasicType.t_bool;
}

/// Returns true if types are equal or were converted to common type. False otherwise
bool autoconvToCommonType(ref ExpressionNode* left, ref ExpressionNode* right, CompilationContext* context)
{
	if (left.type.astType == AstType.type_basic && right.type.astType == AstType.type_basic)
	{
		BasicTypeNode* leftType = left.type.as_basic;
		BasicTypeNode* rightType = right.type.as_basic;

		BasicType commonType = commonBasicType[leftType.basicType][rightType.basicType];
		if (commonType != BasicType.t_error)
		{
			TypeNode* type = context.basicTypeNodes(commonType);
			bool successLeft = autoconvTo(left, type, context);
			bool successRight = autoconvTo(right, type, context);
			if(successLeft && successRight)
				return true;
		}
	}
	else if (left.type.isPointer && right.type.isTypeofNull) {
		right.type = left.type;
		return true;
	}
	else if (left.type.isTypeofNull && right.type.isPointer) {
		left.type = right.type;
		return true;
	}
	else
	{
		// error for user-defined types
	}

	return false;
}

void autoconvToBool(ref ExpressionNode* expr, CompilationContext* context)
{
	if (expr.type.isError) return;
	if (!autoconvTo(expr, context.basicTypeNodes(BasicType.t_bool), context))
		context.error(expr.loc, "Cannot implicitly convert `%s` to bool",
			expr.type.typeName(context));
}

bool isConvertibleTo(TypeNode* fromType, TypeNode* toType, CompilationContext* context)
{
	if (same_type(fromType, toType)) return true;

	if (fromType.astType == AstType.type_basic && toType.astType == AstType.type_basic)
	{
		BasicType fromTypeBasic = fromType.as_basic.basicType;
		BasicType toTypeBasic = toType.as_basic.basicType;
		bool isRegisterTypeFrom =
			(fromTypeBasic >= BasicType.t_bool &&
			fromTypeBasic <= BasicType.t_u64);
		bool isRegisterTypeTo =
			(toTypeBasic >= BasicType.t_bool &&
			toTypeBasic <= BasicType.t_u64);
		// all integer types, pointers and bool can be converted between
		// TODO: bool is special (need to have 0 or 1)
		return isRegisterTypeFrom && isRegisterTypeTo;
	}
	if (fromType.astType == AstType.type_ptr && toType.astType == AstType.type_ptr) return true;
	return false;
}

/// Returns true if conversion was successful. False otherwise
bool autoconvTo(ref ExpressionNode* expr, TypeNode* type, CompilationContext* context)
{

	if (same_type(expr.type, type)) return true;
	string extraError;

	if (expr.type.astType == AstType.type_basic && type.astType == AstType.type_basic)
	{
		BasicType fromType = expr.type.as_basic.basicType;
		BasicType toType = type.as_basic.basicType;
		bool canConvert = isAutoConvertibleFromToBasic[fromType][toType];
		if (canConvert)
		{
			if (expr.astType == AstType.literal_int) {
				//writefln("int %s %s -> %s", expr.loc, expr.type.printer(context), type.printer(context));
				expr.type = type;
			} else {
				expr = cast(ExpressionNode*) context.appendAst!TypeConvExprNode(expr.loc, type, IrIndex(), expr);
			}
			return true;
		}
		else if (expr.astType == AstType.literal_int && toType.isInteger) {
			auto lit = cast(IntLiteralExprNode*) expr;
			if (lit.isSigned) {
				if (numSignedBytesForInt(lit.value) <= integerSize(toType)) {
					expr.type = type;
					return true;
				}
			} else {
				if (numUnsignedBytesForInt(lit.value) <= integerSize(toType)) {
					expr.type = type;
					return true;
				}
			}

			context.error(expr.loc, "Cannot auto-convert integer `0x%X` of type %s to `%s`",
				lit.value,
				expr.type.printer(context),
				type.printer(context));
			return false;
		}
	}
	// auto cast from string literal to c_char*
	else if (expr.astType == AstType.literal_string)
	{
		if (type.astType == AstType.type_ptr &&
			type.as_ptr.base.astType == AstType.type_basic &&
			type.as_ptr.base.as_basic.basicType == BasicType.t_u8)
		{
			auto memberExpr = context.appendAst!MemberExprNode(expr.loc, type, IrIndex(), expr, null, 1);
			memberExpr.astType = AstType.expr_slice_member;
			expr = cast(ExpressionNode*)memberExpr;
			return true;
		}
	}
	else if (expr.type.isStaticArray && type.isSlice)
	{
		if (same_type(expr.type.as_static_array.base, type.as_slice.base))
		{
			expr = cast(ExpressionNode*)context.appendAst!UnaryExprNode(
				expr.loc, type, IrIndex(), UnOp.staticArrayToSlice, expr);
			return true;
		}
	}
	else if (expr.astType == AstType.literal_null) {
		if (type.isPointer) {
			expr.type = type;
			return true;
		} else if (type.isSlice) {
			expr.type = type;
			return true;
		}
	}
	else
	{
		extraError = ". Cannot convert from/to user-defined type";
	}

	return false;
}

void setResultType(BinaryExprNode* b, CompilationContext* context)
{
	TypeNode* resRype = context.basicTypeNodes(BasicType.t_error);
	if (b.left.type.isError || b.right.type.isError) return;

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
			if (b.left.type.isPointer && b.right.type.isPointer)
			{
				if (
					same_type(b.left.type, b.right.type) ||
					b.left.type.as_ptr.isVoidPtr ||
					b.right.type.as_ptr.isVoidPtr)
				{
					resRype = context.basicTypeNodes(BasicType.t_bool);
					break;
				}
			}

			if (autoconvToCommonType(b.left, b.right, context))
				resRype = context.basicTypeNodes(BasicType.t_bool);
			else
				context.error(b.left.loc, "Cannot compare `%s` and `%s`",
					b.left.type.typeName(context),
					b.right.type.typeName(context));
			break;

		case MINUS:
			if (b.left.type.isPointer && b.right.type.isPointer) // handle ptr - ptr
			{
				if (same_type(b.left.type, b.right.type))
				{
					b.op = BinOp.PTR_DIFF;
					resRype = context.basicTypeNodes(BasicType.t_i64);
					break;
				}
				else
				{
					context.error(b.loc, "cannot subtract pointers to different types: `%s` and `%s`",
						b.left.type.printer(context), b.right.type.printer(context));
					break;
				}
			} else if (b.left.type.isPointer && b.right.type.isInteger) { // handle ptr - int
				b.op = BinOp.PTR_PLUS_INT;
				(cast(IntLiteralExprNode*)b.right).negate(b.loc, *context);
				resRype = b.left.type;
				break;
			}
			goto case DIV;

		case PLUS:
			// handle int + ptr and ptr + int
			if (b.left.type.isPointer && b.right.type.isInteger) {
				b.op = BinOp.PTR_PLUS_INT;
				resRype = b.left.type;
				break;
			} else if (b.left.type.isInteger && b.right.type.isPointer) {
				b.op = BinOp.PTR_PLUS_INT;
				// canonicalize
				swap(b.left, b.right);
				resRype = b.left.type;
				break;
			}

			goto case DIV;

		// arithmetic op int float
		case DIV, REMAINDER, MULT, SHL, SHR, ASHR, BITWISE_AND, BITWISE_OR, XOR:
			if (autoconvToCommonType(b.left, b.right, context))
				resRype = b.left.type;
			else
			{
				context.error(b.loc, "Cannot perform `%s` %s `%s` operation",
					b.left.type.typeName(context), binOpStrings[b.op],
					b.right.type.typeName(context));
			}
			break;

		case MINUS_ASSIGN:
			if (b.left.type.isPointer && b.right.type.isInteger) {
				b.op = BinOp.PTR_PLUS_INT_ASSIGN;
				(cast(IntLiteralExprNode*)b.right).negate(b.loc, *context);
				resRype = b.left.type;
				break;
			}
			goto case BITWISE_AND_ASSIGN;

		case PLUS_ASSIGN:
			if (b.left.type.isPointer && b.right.type.isInteger) {
				b.op = BinOp.PTR_PLUS_INT_ASSIGN;
				resRype = b.left.type;
				break;
			}
			goto case BITWISE_AND_ASSIGN;

		case BITWISE_AND_ASSIGN, BITWISE_OR_ASSIGN, REMAINDER_ASSIGN, SHL_ASSIGN, SHR_ASSIGN,
			ASHR_ASSIGN, DIV_ASSIGN, MULT_ASSIGN, XOR_ASSIGN, ASSIGN:
			bool success = autoconvTo(b.right, b.left.type, context);
			if (!success)
				context.error(b.loc, "Cannot perform `%s` %s `%s` operation",
					b.left.type.typeName(context), binOpStrings[b.op],
					b.right.type.typeName(context));
			break;

		default:
			context.internal_error(b.loc, "Unimplemented op %s", b.op);
			assert(false);
	}
	b.type = resRype;
}

void calcType(BinaryExprNode* b, CompilationContext* context)
{
	assert(b.left.type, format("left(%s).type: is null", b.left.astType));
	assert(b.right.type, format("right(%s).type: is null", b.right.astType));

	setResultType(b, context);
}


void type_check_module(ModuleDeclNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	foreach (decl; node.declarations) require_type_check(decl, state);
	node.state = AstNodeState.type_check_done;
}

void type_check_func(FunctionDeclNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	auto prevFunc = state.curFunc;
	state.curFunc = node;
	node.backendData.callingConvention = &win64_call_conv;

	if (node.returnType.isOpaqueStruct) {
		state.context.error(node.returnType.loc,
			"function cannot return opaque type `%s`",
			node.returnType.printer(state.context));
	}

	foreach (VariableDeclNode* param; node.parameters) {
		require_type_check(param.as_node, state);
	}

	if (node.block_stmt)
	{
		require_type_check(node.block_stmt.as_node, state);
	}
	state.curFunc = prevFunc;
	node.state = AstNodeState.type_check_done;
}

void type_check_var(VariableDeclNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	TypeNode* type = node.type.foldAliases;
	require_type_check(type.as_node, state);

	if (type.isOpaqueStruct) {
		string msg = node.isParameter ?
			"cannot declare parameter of opaque type `%2$s`" :
			"cannot declare variable `%s` of opaque type `%s`";

		state.context.error(node.type.loc,
			msg,
			state.context.idString(node.id),
			type.printer(state.context));
	}

	if (node.initializer) {
		require_type_check(node.initializer.as_node, state);
		autoconvTo(node.initializer, type, state.context);
	}

	if (!node.isParameter)
	switch (type.astType) with(AstType)
	{
		case type_static_array, decl_struct, type_slice:
			node.varFlags |= VariableFlags.forceMemoryStorage;
			break;

		default: break;
	}
	node.state = AstNodeState.type_check_done;
}

void type_check_struct(StructDeclNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	foreach (decl; node.declarations) require_type_check(decl, state);
	node.state = AstNodeState.type_check_done;
}

void type_check_enum(EnumDeclaration* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	foreach (decl; node.declarations) require_type_check(decl, state);
	node.state = AstNodeState.type_check_done;
}

void type_check_enum_member(EnumMemberDecl* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.type.as_node, state);

	if (node.initializer !is null) {
		require_type_check(node.initializer.as_node, state);
		autoconvTo(node.initializer, node.type, state.context);
	}
	node.state = AstNodeState.type_check_done;
}

void type_check_block(BlockStmtNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	foreach(stmt; node.statements) require_type_check(stmt, state);
	node.state = AstNodeState.type_check_done;
}

void type_check_if(IfStmtNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.condition.as_node, state);
	autoconvToBool(node.condition, state.context);
	require_type_check(node.thenStatement, state);
	if (node.elseStatement) {
		require_type_check(node.elseStatement, state);
	}
	node.state = AstNodeState.type_check_done;
}

void type_check_while(WhileStmtNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.condition.as_node, state);
	autoconvToBool(node.condition, state.context);
	require_type_check(node.statement, state);
	node.state = AstNodeState.type_check_done;
}

void type_check_do(DoWhileStmtNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.statement, state);
	require_type_check(node.condition.as_node, state);
	autoconvToBool(node.condition, state.context);
	node.state = AstNodeState.type_check_done;
}

void type_check_for(ForStmtNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	foreach(stmt; node.init_statements) require_type_check(stmt, state);
	if (node.condition) {
		require_type_check(node.condition.as_node, state);
		autoconvToBool(node.condition, state.context);
	}
	foreach(stmt; node.increment_statements) require_type_check(stmt, state);
	require_type_check(node.statement, state);
	node.state = AstNodeState.type_check_done;
}

// Check return type and function return type
void type_check_return(ReturnStmtNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	if (!state.curFunc)
	{
		state.context.error(node.loc,
			"Return statement is not inside function");
		return;
	}

	if (node.expression)
	{
		require_type_check(node.expression.as_node, state);
		if (state.curFunc.returnType.isVoid)
		{
			state.context.error(node.expression.loc,
				"Cannot return expression of type `%s` from void function",
				node.expression.type.typeName(state.context));
		}
		else
		{
			autoconvTo(node.expression, state.curFunc.returnType, state.context);
		}
	}
	else
	{
		if (!state.curFunc.returnType.isVoid)
			state.context.error(node.loc,
				"Cannot return void from non-void function",
				node.expression.type.typeName(state.context));
	}
	node.state = AstNodeState.type_check_done;
}

// Get type from variable declaration
void type_check_name_use(NameUseExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	node.type = node.entity.get_node_type;
	node.state = AstNodeState.type_check_done;
}

void type_check_member(MemberExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.aggregate.as_node, state);
	lookupMember(node, state.context);
	node.state = AstNodeState.type_check_done;
}

void type_check_literal_int(IntLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	if (node.isSigned)
		node.type = state.context.basicTypeNodes(minSignedIntType(node.value));
	else
		node.type = state.context.basicTypeNodes(minUnsignedIntType(node.value));
	node.state = AstNodeState.type_check_done;
}

void type_check_literal_string(StringLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	// done in parser
	node.state = AstNodeState.type_check_done;
}

void type_check_literal_null(NullLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	node.type = state.context.basicTypeNodes(BasicType.t_null);
	node.state = AstNodeState.type_check_done;
}

void type_check_literal_bool(BoolLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	node.type = state.context.basicTypeNodes(BasicType.t_bool);
	node.state = AstNodeState.type_check_done;
}

void type_check_binary_op(BinaryExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.left.as_node, state);
	require_type_check(node.right.as_node, state);
	calcType(node, state.context);
	node.state = AstNodeState.type_check_done;
}

void type_check_unary_op(UnaryExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.child.as_node, state);
	assert(node.child.type, format("child(%s).type: is null", node.child.astType));
	switch(node.op) with(UnOp)
	{
		case addrOf:
			// make sure that variable gets stored in memory
			switch(node.child.astType)
			{
				case AstType.expr_var_name_use:
					(cast(AstNode*)node.child).cast_expr_name_use.varDecl.varFlags |= VariableFlags.isAddressTaken;
					break;
				case AstType.expr_index:
					break;
				default:
					state.context.internal_error(node.loc, "Cannot take address of %s", node.child.astType);
			}
			node.type = cast(TypeNode*) state.context.appendAst!PtrTypeNode(node.child.loc, node.child.type);
			break;
		case bitwiseNot:
			node.type = node.child.type;
			break;
		case logicalNot:
			autoconvToBool(node.child, state.context);
			node.type = state.context.basicTypeNodes(BasicType.t_bool);
			break;
		case minus:
			node.type = node.child.type;
			break;
		case preIncrement, postIncrement, preDecrement, postDecrement:
			node.type = node.child.type;
			break;
		case deref:
			if (node.child.type.isError) {
				node.type = node.child.type;
				break;
			}
			if (!node.child.type.isPointer) {
				state.context.unrecoverable_error(node.loc, "Cannot dereference %s", node.child.type.printer(state.context));
			}
			node.type = node.child.type.as_ptr.base;
			break;
		default:
			state.context.internal_error("un op %s not implemented", node.op);
			node.type = node.child.type;
	}
	node.state = AstNodeState.type_check_done;
}

// Get type from function declaration
void type_check_call(CallExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	// TODO: support more than plain func() calls. Such as func_array[42](), (*func_ptr)() etc
	state.context.assertf(node.callee.astType == AstType.expr_func_name_use ||
		node.callee.astType == AstType.expr_type_name_use,
		node.loc, "Only direct function calls are supported right now");
	AstNode* callee = node.callee.as_name_use.entity;

	switch (callee.astType)
	{
		case AstType.decl_function: return type_check_func_call(node, callee.cast_decl_function, state);
		case AstType.decl_struct: return type_check_constructor_call(node, callee.cast_decl_struct, state);
		default:
			node.type = state.context.basicTypeNodes(BasicType.t_error);
			state.context.error(node.loc, "Cannot call %s", callee.astType);
	}
	node.state = AstNodeState.type_check_done;
}

void type_check_func_call(CallExprNode* node, FunctionDeclNode* funcDecl, ref TypeCheckState state)
{
	Array!(VariableDeclNode*) params = funcDecl.parameters;
	auto numParams = params.length;
	auto numArgs = node.args.length;

	if (numArgs < numParams)
		state.context.error(node.loc, "Insufficient parameters to '%s', got %s, expected %s",
			state.context.idString(funcDecl.id), numArgs, numParams);
	else if (numArgs > numParams)
		state.context.error(node.loc, "Too much parameters to '%s', got %s, expected %s",
			state.context.idString(funcDecl.id), numArgs, numParams);

	foreach (i, ref ExpressionNode* arg; node.args)
	{
		require_type_check(params[i].type.as_node, state);
		require_type_check(arg.as_node, state);
		bool success = autoconvTo(arg, params[i].type, state.context);
		if (!success)
			state.context.error(arg.loc,
				"Argument %s, must have type %s, not %s", i+1,
				params[i].type.printer(state.context),
				arg.type.printer(state.context));
	}
	node.type = funcDecl.returnType;
}

void type_check_constructor_call(CallExprNode* node, StructDeclNode* s, ref TypeCheckState state)
{
	size_t numStructMembers;
	foreach(AstNode* member; s.declarations)
	{
		if (member.astType != AstType.decl_var) continue;

		ExpressionNode* initializer;
		if (node.args.length > numStructMembers) { // init from constructor argument
			require_type_check(node.args[numStructMembers].as_node, state);
			autoconvTo(node.args[numStructMembers], member.cast_decl_var.type.foldAliases, state.context);
		} else { // init with initializer from struct definition
			state.context.internal_error(node.loc, "Not implemented");
		}
		++numStructMembers;
	}
	node.type = s.as_node.cast_type_node;
}

void type_check_index(IndexExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.array.as_node, state);
	require_type_check(node.index.as_node, state);
	autoconvTo(node.index, state.context.basicTypeNodes(BasicType.t_i64), state.context);
	switch (node.array.type.astType) with(AstType)
	{
		case type_ptr, type_static_array, type_slice: break; // valid
		default: state.context.internal_error("Cannot index value of type `%s`", node.array.type.printer(state.context));
	}
	node.type = node.array.type.getElementType(state.context);
	node.state = AstNodeState.type_check_done;
}

void type_check_type_conv(TypeConvExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.expr.as_node, state);
	if (!isConvertibleTo(node.expr.type, node.type, state.context))
	{
		state.context.error(node.loc,
			"Cannot auto-convert expression of type `%s` to `%s`",
			node.expr.type.printer(state.context),
			node.type.printer(state.context));
	}
	node.state = AstNodeState.type_check_done;
}

void type_check_ptr(PtrTypeNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.base.as_node, state);
	node.state = AstNodeState.type_check_done;
}

void type_check_static_array(StaticArrayTypeNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.base.as_node, state);
	IrIndex val = eval_static_expr(node.length_expr.as_node, state.context);
	node.length = state.context.constants.get(val).i64.to!uint;
	node.state = AstNodeState.type_check_done;
}

void type_check_slice(SliceTypeNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.base.as_node, state);
	node.state = AstNodeState.type_check_done;
}
