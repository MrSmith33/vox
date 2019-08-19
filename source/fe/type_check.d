/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.type_check;

import std.stdio;
import std.string : format;
import std.typecons : Flag, Yes, No;
import all;

void pass_type_check(ref CompilationContext context, CompilePassPerModule[] subPasses)
{
	auto state = TypeCheckState(&context);

	foreach (ref SourceFileInfo file; context.files.data) {
		AstIndex modIndex = file.mod.get_ast_index(&context);
		require_type_check(modIndex, state);

		if (context.printAstSema && modIndex) {
			auto astPrinter = AstPrinter(&context, 2);
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
void require_type_check(ref AstIndex nodeIndex, CompilationContext* context)
{
	auto state = TypeCheckState(context);
	require_type_check(nodeIndex, state);
}

/// Annotates all expression nodes with their type
/// Type checking, casting
void require_type_check(ref AstIndex nodeIndex, ref TypeCheckState state)
{
	AstNode* node = state.context.getAstNode(nodeIndex);

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
			type_check_member(nodeIndex, cast(MemberExprNode*)node, state); break;
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
bool autoconvToCommonType(ref AstIndex leftIndex, ref AstIndex rightIndex, CompilationContext* context)
{
	ExpressionNode* left = leftIndex.get_expr(context);
	ExpressionNode* right = rightIndex.get_expr(context);
	TypeNode* leftType = left.type.get_type(context);
	TypeNode* rightType = right.type.get_type(context);

	if (leftType.astType == AstType.type_basic && rightType.astType == AstType.type_basic)
	{
		BasicType commonType = commonBasicType[leftType.as_basic.basicType][rightType.as_basic.basicType];
		if (commonType != BasicType.t_error)
		{
			AstIndex type = context.basicTypeNodes(commonType);
			bool successLeft = autoconvTo(leftIndex, type, context);
			bool successRight = autoconvTo(rightIndex, type, context);
			if(successLeft && successRight)
				return true;
		}
	}
	else if (leftType.isPointer && rightType.isTypeofNull) {
		right.type = left.type;
		return true;
	}
	else if (leftType.isTypeofNull && rightType.isPointer) {
		left.type = right.type;
		return true;
	}
	else
	{
		// error for user-defined types
	}

	return false;
}

void autoconvToBool(ref AstIndex exprIndex, CompilationContext* context)
{
	ExpressionNode* expr = exprIndex.get_expr(context);
	if (expr.type.get_type(context).isError) return;
	if (!autoconvTo(exprIndex, context.basicTypeNodes(BasicType.t_bool), context))
		context.error(expr.loc, "Cannot implicitly convert `%s` to bool",
			expr.type.typeName(context));
}

bool isConvertibleTo(AstIndex fromTypeIndex, AstIndex toTypeIndex, CompilationContext* context)
{
	TypeNode* fromType = fromTypeIndex.get_type(context);
	TypeNode* toType = toTypeIndex.get_type(context);

	if (same_type(fromTypeIndex, toTypeIndex, context)) return true;

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
bool autoconvTo(ref AstIndex exprIndex, AstIndex typeIndex, CompilationContext* context)
{
	ExpressionNode* expr = exprIndex.get_expr(context);
	TypeNode* type = typeIndex.get_type(context).foldAliases(context);
	TypeNode* exprType = expr.type.get_type(context);

	if (same_type(expr.type, typeIndex, context)) return true;
	string extraError;

	if (exprType.astType == AstType.type_basic && type.astType == AstType.type_basic)
	{
		BasicType fromType = exprType.as_basic.basicType;
		BasicType toType = type.as_basic.basicType;
		bool canConvert = isAutoConvertibleFromToBasic[fromType][toType];
		if (canConvert)
		{
			if (expr.astType == AstType.literal_int) {
				//writefln("int %s %s -> %s", expr.loc, expr.type.printer(context), type.printer(context));
				// change type of int literal inline
				expr.type = typeIndex;
			} else {
				exprIndex = context.appendAst!TypeConvExprNode(expr.loc, typeIndex, IrIndex(), exprIndex);
			}
			return true;
		}
		else if (expr.astType == AstType.literal_int && toType.isInteger) {
			auto lit = cast(IntLiteralExprNode*) expr;
			if (lit.isSigned) {
				if (numSignedBytesForInt(lit.value) <= integerSize(toType)) {
					expr.type = typeIndex;
					return true;
				}
			} else {
				if (numUnsignedBytesForInt(lit.value) <= integerSize(toType)) {
					expr.type = typeIndex;
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
		if (type.astType == AstType.type_ptr)
		{
			TypeNode* ptrBaseType = type.as_ptr.base.get_type(context);
			if (ptrBaseType.astType == AstType.type_basic &&
				ptrBaseType.as_basic.basicType == BasicType.t_u8)
			{
				auto memberExpr = context.appendAst!MemberExprNode(expr.loc, typeIndex, IrIndex(), exprIndex, AstIndex.init, 1);
				memberExpr.get_node(context).astType = AstType.expr_slice_member;
				exprIndex = memberExpr;
				return true;
			}
		}
	}
	else if (exprType.isStaticArray && type.isSlice)
	{
		if (same_type(exprType.as_static_array.base, type.as_slice.base, context))
		{
			exprIndex = context.appendAst!UnaryExprNode(
				expr.loc, typeIndex, IrIndex(), UnOp.staticArrayToSlice, exprIndex);
			return true;
		}
	}
	else if (expr.astType == AstType.literal_null) {
		if (type.isPointer) {
			expr.type = typeIndex;
			return true;
		} else if (type.isSlice) {
			expr.type = typeIndex;
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

void calcType(BinaryExprNode* b, CompilationContext* context)
{
	assert(b.left.get_expr(context).type, format("left(%s).type: is null", b.left.get_node(context).astType));
	assert(b.right.get_expr(context).type, format("right(%s).type: is null", b.right.get_node(context).astType));

	setResultType(b, context);
}


void type_check_module(ModuleDeclNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	foreach (ref AstIndex decl; node.declarations) require_type_check(decl, state);
	node.state = AstNodeState.type_check_done;
}

void type_check_func(FunctionDeclNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	auto prevFunc = state.curFunc;
	state.curFunc = node;
	node.backendData.callingConvention = &win64_call_conv;

	TypeNode* returnType = node.returnType.get_type(c);
	if (returnType.isOpaqueStruct(c)) {
		c.error(node.loc,
			"function cannot return opaque type `%s`",
			returnType.printer(c));
	}

	foreach (ref AstIndex param; node.parameters) {
		require_type_check(param, state);
	}

	if (node.block_stmt)
	{
		require_type_check(node.block_stmt, state);
	}
	state.curFunc = prevFunc;
	node.state = AstNodeState.type_check_done;
}

void type_check_var(VariableDeclNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	TypeNode* type = node.type.get_type(c);
	node.state = AstNodeState.type_check;
	require_type_check(node.type, state);

	if (type.isOpaqueStruct(c)) {
		if (node.isParameter) {
			c.error(node.loc,
				"cannot declare parameter of opaque type `%s`",
				type.printer(c));
		} else {
			c.error(node.loc,
				"cannot declare variable `%s` of opaque type `%s`",
				c.idString(node.id),
				type.printer(c));
		}
	}

	if (node.initializer) {
		require_type_check(node.initializer, state);
		autoconvTo(node.initializer, node.type, c);
	}

	if (!node.isParameter)
	{
		switch (type.astType) with(AstType)
		{
			case type_static_array, decl_struct, type_slice:
				node.varFlags |= VariableFlags.forceMemoryStorage;
				break;

			default: break;
		}
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
	require_type_check(node.type, state);

	if (node.initializer) {
		require_type_check(node.initializer, state);
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
	require_type_check(node.condition, state);
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
	require_type_check(node.condition, state);
	autoconvToBool(node.condition, state.context);
	require_type_check(node.statement, state);
	node.state = AstNodeState.type_check_done;
}

void type_check_do(DoWhileStmtNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.statement, state);
	require_type_check(node.condition, state);
	autoconvToBool(node.condition, state.context);
	node.state = AstNodeState.type_check_done;
}

void type_check_for(ForStmtNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	foreach(stmt; node.init_statements) require_type_check(stmt, state);
	if (node.condition) {
		require_type_check(node.condition, state);
		autoconvToBool(node.condition, state.context);
	}
	foreach(stmt; node.increment_statements) require_type_check(stmt, state);
	require_type_check(node.statement, state);
	node.state = AstNodeState.type_check_done;
}

// Check return type and function return type
void type_check_return(ReturnStmtNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	if (!state.curFunc)
	{
		c.error(node.loc,
			"Return statement is not inside function");
		return;
	}

	if (node.expression)
	{
		require_type_check(node.expression, state);
		if (state.curFunc.returnType.get_type(c).isVoid)
		{
			c.error(node.expression.get_expr(c).loc,
				"Cannot return expression of type `%s` from void function",
				node.expression.get_expr(c).type.typeName(c));
		}
		else
		{
			autoconvTo(node.expression, state.curFunc.returnType, c);
		}
	}
	else
	{
		if (!state.curFunc.returnType.get_type(c).isVoid)
			c.error(node.loc,
				"Cannot return void from non-void function",
				node.expression.get_expr(c).type.typeName(c));
	}
	node.state = AstNodeState.type_check_done;
}

// Get type from variable declaration
void type_check_name_use(NameUseExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	node.type = node.entity.get_node_type(state.context);
	node.state = AstNodeState.type_check_done;
}

void type_check_member(ref AstIndex nodeIndex, MemberExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;
	node.state = AstNodeState.type_check;

	// try member
	NameUseExprNode* member = node.member.get_expr_name_use(c);
	require_type_check(node.aggregate, state);
	LookupResult res = lookupMember(node, c);
	if (res == LookupResult.success) {
		node.state = AstNodeState.type_check_done;
		return;
	}

	// try UFCS
	AstIndex ufcsNodeIndex = lookupScopeIdRecursive(node.curScope.get_scope(c), member.id(c), node.loc, c);
	if (ufcsNodeIndex)
	{
		AstNode* ufcsNode = c.getAstNode(ufcsNodeIndex);
		if (ufcsNode.astType == AstType.decl_function)
		{
			// rewrite as call
			member.resolve = ufcsNodeIndex;
			member.astType = AstType.expr_func_name_use;
			Array!AstIndex args;
			args.put(c.arrayArena, node.aggregate);
			nodeIndex = c.appendAst!CallExprNode(member.loc, AstIndex(), IrIndex(), node.member, args);
			nodeIndex.get_node(c).state = AstNodeState.name_resolve_done;
			// type check call
			require_type_check(nodeIndex, state);
			return;
		}
	}

	// nothing found
	node.type = c.basicTypeNodes(BasicType.t_error);
	AstIndex objType = node.aggregate.get_node_type(c);
	c.error(node.loc, "`%s` has no member `%s`", objType.printer(c), c.idString(member.id(c)));

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
	require_type_check(node.left, state);
	require_type_check(node.right, state);
	calcType(node, state.context);
	node.state = AstNodeState.type_check_done;
}

void type_check_unary_op(UnaryExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	require_type_check(node.child, state);
	ExpressionNode* child = node.child.get_expr(c);
	assert(child.type, format("child(%s).type: is null", child.astType));
	switch(node.op) with(UnOp)
	{
		case addrOf:
			// make sure that variable gets stored in memory
			switch(child.astType)
			{
				case AstType.expr_var_name_use:
					child.as_name_use.varDecl(c).varFlags |= VariableFlags.isAddressTaken;
					break;
				case AstType.expr_index:
					break;
				default:
					c.internal_error(node.loc, "Cannot take address of %s", child.astType);
			}
			node.type = c.appendAst!PtrTypeNode(node.child.loc(c), node.child.expr_type(c));
			break;
		case bitwiseNot:
			node.type = child.type;
			break;
		case logicalNot:
			autoconvToBool(node.child, c);
			node.type = c.basicTypeNodes(BasicType.t_bool);
			break;
		case minus:
			node.type = child.type;
			break;
		case preIncrement, postIncrement, preDecrement, postDecrement:
			node.type = child.type;
			break;
		case deref:
			if (child.type.get_type(c).isError) {
				node.type = child.type;
				break;
			}
			if (!child.type.get_type(c).isPointer) {
				c.unrecoverable_error(node.loc, "Cannot dereference %s", child.type.printer(c));
			}
			node.type = child.type.get_type(c).as_ptr.base;
			break;
		default:
			c.internal_error("un op %s not implemented", node.op);
			node.type = node.child.expr_type(c);
	}
	node.state = AstNodeState.type_check_done;
}

// Get type from function declaration
void type_check_call(CallExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	// TODO: support more than plain func() calls. Such as func_array[42](), (*func_ptr)() etc
	c.assertf(node.callee.astType(c) == AstType.expr_func_name_use ||
		node.callee.astType(c) == AstType.expr_type_name_use,
		node.loc, "Only direct function calls are supported right now");
	AstNode* callee = node.callee.get_name_use(c).entity.get_node(c);

	switch (callee.astType)
	{
		case AstType.decl_function: return type_check_func_call(node, callee.cast_decl_function, state);
		case AstType.decl_struct: return type_check_constructor_call(node, callee.cast_decl_struct, state);
		default:
			node.type = c.basicTypeNodes(BasicType.t_error);
			c.error(node.loc, "Cannot call %s", callee.astType);
	}
	node.state = AstNodeState.type_check_done;
}

void type_check_func_call(CallExprNode* node, FunctionDeclNode* funcDecl, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	Array!AstIndex params = funcDecl.parameters;
	auto numParams = params.length;
	auto numArgs = node.args.length;

	if (numArgs < numParams)
		c.error(node.loc, "Insufficient parameters to '%s', got %s, expected %s",
			c.idString(funcDecl.id), numArgs, numParams);
	else if (numArgs > numParams)
		c.error(node.loc, "Too much parameters to '%s', got %s, expected %s",
			c.idString(funcDecl.id), numArgs, numParams);

	foreach (i, ref AstIndex arg; node.args)
	{
		VariableDeclNode* param = c.getAst!VariableDeclNode(params[i]);

		require_type_check(param.type, state);
		require_type_check(arg, state);
		bool success = autoconvTo(arg, param.type, c);
		if (!success)
			c.error(arg.loc(c),
				"Argument %s, must have type %s, not %s", i+1,
				param.type.printer(c),
				arg.expr_type(c).printer(c));
	}
	node.type = funcDecl.returnType;
}

void type_check_constructor_call(CallExprNode* node, StructDeclNode* s, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	size_t numStructMembers;
	foreach(AstIndex memberIndex; s.declarations)
	{
		AstNode* member = memberIndex.get_node(c);
		if (member.astType != AstType.decl_var) continue;

		ExpressionNode* initializer;
		if (node.args.length > numStructMembers) { // init from constructor argument
			require_type_check(node.args[numStructMembers], state);
			autoconvTo(node.args[numStructMembers], member.cast_decl_var.type, c);
		} else { // init with initializer from struct definition
			c.internal_error(node.loc, "Not implemented");
		}
		++numStructMembers;
	}
	node.type = c.getAstNodeIndex(s);
}

void type_check_index(IndexExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	require_type_check(node.array, state);
	require_type_check(node.index, state);
	autoconvTo(node.index, c.basicTypeNodes(BasicType.t_i64), c);
	switch (node.array.expr_type(c).astType(c)) with(AstType)
	{
		case type_ptr, type_static_array, type_slice: break; // valid
		default: c.internal_error("Cannot index value of type `%s`", node.array.expr_type(c).printer(c));
	}
	node.type = node.array.expr_type(c).get_type(c).getElementType(c);
	node.state = AstNodeState.type_check_done;
}

void type_check_type_conv(TypeConvExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	require_type_check(node.expr, state);
	if (!isConvertibleTo(node.expr.expr_type(c), node.type, c))
	{
		c.error(node.loc,
			"Cannot auto-convert expression of type `%s` to `%s`",
			node.expr.expr_type(c).printer(c),
			node.type.printer(c));
	}
	node.state = AstNodeState.type_check_done;
}

void type_check_ptr(PtrTypeNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.base, state);
	node.state = AstNodeState.type_check_done;
}

void type_check_static_array(StaticArrayTypeNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;
	require_type_check(node.base, state);
	IrIndex val = eval_static_expr(node.length_expr, c);
	node.length = c.constants.get(val).i64.to!uint;
	node.state = AstNodeState.type_check_done;
}

void type_check_slice(SliceTypeNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.base, state);
	node.state = AstNodeState.type_check_done;
}
