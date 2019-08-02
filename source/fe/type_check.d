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
	auto sem3 = PassTypeCheck(&ctx);
	ctx.u8Ptr = ctx.appendAst!PtrTypeNode(TokenIndex(), ctx.basicTypeNodes(BasicType.t_u8));
	ctx.u8Slice = ctx.appendAst!SliceTypeNode(TokenIndex(), ctx.basicTypeNodes(BasicType.t_u8));

	foreach (ref SourceFileInfo file; ctx.files.data) {
		sem3.visit(file.mod);

		if (ctx.printAstSema && file.mod !is null) {
			auto astPrinter = AstPrinter(&ctx, 2);
			writefln("// AST typed `%s`", file.name);
			astPrinter.printAst(cast(AstNode*)file.mod);
		}
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

/// Annotates all expression nodes with their type
/// Type checking, casting
struct PassTypeCheck
{
	mixin AstVisitorMixin;

	CompilationContext* context;
	FunctionDeclNode* curFunc;

	void visit(ModuleDeclNode* m) {
		foreach (decl; m.declarations) _visit(decl);
	}
	void visit(ImportDeclNode* i) {}
	void visit(FunctionDeclNode* f) {
		auto prevFunc = curFunc;
		curFunc = f;
		f.backendData.callingConvention = &win64_call_conv;

		if (f.returnType.isOpaqueStruct) {
			context.error(f.returnType.loc,
				"function cannot return opaque type `%s`",
				f.returnType.printer(context));
		}

		foreach (VariableDeclNode* param; f.parameters) {
			visit(param);
		}

		if (f.block_stmt)
		{
			visit(f.block_stmt);
		}
		curFunc = prevFunc;
	}
	void visit(VariableDeclNode* v) {
		TypeNode* type = v.type.foldAliases;
		_visit(type);

		if (type.isOpaqueStruct) {
			string msg = v.isParameter ?
				"cannot declare parameter of opaque type `%2$s`" :
				"cannot declare variable `%s` of opaque type `%s`";

			context.error(v.type.loc,
				msg,
				context.idString(v.id),
				type.printer(context));
		}

		if (v.initializer) {
			_visit(v.initializer);
			autoconvTo(v.initializer, type, context);
		}

		if (!v.isParameter)
		switch (type.astType) with(AstType)
		{
			case type_static_array, decl_struct, type_slice:
				v.varFlags |= VariableFlags.forceMemoryStorage;
				break;

			default: break;
		}
	}
	void visit(StructDeclNode* s) {
		foreach (decl; s.declarations) _visit(decl);
	}
	void visit(EnumDeclaration* e) {
		foreach (decl; e.declarations) _visit(decl);
	}
	void visit(EnumMemberDecl* m) {
		_visit(m.type);

		if (m.initializer !is null) {
			if (!m.initializer.isLiteral) {
				context.error(m.initializer.loc, "enum members must be initialized with literals");
				return;
			}

			_visit(m.initializer);
			autoconvTo(m.initializer, m.type, context);
		}
	}
	void visit(BlockStmtNode* b) {
		foreach(stmt; b.statements) _visit(stmt);
	}
	void visit(IfStmtNode* i) {
		_visit(i.condition);
		autoconvToBool(i.condition, context);
		_visit(i.thenStatement);
		if (i.elseStatement) {
			_visit(i.elseStatement);
		}
	}
	void visit(WhileStmtNode* w) {
		_visit(w.condition);
		autoconvToBool(w.condition, context);
		_visit(w.statement);
	}
	void visit(DoWhileStmtNode* d) {
		_visit(d.statement);
		_visit(d.condition);
		autoconvToBool(d.condition, context);
	}
	void visit(ForStmtNode* n) {
		foreach(stmt; n.init_statements) _visit(stmt);
		if (n.condition) {
			_visit(n.condition);
			autoconvToBool(n.condition, context);
		}
		foreach(stmt; n.increment_statements) _visit(stmt);
		_visit(n.statement);
	}
	// Check return type and function return type
	void visit(ReturnStmtNode* r) {
		if (!curFunc)
		{
			context.error(r.loc,
				"Return statement is not inside function");
			return;
		}

		if (r.expression)
		{
			_visit(r.expression);
			if (curFunc.returnType.isVoid)
			{
				context.error(r.expression.loc,
					"Cannot return expression of type `%s` from void function",
					r.expression.type.typeName(context));
			}
			else
			{
				autoconvTo(r.expression, curFunc.returnType, context);
			}
		}
		else
		{
			if (!curFunc.returnType.isVoid)
				context.error(r.loc,
					"Cannot return void from non-void function",
					r.expression.type.typeName(context));
		}
	}
	void visit(BreakStmtNode* r) {}
	void visit(ContinueStmtNode* r) {}

	// Get type from variable declaration
	void visit(NameUseExprNode* v) {
		v.type = v.entity.get_node_type;
	}
	void visit(MemberExprNode* m) {
		_visit(m.aggregate);
		lookupMember(m, context);
	}
	void visit(IntLiteralExprNode* c) {
		if (c.isSigned)
			c.type = context.basicTypeNodes(minSignedIntType(c.value));
		else
			c.type = context.basicTypeNodes(minUnsignedIntType(c.value));
	}
	void visit(StringLiteralExprNode* c) {
		c.type = cast(TypeNode*)context.u8Slice;
	}
	void visit(NullLiteralExprNode* c) {
		c.type = context.basicTypeNodes(BasicType.t_null);
	}
	void visit(BoolLiteralExprNode* c) {
		c.type = context.basicTypeNodes(BasicType.t_bool);
	}
	void visit(BinaryExprNode* b) {
		_visit(b.left);
		_visit(b.right);
		calcType(b, context);
	}
	void visit(UnaryExprNode* u) {
		_visit(u.child);
		assert(u.child.type, format("child(%s).type: is null", u.child.astType));
		switch(u.op) with(UnOp)
		{
			case addrOf:
				// make sure that variable gets stored in memory
				switch(u.child.astType)
				{
					case AstType.expr_var_name_use:
						(cast(AstNode*)u.child).cast_expr_name_use.varDecl.varFlags |= VariableFlags.isAddressTaken;
						break;
					case AstType.expr_index:
						break;
					default:
						context.internal_error(u.loc, "Cannot take address of %s", u.child.astType);
				}
				u.type = cast(TypeNode*) context.appendAst!PtrTypeNode(u.child.loc, u.child.type);
				break;
			case bitwiseNot:
				u.type = u.child.type;
				break;
			case logicalNot:
				autoconvToBool(u.child, context);
				u.type = context.basicTypeNodes(BasicType.t_bool);
				break;
			case minus:
				u.type = u.child.type;
				break;
			case preIncrement, postIncrement, preDecrement, postDecrement:
				u.type = u.child.type;
				break;
			case deref:
				if (u.child.type.isError) {
					u.type = u.child.type;
					break;
				}
				if (!u.child.type.isPointer) {
					context.unrecoverable_error(u.loc, "Cannot dereference %s", u.child.type.printer(context));
				}
				u.type = u.child.type.as_ptr.base;
				break;
			default:
				context.internal_error("un op %s not implemented", u.op);
				u.type = u.child.type;
		}
	}
	// Get type from function declaration
	void visit(CallExprNode* c) {
		// TODO: support more than plain func() calls. Such as func_array[42](), (*func_ptr)() etc
		context.assertf(c.callee.astType == AstType.expr_func_name_use ||
			c.callee.astType == AstType.expr_type_name_use,
			c.loc, "Only direct function calls are supported right now");
		AstNode* callee = c.callee.as_name_use.entity;

		switch (callee.astType)
		{
			case AstType.decl_function: return visitCall(c, callee.cast_decl_function);
			case AstType.decl_struct: return visitConstructor(c, callee.cast_decl_struct);
			default:
				c.type = context.basicTypeNodes(BasicType.t_error);
				context.error(c.loc, "Cannot call %s", callee.astType);
		}

	}
	void visitCall(CallExprNode* c, FunctionDeclNode* funcDecl) {
		Array!(VariableDeclNode*) params = funcDecl.parameters;
		auto numParams = params.length;
		auto numArgs = c.args.length;

		if (numArgs < numParams)
			context.error(c.loc, "Insufficient parameters to '%s', got %s, expected %s",
				context.idString(funcDecl.id), numArgs, numParams);
		else if (numArgs > numParams)
			context.error(c.loc, "Too much parameters to '%s', got %s, expected %s",
				context.idString(funcDecl.id), numArgs, numParams);

		foreach (i, ref ExpressionNode* arg; c.args)
		{
			_visit(arg);
			bool success = autoconvTo(arg, params[i].type, context);
			if (!success)
				context.error(arg.loc,
					"Argument %s, must have type %s, not %s", i+1,
					params[i].type.printer(context),
					arg.type.printer(context));
		}
		c.type = funcDecl.returnType;
	}
	void visitConstructor(CallExprNode* c, StructDeclNode* s) {
		size_t numStructMembers;
		foreach(AstNode* member; s.declarations)
		{
			if (member.astType != AstType.decl_var) continue;

			ExpressionNode* initializer;
			if (c.args.length > numStructMembers) { // init from constructor argument
				_visit(c.args[numStructMembers]);
				autoconvTo(c.args[numStructMembers], member.cast_decl_var.type.foldAliases, context);
			} else { // init with initializer from struct definition
				context.internal_error(c.loc, "Not implemented");
			}
			++numStructMembers;
		}
		c.type = s.as_node.cast_type_node;
	}
	void visit(IndexExprNode* i) {
		_visit(i.array);
		_visit(i.index);
		autoconvTo(i.index, context.basicTypeNodes(BasicType.t_i64), context);
		switch (i.array.type.astType) with(AstType)
		{
			case type_ptr, type_static_array, type_slice: break; // valid
			default: context.internal_error("Cannot index value of type `%s`", i.array.type.printer(context));
		}
		i.type = i.array.type.getElementType(context);
	}
	void visit(TypeConvExprNode* t) {
		_visit(t.expr);
		if (!isConvertibleTo(t.expr.type, t.type, context))
		{
			context.error(t.loc,
				"Cannot auto-convert expression of type `%s` to `%s`",
				t.expr.type.printer(context),
				t.type.printer(context));
		}
	}
	void visit(BasicTypeNode* t) {}
	void visit(PtrTypeNode* t) {}
	void visit(StaticArrayTypeNode* t) {}
	void visit(SliceTypeNode* t) {}
}
