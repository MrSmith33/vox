/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module semantics;

import std.stdio;
import std.string : format;
import std.typecons : Flag, Yes, No;
import all;

///
struct Scope
{
	///
	Symbol*[Identifier] symbols;
	///
	Scope* parentScope;
	///
	string debugName;
	/// Ordered scope is in function body, requires declaration above use
	/// Unordered scope is in struct, module
	bool isOrdered;
}

void pass_semantic_decl(ref CompilationContext ctx)
{
	auto sem1 = SemanticDeclarations(&ctx);
	sem1.visit(ctx.mod);
}

/// Register identifiers in scope tree
struct SemanticDeclarations
{
	mixin AstVisitorMixin;

	CompilationContext* context;
	Scope* currentScope;

	Scope* pushScope(string name, Flag!"ordered" isOrdered)
	{
		Scope* newScope = new Scope;
		newScope.isOrdered = isOrdered;
		newScope.debugName = name;

		if (currentScope)
			newScope.parentScope = currentScope;
		currentScope = newScope;

		return currentScope;
	}

	void popScope()
	{
		if (currentScope.parentScope)
			currentScope = currentScope.parentScope;
		else
			currentScope = null;
	}

	/// Constructs and inserts symbol with id
	Symbol* insert(Identifier id, SourceLocation loc, SymbolClass symClass, AstNode* node)
	{
		typeof(Symbol.flags) flags = currentScope.isOrdered ? SymbolFlags.isInOrderedScope : 0;
		auto sym = new Symbol(id, loc, symClass, flags, node);
		insert(sym);
		return sym;
	}

	/// Inserts symbol `sym`
	void insert(Symbol* sym)
	{
		if (auto s = currentScope.symbols.get(sym.id, null))
		{
			context.error(sym.loc,
				"declaration `%s` is already defined at %s", context.idString(sym.id), s.loc);
		}
		currentScope.symbols[sym.id] = sym;
	}

	void visit(ModuleDeclNode* m) {
		context.mod._scope = pushScope("Module", No.ordered);
		foreach (decl; context.mod.declarations) _visit(decl);
		popScope;
	}
	void visit(FunctionDeclNode* f) {
		context.mod.addFunction(f);
		f.resolveSymbol = insert(f.id, f.loc, SymbolClass.c_function, cast(AstNode*)f);
		f._scope = pushScope(context.idString(f.id), Yes.ordered);
		foreach (param; f.parameters) visit(param);
		if (f.block_stmt) visit(f.block_stmt);
		popScope;
	}
	void visit(VariableDeclNode* v) {
		v.resolveSymbol = insert(v.id, v.loc, SymbolClass.c_variable, cast(AstNode*)v);
		if (v.initializer) _visit(v.initializer);
	}
	void visit(StructDeclNode* s) {
		s.resolveSymbol = insert(s.id, s.loc, SymbolClass.c_struct, cast(AstNode*)s);
		s._scope = pushScope(context.idString(s.id), No.ordered);
		foreach (decl; s.declarations) _visit(decl);
		popScope;
	}
	void visit(BlockStmtNode* b) {
		b._scope = pushScope("Block", Yes.ordered);
		foreach(stmt; b.statements) _visit(stmt);
		popScope;
	}
	void visit(IfStmtNode* i) {
		_visit(i.condition);
		i.then_scope = pushScope("Then", Yes.ordered);
		_visit(i.thenStatement);
		popScope;
		if (i.elseStatement) {
			i.else_scope = pushScope("Else", Yes.ordered);
			_visit(i.elseStatement);
			popScope;
		}
	}
	void visit(WhileStmtNode* w) {
		_visit(w.condition);
		w._scope = pushScope("While", Yes.ordered);
		_visit(w.statement);
		popScope;
	}
	void visit(DoWhileStmtNode* d) {
		d._scope = pushScope("While", Yes.ordered);
		_visit(d.statement);
		popScope;
		_visit(d.condition);
	}
	void visit(ReturnStmtNode* r) {}
	void visit(BreakStmtNode* r) {}
	void visit(ContinueStmtNode* r) {}
	void visit(NameUseExprNode* v) {}
	void visit(MemberExprNode* m) {}
	void visit(IntLiteralExprNode* c) {}
	void visit(StringLiteralExprNode* c) {}
	void visit(BinaryExprNode* b) {
		if (b.isAssignment)
		{
			if (!b.isStatement)
				context.error(b.loc,
					"Cannot use assignment here. Only can use as statement.");
		}
	}
	void visit(UnaryExprNode* u) {}
	void visit(CallExprNode* c) {}
	void visit(IndexExprNode* i) {}
	void visit(TypeConvExprNode* c) {}
	void visit(BasicTypeNode* t) {}
	void visit(PtrTypeNode* t) {}
	void visit(StaticArrayTypeNode* t) {}
	void visit(SliceTypeNode* t) {}
	void visit(StructTypeNode* t) {}
}

void pass_semantic_lookup(ref CompilationContext ctx)
{
	auto sem2 = SemanticLookup(&ctx);
	sem2.visit(ctx.mod);
}

/// Resolves all symbol references (variable/type/function uses)
/// using information collected on previous pass
struct SemanticLookup
{
	mixin AstVisitorMixin;

	CompilationContext* context;

	// TODO: do not maintain all visible symbols for current scope
	// We will only use a small portion of visible symbols in each scope,
	// so maintaining this is most probably wasted effort, and
	// it is faster to walk up the scope stack. Need to benchmark.
	Symbol*[Identifier] symbols;

	Scope* currentScope;

	Identifier id_ptr;
	Identifier id_length;

	/// Used in 2 semantic pass
	void pushCompleteScope(Scope* newScope)
	{
		currentScope = newScope;
		foreach (id, sym; newScope.symbols)
		{
			if (auto outerSymbol = symbols.get(sym.id, null))
				sym.outerSymbol = outerSymbol;
			symbols[id] = sym;
		}
	}

	/// Used in 2 semantic pass
	void popScope()
	{
		assert(currentScope);

		// Pop all symbols of the scope we are leaving from symbols
		foreach(id, sym; currentScope.symbols)
		{
			if (sym.outerSymbol) // replace by symbol from outer scope
				symbols[id] = sym.outerSymbol;
			else // or simply remove it if no such symbol
				symbols.remove(id);
		}

		if (currentScope.parentScope)
			currentScope = currentScope.parentScope;
		else
			currentScope = null;
	}

	/// Look up symbol by Identifier. Searches the whole stack of scopes.
	Symbol* lookup(const Identifier id, SourceLocation from)
	{
		auto sym = symbols.get(id, null);
		while (sym)
		{
			// forward reference allowed for unordered scope
			if (!sym.isInOrderedScope) break;
			// not a forward reference
			else if (from.start > sym.loc.start) break;

			sym = sym.outerSymbol;
		}

		if (sym) {
			// exists
		}
		else
		{
			context.error(from, "undefined identifier `%s`", context.idString(id));
		}
		return sym;
	}

	/// Look up member by Identifier. Searches aggregate scope for identifier.
	void lookupMember(MemberExprNode* expr)
	{
		Identifier id = expr.member.id;
		string idStr = context.idString(id);

		if (expr.aggregate.astType != AstType.expr_name_use) {
			context.error(expr.loc, "Cannot resolve `%s` for %s", idStr, expr.aggregate.astType);
			return;
		}

		Symbol* aggSym = (cast(NameUseExprNode*)expr.aggregate).getSym;

		if (aggSym.symClass != SymbolClass.c_variable) {
			context.error(expr.loc, "Cannot resolve `%s` for %s", idStr, aggSym.symClass);
			return;
		}

		VariableDeclNode* varDecl = aggSym.varDecl;

		bool success;
		switch (varDecl.type.astType)
		{
			case AstType.type_slice:
				success = lookupSliceMember(expr, varDecl.type.sliceTypeNode, id);
				break;

			case AstType.type_struct:
				success = lookupStructMember(expr, varDecl.type.structTypeNode, id);
				break;

			default: break;
		}
		if (!success)
			context.error(expr.loc, "`%s` of type `%s` has no member `%s`",
				varDecl.strId(context), varDecl.type.printer(context), idStr);
	}

	bool lookupSliceMember(MemberExprNode* expr, SliceTypeNode* sliceType, Identifier id)
	{
		if (id == id_ptr)
		{
			expr.memberIndex = 1;
			expr.type = cast(TypeNode*) new PtrTypeNode(sliceType.loc, sliceType.base);
			return true;
		}
		else if (id == id_length)
		{
			expr.memberIndex = 0;
			expr.type = context.basicTypeNodes(BasicType.t_u64);
			return true;
		}

		return false;
	}

	bool lookupStructMember(MemberExprNode* expr, StructTypeNode* structType, Identifier id)
	{
		StructDeclNode* structDecl = structType.getSym.structDecl;
		Symbol* memberSym = structDecl._scope.symbols.get(id, null);
		expr.member.resolveSymbol(memberSym);

		if (memberSym)
		{
			final switch(memberSym.symClass)
			{
				case SymbolClass.c_function:
					context.internal_error("member functions/UFCS calls are not implemented");
					assert(false);

				case SymbolClass.c_variable:
					VariableDeclNode* memberVar = expr.member.getSym.varDecl;
					expr.type = memberVar.type;
					expr.memberIndex = memberVar.scopeIndex;
					return true;

				case SymbolClass.c_struct:
					context.internal_error("member structs are not implemented");
					assert(false);
			}
		}

		return false;
	}

	void visit(ModuleDeclNode* m) {
		id_ptr = context.idMap.getOrRegNoDup("ptr");
		id_length = context.idMap.getOrRegNoDup("length");

		pushCompleteScope(m._scope);
		foreach (decl; m.declarations) _visit(decl);
		popScope;
	}
	void visit(FunctionDeclNode* f) {
		pushCompleteScope(f._scope);
		_visit(f.returnType);
		foreach (param; f.parameters) visit(param);
		if (f.block_stmt) visit(f.block_stmt);
		popScope;
	}
	void visit(VariableDeclNode* v) {
		_visit(v.type);
		if (v.initializer) _visit(v.initializer);
	}
	void visit(StructDeclNode* s) {
		pushCompleteScope(s._scope);
		foreach (decl; s.declarations) _visit(decl);
		popScope;
	}
	void visit(BlockStmtNode* b) {
		pushCompleteScope(b._scope);
		foreach(stmt; b.statements) _visit(stmt);
		popScope;
	}
	void visit(IfStmtNode* i) {
		_visit(i.condition);
		pushCompleteScope(i.then_scope);
		_visit(i.thenStatement);
		popScope;
		if (i.elseStatement) {
			pushCompleteScope(i.else_scope);
			_visit(i.elseStatement);
			popScope;
		}
	}
	void visit(WhileStmtNode* w) {
		_visit(w.condition);
		pushCompleteScope(w._scope);
		_visit(w.statement);
		popScope;
	}
	void visit(DoWhileStmtNode* d) {
		pushCompleteScope(d._scope);
		_visit(d.statement);
		popScope;
		_visit(d.condition);
	}
	void visit(ReturnStmtNode* r) {
		if (r.expression) _visit(r.expression);
	}
	void visit(BreakStmtNode* r) {}
	void visit(ContinueStmtNode* r) {}
	void visit(NameUseExprNode* v) {
		v.resolveSymbol = lookup(v.id, v.loc);
	}
	void visit(MemberExprNode* m) {
		_visit(m.aggregate);
		lookupMember(m);
	}
	void visit(IntLiteralExprNode* c) {}
	void visit(StringLiteralExprNode* c) {}
	void visit(BinaryExprNode* b) {
		_visit(b.left);
		_visit(b.right);
	}
	void visit(UnaryExprNode* u) { _visit(u.child); }
	void visit(CallExprNode* c) {
		_visit(c.callee);
		foreach (arg; c.args) _visit(arg); }
	void visit(IndexExprNode* i) {
		_visit(i.array);
		_visit(i.index);
	}
	void visit(TypeConvExprNode* t) { _visit(t.type); _visit(t.expr); }
	void visit(BasicTypeNode* t) {}
	void visit(PtrTypeNode* t) { _visit(t.base); }
	void visit(StaticArrayTypeNode* t) { _visit(t.base); }
	void visit(SliceTypeNode* t) { _visit(t.base); }
	void visit(StructTypeNode* t) { t.resolveSymbol = lookup(t.id, t.loc); }
}

void pass_semantic_type(ref CompilationContext ctx)
{
	auto sem3 = SemanticStaticTypes(&ctx);
	sem3.visit(ctx.mod);

	if (ctx.printAstSema && ctx.mod !is null) {
		auto astPrinter = AstPrinter(&ctx, 2);
		astPrinter.printAst(cast(AstNode*)ctx.mod);
	}
}

/// Annotates all expression nodes with their type
/// Type checking, casting
struct SemanticStaticTypes
{
	mixin AstVisitorMixin;

	CompilationContext* context;
	FunctionDeclNode* curFunc;
	PtrTypeNode* u8Ptr;
	SliceTypeNode* u8Slice;

	bool isBool(TypeNode* type)
	{
		return
			type.astType == AstType.type_basic &&
			type.basicTypeNode.basicType == BasicType.t_bool;
	}

	/// Returns true if types are equal or were converted to common type. False otherwise
	bool autoconvToCommonType(ref ExpressionNode* left, ref ExpressionNode* right)
	{
		if (left.type.astType == AstType.type_basic && right.type.astType == AstType.type_basic)
		{
			BasicTypeNode* leftType = left.type.basicTypeNode;
			BasicTypeNode* rightType = right.type.basicTypeNode;

			BasicType commonType = commonBasicType[leftType.basicType][rightType.basicType];
			bool successLeft = autoconvTo(left, commonType, Yes.force);
			bool successRight = autoconvTo(right, commonType, Yes.force);
			if(successLeft && successRight)
				return true;
		}
		else
		{
			// error for user-defined types

		}

		context.error(left.loc, "No common type between `%s` and `%s`",
			left.type.typeName(context),
			right.type.typeName(context));

		return false;
	}

	bool autoconvToBool(ref ExpressionNode* expr)
	{
		return autoconvTo(expr, BasicType.t_bool, No.force);
	}

	/// Returns true if conversion was successful. False otherwise
	bool autoconvTo(ref ExpressionNode* expr, BasicType toType, Flag!"force" force)
	{
		TypeNode* type = context.basicTypeNodes(toType);
		// Skip if already the same type
		if (sameType(expr.type, type)) return true;

		if (expr.type.astType == AstType.type_basic)
		{
			BasicType fromType = expr.type.basicTypeNode.basicType;
			bool canConvert = isAutoConvertibleFromToBasic[fromType][toType];
			if (canConvert || force)
			{
				expr = cast(ExpressionNode*) new TypeConvExprNode(expr.loc, type, IrIndex(), expr);
				return true;
			}
		}

		context.error(expr.loc, "Cannot auto-convert expression of type `%s` to `%s`",
			expr.type.printer(context),
			type.printer(context));
		return false;
	}

	bool isConvertibleTo(TypeNode* fromType, TypeNode* toType)
	{
		if (sameType(fromType, toType)) return true;

		if (fromType.astType == AstType.type_basic && toType.astType == AstType.type_basic)
		{
			BasicType fromTypeBasic = fromType.basicTypeNode.basicType;
			BasicType toTypeBasic = toType.basicTypeNode.basicType;
			bool isRegisterTypeFrom =
				(fromTypeBasic >= BasicType.t_bool &&
				fromTypeBasic <= BasicType.t_u64) ||
				fromType.astType == AstType.type_ptr;
			bool isRegisterTypeTo =
				(toTypeBasic >= BasicType.t_bool &&
				toTypeBasic <= BasicType.t_u64) ||
				toType.astType == AstType.type_ptr;
			// all integer types, pointers and bool can be converted between
			return isRegisterTypeFrom && isRegisterTypeTo;
		}
		return false;
	}

	bool autoconvTo(ref ExpressionNode* expr, TypeNode* type)
	{
		if (sameType(expr.type, type)) return true;

		string extraError;

		if (expr.type.astType == AstType.type_basic && type.astType == AstType.type_basic)
		{
			BasicType fromType = expr.type.basicTypeNode.basicType;
			BasicType toType = type.basicTypeNode.basicType;
			bool canConvert = isAutoConvertibleFromToBasic[fromType][toType];
			if (canConvert)
			{
				if (expr.astType == AstType.literal_int) {
					//writefln("int %s %s -> %s", expr.loc, expr.type.printer(context), type.printer(context));
					expr.type = type;
				} else {
					expr = cast(ExpressionNode*) new TypeConvExprNode(expr.loc, type, IrIndex(), expr);
				}
				return true;
			}
		}
		// auto cast from string literal to c_char*
		else if (expr.astType == AstType.literal_string)
		{
			if (type.astType == AstType.type_slice &&
				type.ptrTypeNode.base.astType == AstType.type_basic &&
				type.ptrTypeNode.base.basicTypeNode.basicType == BasicType.t_u8)
			{
				return true;
			}
		}
		else
		{
			extraError = ". Cannot convert from/to user-defined type";
		}

		context.error(expr.loc, "Cannot auto-convert expression of type `%s` to `%s`%s",
			expr.type.printer(context),
			type.printer(context),
			extraError);
		return false;
	}

	void setResultType(BinaryExprNode* b)
	{
		TypeNode* resRype = context.basicTypeNodes(BasicType.t_error);
		switch(b.op) with(BinOp)
		{
			/*
			// logic ops. Requires both operands to be bool
			case AND_AND, OR_OR:
				bool successLeft = autoconvToBool(b.left);
				bool successRight = autoconvToBool(b.right);
				if (successLeft && successRight)
				{
					resRype = context.basicTypeNodes(BasicType.t_bool);
				}
				else
				{
					if (!successLeft) context.error(b.left.loc, "Cannot implicitly convert `%s` of type `%s` to bool",
						b.left.type.typeName(context),
						b.right.type.typeName(context));
					if (!successRight) context.error(b.right.loc, "Cannot implicitly convert `%s` of type `%s` to bool",
						b.left.type.typeName(context),
						b.right.type.typeName(context));
				}
				break;
		*/
			// logic ops. Requires both operands to be of the same type
			case EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL:
				if (autoconvToCommonType(b.left, b.right))
					resRype = context.basicTypeNodes(BasicType.t_bool);
				else
					context.error(b.left.loc, "Cannot compare `%s` and `%s`",
						b.left.type.typeName(context),
						b.right.type.typeName(context));
				break;

			// arithmetic op int float
			case MINUS, PLUS, DIV, MULT:
				if (autoconvToCommonType(b.left, b.right))
					resRype = b.left.type;
				else
				{
					context.error(b.left.loc, "Cannot perform `%s` %s `%s` operation",
						b.left.type.typeName(context), b.op,
						b.right.type.typeName(context));
				}
				break;

			case ASSIGN:
				if (b.left.astType == AstType.expr_name_use ||
					b.left.astType == AstType.expr_index ||
					b.left.astType == AstType.expr_member)
				{
					autoconvTo(b.right, b.left.type);
				}
				else
					context.error(b.left.loc, "Cannot perform assignment into %s", b.left.astType);
				resRype = context.basicTypeNodes(BasicType.t_void);
				break;
		/*
			// arithmetic op int
			case AND: goto case;
			case ASHR: goto case;
			case OR: goto case;
			case PERCENT: goto case;
			case SHL: goto case;
			case SHR: goto case;
			case XOR:
				resRype = context.basicTypeNodes(BasicType.t_i32);
				break;

			// arithmetic opEqual
			case AND_EQUAL: goto case;
			case ASHR_EQUAL: goto case;
			case MINUS_EQUAL: goto case;
			case OR_EQUAL: goto case;
			case PERCENT_EQUAL: goto case;
			case PLUS_EQUAL: goto case;
			case SHL_EQUAL: goto case;
			case SHR_EQUAL: goto case;
			case SLASH_EQUAL: goto case;
			case STAR_EQUAL: goto case;
			case XOR_EQUAL:
				resRype = context.basicTypeNodes(BasicType.t_i32);
				break;*/
			default:
				context.internal_error(b.loc, "Unimplemented op %s", b.op);
				assert(false);
		}
		b.type = resRype;
		b.type.assertImplemented(b.loc, context);
	}

	void calcType(BinaryExprNode* b)
	{
		assert(b.left.type, format("left(%s).type: is null", b.left.astType));
		assert(b.right.type, format("right(%s).type: is null", b.right.astType));

		setResultType(b);
	}

	void checkBodyForReturnType(FunctionDeclNode* f) {
		if (f.returnType.isVoid) return; // void functions don't need return at the end

		if (f.block_stmt.statements.length > 0)
		{
			AstNode* lastStmt = f.block_stmt.statements[$-1];
			if (lastStmt.astType == AstType.stmt_return)
				return; // return type is already checked
		}

		// is checked in IR gen
		//context.error(f.loc,
		//	"function `%s` has no return statement, but is expected to return a value of type %s",
		//	context.idString(f.id), f.returnType.typeName(context));
	}

	void visit(ModuleDeclNode* m) {
		u8Ptr = new PtrTypeNode(SourceLocation(), context.basicTypeNodes(BasicType.t_u8));
		u8Slice = new SliceTypeNode(SourceLocation(), context.basicTypeNodes(BasicType.t_u8));
		foreach (decl; m.declarations) _visit(decl);
	}
	void visit(FunctionDeclNode* f) {
		auto prevFunc = curFunc;
		curFunc = f;
		f.backendData.callingConvention = &win64_call_conv;
		foreach (param; f.parameters) visit(param);
		if (f.block_stmt)
		{
			visit(f.block_stmt);
			checkBodyForReturnType(f);
		}
		curFunc = prevFunc;
	}
	void visit(VariableDeclNode* v) {
		_visit(v.type);
		if (v.initializer) {
			_visit(v.initializer);
			autoconvTo(v.initializer, v.type);
		}

		switch (v.type.astType) with(AstType)
		{
			case type_static_array, type_struct, type_slice:
				v.varFlags |= VariableFlags.forceMemoryStorage;
				break;

			default: break;
		}
	}
	void visit(StructDeclNode* s) {
		foreach (decl; s.declarations) _visit(decl);
	}
	void visit(BlockStmtNode* b) {
		foreach(stmt; b.statements) _visit(stmt);
	}
	void visit(IfStmtNode* i) {
		_visit(i.condition);
		autoconvToBool(i.condition);
		_visit(i.thenStatement);
		if (i.elseStatement) {
			_visit(i.elseStatement);
		}
	}
	void visit(WhileStmtNode* w) {
		_visit(w.condition);
		autoconvToBool(w.condition);
		_visit(w.statement);
	}
	void visit(DoWhileStmtNode* d) {
		_visit(d.statement);
		_visit(d.condition);
		autoconvToBool(d.condition);
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
				autoconvTo(r.expression, curFunc.returnType);
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
		v.type = v.getSym.getType;
		v.type.assertImplemented(v.loc, context);
	}
	void visit(MemberExprNode* m) {
		_visit(m.aggregate);
		m.type.assertImplemented(m.loc, context);
	}
	void visit(IntLiteralExprNode* c) {
		if (c.value < 0)
		{
			if (cast(byte)(c.value & 0xFF) == c.value)
				c.type = context.basicTypeNodes(BasicType.t_i8);
			else if (cast(short)(c.value & 0xFFFF) == c.value)
				c.type = context.basicTypeNodes(BasicType.t_i16);
			else if (cast(int)(c.value & 0xFFFF_FFFF) == c.value)
				c.type = context.basicTypeNodes(BasicType.t_i32);
			else
				c.type = context.basicTypeNodes(BasicType.t_i64);
		}
		else
		{
			if (cast(ubyte)(c.value & 0xFF) == c.value)
				c.type = context.basicTypeNodes(BasicType.t_u8);
			else if (cast(ushort)(c.value & 0xFFFF) == c.value)
				c.type = context.basicTypeNodes(BasicType.t_u16);
			else if (cast(uint)(c.value & 0xFFFF_FFFF) == c.value)
				c.type = context.basicTypeNodes(BasicType.t_u32);
			else
				c.type = context.basicTypeNodes(BasicType.t_u64);
		}
	}
	void visit(StringLiteralExprNode* c) {
		c.type = cast(TypeNode*)u8Ptr;
	}
	void visit(BinaryExprNode* b) {
		_visit(b.left);
		_visit(b.right);
		calcType(b);
		b.type.assertImplemented(b.loc, context);
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
					case AstType.expr_name_use:
						(cast(NameUseExprNode*)u.child).getSym.varDecl.varFlags |= VariableFlags.isAddressTaken;
						break;
					default:
						context.internal_error("Cannot take address of %s", u.child.astType);
				}
				u.type = cast(TypeNode*) new PtrTypeNode(u.child.loc, u.child.type);
				break;
			default:
				context.internal_error("un op %s not implemented", u.op);
				u.type = u.child.type;
		}
	}
	// Get type from function declaration
	void visit(CallExprNode* c) {
		// TODO: support more than plain func() calls. Such as func_array[42](), (*func_ptr)() etc
		context.assertf(c.callee.astType == AstType.expr_name_use,
			c.loc, "Only direct function calls are supported right now");
		Symbol* calleeSym = (cast(NameUseExprNode*)c.callee).getSym;

		if (calleeSym.symClass != SymbolClass.c_function)
		{
			context.error(c.loc, "Cannot call %s", calleeSym.symClass);
			return;
		}

		VariableDeclNode*[] params = calleeSym.funcDecl.parameters;
		auto numParams = params.length;
		auto numArgs = c.args.length;

		if (numArgs < numParams)
			context.error(c.loc, "Insufficient parameters to '%s', got %s, expected %s",
				context.idString(calleeSym.id), numArgs, numParams);
		else if (numArgs > numParams)
			context.error(c.loc, "Too much parameters to '%s', got %s, expected %s",
				context.idString(calleeSym.id), numArgs, numParams);

		foreach (i, ExpressionNode* arg; c.args)
		{
			_visit(arg);
			autoconvTo(arg, params[i].type);
			//if (!sameType(arg.type, params[i].type))
			//	context.error(arg.loc,
			//		"Argument %s, must have type %s, not %s", i+1,
			//			params[i].type.printer(context),
			//			arg.type.printer(context));
		}
		c.type = calleeSym.getType;
	}
	void visit(IndexExprNode* i) {
		_visit(i.array);
		_visit(i.index);
		autoconvTo(i.index, BasicType.t_i64, No.force);
		i.type = i.array.type.getElementType(context);
	}
	void visit(TypeConvExprNode* t) {
		_visit(t.expr);
		if (!isConvertibleTo(t.expr.type, t.type))
		{
			context.error(t.loc,
				"Cannot auto-convert expression of type `%s` to `%s`",
				t.expr.type.printer(context),
				t.type.printer(context));
		}
		t.type.assertImplemented(t.loc, context);
	}
	void visit(BasicTypeNode* t) {}
	void visit(PtrTypeNode* t) {}
	void visit(StaticArrayTypeNode* t) {}
	void visit(SliceTypeNode* t) {}
	void visit(StructTypeNode* t) {}
}
