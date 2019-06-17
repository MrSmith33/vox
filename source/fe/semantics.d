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

///
struct Scope
{
	///
	HashMap!(Identifier, Symbol*, Identifier.max) symbols;
	/// Imported modules
	Array!(ModuleDeclNode*) imports;
	///
	Scope* parentScope;
	///
	string debugName;
	/// Ordered scope is in function body, requires declaration above use
	/// Unordered scope is in struct, module
	bool isOrdered;
}

void pass_semantic_decl(ref CompilationContext ctx, CompilePassPerModule[] subPasses)
{
	auto sem1 = SemanticDeclarations(&ctx);
	foreach (ref SourceFileInfo file; ctx.files.data) {
		sem1.visit(file.mod);
	}
}

/// Register identifiers in scope tree
struct SemanticDeclarations
{
	mixin AstVisitorMixin;

	CompilationContext* context;
	Scope* currentScope;
	ModuleDeclNode* mod;

	Scope* pushScope(string name, Flag!"ordered" isOrdered)
	{
		Scope* newScope = context.appendAst!Scope;
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
	Symbol* insert(Identifier id, TokenIndex loc, SymbolClass symClass, AstNode* node)
	{
		typeof(Symbol.flags) flags = currentScope.isOrdered ? SymbolFlags.isInOrderedScope : 0;
		Symbol* sym = context.appendAst!Symbol(id, loc, symClass, flags, node);
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
		currentScope.symbols.put(context.arrayArena, sym.id, sym);
	}

	void visit(ModuleDeclNode* m) {
		mod = m;
		mod._scope = pushScope("Module", No.ordered);
		foreach (decl; mod.declarations) _visit(decl);
		popScope;
	}
	void visit(ImportDeclNode* i) {
		ModuleDeclNode* m = context.findModule(i.id);
		currentScope.imports.put(context.arrayArena, m);
		if (m is null)
			context.error(i.loc, "Cannot find module `%s`", context.idString(i.id));
	}
	void visit(FunctionDeclNode* f) {
		mod.addFunction(context.arrayArena, f);
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
	void visit(EnumDeclaration* e) {
		if (e.isAnonymous)
		{
			foreach (decl; e.declarations) _visit(decl);
		}
		else
		{
			e.resolveSymbol = insert(e.id, e.loc, SymbolClass.c_enum, cast(AstNode*)e);
			e._scope = pushScope(context.idString(e.id), No.ordered);
			foreach (decl; e.declarations) _visit(decl);
			popScope;
		}
	}
	void visit(EnumMemberDecl* m) {
		m.resolveSymbol = insert(m.id, m.loc, SymbolClass.c_enum_member, cast(AstNode*)m);
		if (m.initializer) _visit(m.initializer);
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
	void visit(NullLiteralExprNode* c) {}
	void visit(BoolLiteralExprNode* c) {}
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

void pass_semantic_lookup(ref CompilationContext ctx, CompilePassPerModule[] subPasses)
{
	auto sem2 = SemanticLookup(&ctx);
	foreach (ref SourceFileInfo file; ctx.files.data) {
		sem2.visit(file.mod);
	}
	sem2.symbols.free(ctx.arrayArena);
}

/// Error means that lookup failed due to earlier failure or error, so no new error should be produced
enum LookupResult : ubyte {
	success,
	failure,
	error
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
	HashMap!(Identifier, Symbol*, Identifier.max) symbols;

	Scope* currentScope;

	Identifier id_ptr;
	Identifier id_length;

	void pushCompleteScope(Scope* newScope)
	{
		currentScope = newScope;
		foreach (id, sym; newScope.symbols)
		{
			if (auto outerSymbol = symbols.get(sym.id, null))
				sym.outerSymbol = outerSymbol;
			symbols.put(context.arrayArena, id, sym);
		}
	}

	void popScope()
	{
		assert(currentScope);

		// Pop all symbols of the scope we are leaving from symbols
		foreach(id, sym; currentScope.symbols)
		{
			if (sym.outerSymbol) // replace by symbol from outer scope
				symbols.put(context.arrayArena, id, sym.outerSymbol);
			else // or simply remove it if no such symbol
				symbols.remove(context.arrayArena, id);
		}

		if (currentScope.parentScope)
			currentScope = currentScope.parentScope;
		else
			currentScope = null;
	}

	/// Look up symbol by Identifier. Searches the whole stack of scopes.
	Symbol* lookup(const Identifier id, TokenIndex from)
	{
		Symbol* sym = symbols.get(id, null);
		// first phase
		while (sym)
		{
			// forward reference allowed for unordered scope
			if (!sym.isInOrderedScope) { break; }
			// ordered scope
			else
			{
				uint fromStart = context.tokenLocationBuffer[from].start;
				uint toStart = context.tokenLocationBuffer[sym.loc].start;
				if (fromStart > toStart) { break; }
			}

			sym = sym.outerSymbol;
		}

		if (sym) return sym;

		// second phase
		sym = lookupImports(id, from);

		if (sym) {
			// exists
		}
		else
		{
			context.error(from, "undefined identifier `%s`", context.idString(id));
		}
		return sym;
	}

	Symbol* lookupImports(const Identifier id, TokenIndex from)
	{
		Scope* sc = currentScope;
		while (sc)
		{
			Symbol* sym;
			ModuleDeclNode* symMod;

			foreach (ModuleDeclNode* imp; sc.imports)
			{
				// TODO: check that import is higher in ordered scopes
				Symbol* scopeSym = imp._scope.symbols.get(id, null);
				if (!scopeSym) continue;

				if (scopeSym && sym && scopeSym != sym)
				{
					string mod1Id = context.idString(symMod.id);
					string sym1Id = context.idString(sym.id);

					string mod2Id = context.idString(imp.id);
					string sym2Id = context.idString(scopeSym.id);

					context.error(from,
						"`%s.%s` at %s conflicts with `%s.%s` at %s",
						mod1Id, sym1Id, FmtSrcLoc(sym.loc, context),
						mod2Id, sym2Id, FmtSrcLoc(scopeSym.loc, context));
				}

				sym = scopeSym;
				symMod = imp;
			}

			if (sym) return sym;

			sc = sc.parentScope;
		}
		return null;
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

		// skip unresolved symbol
		if (!expr.aggregate.isSymResolved) return;

		Symbol* aggSym = (cast(NameUseExprNode*)expr.aggregate).getSym;

		switch(aggSym.symClass)
		{
			case SymbolClass.c_variable:
				LookupResult success;
				VariableDeclNode* varDecl = aggSym.varDecl;

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
				if (success == LookupResult.failure)
					context.error(expr.loc, "`%s` of type `%s` has no member `%s`",
						varDecl.strId(context), varDecl.type.printer(context), idStr);
				break;

			case SymbolClass.c_enum:
				EnumDeclaration* enumDecl = aggSym.enumDecl;
				context.assertf(!enumDecl.isAnonymous, expr.loc,
					"Trying to get member from anonymous enum defined at %s",
					context.tokenLoc(enumDecl.loc));

				Symbol* memberSym = enumDecl._scope.symbols.get(id, null);
				if (memberSym is null)
				{
					context.error(expr.loc, "Enum `%s` has no member `%s`",
						enumDecl.strId(context), idStr);
					break;
				}

				context.assertf(memberSym.symClass == SymbolClass.c_enum_member, expr.loc,
					"Unexpected enum member %s", memberSym.symClass);

				expr.member.resolveSymbol(memberSym);

				EnumMemberDecl* enumMember = memberSym.enumMember;
				expr.type = enumMember.type;
				expr.memberIndex = enumMember.scopeIndex;
				break;

			default:
				context.error(expr.loc, "Cannot resolve `%s` for %s", idStr, aggSym.symClass);
				return;
		}
	}

	LookupResult lookupSliceMember(MemberExprNode* expr, SliceTypeNode* sliceType, Identifier id)
	{
		if (id == id_ptr)
		{
			expr.memberIndex = 1;
			expr.type = cast(TypeNode*) context.appendAst!PtrTypeNode(sliceType.loc, sliceType.base);
			return LookupResult.success;
		}
		else if (id == id_length)
		{
			expr.memberIndex = 0;
			expr.type = context.basicTypeNodes(BasicType.t_u64);
			return LookupResult.success;
		}

		return LookupResult.failure;
	}

	LookupResult lookupStructMember(MemberExprNode* expr, StructTypeNode* structType, Identifier id)
	{
		// skip unresolved symbol
		if (!structType.isSymResolved) return LookupResult.error;

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
					// skip unresolved symbol
					if (!expr.member.isSymResolved) return LookupResult.error;

					VariableDeclNode* memberVar = expr.member.getSym.varDecl;
					expr.type = memberVar.type;
					expr.memberIndex = memberVar.scopeIndex;
					return LookupResult.success;

				case SymbolClass.c_struct:
					context.internal_error("member structs are not implemented");
					assert(false);

				case SymbolClass.c_enum:
					context.internal_error("member enums are not implemented");
					assert(false);

				case SymbolClass.c_enum_member:
					context.internal_error("enums member are not implemented");
					assert(false);
			}
		}

		return LookupResult.failure;
	}

	void visit(ModuleDeclNode* m) {
		id_ptr = context.idMap.getOrRegNoDup("ptr");
		id_length = context.idMap.getOrRegNoDup("length");

		pushCompleteScope(m._scope);
		foreach (decl; m.declarations) _visit(decl);
		popScope;
	}
	void visit(ImportDeclNode* i) {}
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
	void visit(EnumDeclaration* e) {
		if (e.isAnonymous)
		{
			foreach (decl; e.declarations) _visit(decl);
		}
		else
		{
			pushCompleteScope(e._scope);
			foreach (decl; e.declarations) _visit(decl);
			popScope;
		}
	}
	void visit(EnumMemberDecl* m) {
		_visit(m.type);
		if (m.initializer) _visit(m.initializer);
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
	void visit(NullLiteralExprNode* c) {}
	void visit(BoolLiteralExprNode* c) {}
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

void pass_semantic_type(ref CompilationContext ctx, CompilePassPerModule[] subPasses)
{
	auto sem3 = SemanticStaticTypes(&ctx);
	foreach (ref SourceFileInfo file; ctx.files.data) {
		sem3.visit(file.mod);

		if (ctx.printAstSema && file.mod !is null) {
			auto astPrinter = AstPrinter(&ctx, 2);
			writefln("// AST sema `%s`", file.name);
			astPrinter.printAst(cast(AstNode*)file.mod);
		}
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

	void autoconvToBool(ref ExpressionNode* expr)
	{
		if (!autoconvTo(expr, BasicType.t_bool, No.force))
			context.error(expr.loc, "Cannot implicitly convert `%s` to bool",
				expr.type.typeName(context));
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
				expr = cast(ExpressionNode*) context.appendAst!TypeConvExprNode(expr.loc, type, IrIndex(), expr);
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
			if (type.astType == AstType.type_slice &&
				type.sliceTypeNode.base.astType == AstType.type_basic &&
				type.sliceTypeNode.base.basicTypeNode.basicType == BasicType.t_u8)
			{
				return true;
			}
		}
		else if (expr.astType == AstType.literal_null && type.isPointer) {
			expr.type = type;
			return true;
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
		if (b.left.type.isError || b.right.type.isError) return;

		switch(b.op) with(BinOp)
		{
			// logic ops. Requires both operands to be bool
			case LOGIC_AND, LOGIC_OR:
				autoconvToBool(b.left);
				autoconvToBool(b.right);
				resRype = context.basicTypeNodes(BasicType.t_bool);
				break;
			// logic ops. Requires both operands to be of the same type
			case EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL:
				if (b.left.type.isPointer && b.right.type.isPointer)
				{
					if (
						sameType(b.left.type, b.right.type) ||
						b.left.type.ptrTypeNode.isVoidPtr ||
						b.right.type.ptrTypeNode.isVoidPtr)
					{
						resRype = context.basicTypeNodes(BasicType.t_bool);
						break;
					}
				}

				if (autoconvToCommonType(b.left, b.right))
					resRype = context.basicTypeNodes(BasicType.t_bool);
				else
					context.error(b.left.loc, "Cannot compare `%s` and `%s`",
						b.left.type.typeName(context),
						b.right.type.typeName(context));
				break;

			case MINUS:
				if (b.left.type.isPointer && b.right.type.isPointer) // handle ptr - ptr
				{
					if (sameType(b.left.type, b.right.type))
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
				if (autoconvToCommonType(b.left, b.right))
					resRype = b.left.type;
				else
				{
					context.error(b.left.loc, "Cannot perform `%s` %s `%s` operation",
						b.left.type.typeName(context), b.op,
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
				if (b.left.astType == AstType.expr_name_use ||
					b.left.astType == AstType.expr_index ||
					b.left.astType == AstType.expr_member ||
					(b.left.astType == AstType.expr_un_op && (cast(UnaryExprNode*)b.left).op == UnOp.deref))
				{
					autoconvTo(b.right, b.left.type);
				}
				else
					context.error(b.left.loc, "Cannot perform assignment into %s", b.left.astType);
				resRype = context.basicTypeNodes(BasicType.t_void);
				break;

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

		if (!f.block_stmt.statements.empty)
		{
			AstNode* lastStmt = f.block_stmt.statements.back;
			if (lastStmt.astType == AstType.stmt_return)
				return; // return type is already checked
		}

		// is checked in IR gen
		//context.error(f.loc,
		//	"function `%s` has no return statement, but is expected to return a value of type %s",
		//	context.idString(f.id), f.returnType.typeName(context));
	}

	void visit(ModuleDeclNode* m) {
		u8Ptr = context.appendAst!PtrTypeNode(TokenIndex(), context.basicTypeNodes(BasicType.t_u8));
		u8Slice = context.appendAst!SliceTypeNode(TokenIndex(), context.basicTypeNodes(BasicType.t_u8));
		foreach (decl; m.declarations) _visit(decl);
	}
	void visit(ImportDeclNode* i) {}
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

		if (!v.isParameter)
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
			autoconvTo(m.initializer, m.type);
		}
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
		if (c.isSigned)
			c.type = context.basicTypeNodes(minSignedIntType(c.value));
		else
			c.type = context.basicTypeNodes(minUnsignedIntType(c.value));
	}
	void visit(StringLiteralExprNode* c) {
		c.type = cast(TypeNode*)u8Ptr;
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
				u.type = cast(TypeNode*) context.appendAst!PtrTypeNode(u.child.loc, u.child.type);
				break;
			case bitwiseNot:
				u.type = u.child.type;
				break;
			case logicalNot:
				autoconvToBool(u.child);
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
				u.type = u.child.type.ptrTypeNode.base;
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

		switch (calleeSym.symClass)
		{
			case SymbolClass.c_function: return visitCall(c, calleeSym.funcDecl);
			case SymbolClass.c_struct: return visitConstructor(c, calleeSym.structDecl);
			default:
				c.type = context.basicTypeNodes(BasicType.t_error);
				context.error(c.loc, "Cannot call %s", calleeSym.symClass);
		}

	}
	void visitCall(CallExprNode* c, FunctionDeclNode* funcDecl) {
		Array!(VariableDeclNode*) params = funcDecl.parameters;
		auto numParams = params.length;
		auto numArgs = c.args.length;

		if (numArgs < numParams)
			context.error(c.loc, "Insufficient parameters to '%s', got %s, expected %s",
				context.idString(funcDecl.backendData.name), numArgs, numParams);
		else if (numArgs > numParams)
			context.error(c.loc, "Too much parameters to '%s', got %s, expected %s",
				context.idString(funcDecl.backendData.name), numArgs, numParams);

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
		c.type = funcDecl.returnType;
	}
	void visitConstructor(CallExprNode* c, StructDeclNode* s) {
		size_t numStructMembers;
		foreach(AstNode* member; s.declarations)
		{
			if (member.astType == AstType.decl_var)
			{
				ExpressionNode* initializer;
				if (c.args.length > numStructMembers) { // init from constructor argument
					initializer = c.args[numStructMembers];
				} else { // init with initializer from struct definition

				}
				++numStructMembers;
			}
		}
		context.unrecoverable_error(c.loc, "visitConstructor not impl");
	}
	void visit(IndexExprNode* i) {
		_visit(i.array);
		_visit(i.index);
		autoconvTo(i.index, BasicType.t_i64, No.force);
		switch (i.array.type.astType) with(AstType)
		{
			case type_ptr, type_static_array, type_slice: break; // valid
			default: context.internal_error("Cannot index value of type `%s`", i.array.type.printer(context));
		}
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
