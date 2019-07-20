/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.names_resolve;

import std.stdio;
import std.string : format;
import all;

void pass_names_resolve(ref CompilationContext ctx, CompilePassPerModule[] subPasses)
{
	auto sem2 = PassNamesResolve(&ctx);
	foreach (ref SourceFileInfo file; ctx.files.data) {
		sem2.visit(file.mod);
	}
}

/// Error means that lookup failed due to earlier failure or error, so no new error should be produced
enum LookupResult : ubyte {
	success,
	failure,
	error
}

/// Resolves all symbol references (variable/type/function uses)
/// using information collected on previous pass
struct PassNamesResolve
{
	mixin AstVisitorMixin;

	CompilationContext* context;

	Scope* currentScope;

	void pushCompleteScope(Scope* newScope)
	{
		currentScope = newScope;
	}

	void popScope()
	{
		assert(currentScope);

		if (currentScope.parentScope)
			currentScope = currentScope.parentScope;
		else
			currentScope = null;
	}

	/// Look up symbol by Identifier. Searches the whole stack of scopes.
	AstNode* lookup(const Identifier id, TokenIndex from)
	{
		Scope* sc = currentScope;

		// first phase
		while(sc)
		{
			AstNode* sym = sc.symbols.get(id, null);

			if (sym)
			{
				// forward reference allowed for unordered scope
				if (!sym.isInOrderedScope) {
					return sym;
				} else { // ordered scope
					// we need to skip forward references in ordered scope
					uint fromStart = context.tokenLocationBuffer[from].start;
					uint toStart = context.tokenLocationBuffer[sym.loc].start;
					// backward reference
					if (fromStart > toStart) {
						return sym;
					}
				}
			}

			sc = sc.parentScope;
		}

		// second phase
		AstNode* sym = lookupImports(id, from);

		if (sym) {
			return sym;
		} else {
			context.error(from, "undefined identifier `%s`", context.idString(id));
			return cast(AstNode*)&context.errorNode;
		}
	}

	AstNode* lookupImports(const Identifier id, TokenIndex from)
	{
		Scope* sc = currentScope;
		while (sc)
		{
			AstNode* sym;
			ModuleDeclNode* symMod;

			foreach (ModuleDeclNode* imp; sc.imports)
			{
				// TODO: check that import is higher in ordered scopes
				AstNode* scopeSym = imp._scope.symbols.get(id, null);
				if (!scopeSym) continue;

				if (scopeSym && sym && scopeSym != sym)
				{
					string mod1Id = context.idString(symMod.id);
					string sym1Id = context.idString(sym.get_node_id);

					string mod2Id = context.idString(imp.id);
					string sym2Id = context.idString(scopeSym.get_node_id);

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

	void visit(ModuleDeclNode* m) {
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
	void visit(ForStmtNode* n) {
		pushCompleteScope(n._scope);
		foreach(stmt; n.init_statements) _visit(stmt);
		if (n.condition) _visit(n.condition);
		foreach(stmt; n.increment_statements) _visit(stmt);
		_visit(n.statement);
		popScope;
	}
	void visit(ReturnStmtNode* r) {
		if (r.expression) _visit(r.expression);
	}
	void visit(BreakStmtNode* r) {}
	void visit(ContinueStmtNode* r) {}
	void visit(NameUseExprNode* v) {
		v.resolve = lookup(v.id, v.loc);
		if (v.entity !is null)
		{
			switch(v.entity.astType) with(AstType) {
				case decl_function:
					v.astType = expr_func_name_use; break;
				case decl_var:
					v.astType = expr_var_name_use; break;
				case decl_struct:
					v.astType = expr_type_name_use; break;
				case decl_enum_member:
					v.astType = expr_var_name_use; break;
				case decl_enum:
					v.astType = expr_type_name_use; break;
				case error:
					v.astType = expr_var_name_use; break;
				default:
					context.internal_error("Unknown entity %s", v.entity.astType);
			}
		}
	}
	void visit(MemberExprNode* m) {
		_visit(m.aggregate);
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
}
