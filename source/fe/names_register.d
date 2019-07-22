/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.names_register;

import std.stdio;
import std.string : format;
import std.typecons : Flag, Yes, No;
import all;


void pass_names_register(ref CompilationContext ctx, CompilePassPerModule[] subPasses)
{
	auto sem1 = PassNamesRegister(&ctx);
	foreach (ref SourceFileInfo file; ctx.files.data) {
		sem1.visit(file.mod);
	}
}

/// Register identifiers in scope tree
struct PassNamesRegister
{
	mixin AstVisitorMixin;

	CompilationContext* context;
	Scope* currentScope;
	ModuleDeclNode* mod;

	Scope* pushScope(string name, Flag!"ordered" isOrdered)
	{
		Scope* newScope = context.appendAst!Scope;
		newScope.debugName = name;
		newScope.isOrdered = isOrdered;

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
	void insert(Identifier id, AstNode* node)
	{
		node.flags |= currentScope.isOrdered ? AstFlags.isInOrderedScope : 0;
		if (auto s = currentScope.symbols.get(id, null))
		{
			context.error(node.loc,
				"declaration `%s` is already defined at %s", context.idString(id), FmtSrcLoc(s.loc, context));
		}
		currentScope.symbols.put(context.arrayArena, id, node);
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
		i.state = AstNodeState.name_resolve;
	}
	void visit(FunctionDeclNode* f) {
		mod.addFunction(context.arrayArena, f);
		insert(f.id, cast(AstNode*)f);
		f._scope = pushScope(context.idString(f.id), Yes.ordered);
		foreach (param; f.parameters) visit(param);
		if (f.block_stmt) visit(f.block_stmt);
		popScope;
	}
	void visit(VariableDeclNode* v) {
		insert(v.id, cast(AstNode*)v);
		if (v.initializer) _visit(v.initializer);
	}
	void visit(StructDeclNode* s) {
		insert(s.id, cast(AstNode*)s);
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
			insert(e.id, cast(AstNode*)e);
			e._scope = pushScope(context.idString(e.id), No.ordered);
			foreach (decl; e.declarations) _visit(decl);
			popScope;
		}
	}
	void visit(EnumMemberDecl* m) {
		insert(m.id, cast(AstNode*)m);
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
	void visit(ForStmtNode* n) {
		n._scope = pushScope("For", Yes.ordered);
		foreach(stmt; n.init_statements) _visit(stmt);
		if (n.condition) _visit(n.condition);
		foreach(stmt; n.increment_statements) _visit(stmt);
		_visit(n.statement);
		popScope;
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
	void visit(BinaryExprNode* b) {}
	void visit(UnaryExprNode* u) {}
	void visit(CallExprNode* c) {}
	void visit(IndexExprNode* i) {}
	void visit(TypeConvExprNode* c) {}
	void visit(BasicTypeNode* t) {}
	void visit(PtrTypeNode* t) {}
	void visit(StaticArrayTypeNode* t) {}
	void visit(SliceTypeNode* t) {}
}
