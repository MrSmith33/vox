/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module ast.visitor;

import all;

mixin template AstVisitorMixin() {
	void _visit(TypeNode* n) { _visit(cast(AstNode*)n); }
	void _visit(ExpressionNode* n) { _visit(cast(AstNode*)n); }
	void _visit(AstNode* n)
	{
		final switch(n.astType) with(AstType)
		{
			case error: context.internal_error(n.loc, "Visiting error node"); break;
			case abstract_node: context.internal_error(n.loc, "Visiting abstract node"); break;

			case decl_module: auto m = cast(ModuleDeclNode*)n; visit(m); break;
			case decl_function: auto f = cast(FunctionDeclNode*)n; visit(f); break;
			case decl_var: auto v = cast(VariableDeclNode*)n; visit(v); break;
			case decl_struct: auto s = cast(StructDeclNode*)n; visit(s); break;
			case decl_enum: auto e = cast(EnumDeclaration*)n; visit(e); break;
			case decl_enum_member: auto e = cast(EnumMemberDecl*)n; visit(e); break;

			case stmt_block: auto b = cast(BlockStmtNode*)n; visit(b); break;
			case stmt_if: auto i = cast(IfStmtNode*)n; visit(i); break;
			case stmt_while: auto w = cast(WhileStmtNode*)n; visit(w); break;
			case stmt_do_while: auto d = cast(DoWhileStmtNode*)n; visit(d); break;
			case stmt_return: auto r = cast(ReturnStmtNode*)n; visit(r); break;
			case stmt_break: auto b = cast(BreakStmtNode*)n; visit(b); break;
			case stmt_continue: auto c = cast(ContinueStmtNode*)n; visit(c); break;

			case expr_name_use: auto v = cast(NameUseExprNode*)n; visit(v); break;
			case expr_member: auto m = cast(MemberExprNode*)n; visit(m); break;
			case expr_bin_op: auto b = cast(BinaryExprNode*)n; visit(b); break;
			case expr_un_op: auto u = cast(UnaryExprNode*)n; visit(u); break;
			case expr_call: auto c = cast(CallExprNode*)n; visit(c); break;
			case expr_index: auto i = cast(IndexExprNode*)n; visit(i); break;
			case expr_type_conv: auto t = cast(TypeConvExprNode*)n; visit(t); break;

			case literal_int: auto l = cast(IntLiteralExprNode*)n; visit(l); break;
			case literal_string: auto l = cast(StringLiteralExprNode*)n; visit(l); break;

			case type_basic: auto t = cast(BasicTypeNode*)n; visit(t); break;
			case type_ptr: auto t = cast(PtrTypeNode*)n; visit(t); break;
			case type_static_array: auto t = cast(StaticArrayTypeNode*)n; visit(t); break;
			case type_slice: auto t = cast(SliceTypeNode*)n; visit(t); break;
			case type_struct: auto t = cast(StructTypeNode*)n; visit(t); break;
		}
	}
}

/*	// Visitor code
	void visit(ModuleDeclNode* m) {
		foreach (decl; m.declarations) _visit(decl); }
	void visit(FunctionDeclNode* f) {
		foreach (param; f.parameters) visit(param);
		if (f.block_stmt) visit(f.block_stmt); }
	void visit(VariableDeclNode* v) {}
	void visit(StructDeclNode* s) {
		foreach (decl; s.declarations) _visit(decl); }
	void visit(EnumDeclaration* e) {
		foreach (decl; e.declarations) _visit(decl); }
	void visit(EnumMemberDecl* m) {}
	void visit(BlockStmtNode* b) {
		foreach (stmt; b.statements) _visit(stmt); }
	void visit(IfStmtNode* i) {
		_visit(cast(AstNode*)i.condition);
		_visit(cast(AstNode*)i.thenStatement);
		if (i.elseStatement) _visit(i.elseStatement); }
	void visit(WhileStmtNode* w) {
		_visit(cast(AstNode*)w.condition);
		_visit(cast(AstNode*)w.statement); }
	void visit(DoWhileStmtNode* d) {
		_visit(cast(AstNode*)d.condition);
		_visit(cast(AstNode*)d.statement); }
	void visit(ReturnStmtNode* r) {
		if (r.expression) _visit(r.expression); }
	void visit(BreakStmtNode* r) {}
	void visit(ContinueStmtNode* r) {}
	void visit(AssignStmtNode* a) {
		_visit(a.left); _visit(a.right); }
	void visit(NameUseExprNode* v) {}
	void visit(LiteralExprNode* c) {}
	void visit(BinaryExprNode* b) {
		_visit(cast(AstNode*)b.left);
		_visit(cast(AstNode*)b.right); }
	void visit(UnaryExprNode* u) {
		_visit(cast(AstNode*)u.child); }
	void visit(CallExprNode* c) {
		foreach (arg; c.args) _visit(arg); }
	void visit(IndexExprNode* i) {
		_visit(i.array); _visit(i.index); }
	void visit(TypeConvExprNode* t) {
		_visit(t.type); _visit(t.expr); }
	void visit(BasicTypeNode* t) {}
	void visit(PtrTypeNode* t) {}
	void visit(StaticArrayTypeNode* t) {}
	void visit(SliceTypeNode* t) {}
	void visit(StructTypeNode* t) {}
*/
