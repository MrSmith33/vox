/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.ast.dump;

import std.stdio;
import std.range : repeat;

import all;

struct AstPrinter {
	mixin AstVisitorMixin;

	CompilationContext* context;
	int indentSize = 1;

	private int indent;

	void print(Args...)(Args args) {
		auto i = ' '.repeat(indent);
		writeln(i, args);
	}

	void pr_node(AstNode* node) { // print node
		indent += indentSize; _visit(node); indent -= indentSize;
	}

	void visit(ModuleDeclNode* m) {
		print("MODULE");
		foreach (decl; m.declarations) pr_node(decl);
	}
	void visit(ImportDeclNode* i) {
		print("IMPORT ", context.idString(i.id));
	}
	void visit(FunctionDeclNode* f) {
		print("FUNC ", f.returnType.printer(context), " ", f.strId(context));
		foreach (param; f.parameters) pr_node(cast(AstNode*)param);
		if (f.block_stmt) pr_node(cast(AstNode*)f.block_stmt);
	}
	void visit(VariableDeclNode* v) {
		print(v.isParameter ? "PARAM " : "VAR ", v.type.printer(context), " ", v.strId(context));
		if (v.initializer) pr_node(cast(AstNode*)v.initializer);
	}
	void visit(StructDeclNode* s) {
		print("STRUCT ", s.strId(context));
		foreach (decl; s.declarations) pr_node(decl); }
	void visit(EnumDeclaration* e) {
		if (e.isAnonymous)
			print("ENUM ", e.memberType.printer(context));
		else
			print("ENUM ", e.memberType.printer(context), " ", e.strId(context));
		foreach (decl; e.declarations) pr_node(decl);
	}
	void visit(EnumMemberDecl* m) {
		print("ENUM MEMBER ", m.type.printer(context), " ", m.strId(context));
		if (m.initializer) pr_node(cast(AstNode*)m.initializer);
	}
	void visit(BlockStmtNode* b) {
		print("BLOCK");
		foreach(stmt; b.statements) pr_node(stmt); }
	void visit(IfStmtNode* i) {
		print("IF"); pr_node(cast(AstNode*)i.condition);
		print("THEN"); pr_node(i.thenStatement);
		if (i.elseStatement) { print("ELSE"); pr_node(i.elseStatement); }
	}
	void visit(WhileStmtNode* w) {
		print("WHILE");
		pr_node(cast(AstNode*)w.condition);
		pr_node(cast(AstNode*)w.statement); }
	void visit(DoWhileStmtNode* d) {
		print("DO");
		pr_node(cast(AstNode*)d.condition);
		print("WHILE");
		pr_node(cast(AstNode*)d.statement); }
	void visit(ReturnStmtNode* r) {
		print("RETURN");
		if (r.expression) pr_node(cast(AstNode*)r.expression); }
	void visit(BreakStmtNode* r) { print("BREAK"); }
	void visit(ContinueStmtNode* r) { print("CONTINUE"); }
	void visit(NameUseExprNode* v) {
		if (v.isSymResolved)
			print("VAR_USE ", v.getSym.getType.printer(context), " ", v.strId(context));
		else
			print("VAR_USE ", v.strId(context));
	}
	void visit(MemberExprNode* m) {
		print("MEMBER ", m.type.printer(context));
		pr_node(cast(AstNode*)m.aggregate);
		pr_node(cast(AstNode*)m.member);
	}
	void visit(IntLiteralExprNode* lit) { print("Int LITERAL ", lit.type.printer(context), " ", lit.value); }
	void visit(StringLiteralExprNode* lit) { print("String LITERAL ", lit.type.printer(context), " ", lit.value); }
	void visit(BinaryExprNode* b) {
		if (b.type) print("BINOP ", b.type.printer(context), " ", b.op);
		else print("BINOP ", b.op);
		pr_node(cast(AstNode*)b.left);
		pr_node(cast(AstNode*)b.right); }
	void visit(UnaryExprNode* u) {
		if (u.type) print("UNOP ", u.type.printer(context), " ", u.op);
		else print("UNOP ", u.op);
		pr_node(cast(AstNode*)u.child); }
	void visit(CallExprNode* c) {
		print("CALL");
		pr_node(cast(AstNode*)c.callee);
		foreach (arg; c.args) pr_node(cast(AstNode*)arg); }
	void visit(IndexExprNode* i) {
		print("INDEX"); pr_node(cast(AstNode*)i.array); pr_node(cast(AstNode*)i.index); }
	void visit(TypeConvExprNode* t) {
		print("CAST to ", t.type.printer(context));
		pr_node(cast(AstNode*)t.expr); }
	void visit(BasicTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }
	void visit(PtrTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }
	void visit(StaticArrayTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }
	void visit(SliceTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }
	void visit(StructTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }

	void printAst(AstNode* n)
	{
		indent = -indentSize;
		if (!n) return;
		pr_node(n);
	}
}

struct AstDotPrinter {
	mixin AstVisitorMixin;

	CompilationContext* context;
	int indentSize = 1;

	private int indent;

	void printLabel(N, Args...)(N* node, string format, Args args) {
		writef(`  node_%s [label="`, cast(void*)node);
		writef(format, args);
		writeln(`"];`);
	}

	void pr_node_edge(N1, N2)(N1* parent, N2* node) { // print node
		writeln(`  node_`, cast(void*)parent, ` -> node_`, cast(void*)node);
		_visit(cast(AstNode*)node);
	}

	void visit(ModuleDeclNode* m) {
		printLabel(m, "Module");
		foreach (decl; m.declarations) pr_node_edge(m, decl);
	}
	void visit(ImportDeclNode* i) {
		printLabel(i, `IMPORT\n%s`, context.idString(i.id));
	}
	void visit(FunctionDeclNode* f) {
		printLabel(f, `FUNC\n%s %s`, f.returnType.printer(context), f.strId(context));
		foreach (param; f.parameters) pr_node_edge(f, param);
		if (f.block_stmt) pr_node_edge(f, f.block_stmt);
	}
	void visit(VariableDeclNode* v) {
		printLabel(v, v.isParameter ? `PARAM\n%s %s` : `VAR\n%s %s`, v.type.printer(context), v.strId(context));
		if (v.initializer) pr_node_edge(v, v.initializer);
	}
	void visit(StructDeclNode* s) {
		printLabel(s, `STRUCT\n%s`, s.strId(context));
		foreach (decl; s.declarations) pr_node_edge(s, decl); }
	void visit(EnumDeclaration* e) {
		if (e.isAnonymous)
			printLabel(e, `ENUM\n%s`, e.memberType.printer(context));
		else
			printLabel(e, `ENUM\n%s %s`, e.memberType.printer(context), e.strId(context));
		foreach (decl; e.declarations) pr_node_edge(e, decl);
	}
	void visit(EnumMemberDecl* m) {
		printLabel(m, `ENUM MEMBER\n%s %s`, m.type.printer(context), m.strId(context));
		if (m.initializer) pr_node_edge(m, m.initializer);
	}
	void visit(BlockStmtNode* b) {
		printLabel(b, "BLOCK");
		foreach(stmt; b.statements) pr_node_edge(b, stmt); }
	void visit(IfStmtNode* i) {
		printLabel(i, "IF"); pr_node_edge(i, i.condition);
		pr_node_edge(i, i.thenStatement);
		if (i.elseStatement) { pr_node_edge(i, i.elseStatement); }
	}
	void visit(WhileStmtNode* w) {
		printLabel(w, "WHILE");
		pr_node_edge(w, w.condition);
		pr_node_edge(w, w.statement); }
	void visit(DoWhileStmtNode* d) {
		printLabel(d, "DO");
		pr_node_edge(d, d.condition);
		pr_node_edge(d, d.statement); }
	void visit(ReturnStmtNode* r) {
		printLabel(r, "RETURN");
		if (r.expression) pr_node_edge(r, r.expression); }
	void visit(BreakStmtNode* r) { printLabel(r, "BREAK"); }
	void visit(ContinueStmtNode* r) { printLabel(r, "CONTINUE"); }
	void visit(NameUseExprNode* v) {
		if (v.isSymResolved)
			printLabel(v, `VAR_USE\n%s %s`, v.getSym.getType.printer(context), v.strId(context));
		else
			printLabel(v, `VAR_USE\n%s`, v.strId(context));
	}
	void visit(MemberExprNode* m) {
		printLabel(m, "MEMBER %s", m.member.strId(context));
		pr_node_edge(m, m.aggregate);
	}
	void visit(IntLiteralExprNode* lit) { printLabel(lit, `Int LITERAL\n%s %s`, lit.type.printer(context), lit.value); }
	void visit(StringLiteralExprNode* lit) { printLabel(lit, `String LITERAL\n%s %s`, lit.type.printer(context), lit.value); }
	void visit(BinaryExprNode* b) {
		if (b.type) printLabel(b, `BINOP\n%s %s`, b.type.printer(context), b.op);
		else printLabel(b, `BINOP\n%s`, b.op);
		pr_node_edge(b, b.left);
		pr_node_edge(b, b.right); }
	void visit(UnaryExprNode* u) {
		if (u.type) printLabel(u, `UNOP\n%s %s`, u.type.printer(context), u.op);
		else printLabel(u, `UNOP\n%s`, u.op);
		pr_node_edge(u, u.child); }
	void visit(CallExprNode* c) {
		printLabel(c, `CALL`);
		pr_node_edge(c, c.callee);
		foreach (arg; c.args) pr_node_edge(c, arg); }
	void visit(IndexExprNode* i) {
		printLabel(i, `INDEX`); pr_node_edge(i, i.array); pr_node_edge(i, i.index); }
	void visit(TypeConvExprNode* t) {
		printLabel(t, `CAST\n%s`, t.type.printer(context));
		pr_node_edge(t, t.expr); }
	void visit(BasicTypeNode* t) { printLabel(t, `TYPE\n%s`, t.typeNode.printer(context)); }
	void visit(PtrTypeNode* t) { printLabel(t, `TYPE\n%s`, t.typeNode.printer(context)); }
	void visit(StaticArrayTypeNode* t) { printLabel(t, `TYPE\n%s`, t.typeNode.printer(context)); }
	void visit(SliceTypeNode* t) { printLabel(t, `TYPE\n%s`, t.typeNode.printer(context)); }
	void visit(StructTypeNode* t) { printLabel(t, `TYPE\n%s`, t.typeNode.printer(context)); }

	void printAst(AstNode* n)
	{
		writeln(`digraph AST {`);
		if (n) _visit(n);
		writeln(`}`);
	}
}
