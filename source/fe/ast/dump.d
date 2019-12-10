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

	void pr_node(AstIndex nodeIndex) { // print node
		AstNode* node = context.getAstNode(nodeIndex);
		indent += indentSize; _visit(node); indent -= indentSize;
	}
	void pr_nodes(AstNodes nodes) { // print node
		indent += indentSize;
		foreach (AstIndex nodeIndex; nodes) {
			AstNode* node = context.getAstNode(nodeIndex);
			_visit(node);
		}
		indent -= indentSize;
	}

	void visit(AliasDeclNode* n) {
		print("ALIAS ", context.idString(n.id));
		pr_node(n.initializer);
	}
	void visit(ModuleDeclNode* m) {
		print("MODULE ", context.files[m.moduleIndex.fileIndex].name);
		pr_nodes(m.declarations);
	}
	void visit(ImportDeclNode* i) {
		print("IMPORT ", context.idString(i.id));
	}
	void visit(FunctionDeclNode* f) {
		auto sig = f.signature.get!FunctionSignatureNode(context);
		print("FUNC ", sig.returnType.printer(context), " ", context.idString(f.id));
		pr_nodes(sig.parameters);
		if (f.block_stmt) pr_node(f.block_stmt);
	}
	void visit(VariableDeclNode* v) {
		print(v.isParameter ? "PARAM " : "VAR ", v.type.printer(context), " ", context.idString(v.id));
		if (v.initializer) pr_node(v.initializer);
	}
	void visit(StructDeclNode* s) {
		print("STRUCT ", context.idString(s.id));
		pr_nodes(s.declarations); }
	void visit(EnumDeclaration* e) {
		if (e.isAnonymous)
			print("ENUM ", e.memberType.printer(context));
		else
			print("ENUM ", e.memberType.printer(context), " ", context.idString(e.id));
		pr_nodes(e.declarations);
	}
	void visit(EnumMemberDecl* m) {
		print("ENUM MEMBER ", m.type.printer(context), " ", context.idString(m.id));
		if (m.initializer) pr_node(m.initializer);
	}
	void visit(StaticIfDeclNode* n) {
		print("#IF"); pr_node(n.condition);
		print("#THEN");
		pr_nodes(n.thenItems);
		print("#ELSE");
		pr_nodes(n.elseItems);
	}
	void visit(BlockStmtNode* b) {
		print("BLOCK");
		pr_nodes(b.statements); }
	void visit(IfStmtNode* i) {
		print("IF"); pr_node(i.condition);
		print("THEN"); pr_nodes(i.thenStatements);
		if (!i.elseStatements.empty) {
			print("ELSE"); pr_nodes(i.elseStatements);
		}
	}
	void visit(WhileStmtNode* w) {
		print("WHILE");
		pr_node(w.condition);
		pr_nodes(w.statements); }
	void visit(DoWhileStmtNode* d) {
		print("DO");
		pr_node(d.condition);
		print("WHILE");
		pr_nodes(d.statements); }
	void visit(ForStmtNode* n) {
		print("FOR");
		pr_nodes(n.init_statements);
		print("COND");
		if (n.condition) pr_node(n.condition);
		print("INC");
		pr_nodes(n.increment_statements);
		pr_nodes(n.body_statements); }
	void visit(ReturnStmtNode* r) {
		print("RETURN");
		if (r.expression) pr_node(r.expression); }
	void visit(BreakStmtNode* r) { print("BREAK"); }
	void visit(ContinueStmtNode* r) { print("CONTINUE"); }
	void visit(NameUseExprNode* v) {
		if (v.isSymResolved)
			print("NAME_USE ", v.type.printer(context), " ", context.idString(v.id(context)));
		else
			print("NAME_USE ", context.idString(v.id(context)));
	}
	void visit(MemberExprNode* m) {
		print("MEMBER ", m.type.printer(context), " ", context.idString(m.memberId(context)));
		pr_node(m.aggregate);
	}
	void visit(IntLiteralExprNode* lit) {
		if (lit.isSigned)
			print("Int LITERAL ", lit.type.printer(context), " ", cast(long)lit.value);
		else
			print("Int LITERAL ", lit.type.printer(context), " ", lit.value);
	}
	void visit(StringLiteralExprNode* lit) { print("String LITERAL ", lit.type.printer(context), " ", lit.value); }
	void visit(NullLiteralExprNode* lit) { print("null LITERAL"); }
	void visit(BoolLiteralExprNode* lit) {
		if (lit.value) print("TRUE ", lit.type.printer(context));
		else print("FALSE ", lit.type.printer(context)); }
	void visit(BinaryExprNode* b) {
		if (b.type) print("BINOP ", b.type.printer(context), " ", b.op);
		else print("BINOP ", b.op);
		pr_node(b.left);
		pr_node(b.right); }
	void visit(UnaryExprNode* u) {
		if (u.type) print("UNOP ", u.type.printer(context), " ", u.op);
		else print("UNOP ", u.op);
		pr_node(u.child); }
	void visit(CallExprNode* c) {
		if (c.callee && c.callee.astType(context) == AstType.decl_function)
			print("CALL ", context.idString(c.callee.get_node_id(context)));
		else {
			print("CALL");
			pr_node(c.callee);
		}
		pr_nodes(c.args); }
	void visit(IndexExprNode* i) {
		print("INDEX"); pr_node(i.array); pr_node(i.index); }
	void visit(SliceExprNode* i) {
		print("SLICE"); pr_node(i.array); pr_node(i.fromIndex); pr_node(i.toIndex); }
	void visit(TypeConvExprNode* t) {
		print("CAST to ", t.type.printer(context));
		pr_node(t.expr); }
	void visit(BasicTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }
	void visit(FunctionSignatureNode* t) { print("TYPE ", t.typeNode.printer(context)); }
	void visit(PtrTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }
	void visit(StaticArrayTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }
	void visit(SliceTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }

	void printAst(AstNode* n)
	{
		indent = -indentSize;
		if (!n) return;
		pr_node(context.getAstNodeIndex(n));
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

	void pr_node_edge(N)(N* parent, AstIndex nodeIndex) { // print node
		AstNode* node = context.getAstNode(nodeIndex);
		writeln(`  node_`, cast(void*)parent, ` -> node_`, cast(void*)node);
		_visit(node);
	}
	void pr_node_edges(N)(N* parent, AstNodes nodes) { // print nodes
		foreach (nodeIndex; nodes) {
			AstNode* node = context.getAstNode(nodeIndex);
			writeln(`  node_`, cast(void*)parent, ` -> node_`, cast(void*)node);
			_visit(node);
		}
	}

	void visit(AliasDeclNode* n) {
		printLabel(n, `ALIAS\n%s`, context.idString(n.id));
		pr_node_edge(n, n.initializer);
	}
	void visit(ModuleDeclNode* m) {
		printLabel(m, "Module");
		pr_node_edges(m, m.declarations);
	}
	void visit(ImportDeclNode* i) {
		printLabel(i, `IMPORT\n%s`, context.idString(i.id));
	}
	void visit(FunctionDeclNode* f) {
		auto sig = f.signature.get!FunctionSignatureNode(context);
		printLabel(f, `FUNC\n%s %s`, sig.returnType.printer(context), context.idString(f.id));
		pr_node_edges(f, sig.parameters);
		if (f.block_stmt) pr_node_edge(f, f.block_stmt);
	}
	void visit(VariableDeclNode* v) {
		printLabel(v, v.isParameter ? `PARAM\n%s %s` : `VAR\n%s %s`, v.type.printer(context), context.idString(v.id));
		if (v.initializer) pr_node_edge(v, v.initializer);
	}
	void visit(StructDeclNode* s) {
		printLabel(s, `STRUCT\n%s`, context.idString(s.id));
		pr_node_edges(s, s.declarations); }
	void visit(EnumDeclaration* e) {
		if (e.isAnonymous)
			printLabel(e, `ENUM\n%s`, e.memberType.printer(context));
		else
			printLabel(e, `ENUM\n%s %s`, e.memberType.printer(context), context.idString(e.id));
		pr_node_edges(e, e.declarations);
	}
	void visit(EnumMemberDecl* m) {
		printLabel(m, `ENUM MEMBER\n%s %s`, m.type.printer(context), context.idString(m.id));
		if (m.initializer) pr_node_edge(m, m.initializer);
	}
	void visit(StaticIfDeclNode* n) {
		printLabel(n, "#IF"); pr_node_edge(n, n.condition);
		pr_node_edges(n, n.thenItems);
		pr_node_edges(n, n.elseItems);
	}
	void visit(BlockStmtNode* b) {
		printLabel(b, "BLOCK");
		pr_node_edges(b, b.statements); }
	void visit(IfStmtNode* i) {
		printLabel(i, "IF"); pr_node_edge(i, i.condition);
		pr_node_edges(i, i.thenStatements);
		pr_node_edges(i, i.elseStatements);
	}
	void visit(WhileStmtNode* w) {
		printLabel(w, "WHILE");
		pr_node_edge(w, w.condition);
		pr_node_edges(w, w.statements); }
	void visit(DoWhileStmtNode* d) {
		printLabel(d, "DO");
		pr_node_edge(d, d.condition);
		pr_node_edges(d, d.statements); }
	void visit(ForStmtNode* n) {
		printLabel(n, "FOR");
		pr_node_edges(n, n.init_statements);
		if (n.condition) pr_node_edge(n, n.condition);
		pr_node_edges(n, n.increment_statements);
		pr_node_edges(n, n.body_statements); }
	void visit(ReturnStmtNode* r) {
		printLabel(r, "RETURN");
		if (r.expression) pr_node_edge(r, r.expression); }
	void visit(BreakStmtNode* r) { printLabel(r, "BREAK"); }
	void visit(ContinueStmtNode* r) { printLabel(r, "CONTINUE"); }
	void visit(NameUseExprNode* v) {
		if (v.isSymResolved)
			printLabel(v, `NAME_USE\n%s %s`, v.entity.get_node_type(context).printer(context), context.idString(v.id(context)));
		else
			printLabel(v, `NAME_USE\n%s`, context.idString(v.id(context)));
	}
	void visit(MemberExprNode* m) {
		printLabel(m, "MEMBER %s", context.idString(m.memberId(context)));
		pr_node_edge(m, m.aggregate);
	}
	void visit(IntLiteralExprNode* lit) { printLabel(lit, `Int LITERAL\n%s %s`, lit.type.printer(context), lit.value); }
	void visit(StringLiteralExprNode* lit) { printLabel(lit, `String LITERAL\n%s %s`, lit.type.printer(context), lit.value); }
	void visit(NullLiteralExprNode* lit) { printLabel(lit, `null LITERAL`); }
	void visit(BoolLiteralExprNode* lit) { if (lit.value) printLabel(lit, "TRUE"); else printLabel(lit, "FALSE"); }
	void visit(BinaryExprNode* b) {
		if (b.type) printLabel(b, `BINOP\n%s %s`, b.type.printer(context), b.op);
		else printLabel(b, `BINOP\n%s`, b.op);
		pr_node_edge(b, b.left);
		pr_node_edge(b, b.right); }
	void visit(UnaryExprNode* n) {
		if (n.type) printLabel(n, `UNOP\n%s %s`, n.type.printer(context), n.op);
		else printLabel(n, `UNOP\n%s`, n.op);
		pr_node_edge(n, n.child); }
	void visit(CallExprNode* n) {
		printLabel(n, `CALL`);
		pr_node_edge(n, n.callee);
		pr_node_edges(n, n.args); }
	void visit(IndexExprNode* n) {
		printLabel(n, `INDEX`); pr_node_edge(n, n.array); pr_node_edge(n, n.index); }
	void visit(SliceExprNode* n) {
		printLabel(n, `SLICE`); pr_node_edge(n, n.array); pr_node_edge(n, n.fromIndex); pr_node_edge(n, n.toIndex); }
	void visit(TypeConvExprNode* n) {
		printLabel(n, `CAST\n%s`, n.type.printer(context));
		pr_node_edge(n, n.expr); }
	void visit(BasicTypeNode* n) { printLabel(n, `TYPE\n%s`, n.typeNode.printer(context)); }
	void visit(FunctionSignatureNode* n) { printLabel(n, `TYPE\n%s`, n.typeNode.printer(context)); }
	void visit(PtrTypeNode* n) { printLabel(n, `TYPE\n%s`, n.typeNode.printer(context)); }
	void visit(StaticArrayTypeNode* n) { printLabel(n, `TYPE\n%s`, n.typeNode.printer(context)); }
	void visit(SliceTypeNode* n) { printLabel(n, `TYPE\n%s`, n.typeNode.printer(context)); }

	void printAst(AstNode* n)
	{
		writeln(`digraph AST {`);
		if (n) _visit(n);
		writeln(`}`);
	}
}
