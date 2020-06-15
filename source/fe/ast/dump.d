/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.ast.dump;

import std.stdio;

import all;

struct AstPrintState
{
	CompilationContext* context;

	int indentSize = 1;
	private int indent;

	void print(Args...)(Args args) {
		import std.range : repeat;
		write(' '.repeat(indent)); // indent
		writeln(args);
	}
}

void print_ast(AstIndex nodeIndex, CompilationContext* context, int indentSize = 1)
{
	auto state = AstPrintState(context, indentSize);
	state.indent = -state.indentSize;
	print_ast(nodeIndex, state);
}

void print_ast(AstNodes nodes, ref AstPrintState state)
{
	foreach(AstIndex item; nodes) print_ast(item, state);
}

void print_ast(AstIndex nodeIndex, ref AstPrintState state)
{
	if (nodeIndex.isUndefined) return;

	state.indent += state.indentSize;
	scope(exit) state.indent -= state.indentSize;
	AstNode* node = state.context.getAstNode(nodeIndex);
	final switch(node.astType) with(AstType)
	{
		case error: state.context.internal_error(node.loc, "Visiting error node"); break;
		case abstract_node: state.context.internal_error(node.loc, "Visiting abstract node"); break;

		case decl_alias: print_alias(cast(AliasDeclNode*)node, state); break;
		case decl_builtin: break; // skip
		case decl_enum: print_enum(cast(EnumDeclaration*)node, state); break;
		case decl_enum_member: print_enum_member(cast(EnumMemberDecl*)node, state); break;
		case decl_function: print_func(cast(FunctionDeclNode*)node, state); break;
		case decl_import: print_import(cast(ImportDeclNode*)node, state); break;
		case decl_module: print_module(cast(ModuleDeclNode*)node, state); break;
		case decl_static_assert: print_static_assert(cast(StaticAssertDeclNode*)node, state); break;
		case decl_static_if: print_static_if(cast(StaticIfDeclNode*)node, state); break;
		case decl_struct: print_struct(cast(StructDeclNode*)node, state); break;
		case decl_template: print_template(cast(TemplateDeclNode*)node, state); break;
		case decl_template_param: print_template_param(cast(TemplateParamDeclNode*)node, state); break;
		case decl_var: print_var(cast(VariableDeclNode*)node, state); break;

		case stmt_block: print_block(cast(BlockStmtNode*)node, state); break;
		case stmt_if: print_if(cast(IfStmtNode*)node, state); break;
		case stmt_while: print_while(cast(WhileStmtNode*)node, state); break;
		case stmt_do_while: print_do(cast(DoWhileStmtNode*)node, state); break;
		case stmt_for: print_for(cast(ForStmtNode*)node, state); break;
		case stmt_switch: print_switch(cast(SwitchStmtNode*)node, state); break;
		case stmt_return: print_return(cast(ReturnStmtNode*)node, state); break;
		case stmt_break: print_break(cast(BreakStmtNode*)node, state); break;
		case stmt_continue: print_continue(cast(ContinueStmtNode*)node, state); break;

		case expr_name_use: print_name_use(cast(NameUseExprNode*)node, state); break;
		case expr_member: print_member(cast(MemberExprNode*)node, state); break;
		case expr_bin_op: print_binary_op(cast(BinaryExprNode*)node, state); break;
		case expr_un_op: print_unary_op(cast(UnaryExprNode*)node, state); break;
		case expr_call: print_call(cast(CallExprNode*)node, state); break;
		case expr_index: print_index(cast(IndexExprNode*)node, state); break;
		case expr_slice: print_expr_slice(cast(SliceExprNode*)node, state); break;
		case expr_type_conv: print_type_conv(cast(TypeConvExprNode*)node, state); break;

		case literal_int: print_literal_int(cast(IntLiteralExprNode*)node, state); break;
		case literal_string: print_literal_string(cast(StringLiteralExprNode*)node, state); break;
		case literal_null: print_literal_null(cast(NullLiteralExprNode*)node, state); break;
		case literal_bool: print_literal_bool(cast(BoolLiteralExprNode*)node, state); break;

		case type_basic: print_type_basic(cast(BasicTypeNode*)node, state); break;
		case type_func_sig: print_func_sig(cast(FunctionSignatureNode*)node, state); break;
		case type_ptr: print_ptr(cast(PtrTypeNode*)node, state); break;
		case type_static_array: print_static_array(cast(StaticArrayTypeNode*)node, state); break;
		case type_slice: print_slice(cast(SliceTypeNode*)node, state); break;
	}
}

/*
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
	void visit(TemplateDeclNode* n) {
		printLabel(n, "TEMPLATE %s", context.idString(n.id));
		pr_node_edges(n, n.parameters);
		pr_node_edge(n, n.body);
	}
	void visit(TemplateParamDeclNode* n) {
		printLabel(n, "TEMPLATE PARAM %s", context.idString(n.id));
	}
	void visit(BlockStmtNode* b) {
		printLabel(b, "BLOCK");
		pr_node_edges(b, b.statements); }
	void visit(IfStmtNode* i) {
		printLabel(i, "IF"); pr_node_edge(i, i.condition);
		pr_node_edges(i, i.thenStatements);
		pr_node_edges(i, i.elseStatements);
	}
	void visit(WhileStmtNode* node) {
		printLabel(node, "WHILE");
		pr_node_edge(node, node.condition);
		pr_node_edges(node, node.statements); }
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
	void visit(SwitchStmtNode* n) {
		printLabel(n, "SWITCH");
		pr_node_edge(n, n.condition);
		foreach (SwitchCase c; n.cases) {
			pr_node_edge(n, c.expr);
			pr_node_edge(n, c.stmt);
		}
		if (n.elseStmt) {
			pr_node_edge(n, n.elseStmt);
		}
	}
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
		printLabel(n, `INDEX`); pr_node_edge(n, n.array); pr_node_edges(n, n.indicies); }
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
*/
