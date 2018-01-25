/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lang.semantics.symbol;

import lang.ast.ast;

enum SymbolClass
{
	c_function,
	c_variable,
	c_struct
}

class Symbol
{
	SymbolClass symClass;

}



class Scope
{

}

class ScopeVisitor : AstVisitor
{
	override void visit(Module m) { print("MODULE"); foreach(f; m.functions) pr_node(f); }
	override void visit(FunctionDeclaration f) {
		print("FUNC ", idMap.get(f.id));
		indent += indentSize;
		foreach(p; f.parameters)
			print("PARAM ", idMap.get(p.id));
		indent -= indentSize;
		pr_node(f.statements);
	}
	override void visit(IfStatement n) { print("IF"); pr_node(n.condition); pr_node(n.thenStatement); }
	override void visit(IfElseStatement n) { print("IF"); pr_node(n.condition); pr_node(n.thenStatement); pr_node(n.elseStatement); }
	override void visit(WhileStatement w) { print("WHILE"); pr_node(w.condition); pr_node(w.statement); }
	override void visit(DoWhileStatement w) { print("DO"); pr_node(w.condition); pr_node(w.statement); }
	override void visit(ReturnStatement r) { print("RETURN"); if (r.expression) pr_node(r.expression); }
	override void visit(BlockStatement b) { print("BLOCK"); foreach(s; b.statements) pr_node(s); }
	override void visit(ExpressionStatement e) { print("EXPR"); pr_node(e.expression); }
	override void visit(VariableExpression v) { print("VAR ", idMap.get(v.id)); }
	override void visit(ConstExpression c) { print("CONST ", c.value); }
	override void visit(BinaryExpression b) { print("BINOP ", b.op); pr_node(b.left); pr_node(b.right); }
	override void visit(CallExpression c) { print("CALL ", idMap.get(c.calleeId)); foreach(a; c.args) pr_node(a); }
}
