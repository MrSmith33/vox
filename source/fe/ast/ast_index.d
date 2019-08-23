/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.ast_index;

import all;

struct AstIndex
{
	uint storageIndex;
	bool isDefined() const { return storageIndex != 0; }
	bool isUndefined() const { return storageIndex == 0; }

	bool opCast(T : bool)() const {
		return storageIndex != 0;
	}

	T* get(T)(CompilationContext* c) { return c.getAst!T(this); }
	AstNode* get_node(CompilationContext* c) { return c.getAstNode(this); }
	ExpressionNode* get_expr(CompilationContext* c) { return c.getAstExpr(this); }
	TypeNode* get_type(CompilationContext* c) { return c.getAstType(get_node_type(this, c)); }
	NameUseExprNode* get_name_use(CompilationContext* c) { return c.getAst!NameUseExprNode(this); }
	Scope* get_scope(CompilationContext* c) { return c.getAstScope(this); }

	ref TokenIndex loc(CompilationContext* c) { return c.getAstNode(this).loc; }
	ref AstType astType(CompilationContext* c) { return c.getAstNode(this).astType; }
	AstNodeState state(CompilationContext* c) { return c.getAstNode(this).state; }
	void setState(CompilationContext* c, AstNodeState newState) { return c.getAstNode(this).state = newState; }
	ushort flags(CompilationContext* c) { return c.getAstNode(this).flags; }

	ref AstIndex expr_type(CompilationContext* c) { return get_expr(c).type; }

	bool isDeclaration(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isDeclaration); }
	bool isScope(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isScope); }
	bool isExpression(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isExpression); }
	bool isStatement(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isStatement); }
	bool isType(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isType); }
	bool isLvalue(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isLvalue); }
	bool isLiteral(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isLiteral); }
	bool isAssignment(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isAssignment); }
	bool isArgument(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isArgument); }
	bool isGlobal(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isGlobal); }
	bool isInOrderedScope(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isInOrderedScope); }

	// type functions
	AstIndex getElementType(CompilationContext* c) { return get_type(c).getElementType(c); }
	bool isErrorType(CompilationContext* c) { return get_type(c).isError; }
	bool isPointerType(CompilationContext* c) { return get_type(c).isPointer; }
}


AstNodePrinter printer(AstIndex nodeIndex, CompilationContext* context)
{
	return AstNodePrinter(nodeIndex, context);
}

struct AstNodePrinter
{
	AstIndex nodeIndex;
	CompilationContext* context;

	void toString(scope void delegate(const(char)[]) sink) {
		if (!nodeIndex) {
			sink("<null>");
			return;
		}
		AstNode* node = context.getAstNode(nodeIndex);
		if (node.isType) node.as_type(context).printType(sink, context);
	}
}
