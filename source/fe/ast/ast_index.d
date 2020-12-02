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
	// used to cast from enum of AstIndex to AstIndex.
	AstIndex opCast(T : AstIndex)() const {
		return this;
	}

	T* get(T)(CompilationContext* c) { return c.getAst!T(this); }
	AstNode* get_node(CompilationContext* c) { return c.getAstNode(this); }
	ExpressionNode* get_expr(CompilationContext* c) { return c.getAstExpr(this); }
	TypeNode* get_type(CompilationContext* c) { return c.getAstType(get_node_type(this, c)); }
	NameUseExprNode* get_name_use(CompilationContext* c) { return c.getAst!NameUseExprNode(this); }

	Scope* get_scope(CompilationContext* c) { return c.getAstScope(this); }
	// null scope index is treated as empty scope and results in null AstIndex
	AstIndex lookup_scope(Identifier id, CompilationContext* c) {
		if (isUndefined) return AstIndex.init;
		Scope* sc = c.getAstScope(this);
		return sc.symbols.get(id, AstIndex.init);
	}
	void insert_scope(Identifier id, AstIndex nodeIndex, CompilationContext* c) {
		Scope* sc = c.getAstScope(this);
		assert(isDefined, "scope is undefined");
		sc.insert(id, nodeIndex, c);
	}

	ref TokenIndex loc(CompilationContext* c) { return c.getAstNode(this).loc; }
	ref AstType astType(CompilationContext* c) { return c.getAstNode(this).astType; }
	AstNodeState state(CompilationContext* c) { return c.getAstNode(this).state; }
	void setState(CompilationContext* c, AstNodeState newState) { return c.getAstNode(this).state = newState; }
	ref ushort flags(CompilationContext* c) { return c.getAstNode(this).flags; }

	bool isDeclaration(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isDeclaration); }
	bool isScope(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isScope); }
	bool isExpression(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isExpression); }
	bool isStatement(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isStatement); }
	bool isType(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isType); }
	bool isLvalue(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isLvalue); }
	bool isTemplate(CompilationContext* c) { return cast(bool)(flags(c) & AstFlags.isTemplate); }

	// type functions
	AstIndex getElementType(CompilationContext* c) { return get_type(c).getElementType(c); }
	bool isErrorType() { return this == CommonAstNodes.type_error; }
	bool isPointerType(CompilationContext* c) { return get_type(c).isPointer; }
	bool isNoreturnType(CompilationContext* c) { return get_type(c).isNoreturn; }
	bool isVoidType(CompilationContext* c) { return get_type(c).isVoid; }
	IsSigned isSigned(CompilationContext* c) { return get_type(c).isSigned; }
	bool isFloat(CompilationContext* c) { return get_type(c).isFloat; }
	bool isMetaType(CompilationContext* c) { return get_type(c).isMetaType; }
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
