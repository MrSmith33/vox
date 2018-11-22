/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module ast;

import std.format : formattedWrite;
import std.range : repeat;
import std.stdio;
import std.string : format;

import all;

enum AstType : ubyte {
	error,
	abstract_node,

	decl_module,
	decl_function,
	decl_var,
	decl_struct,

	stmt_block,
	stmt_if,
	stmt_while,
	stmt_do_while,
	stmt_return,
	stmt_break,
	stmt_continue,
	stmt_assign,

	expr_var,
	expr_literal,
	expr_bin_op,
	expr_call,
	expr_index,
	expr_type_conv,

	type_basic,
	type_ptr,
	type_static_array,
	type_user,
}

enum AstFlags {
	isDeclaration = 1 << 0,
	isScope       = 1 << 1,
	isExpression  = 1 << 2,
	isStatement   = 1 << 3,
	isType        = 1 << 4,
	isSymResolved = 1 << 5,
	/// Is added to expression nodes that are being assigned to
	isLvalue      = 1 << 6,
}

mixin template AstNodeData(AstType _astType = AstType.abstract_node, int default_flags = 0) {
	this(Args...)(SourceLocation loc, Args args) {
		this(loc);
		enum len = this.tupleof.length - 3;
		enum numDefault = len - args.length;
		static assert(args.length <= len, "Too many args");
		this.tupleof[3..$-numDefault] = args;
	}

	this(SourceLocation loc) {
		this.loc = loc; this.astType = _astType; this.flags = cast(ushort)default_flags; }
	/*this(SourceLocation loc, int flags) {
		this.loc = loc; this.astType = _astType; this.flags = cast(ushort)(default_flags|flags); }
	this(SourceLocation loc, AstType astType) {
		this.loc = loc; this.astType = astType; this.flags = cast(ushort)default_flags; }
	this(SourceLocation loc, AstType astType, int flags) {
		this.loc = loc; this.astType = astType; this.flags = cast(ushort)(default_flags|flags); }*/
	SourceLocation loc;
	AstType astType = _astType;
	ushort flags;

	bool isDeclaration() { return cast(bool)(flags & AstFlags.isDeclaration); }
	bool isScope() { return cast(bool)(flags & AstFlags.isScope); }
	bool isExpression() { return cast(bool)(flags & AstFlags.isExpression); }
	bool isStatement() { return cast(bool)(flags & AstFlags.isStatement); }
	bool isType() { return cast(bool)(flags & AstFlags.isType); }
	bool isSymResolved() { return cast(bool)(flags & AstFlags.isSymResolved); }
	bool isLvalue() { return cast(bool)(flags & AstFlags.isLvalue); }
}

mixin template SymRefNodeData() {
	SymbolRef symRef;
	string strId(CompilationContext* context) { return context.idString(symRef.id(isSymResolved)); }
	Identifier id() { return symRef.id(isSymResolved); }
	void resolveSymbol(Symbol* symbol) {
		symRef._symbol = symbol;
		flags |= AstFlags.isSymResolved;
	}
	Symbol* getSym() { assert(isSymResolved, "Unresolved symbol"); return symRef._symbol; }
}

struct AstNode {
	mixin AstNodeData;
}

// ----------------------------------- Types -----------------------------------
// -----------------------------------------------------------------------------

mixin template TypeNodeData(AstType _astType) {
	mixin AstNodeData!(_astType, AstFlags.isType);
}

struct TypePrinter
{
	TypeNode* node;
	CompilationContext* ctx;

	void toString(scope void delegate(const(char)[]) sink) {
		node.toString(sink, ctx);
	}
}

struct TypeNode {
	mixin AstNodeData!(AstType.abstract_node, AstFlags.isType);

	BasicTypeNode* basicTypeNode() { return cast(BasicTypeNode*)&this; }
	PtrTypeNode* ptrTypeNode() { return cast(PtrTypeNode*)&this; }
	StaticArrayTypeNode* staticArrayTypeNode() { return cast(StaticArrayTypeNode*)&this; }
	UserTypeNode* userTypeNode() { return cast(UserTypeNode*)&this; }

	ulong alignment()
	{
		switch(astType)
		{
			case AstType.type_basic: return basicTypeNode.alignment;
			case AstType.type_ptr: return ptrTypeNode.alignment;
			case AstType.type_static_array: return staticArrayTypeNode.alignment;
			case AstType.type_user: return userTypeNode.alignment;
			default: assert(false, format("got %s", astType));
		}
	}

	ulong size()
	{
		switch(astType)
		{
			case AstType.type_basic: return basicTypeNode.size;
			case AstType.type_ptr: return ptrTypeNode.size;
			case AstType.type_static_array: return staticArrayTypeNode.size;
			case AstType.type_user: return userTypeNode.size;
			default: assert(false, format("got %s", astType));
		}
	}

	string typeName(CompilationContext* context) {
		if (&this == null) return null;
		assert(isType);
		switch(astType)
		{
			case AstType.type_basic:
				return basicTypeNode.strId;
			case AstType.type_ptr:
				return "ptr";
			case AstType.type_static_array: return "[num]";
			case AstType.type_user:
				return userTypeNode.strId(context);
			default: assert(false, format("got %s", astType));
		}
	}

	TypePrinter printer(CompilationContext* context) {
		return TypePrinter(&this, context);
	}

	bool sameType(TypeNode* t2) {
		assert(isType, format("this is %s, not type", astType));
		assert(t2.isType, format("t2 is %s, not type", t2.astType));
		if (astType != t2.astType) return false;

		switch(astType)
		{
			case AstType.type_basic:
				return basicTypeNode.basicType == t2.basicTypeNode.basicType;
			case AstType.type_ptr:
				return ptrTypeNode.base == t2.ptrTypeNode.base;
			case AstType.type_static_array:
				return staticArrayTypeNode.base == t2.staticArrayTypeNode.base &&
					staticArrayTypeNode.length == t2.staticArrayTypeNode.length;
			case AstType.type_user:
				return cast(void*)(&this) == cast(void*)(t2);
			default:
				assert(false, format("got %s %s", astType, t2.astType));
		}
	}

	bool isVoid() {
		return astType == AstType.type_basic &&
			basicTypeNode.basicType == BasicType.t_void;
	}
	bool isError() {
		return astType == AstType.type_basic &&
			basicTypeNode.basicType == BasicType.t_error;
	}

	void assertImplemented(SourceLocation loc, CompilationContext* context) {
		if (!isImplemented)
			context.error(loc, "Type is not implemented `%s`",
				typeName(context));
	}

	bool isImplemented() {
		switch (astType)
		{
			case AstType.type_basic:
			switch (basicTypeNode.basicType)
			{
				case BasicType.t_bool: return true;
				case BasicType.t_i32: return true;
				case BasicType.t_i64: return true;
				default: return false;
			}

			case AstType.type_ptr:
				return ptrTypeNode.base.isImplemented;

			default: return false;
		}
	}

	TypeNode* getElementType(CompilationContext* context) {
		switch(astType)
		{
			case AstType.type_ptr: return ptrTypeNode.base;
			case AstType.type_static_array: return staticArrayTypeNode.base;
			default: context.internal_error(loc, "%s is not indexable", astType); assert(false);
		}
	}

	IrValueType irType(CompilationContext* context) {
		switch (astType)
		{
			case AstType.type_basic:
			switch(basicTypeNode.basicType)
			{
				case BasicType.t_void: return IrValueType.void_t;
				case BasicType.t_bool: return IrValueType.i32;
				case BasicType.t_i32: return IrValueType.i32;
				case BasicType.t_i64: return IrValueType.i64;
				default: break;
			}
			break;

			case AstType.type_ptr: return IrValueType.ptr;

			default: break;
		}
		context.internal_error(loc, "Cannot convert `%s` to IrValueType", astType);
		assert(false);
	}

	void toString(scope void delegate(const(char)[]) sink, CompilationContext* ctx) {
		switch(astType)
		{
			case AstType.type_basic:
				sink(basicTypeNames[basicTypeNode.basicType]);
				break;
			case AstType.type_ptr:
				ptrTypeNode.base.toString(sink, ctx);
				sink("*");
				break;
			case AstType.type_static_array:
				staticArrayTypeNode.base.toString(sink, ctx);
				formattedWrite(sink, "[%s]", staticArrayTypeNode.length);
				break;
			case AstType.type_user:
				sink(userTypeNode.strId(ctx));
				sink("*");
				break;
			default: assert(false, format("%s is not type", astType));
		}
	}
}

enum BasicTypeFlag : ubyte {
	isFloat    = 1 << 0,
	isInteger  = 1 << 1,
	isUnsigned = 1 << 2,
	isBoolean  = 1 << 3,
}

enum POINTER_SIZE = 8;
BasicTypeNode basicTypeNode(ulong size, BasicType basicType, int typeFlags = 0)
{
	return BasicTypeNode(SourceLocation(), size, basicType, cast(ubyte)typeFlags);
}

struct BasicTypeNode {
	mixin TypeNodeData!(AstType.type_basic);
	ulong size; // -1 arch dependent
	ulong alignment() { return size; }
	BasicType basicType;
	ubyte typeFlags;
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	string strId() { return basicTypeNames[basicType]; }

	bool isFloat() { return cast(bool)(typeFlags & BasicTypeFlag.isFloat); }
	bool isInteger() { return cast(bool)(typeFlags & BasicTypeFlag.isInteger); }
	bool isUnsigned() { return cast(bool)(typeFlags & BasicTypeFlag.isUnsigned); }
	bool isBoolean() { return cast(bool)(typeFlags & BasicTypeFlag.isBoolean); }
}

struct PtrTypeNode {
	mixin TypeNodeData!(AstType.type_ptr);
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	TypeNode* base;
	ulong size() { return POINTER_SIZE; }
	ulong alignment() { return POINTER_SIZE; }
}

struct StaticArrayTypeNode {
	mixin TypeNodeData!(AstType.type_static_array);
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	TypeNode* base;
	ulong length;
	ulong size() { return base.size * length; }
	ulong alignment() { return base.alignment; }
}

struct UserTypeNode {
	mixin TypeNodeData!(AstType.type_user);
	mixin SymRefNodeData;
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	ulong size = 1; // TODO, set in semantic
	ulong alignment = 1; // TODO, set as max alignment of members
}

// ------------------------------- Declarations --------------------------------
// -----------------------------------------------------------------------------
mixin template ScopeDeclNodeData(AstType _astType, int default_flags = 0) {
	mixin AstNodeData!(_astType, default_flags | AstFlags.isScope | AstFlags.isDeclaration);
	/// Each node can be struct, function or variable
	AstNode*[] declarations;
}

struct ModuleDeclNode {
	mixin ScopeDeclNodeData!(AstType.decl_module);
	Scope* _scope;
	/// Linear list of all functions of a module (including nested and methods)
	FunctionDeclNode*[] functions;
	IrModule irModule;
	IrModule lirModule;
	ubyte[] code;

	void addFunction(FunctionDeclNode* func) {
		func.index = FunctionIndex(cast(uint)functions.length);
		functions ~= func;
	}

	FunctionDeclNode* findFunction(string idStr, CompilationContext* ctx) {
		Identifier id = ctx.idMap.find(idStr);
		if (id == uint.max) return null;
		return findFunction(id);
	}
	FunctionDeclNode* findFunction(Identifier id) {
		Symbol* sym = _scope.symbols.get(id, null);
		if (sym.symClass != SymbolClass.c_function) return null;
		return sym.funcDecl;
	}
}

struct StructDeclNode {
	mixin ScopeDeclNodeData!(AstType.decl_struct);
	mixin SymRefNodeData;
	Scope* _scope;
}

/// Points into ModuleDeclNode.functions
struct FunctionIndex
{
	uint index;
	alias index this;
}

struct FunctionDeclNode {
	mixin AstNodeData!(AstType.decl_function, AstFlags.isDeclaration);
	mixin SymRefNodeData;
	TypeNode* returnType;
	VariableDeclNode*[] parameters;
	BlockStmtNode* block_stmt; // null if external
	Scope* _scope;
	IrFunction* irData;
	IrFunction* lirData;
	FunctionLiveIntervals* liveIntervals;
	ubyte[] code;
	/// Position in buffer or in memory
	void* funcPtr;
	CallConv* callingConvention;
	FunctionIndex index;

	/// External functions have no body
	bool isExternal() { return block_stmt is null; }
}

enum VariableFlags : ubyte {
	isParameter        = 1 << 1,
	forceMemoryStorage = 1 << 0,
}

struct VariableDeclNode {
	mixin AstNodeData!(AstType.decl_var, AstFlags.isDeclaration | AstFlags.isStatement);
	mixin SymRefNodeData;
	TypeNode* type;
	ExpressionNode* initializer; // may be null
	ubyte varFlags;
	ushort paramIndex; // 0 for non-params
	IrIndex irRef;
	IrVar irVar; // unique id of variable within a function
	IrIndex stackSlotId;
	bool isParameter() { return cast(bool)(varFlags & VariableFlags.isParameter); }
	bool forceMemoryStorage() { return cast(bool)(varFlags & VariableFlags.forceMemoryStorage); }
}


// -------------------------------- Statements ---------------------------------
// -----------------------------------------------------------------------------
struct IfStmtNode {
	mixin AstNodeData!(AstType.stmt_if, AstFlags.isStatement);
	ExpressionNode* condition;
	AstNode* thenStatement;
	AstNode* elseStatement; // Nullable
	Scope* then_scope;
	Scope* else_scope;
}

struct WhileStmtNode {
	mixin AstNodeData!(AstType.stmt_while, AstFlags.isStatement);
	ExpressionNode* condition;
	AstNode* statement;
	Scope* _scope;
}

struct DoWhileStmtNode {
	mixin AstNodeData!(AstType.stmt_do_while, AstFlags.isStatement);
	ExpressionNode* condition;
	AstNode* statement;
	Scope* _scope;
}

struct ReturnStmtNode {
	mixin AstNodeData!(AstType.stmt_return, AstFlags.isStatement);
	ExpressionNode* expression; // Nullable
}

struct BreakStmtNode {
	mixin AstNodeData!(AstType.stmt_break, AstFlags.isStatement);
}

struct ContinueStmtNode {
	mixin AstNodeData!(AstType.stmt_continue, AstFlags.isStatement);
}

struct BlockStmtNode {
	mixin AstNodeData!(AstType.stmt_block, AstFlags.isStatement);
	/// Each node can be expression, declaration or expression
	AstNode*[] statements;
	Scope* _scope;
}

enum AssignOp : ubyte {
	opAssign,
	opIndexAssign
}

struct AssignStmtNode {
	mixin AstNodeData!(AstType.stmt_assign, AstFlags.isStatement);
	AssignOp op;
	ExpressionNode* left;
	ExpressionNode* right;
}

// ------------------------------- Expressions ---------------------------------
// -----------------------------------------------------------------------------
mixin template ExpressionNodeData(AstType _astType) {
	mixin AstNodeData!(_astType, AstFlags.isExpression);
	TypeNode* type;
	IrIndex irRef;
}

// Abstract node, must not be instantiated
struct ExpressionNode {
	mixin ExpressionNodeData!(AstType.abstract_node);
}

struct VariableExprNode {
	mixin ExpressionNodeData!(AstType.expr_var);
	mixin SymRefNodeData;
}

struct LiteralExprNode {
	mixin ExpressionNodeData!(AstType.expr_literal);
	long value;
}

enum BinOp : ubyte {
	// logic ops
	//AND_AND,
	//OR_OR,

	EQUAL_EQUAL,
	NOT_EQUAL,
	GREATER,
	GREATER_EQUAL,
	LESS,
	LESS_EQUAL,

	// arithmetic ops
	//AND,
	//ASHR,
	MINUS,
	//OR,
	//PERCENT,
	PLUS,
	//SHL,
	//SHR,
	SLASH,
	STAR,
	//XOR,
/*
	// arithmetic opEquals
	AND_EQUAL,
	ASHR_EQUAL,
	MINUS_EQUAL,
	OR_EQUAL,
	PERCENT_EQUAL,
	PLUS_EQUAL,
	SHL_EQUAL,
	SHR_EQUAL,
	SLASH_EQUAL,
	STAR_EQUAL,
	XOR_EQUAL,*/
}

enum BinOp BIN_OP_LOGIC_FIRST = BinOp.EQUAL_EQUAL;
enum BinOp BIN_OP_LOGIC_LAST = BinOp.LESS_EQUAL;
enum BinOp BIN_OP_ARITH_FIRST = BinOp.MINUS;
enum BinOp BIN_OP_ARITH_LAST = BinOp.STAR;
//enum BinOp BIN_OP_ARITH_EQUALS_FIRST = BinOp.AND_EQUAL;
//enum BinOp BIN_OP_ARITH_EQUALS_LAST = BinOp.XOR_EQUAL;

struct BinaryExprNode {
	mixin ExpressionNodeData!(AstType.expr_bin_op);
	BinOp op;
	ExpressionNode* left;
	ExpressionNode* right;
}

struct TypeConvExprNode {
	mixin ExpressionNodeData!(AstType.expr_type_conv);
	ExpressionNode* expr;
}

struct CallExprNode {
	mixin ExpressionNodeData!(AstType.expr_call);
	mixin SymRefNodeData; /// Callee
	ExpressionNode*[] args;
}

struct IndexExprNode {
	mixin ExpressionNodeData!(AstType.expr_index);
	ExpressionNode* array;
	ExpressionNode* index;
}


//         ##   ##   #####    #####    #####   #######    ###    ######
//          #   #      #     #     #     #        #      #   #   #     #
//          #   #      #     #           #        #     #     #  #     #
//           # #       #      #####      #        #     #     #  ######
//           # #       #           #     #        #     #     #  #   #
//            #        #     #     #     #        #      #   #   #    #
//            #      #####    #####    #####      #       ###    #     #
// -----------------------------------------------------------------------------
enum VisitOrder { pre, post }
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
			case stmt_block: auto b = cast(BlockStmtNode*)n; visit(b); break;
			case stmt_if: auto i = cast(IfStmtNode*)n; visit(i); break;
			case stmt_while: auto w = cast(WhileStmtNode*)n; visit(w); break;
			case stmt_do_while: auto d = cast(DoWhileStmtNode*)n; visit(d); break;
			case stmt_return: auto r = cast(ReturnStmtNode*)n; visit(r); break;
			case stmt_break: auto b = cast(BreakStmtNode*)n; visit(b); break;
			case stmt_continue: auto c = cast(ContinueStmtNode*)n; visit(c); break;
			case stmt_assign: auto a = cast(AssignStmtNode*)n; visit(a); break;
			case expr_var: auto v = cast(VariableExprNode*)n; visit(v); break;
			case expr_literal: auto l = cast(LiteralExprNode*)n; visit(l); break;
			case expr_bin_op: auto b = cast(BinaryExprNode*)n; visit(b); break;
			case expr_call: auto c = cast(CallExprNode*)n; visit(c); break;
			case expr_index: auto i = cast(IndexExprNode*)n; visit(i); break;
			case expr_type_conv: auto t = cast(TypeConvExprNode*)n; visit(t); break;
			case type_basic: auto t = cast(BasicTypeNode*)n; visit(t); break;
			case type_ptr: auto t = cast(PtrTypeNode*)n; visit(t); break;
			case type_static_array: auto t = cast(StaticArrayTypeNode*)n; visit(t); break;
			case type_user: auto t = cast(UserTypeNode*)n; visit(t); break;
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
	void visit(VariableExprNode* v) {}
	void visit(LiteralExprNode* c) {}
	void visit(BinaryExprNode* b) {
		_visit(cast(AstNode*)b.left);
		_visit(cast(AstNode*)b.right); }
	void visit(CallExprNode* c) {
		foreach (arg; c.args) _visit(arg); }
	void visit(IndexExprNode* i) {
		_visit(i.array); _visit(i.index); }
	void visit(TypeConvExprNode* t) {
		_visit(t.type); _visit(t.expr); }
	void visit(BasicTypeNode* t) {}
	void visit(PtrTypeNode* t) {}
	void visit(StaticArrayTypeNode* t) {}
	void visit(UserTypeNode* t) {}
*/

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
	void visit(FunctionDeclNode* f) {
		print("FUNC ", f.returnType.printer(context), " ", f.strId(context));
		foreach (param; f.parameters) pr_node(cast(AstNode*)param);
		if (f.block_stmt) pr_node(cast(AstNode*)f.block_stmt);
	}
	void visit(VariableDeclNode* v) {
		print(v.isParameter ? "PARAM " : "VAR ", v.type.printer(context), " ", v.strId(context));
	}
	void visit(StructDeclNode* s) {
		print("STRUCT ", s.strId(context));
		foreach (decl; s.declarations) pr_node(decl); }
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
	void visit(AssignStmtNode* a) { print("ASSIGN"); pr_node(cast(AstNode*)a.left); pr_node(cast(AstNode*)a.right); }
	void visit(VariableExprNode* v) {
		if (v.isSymResolved)
			print("VAR_USE ", v.getSym.getType.printer(context), " ", v.strId(context));
		else
			print("VAR_USE ", v.strId(context));
	}
	void visit(LiteralExprNode* c) { print("LITERAL ", c.type.printer(context), " ", c.value); }
	void visit(BinaryExprNode* b) {
		if (b.type) print("BINOP ", b.type.printer(context), " ", b.op);
		else print("BINOP ", b.op);
		pr_node(cast(AstNode*)b.left);
		pr_node(cast(AstNode*)b.right); }
	void visit(CallExprNode* c) {
		if (c.isSymResolved)
			print("CALL ", c.strId(context), " ", c.getSym.getType.printer(context));
		else print("CALL ", c.strId(context));
		foreach (arg; c.args) pr_node(cast(AstNode*)arg); }
	void visit(IndexExprNode* i) {
		print("INDEX"); pr_node(cast(AstNode*)i.array); pr_node(cast(AstNode*)i.index); }
	void visit(TypeConvExprNode* t) {
		print("CAST to ", t.type.printer(context));
		pr_node(cast(AstNode*)t.expr); }
	void visit(BasicTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }
	void visit(PtrTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }
	void visit(StaticArrayTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }
	void visit(UserTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }

	void printAst(AstNode* n)
	{
		indent = -indentSize;
		if (!n) return;
		pr_node(n);
	}
}
