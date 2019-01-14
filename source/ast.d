/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
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

	expr_name_use,
	expr_member,
	expr_call,
	expr_index,
	expr_bin_op,
	expr_un_op,
	expr_type_conv,

	literal_int,
	literal_string,

	type_basic,
	type_ptr,
	type_static_array,
	type_struct,
}

enum AstFlags {
	isDeclaration = 1 << 0,
	isScope       = 1 << 1,
	isExpression  = 1 << 2,
	/// Can be applied to expression if it is in place of stmt
	isStatement   = 1 << 3,
	isType        = 1 << 4,
	isSymResolved = 1 << 5,
	/// Is added to expression nodes that are being assigned to
	isLvalue      = 1 << 6,
	isLiteral     = 1 << 7,
	isAssignment  = 1 << 8,
	/// Marks expression that is used as func argument.
	/// Needed to handle calling conventions properly.
	isArgument    = 1 << 9,
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
	bool isLiteral() { return cast(bool)(flags & AstFlags.isLiteral); }
	bool isAssignment() { return cast(bool)(flags & AstFlags.isAssignment); }
	bool isArgument() { return cast(bool)(flags & AstFlags.isArgument); }
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
	StructTypeNode* structTypeNode() { return cast(StructTypeNode*)&this; }

	uint alignment()
	{
		switch(astType)
		{
			case AstType.type_basic: return basicTypeNode.alignment;
			case AstType.type_ptr: return ptrTypeNode.alignment;
			case AstType.type_static_array: return staticArrayTypeNode.alignment;
			case AstType.type_struct: return structTypeNode.alignment;
			default: assert(false, format("got %s", astType));
		}
	}

	uint size()
	{
		switch(astType)
		{
			case AstType.type_basic: return basicTypeNode.size;
			case AstType.type_ptr: return ptrTypeNode.size;
			case AstType.type_static_array: return staticArrayTypeNode.size;
			case AstType.type_struct: return structTypeNode.size;
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
			case AstType.type_struct:
				return structTypeNode.strId(context);
			default: assert(false, format("got %s", astType));
		}
	}

	TypePrinter printer(CompilationContext* context) {
		return TypePrinter(&this, context);
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
				case BasicType.t_void: return true;
				case BasicType.t_bool: return true;
				case BasicType.t_i8: return true;
				case BasicType.t_u8: return true;
				case BasicType.t_i32: return true;
				case BasicType.t_i64: return true;
				case BasicType.t_u64: return true;
				default: return false;
			}

			case AstType.type_ptr: return true;
			case AstType.type_struct: return true;

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
			case AstType.type_struct:
				sink(structTypeNode.strId(ctx));
				break;
			default: assert(false, format("%s is not type", astType));
		}
	}
}

IrIndex genIrType(TypeNode* t, CompilationContext* context) {
	switch (t.astType)
	{
		case AstType.type_basic: return genIrType(t.basicTypeNode, context);
		case AstType.type_ptr: return genIrType(t.ptrTypeNode, context);
		case AstType.type_static_array: return genIrType(t.staticArrayTypeNode, context);
		case AstType.type_struct: return genIrType(t.structTypeNode, context);
		default:
			context.internal_error(t.loc, "Cannot convert `%s` to ir type", t.astType);
			assert(false);
	}
}

IrIndex genIrType(BasicTypeNode* t, CompilationContext* context)
	out(res; res.isTypeBasic, "Not a basic type")
{
	switch(t.basicType)
	{
		case BasicType.t_void: return makeBasicTypeIndex(IrValueType.void_t);
		case BasicType.t_bool: return makeBasicTypeIndex(IrValueType.i32);
		case BasicType.t_u8: return makeBasicTypeIndex(IrValueType.i8);
		case BasicType.t_i8: return makeBasicTypeIndex(IrValueType.i8);
		case BasicType.t_i16: return makeBasicTypeIndex(IrValueType.i16);
		case BasicType.t_u16: return makeBasicTypeIndex(IrValueType.i16);
		case BasicType.t_i32: return makeBasicTypeIndex(IrValueType.i32);
		case BasicType.t_u32: return makeBasicTypeIndex(IrValueType.i32);
		case BasicType.t_i64: return makeBasicTypeIndex(IrValueType.i64);
		case BasicType.t_u64: return makeBasicTypeIndex(IrValueType.i64);
		default:
			context.internal_error(t.loc, "Cannot convert %s to IrIndex", t.basicType);
			assert(false);
	}
}

IrIndex genIrType(PtrTypeNode* t, CompilationContext* context)
	out(res; res.isTypePointer, "Not a pointer type")
{
	if (t.irType.isDefined) return t.irType;
	t.irType = context.types.appendPtr(t.base.genIrType(context));
	return t.irType;
}

IrIndex genIrType(StaticArrayTypeNode* t, CompilationContext* context)
	out(res; res.isTypeArray, "Not a array type")
{
	if (t.irType.isDefined) return t.irType;
	t.irType = context.types.appendArray(t.base.genIrType(context), t.length);
	return t.irType;
}

IrIndex genIrType(StructTypeNode* t, CompilationContext* context)
	out(res; res.isTypeStruct, "Not a struct type")
{
	StructDeclNode* s = t.getSym.structDecl;

	if (s.irType.isDefined) return s.irType;

	uint numFields = 0;
	foreach(AstNode* member; s.declarations)
		if (member.astType == AstType.decl_var)
		{
			++numFields;
		}

	s.irType = context.types.appendStruct(numFields);
	IrTypeStruct* structType = &context.types.get!IrTypeStruct(s.irType);
	IrTypeStructMember[] members = structType.members;

	uint memberIndex;
	uint memberOffset;
	foreach(AstNode* member; s.declarations)
		if (member.astType == AstType.decl_var)
		{
			IrIndex type = (cast(VariableDeclNode*)member).type.genIrType(context);
			uint memberSize = context.types.typeSize(type);

			members[memberIndex++] = IrTypeStructMember(type, memberOffset);
			// TODO: alignment
			memberOffset += memberSize;
		}
	structType.size = memberOffset;
	context.todo("genIrType struct alignment/size");
	return s.irType;
}

bool sameType(TypeNode* t1, TypeNode* t2) {
	assert(t1.isType, format("this is %s, not type", t1.astType));
	assert(t2.isType, format("t2 is %s, not type", t2.astType));

	if (t1.astType != t2.astType) {
		return false;
	}

	switch(t1.astType) with(AstType)
	{
		case type_basic:
			return t1.basicTypeNode.basicType == t2.basicTypeNode.basicType;
		case type_ptr: return sameType(t1.ptrTypeNode, t2.ptrTypeNode);
		case type_static_array: return sameType(t1.staticArrayTypeNode, t2.staticArrayTypeNode);
		case type_struct:
			return sameType(t1.structTypeNode, t2.structTypeNode);
		default:
			assert(false, format("got %s %s", t1.astType, t2.astType));
	}
}

enum BasicTypeFlag : ubyte {
	isFloat    = 1 << 0,
	isInteger  = 1 << 1,
	isUnsigned = 1 << 2,
	isBoolean  = 1 << 3,
}

enum POINTER_SIZE = 8;
BasicTypeNode basicTypeNode(uint size, BasicType basicType, int typeFlags = 0)
{
	return BasicTypeNode(SourceLocation(), size, basicType, cast(ubyte)typeFlags);
}

struct BasicTypeNode {
	mixin TypeNodeData!(AstType.type_basic);
	uint size;
	uint alignment() { return size; }
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
	IrIndex irType;
	uint size() { return POINTER_SIZE; }
	uint alignment() { return POINTER_SIZE; }
}

bool sameType(PtrTypeNode* t1, PtrTypeNode* t2)
{
	return sameType(t1.base, t2.base);
}

struct StaticArrayTypeNode {
	mixin TypeNodeData!(AstType.type_static_array);
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	TypeNode* base;
	uint length;
	IrIndex irType;
	uint size() { return cast(uint)(base.size * length); } // TODO check overflow
	uint alignment() { return base.alignment; }
}

bool sameType(StaticArrayTypeNode* t1, StaticArrayTypeNode* t2)
{
	return sameType(t1.base, t2.base) && (t1.length == t2.length);
}

struct StructTypeNode {
	mixin TypeNodeData!(AstType.type_struct);
	mixin SymRefNodeData;
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	uint size = 1; // TODO, set in semantic
	uint alignment = 1; // TODO, set as max alignment of members
}

bool sameType(StructTypeNode* t1, StructTypeNode* t2)
{
	return t1.getSym is t2.getSym;
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
	/// Linear list of all functions of a module (including nested and methods and externals)
	FunctionDeclNode*[] functions;
	IrModule irModule;
	IrModule lirModule;
	ubyte[] code;

	void addFunction(FunctionDeclNode* func) {
		func.backendData.index = FunctionIndex(cast(uint)functions.length);
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
	IrIndex irType;
}

/// Points into ModuleDeclNode.functions
struct FunctionIndex
{
	uint index;
	alias index this;
}

struct FunctionBackendData
{
	IrFunction* irData;
	IrFunction* lirData;
	FunctionLiveIntervals* liveIntervals;
	ubyte[] code;
	/// Position in buffer or in memory
	void* funcPtr;
	///
	StackLayout stackLayout;
	///
	CallConv* callingConvention;
	///
	FunctionIndex index;
	///
	IrIndex returnType;
	///
	Identifier name;
}

struct FunctionDeclNode {
	mixin AstNodeData!(AstType.decl_function, AstFlags.isDeclaration);
	mixin SymRefNodeData;
	TypeNode* returnType;
	VariableDeclNode*[] parameters;
	BlockStmtNode* block_stmt; // null if external
	Scope* _scope;
	FunctionBackendData backendData;

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
	ushort scopeIndex; // stores index of parameter or index of member (for struct fields)
	IrIndex irValue; // kind is variable or stackSlot, unique id of variable within a function
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

// ------------------------------- Expressions ---------------------------------
// -----------------------------------------------------------------------------
mixin template ExpressionNodeData(AstType _astType, int default_flags = 0) {
	mixin AstNodeData!(_astType, default_flags | AstFlags.isExpression);
	TypeNode* type;
	// can be stack slot, global, variable, virtualRegister, constant
	IrIndex irValue;
}

// Abstract node, must not be instantiated
struct ExpressionNode {
	mixin ExpressionNodeData!(AstType.abstract_node);
}

struct NameUseExprNode {
	mixin ExpressionNodeData!(AstType.expr_name_use);
	mixin SymRefNodeData;
}

struct IntLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_int, AstFlags.isLiteral);
	long value;
}

struct StringLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_string, AstFlags.isLiteral);
	string value;
}

enum BinOp : ubyte {
	// logic ops
	LOGIC_AND,          // &&
	LOGIC_OR,           // ||

	// comparisons are converted into IrBinaryCondition, order is important
	EQUAL,              // ==
	NOT_EQUAL,          // !=
	GREATER,            // >
	GREATER_EQUAL,      // >=
	LESS,               // <
	LESS_EQUAL,         // <=

	// arithmetic ops
	BITWISE_AND,        // &
	BITWISE_OR,         // |
	REMAINDER,          // %
	SHL,                // <<
	SHR,                // >>
	ASHR,               // >>>
	MINUS,              // -
	PLUS,               // +
	DIV,                // /
	MULT,               // *
	XOR,                // ^

	// arithmetic opEquals
	ASSIGN,             // =
	BITWISE_AND_ASSIGN, // &=
	BITWISE_OR_ASSIGN,  // |=
	REMAINDER_ASSIGN,   // %=
	SHL_ASSIGN,         // <<=
	SHR_ASSIGN,         // >>=
	ASHR_ASSIGN,        // >>>=
	MINUS_ASSIGN,       // -=
	PLUS_ASSIGN,        // +=
	DIV_ASSIGN,         // /=
	MULT_ASSIGN,        // *=
	XOR_ASSIGN,         // ^=

	// member access
	DOT,                // .
}

enum BinOp BIN_OP_LOGIC_FIRST = BinOp.EQUAL;
enum BinOp BIN_OP_LOGIC_LAST = BinOp.LESS_EQUAL;
enum BinOp BIN_OP_ARITH_FIRST = BinOp.MINUS;
enum BinOp BIN_OP_ARITH_LAST = BinOp.MULT;
//enum BinOp BIN_OP_ARITH_EQUALS_FIRST = BinOp.AND_EQUAL;
//enum BinOp BIN_OP_ARITH_EQUALS_LAST = BinOp.XOR_EQUAL;

struct BinaryExprNode {
	mixin ExpressionNodeData!(AstType.expr_bin_op);
	BinOp op;
	ExpressionNode* left;
	ExpressionNode* right;
}

enum UnOp : ubyte {
	plus, // +
	minus, // -
	logicalNot, // !
	bitwiseNot, // ~
	deref, // *
	addrOf, // &
	preIncrement, // ++x
	preDecrement, // --x
	postIncrement, // x++
	postDecrement, // x--
}

struct UnaryExprNode {
	mixin ExpressionNodeData!(AstType.expr_un_op);
	UnOp op;
	ExpressionNode* child;
}

// member access of aggregate.member form
struct MemberExprNode {
	mixin ExpressionNodeData!(AstType.expr_member);
	ExpressionNode* aggregate;
	NameUseExprNode* member; // member name
	uint memberIndex; // resolved index of member being accessed
}

struct TypeConvExprNode {
	mixin ExpressionNodeData!(AstType.expr_type_conv);
	ExpressionNode* expr;
}

struct CallExprNode {
	mixin ExpressionNodeData!(AstType.expr_call);
	ExpressionNode* callee;
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
	void visit(StructTypeNode* t) {}
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
		if (v.initializer) pr_node(cast(AstNode*)v.initializer);
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
	void visit(NameUseExprNode* v) {
		if (v.isSymResolved)
			print("VAR_USE ", v.getSym.getType.printer(context), " ", v.strId(context));
		else
			print("VAR_USE ", v.strId(context));
	}
	void visit(MemberExprNode* m) {
		print("MEMBER");
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
		_visit(cast(AstNode*)u.child); }
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
	void visit(StructTypeNode* t) { printLabel(t, `TYPE\n%s`, t.typeNode.printer(context)); }

	void printAst(AstNode* n)
	{
		writeln(`digraph AST {`);
		if (n) _visit(n);
		writeln(`}`);
	}
}
