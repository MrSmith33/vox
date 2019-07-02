/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.ast.declaration;

import std.stdio;
import all;

ModuleDeclNode* cast_decl_module(AstNode* n) { if (n.astType == AstType.decl_module) return cast(ModuleDeclNode*)n; return null; }
StructDeclNode* cast_decl_struct(AstNode* n) { if (n.astType == AstType.decl_struct) return cast(StructDeclNode*)n; return null; }
FunctionDeclNode* cast_decl_function(AstNode* n) { if (n.astType == AstType.decl_function) return cast(FunctionDeclNode*)n; return null; }
VariableDeclNode* cast_decl_var(AstNode* n) { if (n.astType == AstType.decl_var) return cast(VariableDeclNode*)n; return null; }
EnumDeclaration* cast_decl_enum(AstNode* n) { if (n.astType == AstType.decl_enum) return cast(EnumDeclaration*)n; return null; }
EnumMemberDecl* cast_decl_enum_member(AstNode* n) { if (n.astType == AstType.decl_enum_member) return cast(EnumMemberDecl*)n; return null; }

mixin template ScopeDeclNodeData(AstType _astType, int default_flags = 0) {
	mixin AstNodeData!(_astType, default_flags | AstFlags.isScope | AstFlags.isDeclaration);
	/// Each node can be struct, function or variable
	Array!(AstNode*) declarations;
}

/// Index into CompilationContext.files
struct ModuleIndex
{
	uint fileIndex;
}

struct ModuleDeclNode {
	mixin ScopeDeclNodeData!(AstType.decl_module);
	Scope* _scope;
	/// Linear list of all functions of a module (including nested and methods and externals)
	Array!(FunctionDeclNode*) functions;
	IrModule irModule;
	IrModule lirModule;
	LinkIndex objectSymIndex;
	ModuleIndex moduleIndex;
	/// module identifier. Used by import declaration.
	Identifier id;

	void addFunction(ref ArrayArena arrayArena, FunctionDeclNode* func) {
		func.backendData.index = FunctionIndex(cast(uint)functions.length, moduleIndex);
		functions.put(arrayArena, func);
	}

	FunctionDeclNode* findFunction(string idStr, CompilationContext* ctx) {
		Identifier id = ctx.idMap.find(idStr);
		if (id.isUndefined) return null;
		return findFunction(id);
	}
	FunctionDeclNode* findFunction(Identifier id) {
		AstNode* sym = _scope.symbols.get(id, null);
		if (sym is null) return null;
		return sym.cast_decl_function;
	}
}

struct ImportDeclNode
{
	mixin AstNodeData!(AstType.decl_import, AstFlags.isDeclaration);
	Identifier id;
}

struct StructDeclNode {
	mixin ScopeDeclNodeData!(AstType.decl_struct, AstFlags.isType);
	Identifier id;
	IrIndex irType;
	Scope* _scope;
	uint size = 1;
	uint alignment = 1;

	this(TokenIndex loc, Array!(AstNode*) members, Identifier id, bool _isOpaque)
	{
		this.loc = loc;
		this.astType = AstType.decl_struct;
		this.flags = AstFlags.isScope | AstFlags.isDeclaration | AstFlags.isType;
		this.declarations = members;
		this.id = id;
		if (_isOpaque) flags |= AstFlags.user1;
	}

	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	bool isOpaque() { return cast(bool)(flags & AstFlags.user1); }
}

/// Refers to a function inside a module
struct FunctionIndex
{
	/// Index into ModuleDeclNode.functions
	uint functionIndex;
	ModuleIndex moduleIndex;
}

struct FunctionBackendData
{
	/// Machine-independent IR
	IrFunction* irData;
	/// Machine-level IR
	IrFunction* lirData;
	///
	FunctionLiveIntervals liveIntervals;
	/// Executable machine-code bytes
	ubyte[] code;
	/// Position in buffer or in memory
	void* funcPtr;
	///
	StackLayout stackLayout;
	///
	CallConv* callingConvention;
	/// Callers will use this index to call this function.
	FunctionIndex index;
	/// Index of IrValueKind.type kind
	IrIndex returnType; // TODO: remove in favor of `type`
	/// Index of function type
	IrIndex irType;
	///
	Identifier name;
	///
	LinkIndex objectSymIndex;
}

struct FunctionDeclNode {
	mixin AstNodeData!(AstType.decl_function, AstFlags.isDeclaration);
	TypeNode* returnType;
	Array!(VariableDeclNode*) parameters;
	BlockStmtNode* block_stmt; // null if external
	Scope* _scope;
	FunctionBackendData backendData;

	this(TokenIndex loc, TypeNode* retType, Array!(VariableDeclNode*) parameters, BlockStmtNode* block, Identifier id)
	{
		this.loc = loc;
		this.astType = AstType.decl_function;
		this.flags = AstFlags.isDeclaration;
		this.returnType = retType;
		this.parameters = parameters;
		this.block_stmt = block;
		this.backendData.name = id;
	}

	/// External functions have no body
	bool isExternal() { return block_stmt is null; }
	ref Identifier id() { return backendData.name; }
}

enum VariableFlags : ubyte {
	forceMemoryStorage = 1 << 0,
	isParameter        = 1 << 1,
	isAddressTaken     = 1 << 2,
}

struct VariableDeclNode
{
	mixin AstNodeData!(AstType.decl_var, AstFlags.isDeclaration | AstFlags.isStatement);
	TypeNode* type;
	ExpressionNode* initializer; // may be null
	Identifier id;
	ubyte varFlags; // VariableFlags
	ushort scopeIndex; // stores index of parameter or index of member (for struct fields)
	IrIndex irValue; // kind is variable or stackSlot, unique id of variable within a function
	bool forceMemoryStorage() { return cast(bool)(varFlags & VariableFlags.forceMemoryStorage); }
	bool isParameter() { return cast(bool)(varFlags & VariableFlags.isParameter); }
	bool isAddressTaken() { return cast(bool)(varFlags & VariableFlags.isAddressTaken); }
}

struct EnumDeclaration
{
	mixin ScopeDeclNodeData!(AstType.decl_enum);
	TypeNode* memberType;
	Scope* _scope;
	Identifier id;
	TypeNode* typeNode() { return cast(TypeNode*)&this; }

	this(TokenIndex loc, Array!(AstNode*) members, TypeNode* memberType, Identifier id)
	{
		this.loc = loc;
		this.astType = AstType.decl_enum;
		this.flags = AstFlags.isScope | AstFlags.isDeclaration;
		this.declarations = members;
		this.memberType = memberType;
		this.id = id;
	}

	/// Anonymous
	this(TokenIndex loc, Array!(AstNode*) members, TypeNode* memberType)
	{
		this.loc = loc;
		this.astType = AstType.decl_enum;
		this.flags = AstFlags.isScope | AstFlags.isDeclaration | AstFlags.user1;
		this.declarations = members;
		this.memberType = memberType;
	}

	bool isAnonymous() { return cast(bool)(flags & AstFlags.user1); }
}

struct EnumMemberDecl
{
	mixin AstNodeData!(AstType.decl_enum_member, AstFlags.isDeclaration | AstFlags.isStatement);
	TypeNode* type;
	ExpressionNode* initializer;
	Identifier id;
	ushort scopeIndex;
}
