/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module symbol;

import std.string : format;
import all;

///
enum SymbolClass : ubyte
{
	c_function,
	c_variable,
	c_struct
}

///
enum SymbolFlags : ubyte
{
	isInOrderedScope = 1 << 0,
}

///
struct SymbolRef
{
	this(Identifier identifier)
	{
		this._id = identifier;
	}

	union
	{
		Symbol* _symbol; // used when resolved, Symbol contains Identifier internally
		Identifier _id; // used when not yet resolved
	}
	/// Resolved is stored in isSymResolved flag of AstNode
	Identifier id(bool resolved) { return resolved ? _symbol.id : _id; }
}

///
struct Symbol
{
	Identifier id;
	SourceLocation loc;
	SymbolClass symClass;
	ubyte flags;
	AstNode* node;
	/// Symbol in outer scope with the same id. Can be null
	Symbol* outerSymbol;

	bool isInOrderedScope() { return cast(bool)(flags & SymbolFlags.isInOrderedScope); }

	VariableDeclNode* varDecl()
	{
		assert(node.astType == AstType.decl_var, format("varDecl used on %s", node.astType));
		return cast(VariableDeclNode*)node;
	}

	FunctionDeclNode* funcDecl()
	{
		assert(node.astType == AstType.decl_function, format("funcDecl used on %s", node.astType));
		return cast(FunctionDeclNode*)node;
	}

	StructDeclNode* structDecl()
	{
		assert(node.astType == AstType.decl_struct, format("structDecl used on %s", node.astType));
		return cast(StructDeclNode*)node;
	}

	TypeNode* getType()
	{
		switch(node.astType) with(AstType)
		{
			case decl_function: return (cast(FunctionDeclNode*)node).returnType;
			case decl_var: return (cast(VariableDeclNode*)node).type;
			case expr_name_use, literal_int, literal_string, expr_bin_op, expr_un_op, expr_call, expr_index, expr_type_conv:
				return (cast(ExpressionNode*)node).type;
			case type_basic: return cast(TypeNode*)node;
			case type_struct: return cast(TypeNode*)node;
			default: assert(false, format("getType used on %s", node.astType));
		}
	}
}
