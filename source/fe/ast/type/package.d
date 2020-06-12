/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.type;

import all;

public import fe.ast.type.basic;
public import fe.ast.type.func_sig;
public import fe.ast.type.ptr;
public import fe.ast.type.slice;
public import fe.ast.type.static_array;


enum POINTER_SIZE = 8;

struct TypeNode
{
	AstNode base;
	alias base this;

	BasicTypeNode* as_basic() { if (astType == AstType.type_basic) return cast(BasicTypeNode*)&this; return null; }
	FunctionSignatureNode* as_func_sig() { if (astType == AstType.type_func_sig) return cast(FunctionSignatureNode*)&this; return null; }
	PtrTypeNode* as_ptr() { if (astType == AstType.type_ptr) return cast(PtrTypeNode*)&this; return null; }
	SliceTypeNode* as_slice() { if (astType == AstType.type_slice) return cast(SliceTypeNode*)&this; return null; }
	StaticArrayTypeNode* as_static_array() { if (astType == AstType.type_static_array) return cast(StaticArrayTypeNode*)&this; return null; }
	StructDeclNode* as_struct() { if (astType == AstType.decl_struct) return cast(StructDeclNode*)&this; return null; }
	AliasDeclNode* as_alias() { if (astType == AstType.decl_alias) return cast(AliasDeclNode*)&this; return null; }
	NameUseExprNode* as_name_use() { if (astType == AstType.expr_name_use) return cast(NameUseExprNode*)&this; return null; }
	EnumDeclaration* as_enum() { if (astType == AstType.decl_enum) return cast(EnumDeclaration*)&this; return null; }

	TypeNode* foldAliases(CompilationContext* c) {
		if (astType == AstType.expr_name_use) return as_name_use.entity.get_type(c);
		return &this;
	}

	uint alignment(CompilationContext* c)
	{
		return typeAlignment(&this, c);
	}

	uint size(CompilationContext* c)
	{
		return typeSize(&this, c);
	}

	IrArgSize argSize(CompilationContext* c)
	{
		return size(c).sizeToIrArgSize(c);
	}

	bool isOpaqueStruct(CompilationContext* c) {
		TypeNode* t = foldAliases(c);
		if (t.astType == AstType.expr_name_use) t = c.getAstNode(t.as_name_use.entity).as_type(c);
		return t.astType == AstType.decl_struct && t.as_struct.isOpaque;
	}

	TypePrinter printer(CompilationContext* c) {
		return TypePrinter(&this, c);
	}

	bool isPassByPtr(CompilationContext* c)
	{
		if (astType == AstType.type_slice) return true;
		if (astType == AstType.decl_struct)
		{
			switch(size(c)) {
				case 1: return false;
				case 2: return false;
				case 4: return false;
				case 8: return false;
				default: return true;
			}
		}
		return false;
	}

	bool isTypeofNull() {
		return astType == AstType.type_basic &&
			as_basic.basicType == BasicType.t_null;
	}
	bool isTypeBasic() {
		return astType == AstType.type_basic;
	}
	bool isNoreturn() {
		return astType == AstType.type_basic &&
			as_basic.basicType == BasicType.t_noreturn;
	}
	bool isVoid() {
		return astType == AstType.type_basic &&
			as_basic.basicType == BasicType.t_void;
	}
	bool isNoreturnOrVoid() {
		return astType == AstType.type_basic &&
			as_basic.basicType == BasicType.t_void;
	}
	bool isError() {
		return astType == AstType.type_basic &&
			as_basic.basicType == BasicType.t_error;
	}
	bool isPointer() { return astType == AstType.type_ptr; }
	bool isSlice() { return astType == AstType.type_slice; }
	bool isStaticArray() { return astType == AstType.type_static_array; }
	bool isInteger() { return astType == AstType.type_basic && as_basic.isInteger; }
	bool isBool() { return astType == AstType.type_basic &&
			as_basic.basicType == BasicType.t_bool; }
	bool isStruct() { return astType == AstType.decl_struct; }
	bool isFuncSignature() { return astType == AstType.type_func_sig; }
	bool isMetaType() {
		return astType == AstType.type_basic &&
		(as_basic.isAlias || as_basic.isType); }
	bool isAlias() { return astType == AstType.type_basic && as_basic.isAlias; }

	IsSigned isSigned() {
		if (astType == AstType.type_basic) return as_basic.isSigned;
		return IsSigned.no;
	}

	AstIndex getElementType(CompilationContext* c) {
		switch(astType)
		{
			case AstType.type_ptr: return as_ptr.base;
			case AstType.type_static_array: return as_static_array.base;
			case AstType.type_slice: return as_slice.base;
			default: c.internal_error(loc, "%s is not indexable", astType); assert(false);
		}
	}

	IrIndex gen_default_value(CompilationContext* c)
	{
		switch(astType)
		{
			case AstType.type_basic: return as_basic.gen_default_value(c);
			case AstType.type_ptr: return as_ptr.gen_default_value(c);
			case AstType.type_static_array: return as_static_array.gen_default_value_static_array(c);
			case AstType.type_slice: return as_slice.gen_default_value_slice(c);
			case AstType.decl_struct: return as_struct.gen_default_value_struct(c);
			case AstType.expr_name_use: return as_name_use.entity.get_type(c).gen_default_value(c);
			default: assert(false, format("got %s", astType));
		}
	}
}

uint typeSize(AstIndex typeIndex, CompilationContext* c)
{
	return typeIndex.get_type(c).typeSize(c);
}

uint typeSize(TypeNode* type, CompilationContext* c)
{
	c.assertf(type.state >= AstNodeState.type_check_done, type.loc, "%s %s", type.typeName(c), type.state);
	switch(type.astType)
	{
		case AstType.type_basic: return type.as_basic.size;
		case AstType.type_ptr: return type.as_ptr.size;
		case AstType.type_static_array: return type.as_static_array.size(c);
		case AstType.type_slice: return type.as_slice.size;
		case AstType.decl_struct: return type.as_struct.size(c);
		case AstType.expr_name_use: return type.as_name_use.entity.typeSize(c);
		default: assert(false, format("got %s", type.astType));
	}
}

uint typeAlignment(AstIndex typeIndex, CompilationContext* c)
{
	return typeIndex.get_type(c).typeAlignment(c);
}

uint typeAlignment(TypeNode* type, CompilationContext* c)
{
	switch(type.astType)
	{
		case AstType.type_basic: return type.as_basic.alignment;
		case AstType.type_ptr: return type.as_ptr.alignment;
		case AstType.type_static_array: return type.as_static_array.alignment(c);
		case AstType.type_slice: return type.as_slice.alignment;
		case AstType.decl_struct: return type.as_struct.alignment(c);
		case AstType.expr_name_use: return type.as_name_use.entity.typeAlignment(c);
		default: assert(false, format("got %s", type.astType));
	}
}

IrArgSize typeArgSize(AstIndex typeIndex, CompilationContext* c)
{
	return typeIndex.typeSize(c).sizeToIrArgSize(c);
}

string typeName(AstIndex typeIndex, CompilationContext* c)
{
	return typeIndex.get_type(c).typeName(c);
}

string typeName(TypeNode* type, CompilationContext* c)
{
	switch(type.astType)
	{
		case AstType.type_basic:
			return type.as_basic.strId;
		case AstType.type_ptr:
			return "ptr";
		case AstType.type_static_array: return "[num]";
		case AstType.type_slice: return "[]";
		case AstType.decl_struct: return c.idString(type.as_struct.id);
		case AstType.expr_name_use: return c.idString(type.as_name_use.id(c));
		default: assert(false, format("got %s", type.astType));
	}
}

struct TypePrinter
{
	TypeNode* node;
	CompilationContext* ctx;

	void toString(scope void delegate(const(char)[]) sink) {
		if (node) node.printType(sink, ctx); else sink("<null>");
	}
}

void printType(AstIndex t, scope void delegate(const(char)[]) sink, CompilationContext* ctx) {
	if (t.isUndefined) {
		sink("?");
		return;
	}
	printType(t.get_type(ctx), sink, ctx);
}

void printType(TypeNode* t, scope void delegate(const(char)[]) sink, CompilationContext* ctx) {
	if (t is null) {
		sink("?");
		return;
	}

	switch(t.astType)
	{
		case AstType.type_basic:
			sink(basicTypeNames[t.as_basic.basicType]);
			break;
		case AstType.type_func_sig:
			FunctionSignatureNode* func_sig = t.as_func_sig;
			func_sig.returnType.get_node_type(ctx).printType(sink, ctx);
			sink(" function(");
			foreach(i, param; func_sig.parameters)
			{
				if (i > 0) sink(", ");
				param.get_node_type(ctx).printType(sink, ctx);
				sink(" ");
				sink(ctx.idString(get_node_id(param, ctx)));
			}
			sink(")");
			break;
		case AstType.type_ptr:
			t.as_ptr.base.get_node_type(ctx).printType(sink, ctx);
			sink("*");
			break;
		case AstType.type_static_array:
			t.as_static_array.base.get_node_type(ctx).printType(sink, ctx);
			formattedWrite(sink, "[%s]", t.as_static_array.length);
			break;
		case AstType.type_slice:
			t.as_slice.base.get_node_type(ctx).printType(sink, ctx);
			sink("[]");
			break;
		case AstType.decl_struct:
			sink(ctx.idString(t.as_struct.id));
			break;
		case AstType.expr_name_use:
			sink(ctx.idString(t.as_name_use.id(ctx)));
			break;
		case AstType.decl_enum:
			sink(ctx.idString(t.as_enum.id));
			break;
		default: assert(false, format("%s is not type", t.astType));
	}
}

bool same_type(AstIndex _t1, AstIndex _t2, CompilationContext* c) {
	TypeNode* t1 = c.getAstType(_t1).foldAliases(c);
	TypeNode* t2 = c.getAstType(_t2).foldAliases(c);
	assert(t1.isType, format("t1 is %s, not type", t1.astType));
	assert(t2.isType, format("t2 is %s, not type", t2.astType));

	if (t1.astType != t2.astType) {
		return false;
	}

	switch(t1.astType) with(AstType)
	{
		case type_basic:
			return t1.as_basic.basicType == t2.as_basic.basicType;
		case type_func_sig: return same_type_func_sig(t1.as_func_sig, t2.as_func_sig, c);
		case type_ptr: return same_type_ptr(t1.as_ptr, t2.as_ptr, c);
		case type_static_array: return same_type_static_array(t1.as_static_array, t2.as_static_array, c);
		case type_slice:
			return same_type_slice(t1.as_slice, t2.as_slice, c);
		case decl_struct:
			return t1 == t2;
		default:
			assert(false, format("got %s %s", t1.astType, t2.astType));
	}
}

IrIndex gen_ir_type(AstIndex typeIndex, CompilationContext* c)
{
	return gen_ir_type(c.getAst!TypeNode(typeIndex), c);
}

IrIndex gen_ir_type(TypeNode* typeNode, CompilationContext* c)
{
	switch (typeNode.astType)
	{
		case AstType.type_basic: return gen_ir_type_basic(typeNode.as_basic, c);
		case AstType.type_ptr: return gen_ir_type_ptr(typeNode.as_ptr, c);
		case AstType.type_static_array: return gen_ir_type_static_array(typeNode.as_static_array, c);
		case AstType.type_slice: return gen_ir_type_slice(typeNode.as_slice, c);
		case AstType.decl_struct: return gen_ir_type_struct(typeNode.as_struct, c);
		case AstType.type_func_sig: return gen_ir_type_func_sig(typeNode.as_func_sig, c);
		case AstType.expr_name_use: return gen_ir_type(typeNode.as_name_use.entity.get_node_type(c), c);
		default:
			c.internal_error(typeNode.loc, "Cannot convert `%s` to ir type", typeNode.astType);
			assert(false);
	}
}
