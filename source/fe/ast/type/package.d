/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.type;

import all;

public import fe.ast.type.basic;
public import fe.ast.type.ptr;
public import fe.ast.type.slice;
public import fe.ast.type.static_array;


enum POINTER_SIZE = 8;

BasicTypeNode* cast_type_basic(AstNode* t) { if (t.astType == AstType.type_basic) return cast(BasicTypeNode*)t; return null; }
PtrTypeNode* cast_type_ptr(AstNode* t) { if (t.astType == AstType.type_ptr) return cast(PtrTypeNode*)t; return null; }
SliceTypeNode* cast_type_slice(AstNode* t) { if (t.astType == AstType.type_slice) return cast(SliceTypeNode*)t; return null; }
StaticArrayTypeNode* cast_type_static_array(AstNode* t) { if (t.astType == AstType.type_static_array) return cast(StaticArrayTypeNode*)t; return null; }

TypeNode* cast_type_node(AstNode* node) {
	assert(node);
	assert(node.isType, format("%s", node.astType));
	return cast(TypeNode*)node;
}

struct TypeNode
{
	AstNode base;
	alias base this;

	BasicTypeNode* as_basic() { if (astType == AstType.type_basic) return cast(BasicTypeNode*)&this; return null; }
	PtrTypeNode* as_ptr() { if (astType == AstType.type_ptr) return cast(PtrTypeNode*)&this; return null; }
	SliceTypeNode* as_slice() { if (astType == AstType.type_slice) return cast(SliceTypeNode*)&this; return null; }
	StaticArrayTypeNode* as_static_array() { if (astType == AstType.type_static_array) return cast(StaticArrayTypeNode*)&this; return null; }
	StructDeclNode* as_struct() { if (astType == AstType.decl_struct) return cast(StructDeclNode*)&this; return null; }
	NameUseExprNode* as_name_use() { if (astType == AstType.expr_type_name_use) return cast(NameUseExprNode*)&this; return null; }
	EnumDeclaration* as_enum() { if (astType == AstType.decl_enum) return cast(EnumDeclaration*)&this; return null; }

	TypeNode* foldAliases() {
		if (astType == AstType.expr_type_name_use) return as_name_use.entity.cast_type_node;
		return &this;
	}

	uint alignment()
	{
		switch(astType)
		{
			case AstType.type_basic: return as_basic.alignment;
			case AstType.type_ptr: return as_ptr.alignment;
			case AstType.type_static_array: return as_static_array.alignment;
			case AstType.type_slice: return as_slice.alignment;
			case AstType.decl_struct: return as_struct.alignment;
			case AstType.expr_type_name_use: return as_name_use.entity.cast_type_node.alignment;
			default: assert(false, format("got %s", astType));
		}
	}

	uint size()
	{
		switch(astType)
		{
			case AstType.type_basic: return as_basic.size;
			case AstType.type_ptr: return as_ptr.size;
			case AstType.type_static_array: return as_static_array.size;
			case AstType.type_slice: return as_slice.size;
			case AstType.decl_struct: return as_struct.size;
			case AstType.expr_type_name_use: return as_name_use.entity.cast_type_node.size;
			default: assert(false, format("got %s", base.astType));
		}
	}

	IrArgSize argSize(CompilationContext* context)
	{
		return sizeToIrArgSize(size, context);
	}

	string typeName(CompilationContext* context) {
		assert(isType);
		switch(astType)
		{
			case AstType.type_basic:
				return as_basic.strId;
			case AstType.type_ptr:
				return "ptr";
			case AstType.type_static_array: return "[num]";
			case AstType.type_slice: return "[]";
			case AstType.decl_struct: return context.idString(as_struct.id);
			case AstType.expr_type_name_use: return context.idString(as_name_use.id);
			default: assert(false, format("got %s", astType));
		}
	}

	bool isOpaqueStruct() {
		TypeNode* t = &this;
		if (t.astType == AstType.expr_type_name_use) t = t.as_name_use.entity.cast_type_node;
		return t.astType == AstType.decl_struct && t.as_struct.isOpaque;
	}

	TypePrinter printer(CompilationContext* context) {
		return TypePrinter(&this, context);
	}

	bool isPassByPtr() {
		return astType == AstType.decl_struct || astType == AstType.type_slice;
	}

	bool isTypeofNull() {
		return astType == AstType.type_basic &&
			as_basic.basicType == BasicType.t_null;
	}
	bool isTypeBasic() {
		return astType == AstType.type_basic;
	}
	bool isVoid() {
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

	bool isUnsigned() {
		return astType == AstType.type_basic && as_basic.isUnsigned;
	}

	TypeNode* getElementType(CompilationContext* context) {
		switch(astType)
		{
			case AstType.type_ptr: return as_ptr.base;
			case AstType.type_static_array: return as_static_array.base;
			case AstType.type_slice: return as_slice.base;
			default: context.internal_error(loc, "%s is not indexable", astType); assert(false);
		}
	}
}

/// Used inside MemberExprNode.memberIndex
enum BuiltinMemberIndex : uint {
	MEMBER_MIN,
	MEMBER_MAX,
	MEMBER_PTR,
	MEMBER_LENGTH,
}

struct TypePrinter
{
	TypeNode* node;
	CompilationContext* ctx;

	void toString(scope void delegate(const(char)[]) sink) {
		if (node) node.printType(sink, ctx); else sink("<null>");
	}
}

void printType(TypeNode* t, scope void delegate(const(char)[]) sink, CompilationContext* ctx) {
	switch(t.astType)
	{
		case AstType.type_basic:
			sink(basicTypeNames[t.as_basic.basicType]);
			break;
		case AstType.type_ptr:
			t.as_ptr.base.printType(sink, ctx);
			sink("*");
			break;
		case AstType.type_static_array:
			t.as_static_array.base.printType(sink, ctx);
			formattedWrite(sink, "[%s]", t.as_static_array.length);
			break;
		case AstType.type_slice:
			t.as_slice.base.printType(sink, ctx);
			sink("[]");
			break;
		case AstType.decl_struct:
			sink(ctx.idString(t.as_struct.id));
			break;
		case AstType.expr_type_name_use:
			sink(ctx.idString(t.as_name_use.id));
			break;
		case AstType.expr_name_use:
			sink(ctx.idString(t.as_node.cast_expr_name_use.id));
			break;
		default: assert(false, format("%s is not type", t.astType));
	}
}

bool same_type(TypeNode* _t1, TypeNode* _t2) {
	TypeNode* t1 = _t1.foldAliases;
	TypeNode* t2 = _t2.foldAliases;
	assert(t1.isType, format("t1 is %s, not type", t1.astType));
	assert(t2.isType, format("t2 is %s, not type", t2.astType));

	if (t1.astType != t2.astType) {
		return false;
	}

	switch(t1.astType) with(AstType)
	{
		case type_basic:
			return t1.as_basic.basicType == t2.as_basic.basicType;
		case type_ptr: return same_type_ptr(t1.as_ptr, t2.as_ptr);
		case type_static_array: return same_type_static_array(t1.as_static_array, t2.as_static_array);
		case type_slice:
			return same_type_slice(t1.as_slice, t2.as_slice);
		case decl_struct:
			return t1 == t2;
		default:
			assert(false, format("got %s %s", t1.astType, t2.astType));
	}
}

IrIndex gen_ir_type(TypeNode* t, CompilationContext* context) {
	switch (t.astType)
	{
		case AstType.type_basic: return gen_ir_type_basic(t.as_basic, context);
		case AstType.type_ptr: return gen_ir_type_ptr(t.as_ptr, context);
		case AstType.type_static_array: return gen_ir_type_static_array(t.as_static_array, context);
		case AstType.type_slice: return gen_ir_type_slice(t.as_slice, context);
		case AstType.decl_struct: return gen_ir_type_struct(t.as_struct, context);
		case AstType.expr_type_name_use: return gen_ir_type(t.as_name_use.entity.cast_type_node, context);
		default:
			context.internal_error(t.loc, "Cannot convert `%s` to ir type", t.astType);
			assert(false);
	}
}
