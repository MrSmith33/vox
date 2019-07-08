/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.ast.type;

import std.format : formattedWrite;
import std.string : format;
import all;

BasicTypeNode* cast_type_basic(AstNode* t) { if (t.astType == AstType.type_basic) return cast(BasicTypeNode*)t; return null; }
PtrTypeNode* cast_type_ptr(AstNode* t) { if (t.astType == AstType.type_ptr) return cast(PtrTypeNode*)t; return null; }
SliceTypeNode* cast_type_slice(AstNode* t) { if (t.astType == AstType.type_slice) return cast(SliceTypeNode*)t; return null; }
StaticArrayTypeNode* cast_type_static_array(AstNode* t) { if (t.astType == AstType.type_static_array) return cast(StaticArrayTypeNode*)t; return null; }


struct TypePrinter
{
	TypeNode* node;
	CompilationContext* ctx;

	void toString(scope void delegate(const(char)[]) sink) {
		if (node) node.printType(sink, ctx); else sink("<null>");
	}
}

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

	void printType(scope void delegate(const(char)[]) sink, CompilationContext* ctx) {
		switch(astType)
		{
			case AstType.type_basic:
				sink(basicTypeNames[as_basic.basicType]);
				break;
			case AstType.type_ptr:
				as_ptr.base.printType(sink, ctx);
				sink("*");
				break;
			case AstType.type_static_array:
				as_static_array.base.printType(sink, ctx);
				formattedWrite(sink, "[%s]", as_static_array.length);
				break;
			case AstType.type_slice:
				as_slice.base.printType(sink, ctx);
				sink("[]");
				break;
			case AstType.decl_struct:
				sink(ctx.idString(as_struct.id));
				break;
			case AstType.expr_type_name_use:
				sink(ctx.idString(as_name_use.id));
				break;
			case AstType.expr_name_use:
				sink(ctx.idString(as_node.cast_expr_name_use.id));
				break;
			default: assert(false, format("%s is not type", astType));
		}
	}
}

IrIndex genIrType(TypeNode* t, CompilationContext* context) {
	switch (t.astType)
	{
		case AstType.type_basic: return genIrType(t.as_basic, context);
		case AstType.type_ptr: return genIrType(t.as_ptr, context);
		case AstType.type_static_array: return genIrType(t.as_static_array, context);
		case AstType.type_slice: return genIrType(t.as_slice, context);
		case AstType.decl_struct: return genIrType(t.as_struct, context);
		case AstType.expr_type_name_use: return genIrType(t.as_name_use.entity.cast_type_node, context);
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
		case BasicType.t_bool: return makeBasicTypeIndex(IrValueType.i8);
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

// slice is lowered into struct with two members
IrIndex genIrType(SliceTypeNode* t, CompilationContext* context)
	out(res; res.isTypeStruct, "Not a struct type")
{
	if (t.irType.isDefined) return t.irType;

	t.irType = context.types.appendStruct(2);
	IrTypeStruct* structType = &context.types.get!IrTypeStruct(t.irType);
	IrIndex baseType = t.base.genIrType(context);
	// length
	structType.members[0] = IrTypeStructMember(makeBasicTypeIndex(IrValueType.i64), 0);
	// ptr
	structType.members[1] = IrTypeStructMember(context.types.appendPtr(baseType), POINTER_SIZE);
	structType.size = t.size;
	structType.alignment = POINTER_SIZE;
	return t.irType;
}

IrIndex genIrType(StructDeclNode* s, CompilationContext* context)
	out(res; res.isTypeStruct, "Not a struct type")
{
	if (s.irType.isDefined) return s.irType;

	uint numFields = 0;
	foreach(AstNode* member; s.declarations) {
		if (member.astType == AstType.decl_var)
			++numFields;
	}

	s.irType = context.types.appendStruct(numFields);
	IrTypeStruct* structType = &context.types.get!IrTypeStruct(s.irType);
	IrTypeStructMember[] members = structType.members;

	uint memberIndex;
	uint memberOffset;
	uint maxAlignment = 1;
	foreach(AstNode* member; s.declarations)
	{
		if (member.astType == AstType.decl_var)
		{
			IrIndex type = (cast(VariableDeclNode*)member).type.genIrType(context);
			uint memberSize = context.types.typeSize(type);
			uint memberAlignment = context.types.typeAlignment(type);
			maxAlignment = max(maxAlignment, memberAlignment);
			memberOffset = alignValue!uint(memberOffset, memberAlignment);
			members[memberIndex++] = IrTypeStructMember(type, memberOffset);
			memberOffset += memberSize;
		}
	}

	memberOffset = alignValue!uint(memberOffset, maxAlignment);
	structType.size = memberOffset;
	structType.alignment = maxAlignment;
	return s.irType;
}

IrIndex genIrType(FunctionDeclNode* f, CompilationContext* context)
	out(res; res.isTypeFunction, "Not a function type")
{
	if (f.backendData.irType.isDefined) return f.backendData.irType;

	uint numResults = 0;
	if (!f.returnType.isVoid) numResults = 1;

	f.backendData.irType = context.types.appendFuncSignature(numResults, f.parameters.length);
	auto funcType = &context.types.get!IrTypeFunction(f.backendData.irType);

	if (numResults == 1) {
		IrIndex returnType = f.returnType.genIrType(context);
		funcType.resultTypes[0] = returnType;
	}

	IrIndex[] parameterTypes = funcType.parameterTypes;
	foreach(i, VariableDeclNode* parameter; f.parameters) {
		parameterTypes[i] = parameter.type.genIrType(context);
	}

	return f.backendData.irType;
}

bool sameType(TypeNode* _t1, TypeNode* _t2) {
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
		case type_ptr: return sameType(t1.as_ptr, t2.as_ptr);
		case type_static_array: return sameType(t1.as_static_array, t2.as_static_array);
		case type_slice:
			return sameType(t1.as_slice, t2.as_slice);
		case decl_struct:
			return t1 == t2;
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
enum IrArgSize POINTER_ARG_SIZE = IrArgSize.size64;
BasicTypeNode basicTypeNode(uint size, BasicType basicType, int typeFlags = 0)
{
	return BasicTypeNode(TokenIndex(), size, basicType, cast(ubyte)typeFlags);
}

struct BasicTypeNode {
	mixin AstNodeData!(AstType.type_basic, AstFlags.isType);
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
	mixin AstNodeData!(AstType.type_ptr, AstFlags.isType);
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	TypeNode* base;
	IrIndex irType;
	uint size() { return POINTER_SIZE; }
	uint alignment() { return POINTER_SIZE; }
	bool isVoidPtr() { return base.isVoid; }
}

bool sameType(PtrTypeNode* t1, PtrTypeNode* t2)
{
	return sameType(t1.base, t2.base);
}

struct StaticArrayTypeNode {
	mixin AstNodeData!(AstType.type_static_array, AstFlags.isType);
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

struct SliceTypeNode {
	mixin AstNodeData!(AstType.type_slice, AstFlags.isType);
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	TypeNode* base;
	IrIndex irType;

	uint size() { return POINTER_SIZE * 2; }
	uint alignment() { return POINTER_SIZE; }
}

bool sameType(SliceTypeNode* t1, SliceTypeNode* t2)
{
	return sameType(t1.base, t2.base);
}
