/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module ast.type;

import std.format : formattedWrite;
import std.string : format;
import all;

mixin template TypeNodeData(AstType _astType) {
	mixin AstNodeData!(_astType, AstFlags.isType);
}

struct TypePrinter
{
	TypeNode* node;
	CompilationContext* ctx;

	void toString(scope void delegate(const(char)[]) sink) {
		if (node) node.toString(sink, ctx); else sink("<null>");
	}
}

struct TypeNode {
	mixin AstNodeData!(AstType.abstract_node, AstFlags.isType);

	BasicTypeNode* basicTypeNode() { return cast(BasicTypeNode*)&this; }
	PtrTypeNode* ptrTypeNode() { return cast(PtrTypeNode*)&this; }
	SliceTypeNode* sliceTypeNode() { return cast(SliceTypeNode*)&this; }
	StaticArrayTypeNode* staticArrayTypeNode() { return cast(StaticArrayTypeNode*)&this; }
	StructTypeNode* structTypeNode() { return cast(StructTypeNode*)&this; }

	uint alignment()
	{
		switch(astType)
		{
			case AstType.type_basic: return basicTypeNode.alignment;
			case AstType.type_ptr: return ptrTypeNode.alignment;
			case AstType.type_static_array: return staticArrayTypeNode.alignment;
			case AstType.type_slice: return sliceTypeNode.alignment;
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
			case AstType.type_slice: return sliceTypeNode.size;
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
			case AstType.type_slice: return "[]";
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
				case BasicType.t_u32: return true;
				case BasicType.t_u64: return true;
				default: return false;
			}

			case AstType.type_ptr: return true;
			case AstType.type_slice: return true;
			case AstType.type_struct: return true;

			default: return false;
		}
	}

	TypeNode* getElementType(CompilationContext* context) {
		switch(astType)
		{
			case AstType.type_ptr: return ptrTypeNode.base;
			case AstType.type_static_array: return staticArrayTypeNode.base;
			case AstType.type_slice: return sliceTypeNode.base;
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
			case AstType.type_slice:
				sliceTypeNode.base.toString(sink, ctx);
				sink("[]");
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
		case AstType.type_slice: return genIrType(t.sliceTypeNode, context);
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

IrIndex genIrType(StructTypeNode* t, CompilationContext* context)
	out(res; res.isTypeStruct, "Not a struct type")
{
	StructDeclNode* s = t.getSym.structDecl;

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

bool sameType(TypeNode* t1, TypeNode* t2) {
	assert(t1.isType, format("t1 is %s, not type", t1.astType));
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
		case type_slice:
			return sameType(t1.sliceTypeNode, t2.sliceTypeNode);
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

struct SliceTypeNode {
	mixin TypeNodeData!(AstType.type_slice);
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
