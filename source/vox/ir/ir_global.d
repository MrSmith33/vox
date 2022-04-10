/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// IR global. Stores info about global variables, string/array/struct literals.
module vox.ir.ir_global;

import vox.all;

enum IrGlobalFlags : uint {
	isMutable = 1 << 0,
	isAllZero = 1 << 1,
	needsZeroTermination = 1 << 2,
	/// Global data is copied into static data buffer (or zeroes are reserved for isAllZero)
	/// If (and only if) set, then staticBufferOffset must be valid
	isInBuffer = 1 << 3,
	isString   = 1 << 4,
}

///
@(IrValueKind.global)
struct IrGlobal
{
	/// Type of global. Must be a pointer type
	IrIndex type;
	uint numUsers;
	LinkIndex objectSymIndex;
	//bool isMutable() { return (flags & IrGlobalFlags.isMutable) != 0; }
	//bool isAllZero() { return (flags & IrGlobalFlags.isAllZero) != 0; }
	//bool needsZeroTermination() { return (flags & IrGlobalFlags.needsZeroTermination) != 0; }
	//bool isInBuffer() { return (flags & IrGlobalFlags.isInBuffer) != 0; }
	//bool isString() { return (flags & IrGlobalFlags.isString) != 0; }

	void addUser(IrIndex user) { ++numUsers; }
	void removeUser(IrIndex user) { --numUsers; }
}

///
struct IrGlobalStorage
{
	Arena!IrGlobal buffer;
	Arena!ubyte initializerBuffer;

	private IrIndex f32_sign_bit_constant;
	private IrIndex f64_sign_bit_constant;

	IrIndex get_or_add_f32_sign_bit_constant(CompilationContext* c) {
		if (f32_sign_bit_constant.isDefined) return f32_sign_bit_constant;

		IrIndex type = makeIrType(IrBasicType.f32);
		ubyte[16] buf;
		(cast(uint[])buf[])[] = 0x80000000;

		f32_sign_bit_constant = add_float_sign_bit_constant(type, buf[], c);

		return f32_sign_bit_constant;
	}

	IrIndex get_or_add_f64_sign_bit_constant(CompilationContext* c) {
		if (f64_sign_bit_constant.isDefined) return f64_sign_bit_constant;

		IrIndex type = makeIrType(IrBasicType.f64);
		ubyte[16] buf;
		(cast(ulong[])buf[])[] = 0x8000000000000000;

		f64_sign_bit_constant = add_float_sign_bit_constant(type, buf[], c);

		return f64_sign_bit_constant;
	}

	// Creates a bit pattern constant that is used to flip a bit sign of xmm register
	private IrIndex add_float_sign_bit_constant(IrIndex type, ubyte[] initializer, CompilationContext* c) {
		IrIndex globalIndex = add();
		IrGlobal* global = get(globalIndex);
		global.type = c.types.appendPtr(type);

		ObjectSymbol sym = {
			kind : ObjectSymbolKind.isLocal,
			sectionIndex : c.builtinSections[ObjectSectionType.ro_data],
			moduleIndex : c.builtinModuleIndex,
			flags : ObjectSymbolFlags.isFloat,
			id : c.idMap.getOrReg(c, ":float"),
		};
		global.objectSymIndex = c.objSymTab.addSymbol(sym);

		ObjectSymbol* globalSym = c.objSymTab.getSymbol(global.objectSymIndex);
		globalSym.alignmentPower = 4;

		SizeAndAlignment valueSizealign = c.types.typeSizeAndAlignment(type);
		ubyte[] buffer = c.globals.allocateInitializer(16);
		buffer[] = initializer;
		globalSym.setInitializer(buffer);

		return globalIndex;
	}

	///
	IrIndex add()
	{
		IrIndex globalIndex = IrIndex(cast(uint)buffer.length, IrValueKind.global);
		buffer.put(IrGlobal());
		return globalIndex;
	}

	/// Allocate space for compiletime generated initializers (struct initializers for example)
	/// String literals without escape sequences are sliced directly from source code
	ubyte[] allocateInitializer(uint length)
	{
		return initializerBuffer.voidPut(length);
	}

	///
	IrGlobal* get(IrIndex index)
	{
		assert(index.kind == IrValueKind.global);
		assert(index.storageUintIndex < buffer.length);
		return &buffer.data[index.storageUintIndex];
	}
}
