/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// IR global. Stores info about global variables, string/array/struct literals.
module ir.ir_global;

import all;

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
