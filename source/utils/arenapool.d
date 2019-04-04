/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module utils.arenapool;

///
struct ArenaPool
{
	import utils : alignValue;
	import core.sys.windows.windows : VirtualAlloc, MEM_RESERVE, PAGE_NOACCESS, VirtualFree, MEM_DECOMMIT;

	enum PAGE_SIZE = 65536;
	ubyte[] buffer;
	size_t takenBytes;

	void reserve(size_t size) {
		size_t reservedBytes = alignValue(size, PAGE_SIZE); // round up to page size
		ubyte* ptr = cast(ubyte*)VirtualAlloc(null, reservedBytes, MEM_RESERVE, PAGE_NOACCESS);
		assert(ptr !is null, "VirtualAlloc failed");
		buffer = ptr[0..reservedBytes];
	}

	ubyte[] take(size_t numBytes) {
		if (numBytes == 0) return null;
		ubyte[] result = buffer[takenBytes..takenBytes+numBytes];
		takenBytes += numBytes;
		return result;
	}

	void decommitAll() {
		int res = VirtualFree(buffer.ptr, buffer.length, MEM_DECOMMIT);
		assert(res != 0, "VirtualFree failed");
	}
}
