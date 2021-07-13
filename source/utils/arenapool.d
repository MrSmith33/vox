/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module utils.arenapool;

version(Posix) extern (C) int getpagesize();

// https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/mmap.2.html
// https://stackoverflow.com/questions/21809072/virtual-memory-on-osx-ios-versus-windows-commit-reserve-behaviour

///
struct ArenaPool
{
	import utils : alignValue;
	import std.format;
	import std.stdio;
	import std.string : fromStringz;
	import core.stdc.errno : errno;
	import core.stdc.string : strerror;

	enum PAGE_SIZE = 65_536;
	ubyte[] buffer;
	size_t takenBytes;

	void reserve(size_t size) {
		size_t reservedBytes = alignValue(size, PAGE_SIZE); // round up to page size
		version(Posix) {
			import core.sys.posix.sys.mman : mmap, MAP_ANON, PROT_READ, PROT_WRITE, PROT_EXEC, MAP_PRIVATE, MAP_FAILED;
			enum MAP_NORESERVE = 0x4000;
			auto flags = MAP_PRIVATE | MAP_ANON;
			version(linux) flags |= MAP_NORESERVE;
			ubyte* ptr = cast(ubyte*)mmap(null, reservedBytes, PROT_READ | PROT_WRITE, flags, -1, 0);
			assert(ptr != MAP_FAILED, format("mmap failed, errno %s, %s: requested %s bytes", errno, strerror(errno).fromStringz, size));
		} else version(Windows) {
			import core.sys.windows.windows : VirtualAlloc, MEM_RESERVE, PAGE_NOACCESS;
			ubyte* ptr = cast(ubyte*)VirtualAlloc(null, reservedBytes, MEM_RESERVE, PAGE_NOACCESS);
			assert(ptr !is null, format("VirtualAlloc failed: requested %s bytes", size));
		}
		buffer = ptr[0..reservedBytes];
	}

	ubyte[] take(size_t numBytes) {
		if (numBytes == 0) return null;
		ubyte[] result = buffer[takenBytes..takenBytes+numBytes];
		takenBytes += numBytes;
		return result;
	}

	void decommitAll() {
		version(Posix) {
			import core.sys.posix.sys.mman : munmap;
			if (buffer.ptr is null) return;
			int res = munmap(buffer.ptr, buffer.length);
			assert(res == 0, format("munmap(%X, %s) failed, %s, %s", buffer.ptr, buffer.length, errno, strerror(errno).fromStringz));
		} else version(Windows) {
			import core.sys.windows.windows : VirtualFree, MEM_DECOMMIT;
			int res = VirtualFree(buffer.ptr, buffer.length, MEM_DECOMMIT);
			assert(res != 0, "VirtualFree failed");
		}
	}
}
