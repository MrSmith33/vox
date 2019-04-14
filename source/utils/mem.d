/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module utils.mem;

import utils : PAGE_SIZE, format;

enum MemType
{
	RW,
	RWX
}

ubyte[] alloc_executable_memory(size_t bytes)
{
	return allocate(bytes, cast(void*)0x4000_0000UL, MemType.RWX);
}

bool free_executable_memory(ubyte[] bytes)
{
	return deallocate(bytes);
}

version(Posix)
{
	ubyte[] allocate(size_t bytes, void* location, bool is_executable)
	{
		import core.sys.posix.sys.mman : mmap, MAP_ANON, PROT_READ,
			PROT_WRITE, PROT_EXEC, MAP_PRIVATE, MAP_FAILED;
		if (!bytes) return null;

		int protection = PROT_READ | PROT_WRITE | (is_executable ? PROT_EXEC : 0);

		auto p = mmap(location, bytes, protection, MAP_PRIVATE | MAP_ANON, -1, 0);
		if (p is MAP_FAILED) return null;
		return cast(ubyte[])p[0 .. bytes];
	}

	bool deallocate(ubyte[] b)
	{
		import core.sys.posix.sys.mman : munmap;
		if (b.ptr) munmap(b.ptr, b.length) == 0 || assert(0);
		return true;
	}
}
else version(Windows)
{
	import core.sys.windows.windows :
		FlushInstructionCache, GetLastError, GetCurrentProcess,
		VirtualAlloc, VirtualFree, VirtualProtect,
		MEM_COMMIT, PAGE_READWRITE, MEM_RELEASE, PAGE_EXECUTE_READWRITE, MEM_RESERVE, PAGE_EXECUTE;

	ubyte[] allocate(size_t bytes, void* location, MemType memoryType)
	{
		if (!bytes) return null;

		int protection;

		final switch(memoryType)
		{
			case MemType.RW:  protection = PAGE_READWRITE; break;
			case MemType.RWX: protection = PAGE_EXECUTE_READWRITE; break;
		}

		auto p = VirtualAlloc(location, bytes, MEM_COMMIT | MEM_RESERVE, protection);

		if (p == null)
		{
			import std.stdio;
			import std.windows.syserror;
			int errCode = GetLastError();
			writefln("allocate(%s:bytes, %s:location, %s:memoryType", bytes, location, memoryType);
			writeln(sysErrorString(errCode));
			assert(false, "VirtualAlloc alloc failed");
			return null;
		}

		return cast(ubyte[])p[0 .. bytes];
	}

	bool deallocate(ubyte[] b)
	{
		return b.ptr is null || VirtualFree(b.ptr, 0, MEM_RELEASE) != 0;
	}

	void markAsRW(void* addr, size_t numPages)
	{
		uint val;
		VirtualProtect(addr, numPages*PAGE_SIZE, PAGE_READWRITE, &val);
	}

	void markAsExecutable(void* addr, size_t numPages)
	{
		if (numPages == 0) return;
		uint val;
		int res = VirtualProtect(addr, numPages*PAGE_SIZE, PAGE_EXECUTE, &val);
		assert(res != 0, format("VirtualProtect(%X, %s, PAGE_EXECUTE, %s) failed", addr, numPages*PAGE_SIZE, val));
		FlushInstructionCache(GetCurrentProcess(), addr, numPages*PAGE_SIZE);
	}

	void testAdresses()
	{
		import std.stdio;
		import std.windows.syserror;
		import core.sys.windows.windows;
		size_t successful;
		size_t failed;
		size_t bytes = PAGE_SIZE * 1024;
		foreach(ulong loc; 0..16 * 16)
		{
			void* location = cast(void*)(loc*64*1024*1024);
			auto p = VirtualAlloc(location, bytes, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);

			if (p == null)
			{
				int errCode = GetLastError();
				writefln("Fail loc %s err '%s'", location, sysErrorString(errCode));
				++failed;
			}
			else
			{
				++successful;
				VirtualFree(p, 0, MEM_RELEASE);
				writefln("Success loc %s ptr %s", location, p);
			}
		}

		writefln("s %s", successful);
		writefln("f %s", failed);
	}
}
