/// Copyright: Copyright (c) 2022 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.utils.windows;

extern(C) version(Windows) nothrow:

void* VirtualAlloc(void* lpAddress, size_t dwSize, uint flAllocationType, uint flProtect);
bool VirtualFree(void* lpAddress, size_t dwSize, uint dwFreeType);
bool VirtualProtect(void* lpAddress, size_t dwSize, uint flNewProtect, uint* lpflOldProtect);
bool FlushInstructionCache(void* hProcess, void* lpBaseAddress, size_t dwSize);
uint GetLastError() @trusted;
void* GetCurrentProcess();

enum : uint {
	PAGE_NOACCESS          = 0x0001,
	PAGE_READONLY          = 0x0002,
	PAGE_READWRITE         = 0x0004,
	PAGE_WRITECOPY         = 0x0008,
	PAGE_EXECUTE           = 0x0010,
	PAGE_EXECUTE_READ      = 0x0020,
	PAGE_EXECUTE_READWRITE = 0x0040,
	PAGE_EXECUTE_WRITECOPY = 0x0080,
	PAGE_GUARD             = 0x0100,
	PAGE_NOCACHE           = 0x0200,
}

enum : uint {
	MEM_COMMIT      = 0x00001000,
	MEM_RESERVE     = 0x00002000,
	MEM_DECOMMIT    = 0x00004000,
	MEM_RELEASE     = 0x00008000,
	MEM_FREE        = 0x00010000,
	MEM_PRIVATE     = 0x00020000,
	MEM_MAPPED      = 0x00040000,
	MEM_RESET       = 0x00080000,
	MEM_TOP_DOWN    = 0x00100000,
	MEM_WRITE_WATCH = 0x00200000,
	MEM_PHYSICAL    = 0x00400000,
	MEM_4MB_PAGES   = 0x80000000,
}
