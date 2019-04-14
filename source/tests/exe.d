/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module tests.exe;

import tester;

Test[] exeTests() { return [test28, test29]; }

immutable input28 = q{--- test28
	i32 main(void* hInstance, void* hPrevInstance, u8* lpCmdLine, i32 nShowCmd) {
		return 0;
	}
};
auto test28 = Test("exe no static data, no imports", input28);

immutable input29 = q{--- test29
	u8 WriteConsoleA(
		void* hConsoleOutput,
		u8* lpBuffer,
		u32 nNumberOfCharsToWrite,
		u32* lpNumberOfCharsWritten,
		u64 lpReserved
	);
	void* GetStdHandle(u32 nStdHandle);
	i32 main(void* hInstance, void* hPrevInstance, u8* lpCmdLine, i32 nShowCmd) {
		u8[] array = "Hello world";
		u32 numWritten;
		void* handle = GetStdHandle(0xFFFFFFF5); // STD_OUTPUT_HANDLE
		WriteConsoleA(handle, array.ptr, cast(u32)array.length, &numWritten, 0);
		return 0;
	}
};
auto test29 = Test("exe", input29, null, null, null,
	[DllModule("kernel32", ["WriteConsoleA", "GetStdHandle"])]);

