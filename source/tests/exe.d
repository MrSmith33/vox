/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module tests.exe;

import tester;

Test[] exeTests() { return collectTests!(tests.exe)(); }

@TestInfo()
immutable exe1 = q{--- exe1
	i32 glob;
	// Test exe creation. no static data, no imports
	i32 main(void* hInstance, void* hPrevInstance, u8* lpCmdLine, i32 nShowCmd) {
		return glob;
	}
};


@TestInfo(null, null, [DllModule("kernel32", ["WriteConsoleA", "GetStdHandle"])])
immutable exe2 = q{--- exe2
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
