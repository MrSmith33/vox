/// Copyright: Copyright (c) 2022 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
import libvox;

// Example that uses libvox to compile Vox source code into an executable file
extern(C) void main()
{
	import core.stdc.stdio : printf;

	// Allocates the state of Vox compiler and allocates memory for arenas
	VoxCompiler* c = vox_init();

	// Resets and setups state for a new compilation
	// Previously compiled program will be cleared from memory
	vox_begin_compilation(c);

	// --- Now set all compilation settings, add modules, register host symbols ---

	// Switch to EXE mode
	vox_set_passes_exe(c);

	// Set name of the executable file
	vox_set_output_filename(c, SliceString("main.exe"));

	// Set target OS (you can do cross-compilation if you select OS different from host OS)
	vox_set_target_os(c, TargetOs.windows);

	// Add Vox module to the compilation
	vox_add_module(c, SliceString("main.vx"), SliceString(src));

	// --- End of setup ---

	// Runs all compilation passes
	int status = vox_compile(c);
	printf("exe status %d\n", status);

	// We are done with our program, we can free the memory
	vox_free(c);
}

// Cross-platform Vox hello world source code
immutable src = q{
	#version(windows) {
		@extern(module, "kernel32"):

		enum u32 stdin  = 0xFFFFFFF6;
		enum u32 stdout = 0xFFFFFFF5;
		enum u32 stderr = 0xFFFFFFF4;

		noreturn ExitProcess(u32 uExitCode);
		u8 WriteConsoleA(void* hConsoleOutput, u8* lpBuffer, u32 nNumberOfCharsToWrite, u32* lpNumberOfCharsWritten, void* lpReserved);
		void* GetStdHandle(u32 nStdHandle);

		alias exit = ExitProcess;

		void write(u32 fd, u8[] data) {
			WriteConsoleA(GetStdHandle(fd), data.ptr, cast(u32)data.length, null, null);
		}
	}

	#version(linux) {
		enum u32 stdin  = 0;
		enum u32 stdout = 1;
		enum u32 stderr = 2;

		@extern(syscall, 60)
		void exit(i32 error_code);

		@extern(syscall, 1)
		void sys_write(u32 fd, u8* buf, u64 count);

		void write(u32 fd, u8[] data) {
			sys_write(fd, data.ptr, data.length);
		}
	}

	void main(void* hInstance, void* hPrevInstance, u8* lpCmdLine, i32 nShowCmd)
	{
		#version(windows) u8[] msg = "hello windows\n";
		#version(linux) u8[] msg = "hello linux\n";
		write(stdout, msg);
		exit(0);
	}
};
