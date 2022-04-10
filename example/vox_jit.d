/// Copyright: Copyright (c) 2022 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
import libvox;

extern(C):

// Example that uses libvox to compile Vox source code into an jit mode and call it
void main()
{
	import core.stdc.stdio : printf;

	// Allocates the state of Vox compiler and allocates memory for arenas
	VoxCompiler* c = vox_init();

	// Resets and setups state for a new compilation
	// Previously compiled program will be cleared from memory
	vox_begin_compilation(c);

	// --- Now set all compilation settings, add modules, register host symbols ---

	// Switch to JIT mode
	vox_set_passes_jit(c);

	// Add Vox module to the compilation
	vox_add_module(c, SliceString("main.vx"), SliceString(src));

	// Intern the string into identifier
	Identifier host_id = vox_id_get_or_reg(c, SliceString("host"));
	// Create host module to add host symbols to
	LinkIndex host_mod = vox_get_or_create_external_module(c, host_id);

	Identifier print_id = vox_id_get_or_reg(c, SliceString("print"));
	// Add host function `host_print` to the `host` module
	vox_reg_host_symbol(c, host_mod, print_id, cast(void*)&host_print);

	// --- End of setup ---

	// Runs all compilation passes
	int status = vox_compile(c);
	printf("jit status %d\n", status);

	// Make code pages executable
	vox_make_code_executable(c);

	Identifier vox_hello_id = vox_id_get_or_reg(c, SliceString("main.vox_hello"));
	// Search `main` module for `vox_hello` function and retrieve its address in memory
	void* ptr = vox_find_function(c, vox_hello_id);

	if (ptr) {
		// Such function exists, call it. It follows C ABI
		auto func = cast(void function())ptr;
		func();
	} else {
		printf("Cannot find main.vox_hello\n");
	}

	// We are done with our program, we can free the memory
	vox_free(c);
}

// Our host function we want guest program to see and call
void host_print(SliceString str) {
	import core.stdc.stdio : printf;
	printf(str.ptr);
}

// Source code of a single module
immutable src = q{
	@extern(module, "host") void print(u8[]);
	void vox_hello() {
		print("Hello from Vox");
	}
};
