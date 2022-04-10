/// Copyright: Copyright (c) 2022 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module libvox;

// SliceString that are passed to Vox compiler must remain valid until vox_free

extern(C) nothrow {
	// -- Init libvox
	VoxCompiler* vox_init();
	// -- Free libvox
	void vox_free(VoxCompiler* compiler);

	// -- Reset function. Needs to be run before compiling a new program
	// Previously compiled program will be lost
	void vox_begin_compilation(VoxCompiler* compiler);

	// -- Setup functions
	// Should be run after vox_init -> vox_begin_compilation and before `vox_compile`
	void vox_set_passes_exe(VoxCompiler* compiler);
	void vox_set_passes_jit(VoxCompiler* compiler);
	void vox_set_target_os(VoxCompiler* compiler, TargetOs os);
	void vox_set_output_filename(VoxCompiler* compiler, SliceString outFile);
	Identifier vox_id_get_or_reg(VoxCompiler* compiler, SliceString hostModuleName);
	SliceString vox_id_get_string(VoxCompiler* compiler, Identifier id);
	Identifier vox_id_get_parent(VoxCompiler* compiler, Identifier id);
	LinkIndex vox_get_or_create_external_module(VoxCompiler* compiler, Identifier modId);
	// hostModuleIndex is the one returned by `vox_get_or_create_external_module` function
	void vox_reg_host_symbol(VoxCompiler* compiler, LinkIndex hostModuleIndex, Identifier symId, void* ptr);
	void vox_add_module(VoxCompiler* compiler, SliceString fileName, SliceString filedata);
	// -- Setup functions end

	// -- Runs compilation
	// Should be run after vox_init -> vox_begin_compilation -> setup
	// 0 means no errors, non-zero indicates errors
	int vox_compile(VoxCompiler* compiler);

	// -- Those functions can be called after successfull `vox_compile`
	void vox_make_code_executable(VoxCompiler* compiler);
	void* vox_find_function(VoxCompiler* compiler, Identifier funcId);
}

struct VoxCompiler;
struct Slice(T) {
	this(T[] data) {
		ptr = data.ptr;
		length = data.length;
	}
	ulong length;
	T* ptr;
	T[] slice() { return ptr[0..length]; }
	alias slice this;
}
alias SliceString = Slice!(const(char));
alias Identifier = uint;
alias LinkIndex = uint;

enum TargetOs : ubyte {
	windows,
	linux,
	macos,
}
