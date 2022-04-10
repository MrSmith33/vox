import vox.all;
import core.runtime : Runtime;
import core.stdc.stdlib : malloc, free;

extern(C) export:

struct VoxCompiler {
	Driver driver;
}

VoxCompiler* vox_init() {
	Runtime.initialize();

	auto compiler = cast(VoxCompiler*)malloc(VoxCompiler.sizeof);
	*compiler = VoxCompiler.init;

	compiler.driver.initialize(exePasses);

	return compiler;
}

void vox_set_passes_exe(VoxCompiler* compiler) {
	compiler.driver.context.buildType = BuildType.exe;
	compiler.driver.passes = exePasses;
}

void vox_set_passes_jit(VoxCompiler* compiler) {
	compiler.driver.context.buildType = BuildType.jit;
	compiler.driver.passes = jitPasses;
}

void vox_set_output_filename(VoxCompiler* compiler, SliceString outFile) {
	compiler.driver.context.outputFilename = cast(string)outFile;
}

void vox_set_target_os(VoxCompiler* compiler, TargetOs os) {
	compiler.driver.context.targetOs = os;
}

void vox_set_validation(VoxCompiler* compiler, bool enable) {
	compiler.driver.context.validateIr = enable;
}

void vox_begin_compilation(VoxCompiler* compiler) {
	compiler.driver.beginCompilation();
}

Identifier vox_id_get_or_reg(VoxCompiler* compiler, SliceString hostModuleName) {
	return compiler.driver.context.idMap.getOrRegFqn(&compiler.driver.context, cast(string)hostModuleName);
}

// String is owned by the library
// Will only return the innermost identifier in a list
SliceString vox_id_get_string(VoxCompiler* compiler, Identifier id) {
	return SliceString(compiler.driver.context.idString(id));
}

// If has no parent returns null
Identifier vox_id_get_parent(VoxCompiler* compiler, Identifier id) {
	if (!id.hasParent) return Identifier.init;
	return id.getParent(compiler.driver.context);
}

LinkIndex vox_get_or_create_external_module(VoxCompiler* compiler, Identifier modId) {
	return compiler.driver.context.getOrCreateExternalModule(modId, ObjectModuleKind.isHost);
}

void vox_reg_host_symbol(VoxCompiler* compiler, LinkIndex hostModuleIndex, Identifier symId, void* ptr) {
	compiler.driver.context.addHostSymbol(hostModuleIndex, symId, ptr);
}

void vox_add_module(VoxCompiler* compiler, SliceString fileName, SliceString filedata) {
	compiler.driver.addModule(SourceFileInfo(cast(string)fileName, cast(string)filedata));
}

int vox_compile(VoxCompiler* compiler) {
	try
	{
		compiler.driver.compile();
	}
	catch(CompilationException e) {
		writeln(compiler.driver.context.sink.text);
		if (e.isICE) {
			writeln(e);
		}
		return 1;
	}
	catch(Throwable t) {
		writeln(compiler.driver.context.sink.text);
		writeln(t);
		return 1;
	}
	return 0;
}

void vox_make_code_executable(VoxCompiler* compiler) {
	compiler.driver.markCodeAsExecutable();
}

void* vox_find_function(VoxCompiler* compiler, Identifier funcId) {
	try {
		CompilationContext* c = &compiler.driver.context;
		FunctionDeclNode* func = c.findFunction(funcId);
		ObjectSymbol* funcSym = c.objSymTab.getSymbol(func.backendData.objectSymIndex);
		return funcSym.dataPtr;
	} catch(CompilationException e) {
		writeln(compiler.driver.context.sink.text);
		if (e.isICE) {
			writeln(e);
		}
		return null;
	}
}

void vox_free(VoxCompiler* compiler) {
	compiler.driver.releaseMemory;
	free(compiler);
	Runtime.terminate();
}
