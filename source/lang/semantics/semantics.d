/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lang.semantics.semantics;

import lang.ast.ast;
import lang.lex : SourceLocation;
import lang.error;

struct NativeFunction
{
	Identifier id;
	int numParams;
	NativeFunPtr funcPtr;
}

alias NativeFunPtr = extern(C) int function(int);

struct Callee
{
	private ModuleSemantics sem;
	size_t index;
	bool native;
	bool found;
	void* funcPtr()
	{
		if (native)
			return sem.nativeFunctions[index].funcPtr;
		else
			return sem.functions[index].funcPtr;
	}
}

class ModuleSemantics
{
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	Module moduleNode;
	FunctionSemantics[] functions;
	NativeFunction[] nativeFunctions;
	FunctionSemantics tryGetFunction(Identifier id)
	{
		foreach(fun; functions)
			if (fun.node.id == id) return fun;
		return null;
	}
	FunctionSemantics getFunction(Identifier id)
	{
		if (auto fun = tryGetFunction(id)) return fun;
		throw internal_error("Invalid id requested");
	}
	Callee tryGetCallee(Identifier id)
	{
		foreach(i, fun; functions)
			if (fun.node.id == id) return Callee(this, i, false, true);
		foreach(i, fun; nativeFunctions)
			if (fun.id == id) return Callee(this, i, true, true);
		return Callee(this);
	}
}

class FunctionSemantics
{
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	FunctionDeclaration node;
	Identifier[] localVars; // includes parameters
	void* funcPtr;

	int varIndex(Identifier id)
	{
		foreach(int i, varId; localVars)
			if (varId == id) return i;
		throw internal_error("Invalid id requested");
	}
}

ModuleSemantics analyzeModule(Module moduleDecl, IdentifierMap idMap, NativeFunction[] nativeFuncs = null)
{
	FunctionSemantics[] functions;
	auto funcAnalyser = new FunctionAnalyser(moduleDecl, idMap, nativeFuncs);
	foreach (func; moduleDecl.functions)
	{
		functions ~= funcAnalyser.analyzeFunction(func);
	}
	return new ModuleSemantics(moduleDecl, functions, nativeFuncs);
}

class FunctionAnalyser : FunctionVisitor {
	this(Module moduleDecl, IdentifierMap idMap, NativeFunction[] nativeFuncs)
	{
		this.moduleDecl = moduleDecl;
		this.idMap = idMap;
		this.nativeFuncs = nativeFuncs;
	}
	private Module moduleDecl;
	private IdentifierMap idMap;
	private Identifier[] localVars;
	private NativeFunction[] nativeFuncs;

	// returns index in localVars or -1
	int findRegisteredVar(Identifier id)
	{
		foreach(int i, var; localVars)
			if (var == id) return i;
		return -1;
	}

	void registerVar(Identifier id)
	{
		localVars ~= id;
	}

	FunctionSemantics analyzeFunction(FunctionDeclaration node)
	{
		localVars = null;
		foreach (param; node.parameters)
		{
			if(findRegisteredVar(param.id) == -1) {
				registerVar(param.id);
			} else {
				throw semantics_error(param.loc, "Duplicate parameter '%s' in func '%s'",
					idMap.get(param.id), idMap.get(node.id));
			}
		}
		node.accept(this);
		return new FunctionSemantics(node, localVars, null);
	}

	size_t getFunctionParams(Identifier id)
	{
		foreach(fun; moduleDecl.functions)
			if (fun.id == id) return fun.parameters.length;
		foreach(fun; nativeFuncs)
			if (fun.id == id) return fun.numParams;
		throw new Error("Invalid function id");
	}

	override void visit(VariableExpression v) {
		if (findRegisteredVar(v.id) == -1) registerVar(v.id);
	}

	override void visit(CallExpression c) {
		auto numParams = getFunctionParams(c.calleeId);
		auto numArgs   = c.args.length;

		if (numArgs < numParams)
			throw semantics_error(c.loc, "Insufficient parameters to '%s', got %s, expected %s",
				idMap.get(c.calleeId), numArgs, numParams);
		else if (numArgs > numParams)
			throw semantics_error(c.loc, "Too much parameters to '%s', got %s, expected %s",
				idMap.get(c.calleeId), numArgs, numParams);

		super.visit(c);
	}
}
