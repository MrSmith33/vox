/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lang.semantics;

import lang.ast;
import lang.lex2 : SourceLocation;
import lang.error;


class ModuleSemantics
{
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	Module moduleNode;
	FunctionSemantics[] functions;
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

ModuleSemantics analyzeModule(Module moduleDecl, IdentifierMap idMap)
{
	FunctionSemantics[] functions;
	auto funcAnalyser = new FunctionAnalyser(moduleDecl, idMap);
	foreach (func; moduleDecl.functions)
	{
		functions ~= funcAnalyser.analyzeFunction(func);
	}
	return new ModuleSemantics(moduleDecl, functions);
}

class FunctionAnalyser : DepthAstVisitor {
	this(Module moduleDecl, IdentifierMap idMap)
	{
		this.moduleDecl = moduleDecl;
		this.idMap = idMap;
	}
	private Module moduleDecl;
	private IdentifierMap idMap;
	private Identifier[] localVars;

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

	override void visit(VariableExpression v) {
		if (findRegisteredVar(v.id) == -1) registerVar(v.id);
	}

	override void visit(CallExpression c) {
		FunctionDeclaration funcDecl = moduleDecl.getFunction(c.calleeId);
		auto numParams = funcDecl.parameters.length;
		auto numArgs   = c.args.length;

		if (numArgs < numParams)
			throw semantics_error(c.loc, "Insufficient parameters to '%s', got %s, expected %s",
				idMap.get(c.calleeId), numArgs, numParams);
		else if (numArgs > numParams)
			throw semantics_error(c.loc, "Too much parameters to '%s', got %s, expected %s",
				idMap.get(c.calleeId), numArgs, numParams);
	}
}
