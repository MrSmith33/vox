/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lang.semantics;

import lang.ast;

void semantics_error(Args...)(string msg, Args args) {
	import std.stdio;
	writefln(msg, args);
}

class ModuleSemantics
{
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	Module moduleNode;
	FunctionSemantics[] globalFunctions;
}

class FunctionSemantics
{
	this(typeof(this.tupleof) args) { this.tupleof = args; }
	FunctionDeclaration node;
	bool valid;
	Identifier[] localVars;
	int varIndex(Identifier id)
	{
		foreach(int i, varId; localVars)
			if (varId == id) return i;
		return 0;
	}
}

ModuleSemantics analyzeModule(Module moduleDecl, IdentifierMap idMap)
{
	FunctionSemantics[] globalFunctions;
	auto funcAnalyser = new FunctionAnalyser;
	foreach (func; moduleDecl.functions)
	{
		globalFunctions ~= funcAnalyser.analyzeFunction(func, idMap);
	}
	return new ModuleSemantics(moduleDecl, globalFunctions);
}

class FunctionAnalyser : DepthAstVisitor {
	Identifier[] localVars;

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

	FunctionSemantics analyzeFunction(FunctionDeclaration node, IdentifierMap idMap)
	{
		localVars = null;
		foreach (param; node.parameters)
		{
			if(findRegisteredVar(param.id) == -1)
			{
				registerVar(param.id);
			}
			else
			{
				semantics_error("Duplicate parameter name '%s'", idMap.get(param.id));
				return new FunctionSemantics(node, false, localVars);
			}
		}
		node.accept(this);
		return new FunctionSemantics(node, true, localVars);
	}

	override void visit(VariableExpression v) {
		if (findRegisteredVar(v.id) == -1) registerVar(v.id);
	}
}
