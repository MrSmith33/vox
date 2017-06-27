/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lang.semantics;

import lang.ast;

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
	Identifier[] localVars;
	int varIndex(Identifier id)
	{
		foreach(int i, varId; localVars)
			if (varId == id) return i;
		return 0;
	}
}

ModuleSemantics analyzeModule(Module moduleDecl)
{
	FunctionSemantics[] globalFunctions;
	foreach (func; moduleDecl.functions)
	{
		globalFunctions ~= analyzeFunction(func);
	}
	return new ModuleSemantics(moduleDecl, globalFunctions);
}


FunctionSemantics analyzeFunction(FunctionDeclaration node)
{
	Identifier[] localVars;
	localVars ~= 0;
	return new FunctionSemantics(node, localVars);
}
