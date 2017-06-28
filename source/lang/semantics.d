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
	auto funcAnalyser = new FunctionAnalyser;
	foreach (func; moduleDecl.functions)
	{
		globalFunctions ~= funcAnalyser.analyzeFunction(func);
	}
	return new ModuleSemantics(moduleDecl, globalFunctions);
}

class FunctionAnalyser : DepthAstVisitor {
	Identifier[] localVars;
	FunctionSemantics analyzeFunction(FunctionDeclaration node)
	{
		localVars = null;
		node.accept(this);
		return new FunctionSemantics(node, localVars);
	}
	override void visit(VariableExpression v) {
		foreach(var; localVars)
			if (var == v.id) return;
		localVars ~= v.id;
	}
}
