/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Register identifiers in scope tree
module fe.names_register;

import std.stdio;
import std.string : format;
import std.typecons : Flag, Yes, No;
import all;


void pass_names_register(ref CompilationContext context, CompilePassPerModule[] subPasses)
{
	auto state = NameRegisterState(&context);

	foreach (ref SourceFileInfo file; context.files.data) {
		require_name_register(file.mod.as_node, state);
	}
}


void require_name_register(TypeNode* node, ref NameRegisterState state) {
	require_name_register(node.as_node, state);
}

void require_name_register(ExpressionNode* node, ref NameRegisterState state) {
	require_name_register(node.as_node, state);
}

void require_name_register(AstNode* node, ref NameRegisterState state)
{
	if (node.state >= AstNodeState.name_register_done) return;

	switch(node.astType) with(AstType)
	{
		case error: state.context.internal_error(node.loc, "Visiting error node"); break;
		case abstract_node: state.context.internal_error(node.loc, "Visiting abstract node"); break;

		case decl_module: name_register_module(cast(ModuleDeclNode*)node, state); break;
		case decl_import: name_register_import(cast(ImportDeclNode*)node, state); break;
		case decl_function: name_register_func(cast(FunctionDeclNode*)node, state); break;
		case decl_var: name_register_var(cast(VariableDeclNode*)node, state); break;
		case decl_struct: name_register_struct(cast(StructDeclNode*)node, state); break;
		case decl_enum: name_register_enum(cast(EnumDeclaration*)node, state); break;
		case decl_enum_member: name_register_enum_member(cast(EnumMemberDecl*)node, state); break;

		case stmt_block: name_register_block(cast(BlockStmtNode*)node, state); break;
		case stmt_if: name_register_if(cast(IfStmtNode*)node, state); break;
		case stmt_while: name_register_while(cast(WhileStmtNode*)node, state); break;
		case stmt_do_while: name_register_do(cast(DoWhileStmtNode*)node, state); break;
		case stmt_for: name_register_for(cast(ForStmtNode*)node, state); break;
		case stmt_return: assert(false);
		case stmt_break: assert(false);
		case stmt_continue: assert(false);

		default: state.context.internal_error(node.loc, "Visiting %s node", node.astType); break;
	}
}

struct NameRegisterState
{
	CompilationContext* context;
	Scope* currentScope;

	Scope* pushScope(string name, Flag!"ordered" isOrdered)
	{
		Scope* newScope = context.appendAst!Scope;
		newScope.debugName = name;
		newScope.isOrdered = isOrdered;

		if (currentScope)
			newScope.parentScope = currentScope;
		currentScope = newScope;

		return currentScope;
	}

	void popScope()
	{
		if (currentScope.parentScope)
			currentScope = currentScope.parentScope;
		else
			currentScope = null;
	}

	/// Constructs and inserts symbol with id
	void insert(Identifier id, AstNode* node)
	{
		node.flags |= currentScope.isOrdered ? AstFlags.isInOrderedScope : 0;
		if (auto s = currentScope.symbols.get(id, null))
		{
			context.error(node.loc,
				"declaration `%s` is already defined at %s", context.idString(id), FmtSrcLoc(s.loc, context));
		}
		currentScope.symbols.put(context.arrayArena, id, node);
	}
}


void name_register_module(ModuleDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	node._scope = state.pushScope("Module", No.ordered);
	foreach (decl; node.declarations) require_name_register(decl, state);
	state.popScope;
	node.state = AstNodeState.name_register_done;
}

void name_register_import(ImportDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	ModuleDeclNode* m = state.context.findModule(node.id);
	state.currentScope.imports.put(state.context.arrayArena, m);
	if (m is null)
		state.context.error(node.loc, "Cannot find module `%s`", state.context.idString(node.id));
	node.state = AstNodeState.name_resolve_done;
}

void name_register_func(FunctionDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	state.insert(node.id, node.as_node);
	node._scope = state.pushScope(state.context.idString(node.id), Yes.ordered);
	foreach (param; node.parameters) require_name_register(param.as_node, state);
	if (node.block_stmt) require_name_register(node.block_stmt.as_node, state);
	state.popScope;
	node.state = AstNodeState.name_register_done;
}

void name_register_var(VariableDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	state.insert(node.id, node.as_node);
	if (node.initializer) require_name_register(node.initializer, state);
	node.state = AstNodeState.name_register_done;
}

void name_register_struct(StructDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	state.insert(node.id, node.as_node);
	node._scope = state.pushScope(state.context.idString(node.id), No.ordered);
	foreach (decl; node.declarations) require_name_register(decl, state);
	state.popScope;
	node.state = AstNodeState.name_register_done;
}

void name_register_enum(EnumDeclaration* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	if (node.isAnonymous)
	{
		foreach (decl; node.declarations) require_name_register(decl, state);
	}
	else
	{
		state.insert(node.id, node.as_node);
		node._scope = state.pushScope(state.context.idString(node.id), No.ordered);
		foreach (decl; node.declarations) require_name_register(decl, state);
		state.popScope;
	}
	node.state = AstNodeState.name_register_done;
}

void name_register_enum_member(EnumMemberDecl* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	state.insert(node.id, node.as_node);
	if (node.initializer) require_name_register(node.initializer, state);
	node.state = AstNodeState.name_register_done;
}

void name_register_block(BlockStmtNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	node._scope = state.pushScope("Block", Yes.ordered);
	foreach(stmt; node.statements) require_name_register(stmt, state);
	state.popScope;
	node.state = AstNodeState.name_register_done;
}

void name_register_if(IfStmtNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	require_name_register(node.condition, state);
	node.then_scope = state.pushScope("Then", Yes.ordered);
	require_name_register(node.thenStatement, state);
	state.popScope;
	if (node.elseStatement) {
		node.else_scope = state.pushScope("Else", Yes.ordered);
		require_name_register(node.elseStatement, state);
		state.popScope;
	}
	node.state = AstNodeState.name_register_done;
}

void name_register_while(WhileStmtNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	require_name_register(node.condition, state);
	node._scope = state.pushScope("While", Yes.ordered);
	require_name_register(node.statement, state);
	state.popScope;
	node.state = AstNodeState.name_register_done;
}

void name_register_do(DoWhileStmtNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	node._scope = state.pushScope("While", Yes.ordered);
	require_name_register(node.statement, state);
	state.popScope;
	require_name_register(node.condition, state);
	node.state = AstNodeState.name_register_done;
}

void name_register_for(ForStmtNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	node._scope = state.pushScope("For", Yes.ordered);
	foreach(stmt; node.init_statements) require_name_register(stmt, state);
	if (node.condition) require_name_register(node.condition, state);
	foreach(stmt; node.increment_statements) require_name_register(stmt, state);
	require_name_register(node.statement, state);
	state.popScope;
	node.state = AstNodeState.name_register_done;
}
