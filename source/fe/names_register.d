/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Register identifiers in scope tree
module fe.names_register;

import std.stdio;
import std.string : format;
public import std.typecons : Flag, Yes, No;
import all;


void pass_names_register(ref CompilationContext context, CompilePassPerModule[] subPasses)
{
	auto state = NameRegisterState(&context);

	foreach (ref SourceFileInfo file; context.files.data) {
		AstIndex modIndex = file.mod.get_ast_index(&context);
		require_name_register(modIndex, state);
	}
}

void require_name_register(ref AstIndex nodeIndex, ref NameRegisterState state)
{
	AstNode* node = state.context.getAstNode(nodeIndex);

	switch(node.state) with(AstNodeState)
	{
		case name_register, name_resolve, type_check: state.context.unrecoverable_error(node.loc, "Circular dependency"); return;
		case parse_done: break; // all requirement are done
		case name_register_done, name_resolve_done, type_check_done: return; // already name registered
		default: state.context.internal_error(node.loc, "Node %s in %s state", node.astType, node.state);
	}

	switch(node.astType) with(AstType)
	{
		case error: state.context.internal_error(node.loc, "Visiting error node"); break;
		case abstract_node: state.context.internal_error(node.loc, "Visiting abstract node"); break;

		case decl_module: name_register_module(cast(ModuleDeclNode*)node, state); break;
		case decl_import: name_register_import(cast(ImportDeclNode*)node, state); break;
		case decl_function: name_register_func(nodeIndex, cast(FunctionDeclNode*)node, state); break;
		case decl_var: name_register_var(nodeIndex, cast(VariableDeclNode*)node, state); break;
		case decl_struct: name_register_struct(nodeIndex, cast(StructDeclNode*)node, state); break;
		case decl_enum: name_register_enum(nodeIndex, cast(EnumDeclaration*)node, state); break;
		case decl_enum_member: name_register_enum_member(nodeIndex, cast(EnumMemberDecl*)node, state); break;

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

	AstIndex pushScope(string name, Flag!"ordered" isOrdered)
	{
		AstIndex newScopeIndex = context.appendAst!Scope;
		Scope* newScope = context.getAst!Scope(newScopeIndex);
		newScope.debugName = name;
		newScope.isOrdered = isOrdered;

		if (currentScope)
			newScope.parentScope = currentScope.get_ast_index(context);
		currentScope = newScope;

		return newScopeIndex;
	}

	void popScope()
	{
		if (currentScope.parentScope)
			currentScope = currentScope.parentScope.get_scope(context);
		else
			currentScope = null;
	}

	/// Constructs and inserts symbol with id
	void insert(Identifier id, AstIndex nodeIndex)
	{
		AstNode* node = context.getAstNode(nodeIndex);
		node.flags |= currentScope.isOrdered ? AstFlags.isInOrderedScope : 0;
		if (auto s = currentScope.symbols.get(id, AstIndex.init))
		{
			context.error(node.loc,
				"declaration `%s` is already defined at %s", context.idString(id), FmtSrcLoc(s.get_node(context).loc, context));
		}
		currentScope.symbols.put(context.arrayArena, id, nodeIndex);
	}
}
