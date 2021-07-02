/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// Resolve all symbol references (variable/type/function/enum name uses)
/// using information collected on previous pass
module fe.names_resolve;

import std.stdio;
import std.string : format;
import all;

void pass_names_resolve(ref CompilationContext context, CompilePassPerModule[] subPasses)
{
	auto state = NameResolveState(&context);

	foreach (ref SourceFileInfo file; context.files.data) {
		AstIndex modIndex = file.mod.get_ast_index(&context);
		require_name_resolve(modIndex, state);
		assert(context.analisysStack.length == 0);
	}
}

struct NameResolveState
{
	CompilationContext* context;
}

void require_name_resolve(ref AstIndex nodeIndex, CompilationContext* context)
{
	auto state = NameResolveState(context);
	require_name_resolve(nodeIndex, state);
}

void require_name_resolve(ref AstNodes items, ref NameResolveState state)
{
	foreach(ref AstIndex item; items) require_name_resolve(item, state);
}

void require_name_resolve(ref AstIndex nodeIndex, ref NameResolveState state)
{
	AstNode* node = state.context.getAstNode(nodeIndex);

	switch(node.state) with(AstNodeState)
	{
		case name_register_self, name_register_nested, name_resolve, type_check:
			state.context.push_analized_node(nodeIndex);
			state.context.circular_dependency;
			assert(false);
		case parse_done:
			auto name_state = NameRegisterState(state.context);
			require_name_register_self(0, nodeIndex, name_state);
			state.context.throwOnErrors;
			goto case;
		case name_register_self_done:
			require_name_register(nodeIndex, state.context);
			state.context.throwOnErrors;
			break;
		case name_register_nested_done: break; // all requirement are done
		case name_resolve_done, type_check_done, ir_gen_done: return; // already name resolved
		default: state.context.internal_error(node.loc, "Node %s in %s state", node.astType, node.state);
	}

	state.context.push_analized_node(nodeIndex);
	scope(success) state.context.pop_analized_node;

	if (node.hasAttributes) {
		name_resolve_attributes(node.attributeInfo, state);
	}

	final switch(node.astType) with(AstType)
	{
		case error: state.context.internal_error(node.loc, "Visiting error node"); break;
		case abstract_node: state.context.internal_error(node.loc, "Visiting abstract node"); break;

		case decl_alias: name_resolve_alias(cast(AliasDeclNode*)node, state); break;
		case decl_alias_array: assert(false);
		case decl_builtin: assert(false);
		case decl_builtin_attribute: assert(false);
		case decl_module: name_resolve_module(cast(ModuleDeclNode*)node, state); break;
		case decl_package: assert(false);
		case decl_import: assert(false);
		case decl_function: name_resolve_func(cast(FunctionDeclNode*)node, state); break;
		case decl_var: name_resolve_var(cast(VariableDeclNode*)node, state); break;
		case decl_struct: name_resolve_struct(cast(StructDeclNode*)node, state); break;
		case decl_enum: name_resolve_enum(cast(EnumDeclaration*)node, state); break;
		case decl_enum_member: name_resolve_enum_member(cast(EnumMemberDecl*)node, state); break;
		case decl_static_assert: name_resolve_static_assert(cast(StaticAssertDeclNode*)node, state); break;
		case decl_static_foreach: assert(false);
		case decl_static_if: assert(false);
		case decl_static_version: assert(false);
		case decl_template: assert(false);
		case decl_template_param: assert(false);

		case stmt_block: name_resolve_block(cast(BlockStmtNode*)node, state); break;
		case stmt_if: name_resolve_if(cast(IfStmtNode*)node, state); break;
		case stmt_while: name_resolve_while(cast(WhileStmtNode*)node, state); break;
		case stmt_do_while: name_resolve_do(cast(DoWhileStmtNode*)node, state); break;
		case stmt_for: name_resolve_for(cast(ForStmtNode*)node, state); break;
		case stmt_switch: name_resolve_switch(cast(SwitchStmtNode*)node, state); break;
		case stmt_return: name_resolve_return(cast(ReturnStmtNode*)node, state); break;
		case stmt_break: assert(false);
		case stmt_continue: assert(false);

		case expr_name_use: name_resolve_name_use(nodeIndex, cast(NameUseExprNode*)node, state); break;
		case expr_member: name_resolve_member(cast(MemberExprNode*)node, state); break;
		case expr_bin_op: name_resolve_binary_op(cast(BinaryExprNode*)node, state); break;
		case expr_un_op: name_resolve_unary_op(cast(UnaryExprNode*)node, state); break;
		case expr_call: name_resolve_call(cast(CallExprNode*)node, state); break;
		case expr_index: name_resolve_index(nodeIndex, cast(IndexExprNode*)node, state); break;
		case expr_slice: name_resolve_expr_slice(cast(SliceExprNode*)node, state); break;
		case expr_type_conv: name_resolve_type_conv(cast(TypeConvExprNode*)node, state); break;

		case literal_int: assert(false);
		case literal_float: assert(false);
		case literal_string: assert(false);
		case literal_null: assert(false);
		case literal_bool: assert(false);
		case literal_array: assert(false);

		case type_basic: assert(false);
		case type_func_sig: name_resolve_func_sig(cast(FunctionSignatureNode*)node, state); break;
		case type_ptr: name_resolve_ptr(cast(PtrTypeNode*)node, state); break;
		case type_static_array: name_resolve_static_array(cast(StaticArrayTypeNode*)node, state); break;
		case type_slice: name_resolve_slice(cast(SliceTypeNode*)node, state); break;
	}
}

/// Error means that lookup failed due to earlier failure or error, so no new error should be produced
enum LookupResult : ubyte {
	success,
	failure,
	error
}

/// Look up symbol by Identifier. Searches the stack of scopes.
// Returns errorNode if not found or error occured
AstIndex lookupScopeIdRecursive(Scope* scop, const Identifier id, TokenIndex from, CompilationContext* context)
{
	Scope* sc = scop;

	//writefln("lookup %s", context.idString(id));
	// first phase
	while(sc)
	{
		AstIndex symIndex = sc.symbols.get(id, AstIndex.init);
		//writefln("  scope %s %s %s", context.getAstNodeIndex(sc), sc.debugName, symIndex);

		if (symIndex)
		{
			AstNode* symNode = context.getAstNode(symIndex);
			if (sc.kind == ScopeKind.local)
			{
				// we need to skip forward references in function scope
				uint fromStart = context.tokenLocationBuffer[from].start;
				uint toStart = context.tokenLocationBuffer[symNode.loc].start;
				//writefln("    local %s %s", fromStart, toStart);
				// backward reference
				if (fromStart > toStart) {
					return symIndex;
				}
			}
			else
			{
				// forward reference allowed in global and member scopes
				return symIndex;
			}
		}

		sc = sc.parentScope.get_scope(context);
	}

	// second phase
	return lookupImports(scop, id, from, context);
}

// Returns errorNode if not found or error occured
AstIndex lookupImports(Scope* scop, const Identifier id, TokenIndex from, CompilationContext* context)
{
	while (scop)
	{
		AstIndex symIndex;
		ModuleDeclNode* symMod;

		foreach (AstIndex impIndex; scop.imports)
		{
			ModuleDeclNode* imp = context.getAst!ModuleDeclNode(impIndex);
			// TODO: check that import is higher in ordered scopes
			AstIndex scopeSym = imp.memberScope.lookup_scope(id, context);
			if (!scopeSym) continue;

			if (scopeSym && symIndex && scopeSym != symIndex)
			{
				string mod1Id = context.idString(symMod.id);
				string sym1Id = context.idString(symIndex.get_node_id(context));

				string mod2Id = context.idString(imp.id);
				string sym2Id = context.idString(scopeSym.get_node_id(context));

				context.error(from,
					"`%s.%s` at %s conflicts with `%s.%s` at %s",
					mod1Id, sym1Id, FmtSrcLoc(context.getAstNode(symIndex).loc, context),
					mod2Id, sym2Id, FmtSrcLoc(context.getAstNode(scopeSym).loc, context));
				return CommonAstNodes.node_error;
			}

			symIndex = scopeSym;
			symMod = imp;
		}

		if (symIndex) return symIndex;

		scop = scop.parentScope.get_scope(context);
	}

	return CommonAstNodes.node_error;
}

