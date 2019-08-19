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
	}
}

struct NameResolveState
{
	CompilationContext* context;
	Scope* currentScope;

	void pushScope(AstIndex scopeIndex)
	{
		assert(scopeIndex);
		currentScope = context.getAst!Scope(scopeIndex);
	}

	void popScope()
	{
		assert(currentScope);
		currentScope = currentScope.parentScope.get_scope(context);
	}
}

void require_name_resolve(ref AstIndex nodeIndex, ref NameResolveState state)
{
	AstNode* node = state.context.getAstNode(nodeIndex);

	switch(node.state) with(AstNodeState)
	{
		case name_register, name_resolve, type_check: state.context.unrecoverable_error(node.loc, "Circular dependency"); return;
		case name_register_done: break; // all requirement are done
		case name_resolve_done, type_check_done: return; // already name resolved
		default: state.context.internal_error(node.loc, "Node %s in %s state", node.astType, node.state);
	}

	final switch(node.astType) with(AstType)
	{
		case error: state.context.internal_error(node.loc, "Visiting error node"); break;
		case abstract_node: state.context.internal_error(node.loc, "Visiting abstract node"); break;

		case decl_module: name_resolve_module(cast(ModuleDeclNode*)node, state); break;
		case decl_import: assert(false);
		case decl_function: name_resolve_func(cast(FunctionDeclNode*)node, state); break;
		case decl_var: name_resolve_var(cast(VariableDeclNode*)node, state); break;
		case decl_struct: name_resolve_struct(cast(StructDeclNode*)node, state); break;
		case decl_enum: name_resolve_enum(cast(EnumDeclaration*)node, state); break;
		case decl_enum_member: name_resolve_enum_member(cast(EnumMemberDecl*)node, state); break;

		case stmt_block: name_resolve_block(cast(BlockStmtNode*)node, state); break;
		case stmt_if: name_resolve_if(cast(IfStmtNode*)node, state); break;
		case stmt_while: name_resolve_while(cast(WhileStmtNode*)node, state); break;
		case stmt_do_while: name_resolve_do(cast(DoWhileStmtNode*)node, state); break;
		case stmt_for: name_resolve_for(cast(ForStmtNode*)node, state); break;
		case stmt_return: name_resolve_return(cast(ReturnStmtNode*)node, state); break;
		case stmt_break: assert(false);
		case stmt_continue: assert(false);

		case expr_name_use, expr_var_name_use, expr_func_name_use, expr_member_name_use, expr_type_name_use:
			name_resolve_name_use(cast(NameUseExprNode*)node, state); break;
		case expr_member, expr_struct_member, expr_enum_member, expr_slice_member, expr_static_array_member:
			name_resolve_member(cast(MemberExprNode*)node, state); break;
		case expr_bin_op: name_resolve_binary_op(cast(BinaryExprNode*)node, state); break;
		case expr_un_op: name_resolve_unary_op(cast(UnaryExprNode*)node, state); break;
		case expr_call: name_resolve_call(cast(CallExprNode*)node, state); break;
		case expr_index: name_resolve_index(cast(IndexExprNode*)node, state); break;
		case expr_type_conv: name_resolve_type_conv(cast(TypeConvExprNode*)node, state); break;

		case literal_int: assert(false);
		case literal_string: assert(false);
		case literal_null: assert(false);
		case literal_bool: assert(false);

		case type_basic: assert(false);
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
AstIndex lookupScopeIdRecursive(Scope* scop, const Identifier id, TokenIndex from, CompilationContext* context)
{
	Scope* sc = scop;

	// first phase
	while(sc)
	{
		AstIndex symIndex = sc.symbols.get(id, AstIndex.init);

		if (symIndex)
		{
			AstNode* symNode = context.getAstNode(symIndex);
			// forward reference allowed for unordered scope
			if (!symNode.isInOrderedScope) {
				return symIndex;
			} else { // ordered scope
				// we need to skip forward references in ordered scope
				uint fromStart = context.tokenLocationBuffer[from].start;
				uint toStart = context.tokenLocationBuffer[symNode.loc].start;
				// backward reference
				if (fromStart > toStart) {
					return symIndex;
				}
			}
		}

		sc = sc.parentScope.get_scope(context);
	}

	// second phase
	AstIndex symIndex = lookupImports(scop, id, from, context);

	if (symIndex) {
		return symIndex;
	} else {
		context.error(from, "undefined identifier `%s`", context.idString(id));
		return context.errorNode;
	}
}

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
			AstIndex scopeSym = imp._scope.get_scope(context).symbols.get(id, AstIndex.init);
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
			}

			symIndex = scopeSym;
			symMod = imp;
		}

		if (symIndex) return symIndex;

		scop = scop.parentScope.get_scope(context);
	}

	return AstIndex.init;
}

/// Look up member by Identifier. Searches aggregate scope for identifier.
LookupResult lookupMember(MemberExprNode* expr, CompilationContext* context)
{
	NameUseExprNode* memberNode = context.getAst!NameUseExprNode(expr.member);
	TypeNode* obj = expr.aggregate.get_type(context);

	// Allow member access for pointers to structs
	if (obj.isPointer)
		obj = obj.as_ptr.base.get_type(context);

	memberNode.astType = AstType.expr_member_name_use;
	switch(obj.astType)
	{
		case AstType.type_slice: return lookupSliceMember(expr, obj.as_slice, memberNode.id(context), context);
		case AstType.type_static_array: return lookupStaticArrayMember(expr, obj.as_static_array, memberNode.id(context), context);
			case AstType.decl_struct: return lookupStructMember(expr, obj.as_struct, memberNode.id(context), context);
		case AstType.decl_enum: return lookupEnumMember(expr, obj.as_enum, memberNode.id(context), context);
		case AstType.type_basic: return lookupBasicMember(expr, obj.as_basic, memberNode.id(context), context);
		default:
			context.unrecoverable_error(expr.loc, "Cannot resolve `%s` for %s", context.idString(memberNode.id(context)), obj.astType);
			expr.type = context.basicTypeNodes(BasicType.t_error);
			return LookupResult.error;
	}
}

LookupResult lookupEnumMember(MemberExprNode* expr, EnumDeclaration* enumDecl, Identifier id, CompilationContext* context)
{
	context.assertf(!enumDecl.isAnonymous, expr.loc,
		"Trying to get member from anonymous enum defined at %s",
		context.tokenLoc(enumDecl.loc));

	AstIndex memberIndex = enumDecl._scope.get_scope(context).symbols.get(id, AstIndex.init);
	if (!memberIndex) return LookupResult.failure;

	AstNode* memberNode = memberIndex.get_node(context);

	context.assertf(memberNode.astType == AstType.decl_enum_member, expr.loc,
		"Unexpected enum member %s", memberNode.astType);

	expr.member.get_name_use(context).resolve = memberIndex;

	EnumMemberDecl* enumMember = memberNode.cast_decl_enum_member;
	expr.type = enumMember.type;
	expr.memberIndex = enumMember.scopeIndex;
	expr.astType = AstType.expr_enum_member;
	return LookupResult.success;
}

LookupResult lookupBasicMember(MemberExprNode* expr, BasicTypeNode* basicType, Identifier id, CompilationContext* context)
{
	if (basicType.isInteger)
	{
		if (id == context.commonIds.id_min)
		{
			expr.memberIndex = BuiltinMemberIndex.MEMBER_MIN;
			expr.type = basicType.get_ast_index(context);
			return LookupResult.success;
		}
		else if (id == context.commonIds.id_max)
		{
			expr.memberIndex = BuiltinMemberIndex.MEMBER_MAX;
			expr.type = basicType.get_ast_index(context);
			return LookupResult.success;
		}
	}

	return LookupResult.failure;
}

LookupResult lookupSliceMember(MemberExprNode* expr, SliceTypeNode* sliceType, Identifier id, CompilationContext* context)
{
	expr.astType = AstType.expr_slice_member;
	// use integer indicies, because slice is a struct
	if (id == context.commonIds.id_ptr)
	{
		expr.memberIndex = 1; // ptr
		expr.type = context.appendAst!PtrTypeNode(sliceType.loc, sliceType.base);
		return LookupResult.success;
	}
	else if (id == context.commonIds.id_length)
	{
		expr.memberIndex = 0; // length
		expr.type = context.basicTypeNodes(BasicType.t_u64);
		return LookupResult.success;
	}

	return LookupResult.failure;
}

LookupResult lookupStaticArrayMember(MemberExprNode* expr, StaticArrayTypeNode* arrType, Identifier id, CompilationContext* context)
{
	expr.astType = AstType.expr_static_array_member;
	if (id == context.commonIds.id_ptr)
	{
		expr.memberIndex = BuiltinMemberIndex.MEMBER_PTR;
		expr.type = context.appendAst!PtrTypeNode(arrType.loc, arrType.base);
		return LookupResult.success;
	}
	else if (id == context.commonIds.id_length)
	{
		expr.memberIndex = BuiltinMemberIndex.MEMBER_LENGTH;
		expr.type = context.basicTypeNodes(BasicType.t_u64);
		return LookupResult.success;
	}

	return LookupResult.failure;
}

LookupResult lookupStructMember(MemberExprNode* expr, StructDeclNode* structDecl, Identifier id, CompilationContext* context)
{
	AstIndex memberSym = context.getAstScope(structDecl._scope).symbols.get(id, AstIndex.init);
	if (!memberSym) {
		return LookupResult.failure;
	}

	NameUseExprNode* memberNode = context.getAst!NameUseExprNode(expr.member);
	AstNode* memberSymNode = context.getAstNode(memberSym);

	memberNode.resolve = memberSym;
	expr.astType = AstType.expr_struct_member;

	switch(memberSymNode.astType)
	{
		case AstType.decl_function:
			context.internal_error("member functions/UFCS calls are not implemented");
			assert(false);

		case AstType.decl_var:
			VariableDeclNode* memberVar = memberNode.varDecl(context);
			expr.type = memberVar.type.get_type(context).foldAliases(context).get_ast_index(context);
			expr.memberIndex = memberVar.scopeIndex;
			return LookupResult.success;

		case AstType.decl_struct:
			context.internal_error("member structs are not implemented");
			assert(false);

		case AstType.decl_enum:
			context.internal_error("member enums are not implemented");
			assert(false);

		case AstType.decl_enum_member:
			EnumMemberDecl* enumMember = memberSymNode.cast_decl_enum_member;
			expr.type = enumMember.type;
			expr.memberIndex = enumMember.scopeIndex;
			expr.astType = AstType.expr_enum_member;
			return LookupResult.success;

		default:
			context.internal_error("Unexpected struct member %s", memberSymNode.astType);
			assert(false);
	}
}
