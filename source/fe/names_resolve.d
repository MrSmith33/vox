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

void pass_names_resolve(ref CompilationContext ctx, CompilePassPerModule[] subPasses)
{
	auto state = NameResolveState(&ctx);

	foreach (ref SourceFileInfo file; ctx.files.data) {
		require_name_resolve(file.mod.as_node, state);
	}
}

struct NameResolveState
{
	CompilationContext* context;
	Scope* curScope;

	void pushScope(Scope* scope_)
	{
		assert(scope_);
		curScope = scope_;
	}

	void popScope()
	{
		assert(curScope);
		curScope = curScope.parentScope;
	}
}

void require_name_resolve(AstNode* node, ref NameResolveState state)
{
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
AstNode* lookupScopeIdRecursive(Scope* scop, const Identifier id, TokenIndex from, CompilationContext* context)
{
	Scope* sc = scop;

	// first phase
	while(sc)
	{
		AstNode* sym = sc.symbols.get(id, null);

		if (sym)
		{
			// forward reference allowed for unordered scope
			if (!sym.isInOrderedScope) {
				return sym;
			} else { // ordered scope
				// we need to skip forward references in ordered scope
				uint fromStart = context.tokenLocationBuffer[from].start;
				uint toStart = context.tokenLocationBuffer[sym.loc].start;
				// backward reference
				if (fromStart > toStart) {
					return sym;
				}
			}
		}

		sc = sc.parentScope;
	}

	// second phase
	AstNode* sym = lookupImports(scop, id, from, context);

	if (sym) {
		return sym;
	} else {
		context.error(from, "undefined identifier `%s`", context.idString(id));
		return cast(AstNode*)&context.errorNode;
	}
}

AstNode* lookupImports(Scope* scop, const Identifier id, TokenIndex from, CompilationContext* context)
{
	while (scop)
	{
		AstNode* sym;
		ModuleDeclNode* symMod;

		foreach (ModuleDeclNode* imp; scop.imports)
		{
			// TODO: check that import is higher in ordered scopes
			AstNode* scopeSym = imp._scope.symbols.get(id, null);
			if (!scopeSym) continue;

			if (scopeSym && sym && scopeSym != sym)
			{
				string mod1Id = context.idString(symMod.id);
				string sym1Id = context.idString(sym.get_node_id);

				string mod2Id = context.idString(imp.id);
				string sym2Id = context.idString(scopeSym.get_node_id);

				context.error(from,
					"`%s.%s` at %s conflicts with `%s.%s` at %s",
					mod1Id, sym1Id, FmtSrcLoc(sym.loc, context),
					mod2Id, sym2Id, FmtSrcLoc(scopeSym.loc, context));
			}

			sym = scopeSym;
			symMod = imp;
		}

		if (sym) return sym;

		scop = scop.parentScope;
	}
	return null;
}

/// Look up member by Identifier. Searches aggregate scope for identifier.
void lookupMember(MemberExprNode* expr, CompilationContext* context)
{
	TypeNode* obj = expr.aggregate.as_node.get_node_type;

	// Allow member access for pointers to structs
	if (obj.isPointer)
		obj = obj.as_ptr.base.as_node.get_node_type;

	expr.member.astType = AstType.expr_member_name_use;
	switch(obj.astType)
	{
		case AstType.type_slice: lookupSliceMember(expr, obj.as_slice, expr.member.id, context); break;
		case AstType.type_static_array: lookupStaticArrayMember(expr, obj.as_static_array, expr.member.id, context); break;
			case AstType.decl_struct: lookupStructMember(expr, obj.as_struct, expr.member.id, context); break;
		case AstType.decl_enum: lookupEnumMember(expr, obj.as_enum, expr.member.id, context); break;
		case AstType.type_basic: lookupBasicMember(expr, obj.as_basic, expr.member.id, context); break;
		default:
			context.unrecoverable_error(expr.loc, "Cannot resolve `%s` for %s", context.idString(expr.member.id), obj.astType);
			expr.type = context.basicTypeNodes(BasicType.t_error);
			return;
	}
}

LookupResult lookupEnumMember(MemberExprNode* expr, EnumDeclaration* enumDecl, Identifier id, CompilationContext* context)
{
	context.assertf(!enumDecl.isAnonymous, expr.loc,
		"Trying to get member from anonymous enum defined at %s",
		context.tokenLoc(enumDecl.loc));

	AstNode* memberNode = enumDecl._scope.symbols.get(id, null);
	if (memberNode is null)
	{
		context.error(expr.loc, "Enum `%s` has no member `%s`",
			context.idString(enumDecl.id), context.idString(id));
		return LookupResult.failure;
	}

	context.assertf(memberNode.astType == AstType.decl_enum_member, expr.loc,
		"Unexpected enum member %s", memberNode.astType);

	expr.member.resolve = memberNode;

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
			expr.type = basicType.typeNode;
			return LookupResult.success;
		}
		else if (id == context.commonIds.id_max)
		{
			expr.memberIndex = BuiltinMemberIndex.MEMBER_MAX;
			expr.type = basicType.typeNode;
			return LookupResult.success;
		}
	}

	context.error(expr.loc, "%s has no `%s` property", basicType.typeNode.printer(context), context.idString(id));
	return LookupResult.failure;
}

LookupResult lookupSliceMember(MemberExprNode* expr, SliceTypeNode* sliceType, Identifier id, CompilationContext* context)
{
	expr.astType = AstType.expr_slice_member;
	// use integer indicies, because slice is a struct
	if (id == context.commonIds.id_ptr)
	{
		expr.memberIndex = 1; // ptr
		expr.type = cast(TypeNode*) context.appendAst!PtrTypeNode(sliceType.loc, sliceType.base);
		return LookupResult.success;
	}
	else if (id == context.commonIds.id_length)
	{
		expr.memberIndex = 0; // length
		expr.type = context.basicTypeNodes(BasicType.t_u64);
		return LookupResult.success;
	}

	context.error(expr.loc, "Slice `%s` has no member `%s`", sliceType.typeNode.printer(context), context.idString(id));
	return LookupResult.failure;
}

LookupResult lookupStaticArrayMember(MemberExprNode* expr, StaticArrayTypeNode* arrType, Identifier id, CompilationContext* context)
{
	expr.astType = AstType.expr_static_array_member;
	if (id == context.commonIds.id_ptr)
	{
		expr.memberIndex = BuiltinMemberIndex.MEMBER_PTR;
		expr.type = cast(TypeNode*) context.appendAst!PtrTypeNode(arrType.loc, arrType.base);
		return LookupResult.success;
	}
	else if (id == context.commonIds.id_length)
	{
		expr.memberIndex = BuiltinMemberIndex.MEMBER_LENGTH;
		expr.type = context.basicTypeNodes(BasicType.t_u64);
		return LookupResult.success;
	}

	context.error(expr.loc, "Static array `%s` has no member `%s`", arrType.typeNode.printer(context), context.idString(id));
	return LookupResult.failure;
}

LookupResult lookupStructMember(MemberExprNode* expr, StructDeclNode* structDecl, Identifier id, CompilationContext* context)
{
	AstNode* memberSym = structDecl._scope.symbols.get(id, null);
	if (!memberSym) {
		context.error(expr.loc, "Struct `%s` has no member `%s`", structDecl.typeNode.printer(context), context.idString(id));
		return LookupResult.failure;
	}

	expr.member.resolve = memberSym;
	expr.astType = AstType.expr_struct_member;

	switch(memberSym.astType)
	{
		case AstType.decl_function:
			context.internal_error("member functions/UFCS calls are not implemented");
			assert(false);

		case AstType.decl_var:
			VariableDeclNode* memberVar = expr.member.varDecl;
			expr.type = memberVar.type.foldAliases;
			expr.memberIndex = memberVar.scopeIndex;
			return LookupResult.success;

		case AstType.decl_struct:
			context.internal_error("member structs are not implemented");
			assert(false);

		case AstType.decl_enum:
			context.internal_error("member enums are not implemented");
			assert(false);

		case AstType.decl_enum_member:
			EnumMemberDecl* enumMember = memberSym.cast_decl_enum_member;
			expr.type = enumMember.type;
			expr.memberIndex = enumMember.scopeIndex;
			expr.astType = AstType.expr_enum_member;
			return LookupResult.success;

		default:
			context.internal_error("Unexpected struct member %s", memberSym.astType);
			assert(false);
	}
}
