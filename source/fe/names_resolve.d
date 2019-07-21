/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.names_resolve;

import std.stdio;
import std.string : format;
import all;

void pass_names_resolve(ref CompilationContext ctx, CompilePassPerModule[] subPasses)
{
	auto sem2 = PassNamesResolve(&ctx);
	foreach (ref SourceFileInfo file; ctx.files.data) {
		sem2.visit(file.mod);
	}
}

/// Error means that lookup failed due to earlier failure or error, so no new error should be produced
enum LookupResult : ubyte {
	success,
	failure,
	error
}

/// Look up symbol by Identifier. Searches the stack of scopes.
AstNode* lookup(Scope* scop, const Identifier id, TokenIndex from, CompilationContext* context)
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

struct CommonIdentifiers
{
	Identifier id_ptr;
	Identifier id_length;
	Identifier id_min;
	Identifier id_max;
}

CommonIdentifiers collectIdentifiers(CompilationContext* context) {
	CommonIdentifiers res;
	res.id_ptr = context.idMap.getOrRegNoDup("ptr");
	res.id_length = context.idMap.getOrRegNoDup("length");
	res.id_min = context.idMap.getOrRegNoDup("min");
	res.id_max = context.idMap.getOrRegNoDup("max");
	return res;
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

/// Resolves all symbol references (variable/type/function uses)
/// using information collected on previous pass
struct PassNamesResolve
{
	mixin AstVisitorMixin;

	CompilationContext* context;

	Scope* curScope;

	void pushCompleteScope(Scope* newScope)
	{
		curScope = newScope;
	}

	void popScope()
	{
		assert(curScope);

		if (curScope.parentScope)
			curScope = curScope.parentScope;
		else
			curScope = null;
	}

	void visit(ModuleDeclNode* m) {
		pushCompleteScope(m._scope);
		foreach (decl; m.declarations) _visit(decl);
		popScope;
	}
	void visit(ImportDeclNode* i) {}
	void visit(FunctionDeclNode* f) {
		pushCompleteScope(f._scope);
		_visit(f.returnType);
		foreach (param; f.parameters) visit(param);
		if (f.block_stmt) visit(f.block_stmt);
		popScope;
	}
	void visit(VariableDeclNode* v) {
		_visit(v.type);
		if (v.initializer) _visit(v.initializer);
	}
	void visit(StructDeclNode* s) {
		pushCompleteScope(s._scope);
		foreach (decl; s.declarations) _visit(decl);
		popScope;
	}
	void visit(EnumDeclaration* e) {
		if (e.isAnonymous)
		{
			foreach (decl; e.declarations) _visit(decl);
		}
		else
		{
			pushCompleteScope(e._scope);
			foreach (decl; e.declarations) _visit(decl);
			popScope;
		}
	}
	void visit(EnumMemberDecl* m) {
		_visit(m.type);
		if (m.initializer) _visit(m.initializer);
	}
	void visit(BlockStmtNode* b) {
		pushCompleteScope(b._scope);
		foreach(stmt; b.statements) _visit(stmt);
		popScope;
	}
	void visit(IfStmtNode* i) {
		_visit(i.condition);
		pushCompleteScope(i.then_scope);
		_visit(i.thenStatement);
		popScope;
		if (i.elseStatement) {
			pushCompleteScope(i.else_scope);
			_visit(i.elseStatement);
			popScope;
		}
	}
	void visit(WhileStmtNode* w) {
		_visit(w.condition);
		pushCompleteScope(w._scope);
		_visit(w.statement);
		popScope;
	}
	void visit(DoWhileStmtNode* d) {
		pushCompleteScope(d._scope);
		_visit(d.statement);
		popScope;
		_visit(d.condition);
	}
	void visit(ForStmtNode* n) {
		pushCompleteScope(n._scope);
		foreach(stmt; n.init_statements) _visit(stmt);
		if (n.condition) _visit(n.condition);
		foreach(stmt; n.increment_statements) _visit(stmt);
		_visit(n.statement);
		popScope;
	}
	void visit(ReturnStmtNode* r) {
		if (r.expression) _visit(r.expression);
	}
	void visit(BreakStmtNode* r) {}
	void visit(ContinueStmtNode* r) {}
	void visit(NameUseExprNode* v) {
		v.resolve = lookup(curScope, v.id, v.loc, context);
		if (v.entity !is null)
		{
			switch(v.entity.astType) with(AstType) {
				case decl_function:
					v.astType = expr_func_name_use; break;
				case decl_var:
					v.astType = expr_var_name_use; break;
				case decl_struct:
					v.astType = expr_type_name_use; break;
				case decl_enum_member:
					v.astType = expr_var_name_use; break;
				case decl_enum:
					v.astType = expr_type_name_use; break;
				case error:
					v.astType = expr_var_name_use; break;
				default:
					context.internal_error("Unknown entity %s", v.entity.astType);
			}
		}
	}
	void visit(MemberExprNode* m) {
		_visit(m.aggregate);
	}
	void visit(IntLiteralExprNode* c) {}
	void visit(StringLiteralExprNode* c) {}
	void visit(NullLiteralExprNode* c) {}
	void visit(BoolLiteralExprNode* c) {}
	void visit(BinaryExprNode* b) {
		_visit(b.left);
		_visit(b.right);
	}
	void visit(UnaryExprNode* u) { _visit(u.child); }
	void visit(CallExprNode* c) {
		_visit(c.callee);
		foreach (arg; c.args) _visit(arg); }
	void visit(IndexExprNode* i) {
		_visit(i.array);
		_visit(i.index);
	}
	void visit(TypeConvExprNode* t) { _visit(t.type); _visit(t.expr); }
	void visit(BasicTypeNode* t) {}
	void visit(PtrTypeNode* t) { _visit(t.base); }
	void visit(StaticArrayTypeNode* t) { _visit(t.base); }
	void visit(SliceTypeNode* t) { _visit(t.base); }
}
