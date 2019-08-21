/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.member_access;

import all;

enum MemberSubType
{
	unresolved,
	basic_member,
	nonstatic_struct_member,
	static_struct_member,
	enum_member,
	slice_member,
	static_array_member,
}

// member access of aggregate.member form
struct MemberExprNode {
	mixin ExpressionNodeData!(AstType.expr_member);
	AstIndex aggregate;
	AstIndex member; // member name (NameUseExprNode)
	uint memberIndex; // resolved index of member being accessed
	AstIndex curScope; // set in name resolve pass
}

void name_resolve_member(MemberExprNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	node.curScope = state.context.getAstNodeIndex(state.currentScope);
	// name resolution is done in type check pass
	require_name_resolve(node.aggregate, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_member(ref AstIndex nodeIndex, MemberExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;
	node.state = AstNodeState.type_check;

	// try member
	NameUseExprNode* member = node.member.get_expr_name_use(c);
	require_type_check(node.aggregate, state);

	LookupResult res = lookupMember(node, c);

	if (res == LookupResult.success) {
		nodeIndex.get_node(c).state = AstNodeState.name_resolve_done;
		return;
	}

	// try UFCS
	AstIndex ufcsNodeIndex = lookupScopeIdRecursive(node.curScope.get_scope(c), member.id(c), node.loc, c);
	if (ufcsNodeIndex)
	{
		AstNode* ufcsNode = c.getAstNode(ufcsNodeIndex);
		if (ufcsNode.astType == AstType.decl_function)
		{
			// rewrite as call
			member.resolve(ufcsNodeIndex);
			Array!AstIndex args;
			args.put(c.arrayArena, node.aggregate);
			nodeIndex = c.appendAst!CallExprNode(member.loc, AstIndex(), IrIndex(), node.member, args);
			nodeIndex.get_node(c).state = AstNodeState.name_resolve_done;
			// type check call
			require_type_check(nodeIndex, state);
			return;
		}
	}

	// nothing found
	node.type = c.basicTypeNodes(BasicType.t_error);
	AstIndex objType = node.aggregate.get_node_type(c);
	c.error(node.loc, "`%s` has no member `%s`", objType.printer(c), c.idString(member.id(c)));

	node.state = AstNodeState.type_check_done;
}

/// Look up member by Identifier. Searches aggregate scope for identifier.
LookupResult lookupMember(MemberExprNode* expr, CompilationContext* context)
{
	NameUseExprNode* memberNode = context.getAst!NameUseExprNode(expr.member);
	TypeNode* obj = expr.aggregate.get_type(context);

	// Allow member access for pointers to structs
	if (obj.isPointer)
		obj = obj.as_ptr.base.get_type(context);

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

	expr.subType = MemberSubType.enum_member;

	AstIndex memberIndex = enumDecl._scope.get_scope(context).symbols.get(id, AstIndex.init);
	if (!memberIndex) return LookupResult.failure;

	AstNode* memberNode = memberIndex.get_node(context);

	context.assertf(memberNode.astType == AstType.decl_enum_member, expr.loc,
		"Unexpected enum member %s", memberNode.astType);

	expr.member.get_name_use(context).resolve(memberIndex);

	EnumMemberDecl* enumMember = memberNode.cast_decl_enum_member;
	expr.type = enumMember.type;
	expr.memberIndex = enumMember.scopeIndex;

	return LookupResult.success;
}

LookupResult lookupBasicMember(MemberExprNode* expr, BasicTypeNode* basicType, Identifier id, CompilationContext* context)
{
	expr.subType = MemberSubType.basic_member;

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
	expr.subType = MemberSubType.slice_member;

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
	expr.subType = MemberSubType.static_array_member;

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
	AstIndex entity = context.getAstScope(structDecl._scope).symbols.get(id, AstIndex.init);
	if (!entity) {
		return LookupResult.failure;
	}

	NameUseExprNode* memberNode = context.getAst!NameUseExprNode(expr.member);
	AstNode* entityNode = context.getAstNode(entity);

	switch(entityNode.astType)
	{
		case AstType.decl_function:
			expr.subType = MemberSubType.static_struct_member;
			memberNode.resolve(entity);
			context.internal_error("member functions/UFCS calls are not implemented");
			assert(false);

		case AstType.decl_var:
			expr.subType = MemberSubType.nonstatic_struct_member;
			memberNode.resolve(entity);
			VariableDeclNode* memberVar = memberNode.varDecl(context);
			expr.type = memberVar.type.get_type(context).foldAliases(context).get_ast_index(context);
			expr.memberIndex = memberVar.scopeIndex;
			return LookupResult.success;

		case AstType.decl_struct:
			expr.subType = MemberSubType.static_struct_member;
			memberNode.resolve(entity);
			context.internal_error("member structs are not implemented");
			assert(false);

		case AstType.decl_enum:
			expr.subType = MemberSubType.static_struct_member;
			memberNode.resolve(entity);
			context.internal_error("member enums are not implemented");
			assert(false);

		case AstType.decl_enum_member:
			expr.subType = MemberSubType.enum_member;
			memberNode.resolve(entity);
			EnumMemberDecl* enumMember = entityNode.cast_decl_enum_member;
			expr.type = enumMember.type;
			return LookupResult.success;

		default:
			context.internal_error("Unexpected struct member %s", entityNode.astType);
			assert(false);
	}
}
