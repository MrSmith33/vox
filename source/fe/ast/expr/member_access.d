/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.member_access;

import all;

enum MemberSubType
{
	unresolved,
	builtin_member, // member is decl_builtin
	static_struct_member,
	struct_member,
	struct_method,
	enum_member,
	slice_member,
	alias_array_length,
}

enum MemberExprFlags : ushort {
	needsDeref = AstFlags.userFlag << 0,
}

// member access of aggregate.member form
// loc points to member identifier location
@(AstType.expr_member)
struct MemberExprNode {
	mixin ExpressionNodeData!(AstType.expr_member);

	AstIndex aggregate;
	union
	{
		// when unresolved
		struct {
			private Identifier _memberId; // member name before resolution
			private AstIndex parentScope; // set in parser
		}
		// when resolved
		struct {
			private AstIndex _member; // member node after resolution
			private uint _memberIndex; // resolved index of member being accessed
		}
	}

	bool isSymResolved() { return subType != MemberSubType.unresolved; }
	bool needsDeref() { return cast(bool)(flags & MemberExprFlags.needsDeref); }

	void resolve(MemberSubType subType, AstIndex member, uint memberIndex, CompilationContext* c)
	{
		assert(subType != MemberSubType.unresolved);
		assert(member);
		this.subType = subType;
		_member = member;
		_memberIndex = memberIndex;
	}

	AstIndex member(CompilationContext* c) {
		c.assertf(isSymResolved, loc, "Member access is %s, %s", cast(MemberSubType)subType, state);
		return _member;
	}
	uint memberIndex(CompilationContext* c) {
		c.assertf(isSymResolved, loc, "Member access is %s, %s", cast(MemberSubType)subType, state);
		return _memberIndex;
	}
	ref Identifier memberId(CompilationContext* c) return {
		return isSymResolved ? _member.get_node_id(c) : _memberId;
	}

	this(TokenIndex loc, AstIndex parentScope, AstIndex aggregate, Identifier memberId, AstIndex type = AstIndex.init)
	{
		this.loc = loc;
		this.astType = AstType.expr_member;
		this.state = AstNodeState.name_register_self_done;
		this.parentScope = parentScope;
		this.aggregate = aggregate;
		this._memberId = memberId;
		this.type = type;
	}

	// produce already resolved node
	this(TokenIndex loc, AstIndex parentScope, AstIndex aggregate, AstIndex member, uint memberIndex, MemberSubType subType)
	{
		this.loc = loc;
		this.astType = AstType.expr_member;
		this.state = AstNodeState.name_register_self_done;
		this.parentScope = parentScope;
		this.aggregate = aggregate;
		this._member = member;
		this.subType = subType;
		this._memberIndex = memberIndex;
	}
}

void print_member(MemberExprNode* node, ref AstPrintState state)
{
	state.print("MEMBER ", node.type.printer(state.context), " ", state.context.idString(node.memberId(state.context)), " ", cast(MemberSubType)node.subType);
	print_ast(node.aggregate, state);
}

void post_clone_member(MemberExprNode* node, ref CloneState state)
{
	assert(!node.isSymResolved);
	state.fixScope(node.parentScope);
	state.fixAstIndex(node.aggregate);
}

void name_register_nested_member(MemberExprNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.aggregate, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_member(MemberExprNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	assert(!node.isSymResolved);
	// name resolution is done in type check pass
	require_name_resolve(node.aggregate, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_member(ref AstIndex nodeIndex, MemberExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	node.state = AstNodeState.type_check;

	// try member
	// performs require_type_check on aggregate
	LookupResult res = lookupMember(nodeIndex, node, state);

	if (res == LookupResult.success) {
		lowerMember(nodeIndex, node, state);
		nodeIndex.get_node(c).state = AstNodeState.type_check_done;
		return;
	}

	// try UFCS
	AstIndex callIndex;
	LookupResult ufcsRes = tryUFCSCall(callIndex, node, state);

	if (ufcsRes == LookupResult.success) {
		nodeIndex = callIndex;
		return;
	}

	// nothing found
	node.type = CommonAstNodes.type_error;
	AstIndex objType = node.aggregate.get_node_type(c);
	c.error(node.loc, "`%s` has no member `%s`", objType.printer(c), c.idString(node.memberId(c)));
	nodeIndex.get_node(c).state = AstNodeState.type_check_done;
}

// Creates call node if it is undefined (only creates when lookup is successfull)
LookupResult tryUFCSCall(ref AstIndex callIndex, MemberExprNode* memberNode, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	AstIndex ufcsNodeIndex = lookupScopeIdRecursive(memberNode.parentScope.get_scope(c), memberNode.memberId(c), memberNode.loc, c);
	if (ufcsNodeIndex == CommonAstNodes.node_error) return LookupResult.failure;

	AstType ufcsAstType = ufcsNodeIndex.astType(c);

	if (ufcsAstType == AstType.decl_function)
	{
		// rewrite as call
		createMethodCall(callIndex, memberNode.loc, memberNode.aggregate, ufcsNodeIndex, memberNode.memberId(c), state);
		return LookupResult.success;
	}
	else
		c.internal_error(memberNode.loc, "call of %s is not supported", ufcsAstType);

	return LookupResult.failure;
}

void createMethodCall(ref AstIndex callIndex, TokenIndex loc, AstIndex aggregate, AstIndex member, Identifier calleeId, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	if (callIndex.isUndefined)
		callIndex = c.appendAst!CallExprNode(loc);

	auto call = callIndex.get!CallExprNode(c);
	call.state = AstNodeState.name_resolve_done;
	call.callee = member;
	auto method = call.callee.get!FunctionDeclNode(c);
	auto signature = method.signature.get!FunctionSignatureNode(c);
	if (method.isMember) lowerThisArgument(signature, aggregate, loc, c);
	call.args.putFront(c.arrayArena, aggregate);

	// type check call
	type_check_func_call(call, signature, calleeId, state);
}

/// Makes sure that aggregate is of pointer type
void lowerThisArgument(FunctionSignatureNode* signature, ref AstIndex aggregate, TokenIndex loc, CompilationContext* c)
{
	auto thisType = signature.parameters[0].get_node_type(c); // Struct*
	auto structType = thisType.get!PtrTypeNode(c).base.get_node_type(c); // Struct
	auto aggType = aggregate.get_node_type(c); // Struct or Struct*
	if (aggregate.get_node_type(c) == structType) // rewrite Struct as Struct*
	{
		aggregate.flags(c) |= AstFlags.isLvalue;
		aggregate = c.appendAst!UnaryExprNode(loc, AstIndex.init, UnOp.addrOf, aggregate);
	}
	aggregate.get_node(c).state = AstNodeState.name_resolve_done;
}

/// Look up member by Identifier. Searches aggregate scope for identifier.
LookupResult lookupMember(ref AstIndex nodeIndex, MemberExprNode* expr, ref TypeCheckState state)
{
	CompilationContext* c = state.context;
	if (expr.isSymResolved) {
		expr.type = expr.member(c).get_node_type(c);
		return LookupResult.success;
	}

	if (expr.aggregate.astType(c) == AstType.decl_alias_array)
	{
		expr.resolve(MemberSubType.alias_array_length, c.builtinNodes(BuiltinId.array_length), 0, c);
		expr.type = CommonAstNodes.type_u64;
		return LookupResult.success;
	}

	require_type_check(expr.aggregate, state, IsNested.no);
	TypeNode* objType = expr.aggregate.get_type(c);

	Identifier memberId = expr.memberId(c);
	if (memberId == CommonIds.id_sizeof)
	{
		expr.resolve(MemberSubType.builtin_member, c.builtinNodes(BuiltinId.type_sizeof), 0, c);
		expr.type = CommonAstNodes.type_u64;
		return LookupResult.success;
	}

	// Allow member access for pointers to structs
	if (objType.isPointer)
	{
		auto baseType = objType.as_ptr.base.get_type(c);
		if (baseType.isStruct)
		{
			objType = baseType;
			expr.flags |= MemberExprFlags.needsDeref;
		}
	}

	switch(objType.astType)
	{
		case AstType.type_slice: return lookupSliceMember(expr, objType.as_slice, memberId, c);
		case AstType.type_static_array: return lookupStaticArrayMember(expr, objType.as_static_array, memberId, c);
		case AstType.decl_struct: return lookupStructMember(nodeIndex, expr, objType.as_struct, memberId, state);
		case AstType.decl_enum: return lookupEnumMember(expr, objType.as_enum, memberId, c);
		case AstType.type_basic: return lookupBasicMember(expr, objType.as_basic, memberId, c);
		default: return LookupResult.error;
	}
}

LookupResult lookupEnumMember(MemberExprNode* expr, EnumDeclaration* enumDecl, Identifier id, CompilationContext* c)
{
	c.assertf(!enumDecl.isAnonymous, expr.loc,
		"Trying to get member from anonymous enum defined at %s",
		c.tokenLoc(enumDecl.loc));

	AstIndex memberIndex = enumDecl.memberScope.lookup_scope(id, c);
	if (!memberIndex) return LookupResult.failure;

	EnumMemberDecl* enumMember = memberIndex.get!EnumMemberDecl(c);
	expr.resolve(MemberSubType.enum_member, memberIndex, enumMember.scopeIndex, c);
	expr.type = enumMember.type;

	return LookupResult.success;
}

LookupResult lookupBasicMember(MemberExprNode* expr, BasicTypeNode* basicType, Identifier id, CompilationContext* c)
{
	if (basicType.isInteger)
	{
		if (id == CommonIds.id_min)
		{
			expr.resolve(MemberSubType.builtin_member, c.builtinNodes(BuiltinId.int_min), 0, c);
			expr.type = basicType.get_ast_index(c);
			return LookupResult.success;
		}
		else if (id == CommonIds.id_max)
		{
			expr.resolve(MemberSubType.builtin_member, c.builtinNodes(BuiltinId.int_max), 0, c);
			expr.type = basicType.get_ast_index(c);
			return LookupResult.success;
		}
	}

	return LookupResult.failure;
}

LookupResult lookupSliceMember(MemberExprNode* expr, SliceTypeNode* sliceType, Identifier id, CompilationContext* c)
{
	// use integer indicies, because slice is a struct
	if (id == CommonIds.id_ptr)
	{
		expr.resolve(MemberSubType.slice_member, c.builtinNodes(BuiltinId.slice_ptr), 1, c);
		expr.type = c.appendAst!PtrTypeNode(sliceType.loc, CommonAstNodes.type_type, sliceType.base);
		expr.type.setState(c, AstNodeState.type_check_done);
		return LookupResult.success;
	}
	else if (id == CommonIds.id_length)
	{
		expr.resolve(MemberSubType.slice_member, c.builtinNodes(BuiltinId.slice_length), 0, c);
		expr.type = CommonAstNodes.type_u64;
		expr.type.setState(c, AstNodeState.type_check_done);
		return LookupResult.success;
	}

	return LookupResult.failure;
}

LookupResult lookupStaticArrayMember(MemberExprNode* expr, StaticArrayTypeNode* arrType, Identifier id, CompilationContext* c)
{
	if (id == CommonIds.id_ptr)
	{
		expr.resolve(MemberSubType.builtin_member, c.builtinNodes(BuiltinId.array_ptr), 0, c);
		expr.type = c.appendAst!PtrTypeNode(arrType.loc, CommonAstNodes.type_type, arrType.base);
		expr.type.setState(c, AstNodeState.type_check_done);
		return LookupResult.success;
	}
	else if (id == CommonIds.id_length)
	{
		expr.resolve(MemberSubType.builtin_member, c.builtinNodes(BuiltinId.array_length), 0, c);
		expr.type = CommonAstNodes.type_u64;
		expr.type.setState(c, AstNodeState.type_check_done);
		return LookupResult.success;
	}

	return LookupResult.failure;
}

LookupResult lookupStructMember(ref AstIndex nodeIndex, MemberExprNode* node, StructDeclNode* structDecl, Identifier id, ref TypeCheckState state)
{
	CompilationContext* c = state.context;
	AstIndex entity = c.getAstScope(structDecl.memberScope).symbols.get(id, AstIndex.init);
	if (!entity) {
		return LookupResult.failure;
	}
	AstType entityAstType = entity.astType(c);

	switch(entityAstType)
	{
		case AstType.decl_function:
			node.resolve(MemberSubType.struct_method, entity, 0, c);
			node.type = entity.get_node_type(c);
			return LookupResult.success;

		case AstType.decl_var:
			auto memberVar = entity.get!VariableDeclNode(c);
			node.resolve(MemberSubType.struct_member, entity, memberVar.scopeIndex, c);
			node.type = entity.get_node_type(c);
			return LookupResult.success;

		case AstType.decl_struct:
			node.resolve(MemberSubType.static_struct_member, entity, 0, c);
			node.type = entity.get_node_type(c);
			c.internal_error("member structs are not implemented");
			assert(false);

		case AstType.decl_enum:
			node.resolve(MemberSubType.static_struct_member, entity, 0, c);
			node.type = entity.get_node_type(c);
			c.internal_error("member enums are not implemented");
			assert(false);

		case AstType.decl_enum_member:
			node.resolve(MemberSubType.enum_member, entity, 0, c);
			node.type = entity.get_node_type(c);
			return LookupResult.success;

		default:
			c.internal_error("Unexpected struct member %s", entityAstType);
			assert(false);
	}
}

void lowerMember(ref AstIndex nodeIndex, MemberExprNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;
	switch(node.subType) with(MemberSubType)
	{
		case MemberSubType.struct_method:
			AstIndex effectiveMember = node.member(c).get_effective_node(c);
			// parentheses-less method call
			AstIndex callIndex;
			createMethodCall(callIndex, node.loc, node.aggregate, effectiveMember, node.memberId(c), state);
			nodeIndex = callIndex;
			return;
		default: break;
	}
	if (node.needsDeref) {
		TypeNode* objType = node.aggregate.get_type(c);
		auto baseType = objType.as_ptr.base.get_type(c);
		node.aggregate = c.appendAst!UnaryExprNode(node.loc, c.getAstNodeIndex(baseType), UnOp.deref, node.aggregate);
		node.aggregate.setState(c, AstNodeState.type_check_done);
	}
}

ExprValue ir_gen_member(ref IrGenState gen, IrIndex currentBlock, ref IrLabel nextStmt, MemberExprNode* m)
{
	CompilationContext* c = gen.context;

	switch(m.subType) with(MemberSubType)
	{
		case struct_member, slice_member:
			IrLabel afterAggr = IrLabel(currentBlock);
			ExprValue aggr = ir_gen_expr(gen, m.aggregate, currentBlock, afterAggr);
			TypeNode* objType = m.aggregate.get_type(c);
			currentBlock = afterAggr.blockIndex;

			IrIndex memberIndex = c.constants.add(m.memberIndex(c), IsSigned.no);
			ExprValue result = aggr.member(gen, m.loc, currentBlock, memberIndex);
			gen.builder.addJumpToLabel(currentBlock, nextStmt);
			return result;
		case static_struct_member, struct_method:
			c.unreachable("Not implemented");
			assert(false);
		case enum_member:
			IrIndex result = m.member(c).get!EnumMemberDecl(c).gen_init_value_enum_member(c);
			gen.builder.addJumpToLabel(currentBlock, nextStmt);
			return ExprValue(result);
		case builtin_member:
			BuiltinId builtin = m.member(c).get!BuiltinNode(c).builtin;
			switch(builtin) with(BuiltinId)
			{
				case array_ptr:
					IrLabel afterAggr = IrLabel(currentBlock);
					ExprValue aggr = ir_gen_expr(gen, m.aggregate, currentBlock, afterAggr);
					currentBlock = afterAggr.blockIndex;
					IrIndex ptr = buildGEPEx(gen, m.loc, currentBlock, aggr, c.constants.ZERO, c.constants.ZERO);
					gen.builder.addJumpToLabel(currentBlock, nextStmt);
					return ExprValue(ptr);
				default:
					return ExprValue(eval_builtin_member(builtin, m.aggregate, m.loc, c));
			}
		default:
			c.internal_error(m.loc, "Unexpected node type %s", m.astType);
			assert(false);
	}
}
