/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.decl.enum_;

import vox.all;
import vox.fe.ast.ast_index;

@(AstType.decl_enum)
struct EnumDeclaration
{
	mixin ScopeDeclNodeData!(AstType.decl_enum, AstFlags.isType);
	ScopeIndex parentScope;
	ScopeIndex memberScope;
	AstIndex memberType;
	Identifier id;

	private enum Flags : ushort
	{
		isAnonymous = AstFlags.userFlag
	}

	this(TokenIndex loc, ScopeIndex parentScope, ScopeIndex memberScope, AstNodes members, AstIndex memberType, Identifier id)
	{
		this.loc = loc;
		this.astType = AstType.decl_enum;
		this.flags = AstFlags.isType;
		this.parentScope = parentScope;
		this.memberScope = memberScope;
		this.declarations = members;
		this.memberType = memberType;
		this.id = id;
	}

	/// Anonymous / not a type
	this(TokenIndex loc, ScopeIndex parentScope, ScopeIndex memberScope, AstNodes members, AstIndex memberType)
	{
		this.loc = loc;
		this.astType = AstType.decl_enum;
		this.flags = Flags.isAnonymous;
		this.parentScope = parentScope;
		this.memberScope = memberScope;
		this.declarations = members;
		this.memberType = memberType;
	}

	bool isAnonymous() { return cast(bool)(flags & Flags.isAnonymous); }
	SizeAndAlignment sizealign(CompilationContext* c) {
		c.assertf(!isAnonymous, loc, "Anonymous enums are not a type");
		c.assertf(memberType.isDefined, loc, "Enum has no member type");
		return require_type_size(memberType, c);
	}
}

void print_enum(EnumDeclaration* node, ref AstPrintState state)
{
	if (node.isAnonymous)
		state.print("ENUM ", node.memberType.printer(state.context));
	else
		state.print("ENUM ", node.memberType.printer(state.context), " ", state.context.idString(node.id));
	print_ast(node.declarations, state);
}

void post_clone_enum(EnumDeclaration* node, ref CloneState state)
{
	state.fixScope(node.parentScope);
	state.fixScope(node.memberScope);
	state.fixAstIndex(node.memberType);
	state.fixAstNodes(node.declarations);
}

void name_register_self_enum(AstIndex nodeIndex, EnumDeclaration* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_self;
	if (!node.isAnonymous) node.parentScope.insert_scope(node.id, nodeIndex, state.context);
	node.state = AstNodeState.name_register_self_done;
}

void name_register_nested_enum(AstIndex nodeIndex, EnumDeclaration* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	require_name_register(node.memberType, state);
	require_name_register(node.declarations, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_enum(EnumDeclaration* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.memberType, state);
	require_name_resolve(node.declarations, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_enum(EnumDeclaration* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.memberType, state);
	require_type_check(node.declarations, state, IsNested.no);
	node.state = AstNodeState.type_check_done;
}

IrIndex gen_ir_type_enum(EnumDeclaration* node, CompilationContext* context)
{
	return gen_ir_type(node.memberType, context);
}

IrIndex gen_init_value_enum(EnumDeclaration* node, CompilationContext* c)
{
	c.assertf(node.declarations.length > 0, node.loc, "Enum %s has no members", c.idString(node.id));
	return node.declarations[0].get!EnumMemberDecl(c).gen_init_value_enum_member(c);
}

@(AstType.decl_enum_member)
struct EnumMemberDecl
{
	mixin AstNodeData!(AstType.decl_enum_member);
	ScopeIndex parentScope;
	AstIndex type;
	AstIndex initializer;
	Identifier id;
	ushort scopeIndex;
	IrIndex initValue; // cached value of initializer, calculated in type check
}

IrIndex gen_init_value_enum_member(EnumMemberDecl* node, CompilationContext* c) {
	final switch(node.getPropertyState(NodeProperty.init_value)) {
		case PropertyState.not_calculated: break;
		case PropertyState.calculating: c.circular_dependency;
		case PropertyState.calculated: return node.initValue;
	}

	c.begin_node_property_calculation(node, NodeProperty.init_value);
	scope(exit) c.end_node_property_calculation(node, NodeProperty.init_value);

	if (node.initializer) {
		if (node.type) {
			auto type = node.type.get_node(c);

			if (type.astType == AstType.decl_enum) {
				require_type_check(type.as!EnumDeclaration(c).memberType, c, IsNested.no);
			} else require_type_check(node.type, c);

			require_type_check_expr(node.type, node.initializer, c);
			//writefln("  autoconvTo %s", printer(node.type, c));
			TypeConvResKind res = checkTypeConversion(node.initializer.get_expr_type(c), node.type, node.initializer, c);
			if (res.successful) {
				insertCast(node.initializer, node.type, res, c);
				if (node.initializer.get_expr_type(c) != CommonAstNodes.type_error)
					node.initValue = eval_static_expr(node.initializer, c);
			} else {
				c.error(node.initializer.loc(c),
					"Cannot convert expression of type `%s` to `%s`",
					node.initializer.get_expr_type(c).printer(c),
					node.type.printer(c));
			}
		} else {
			require_type_check(node.initializer, c);
			node.type = get_expr_type(node.initializer, c);
			node.initValue = eval_static_expr(node.initializer, c);
		}
	}
	return node.initValue;
}

void print_enum_member(EnumMemberDecl* node, ref AstPrintState state)
{
	state.print("ENUM MEMBER ", node.type.printer(state.context), " ", state.context.idString(node.id));
	if (node.initializer) print_ast(node.initializer, state);
}

void post_clone_enum_member(EnumMemberDecl* node, ref CloneState state)
{
	state.fixScope(node.parentScope);
	state.fixAstIndex(node.type);
	state.fixAstIndex(node.initializer);
}

void name_register_self_enum_member(AstIndex nodeIndex, EnumMemberDecl* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_self;
	node.parentScope.insert_scope(node.id, nodeIndex, state.context);
	node.state = AstNodeState.name_register_self_done;
}

void name_register_nested_enum_member(AstIndex nodeIndex, EnumMemberDecl* node, ref NameRegisterState state) {
	CompilationContext* c = state.context;
	node.state = AstNodeState.name_register_nested;
	if (node.type) {
		auto type = node.type.get_node(c);
		if (type.astType == AstType.decl_enum) {
			require_name_register(type.as!EnumDeclaration(c).memberType, state);
		} else require_name_register(node.type, state);
	}
	if (node.initializer) require_name_register(node.initializer, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_enum_member(EnumMemberDecl* node, ref NameResolveState state) {
	CompilationContext* c = state.context;
	node.state = AstNodeState.name_resolve;
	if (node.type) {
		auto type = node.type.get_node(c);
		if (type.astType == AstType.decl_enum) {
			require_name_resolve(type.as!EnumDeclaration(c).memberType, state);
		} else require_name_resolve(node.type, state);
	}
	if (node.initializer) require_name_resolve(node.initializer, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_enum_member(EnumMemberDecl* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;
	node.state = AstNodeState.type_check;
	gen_init_value_enum_member(node, c);
	node.state = AstNodeState.type_check_done;
}
