/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.expr.name_use;

import all;

struct NameUseExprNode {
	mixin ExpressionNodeData!(AstType.expr_name_use);
	union
	{
		private AstIndex _entity; // used when resolved, node contains Identifier internally
		private Identifier _id; // used when not yet resolved
	}

	this(TokenIndex loc, Identifier id, AstIndex type = AstIndex.init, IrIndex irValue = IrIndex.init)
	{
		this.loc = loc;
		this.astType = AstType.expr_name_use;
		this.flags = AstFlags.isExpression;
		this._id = id;
		this.type = type;
		this.irValue = irValue;
	}

	void resolve(AstIndex n) {
		assert(n);
		_entity = n;
		flags |= AstFlags.isSymResolved;
	}
	AstIndex entity() { return isSymResolved ? _entity : AstIndex(); }
	Identifier id(CompilationContext* context) {
		return isSymResolved ? _entity.get_node_id(context) : _id;
	}

	T* get(T, AstType _astType)(CompilationContext* context) {
		assert(isSymResolved);
		AstNode* entityNode = context.getAstNode(_entity);
		assert(entityNode.astType == _astType, format("%s used on %s", _astType, entityNode.astType));
		return cast(T*)entityNode;
	}

	alias varDecl = get!(VariableDeclNode, AstType.decl_var);
	alias funcDecl = get!(FunctionDeclNode, AstType.decl_function);
	alias structDecl = get!(StructDeclNode, AstType.decl_struct);
	alias enumDecl = get!(EnumDeclaration, AstType.decl_enum);
	alias enumMember = get!(EnumMemberDecl, AstType.decl_enum_member);
}

void name_resolve_name_use(NameUseExprNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	node.resolve = lookupScopeIdRecursive(state.currentScope, node.id(state.context), node.loc, state.context);
	if (node.entity)
	{
		AstNode* entityNode = state.context.getAstNode(node.entity);
		switch(entityNode.astType) with(AstType) {
			case decl_function:
				node.astType = expr_func_name_use; break;
			case decl_var:
				node.astType = expr_var_name_use; break;
			case decl_struct:
				node.astType = expr_type_name_use;
				node.flags |= AstFlags.isType;
				break;
			case decl_enum_member:
				node.astType = expr_var_name_use; break;
			case decl_enum:
				node.astType = expr_type_name_use;
				node.flags |= AstFlags.isType;
				break;
			case error:
				node.astType = expr_var_name_use; break;
			default:
				state.context.internal_error("Unknown entity %s", entityNode.astType);
		}
	}
	node.state = AstNodeState.name_resolve_done;
}
