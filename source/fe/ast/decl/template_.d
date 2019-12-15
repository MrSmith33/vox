/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.template_;

import all;

@(AstType.decl_template)
struct TemplateDeclNode
{
	mixin AstNodeData!(AstType.decl_template, AstFlags.isDeclaration);
	/// For template name register
	AstIndex parentScope;
	/// Template parameters
	AstNodes parameters;
	/// Templated AST node (currently function or struct)
	AstIndex body;
	/// Points to the first index that needs to be copied
	AstIndex body_start;
	/// Points to the next index after body data
	AstIndex after_body;
	/// Template id. Same as underlying entity.
	Identifier id;
	/// Cached template instances
	Array!TemplateInstance instances;
}

struct TemplateInstance
{
	/// Template arguments of this instance
	AstNodes args;
	/// AST node created for instantiated template. Copy of TemplateDeclNode.body
	AstIndex entity;
}

void post_clone_template(TemplateDeclNode* node, ref CloneState state)
{
	state.fixScope(node.parentScope);
	state.fixAstNodes(node.parameters);
	state.fixAstIndex(node.body);

	// TemplateDeclNode.after_body can be == to CloneState.cloned_to
	// Fix it manually
	// And it doesn't need node post clone code called
	node.body_start.storageIndex += state.offset;
	node.after_body.storageIndex += state.offset;
}

void name_register_self_template(AstIndex nodeIndex, TemplateDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	node.parentScope.insert_scope(node.id, nodeIndex, state.context);
	node.state = AstNodeState.type_check_done;
}


@(AstType.decl_template_param)
struct TemplateParamDeclNode
{
	mixin AstNodeData!(AstType.decl_template_param, AstFlags.isDeclaration);
	Identifier id;
}


struct CloneState
{
	CompilationContext* context;

	// We need to add this offset to all indicies inside copied slots that point into copied slots
	uint offset;

	/// Points to original nodes
	AstIndex cloned_from;
	AstIndex cloned_to;

	/// We want to redirect references to this scope to `instance_scope`
	AstIndex template_parent_scope;
	/// Scope where template was instantiated. Contains template arguments
	AstIndex instance_scope;

	void fixAstIndex(ref AstIndex nodeIndex)
	{
		// null is automatically out of bounds
		if (nodeIndex.storageIndex >= cloned_from.storageIndex && nodeIndex.storageIndex < cloned_to.storageIndex)
		{
			nodeIndex.storageIndex += offset;
			post_clone(nodeIndex, this);
		}
	}

	void fixAstNodes(ref AstNodes nodes)
	{
		nodes = nodes.dup(context.arrayArena);
		foreach(ref AstIndex index; nodes)
			fixAstIndex(index);
	}

	void fixScope(ref AstIndex _scope)
	{
		// null is automatically out of bounds
		if (_scope.storageIndex >= cloned_from.storageIndex && _scope.storageIndex < cloned_to.storageIndex)
		{
			_scope.storageIndex += offset;
			fixScope(_scope.get_scope(context).parentScope);
			// Scope.symbols/imports dont need fixing, because no symbols are registered at this point
		}
		else if (_scope == template_parent_scope)
		{
			assert(_scope.isDefined);
			// reidrect to this scope for instance argument resolution
			_scope = instance_scope;
		}
	}
}

AstIndex get_template_instance(AstIndex templateIndex, TokenIndex start, AstNodes args, ref TypeCheckState state)
{
	CompilationContext* c = state.context;
	auto templ = templateIndex.get!TemplateDeclNode(c);

	// Verify arguments. For now only types are supported
	foreach(size_t i, AstIndex arg; args)
	{
		if (!arg.isType(c))
		{
			c.error(arg.loc(c),
				"Template argument %s, must be a type, not %s", i+1,
				arg.astType(c));
		}
	}

	// Check if there is existing instance
	instance_loop:
	foreach(ref TemplateInstance instance; templ.instances)
	{
		foreach(size_t i, AstIndex arg; instance.args)
		{
			if (!same_type(arg, args[i], c)) continue instance_loop;
		}

		// Found match, reuse instance
		return instance.entity;
	}

	// No matching instances found
	// Create new instance

	// Create scope for arguments
	AstIndex instance_scope = c.appendAst!Scope;
	Scope* newScope = c.getAst!Scope(instance_scope);
	newScope.parentScope = templ.parentScope;
	newScope.debugName = "template instance";
	newScope.kind = newScope.parentScope.get!Scope(c).kind;

	// Register template instance arguments
	foreach(size_t i, AstIndex arg; args)
	{
		auto param = templ.parameters[i].get!TemplateParamDeclNode(c);
		//writefln("reg %s %s into %s", c.idString(param.id), arg, instance_scope);
		newScope.insert(param.id, arg, c);
	}

	// Clone template body and apply fixes
	AstIndex instance = clone_node(templ.body, templ.body_start, templ.after_body, instance_scope, c);

	// Node may need to know if is a result of template instantiation
	instance.flags(c) |= AstFlags.isTemplateInstance;

	// Create identifier for instance
	set_instance_id(instance, args, c);

	// Type check instance
	require_type_check(instance, c);

	// Cache instance
	templ.instances.put(c.arrayArena, TemplateInstance(args, instance));

	return instance;
}

void set_instance_id(AstIndex instance_index, AstNodes instance_args, CompilationContext* c)
{
	Identifier* id = &instance_index.get_node_id(c);
	TextSink* sink = &c.idMap.tempBuf;
	sink.clear;
	sink.put(c.idString(*id));
	sink.put("[");
	foreach(size_t i, AstIndex arg; instance_args)
	{
		if (i > 0) sink.put(", ");
		print_node_name(*sink, arg, c);
	}
	sink.put("]");
	const(char)[] idString = sink.data.data;
	*id = c.idMap.getOrReg(idString);
}

/// Perform copiying of AST subtree and index fixing
/// nodeIndex is the root of subtree
/// node_start..after_node is the range of slots to be copied
/// instance_scope is the scope created around AST subtree copy
AstIndex clone_node(AstIndex nodeIndex, AstIndex node_start, AstIndex after_node, AstIndex instance_scope, CompilationContext* c)
{
	c.assertf(after_node.storageIndex > node_start.storageIndex, "%s > %s", node_start, after_node);
	AstIndex slots_start = AstIndex(c.astBuffer.uintLength);
	c.assertf(slots_start.storageIndex > node_start.storageIndex, "%s > %s", slots_start, node_start);

	uint num_slots_to_copy = after_node.storageIndex - node_start.storageIndex;
	// allocate space at the end
	uint[] slots = c.astBuffer.voidPut(num_slots_to_copy);
	// copy slots
	slots[] = c.astBuffer.bufPtr[node_start.storageIndex..after_node.storageIndex];

	CloneState state = {
		context : c,
		offset : slots_start.storageIndex - node_start.storageIndex,
		cloned_from : node_start,
		cloned_to : after_node,
		template_parent_scope : instance_scope.get_scope(c).parentScope,
		instance_scope : instance_scope,
	};

	state.fixAstIndex(nodeIndex);

	return nodeIndex;
}

/// Applies offset to indicies pointing into copied area
/// Happens before name register
void post_clone(AstIndex nodeIndex, ref CloneState state)
{
	CompilationContext* c = state.context;

	AstNode* node = c.getAstNode(nodeIndex);

	final switch(node.astType) with(AstType)
	{
		case error: c.internal_error(node.loc, "Visiting error node"); break;
		case abstract_node: c.internal_error(node.loc, "Visiting abstract node"); break;

		case decl_alias: post_clone_alias(cast(AliasDeclNode*)node, state); break;
		case decl_builtin: break;
		case decl_module: assert(false);
		case decl_import: post_clone_import(cast(ImportDeclNode*)node, state); break;
		case decl_function: post_clone_func(cast(FunctionDeclNode*)node, state); break;
		case decl_var: post_clone_var(cast(VariableDeclNode*)node, state); break;
		case decl_struct: post_clone_struct(cast(StructDeclNode*)node, state); break;
		case decl_enum: post_clone_enum(cast(EnumDeclaration*)node, state); break;
		case decl_enum_member: post_clone_enum_member(cast(EnumMemberDecl*)node, state); break;
		case decl_static_if: post_clone_static_if(cast(StaticIfDeclNode*)node, state); break;
		case decl_template: post_clone_template(cast(TemplateDeclNode*)node, state); break;
		case decl_template_param: break;

		case stmt_block: post_clone_block(cast(BlockStmtNode*)node, state); break;
		case stmt_if: post_clone_if(cast(IfStmtNode*)node, state); break;
		case stmt_while: post_clone_while(cast(WhileStmtNode*)node, state); break;
		case stmt_do_while: post_clone_do(cast(DoWhileStmtNode*)node, state); break;
		case stmt_for: post_clone_for(cast(ForStmtNode*)node, state); break;
		case stmt_return: post_clone_return(cast(ReturnStmtNode*)node, state); break;
		case stmt_break: break;
		case stmt_continue: break;

		case expr_name_use: post_clone_name_use(cast(NameUseExprNode*)node, state); break;
		case expr_member: post_clone_member(cast(MemberExprNode*)node, state); break;
		case expr_bin_op: post_clone_binary_op(cast(BinaryExprNode*)node, state); break;
		case expr_un_op: post_clone_unary_op(cast(UnaryExprNode*)node, state); break;
		case expr_call: post_clone_call(cast(CallExprNode*)node, state); break;
		case expr_index: post_clone_index(cast(IndexExprNode*)node, state); break;
		case expr_slice: post_clone_expr_slice(cast(SliceExprNode*)node, state); break;
		case expr_type_conv: post_clone_type_conv(cast(TypeConvExprNode*)node, state); break;

		case literal_int: break;
		case literal_string: break;
		case literal_null: break;
		case literal_bool: break;

		case type_basic: break;
		case type_func_sig: post_clone_func_sig(cast(FunctionSignatureNode*)node, state); break;
		case type_ptr: post_clone_ptr(cast(PtrTypeNode*)node, state); break;
		case type_static_array: post_clone_static_array(cast(StaticArrayTypeNode*)node, state); break;
		case type_slice: post_clone_slice(cast(SliceTypeNode*)node, state); break;
	}
}
