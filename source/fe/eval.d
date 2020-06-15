/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Constant folding and Compile-time function evaluation (CTFE)
/// Requires nodes that are evaluated to be type checked
module fe.eval;

import all;

/// Eval only expressions now. No CTFE yet
/// Returns integer constant
IrIndex eval_static_expr(AstIndex nodeIndex, CompilationContext* context)
{
	AstNode* node = context.getAstNode(nodeIndex);

	switch(node.state) with(AstNodeState)
	{
		case name_register_self_done:
			require_name_register(nodeIndex, context);
			context.throwOnErrors;
			goto case;
		case name_register_nested_done:
			require_name_resolve(nodeIndex, context);
			context.throwOnErrors;
			goto case;
		case name_resolve_done:
			// perform type checking of forward referenced node
			require_type_check(nodeIndex, context);
			context.throwOnErrors;
			break;
		case type_check_done: break; // all requirement are done
		default: context.internal_error(node.loc, "Node %s in %s state", node.astType, node.state);
	}

	context.assertf(node !is null, "null node");

	switch (node.astType) with(AstType)
	{
		case decl_enum_member: return node.as!EnumMemberDecl(context).getInitVal(context);
		case expr_name_use: return eval_static_expr_name_use(cast(NameUseExprNode*)node, context);
		case expr_member: return eval_static_expr_member(cast(MemberExprNode*)node, context);
		case expr_bin_op: return eval_static_expr_bin_op(cast(BinaryExprNode*)node, context);
		case expr_un_op: return eval_static_expr_un_op(cast(UnaryExprNode*)node, context);
		case expr_type_conv: return eval_static_expr_type_conv(cast(TypeConvExprNode*)node, context);
		case expr_call: return eval_static_expr_call(cast(CallExprNode*)node, context);
		case literal_int: return ir_gen_literal_int(context, cast(IntLiteralExprNode*)node);
		case literal_string: return ir_gen_literal_string(context, cast(StringLiteralExprNode*)node);
		case literal_null: return ir_gen_literal_null(context, cast(NullLiteralExprNode*)node);
		case literal_bool: return ir_gen_literal_bool(context, cast(BoolLiteralExprNode*)node);
		default:
			context.internal_error(node.loc, "Cannot evaluate static expression %s", node.astType);
			assert(false);
	}
}

/// Evaluates expression that results in $alias and returns it as AstIndex
AstIndex eval_static_expr_alias(AstIndex nodeIndex, CompilationContext* c)
{
	IrIndex val = eval_static_expr(nodeIndex, c);
	AstNode* node = c.getAstNode(nodeIndex);
	if (nodeIndex.get_node_type(c) != CommonAstNodes.type_alias)
		c.internal_error(node.loc, "Cannot evaluate static expression %s as $alias", node.astType);
	if (!val.isSomeConstant)
		c.internal_error(node.loc, "Cannot obtain $alias from %s", val);
	IrConstant con = c.constants.get(val);
	if (con.payloadSize(val) > IrArgSize.size32)
		c.internal_error(node.loc, "Cannot obtain $alias from %s, too big", val);
	return AstIndex(con.i32);
}

/// Evaluates expression that results in $type and returns it as AstIndex
AstIndex eval_static_expr_type(AstIndex nodeIndex, CompilationContext* c)
{
	IrIndex val = eval_static_expr(nodeIndex, c);
	AstNode* node = c.getAstNode(nodeIndex);
	if (nodeIndex.get_node_type(c) != CommonAstNodes.type_type)
		c.internal_error(node.loc, "Cannot evaluate static expression %s as $type", node.astType);
	if (!val.isSomeConstant)
		c.internal_error(node.loc, "Cannot obtain $type from %s", val);
	IrConstant con = c.constants.get(val);
	if (con.payloadSize(val) > IrArgSize.size32)
		c.internal_error(node.loc, "Cannot obtain $type from %s, too big", val);
	return AstIndex(con.i32);
}

IrIndex eval_static_expr_name_use(NameUseExprNode* node, CompilationContext* context)
{
	return eval_static_expr(node.entity, context);
}

IrIndex eval_static_expr_member(MemberExprNode* node, CompilationContext* c)
{
	switch(node.subType) with(MemberSubType)
	{
		case enum_member:
			return eval_static_expr(node.member(c), c);
		case builtin_member:
			return eval_builtin_member(node.member(c).get!BuiltinNode(c).builtin, node.aggregate, node.loc, c);
		default:
			AstIndex nodeIndex = get_ast_index(node, c);
			c.unrecoverable_error(node.loc,
				"Cannot access .%s member of %s while in CTFE",
				get_node_id(nodeIndex, c),
				get_node_kind_name(nodeIndex, c));
			assert(false);
	}
}

IrIndex eval_builtin_member(BuiltinId builtin, AstIndex obj, TokenIndex loc, CompilationContext* c)
{
	AstIndex objType = obj.get_node_type(c);
	switch(builtin) with(BuiltinId)
	{
		case int_min:
			auto b = objType.get!BasicTypeNode(c);
			return c.constants.add(b.minValue, b.isSigned, objType.get_type(c).argSize(c));
		case int_max:
			auto b = objType.get!BasicTypeNode(c);
			return c.constants.add(b.maxValue, b.isSigned, objType.get_type(c).argSize(c));
		case array_length:
			require_type_check(objType, c);
			return c.constants.add(objType.get!StaticArrayTypeNode(c).length, IsSigned.no, IrArgSize.size64);
		case type_sizeof:
			require_type_check(objType, c);
			return c.constants.add(objType.get_type(c).size(c), IsSigned.no, IrArgSize.size64);
		default:
			c.unrecoverable_error(loc,
				"Cannot access .%s member of %s while in CTFE",
				builtinIdStrings[builtin],
				get_node_kind_name(objType, c));
			assert(false);
	}
}

IrIndex eval_static_expr_bin_op(BinaryExprNode* node, CompilationContext* c)
{
	switch (node.op) {
		case BinOp.LOGIC_AND:
			IrIndex leftVal = eval_static_expr(node.left, c);
			IrConstant leftCon = c.constants.get(leftVal);
			if (!leftCon.i64) return c.constants.add(0, IsSigned.no, IrArgSize.size8);
			return eval_static_expr(node.right, c);
		case BinOp.LOGIC_OR:
			IrIndex leftVal = eval_static_expr(node.left, c);
			IrConstant leftCon = c.constants.get(leftVal);
			if (leftCon.i64) return c.constants.add(1, IsSigned.no, IrArgSize.size8);
			return eval_static_expr(node.right, c);
		default:
			IrIndex leftVal = eval_static_expr(node.left, c);
			IrIndex rightVal = eval_static_expr(node.right, c);
			return calcBinOp(node.op, leftVal, rightVal, node.type.typeArgSize(c), c);
	}
}

IrIndex eval_static_expr_un_op(UnaryExprNode* node, CompilationContext* c)
{
	ExpressionNode* child = node.child.get_expr(c);
	switch (node.op) with(UnOp)
	{
		case addrOf:
			switch(child.astType)
			{
				case AstType.expr_name_use:
					AstNode* entity = child.as!NameUseExprNode(c).entity.get_node(c);

					switch (entity.astType)
					{
						// TODO: force IR gen for global var when address is taken
						case AstType.decl_function:
							// type is not pointer to function sig, but sig itself
							return entity.as!FunctionDeclNode(c).getIrIndex(c);
						case AstType.decl_var:
							// must be global
							auto v = entity.as!VariableDeclNode(c);
							if (v.isGlobal)
								return v.getIrIndex(c);
							else
								c.unrecoverable_error(node.loc, "Can only take address of global variable while in CTFE");
							assert(false);
						default:
							c.unrecoverable_error(node.loc, "Cannot take address of %s while in CTFE", entity.astType);
							assert(false);
					}
					assert(false);
				default:
					c.unrecoverable_error(node.loc, "Cannot take address of %s while in CTFE", child.astType);
					assert(false);
			}
			break;
		default:
			c.internal_error(node.loc, "%s not implemented in CTFE", node.op);
			assert(false);
	}
}

IrIndex eval_static_expr_type_conv(TypeConvExprNode* node, CompilationContext* c)
{
	if (node.type == CommonAstNodes.type_alias || node.type == CommonAstNodes.type_type)
	{
		AstIndex aliasedNode = get_effective_node(node.expr, c);
		return c.constants.add(aliasedNode.storageIndex, IsSigned.no, IrArgSize.size32);
	}
	return eval_static_expr(node.expr, c);
}

IrIndex eval_static_expr_call(CallExprNode* node, CompilationContext* c)
{
	AstIndex callee = node.callee.get_effective_node(c);

	switch (callee.astType(c))
	{
		case AstType.decl_struct:
			return eval_constructor(node, callee, c);
		case AstType.decl_function:
			return eval_call(node, callee, c);
		default:
			c.internal_error(node.loc, "Cannot call %s at compile-time", callee.get_node_type(c).get_type(c).printer(c));
			assert(false);
	}
}

IrIndex eval_constructor(CallExprNode* node, AstIndex callee, CompilationContext* c)
{
	StructDeclNode* s = callee.get!StructDeclNode(c);

	if (node.args.length == 0) {
		return s.gen_default_value_struct(c);
	}

	IrIndex structType = s.gen_ir_type_struct(c);
	uint numStructMembers = c.types.get!IrTypeStruct(structType).numMembers;
	IrIndex[] args = c.allocateTempArray!IrIndex(numStructMembers);
	scope(exit) c.freeTempArray(args);

	bool allZeroes = true;
	uint memberIndex;
	foreach(AstIndex member; s.declarations)
	{
		AstNode* memberVarNode = member.get_node(c);
		if (memberVarNode.astType != AstType.decl_var) continue;
		VariableDeclNode* memberVar = memberVarNode.as!VariableDeclNode(c);

		if (node.args.length > memberIndex) { // init from constructor argument
			IrIndex memberValue = eval_static_expr(node.args[memberIndex], c);
			args[memberIndex] = memberValue;
			if (!memberValue.isConstantZero) allZeroes = false;
		} else { // init with initializer from struct definition
			args[memberIndex] = memberVar.gen_default_value_var(c);
		}

		++memberIndex;
	}

	return c.constants.addAggrecateConstant(structType, args);
}

void force_callee_ir_gen(FunctionDeclNode* callee, AstIndex calleeIndex, CompilationContext* c)
{
	switch(callee.state) with(AstNodeState)
	{
		case name_register_self, name_register_nested, name_resolve, type_check, ir_gen:
			c.circular_dependency; assert(false);
		case parse_done:
			auto name_state = NameRegisterState(c);
			require_name_register_self(0, calleeIndex, name_state);
			c.throwOnErrors;
			goto case;
		case name_register_self_done:
			auto name_state = NameRegisterState(c);
			require_name_register(calleeIndex, name_state);
			c.throwOnErrors;
			goto case;
		case name_register_nested_done:
			require_name_resolve(calleeIndex, c);
			c.throwOnErrors;
			goto case;
		case name_resolve_done:
			require_type_check(calleeIndex, c);
			c.throwOnErrors;
			goto case;
		case type_check_done:
			IrGenState state = IrGenState(c);
			ir_gen_function(state, callee);
			goto case; // all requirement are done
		case ir_gen_done: break; // already has IR
		default: c.internal_error(callee.loc, "Node %s in %s state", callee.astType, callee.state);
	}
}

IrIndex eval_call(CallExprNode* node, AstIndex callee, CompilationContext* c)
{
	auto func = callee.get!FunctionDeclNode(c);

	force_callee_ir_gen(func, callee, c);

	if (func.state != AstNodeState.ir_gen_done)
		c.internal_error(node.loc,
			"Function's IR is not yet generated");

	auto signature = func.signature.get!FunctionSignatureNode(c);
	uint numArgs = node.args.length;
	uint numParams = signature.parameters.length;
	IrIndex[] args = c.allocateTempArray!IrIndex(numParams);
	scope(exit) c.freeTempArray(args);

	foreach (i, AstIndex arg; node.args)
	{
		args[i] = eval_static_expr(arg, c);
	}

	foreach(i; numArgs..numParams)
	{
		// use default argument value
		VariableDeclNode* param = c.getAst!VariableDeclNode(signature.parameters[i]);
		c.assertf(param.initializer.isDefined, param.loc, "Undefined default arg %s", c.idString(param.id));
		args[i] = param.gen_default_value_var(c);
	}

	if (func.isBuiltin)
		return eval_call_builtin(node.loc, callee, args, c);

	IrFunction* irData = c.getAst!IrFunction(func.backendData.irData);
	ubyte* vmBuffer = c.vmBuffer.bufPtr;

	IrIndex retType = c.types.getReturnType(irData.type, c);
	c.assertf(!retType.isTypeVoid, node.loc, "Cannot eval call to function returning void");

	uint retSize = c.types.typeSize(retType);
	IrVmSlotInfo returnMem = c.pushVmStack(retSize);

	IrVm vm = IrVm(c, irData);
	vm.pushFrame;
	foreach(uint index, IrVmSlotInfo slot; vm.parameters)
	{
		//writefln("param %s %s", index, slot);
		ubyte[] mem = vmBuffer[slot.offset..slot.offset+slot.length];
		constantToMem(mem, args[index], c);
	}
	vm.run(returnMem);

	ubyte[] returnSlice = vmBuffer[returnMem.offset..returnMem.offset+returnMem.length];
	IrIndex result = memToConstant(returnSlice, retType, c, signature.returnType.isSigned(c));
	vm.popFrame;
	c.popVmStack(returnMem);

	return result;
}

IrIndex eval_call_builtin(TokenIndex loc, AstIndex callee, IrIndex[] args, CompilationContext* c)
{
	switch (callee.storageIndex) {
		case CommonAstNodes.compile_error.storageIndex:
			IrIndex message = args[0];
			c.unrecoverable_error(loc, "%s", stringFromConstant(message, c));
			assert(false);
		default:
			c.internal_error("Unknown builtin function");
			assert(false);
	}
}
