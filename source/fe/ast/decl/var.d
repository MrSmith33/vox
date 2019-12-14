/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.var;

import all;

enum VariableFlags : ushort {
	forceMemoryStorage = AstFlags.userFlag << 0,
	isParameter        = AstFlags.userFlag << 1,
	isAddressTaken     = AstFlags.userFlag << 2,
}

@(AstType.decl_var)
struct VariableDeclNode
{
	mixin AstNodeData!(AstType.decl_var, AstFlags.isDeclaration | AstFlags.isStatement);
	AstIndex parentScope;
	AstIndex type;
	AstIndex initializer; // may be null
	Identifier id;
	ushort scopeIndex; // stores index of parameter or index of member (for struct fields)
	ExprValue initValue;
	ExprValue irValue; // kind is variable or stackSlot, unique id of variable within a function
	bool forceMemoryStorage() { return cast(bool)(flags & VariableFlags.forceMemoryStorage); }
	bool isParameter() { return cast(bool)(flags & VariableFlags.isParameter); }
	bool isAddressTaken() { return cast(bool)(flags & VariableFlags.isAddressTaken); }
}

void post_clone_var(VariableDeclNode* node, ref CloneState state)
{
	state.fixScope(node.parentScope);
	state.fixAstIndex(node.type);
	state.fixAstIndex(node.initializer);
}

void name_register_self_var(AstIndex nodeIndex, VariableDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_self;
	node.parentScope.insert_scope(node.id, nodeIndex, state.context);
	node.state = AstNodeState.name_register_self_done;
}

void name_register_nested_var(AstIndex nodeIndex, VariableDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_nested;
	if (node.initializer) require_name_register(node.initializer, state);
	node.state = AstNodeState.name_register_nested_done;
}

void name_resolve_var(VariableDeclNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.type, state);
	if (node.initializer) require_name_resolve(node.initializer, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_var(VariableDeclNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	TypeNode* type = node.type.get_type(c);
	node.state = AstNodeState.type_check;
	require_type_check(node.type, state);

	if (type.isOpaqueStruct(c)) {
		if (node.isParameter) {
			c.error(node.loc,
				"cannot declare parameter of opaque type `%s`",
				type.printer(c));
		} else {
			c.error(node.loc,
				"cannot declare variable `%s` of opaque type `%s`",
				c.idString(node.id),
				type.printer(c));
		}
	}

	if (node.initializer) {
		require_type_check(node.initializer, state);
		bool res = autoconvTo(node.initializer, node.type, c);
		if (!res) {
			c.error(node.loc,
				"cannot convert initializer of type `%s` to `%s`",
				node.initializer.get_node_type(c).printer(c), type.printer(c));
		}
	}

	if (!node.isParameter)
	{
		switch (type.astType) with(AstType)
		{
			case type_static_array, decl_struct, type_slice:
				node.flags |= VariableFlags.forceMemoryStorage;
				break;

			default: break;
		}
	}
	node.state = AstNodeState.type_check_done;
}

void ir_gen_local_var(ref IrGenState gen, IrIndex curBlock, ref IrLabel nextStmt, VariableDeclNode* v)
{
	CompilationContext* c = gen.context;
	TypeNode* varType = c.getAstType(v.type).foldAliases(c);
	c.assertf(!v.isGlobal, v.loc, "Variable is global");

	if (c.buildDebug)
		v.flags |= VariableFlags.forceMemoryStorage;

	// Allocate stack slot for parameter that is passed via stack
	bool isParamWithSlot = v.isParameter && gen.fun.backendData.getCallConv(c).isParamOnStack(v.scopeIndex);
	bool needsStackSlot = v.forceMemoryStorage || isParamWithSlot || v.isAddressTaken;
	//bool needsStackSlot = v.forceMemoryStorage || v.isAddressTaken;

	IrIndex initializer;
	if (needsStackSlot)
	{
		auto slotKind = v.isParameter ? StackSlotKind.parameter : StackSlotKind.local;
		IrIndex irType = varType.gen_ir_type(c);
		ExprValueKind valueKind = ExprValueKind.ptr_to_data;

		// pointer is pushed on the stack, so pointer to pointer to data
		if (v.isParameter && varType.isPassByPtr(c)) {
			irType = c.types.appendPtr(irType);
			valueKind = ExprValueKind.ptr_to_ptr_to_data;
		}

		// allocate stack slot
		IrIndex slot = gen.fun.backendData.stackLayout.addStackItem(c, irType, slotKind, v.scopeIndex);
		v.irValue = ExprValue(slot, valueKind, IsLvalue.yes);
	}
	else
	{
		IrIndex valueType = varType.gen_ir_type(c);
		if (v.isParameter)
		{
			// register parameter input
			IrIndex type = valueType;
			if (varType.isPassByPtr(c)) // value is already passed as a pointer
			{
				type = c.types.appendPtr(type);
				IrArgSize argSize = sizeToIrArgSize(c.types.typeSize(type), c);
				ExtraInstrArgs extra = {type : type, argSize : argSize};
				InstrWithResult param = gen.builder.emitInstr!(IrOpcode.parameter)(gen.ir.entryBasicBlock, extra);
				gen.ir.get!IrInstr_parameter(param.instruction).index(gen.ir) = v.scopeIndex;
				v.irValue = ExprValue(param.result, ExprValueKind.ptr_to_data, IsLvalue.yes);
			}
			else
			{
				IrArgSize argSize = sizeToIrArgSize(c.types.typeSize(type), c);

				// allocate new variable
				v.irValue = ExprValue(gen.builder.newIrVarIndex(type), ExprValueKind.value, IsLvalue.yes);

				ExtraInstrArgs extra = {type : type, argSize : argSize};
				InstrWithResult param = gen.builder.emitInstr!(IrOpcode.parameter)(gen.ir.entryBasicBlock, extra);
				gen.ir.get!IrInstr_parameter(param.instruction).index(gen.ir) = v.scopeIndex;

				store(gen, v.loc, curBlock, v.irValue, param.result);
			}
		}
		else
		{
			// allocate new variable
			v.irValue = ExprValue(gen.builder.newIrVarIndex(valueType), ExprValueKind.value, IsLvalue.yes);
		}
	}

	if (!v.isParameter)
	{
		// initialize variable by default or with user-specified value
		if (v.initializer)
		{
			IrLabel afterExpr = IrLabel(curBlock);
			ExprValue initValue = ir_gen_expr(gen, v.initializer, curBlock, afterExpr);
			curBlock = afterExpr.blockIndex;
			IrIndex val = getRvalue(gen, v.loc, curBlock, initValue);
			store(gen, v.loc, curBlock, v.irValue, val);
		}
		else
		{
			// TODO: default init structs, arrays, slices
			if (varType.as_basic || varType.as_ptr)
			{
				IrIndex value = c.constants.ZERO;
				store(gen, v.loc, curBlock, v.irValue, value);
			}
		}
	}

	gen.builder.addJumpToLabel(curBlock, nextStmt);
}

void ir_gen_decl_var(ref IrGenState gen, VariableDeclNode* v)
{
	CompilationContext* c = gen.context;
	if (v.isGlobal)
	{
		// TODO: initializers
		IrIndex globalIndex = c.globals.add();
		v.irValue = ExprValue(globalIndex, ExprValueKind.ptr_to_data, IsLvalue.yes);
		IrGlobal* global = &c.globals.get(globalIndex);
		global.flags |= IrGlobalFlags.isAllZero | IrGlobalFlags.isMutable;
		TypeNode* varType = c.getAstType(v.type).foldAliases(c);
		IrIndex valueType = varType.gen_ir_type(c);
		global.type = c.types.appendPtr(valueType);
		uint valueSize = c.types.typeSize(valueType);
		global.length = valueSize;
		global.moduleSymIndex = gen.mod.objectSymIndex;
		return;
	}
}
