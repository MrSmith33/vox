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
	AstIndex initializer; // may be null, stores initializer for variables, default argument for parameters
	Identifier id;
	ushort scopeIndex; // stores index of parameter or index of member (for struct fields)
	ExprValue initValue;
	// for local vars kind is variable or stackSlot, unique id of variable within a function
	// for global it is IrValueKind.global
	// nothing is generated for members
	ExprValue irValue;
	IrIndex defaultVal;
	bool forceMemoryStorage() { return cast(bool)(flags & VariableFlags.forceMemoryStorage); }
	bool isParameter() { return cast(bool)(flags & VariableFlags.isParameter); }
	bool isAddressTaken() { return cast(bool)(flags & VariableFlags.isAddressTaken); }

	IrIndex getIrIndex(CompilationContext* c)
	{
		c.assertf(isGlobal, "Must be used for globals only");
		return irValue.irValue;
	}
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

	node.state = AstNodeState.type_check;
	require_type_check(node.type, state);
	TypeNode* type = node.type.get_type(c);

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

// only called for members, parameters and globals
IrIndex gen_default_value_var(VariableDeclNode* node, CompilationContext* c)
{
	if (node.defaultVal.isDefined) return node.defaultVal;
	c.assertf(node.isParameter || node.isMember || node.isGlobal, node.loc, "gen_default_value_var");
	if (node.initializer)
	{
		node.defaultVal = eval_static_expr(node.initializer, c);
	}
	else
	{
		node.defaultVal = node.type.get_type(c).gen_default_value(c);
	}
	return node.defaultVal;
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
			IrIndex value = varType.gen_default_value(c);
			store(gen, v.loc, curBlock, v.irValue, value);
		}
	}

	gen.builder.addJumpToLabel(curBlock, nextStmt);
}

void ir_gen_decl_var(ref IrGenState gen, VariableDeclNode* v)
{
	CompilationContext* c = gen.context;
	if (v.isGlobal)
	{
		IrIndex globalIndex = v.getIrIndex(c);

		TypeNode* varType = c.getAstType(v.type).foldAliases(c);
		IrIndex valueType = varType.gen_ir_type(c);

		IrGlobal* global = c.globals.get(globalIndex);
		global.type = c.types.appendPtr(valueType);

		uint valueSize = c.types.typeSize(valueType);

		// symbol is created in parser
		ObjectSymbol* globalSym = &c.objSymTab.getSymbol(global.objectSymIndex);
		globalSym.length = valueSize;
		globalSym.alignment = c.types.typeAlignment(valueType);

		IrIndex initializer = gen_default_value_var(v, c);
		if (initializer.isConstantZero)
		{
			globalSym.flags |= ObjectSymbolFlags.isAllZero;
		}
		else
		{
			ubyte[] buffer = c.globals.allocateInitializer(valueSize);
			void onGlobal(ubyte[] subbuffer, IrIndex index, CompilationContext* c)
			{
				c.assertf(index.isGlobal, "%s is not a constant", index);

				// initialize with 0
				// generate ObjectSymbolReference for linker to fix
				subbuffer[] = 0;

				assert(subbuffer.ptr >= buffer.ptr);
				size_t offset = subbuffer.ptr - buffer.ptr;
				assert(offset <= uint.max);

				IrGlobal* toGlobal = c.globals.get(index);
				assert(toGlobal.objectSymIndex.isDefined);

				ObjectSymbolReference r = {
					fromSymbol : global.objectSymIndex,
					referencedSymbol : toGlobal.objectSymIndex,
					refOffset : cast(uint)offset,
					extraOffset : 0,
					refKind : ObjectSymbolRefKind.absolute64,
				};
				c.objSymTab.addReference(r);
			}
			//if (!initializer.isConstantZero)
			//	writefln("%s %s", c.idString(v.id), IrIndexDump(initializer, c, IrInstructionSet.ir));
			constantToMem(buffer, initializer, c, &onGlobal);
			globalSym.setInitializer(buffer);
		}

		return;
	}
}
