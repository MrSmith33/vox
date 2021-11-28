/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.var;

import all;

enum VariableFlags : ushort {
	forceMemoryStorage = AstFlags.userFlag << 0,
	isParameter        = AstFlags.userFlag << 1,
	isVariadicParam    = AstFlags.userFlag << 2,
	isAddressTaken     = AstFlags.userFlag << 3,
}

@(AstType.decl_var)
struct VariableDeclNode
{
	mixin AstNodeData!(AstType.decl_var);
	AstIndex parentScope;
	AstIndex type;
	AstIndex initializer; // may be null, stores initializer for variables, default argument for parameters
	Identifier id;
	ushort scopeIndex; // stores index of parameter or index of member (for struct fields)
	// for local vars kind is variable or stackSlot, unique id of variable within a function
	// for global it is IrValueKind.global
	// nothing is generated for members
	ExprValue irValue;
	IrIndex defaultVal;
	bool forceMemoryStorage() { return cast(bool)(flags & VariableFlags.forceMemoryStorage); }
	bool isParameter() { return cast(bool)(flags & VariableFlags.isParameter); }
	bool isVariadicParam() { return cast(bool)(flags & VariableFlags.isVariadicParam); }
	bool isAddressTaken() { return cast(bool)(flags & VariableFlags.isAddressTaken); }

	IrIndex getIrIndex(CompilationContext* c)
	{
		c.assertf(isGlobal, "Must be used for globals only");
		return irValue.irValue;
	}
}

void print_var(VariableDeclNode* node, ref AstPrintState state)
{
	state.print(
		node.isParameter ? "PARAM " : "VAR ",
		node.type.printer(state.context), " ", state.context.idString(node.id));
	if (node.initializer) print_ast(node.initializer, state);
}

void post_clone_var(VariableDeclNode* node, ref CloneState state)
{
	state.fixScope(node.parentScope);
	state.fixAstIndex(node.type);
	state.fixAstIndex(node.initializer);
}

void name_register_self_var(AstIndex nodeIndex, VariableDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register_self;
	// registered during expansion in function signature
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
	scope (exit) node.state = AstNodeState.type_check_done;

	require_type_check(node.type, state);
	check_is_type(node.type, c);

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

	if (!node.isLocal)
		gen_init_value_var(node, c);
}

// only called for members, parameters and globals
IrIndex gen_init_value_var(VariableDeclNode* node, CompilationContext* c)
{
	if (node.defaultVal.isDefined) return node.defaultVal;
	c.assertf(node.isParameter || node.isMember || node.isGlobal, node.loc, "gen_init_value_var");
	if (node.initializer)
	{
		node.defaultVal = eval_static_expr(node.initializer, c);
	}
	else
	{
		node.defaultVal = node.type.get_type(c).gen_init_value(c);
	}
	return node.defaultVal;
}

void ir_gen_local_var(ref IrGenState gen, IrIndex curBlock, ref IrLabel nextStmt, VariableDeclNode* v)
{
	CompilationContext* c = gen.context;
	TypeNode* varType = c.getAstType(v.type).foldAliases(c);
	c.assertf(!v.isGlobal, v.loc, "Variable is global");

	IrIndex irType = varType.gen_ir_type(c);

	if (c.buildDebug)
		v.flags |= VariableFlags.forceMemoryStorage;

	bool needsStackSlot = v.forceMemoryStorage || v.isAddressTaken;

	if (needsStackSlot)
	{
		// allocate stack slot
		IrIndex slot = gen.builder.appendStackSlot(irType, c.types.typeSizeAndAlignment(irType), StackSlotKind.local);
		v.irValue = ExprValue(slot, ExprValueKind.ptr_to_data, IsLvalue.yes);
	}
	else
	{
		// allocate new variable
		v.irValue = ExprValue(gen.builder.newIrVarIndex(irType), ExprValueKind.value, IsLvalue.yes);
	}

	if (v.isParameter)
	{
		ExtraInstrArgs extra = {type : irType};
		InstrWithResult param = gen.builder.emitInstr!(IrOpcode.parameter)(gen.ir.entryBasicBlock, extra);
		gen.ir.get!IrInstr_parameter(param.instruction).index(gen.ir) = v.scopeIndex;
		v.irValue.store(gen, v.loc, curBlock, param.result);
	}
	else
	{
		// initialize variable by default or with user-specified value
		if (v.initializer)
		{
			IrLabel afterExpr = IrLabel(curBlock);
			ExprValue initValue = ir_gen_expr(gen, v.initializer, curBlock, afterExpr);
			curBlock = afterExpr.blockIndex;
			IrIndex val = initValue.rvalue(gen, v.loc, curBlock);
			v.irValue.store(gen, v.loc, curBlock, val);
		}
		else
		{
			IrIndex value = varType.gen_init_value(c);
			v.irValue.store(gen, v.loc, curBlock, value);
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
		IrIndex irType = varType.gen_ir_type(c);

		IrGlobal* global = c.globals.get(globalIndex);
		global.type = c.types.appendPtr(irType);

		SizeAndAlignment valueSizealign = c.types.typeSizeAndAlignment(irType);

		// symbol is created in parser
		ObjectSymbol* globalSym = c.objSymTab.getSymbol(global.objectSymIndex);
		globalSym.length = valueSizealign.size;
		globalSym.alignmentPower = valueSizealign.alignmentPower;

		IrIndex initializer = v.defaultVal;
		if (initializer.isConstantZero)
		{
			globalSym.flags |= ObjectSymbolFlags.isAllZero;
		}
		else
		{
			ubyte[] buffer = c.globals.allocateInitializer(valueSizealign.size);
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
			constantToMem(buffer, initializer, c, &onGlobal);
			globalSym.setInitializer(buffer);
		}

		return;
	}
}
