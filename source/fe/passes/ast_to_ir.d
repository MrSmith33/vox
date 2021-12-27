/**
Copyright: Copyright (c) 2017-2020 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.passes.ast_to_ir;

import std.stdio;
import all;


void pass_ir_gen(ref CompilationContext ctx, CompilePassPerModule[] subPasses) {
	IrGenState state = {
		context : &ctx
	};
	foreach (ref SourceFileInfo file; ctx.files.data) {
		ir_gen_module_globals(state, file.mod);
	}
	foreach (ref SourceFileInfo file; ctx.files.data) {
		ir_gen_module_func(state, file.mod);
	}
}

enum MAX_GEP_INDICES = 255;
struct IrGenState
{
	CompilationContext* context;
	alias context this;

	IrBuilder builder;
	IrFunction* ir;
	FunctionDeclNode* fun;

	IrIndex[MAX_GEP_INDICES+2] gepBuf = void; // 2 is extra parameters to GEP instruction

	IrLabel* currentLoopHeader;
	IrLabel* currentLoopEnd;
}

void ir_gen_decl(ref IrGenState gen, AstIndex nodeIndex)
{
	CompilationContext* c = gen.context;
	AstNode* n = c.getAstNode(nodeIndex);
	switch(n.astType) with(AstType)
	{
		case decl_enum, decl_enum_member, decl_function, decl_struct, decl_import, decl_alias, decl_template, decl_static_assert: break;
		case decl_var: ir_gen_decl_var(c, cast(VariableDeclNode*)n); break;
		default:
			c.internal_error(n.loc, "ir_gen_decl %s in %s state", n.astType, n.state);
	}
}

void ir_gen_stmt(ref IrGenState gen, AstIndex astIndex, IrIndex curBlock, ref IrLabel nextStmt)
{
	CompilationContext* c = gen.context;
	AstNode* n = c.getAstNode(astIndex);
	switch(n.astType) with(AstType)
	{
		case stmt_block:       ir_gen_block   (gen, curBlock, nextStmt, cast(BlockStmtNode*)n); break;
		case stmt_if:          ir_gen_if      (gen, curBlock, nextStmt, cast(IfStmtNode*)n); break;
		case stmt_while:       ir_gen_while   (gen, curBlock, nextStmt, cast(WhileStmtNode*)n); break;
		case stmt_do_while:    ir_gen_do      (gen, curBlock, nextStmt, cast(DoWhileStmtNode*)n); break;
		case stmt_for:         ir_gen_for     (gen, curBlock, nextStmt, cast(ForStmtNode*)n); break;
		case stmt_switch:      ir_gen_switch  (gen, curBlock, nextStmt, cast(SwitchStmtNode*)n); break;
		case stmt_return:      ir_gen_return  (gen, curBlock, nextStmt, cast(ReturnStmtNode*)n); break;
		case stmt_break:       ir_gen_break   (gen, curBlock, nextStmt, cast(BreakStmtNode*)n); break;
		case stmt_continue:    ir_gen_continue(gen, curBlock, nextStmt, cast(ContinueStmtNode*)n); break;

		// expression statement, must have side effect
		case expr_call:        ir_gen_call(gen, curBlock, nextStmt, cast(CallExprNode*)n); break;
		case expr_bin_op:      ir_gen_expr_binary_op(gen, curBlock, nextStmt, cast(BinaryExprNode*)n); break;
		case expr_un_op:       ir_gen_expr_unary_op(gen, curBlock, nextStmt, cast(UnaryExprNode*)n); break;
		// should be catched in semantic check, since they have no side effect
		case expr_member:
		case expr_name_use:
		case expr_index:
		case expr_slice:
		case expr_type_conv:
		case literal_int:
		case literal_float:
		case literal_string:
		case literal_null:
		case literal_bool:
		case decl_template_param:
			c.internal_error(n.loc, "stmt %s in %s state", n.astType, n.state);

		// declaration statement
		case decl_alias:
		case decl_enum:
		case decl_enum_member:
		case decl_function:
		case decl_struct:
		case decl_template:
		case decl_static_assert:
		case decl_import:      gen.builder.addJumpToLabel(curBlock, nextStmt); break;
		case decl_var:         ir_gen_local_var(gen, curBlock, nextStmt, cast(VariableDeclNode*)n); break;

		default: c.internal_error("%s", n.astType);
	}
}

ExprValue ir_gen_expr(ref IrGenState gen, AstIndex astIndex, IrIndex curBlock, ref IrLabel nextStmt)
{
	CompilationContext* c = gen.context;
	AstNode* n = gen.getAstNode(astIndex);
	switch(n.astType) with(AstType)
	{
		case expr_name_use:    return ir_gen_name_use(gen, curBlock, nextStmt, cast(NameUseExprNode*)n);
		case expr_member:      return ir_gen_member(gen, curBlock, nextStmt, cast(MemberExprNode*)n);
		case expr_call:        return ir_gen_call(gen, curBlock, nextStmt, cast(CallExprNode*)n);
		case expr_index:       return ir_gen_index(gen, curBlock, nextStmt, cast(IndexExprNode*)n);
		case expr_slice:       return ir_gen_expr_slice(gen, curBlock, nextStmt, cast(SliceExprNode*)n);
		case expr_bin_op:      return ir_gen_expr_binary_op(gen, curBlock, nextStmt, cast(BinaryExprNode*)n);
		case expr_un_op:       return ir_gen_expr_unary_op(gen, curBlock, nextStmt, cast(UnaryExprNode*)n);
		case expr_type_conv:   return ir_gen_expr_type_conv(gen, curBlock, nextStmt, cast(TypeConvExprNode*)n);
		case literal_int: {
			IrIndex irValue = ir_gen_literal_int(gen.context, cast(IntLiteralExprNode*)n);
			gen.builder.addJumpToLabel(curBlock, nextStmt);
			return ExprValue(irValue);
		}
		case literal_float: {
			IrIndex irValue = ir_gen_literal_float(gen.context, cast(FloatLiteralExprNode*)n);
			gen.builder.addJumpToLabel(curBlock, nextStmt);
			return ExprValue(irValue);
		}
		case literal_string: {
			IrIndex irValue = ir_gen_literal_string(gen.context, cast(StringLiteralExprNode*)n);
			gen.builder.addJumpToLabel(curBlock, nextStmt);
			return ExprValue(irValue);
		}
		case literal_null: {
			IrIndex irValue = ir_gen_literal_null(gen.context, cast(NullLiteralExprNode*)n);
			gen.builder.addJumpToLabel(curBlock, nextStmt);
			return ExprValue(irValue);
		}
		case literal_bool: {
			IrIndex irValue = ir_gen_literal_bool(gen.context, cast(BoolLiteralExprNode*)n);
			gen.builder.addJumpToLabel(curBlock, nextStmt);
			return ExprValue(irValue);
		}
		case literal_special: {
			IrIndex irValue = ir_gen_literal_special(gen.context, cast(SpecialLiteralExprNode*)n);
			gen.builder.addJumpToLabel(curBlock, nextStmt);
			return ExprValue(irValue);
		}
		case decl_struct, type_basic, type_ptr, type_slice, type_static_array: {
			IrIndex irValue = gen.context.constants.add(makeIrType(IrBasicType.i32), astIndex.storageIndex);
			gen.builder.addJumpToLabel(curBlock, nextStmt);
			return ExprValue(irValue);
		}
		case decl_enum_member: {
			gen.builder.addJumpToLabel(curBlock, nextStmt);
			return ExprValue(n.as!EnumMemberDecl(c).gen_init_value_enum_member(c));
		}
		case decl_var: {
			auto v = n.as!VariableDeclNode(c);
			if (v.isGlobal)
			{
				ir_gen_decl_var(c, v);
			}
			c.assertf(v.irValue.irValue.isDefined, "Value is undefined");
			ExprValue result = v.irValue;
			gen.builder.addJumpToLabel(curBlock, nextStmt);
			return result;
		}
		case decl_function: {
			gen.builder.addJumpToLabel(curBlock, nextStmt);
			return ExprValue(n.as!FunctionDeclNode(c).getIrIndex(c));
		}
		default:
			c.internal_error(n.loc, "Expected expression, not %s", n.astType);
	}
}

void ir_gen_branch(ref IrGenState gen, AstIndex astIndex, IrIndex curBlock, ref IrLabel trueExit, ref IrLabel falseExit)
{
	CompilationContext* c = gen.context;
	AstNode* n = gen.getAstNode(astIndex);
	switch(n.astType) with(AstType)
	{
		case literal_int, literal_float, literal_string, expr_index, expr_slice: // TODO: expr_index may return bool
			gen.internal_error("Trying to branch directly on %s, must be wrapped in convertion to bool", n.astType);
		case expr_bin_op:    ir_gen_branch_binary_op   (gen, curBlock, trueExit, falseExit, cast(BinaryExprNode*)n); break;
		case expr_type_conv: ir_gen_branch_type_conv   (gen, curBlock, trueExit, falseExit, cast(TypeConvExprNode*)n); break;
		case expr_un_op:     ir_gen_branch_unary_op    (gen, curBlock, trueExit, falseExit, cast(UnaryExprNode*)n); break;
		case literal_bool:   ir_gen_branch_literal_bool(gen, curBlock, trueExit, falseExit, cast(BoolLiteralExprNode*)n); break;
		case expr_name_use, expr_call, expr_member:
			IrLabel afterExpr = IrLabel(curBlock);
			ExprValue lval = ir_gen_expr(gen, astIndex, curBlock, afterExpr);
			curBlock = afterExpr.blockIndex;
			IrIndex rval = lval.rvalue(gen, n.loc, curBlock);
			addUnaryBranch(gen, rval, curBlock, trueExit, falseExit);
			break;

		default: gen.internal_error(n.loc, "Expected expression, not %s", n.astType);
	}
}

void genBlock(ref IrGenState gen, AstNode* parent, ref AstNodes statements, IrIndex currentBlock, ref IrLabel nextStmt)
{
	foreach (i, AstIndex stmt; statements)
	{
		// if not the last statement of block
		if (i < statements.length - 1)
		{
			// nested statement will jump here at its end
			IrLabel afterStmt = IrLabel(currentBlock);

			// compile nested statement
			ir_gen_stmt(gen, stmt, currentBlock, afterStmt);

			if (afterStmt.numPredecessors == 0)
			{
				// Nested statement never returns here
				// Skip the rest of block statements
				return;
			}

			// If statement returned, get the new current block,
			// as it could have splitted the CFG and created a new block
			currentBlock = afterStmt.blockIndex;
			// Also seal it, since no other block can jump here
			gen.builder.sealBlock(currentBlock);
		}
		else // last statement
		{
			// let last statement exit straight to outer scope
			ir_gen_stmt(gen, stmt, currentBlock, nextStmt);

			// if statement hasn't returned here, let outer scope handle this
			// the body exit is handled by function decl code
		}
	}

	if (statements.length == 0)
		gen.builder.addJumpToLabel(currentBlock, nextStmt);
}

IrIndex makeBoolValue(ref IrGenState gen, ExpressionNode* n, IrIndex currentBlock, ref IrLabel nextStmt)
{
	CompilationContext* c = gen.context;
	IrBuilder* builder = &gen.builder;

	IrLabel trueLabel = IrLabel(currentBlock);
	IrLabel falseLabel = IrLabel(currentBlock);
	IrLabel nextLabel = IrLabel(currentBlock);
	IrIndex nextBlock;
	ir_gen_branch(gen, c.getAstNodeIndex(n), currentBlock, trueLabel, falseLabel);

	IrIndex value;
	IrIndex irType = n.type.gen_ir_type(c);

	if (trueLabel.numPredecessors != 0)
	{
		IrIndex trueBlock = trueLabel.blockIndex;
		builder.sealBlock(trueBlock);
		builder.addJumpToLabel(trueBlock, nextLabel);

		if (falseLabel.numPredecessors != 0) // both blocks exist
		{
			IrIndex falseBlock = falseLabel.blockIndex;
			builder.sealBlock(falseBlock);
			builder.addJumpToLabel(falseBlock, nextLabel);

			nextBlock = nextLabel.blockIndex;
			builder.sealBlock(nextBlock);

			IrIndex phiIndex = builder.addPhi(nextBlock, irType, IrIndex.init);
			IrIndex trueValue = c.constants.add(irType, 1);
			builder.addPhiArg(phiIndex, trueValue);
			IrIndex falseValue = c.constants.addZeroConstant(irType);
			builder.addPhiArg(phiIndex, falseValue);
			value = builder.ir.getPhi(phiIndex).result;
		}
		else // only true block exists
		{
			nextBlock = trueBlock;
			value = c.constants.add(irType, 1);
		}
	}
	else if (falseLabel.numPredecessors != 0) // only false block exists
	{
		nextBlock = falseLabel.blockIndex;
		builder.sealBlock(nextBlock);

		value = c.constants.addZeroConstant(irType);
	}

	builder.addJumpToLabel(nextBlock, nextStmt);

	return value;
}

void addUnaryBranch(ref IrGenState gen, IrIndex value, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit)
{
	CompilationContext* c = gen.context;
	if (value.isSimpleConstant)
	{
		long conValue = c.constants.get(value).i64;
		if (conValue != 0)
			gen.builder.addJumpToLabel(currentBlock, trueExit);
		else
			gen.builder.addJumpToLabel(currentBlock, falseExit);
		return;
	}

	IrArgSize argSize = sizeToIrArgSize(c.types.typeSize(gen.ir.getValueType(c, value)), c);
	gen.builder.addUnaryBranch(currentBlock, IrUnaryCondition.not_zero, argSize, value, trueExit, falseExit);
}


enum ExprValueKind : ubyte {
	// irValue is variable (isLvalue=true), constant or vreg (isLvalue=false) being used directly
	value,
	// it is data in source language, but prt to data in IR
	// for example 1st parameter of function that is passed as pointer to struct in RCX in win64 CC
	// irValue is pointer value (vreg, global, stack slot)
	ptr_to_data,
	// it is data in source language, but prt to ptr to data in IR
	// for example 5th parameter of function that is passed as pointer to struct on stack in win64 CC
	ptr_to_ptr_to_data,
	// irValue is variable (isLvalue=true), constant or vreg (isLvalue=false)
	// variable can contain aggregate value as well as pointer to aggregate
	// numIndices indicates number of gepBuf indices being used
	struct_sub_index,
}

enum IsLvalue : bool {
	no = false,
	yes = true,
}

/// Lvalue means that value is stored in global, stack slot or variable.
///
struct ExprValue
{
	// IR value
	IrIndex irValue;
	// Describes irValue
	private ExprValueKind kind = ExprValueKind.value;
	// true if can be assigned or address taken
	private IsLvalue isLvalue = IsLvalue.no;
	//
	private Array!IrIndex indices;
	// used as offset into aggregate values
	//IrIndex offset;

	ExprValue addrOf(ref IrGenState gen, TokenIndex loc, IrIndex currentBlock)
	{
		ExprValue res = this;
		res.isLvalue = IsLvalue.no;
		return res;
	}

	ExprValue deref(ref IrGenState gen, TokenIndex loc, IrIndex currentBlock)
	{
		switch (kind) {
			case ExprValueKind.value:
				return ExprValue(irValue, ExprValueKind.ptr_to_data, IsLvalue.yes);
			case ExprValueKind.ptr_to_data:
				return ExprValue(irValue, ExprValueKind.ptr_to_ptr_to_data, IsLvalue.yes);
			default:
				gen.context.internal_error(loc, "%s", kind);
		}
	}

	/// loads value from pointer. pointer must be an rvalue
	IrIndex load(ref IrGenState gen, TokenIndex loc, IrIndex currentBlock)
	{
		CompilationContext* c = gen.context;

		IrIndex source = this.irValue;

		switch (source.kind) with(IrValueKind)
		{
			case variable:
				// it's variable holding pointer
				source = gen.builder.readVariable(currentBlock, source);
				goto case;

			case stackSlot, global, virtualRegister:
				// those are already a pointer
				IrIndex resultType = c.types.getPointerBaseType(gen.ir.getValueType(c, source));
				ExtraInstrArgs extra = {type : resultType};
				if (resultType.isTypeStruct)
					return gen.builder.emitInstr!(IrOpcode.load_aggregate)(currentBlock, extra, source).result;
				else
				{
					extra.argSize = resultType.getTypeArgSize(c);
					return gen.builder.emitInstr!(IrOpcode.load)(currentBlock, extra, source).result;
				}

			default:
				c.internal_error(loc, "Cannot load from %s", source.kind);
		}
	}

	/// returns value as intended by frontend
	IrIndex rvalue(ref IrGenState gen, TokenIndex loc, IrIndex currentBlock)
	{
		CompilationContext* c = gen.context;
		ExprValue source = this;
		//writefln("getRvalue %s", source);
		switch (source.kind) with(ExprValueKind)
		{
			case value:
				switch (source.irValue.kind) with(IrValueKind)
				{
					case variable: return gen.builder.readVariable(currentBlock, source.irValue);
					default: return source.irValue;
				}
			case ptr_to_data:
				if (source.isLvalue) {
					return source.load(gen, loc, currentBlock);
				} else {
					if (source.irValue.isVariable) {
						return gen.builder.readVariable(currentBlock, source.irValue);
					}
					return source.irValue;
				}
			case ptr_to_ptr_to_data:
				IrIndex ptr = source.load(gen, loc, currentBlock);
				return ExprValue(ptr).load(gen, loc, currentBlock);
			case struct_sub_index:
				IrIndex aggr = source.irValue;
				if (aggr.isVariable) {
					aggr = gen.builder.readVariable(currentBlock, aggr);
				}

				IrIndex aggrType = gen.ir.getValueType(c, aggr);
				switch (aggrType.typeKind) {
					case IrTypeKind.pointer:
						IrIndex ZERO = c.constants.addZeroConstant(makeIrType(IrBasicType.i32));
						IrIndex ptr = buildGEP(gen, loc, currentBlock, aggr, ZERO, source.indices[]);
						if (source.isLvalue) {
							return ExprValue(ptr).load(gen, loc, currentBlock);
						} else {
							return ptr;
						}
					case IrTypeKind.array:
					case IrTypeKind.struct_t:
						IrIndex memberType = c.types.getAggregateMember(aggrType, c, source.indices[]).type;
						ExtraInstrArgs extra = { type : memberType };
						IrIndex[] args = gen.gepBuf[0..source.indices.length+1];
						args[0] = aggr;
						args[1..$] = source.indices[];
						return gen.builder.emitInstr!(IrOpcode.get_element)(currentBlock, extra, args).result;
					default: c.internal_error("%s", aggrType.typeKind);
				}
			default:
				c.internal_error(loc, "Cannot load from %s", source.kind);
		}
	}

	/// writes value to a pointer or variable
	void store(ref IrGenState gen, TokenIndex loc, IrIndex currentBlock, IrIndex value)
	{
		CompilationContext* c = gen.context;
		ExprValue destination = this;
		//writefln("store %s %s", destination, value);
		switch (destination.kind)
		{
			case ExprValueKind.ptr_to_ptr_to_data:
				destination.irValue = destination.load(gen, loc, currentBlock);
				goto case;

			case ExprValueKind.ptr_to_data:
				switch (destination.irValue.kind) with(IrValueKind)
				{
					case variable:
						IrIndex ptr = gen.builder.readVariable(currentBlock, destination.irValue);
						gen.builder.emitInstr!(IrOpcode.store)(currentBlock, ExtraInstrArgs(), ptr, value);
						return;
					case stackSlot, global, virtualRegister:
						ExtraInstrArgs extra;
						// destination must be a pointer
						gen.builder.emitInstr!(IrOpcode.store)(currentBlock, extra, destination.irValue, value);
						return;

					default: break;
				}
				break;

			case ExprValueKind.struct_sub_index:
				IrIndex aggr = gen.builder.readVariable(currentBlock, destination.irValue);
				IrIndex aggrType = gen.ir.getValueType(c, aggr);
				//writefln("Store %s", IrIndexDump(aggrType, c, gen.ir));
				switch (aggrType.typeKind) {
					case IrTypeKind.pointer:
						IrIndex ZERO = c.constants.addZeroConstant(makeIrType(IrBasicType.i32));
						IrIndex ptr = buildGEP(gen, loc, currentBlock, aggr, ZERO, destination.indices[]);
						gen.builder.emitInstr!(IrOpcode.store)(currentBlock, ExtraInstrArgs(), ptr, value);
						break;
					case IrTypeKind.struct_t:
					case IrTypeKind.array:
						IrIndex[] args = gen.gepBuf[0..destination.indices.length+2];
						args[0] = aggr;
						args[1] = value;
						args[2..$] = destination.indices[];
						ExtraInstrArgs extra = { type : aggrType };
						IrIndex res = gen.builder.emitInstr!(IrOpcode.insert_element)(currentBlock, extra, args).result;
						gen.builder.writeVariable(currentBlock, destination.irValue, res);
						break;
					default: c.internal_error("%s", aggrType.typeKind);
				}
				return;

			default:
				switch (destination.irValue.kind) with(IrValueKind)
				{
					case stackSlot, global, virtualRegister:
						ExtraInstrArgs extra;
						// destination must be a pointer
						gen.builder.emitInstr!(IrOpcode.store)(currentBlock, extra, destination.irValue, value);
						return;
					case variable:
						gen.builder.writeVariable(currentBlock, destination.irValue, value);
						return;
					default:
						break;
				}
		}
		c.internal_error(loc, "Cannot store into %s", destination.irValue.kind);
	}

	/// Returns reference to aggregate member
	/// Index must be a constant when accessing struct members
	ExprValue member(ref IrGenState gen, TokenIndex loc, IrIndex currentBlock, IrIndex index)
	{
		CompilationContext* c = gen.context;
		ExprValue aggr = this;
		//writefln("getAggregateMember %s %s", aggr, index);

		if (aggr.irValue.isVariable) {
			switch (aggr.kind) with(ExprValueKind)
			{
				case value:
					Array!IrIndex resIndices;
					resIndices.put(c.arrayArena, index);
					return ExprValue(aggr.irValue, ExprValueKind.struct_sub_index, IsLvalue.yes, resIndices);
				case struct_sub_index:
					aggr.indices.put(c.arrayArena, index);
					return aggr;
				default:
					aggr.irValue = gen.builder.readVariable(currentBlock, aggr.irValue);
					//writefln("  -1 %s", IrIndexDump(aggr.irValue, &gen.builder));
					break;
			}
		}

		switch (aggr.kind) with(ExprValueKind)
		{
			case ptr_to_ptr_to_data:
				aggr.irValue = aggr.load(gen, loc, currentBlock);
				break;
			default: break;
		}

		IrIndex aggrType = gen.ir.getValueType(c, aggr.irValue);
		//writefln("  -2 %s", IrIndexDump(aggrType, &gen.builder));

		switch (aggrType.typeKind) {
			case IrTypeKind.pointer:
				IrIndex ZERO = c.constants.addZeroConstant(makeIrType(IrBasicType.i32));
				return ExprValue(buildGEP(gen, loc, currentBlock, aggr.irValue, ZERO, index), ExprValueKind.ptr_to_data, IsLvalue.yes);
			case IrTypeKind.struct_t: {
				IrIndex aggrVal = aggr.irValue;

				switch (aggrVal.kind) with(IrValueKind) {
					case constantAggregate, constantZero:
						aggrVal = c.constants.getAggregateMember(aggrVal, index, c);
						assert(aggrVal.isDefined);
						return ExprValue(aggrVal);
					case virtualRegister:
						IrIndex memberType = c.types.getAggregateMember(aggrType, c, index).type;
						ExtraInstrArgs extra = { type : memberType };
						IrIndex[] args = gen.gepBuf[0..2];
						args[0] = aggrVal;
						args[1] = index;
						aggrVal = gen.builder.emitInstr!(IrOpcode.get_element)(currentBlock, extra, args).result;
						return ExprValue(aggrVal);
					default:
						c.internal_error("Cannot read struct member from %s", aggrVal.kind);
				}
			}
			default: c.internal_error("%s", aggrType.typeKind);
		}
	}
}

IrIndex buildGEPEx(ref IrGenState gen, TokenIndex loc, IrIndex currentBlock, ExprValue aggrPtrExpr, IrIndex ptrIndex, IrIndex[] indices...)
{
	CompilationContext* c = gen.context;
	IrIndex aggrPtr = aggrPtrExpr.irValue;
	if (aggrPtr.isVariable) {
		aggrPtr = gen.builder.readVariable(currentBlock, aggrPtr);
	}
	switch (aggrPtrExpr.kind) with(ExprValueKind)
	{
		case ptr_to_data: break;
		case struct_sub_index:
			IrIndex aggrType = gen.ir.getValueType(c, aggrPtr);
			switch (aggrType.typeKind) {
				case IrTypeKind.pointer:
					IrIndex ZERO = c.constants.addZeroConstant(makeIrType(IrBasicType.i32));
					aggrPtr = buildGEP(gen, loc, currentBlock, aggrPtr, ZERO, aggrPtrExpr.indices[]);
					break;

				default: c.internal_error("aggrType.typeKind == %s", aggrType.typeKind);
			}
			break;

		default: c.internal_error("aggrPtrExpr.kind == %s", aggrPtrExpr.kind);
	}
	return buildGEP(gen, loc, currentBlock, aggrPtr, ptrIndex, indices);
}

IrIndex buildGEP(ref IrGenState gen, TokenIndex loc, IrIndex currentBlock, IrIndex aggrPtr, IrIndex ptrIndex, IrIndex[] indices...)
{
	CompilationContext* c = gen.context;
	c.assertf(indices.length < MAX_GEP_INDICES,
		"too much indices for GEP instruction (%s) > %s",
		indices.length, MAX_GEP_INDICES);

	if (aggrPtr.isVariable) {
		aggrPtr = gen.builder.readVariable(currentBlock, aggrPtr);
	}

	IrIndex aggrPtrType = gen.ir.getValueType(c, aggrPtr);
	IrIndex aggrType = c.types.getPointerBaseType(aggrPtrType);

	foreach (i, IrIndex memberIndex; indices)
	{
		gen.gepBuf[i+2] = memberIndex;
		final switch(aggrType.typeKind)
		{
			case IrTypeKind.basic:
				c.internal_error(loc, "Cannot index basic type %s", aggrType.typeKind);

			case IrTypeKind.pointer:
				c.internal_error(loc, "Cannot index pointer with GEP instruction, use load first");

			case IrTypeKind.array:
				aggrType = c.types.getArrayElementType(aggrType);
				break;

			case IrTypeKind.struct_t:
				c.assertf(memberIndex.isSimpleConstant, loc,
					"Structs can only be indexed with constants, not with %s", memberIndex);
				aggrType = c.types.getAggregateMember(aggrType, c, memberIndex).type;
				break;

			case IrTypeKind.func_t:
				c.internal_error(loc, "Cannot index function type");
		}
	}

	if (indices.length == 0 && ptrIndex.isConstantZero)
		return aggrPtr; // skip no op GEP

	ExtraInstrArgs extra = { type : c.types.appendPtr(aggrType) };
	IrIndex[] args = gen.gepBuf[0..indices.length+2];
	args[0] = aggrPtr;
	args[1] = ptrIndex;
	IrIndex result = gen.builder.emitInstr!(IrOpcode.get_element_ptr)(currentBlock, extra, args).result;
	return result;
}
