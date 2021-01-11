/**
Copyright: Copyright (c) 2017-2020 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.ast_to_ir;

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

enum MAX_GEP_INDICIES = 255;
struct IrGenState
{
	CompilationContext* context;
	alias context this;

	IrBuilder builder;
	IrFunction* ir;
	FunctionDeclNode* fun;

	IrIndex[MAX_GEP_INDICIES+2] gepBuf = void; // 2 is extra parameters to GEP instruction

	IrLabel* currentLoopHeader;
	IrLabel* currentLoopEnd;
}

enum ExprValueKind : ubyte {
	// irValue is variable (isLvalue=true), constant or vreg (isLvalue=false) being used directly
	value,
	// it is data in source language, but prt to data in IR
	// for example 1st parameter of function that is passed as pointer to struct in RCX in win64 CC
	ptr_to_data,
	// it is data in source language, but prt to ptr to data in IR
	// for example 5th parameter of function that is passed as pointer to struct on stack in win64 CC
	ptr_to_ptr_to_data,
	// irValue is variable (isLvalue=true), constant or vreg (isLvalue=false)
	// numIndicies indicates number of gepBuf indicies being used
	struct_sub_index,
}

/// Lvalue means that value is stored in global, stack slot or variable.
///
struct ExprValue
{
	// IR value
	IrIndex irValue;
	// Describes irValue
	ExprValueKind kind = ExprValueKind.value;
	// true if can be assigned or address taken
	IsLvalue isLvalue = IsLvalue.no;
	// indicates number of gepBuf indicies being used
	ubyte numIndicies = 0;
}

enum IsLvalue : bool {
	no = false,
	yes = true,
}

void ir_gen_decl(ref IrGenState gen, AstIndex nodeIndex)
{
	CompilationContext* c = gen.context;
	AstNode* n = c.getAstNode(nodeIndex);
	switch(n.astType) with(AstType)
	{
		case decl_enum, decl_enum_member, decl_function, decl_struct, decl_import, decl_alias, decl_template, decl_static_assert: break;
		case decl_var: ir_gen_decl_var(gen, cast(VariableDeclNode*)n); break;
		default:
			c.internal_error(n.loc, "ir_gen_decl %s in %s state", n.astType, n.state);
			assert(false);
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
			assert(false);

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

		default: c.internal_error("%s", n.astType); assert(false);
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
		default:
			c.internal_error(n.loc, "Expected expression, not %s", n.astType);
			assert(false);
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
			break;
		case expr_bin_op:    ir_gen_branch_binary_op   (gen, curBlock, trueExit, falseExit, cast(BinaryExprNode*)n); break;
		case expr_type_conv: ir_gen_branch_type_conv   (gen, curBlock, trueExit, falseExit, cast(TypeConvExprNode*)n); break;
		case expr_un_op:     ir_gen_branch_unary_op    (gen, curBlock, trueExit, falseExit, cast(UnaryExprNode*)n); break;
		case literal_bool:   ir_gen_branch_literal_bool(gen, curBlock, trueExit, falseExit, cast(BoolLiteralExprNode*)n); break;
		case expr_name_use, expr_call, expr_member:
			IrLabel afterExpr = IrLabel(curBlock);
			ExprValue lval = ir_gen_expr(gen, astIndex, curBlock, afterExpr);
			curBlock = afterExpr.blockIndex;
			IrIndex rval = getRvalue(gen, n.loc, curBlock, lval);
			addUnaryBranch(gen, rval, curBlock, trueExit, falseExit);
			break;

		default: gen.internal_error(n.loc, "Expected expression, not %s", n.astType);
	}
}

/// destination must be pointer or variable
void store(ref IrGenState gen, TokenIndex loc, IrIndex currentBlock, ExprValue destination, IrIndex value)
{
	//writefln("store %s %s", destination, value);
	switch (destination.kind)
	{
		case ExprValueKind.ptr_to_ptr_to_data:
			destination.irValue = load(gen, loc, currentBlock, destination.irValue);
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
	gen.context.internal_error(loc, "Cannot store into %s", destination.irValue.kind);
	assert(false);
}

/// returns value as intended by frontend
IrIndex getRvalue(ref IrGenState gen, TokenIndex loc, IrIndex currentBlock, ExprValue source)
{
	CompilationContext* c = gen.context;
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
			return load(gen, loc, currentBlock, source.irValue);
		case ptr_to_ptr_to_data:
			IrIndex ptr = load(gen, loc, currentBlock, source.irValue);
			return load(gen, loc, currentBlock, ptr);
		default:
			c.internal_error(loc, "Cannot load from %s", source.kind);
			assert(false);
	}
}

/// loads value from pointer. pointer must be rvalue (getRvalue must be called before passing here)
IrIndex load(ref IrGenState gen, TokenIndex loc, IrIndex currentBlock, IrIndex source)
{
	CompilationContext* c = gen.context;

	if (source.isVariable) {
		source = gen.builder.readVariable(currentBlock, source);
	}

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
			assert(false);
	}
}

ExprValue getAggregateMember(ref IrGenState gen, TokenIndex loc, IrIndex currentBlock, ExprValue aggr, IrIndex[] indicies...)
{
	CompilationContext* c = gen.context;
	//writefln("getAggregateMember %s", aggr);
	if (aggr.irValue.isVariable) {
		aggr.irValue = gen.builder.readVariable(currentBlock, aggr.irValue);
		//writefln("  %s", IrIndexDump(aggr.irValue, &gen.builder));
	}

	switch (aggr.kind) with(ExprValueKind)
	{
		case ptr_to_ptr_to_data:
			aggr.irValue = load(gen, loc, currentBlock, aggr.irValue);
			break;
		default: break;
	}

	IrIndex aggrType = gen.ir.getValueType(c, aggr.irValue);
	//writefln("  %s", IrIndexDump(aggrType, &gen.builder));

	switch (aggrType.typeKind) {
		case IrTypeKind.pointer: return ExprValue(buildGEP(gen, loc, currentBlock, aggr.irValue, c.constants.ZERO, indicies), ExprValueKind.ptr_to_data, IsLvalue.yes);
		case IrTypeKind.struct_t: return ExprValue(getStructMember(gen, currentBlock, aggr.irValue, indicies));
		default: c.internal_error("%s", aggrType.typeKind); assert(false);
	}
}

// cannot assign into struct member when struct is a variable, because we return rvalue here
// we need to have 2 functions: one for read, one for write
IrIndex getStructMember(ref IrGenState gen, IrIndex currentBlock, IrIndex aggr, IrIndex[] indicies...)
{
	CompilationContext* c = gen.context;
	IrIndex aggrType = gen.ir.getValueType(c, aggr);

	foreach (i, IrIndex memberIndex; indicies)
	{
		c.assertf(memberIndex.isSimpleConstant,
			"Structs can only be indexed with constants, not with %s", memberIndex);
		gen.gepBuf[i+1] = memberIndex;
		c.assertf(aggrType.isTypeStruct, "Cannot index %s", IrIndexDump(aggrType, c, gen.ir));

		aggrType = c.types.getAggregateMember(aggrType, c, memberIndex).type;

		if (aggr.kind == IrValueKind.constantAggregate || aggr.kind == IrValueKind.constantZero)
		{
			uint memberIndexVal = c.constants.get(memberIndex).i32;
			aggr = c.constants.getAggregateMember(aggr, memberIndexVal);
		}
	}

	switch (aggr.kind) with(IrValueKind) {
		case constantAggregate, constantZero, global:
			assert(aggr.isDefined);
			break;
		case virtualRegister:
			ExtraInstrArgs extra = { type : aggrType };
			IrIndex[] args = gen.gepBuf[0..indicies.length+1];
			args[0] = aggr;
			aggr = gen.builder.emitInstr!(IrOpcode.get_element)(currentBlock, extra, args).result;
			break;
		default:
			c.internal_error("Cannot read struct member from %s", aggr.kind); assert(false);
	}

	return aggr;
}

IrIndex buildGEP(ref IrGenState gen, TokenIndex loc, IrIndex currentBlock, IrIndex aggrPtr, IrIndex ptrIndex, IrIndex[] indicies...)
{
	CompilationContext* c = gen.context;
	c.assertf(indicies.length < MAX_GEP_INDICIES,
		"too much indicies for GEP instruction (%s) > %s",
		indicies.length, MAX_GEP_INDICIES);

	if (aggrPtr.isVariable) {
		aggrPtr = gen.builder.readVariable(currentBlock, aggrPtr);
	}

	IrIndex aggrPtrType = gen.ir.getValueType(c, aggrPtr);
	IrIndex aggrType = c.types.getPointerBaseType(aggrPtrType);

	foreach (i, IrIndex memberIndex; indicies)
	{
		gen.gepBuf[i+2] = memberIndex;
		final switch(aggrType.typeKind)
		{
			case IrTypeKind.basic:
				c.internal_error(loc, "Cannot index basic type %s", aggrType.typeKind);
				break;

			case IrTypeKind.pointer:
				c.internal_error(loc, "Cannot index pointer with GEP instruction, use load first");
				break;

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
				break;
		}
	}

	if (indicies.length == 0 && ptrIndex.isConstantZero)
		return aggrPtr; // skip no op GEP

	ExtraInstrArgs extra = { type : c.types.appendPtr(aggrType) };
	IrIndex[] args = gen.gepBuf[0..indicies.length+2];
	args[0] = aggrPtr;
	args[1] = ptrIndex;
	IrIndex result = gen.builder.emitInstr!(IrOpcode.get_element_ptr)(currentBlock, extra, args).result;
	return result;
}

void genBlock(ref IrGenState gen, AstNode* parent, ref Array!AstIndex statements, IrIndex currentBlock, ref IrLabel nextStmt)
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

			IrIndex phiIndex = builder.addPhi(nextBlock, n.type.gen_ir_type(c), IrIndex.init);
			IrIndex trueValue = c.constants.add(1, IsSigned.no, n.type.typeArgSize(c));
			builder.addPhiArg(phiIndex, trueValue);
			IrIndex falseValue = c.constants.add(0, IsSigned.no, n.type.typeArgSize(c));
			builder.addPhiArg(phiIndex, falseValue);
			value = builder.ir.getPhi(phiIndex).result;
		}
		else // only true block exists
		{
			nextBlock = trueBlock;
			value = c.constants.add(1, IsSigned.no, n.type.typeArgSize(c));
		}
	}
	else if (falseLabel.numPredecessors != 0) // only false block exists
	{
		nextBlock = falseLabel.blockIndex;
		builder.sealBlock(nextBlock);

		value = c.constants.add(0, IsSigned.no, n.type.typeArgSize(c));
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
