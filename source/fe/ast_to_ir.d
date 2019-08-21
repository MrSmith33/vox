/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.ast_to_ir;

import std.stdio;
import all;


void pass_ir_gen(ref CompilationContext ctx, CompilePassPerModule[] subPasses) {
	auto astToIr = AstToIr(&ctx);
	foreach (ref SourceFileInfo file; ctx.files.data) {
		astToIr.visit(file.mod);
	}
}

//version = IrGenPrint;
//version = CfgGenPrint;

/// Converts AST to in-memory linear IR
struct AstToIr
{
	void visitType(AstIndex astIndex) {
		AstNode* node = context.getAstNode(astIndex);
		context.assertf(node.isType, node.loc, "Expected type, not %s", node.astType);
		switch(node.astType) with(AstType)
		{
			case type_basic: auto t = cast(BasicTypeNode*)node; visit(t); break;
			case type_ptr: auto t = cast(PtrTypeNode*)node; visit(t); break;
			case type_static_array: auto t = cast(StaticArrayTypeNode*)node; visit(t); break;
			case type_slice: auto t = cast(SliceTypeNode*)node; visit(t); break;
			case decl_struct: auto t = cast(StructDeclNode*)node; visit(t); break;

			default: context.unreachable(); assert(false);
		}
	}
	void visitExprValue(AstIndex astIndex, IrIndex currentBlock, ref IrLabel nextStmt) {
		AstNode* node = context.getAstNode(astIndex);
		context.assertf(node.isExpression, node.loc, "Expected expression, not %s", node.astType);
		switch(node.astType) with(AstType)
		{
			case expr_name_use:
				visitExprValue(cast(NameUseExprNode*)node, currentBlock, nextStmt); break;
			case expr_member: visitExprValue(cast(MemberExprNode*)node, currentBlock, nextStmt); break;
			case expr_call: visitExprValue(cast(CallExprNode*)node, currentBlock, nextStmt); break;
			case expr_index: visitExprValue(cast(IndexExprNode*)node, currentBlock, nextStmt); break;
			case expr_bin_op: visitExprValue(cast(BinaryExprNode*)node, currentBlock, nextStmt); break;
			case expr_un_op: visitExprValue(cast(UnaryExprNode*)node, currentBlock, nextStmt); break;
			case expr_type_conv: visitExprValue(cast(TypeConvExprNode*)node, currentBlock, nextStmt); break;
			case literal_int: visitExprValue(cast(IntLiteralExprNode*)node, currentBlock, nextStmt); break;
			case literal_string: visitExprValue(cast(StringLiteralExprNode*)node, currentBlock, nextStmt); break;
			case literal_null: visitExprValue(cast(NullLiteralExprNode*)node, currentBlock, nextStmt); break;
			case literal_bool: visitExprValue(cast(BoolLiteralExprNode*)node, currentBlock, nextStmt); break;
			case decl_enum_member: visitExprValue(cast(EnumMemberDecl*)node, currentBlock, nextStmt); break;
			default: context.internal_error("%s", node.astType); assert(false);
		}
	}
	void visitExprBranch(AstIndex astIndex, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit) {
		AstNode* node = context.getAstNode(astIndex);
		context.assertf(node.isExpression, node.loc, "Expected expression, not %s", node.astType);
		switch(node.astType) with(AstType)
		{
			case literal_int, literal_string, expr_index: // TODO: expr_index may return bool
				context.internal_error("Trying to branch directly on %s, must be wrapped in convertion to bool", node.astType);
				break;
			case expr_name_use: visitExprBranch(cast(NameUseExprNode*)node, currentBlock, trueExit, falseExit); break;
			case expr_bin_op: visitExprBranch(cast(BinaryExprNode*)node, currentBlock, trueExit, falseExit); break;
			case expr_type_conv: visitExprBranch(cast(TypeConvExprNode*)node, currentBlock, trueExit, falseExit); break;
			case expr_un_op: visitExprBranch(cast(UnaryExprNode*)node, currentBlock, trueExit, falseExit); break;
			case expr_call: visitExprBranch(cast(CallExprNode*)node, currentBlock, trueExit, falseExit); break;
			case literal_bool: visitExprBranch(cast(BoolLiteralExprNode*)node, currentBlock, trueExit, falseExit); break;
			case expr_member: visitExprBranch(cast(MemberExprNode*)node, currentBlock, trueExit, falseExit); break;

			default: context.internal_error("%s", node.astType); assert(false);
		}
	}
	void visitDecl(AstIndex astIndex, IrIndex currentBlock, ref IrLabel nextStmt) {
		AstNode* node = context.getAstNode(astIndex);
		context.assertf(node.isDeclaration, node.loc, "Expected declaration, not %s", node.astType);
		switch(node.astType) with(AstType)
		{
			case decl_function, decl_struct, decl_import:
				// skip
				if (currentBlock.isDefined)
					builder.addJumpToLabel(currentBlock, nextStmt);
				break;

			case decl_var: auto v = cast(VariableDeclNode*)node; visit(v, currentBlock, nextStmt); break;

			default: context.unreachable(); assert(false);
		}
	}
	void visitStmt(AstIndex astIndex, IrIndex currentBlock, ref IrLabel nextStmt) {
		AstNode* node = context.getAstNode(astIndex);
		if (node.isDeclaration) {
			visitDecl(astIndex, currentBlock, nextStmt);
			return;
		} else if (node.isExpression) {
			visitExprValue(astIndex, currentBlock, nextStmt);
			return;
		}

		context.assertf(node.isStatement, node.loc, "Expected statement, not %s", node.astType);
		switch(node.astType) with(AstType)
		{
			case stmt_block: visit(cast(BlockStmtNode*)node, currentBlock, nextStmt); break;
			case stmt_if: visit(cast(IfStmtNode*)node, currentBlock, nextStmt); break;
			case stmt_while: visit(cast(WhileStmtNode*)node, currentBlock, nextStmt); break;
			case stmt_do_while: visit(cast(DoWhileStmtNode*)node, currentBlock, nextStmt); break;
			case stmt_for: visit(cast(ForStmtNode*)node, currentBlock, nextStmt); break;
			case stmt_return: visit(cast(ReturnStmtNode*)node, currentBlock, nextStmt); break;
			case stmt_break: visit(cast(BreakStmtNode*)node, currentBlock, nextStmt); break;
			case stmt_continue: visit(cast(ContinueStmtNode*)node, currentBlock, nextStmt); break;

			default: context.unreachable(); assert(false);
		}
	}

	CompilationContext* context;
	ModuleDeclNode* mod;

	IrBuilder builder;
	IrFunction* ir;
	FunctionDeclNode* fun;

	enum MAX_GEP_INDICIES = 255;
	IrIndex[MAX_GEP_INDICIES+2] gepBuf = void; // 2 is extra parameters to GEP instruction

	IrLabel* currentLoopHeader;
	IrLabel* currentLoopEnd;

	void visit(ModuleDeclNode* m)
	{
		mod = m;

		version(IrGenPrint) writeln("[IR GEN] module begin");

		foreach (AstIndex decl; m.declarations)
		{
			AstNode* declNode = context.getAstNode(decl);
			declNode.flags |= AstFlags.isGlobal;

			if (declNode.astType == AstType.decl_var)
			{
				IrLabel label;
				visitDecl(decl, IrIndex(), label);
				context.assertf(label.numPredecessors == 0, "Global var must not gen code");
			}
		}

		foreach (AstIndex decl; m.functions)
		{
			FunctionDeclNode* func = context.getAst!FunctionDeclNode(decl);
			visit(func);

			IrFunction* irData = context.getAst!IrFunction(func.backendData.irData);

			// can be null if function is external
			if (irData)
			{
				m.irModule.addFunction(*context, irData);
				if (context.validateIr) validateIrFunction(*context, *irData);
				if (context.printIr && context.printDumpOf(func)) dumpFunction(*irData, *context);
			}
		}

		version(IrGenPrint) writeln("[IR GEN] module end");
	}

	void visit(FunctionDeclNode* f)
	{
		version(IrGenPrint) writefln("[IR GEN] function (%s) begin", f.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] function (%s) end", f.loc);

		// skip external functions, they don't have a body
		if (f.isExternal) return;

		fun = f;
		scope(exit) fun = null;

		// create new function
		AstIndex irIndex = context.appendAst!IrFunction;
		f.backendData.irData = irIndex;
		ir = context.getAst!IrFunction(irIndex);
		ir.backendData = &f.backendData;

		ir.backendData.returnType = f.returnType.gen_ir_type(context);
		ir.type = f.gen_ir_type_func(context);
		ir.instructionSet = IrInstructionSet.ir;

		version(IrGenPrint) writefln("[IR GEN] function 1");
		builder.begin(ir, context);

		version(IrGenPrint) writefln("[IR GEN] function parameters");
		foreach (AstIndex param; f.parameters)
		{
			IrLabel dummy;
			visitDecl(param, ir.entryBasicBlock, dummy);
		}

		builder.addJump(ir.entryBasicBlock);

		IrIndex body_block = builder.addBasicBlock();
		builder.addBlockTarget(ir.entryBasicBlock, body_block);
		builder.sealBlock(body_block);

		// label at the end of body
		IrLabel bodyExitLabel = IrLabel(body_block);

		version(IrGenPrint) writefln("[IR GEN] function body");
		// compile body
		visitStmt(f.block_stmt, body_block, bodyExitLabel);

		IrIndex currentBlock = bodyExitLabel.blockIndex;
		// In case new block was created, no new predecessors will be added
		builder.sealBlock(currentBlock);

		version(IrGenPrint) writefln("[IR GEN] function return");
		if (!context.types.isVoid(ir.backendData.returnType))
		{
			// currentBlock must be finished with retVal
			if (!ir.getBlock(currentBlock).isFinished)
			{
				context.error(f.loc,
					"function `%s` has no return statement, but is expected to return a value of type %s",
					context.idString(f.id), f.returnType.typeName(context));
			}
		}
		else
		{
			// currentBlock must be finished with ret or, not finished
			if (!ir.getBlock(currentBlock).isFinished)
			{
				builder.addReturn(currentBlock);
			}
		}

		version(IrGenPrint) writefln("[IR GEN] function seal exit");

		// all blocks with return (exit's predecessors) already connected, seal exit block
		builder.sealBlock(ir.exitBasicBlock);
	}

	/// destination must be pointer or variable
	void store(IrIndex currentBlock, IrIndex destination, IrIndex value)
	{
		version(IrGenPrint) writefln("[IR GEN] store %s to '%s'", value, destination);

		switch (destination.kind) with(IrValueKind)
		{
			case stackSlot, global, virtualRegister:
				ExtraInstrArgs extra;
				// destination must be a pointer
				builder.emitInstr!IrInstr_store(currentBlock, extra, destination, value);
				break;
			case variable:
				builder.writeVariable(currentBlock, destination, value);
				break;
			default:
				context.internal_error("Cannot store into %s", destination.kind);
				assert(false);
		}
	}

	/// source must be pointer or variable
	IrIndex load(IrIndex currentBlock, IrIndex source)
	{
		version(IrGenPrint) writefln("[IR GEN] load from '%s'", source);
		switch (source.kind) with(IrValueKind)
		{
			case stackSlot, global, virtualRegister:
				IrIndex resultType = context.types.getPointerBaseType(ir.getValueType(*context, source));
				ExtraInstrArgs extra = {type : resultType};
				if (resultType.isTypeStruct)
					return builder.emitInstr!IrInstr_load_aggregate(currentBlock, extra, source).result;
				else
					return builder.emitInstr!IrInstr_load(currentBlock, extra, source).result;
			case variable:
				return builder.readVariable(currentBlock, source);
			case constant:
				return source;
			default:
				context.internal_error("Cannot load from %s", source.kind);
				assert(false);
		}
	}

	static struct LRValue {
		IrIndex value;
		bool isLvalue;
	}

	LRValue getMember(IrIndex currentBlock, IrIndex aggr, IrIndex[] indicies...)
	{
		if (aggr.isVariable) {
			aggr = builder.readVariable(currentBlock, aggr);
		}

		IrIndex aggrType = ir.getValueType(*context, aggr);

		switch (aggrType.typeKind) {
			case IrTypeKind.pointer: return LRValue(buildGEP(currentBlock, aggr, context.constants.ZERO, indicies), true);
			case IrTypeKind.struct_t: return LRValue(getStructMember(currentBlock, aggr, indicies), false);
			default: context.internal_error("%s", aggrType.typeKind); assert(false);
		}
	}

	// cannot assign into struct member when struct is a variable, because we return rvalue here
	// we need to have 2 functions: one for read, one for write
	IrIndex getStructMember(IrIndex currentBlock, IrIndex aggr, IrIndex[] indicies...)
	{
		IrIndex aggrType = ir.getValueType(*context, aggr);
		context.assertf(aggr.isConstantAggregate, "%s", aggr.kind);
		foreach (i, IrIndex memberIndex; indicies)
		{
			switch(aggrType.typeKind)
			{
				case IrTypeKind.struct_t:
					context.assertf(memberIndex.isConstant, "Structs can only be indexed with constants, not with %s", memberIndex);
					uint memberIndexVal = context.constants.get(memberIndex).i32;
					aggrType = context.types.getStructMemberType(aggrType, memberIndexVal, *context);
					aggr = context.constants.getAggregateMember(aggr, memberIndexVal);
					break;

				default: context.internal_error("Cannot index %s", IrIndexDump(aggrType, *context, *ir)); assert(false);
			}
		}
		assert(aggr.isDefined);
		return aggr;
	}

	IrIndex buildGEP(IrIndex currentBlock, IrIndex aggrPtr, IrIndex ptrIndex, IrIndex[] indicies...)
	{
		context.assertf(indicies.length < MAX_GEP_INDICIES,
			"too much indicies for GEP instruction (%s) > %s",
			indicies.length, MAX_GEP_INDICIES);

		if (aggrPtr.isVariable) {
			aggrPtr = builder.readVariable(currentBlock, aggrPtr);
		}

		IrIndex aggrPtrType = ir.getValueType(*context, aggrPtr);
		IrIndex aggrType = context.types.getPointerBaseType(aggrPtrType);

		foreach (i, IrIndex memberIndex; indicies)
		{
			gepBuf[i+2] = memberIndex;
			final switch(aggrType.typeKind)
			{
				case IrTypeKind.basic:
					context.internal_error("Cannot index basic type %s", aggrType.typeKind);
					break;

				case IrTypeKind.pointer:
					context.internal_error("Cannot index pointer with GEP instruction, use load first");
					break;

				case IrTypeKind.array:
					aggrType = context.types.getArrayElementType(aggrType);
					break;

				case IrTypeKind.struct_t:
					context.assertf(memberIndex.isConstant, "Structs can only be indexed with constants, not with %s", memberIndex);
					uint memberIndexVal = context.constants.get(memberIndex).i32;
					aggrType = context.types.getStructMemberType(aggrType, memberIndexVal, *context);
					break;

				case IrTypeKind.func_t:
					context.internal_error("Cannot index function type");
					break;
			}
		}

		ExtraInstrArgs extra = { type : context.types.appendPtr(aggrType) };
		IrIndex[] args = gepBuf[0..indicies.length+2];
		args[0] = aggrPtr;
		args[1] = ptrIndex;
		IrIndex result = builder.emitInstr!IrInstr_get_element_ptr(currentBlock, extra, args).result;
		return result;
	}

	void visit(VariableDeclNode* v, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(CfgGenPrint) writefln("[CFG GEN] beg VAR_DECL cur %s next %s", currentBlock, nextStmt);
		version(CfgGenPrint) scope(success) writefln("[CFG GEN] end VAR_DECL cur %s next %s", currentBlock, nextStmt);
		version(IrGenPrint) writefln("[IR GEN] Var decl (%s) begin %s", v.loc, v.strId(context));
		version(IrGenPrint) scope(success) writefln("[IR GEN] Var decl (%s) end %s", v.loc, v.strId(context));

		TypeNode* varType = context.getAstType(v.type).foldAliases(context);

		if (v.isGlobal)
		{
			// TODO: initializers
			v.irValue = context.globals.add();
			IrGlobal* global = &context.globals.get(v.irValue);
			global.flags |= IrGlobalFlags.isAllZero | IrGlobalFlags.isMutable;
			IrIndex valueType = varType.gen_ir_type(context);
			global.type = context.types.appendPtr(valueType);
			uint valueSize = context.types.typeSize(valueType);
			global.length = valueSize;
			global.moduleSymIndex = mod.objectSymIndex;
			return;
		}

		if (context.buildDebug)
			v.flags |= VariableFlags.forceMemoryStorage;

		// Allocate stack slot for parameter that is passed via stack
		bool isParamWithSlot = v.isParameter && ir.backendData.callingConvention.isParamOnStack(v.scopeIndex);
		bool needsStackSlot = v.forceMemoryStorage || isParamWithSlot || v.isAddressTaken;

		IrIndex initializer;
		if (needsStackSlot)
		{
			auto slotKind = v.isParameter ? StackSlotKind.parameter : StackSlotKind.local;
			// allocate stack slot
			v.irValue = fun.backendData.stackLayout.addStackItem(context, varType.gen_ir_type(context), slotKind, v.scopeIndex);
		}
		else
		{
			if (v.isParameter)
			{
				// register parameter input
				IrIndex valueType = varType.gen_ir_type(context);
				IrIndex type = valueType;
				if (varType.isPassByPtr) // value is already passed as a pointer
				{
					type = context.types.appendPtr(type);
					IrArgSize argSize = sizeToIrArgSize(context.types.typeSize(type), context);
					ExtraInstrArgs extra = {type : type, argSize : argSize};
					InstrWithResult param = builder.emitInstr!IrInstr_parameter(ir.entryBasicBlock, extra);
					ir.get!IrInstr_parameter(param.instruction).index = v.scopeIndex;
					v.irValue = param.result;
				}
				else
				{
					IrArgSize argSize = sizeToIrArgSize(context.types.typeSize(type), context);

					// allocate new variable
					v.irValue = builder.newIrVarIndex(type);

					ExtraInstrArgs extra = {type : type, argSize : argSize};
					InstrWithResult param = builder.emitInstr!IrInstr_parameter(ir.entryBasicBlock, extra);
					ir.get!IrInstr_parameter(param.instruction).index = v.scopeIndex;

					store(currentBlock, v.irValue, param.result);
				}
			}
			else
			{
				// allocate new variable
				v.irValue = builder.newIrVarIndex(varType.gen_ir_type(context));
			}
		}

		if (!v.isParameter)
		{
			// initialize variable by default or with user-specified value
			if (v.initializer)
			{
				IrLabel afterExpr = IrLabel(currentBlock);
				visitExprValue(v.initializer, currentBlock, afterExpr);
				currentBlock = afterExpr.blockIndex;
				store(currentBlock, v.irValue, context.getAstExpr(v.initializer).irValue);
			}
			else
			{
				// TODO: default init structs, arrays, slices
				if (varType.as_basic || varType.as_ptr)
				{
					IrIndex value = context.constants.ZERO;
					store(currentBlock, v.irValue, value);
				}
			}
		}

		builder.addJumpToLabel(currentBlock, nextStmt);
	}
	void visit(StructDeclNode* s) {}
	void visit(BlockStmtNode* b, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		genBlock(b.as_node, b.statements, currentBlock, nextStmt);
	}
	void genBlock(AstNode* parent, ref Array!AstIndex statements, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(CfgGenPrint) writefln("[CFG GEN] beg BLOCK cur %s next %s", currentBlock, nextStmt);
		version(CfgGenPrint) scope(success) writefln("[CFG GEN] end BLOCK cur %s next %s", currentBlock, nextStmt);
		version(IrGenPrint) writefln("[IR GEN] block (%s) begin", parent.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] block (%s) end", parent.loc);
		foreach (i, AstIndex stmt; statements)
		{
			version(IrGenPrint) writefln("[IR GEN]   stmt %s/%s", i+1, statements.length);
			// if not the last statement of block
			if (i < statements.length - 1)
			{
				// nested statement will jump here at its end
				IrLabel afterStmt = IrLabel(currentBlock);
				version(CfgGenPrint) writefln("[CFG GEN] beg cur %s afterStmt %s", currentBlock, afterStmt);

				// compile nested statement
				visitStmt(stmt, currentBlock, afterStmt);

				if (afterStmt.numPredecessors == 0)
				{
					version(IrGenPrint) writefln("[IR GEN]   no returns from stmt %s/%s, skipping the rest", i+1, statements.length);
					// Nested statement never returns here
					// Skip the rest of block statements
					return;
				}

				// If statement returned, get the new current block,
				// as it could have splitted the CFG and created a new block
				currentBlock = afterStmt.blockIndex;
				// Also seal it, since no other block can jump here
				builder.sealBlock(currentBlock);
				version(CfgGenPrint) writefln("[CFG GEN] end cur %s afterStmt %s", currentBlock, afterStmt);
			}
			else // last statement
			{
				version(CfgGenPrint) writefln("[CFG GEN] beg last cur %s next %s", currentBlock, nextStmt);
				// let last statement exit straight to outer scope
				visitStmt(stmt, currentBlock, nextStmt);
				version(CfgGenPrint) writefln("[CFG GEN] end last cur %s next %s", currentBlock, nextStmt);

				// if statement hasn't returned here, let outer scope handle this
				// the body exit is handled by function decl code
			}
		}

		if (statements.length == 0)
			builder.addJumpToLabel(currentBlock, nextStmt);
	}

	void visit(IfStmtNode* i, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(CfgGenPrint) writefln("[CFG GEN] beg IF cur %s next %s", currentBlock, nextStmt);
		version(CfgGenPrint) scope(success) writefln("[CFG GEN] end IF cur %s next %s", currentBlock, nextStmt);
		if (i.elseStatement) // if then else
		{
			version(IrGenPrint) writefln("[IR GEN] if-else (%s) begin", i.loc);
			version(IrGenPrint) scope(success) writefln("[IR GEN] if-else (%s) end", i.loc);
			IrLabel trueLabel = IrLabel(currentBlock);
			IrLabel falseLabel = IrLabel(currentBlock);
			version(CfgGenPrint) writefln("[CFG GEN] before cond expr: true %s false %s", trueLabel, falseLabel);
			visitExprBranch(i.condition, currentBlock, trueLabel, falseLabel);
			version(CfgGenPrint) writefln("[CFG GEN] after cond expr: true %s false %s", trueLabel, falseLabel);

			if (trueLabel.numPredecessors != 0)
			{
				IrIndex thenBlock = trueLabel.blockIndex;
				builder.sealBlock(thenBlock);
				visitStmt(i.thenStatement, thenBlock, nextStmt);
			}
			else
				version(IrGenPrint) writeln("[IR GEN]   skip then stmt. Condition didn't jump here");
			version(CfgGenPrint) writefln("[CFG GEN] after true stmt: true %s false %s", trueLabel, falseLabel);

			if (falseLabel.numPredecessors != 0)
			{
				IrIndex elseBlock = falseLabel.blockIndex;
				builder.sealBlock(elseBlock);
				visitStmt(i.elseStatement, elseBlock, nextStmt);
			}
			else
				version(IrGenPrint) writeln("[IR GEN]   skip else stmt. Condition didn't jump here");
			version(CfgGenPrint) writefln("[CFG GEN] after false stmt: true %s false %s", trueLabel, falseLabel);
		}
		else // if then
		{
			version(IrGenPrint) writefln("[IR GEN] if (%s) begin", i.loc);
			version(IrGenPrint) scope(success) writefln("[IR GEN] if (%s) end", i.loc);
			IrLabel trueLabel = IrLabel(currentBlock);
			version(CfgGenPrint) writefln("[CFG GEN] before cond expr: true %s next %s", trueLabel, nextStmt);
			visitExprBranch(i.condition, currentBlock, trueLabel, nextStmt);
			version(CfgGenPrint) writefln("[CFG GEN] after cond expr: true %s next %s", trueLabel, nextStmt);

			if (trueLabel.numPredecessors != 0)
			{
				IrIndex thenBlock = trueLabel.blockIndex;
				builder.sealBlock(thenBlock);
				visitStmt(i.thenStatement, thenBlock, nextStmt);
			}
			else
				version(IrGenPrint) writeln("[IR GEN]   skip then stmt. Condition didn't jump here");
			version(CfgGenPrint) writefln("[CFG GEN] after true stmt: true %s next %s", trueLabel, nextStmt);
		}
	}
	void visit(WhileStmtNode* n, IrIndex currentBlock, ref IrLabel nextStmt) {
		version(CfgGenPrint) writefln("[CFG GEN] beg WHILE cur %s next %s", currentBlock, nextStmt);
		version(CfgGenPrint) scope(success) writefln("[CFG GEN] end WHILE cur %s next %s", currentBlock, nextStmt);
		//writefln("loop cur 1 %s", currentBlock);
		// loop header
		IrLabel loopHeaderLabel = IrLabel(currentBlock);

		IrLabel* prevLoopHeader = currentLoopHeader; // save continue label
		currentLoopHeader = &loopHeaderLabel;
		scope(exit) currentLoopHeader = prevLoopHeader; // restore continue label

		IrLabel* prevLoopEnd = currentLoopEnd; // save break label
		currentLoopEnd = &nextStmt;
		scope(exit) currentLoopEnd = prevLoopEnd; // restore break label

		builder.addJumpToLabel(currentBlock, loopHeaderLabel);

		// we need loop header in a separate block because it will
		// have 2 predecessors: currentBlock and loop body
		builder.forceAllocLabelBlock(loopHeaderLabel);
		IrIndex loopHeaderBlock = loopHeaderLabel.blockIndex;
		currentBlock = loopHeaderBlock;

		//writefln("loop head %s", loopHeaderBlock);
		ir.getBlock(loopHeaderBlock).isLoopHeader = true;

		IrLabel bodyLabel = IrLabel(currentBlock);
		//writefln("loop body %s, next %s", bodyLabel.blockIndex, nextStmt.blockIndex);

		// will force allocate body block
		visitExprBranch(n.condition, loopHeaderBlock, bodyLabel, nextStmt);

		// body
		if (bodyLabel.numPredecessors > 0)
		{
			currentBlock = bodyLabel.blockIndex;
			//writefln("loop body %s, next %s", bodyLabel.blockIndex, nextStmt.blockIndex);
			builder.sealBlock(currentBlock);

			IrBasicBlock* block = &ir.getBlock(currentBlock);
			assert(!block.isFinished);
			visitStmt(n.statement, currentBlock, loopHeaderLabel);
		}

		builder.sealBlock(loopHeaderBlock);
	}
	void visit(DoWhileStmtNode* d, IrIndex currentBlock, ref IrLabel nextStmt) {
		context.unreachable;
	}
	void visit(ForStmtNode* n, IrIndex currentBlock, ref IrLabel nextStmt) {
		//writefln("loop cur 1 %s", currentBlock);

		// init statements
		IrLabel afterInitLabel = IrLabel(currentBlock);
		genBlock(n.as_node, n.init_statements, currentBlock, afterInitLabel);
		currentBlock = afterInitLabel.blockIndex;

		// loop header
		IrLabel loopHeaderLabel = IrLabel(currentBlock);
		// increment section of body
		IrLabel incrementLabel = IrLabel(currentBlock);

		// continue label
		IrLabel* prevLoopHeader = currentLoopHeader; // save continue label
		currentLoopHeader = &incrementLabel;
		scope(exit) currentLoopHeader = prevLoopHeader; // restore continue label

		// break label
		IrLabel* prevLoopEnd = currentLoopEnd; // save break label
		currentLoopEnd = &nextStmt;
		scope(exit) currentLoopEnd = prevLoopEnd; // restore break label

		builder.addJumpToLabel(currentBlock, loopHeaderLabel);

		// we need loop header in a separate block because it will
		// have 2 predecessors: currentBlock and loop body
		builder.forceAllocLabelBlock(loopHeaderLabel);
		IrIndex loopHeaderBlock = loopHeaderLabel.blockIndex;
		currentBlock = loopHeaderBlock;

		//writefln("loop head %s", loopHeaderBlock);
		ir.getBlock(loopHeaderBlock).isLoopHeader = true;

		IrLabel bodyLabel = IrLabel(currentBlock);
		//writefln("loop body %s, next %s", bodyLabel.blockIndex, nextStmt.blockIndex);

		// will force allocate body block
		if (n.condition)
			visitExprBranch(n.condition, loopHeaderBlock, bodyLabel, nextStmt);
		else
			builder.addJumpToLabel(loopHeaderBlock, bodyLabel);

		// body
		if (bodyLabel.numPredecessors > 0)
		{
			currentBlock = bodyLabel.blockIndex;
			//writefln("loop body %s, next %s", bodyLabel.blockIndex, nextStmt.blockIndex);
			builder.sealBlock(currentBlock);

			IrBasicBlock* block = &ir.getBlock(currentBlock);
			assert(!block.isFinished);
			visitStmt(n.statement, currentBlock, incrementLabel);

			if (incrementLabel.numPredecessors > 0)
			{
				builder.sealBlock(incrementLabel.blockIndex);
				genBlock(n.as_node, n.increment_statements, incrementLabel.blockIndex, loopHeaderLabel);
			}
		}

		builder.sealBlock(loopHeaderBlock);
	}
	void visit(ReturnStmtNode* r, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(CfgGenPrint) writefln("[CFG GEN] beg RETURN cur %s next %s", currentBlock, nextStmt);
		version(CfgGenPrint) scope(success) writefln("[CFG GEN] end RETURN cur %s next %s", currentBlock, nextStmt);
		version(IrGenPrint) writefln("[IR GEN] return (%s) begin", r.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] return (%s) end", r.loc);
		if (r.expression)
		{
			IrLabel afterExpr = IrLabel(currentBlock);
			visitExprValue(r.expression, currentBlock, afterExpr);
			currentBlock = afterExpr.blockIndex;
			builder.addReturn(currentBlock, context.getAstExpr(r.expression).irValue);
		}
		else builder.addReturn(currentBlock);
	}
	void visit(BreakStmtNode* b, IrIndex currentBlock, ref IrLabel nextStmt) {
		if (currentLoopEnd is null) context.unrecoverable_error(b.loc, "break is not within the loop");
		builder.addJumpToLabel(currentBlock, *currentLoopEnd);
	}
	void visit(ContinueStmtNode* c, IrIndex currentBlock, ref IrLabel nextStmt) {
		if (currentLoopHeader is null) context.unrecoverable_error(c.loc, "continue is not within the loop");
		builder.addJumpToLabel(currentBlock, *currentLoopHeader);
	}

	void visitExprValue(NameUseExprNode* v, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(CfgGenPrint) writefln("[CFG GEN] beg VAR_USE VAL cur %s next %s", currentBlock, nextStmt);
		version(CfgGenPrint) scope(success) writefln("[CFG GEN] end VAR_USE VAL cur %s next %s", currentBlock, nextStmt);
		version(IrGenPrint) writefln("[IR GEN] var expr value (%s) begin", v.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] var expr value (%s) end", v.loc);

		AstNode* entity = v.entity.get_node(context);
		switch (entity.astType) with(AstType)
		{
			case decl_enum_member:
			{
				EnumMemberDecl* member = entity.cast_decl_enum_member;
				IrLabel after = IrLabel(currentBlock);
				visitExprValue(member.initializer, currentBlock, after);
				currentBlock = after.blockIndex;
				v.irValue = context.getAstExpr(member.initializer).irValue;
				break;
			}
			case decl_var:
			{
				if (v.isLvalue) {
					v.irValue = v.varDecl(context).irValue;
				}
				else {
					TypeNode* type = context.getAstType(v.varDecl(context).type).foldAliases(context);
					if (v.isArgument && type.isPassByPtr)
					{
						IrIndex irType = type.gen_ir_type(context);
						uint size = context.types.typeSize(irType);
						if (size == 1 || size == 2 || size == 4 || size == 8)
						{
							// pass by value
							v.irValue = load(currentBlock, v.varDecl(context).irValue);
						}
						else
						{
							// pass pointer
							context.todo("need to pass pointer to copy");
							v.irValue = v.varDecl(context).irValue;
						}
					}
					else
					{
						v.irValue = load(currentBlock, v.varDecl(context).irValue);
					}
				}
				break;
			}
			default:
				writefln("visitExprValue %s", entity.astType);
				context.unreachable; assert(false);
		}

		builder.addJumpToLabel(currentBlock, nextStmt);
	}
	void visitExprValue(MemberExprNode* m, IrIndex currentBlock, ref IrLabel nextStmt) {
		version(CfgGenPrint) writefln("[CFG GEN] beg MEMBER cur %s next %s", currentBlock, nextStmt);
		version(CfgGenPrint) scope(success) writefln("[CFG GEN] end MEMBER cur %s next %s", currentBlock, nextStmt);

		CompilationContext* c = context;

		switch(m.subType) with(MemberSubType)
		{
			case static_array_member:
				if (m.isLvalue) {
					c.internal_error(m.loc, "cannot assign static array member");
				}
				if (m.memberIndex == BuiltinMemberIndex.MEMBER_LENGTH) // length
				{
					StaticArrayTypeNode* arr = m.aggregate.get_node_type(c).cast_type_static_array(c);
					m.irValue = c.constants.add(arr.length, IsSigned.no, IrArgSize.size64);
				}
				else if (m.memberIndex == BuiltinMemberIndex.MEMBER_PTR) // ptr
				{
					IrLabel afterAggr = IrLabel(currentBlock);
					m.aggregate.get_node(c).flags |= AstFlags.isLvalue;
					visitExprValue(m.aggregate, currentBlock, afterAggr);
					currentBlock = afterAggr.blockIndex;
					m.irValue = buildGEP(currentBlock, m.aggregate.get_expr(c).irValue, c.constants.ZERO, c.constants.ZERO);
				}
				break;
			case nonstatic_struct_member, slice_member:
				IrLabel afterAggr = IrLabel(currentBlock);
				m.aggregate.get_node(c).flags |= AstFlags.isLvalue;
				visitExprValue(m.aggregate, currentBlock, afterAggr);
				currentBlock = afterAggr.blockIndex;

				IrIndex memberIndex = c.constants.add(m.memberIndex, IsSigned.no);
				LRValue rlVal = getMember(currentBlock, m.aggregate.get_expr(c).irValue, memberIndex);

				if (m.isLvalue) {
					if (rlVal.isLvalue) m.irValue = rlVal.value;
					else c.internal_error(m.loc, "member expression is not an l-value");
				}
				else {
					if (rlVal.isLvalue)
						m.irValue = load(currentBlock, rlVal.value);
					else m.irValue = rlVal.value;
				}
				break;
			case static_struct_member:
				context.unreachable("Not implemented");
				break;
			case enum_member:
				EnumMemberDecl* member = m.member.get_name_use(c).enumMember(c);
				IrLabel afterAggr = IrLabel(currentBlock);
				visitExprValue(member.initializer, currentBlock, afterAggr);
				currentBlock = afterAggr.blockIndex;
				m.irValue = member.initializer.get_expr(c).irValue;
				break;
			case basic_member:
				TypeNode* obj = m.aggregate.get_node_type(c).get_type(c);
				if (auto b = obj.as_basic)
				{
					if (b.isInteger)
					{
						if (m.memberIndex == BuiltinMemberIndex.MEMBER_MIN)
						{
							m.irValue = c.constants.add(b.minValue, b.isSigned, obj.argSize(c));
							break;
						}
						else if (m.memberIndex == BuiltinMemberIndex.MEMBER_MAX)
						{
							m.irValue = c.constants.add(b.maxValue, b.isSigned, obj.argSize(c));
							break;
						}
					}
				}
				c.internal_error(m.loc, "Unexpected node type %s", m.astType);
				break;
			default:
				c.internal_error(m.loc, "Unexpected node type %s", m.astType);
		}

		builder.addJumpToLabel(currentBlock, nextStmt);
	}
	void visitExprValue(IntLiteralExprNode* c, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(CfgGenPrint) writefln("[CFG GEN] beg INT LITERAL VAL cur %s next %s", currentBlock, nextStmt);
		version(CfgGenPrint) scope(success) writefln("[CFG GEN] end INT LITERAL VAL cur %s next %s", currentBlock, nextStmt);
		version(IrGenPrint) writefln("[IR GEN] int literal value (%s) value %s", c.loc, c.value);

		if (!c.irValue.isDefined) {
			c.irValue = context.constants.add(c.value, c.isSigned, c.type.typeArgSize(context));
		}

		builder.addJumpToLabel(currentBlock, nextStmt);
	}
	void visitExprValue(StringLiteralExprNode* c, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(CfgGenPrint) writefln("[CFG GEN] beg STR LITERAL VAL cur %s next %s", currentBlock, nextStmt);
		version(CfgGenPrint) scope(success) writefln("[CFG GEN] end STR LITERAL VAL cur %s next %s", currentBlock, nextStmt);
		version(IrGenPrint) writefln("[IR GEN] str literal value (%s) value %s", c.loc, c.value);

		builder.addJumpToLabel(currentBlock, nextStmt);
	}

	void visitExprValue(NullLiteralExprNode* c, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		if (!c.irValue.isDefined) {
			if (c.type.get_type(context).isPointer) {
				c.irValue = context.constants.add(0, IsSigned.no, SIZET_SIZE);
			} else if (c.type.get_type(context).isSlice) {
				IrIndex irValue = context.constants.add(0, IsSigned.no, SIZET_SIZE); // ptr and length
				c.irValue = context.constants.addAggrecateConstant(c.type.gen_ir_type(context), irValue, irValue);
			} else context.internal_error(c.loc, "%s", c.type.printer(context));
		}

		builder.addJumpToLabel(currentBlock, nextStmt);
	}

	void visitExprValue(BoolLiteralExprNode* c, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		if (!c.irValue.isDefined) {
			if (c.value)
				c.irValue = context.constants.add(1, IsSigned.no, c.type.typeArgSize(context));
			else
				c.irValue = context.constants.add(0, IsSigned.no, c.type.typeArgSize(context));
		}
		builder.addJumpToLabel(currentBlock, nextStmt);
	}

	void visitExprValue(EnumMemberDecl* n, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		IrLabel afterExpr = IrLabel(currentBlock);
		visitExprValue(n.initializer, currentBlock, afterExpr);
		currentBlock = afterExpr.blockIndex;
		builder.addJumpToLabel(currentBlock, nextStmt);
	}

	// In value mode only uses trueExit as nextStmt
	void visitBinOpImpl(bool forValue)(BinaryExprNode* b, ref IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit)
	{
		IrLabel afterLeft = IrLabel(currentBlock);
		visitExprValue(b.left, currentBlock, afterLeft);
		currentBlock = afterLeft.blockIndex;
		version(CfgGenPrint) writefln("[CFG GEN] after left cur %s true %s false %s", currentBlock, trueExit, falseExit);
		IrLabel afterRight = IrLabel(currentBlock);
		visitExprValue(b.right, currentBlock, afterRight);
		currentBlock = afterRight.blockIndex;
		version(CfgGenPrint) writefln("[CFG GEN] after right cur %s true %s false %s", currentBlock, trueExit, falseExit);

		ExpressionNode* leftExpr = b.left.get_expr(context);
		ExpressionNode* rightExpr = b.right.get_expr(context);

		context.assertf(leftExpr.irValue.isDefined, leftExpr.loc, "%s null IR val", leftExpr.astType);
		context.assertf(rightExpr.irValue.isDefined, rightExpr.loc, "%s null IR val", rightExpr.astType);

		// constant folding
		if (leftExpr.irValue.isConstant && rightExpr.irValue.isConstant)
		{
			IrIndex value = calcBinOp(b.op, leftExpr.irValue, rightExpr.irValue, b.type.typeArgSize(context), context);
			static if (forValue)
			{
				b.irValue = value;
				context.assertf(b.irValue.isDefined, b.loc, "%s null IR val", b.astType);
			}
			else
			{
				if (context.constants.get(value).i8)
					builder.addJumpToLabel(currentBlock, trueExit);
				else
					builder.addJumpToLabel(currentBlock, falseExit);
			}
			version(IrGenPrint) writefln("[IR GEN]   const expr %s", value);
			return;
		}

		auto leftValue = leftExpr.irValue;
		auto rightValue = rightExpr.irValue;

		static if (forValue)
		{
			switch(b.op) with(BinOp)
			{
				case EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL:
					version(IrGenPrint) writefln("[IR GEN]   rel op value %s", b.op);
					ExtraInstrArgs extra = {cond : convertBinOpToIrCond(b.op), type : b.type.gen_ir_type(context), argSize : leftExpr.type.typeArgSize(context) };
					b.irValue = builder.emitInstr!IrInstr_set_binary_cond(
						currentBlock, extra, leftValue, rightValue).result;
					break;

				case PTR_PLUS_INT:
					assert(leftExpr.type.get_type(context).isPointer && rightExpr.type.get_type(context).isInteger);
					b.irValue = buildGEP(currentBlock, leftValue, rightExpr.irValue);
					break;

				case PTR_DIFF:
					assert(leftExpr.type.get_type(context).isPointer && rightExpr.type.get_type(context).isPointer);

					ExtraInstrArgs extra = { type : b.type.gen_ir_type(context), argSize : leftExpr.type.typeArgSize(context) };
					b.irValue = builder.emitInstr!IrInstr_sub(currentBlock, extra, leftValue, rightValue).result;

					// divide by elem size
					TypeNode* baseType = leftExpr.type.get_type(context).as_ptr.base.get_type(context);
					uint elemSize = baseType.size(context);
					if (elemSize == 1 || baseType.isVoid) break;

					ExtraInstrArgs extra2 = { type : makeBasicTypeIndex(IrValueType.i64), argSize : leftExpr.type.typeArgSize(context) };
					IrIndex elemSizeValue = context.constants.add(elemSize, IsSigned.no, leftExpr.type.typeArgSize(context));
					b.irValue = builder.emitInstr!IrInstr_sdiv(currentBlock, extra, b.irValue, elemSizeValue).result;
					break;

				// TODO
				case PLUS, MINUS, DIV, REMAINDER, MULT, SHL, SHR, ASHR, XOR, BITWISE_AND, BITWISE_OR:
					ExtraInstrArgs extra = {
						opcode : binOpcode(b.op, leftExpr.type.get_type(context).isUnsigned, b.loc),
						type : b.type.gen_ir_type(context),
						argSize : b.type.typeArgSize(context)
					};
					b.irValue = builder.emitInstr!IrInstr_any_binary_instr(
						currentBlock, extra, leftValue, rightValue).result;
					break;
				default: context.internal_error(b.loc, "Opcode `%s` is not implemented", b.op); break;
			}
			context.assertf(b.irValue.isDefined, b.loc, "%s null IR val", b.astType);
			builder.addJumpToLabel(currentBlock, trueExit);
		}
		else // branch
		{
			switch(b.op) with(BinOp)
			{
				case EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL:
					version(IrGenPrint) writefln("[IR GEN]   rel op branch %s", b.op);
					auto branch = builder.addBinBranch(
						currentBlock, convertBinOpToIrCond(b.op), leftExpr.type.typeArgSize(context),
						leftValue, rightValue, trueExit, falseExit);
					break;
				default: context.internal_error(b.loc, "Opcode `%s` is not implemented", b.op); break;
			}
		}
	}

	void visitExprBranch(BinaryExprNode* b, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit)
	{
		version(CfgGenPrint) writefln("[CFG GEN] beg BINOP BR cur %s true %s false %s", currentBlock, trueExit, falseExit);
		version(CfgGenPrint) scope(success) writefln("[CFG GEN] end BINOP BR cur %s true %s false %s", currentBlock, trueExit, falseExit);
		version(IrGenPrint) writefln("[IR GEN] bin expr branch (%s) begin", b.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] bin expr branch (%s) end", b.loc);
		if (b.isAssignment)
		{
			context.error(b.loc, "Cannot assign inside condition");
		}
		if (b.op == BinOp.LOGIC_AND)
		{
			IrLabel cond2Label = IrLabel(currentBlock);
			visitExprBranch(b.left, currentBlock, cond2Label, falseExit);

			if (cond2Label.numPredecessors != 0)
			{
				IrIndex cond2Block = cond2Label.blockIndex;
				builder.sealBlock(cond2Block);
				visitExprBranch(b.right, cond2Block, trueExit, falseExit);
			}

			return;
		}
		else if (b.op == BinOp.LOGIC_OR)
		{
			IrLabel cond2Label = IrLabel(currentBlock);
			visitExprBranch(b.left, currentBlock, trueExit, cond2Label);

			if (cond2Label.numPredecessors != 0)
			{
				IrIndex cond2Block = cond2Label.blockIndex;
				builder.sealBlock(cond2Block);
				visitExprBranch(b.right, cond2Block, trueExit, falseExit);
			}

			return;
		}
		visitBinOpImpl!false(b, currentBlock, trueExit, falseExit);
	}

	void visitExprBranch(UnaryExprNode* u, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit)
	{
		switch(u.op) with(UnOp)
		{
			case logicalNot:
				visitExprBranch(u.child, currentBlock, falseExit, trueExit);
				break;

			default: context.internal_error(u.loc, "Opcode `%s` is not implemented", u.op); break;
		}
	}

	void visitExprBranch(NameUseExprNode* n, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit)
	{
		IrLabel afterExpr = IrLabel(currentBlock);
		visitExprValue(n, currentBlock, afterExpr);
		currentBlock = afterExpr.blockIndex;

		addUnaryBranch(n.irValue, currentBlock, trueExit, falseExit);
	}

	void visitExprBranch(MemberExprNode* n, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit)
	{
		IrLabel afterExpr = IrLabel(currentBlock);
		visitExprValue(n, currentBlock, afterExpr);
		currentBlock = afterExpr.blockIndex;

		addUnaryBranch(n.irValue, currentBlock, trueExit, falseExit);
	}

	void visitExprBranch(BoolLiteralExprNode* n, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit)
	{
		if (n.value)
			builder.addJumpToLabel(currentBlock, trueExit);
		else
			builder.addJumpToLabel(currentBlock, falseExit);
		return;
	}

	void visitExprBranch(CallExprNode* n, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit)
	{
		IrLabel afterExpr = IrLabel(currentBlock);
		visitExprValue(n, currentBlock, afterExpr);
		currentBlock = afterExpr.blockIndex;

		addUnaryBranch(n.irValue, currentBlock, trueExit, falseExit);
	}

	IrOpcode binOpcode(BinOp binop, bool isUnsigned, TokenIndex loc) {
		switch(binop) with(BinOp)
		{
			case PLUS, PLUS_ASSIGN: return IrOpcode.add;
			case MINUS, MINUS_ASSIGN: return IrOpcode.sub;
			case DIV, DIV_ASSIGN:
				if (isUnsigned) return IrOpcode.udiv;
				else return IrOpcode.sdiv;
			case REMAINDER, REMAINDER_ASSIGN:
				if (isUnsigned) return IrOpcode.urem;
				else return IrOpcode.srem;
			case MULT, MULT_ASSIGN:
				if (isUnsigned) return IrOpcode.umul;
				else return IrOpcode.smul;
			case SHL, SHL_ASSIGN: return IrOpcode.shl;
			case SHR, SHR_ASSIGN: return IrOpcode.lshr;
			case ASHR, ASHR_ASSIGN: return IrOpcode.ashr;
			case XOR, XOR_ASSIGN: return IrOpcode.xor;
			case BITWISE_AND, BITWISE_AND_ASSIGN: return IrOpcode.and;
			case BITWISE_OR, BITWISE_OR_ASSIGN: return IrOpcode.or;
			default:
				context.internal_error(loc, "assign op %s not implemented", binop);
				assert(false);
		}
	}

	void visitExprValue(BinaryExprNode* b, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(CfgGenPrint) writefln("[CFG GEN] beg BINOP VAL cur %s next %s", currentBlock, nextStmt);
		version(CfgGenPrint) scope(success) writefln("[CFG GEN] end BINOP VAL cur %s next %s", currentBlock, nextStmt);
		version(IrGenPrint) writefln("[IR GEN] bin expr value (%s) begin", b.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] bin expr value (%s) end", b.loc);

		if (b.op == BinOp.LOGIC_AND || b.op == BinOp.LOGIC_OR)
		{
			IrLabel afterChild = IrLabel(currentBlock);
			b.irValue = makeBoolValue(cast(ExpressionNode*)b, currentBlock, afterChild);
			currentBlock = afterChild.blockIndex;
			builder.addJumpToLabel(currentBlock, nextStmt);
			return;
		}

		if (b.isAssignment)
		{
			IrLabel afterLeft = IrLabel(currentBlock);
			visitExprValue(b.left, currentBlock, afterLeft);
			currentBlock = afterLeft.blockIndex;

			IrLabel afterRight = IrLabel(currentBlock);
			visitExprValue(b.right, currentBlock, afterRight);
			currentBlock = afterRight.blockIndex;

			ExpressionNode* leftExpr = b.left.get_expr(context);
			ExpressionNode* rightExpr = b.right.get_expr(context);

			context.assertf(leftExpr.irValue.isDefined, leftExpr.loc, "%s null IR val", leftExpr.astType);
			context.assertf(rightExpr.irValue.isDefined, rightExpr.loc, "%s null IR val", rightExpr.astType);

			if (b.op == BinOp.ASSIGN) {
				store(currentBlock, leftExpr.irValue, rightExpr.irValue);
				b.irValue = rightExpr.irValue;
			}
			else if (b.op == BinOp.PTR_PLUS_INT_ASSIGN)
			{
				IrIndex leftRvalue = load(currentBlock, leftExpr.irValue);
				assert(leftExpr.type.get_type(context).isPointer && rightExpr.type.get_type(context).isInteger);
				IrIndex opResult = buildGEP(currentBlock, leftRvalue, rightExpr.irValue);
				store(currentBlock, leftExpr.irValue, opResult);
				b.irValue = opResult;
			}
			else
			{
				IrIndex leftRvalue = load(currentBlock, leftExpr.irValue);

				if (leftRvalue.isConstant && rightExpr.irValue.isConstant)
				{
					b.irValue = calcBinOp(binOpAssignToRegularOp(b.op), leftRvalue, rightExpr.irValue, leftExpr.type.typeArgSize(context), context);
				}
				else
				{
					ExtraInstrArgs extra = {
						opcode : binOpcode(b.op, leftExpr.type.get_type(context).isUnsigned, b.loc),
						type : leftExpr.type.gen_ir_type(context),
						argSize : leftExpr.type.typeArgSize(context)
					};
					b.irValue = builder.emitInstr!IrInstr_any_binary_instr(
						currentBlock, extra, leftRvalue, rightExpr.irValue).result;
				}

				store(currentBlock, leftExpr.irValue, b.irValue);
			}
			context.assertf(b.irValue.isDefined, b.loc, "%s %s null IR val", b.op, b.astType);

			builder.addJumpToLabel(currentBlock, nextStmt);
		}
		else
		{
			IrLabel fake;
			visitBinOpImpl!true(b, currentBlock, nextStmt, fake);
			assert(fake.numPredecessors == 0);
		}
	}

	IrIndex makeBoolValue(ExpressionNode* n, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		IrLabel trueLabel = IrLabel(currentBlock);
		IrLabel falseLabel = IrLabel(currentBlock);
		IrLabel nextLabel = IrLabel(currentBlock);
		IrIndex nextBlock;
		visitExprBranch(context.getAstNodeIndex(n), currentBlock, trueLabel, falseLabel);

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

				IrIndex phiIndex = builder.addPhi(nextBlock, n.type.gen_ir_type(context), IrIndex.init);
				IrIndex trueValue = context.constants.add(1, IsSigned.no, n.type.typeArgSize(context));
				builder.addPhiArg(phiIndex, trueBlock, trueValue);
				IrIndex falseValue = context.constants.add(0, IsSigned.no, n.type.typeArgSize(context));
				builder.addPhiArg(phiIndex, falseBlock, falseValue);
				value = builder.ir.get!IrPhi(phiIndex).result;
			}
			else // only true block exists
			{
				nextBlock = trueBlock;
				value = context.constants.add(1, IsSigned.no, n.type.typeArgSize(context));
			}
		}
		else if (falseLabel.numPredecessors != 0) // only false block exists
		{
			nextBlock = falseLabel.blockIndex;
			builder.sealBlock(nextBlock);

			value = context.constants.add(0, IsSigned.no, n.type.typeArgSize(context));
		}

		builder.addJumpToLabel(nextBlock, nextStmt);

		return value;
	}

	void visitExprValue(UnaryExprNode* u, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		switch(u.op) with(UnOp)
		{
			case addrOf:
				u.child.get_node(context).flags |= AstFlags.isLvalue;
				IrLabel afterChild = IrLabel(currentBlock);
				visitExprValue(u.child, currentBlock, afterChild);
				currentBlock = afterChild.blockIndex;
				u.irValue = u.child.get_expr(context).irValue;
				break;
			case bitwiseNot:
				IrLabel afterChild = IrLabel(currentBlock);
				visitExprValue(u.child, currentBlock, afterChild);
				currentBlock = afterChild.blockIndex;
				ExtraInstrArgs extra = {type : u.type.gen_ir_type(context), argSize : u.type.typeArgSize(context) };
				u.irValue = builder.emitInstr!IrInstr_not(currentBlock, extra, u.child.get_expr(context).irValue).result;
				break;
			case logicalNot:
				IrLabel afterChild = IrLabel(currentBlock);
				u.irValue = makeBoolValue(cast(ExpressionNode*)u, currentBlock, afterChild);
				currentBlock = afterChild.blockIndex;
				break;
			case minus:
				IrLabel afterChild = IrLabel(currentBlock);
				visitExprValue(u.child, currentBlock, afterChild);
				currentBlock = afterChild.blockIndex;
				ExtraInstrArgs extra = {type : u.type.gen_ir_type(context), argSize : u.type.typeArgSize(context) };
				u.irValue = builder.emitInstr!IrInstr_neg(currentBlock, extra, u.child.get_expr(context).irValue).result;
				break;
			case preIncrement, postIncrement, preDecrement, postDecrement:
				IrOpcode opcode = IrOpcode.sub;
				if (u.op == preIncrement || u.op == postIncrement) opcode = IrOpcode.add;

				ExpressionNode* childExpr = u.child.get_expr(context);
				TypeNode* childType = childExpr.type.get_type(context);
				childExpr.flags |= AstFlags.isLvalue;

				IrLabel afterChild = IrLabel(currentBlock);
				visitExprValue(u.child, currentBlock, afterChild);
				currentBlock = afterChild.blockIndex;

				IrIndex increment = context.constants.ONE; // integers increment by 1
				if (childType.isPointer) { // pointers increment by size of element
					uint size = childType.as_ptr.base.typeSize(context);
					increment = context.constants.add(size, IsSigned.no);
				}

				IrArgSize argSize = childType.argSize(context);
				IrIndex rval = load(currentBlock, childExpr.irValue);
				ExtraInstrArgs extra = {
					opcode : opcode,
					type : childType.gen_ir_type(context),
					argSize : argSize
				};
				IrIndex opResult = builder.emitInstr!IrInstr_any_binary_instr(
					currentBlock, extra, rval, increment).result;
				store(currentBlock, childExpr.irValue, opResult);

				if (u.op == preIncrement || u.op == preDecrement) u.irValue = opResult;
				else u.irValue = rval;
				break;
			case deref:
				IrLabel afterChild = IrLabel(currentBlock);
				visitExprValue(u.child, currentBlock, afterChild);
				currentBlock = afterChild.blockIndex;
				if (u.isLvalue) {
					u.irValue = u.child.get_expr(context).irValue;
				} else {
					u.irValue = load(currentBlock, u.child.get_expr(context).irValue);
				}
				break;
			case staticArrayToSlice:
				IrLabel afterChild = IrLabel(currentBlock);
				ExpressionNode* childExpr = u.child.get_expr(context);
				childExpr.flags |= AstFlags.isLvalue;
				visitExprValue(u.child, currentBlock, afterChild);
				currentBlock = afterChild.blockIndex;

				IrIndex type = ir.getValueType(*context, childExpr.irValue);
				context.assertf(type.isTypePointer, "%s", type); // pointer to static array

				// pointer to first element
				IrIndex ptr = buildGEP(currentBlock, childExpr.irValue, context.constants.ZERO, context.constants.ZERO);
				// array length
				IrIndex length = context.constants.add(childExpr.type.get_type(context).as_static_array.length, IsSigned.no, IrArgSize.size64);

				// combine into slice {i64, T*}
				IrIndex resType = u.type.gen_ir_type(context);
				ExtraInstrArgs extra = { type : resType };
				InstrWithResult res = builder.emitInstr!IrInstr_create_aggregate(currentBlock, extra, length, ptr);
				u.irValue = res.result;
				break;
			default:
				context.internal_error(u.loc, "un op %s not implemented", u.op);
				builder.addJumpToLabel(currentBlock, nextStmt);
				break;
		}
		builder.addJumpToLabel(currentBlock, nextStmt);
	}

	void visitExprValue(CallExprNode* c, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(CfgGenPrint) writefln("[CFG GEN] beg CALL VAL cur %s next %s", currentBlock, nextStmt);
		version(CfgGenPrint) scope(success) writefln("[CFG GEN] end CALL VAL cur %s next %s", currentBlock, nextStmt);
		version(IrGenPrint) writefln("[IR GEN] call value (%s) begin", c.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] call value (%s) end", c.loc);

		AstNode* callee = c.callee.get_expr(context).as_name_use.entity.get_node(context);

		switch (callee.astType)
		{
			case AstType.decl_function: return visitCall(c, callee.cast_decl_function, currentBlock, nextStmt);
			case AstType.decl_struct: return visitConstructor(c, callee.cast_decl_struct, currentBlock, nextStmt);
			default:
				c.type = context.basicTypeNodes(BasicType.t_error);
				context.error(c.loc, "Cannot call %s", callee.astType);
		}
	}

	void visitCall(CallExprNode* c, FunctionDeclNode* callee, IrIndex currentBlock, ref IrLabel nextStmt) {
		context.assertf(c.args.length <= IrInstrHeader.MAX_ARGS,
			"Cannot generate a call with %s arguments, max args is %s",
			c.args.length, IrInstrHeader.MAX_ARGS);

		IrIndex[] args = context.allocateTempArray!IrIndex(c.args.length);
		scope(exit) context.freeTempArray(args);

		foreach (i, AstIndex arg; c.args)
		{
			IrLabel afterArg = IrLabel(currentBlock);
			ExpressionNode* node = arg.get_expr(context);
			node.flags |= AstFlags.isArgument;
			visitExprValue(arg, currentBlock, afterArg);
			currentBlock = afterArg.blockIndex;
			args[i] = node.irValue;
			debug context.assertf(node.irValue.isDefined, "Arg %s %s (%s) is undefined", i+1, node.astType, context.tokenLoc(node.loc));
		}

		// TODO: support more than plain func() calls. Such as func_array[42](), (*func_ptr)() etc
		// need handling of function pointers, need function types in IR for that

		version(IrGenPrint) writefln("[IR GEN] call args %s, callee %s", args, callee.backendData.index);
		builder.emitInstrPreheader(IrInstrPreheader_call(callee.backendData.index));

		if (callee.returnType.get_type(context).isVoid)
		{
			InstrWithResult res = builder.emitInstr!IrInstr_call(currentBlock, args);
			context.assertf(!res.result.isDefined, "Call has result");
		}
		else
		{
			callee.backendData.returnType = callee.returnType.gen_ir_type(context);

			ExtraInstrArgs extra = {hasResult : true, type : callee.backendData.returnType};
			InstrWithResult res = builder.emitInstr!IrInstr_call(currentBlock, extra, args);
			c.irValue = res.result;
		}

		if (c.isLvalue) {
			context.internal_error(c.loc, "Call cannot be an l-value");
		}

		builder.addJumpToLabel(currentBlock, nextStmt);
	}

	void visitConstructor(CallExprNode* c, StructDeclNode* s, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		IrIndex structType = s.gen_ir_type_struct(context);
		uint numStructMembers = context.types.get!IrTypeStruct(structType).numMembers;
		IrIndex[] args = context.allocateTempArray!IrIndex(numStructMembers);
		scope(exit) context.freeTempArray(args);

		uint memberIndex;
		foreach(AstIndex member; s.declarations)
		{
			if (member.get_node(context).astType != AstType.decl_var) continue;

			AstIndex initializer;
			if (c.args.length > memberIndex) { // init from constructor argument
				initializer = c.args[memberIndex];
			} else { // init with initializer from struct definition
				context.internal_error(c.loc, "Not implemented");
			}

			IrLabel afterArg = IrLabel(currentBlock);
			visitExprValue(initializer, currentBlock, afterArg);
			args[memberIndex] = initializer.get_expr(context).irValue;
			currentBlock = afterArg.blockIndex;

			++memberIndex;
		}

		ExtraInstrArgs extra = { type : structType };
		InstrWithResult res = builder.emitInstr!IrInstr_create_aggregate(currentBlock, extra, args);
		c.irValue = res.result;

		if (c.isLvalue) {
			context.internal_error(c.loc, "Constructor cannot be an l-value");
		}

		c.type = context.getAstNodeIndex(s);
	}

	void visitExprValue(IndexExprNode* i, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(CfgGenPrint) writefln("[CFG GEN] beg INDEX VAL cur %s next %s", currentBlock, nextStmt);
		version(CfgGenPrint) scope(success) writefln("[CFG GEN] end INDEX VAL cur %s next %s", currentBlock, nextStmt);
		ExpressionNode* arrayExpr = i.array.get_expr(context);
		ExpressionNode* indexExpr = i.index.get_expr(context);

		IrLabel afterIndex = IrLabel(currentBlock);
		arrayExpr.flags |= AstFlags.isLvalue;
		visitExprValue(i.array, currentBlock, afterIndex);
		currentBlock = afterIndex.blockIndex;

		IrLabel afterRight = IrLabel(currentBlock);
		visitExprValue(i.index, currentBlock, afterRight);
		currentBlock = afterRight.blockIndex;

		IrIndex aggregateIndex = context.constants.ZERO;
		IrIndex slicePtrIndex = context.constants.ONE;

		switch (arrayExpr.type.get_type(context).astType) with(AstType)
		{
			case type_ptr:
				i.irValue = buildGEP(currentBlock, arrayExpr.irValue, indexExpr.irValue);
				break;
			case type_static_array:
				IrIndex type = ir.getValueType(*context, arrayExpr.irValue);
				if (type.isTypePointer)
					i.irValue = buildGEP(currentBlock, arrayExpr.irValue, aggregateIndex, indexExpr.irValue);
				else {
					context.assertf(type.isTypeArray, "%s", IrIndexDump(type, *context, *ir));
					i.irValue = buildGEP(currentBlock, arrayExpr.irValue, indexExpr.irValue);
				}
				break;
			case type_slice:
				IrIndex ptrPtr = buildGEP(currentBlock, arrayExpr.irValue, aggregateIndex, slicePtrIndex);
				IrIndex ptr = load(currentBlock, ptrPtr);
				i.irValue = buildGEP(currentBlock, ptr, indexExpr.irValue);
				break;
			default:
				context.internal_error("Cannot index %s", arrayExpr.type.printer(context));
				break;
		}

		if (!i.isLvalue) {
			i.irValue = load(currentBlock, i.irValue);
		}

		builder.addJumpToLabel(currentBlock, nextStmt);
	}

	void addUnaryBranch(IrIndex value, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit)
	{
		if (value.isConstant)
		{
			long conValue = context.constants.get(value).i64;
			if (conValue != 0)
				builder.addJumpToLabel(currentBlock, trueExit);
			else
				builder.addJumpToLabel(currentBlock, falseExit);
			return;
		}

		IrArgSize argSize = sizeToIrArgSize(context.types.typeSize(ir.getValueType(*context, value)), context);
		builder.addUnaryBranch(currentBlock, IrUnaryCondition.not_zero, argSize, value, trueExit, falseExit);
	}

	void visitExprBranch(TypeConvExprNode* t, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit) {
		version(CfgGenPrint) writefln("[CFG GEN] beg TYPE_CONV BR cur %s true %s false %s", currentBlock, trueExit, falseExit);
		version(CfgGenPrint) scope(success) writefln("[CFG GEN] end TYPE_CONV BR cur %s true %s false %s", currentBlock, trueExit, falseExit);
		IrLabel afterExpr = IrLabel(currentBlock);
		visitExprValue(t.expr, currentBlock, afterExpr);
		currentBlock = afterExpr.blockIndex;

		ExpressionNode* childExpr = t.expr.get_expr(context);
		IrIndex from = childExpr.type.gen_ir_type(context);

		if (childExpr.irValue.isConstant ||
			from == makeBasicTypeIndex(IrValueType.i8) ||
			from == makeBasicTypeIndex(IrValueType.i16) ||
			from == makeBasicTypeIndex(IrValueType.i32) ||
			from == makeBasicTypeIndex(IrValueType.i64))
		{
			addUnaryBranch(childExpr.irValue, currentBlock, trueExit, falseExit);
			return;
		}
		else
		{
			//t.irValue = builder.emitInstr1(IrOpcode.o_conv, to, childExpr.irValue);
			context.internal_error(t.loc, "%s to %s", childExpr.type.printer(context), t.type.printer(context));
		}
		context.unreachable;
	}
	void visitExprValue(TypeConvExprNode* t, IrIndex currentBlock, ref IrLabel nextStmt) {
		version(CfgGenPrint) writefln("[CFG GEN] beg TYPE_CONV VAL cur %s next %s", currentBlock, nextStmt);
		version(CfgGenPrint) scope(success) writefln("[CFG GEN] end TYPE_CONV VAL cur %s next %s", currentBlock, nextStmt);
		IrLabel afterExpr = IrLabel(currentBlock);
		visitExprValue(t.expr, currentBlock, afterExpr);
		currentBlock = afterExpr.blockIndex;

		ExpressionNode* childExpr = t.expr.get_expr(context);
		IrIndex to = t.type.gen_ir_type(context);
		IrIndex from = childExpr.type.gen_ir_type(context);
		if (childExpr.irValue.isConstant || (from == makeBasicTypeIndex(IrValueType.i32) && to == makeBasicTypeIndex(IrValueType.i64)))
		{
			t.irValue = childExpr.irValue;
		}
		else if (t.type.get_type(context).isBool)
		{
			ExtraInstrArgs extra = { type : to, cond : IrUnaryCondition.not_zero };
			t.irValue = builder.emitInstr!IrInstr_set_unary_cond(currentBlock, extra, childExpr.irValue).result;
		}
		else
		{
			ExtraInstrArgs extra = {type : to};
			t.irValue = builder.emitInstr!IrInstr_conv(currentBlock, extra, childExpr.irValue).result;
		}
		builder.addJumpToLabel(currentBlock, nextStmt);
	}
	void visit(BasicTypeNode* t) { context.unreachable; }
	void visit(PtrTypeNode* t) { context.unreachable; }
	void visit(StaticArrayTypeNode* t) { context.unreachable; }
	void visit(SliceTypeNode* t) { context.unreachable; }
}

IrIndex calcBinOp(BinOp op, IrIndex left, IrIndex right, IrArgSize argSize, CompilationContext* context)
{
	IrConstant leftCon = context.constants.get(left);
	IrConstant rightCon = context.constants.get(right);

	bool isAnySigned = left.isSignedConstant || right.isSignedConstant;

	switch(op)
	{
		case BinOp.EQUAL:         return context.constants.add(cast(ubyte)(leftCon.i64 == rightCon.i64), IsSigned.no, argSize);
		case BinOp.NOT_EQUAL:     return context.constants.add(cast(ubyte)(leftCon.i64 != rightCon.i64), IsSigned.no, argSize);
		case BinOp.GREATER:       return context.constants.add(cast(ubyte)(leftCon.i64 >  rightCon.i64), IsSigned.no, argSize);
		case BinOp.GREATER_EQUAL: return context.constants.add(cast(ubyte)(leftCon.i64 >= rightCon.i64), IsSigned.no, argSize);
		case BinOp.LESS:          return context.constants.add(cast(ubyte)(leftCon.i64 <  rightCon.i64), IsSigned.no, argSize);
		case BinOp.LESS_EQUAL:    return context.constants.add(cast(ubyte)(leftCon.i64 <= rightCon.i64), IsSigned.no, argSize);
		case BinOp.PLUS:          return context.constants.add(leftCon.i64 + rightCon.i64, cast(IsSigned)isAnySigned, argSize);
		case BinOp.MINUS:         return context.constants.add(leftCon.i64 - rightCon.i64, cast(IsSigned)isAnySigned, argSize);
		case BinOp.MULT:          return context.constants.add(leftCon.i64 * rightCon.i64, cast(IsSigned)isAnySigned, argSize);
		case BinOp.DIV:           return context.constants.add(leftCon.i64 / rightCon.i64, cast(IsSigned)isAnySigned, argSize);
		case BinOp.REMAINDER:     return context.constants.add(leftCon.i64 % rightCon.i64, cast(IsSigned)isAnySigned, argSize);

		// TODO: we need type info here, to correctly mask the shift size
		case BinOp.SHL:           return context.constants.add(leftCon.i64 << rightCon.i64, cast(IsSigned)left.isSignedConstant, argSize);
		case BinOp.SHR:
			ulong result;
			final switch(left.constantSize)
			{
				case IrArgSize.size8:  result = leftCon.i8 >>> rightCon.i64; break;
				case IrArgSize.size16: result = leftCon.i16 >>> rightCon.i64; break;
				case IrArgSize.size32: result = leftCon.i32 >>> rightCon.i64; break;
				case IrArgSize.size64: result = leftCon.i64 >>> rightCon.i64; break;
			}
			return context.constants.add(result, cast(IsSigned)left.isSignedConstant, argSize);
		case BinOp.ASHR:
			ulong result;
			final switch(left.constantSize)
			{
				case IrArgSize.size8:  result = leftCon.i8 >> rightCon.i64; break;
				case IrArgSize.size16: result = leftCon.i16 >> rightCon.i64; break;
				case IrArgSize.size32: result = leftCon.i32 >> rightCon.i64; break;
				case IrArgSize.size64: result = leftCon.i64 >> rightCon.i64; break;
			}
			return context.constants.add(result, cast(IsSigned)left.isSignedConstant, argSize);
		case BinOp.BITWISE_OR:    return context.constants.add(leftCon.i64 | rightCon.i64, cast(IsSigned)isAnySigned, argSize);
		case BinOp.BITWISE_AND:   return context.constants.add(leftCon.i64 & rightCon.i64, cast(IsSigned)isAnySigned, argSize);
		case BinOp.XOR:           return context.constants.add(leftCon.i64 ^ rightCon.i64, cast(IsSigned)isAnySigned, argSize);

		default:
			context.internal_error("Opcode `%s` is not implemented", op);
			assert(false);
	}
}

IrBinaryCondition convertBinOpToIrCond(BinOp op)
{
	assert(op >= BinOp.EQUAL && op <= BinOp.LESS_EQUAL);
	return cast(IrBinaryCondition)(op - BinOp.EQUAL);
}
