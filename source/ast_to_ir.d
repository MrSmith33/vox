module ast_to_ir;

import std.stdio;
import compiler1;
import ir;
import utils;

void pass_new_ir_gen(ref CompilationContext ctx) {
	auto astToIr = AstToIr(&ctx);
	astToIr.visit(ctx.mod);
}

//version = IrGenPrint;

/// Converts AST to in-memory linear IR
struct AstToIr
{
	void visitType(TypeNode* n) {
		context.assertf(n.isType, n.loc, "Expected type, not %s", n.astType);
		switch(n.astType) with(AstType)
		{
			case type_basic: auto t = cast(BasicTypeNode*)n; visit(t); break;
			case type_ptr: auto t = cast(PtrTypeNode*)n; visit(t); break;
			case type_static_array: auto t = cast(StaticArrayTypeNode*)n; visit(t); break;
			case type_user: auto t = cast(UserTypeNode*)n; visit(t); break;

			default: context.unreachable(); assert(false);
		}
	}
	void visitExprValue(ExpressionNode* n, IrIndex currentBlock, ref IrLabel nextStmt) {
		context.assertf(n.isExpression, n.loc, "Expected expression, not %s", n.astType);
		switch(n.astType) with(AstType)
		{
			case expr_var: auto v = cast(VariableExprNode*)n; visitExprValue(v, currentBlock, nextStmt); break;
			case expr_literal: auto l = cast(LiteralExprNode*)n; visitExprValue(l, currentBlock, nextStmt); break;
			case expr_bin_op: auto b = cast(BinaryExprNode*)n; visitExprValue(b, currentBlock, nextStmt); break;
			case expr_call: auto c = cast(CallExprNode*)n; visitExprValue(c, currentBlock, nextStmt); break;
			case expr_index: auto i = cast(IndexExprNode*)n; visitExprValue(i, currentBlock, nextStmt); break;
			case expr_type_conv: auto t = cast(TypeConvExprNode*)n; visitExprValue(t, currentBlock, nextStmt); break;

			default: context.unreachable(); assert(false);
		}
	}
	void visitExprBranch(ExpressionNode* n, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit) {
		context.assertf(n.isExpression, n.loc, "Expected expression, not %s", n.astType);
		switch(n.astType) with(AstType)
		{
			case expr_var: auto v = cast(VariableExprNode*)n; visitExprBranch(v, currentBlock, trueExit, falseExit); break;
			case expr_literal: auto l = cast(LiteralExprNode*)n; visitExprBranch(l, currentBlock, trueExit, falseExit); break;
			case expr_bin_op: auto b = cast(BinaryExprNode*)n; visitExprBranch(b, currentBlock, trueExit, falseExit); break;
			case expr_call: auto c = cast(CallExprNode*)n; visitExprBranch(c, currentBlock, trueExit, falseExit); break;
			case expr_index: auto i = cast(IndexExprNode*)n; visitExprBranch(i, currentBlock, trueExit, falseExit); break;
			case expr_type_conv: auto t = cast(TypeConvExprNode*)n; visitExprBranch(t, currentBlock, trueExit, falseExit); break;

			default: context.unreachable(); assert(false);
		}
	}
	void visitDecl(AstNode* n, IrIndex currentBlock, ref IrLabel nextStmt) {
		context.assertf(n.isDeclaration, n.loc, "Expected declaration, not %s", n.astType);
		switch(n.astType) with(AstType)
		{
			case decl_function, decl_struct:
				// skip
				if (currentBlock.isDefined)
					builder.addJumpToLabel(currentBlock, nextStmt);
				break;

			case decl_var: auto v = cast(VariableDeclNode*)n; visit(v, currentBlock, nextStmt); break;

			default: context.unreachable(); assert(false);
		}

	}
	void visitStmt(AstNode* n, IrIndex currentBlock, ref IrLabel nextStmt) {
		if (n.isDeclaration)
		{
			visitDecl(n, currentBlock, nextStmt);
			return;
		}

		context.assertf(n.isStatement, n.loc, "Expected statement, not %s", n.astType);
		switch(n.astType) with(AstType)
		{
			case stmt_block: auto b = cast(BlockStmtNode*)n; visit(b, currentBlock, nextStmt); break;
			case stmt_if: auto i = cast(IfStmtNode*)n; visit(i, currentBlock, nextStmt); break;
			case stmt_while: auto w = cast(WhileStmtNode*)n; visit(w, currentBlock, nextStmt); break;
			case stmt_do_while: auto d = cast(DoWhileStmtNode*)n; visit(d, currentBlock, nextStmt); break;
			case stmt_return: auto r = cast(ReturnStmtNode*)n; visit(r, currentBlock, nextStmt); break;
			case stmt_break: auto b = cast(BreakStmtNode*)n; visit(b, currentBlock, nextStmt); break;
			case stmt_continue: auto c = cast(ContinueStmtNode*)n; visit(c, currentBlock, nextStmt); break;
			case stmt_assign: auto a = cast(AssignStmtNode*)n; visit(a, currentBlock, nextStmt); break;

			default: context.unreachable(); assert(false);
		}
	}

	CompilationContext* context;

	Identifier tempId;
	Identifier startId;
	Identifier thenId;
	Identifier elseId;
	Identifier blkId;

	IrBuilder builder;
	IrFunction* ir;

	void visit(ModuleDeclNode* m)
	{
		version(IrGenPrint) writeln("[IR GEN] module begin");

		tempId = context.idMap.getOrRegNoDup("__tmp");
		startId = context.idMap.getOrRegNoDup("start");
		thenId = context.idMap.getOrRegNoDup("then");
		elseId = context.idMap.getOrRegNoDup("else");
		blkId = context.idMap.getOrRegNoDup("blk");
		foreach (decl; m.functions) {
			visit(decl);
			// can be null if function is external
			if (decl.irData) m.irModule.addFunction(decl.irData);
		}
		version(IrGenPrint) writeln("[IR GEN] module end");
	}

	void visit(FunctionDeclNode* f)
	{
		version(IrGenPrint) writefln("[IR GEN] function (%s) begin", f.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] function (%s) end", f.loc);
		if (f.block_stmt is null) // external function
		{
			ExternalSymbol* sym = f.id in context.externalSymbols;
			if (sym is null)
			{
				context.error(f.loc,
					"Unresolved external function %s", f.strId(context));
				return;
			}
			f.funcPtr = sym.ptr;
			// TODO: check that parameters match
			return;
		}

		// create new function
		ir = new IrFunction;
		f.irData = ir;
		ir.returnType = IrValueType.i32;
		ir.name = f.id;

		version(IrGenPrint) writefln("[IR GEN] function 1");
		builder.begin(ir, context);

		version(IrGenPrint) writefln("[IR GEN] function parameters");
		foreach (i, param; f.parameters)
		{
			IrLabel dummy;
			visitDecl(cast(AstNode*)param, ir.entryBasicBlock, dummy);
		}

		builder.addJump(ir.entryBasicBlock);

		IrIndex body_block = builder.addBasicBlock();
		builder.addBlockTarget(ir.entryBasicBlock, body_block);
		builder.sealBlock(body_block);

		// label at the end of body
		IrLabel bodyExitLabel = IrLabel(body_block);

		version(IrGenPrint) writefln("[IR GEN] function body");
		// compile body
		visitStmt(cast(AstNode*)f.block_stmt, body_block, bodyExitLabel);

		IrIndex currentBlock = bodyExitLabel.blockIndex;
		// In case new block was created, no new predecessors will be added
		builder.sealBlock(currentBlock);

		version(IrGenPrint) writefln("[IR GEN] function return");
		if (ir.returnType != IrValueType.void_t)
		{
			// currentBlock must be finished with retVal
		}
		else
		{
			// currentBlock must be finished with ret or, not finished
		}

		version(IrGenPrint) writefln("[IR GEN] function seal exit");

		// all blocks with return (exit predecessors) already connected, seal exit block
		builder.sealBlock(ir.exitBasicBlock);
	}

	void store(IrIndex currentBlock, VariableDeclNode* v, IrIndex value)
	{
		//if (v.forceMemoryStorage)
		//{
		//	builder.addInstruction!IrStoreInstr(currentBlock, IrOpcode.store);
		//	builder.emitInstr2(IrOpcode.o_store, v.type.irType(context), v.stackSlotId, value);
		//}
		//else
		//{
			version(IrGenPrint) writefln("[IR GEN] store %s to %s", value, v.irVar.name);
			builder.writeVariable(currentBlock, v.irVar, value);
		//}
	}

	IrIndex load(IrIndex currentBlock, VariableDeclNode* v)
	{
		//if (v.forceMemoryStorage)
		//{
		//	return builder.emitInstr1(IrOpcode.o_load, v.type.irType(context), v.stackSlotId);
		//}
		//else
		//{
			auto result = builder.readVariable(currentBlock, v.irVar);
			version(IrGenPrint) writefln("[IR GEN] load %s from %s", result, v.irVar.name);
			return result;
		//}
	}

	void visit(VariableDeclNode* v, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(IrGenPrint) writefln("[IR GEN] Var decl (%s) begin %s", v.loc, v.strId(context));
		version(IrGenPrint) scope(success) writefln("[IR GEN] Var decl (%s) end %s", v.loc, v.strId(context));
		v.irVar = IrVar(v.id, builder.newIrVarId());//, v.type.irType(context));

		//if (context.buildDebug)
		//	v.varFlags |= VariableFlags.forceMemoryStorage;

		// Allocate stack slot for parameter that is passed via stack
		//bool isParamWithSlot = v.isParameter && ir.callingConvention.isParamOnStack(v.paramIndex);
		//bool needsStackSlot = v.forceMemoryStorage || isParamWithSlot;

		//if (needsStackSlot)
		//{
		//	v.stackSlotId = ir.stackLayout.addStackItem(v.type.size, v.type.alignment, v.isParameter, v.paramIndex);
		//}

		if (v.isParameter)
		{
			//++ir.numParameters;
			IrIndex paramIndex = builder.addInstruction!IrInstrParameter(ir.entryBasicBlock, IrOpcode.parameter);
			ir.get!IrInstrParameter(paramIndex).index = v.paramIndex;
			IrIndex paramValue = ir.get!IrInstrHeader(paramIndex).result;
			//instr.stackSlot = v.stackSlotId;

			builder.writeVariable(currentBlock, v.irVar, paramValue);
		}
		else
		{
			IrIndex value = context.addConstant(IrConstant(0));
			store(currentBlock, v, value);
		}
		builder.addJumpToLabel(currentBlock, nextStmt);
	}
	void visit(StructDeclNode* s)
	{
		// skip
	}
	void visit(BlockStmtNode* b, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(IrGenPrint) writefln("[IR GEN] block (%s) begin", b.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] block (%s) end", b.loc);
		foreach (i, AstNode* stmt; b.statements)
		{
			version(IrGenPrint) writefln("[IR GEN]   stmt %s/%s", i+1, b.statements.length);
			// if not the last statement of block
			if (i < b.statements.length - 1)
			{
				// nested statement will jump here at its end
				IrLabel afterStmt = IrLabel(currentBlock);

				// compile nested statement
				visitStmt(stmt, currentBlock, afterStmt);

				if (afterStmt.numPredecessors == 0)
				{
					version(IrGenPrint) writefln("[IR GEN]   no returns from stmt %s/%s, skipping the rest", i+1, b.statements.length);
					// Nested statement never returns here
					// Skip the rest of block statements
					break;
				}

				// If statement returned, get the new current block,
				// as it could have splitted the CFG and created a new block
				currentBlock = afterStmt.blockIndex;
				// Also seal it, since no other block can jump here
				builder.sealBlock(currentBlock);
			}
			else // last statement
			{
				// let last statement exit straight to outer scope
				visitStmt(stmt, currentBlock, nextStmt);

				// if statement hasn't returned here, let outer scope handle this
				// the body exit is handled by function decl code
			}
		}
	}

	void visit(IfStmtNode* i, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		if (i.elseStatement) // if then else
		{
			version(IrGenPrint) writefln("[IR GEN] if-else (%s) begin", i.loc);
			version(IrGenPrint) scope(success) writefln("[IR GEN] if-else (%s) end", i.loc);
			IrLabel trueLabel = IrLabel(currentBlock);
			IrLabel falseLabel = IrLabel(currentBlock);
			visitExprBranch(i.condition, currentBlock, trueLabel, falseLabel);

			if (trueLabel.numPredecessors != 0)
			{
				IrIndex thenBlock = trueLabel.blockIndex;
				builder.sealBlock(thenBlock);
				visitStmt(i.thenStatement, thenBlock, nextStmt);
			}
			else
				version(IrGenPrint) writeln("[IR GEN]   skip then stmt. Condition didn't jump here");

			if (falseLabel.numPredecessors != 0)
			{
				IrIndex elseBlock = falseLabel.blockIndex;
				builder.sealBlock(elseBlock);
				visitStmt(i.elseStatement, elseBlock, nextStmt);
			}
			else
				version(IrGenPrint) writeln("[IR GEN]   skip else stmt. Condition didn't jump here");
		}
		else // if then
		{
			version(IrGenPrint) writefln("[IR GEN] if (%s) begin", i.loc);
			version(IrGenPrint) scope(success) writefln("[IR GEN] if (%s) end", i.loc);
			IrLabel trueLabel = IrLabel(currentBlock);
			visitExprBranch(i.condition, currentBlock, trueLabel, nextStmt);

			if (trueLabel.numPredecessors != 0)
			{
				IrIndex thenBlock = trueLabel.blockIndex;
				builder.sealBlock(thenBlock);
				visitStmt(i.thenStatement, thenBlock, nextStmt);
			}
			else
				version(IrGenPrint) writeln("[IR GEN]   skip then stmt. Condition didn't jump here");
		}
	}
	void visit(WhileStmtNode* w, IrIndex currentBlock, ref IrLabel nextStmt) {

	}
	void visit(DoWhileStmtNode* d, IrIndex currentBlock, ref IrLabel nextStmt) {

	}
	void visit(ReturnStmtNode* r, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(IrGenPrint) writefln("[IR GEN] return (%s) begin", r.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] return (%s) end", r.loc);
		if (r.expression)
		{
			IrLabel afterExpr = IrLabel(currentBlock);
			visitExprValue(r.expression, currentBlock, afterExpr);
			currentBlock = afterExpr.blockIndex;
			builder.addReturn(currentBlock, cast(IrIndex)r.expression.irRef);
		}
		else builder.addReturn(currentBlock);
	}
	void visit(BreakStmtNode* b, IrIndex currentBlock, ref IrLabel nextStmt) {}
	void visit(ContinueStmtNode* c, IrIndex currentBlock, ref IrLabel nextStmt) {}
	void visit(AssignStmtNode* a, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(IrGenPrint) writefln("[IR GEN] assign (%s) begin", a.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] assign (%s) end", a.loc);
		IrLabel afterLeft = IrLabel(currentBlock);
		visitExprValue(a.left, currentBlock, afterLeft);
		currentBlock = afterLeft.blockIndex;
		IrLabel afterRight = IrLabel(currentBlock);
		visitExprValue(a.right, currentBlock, afterRight);
		currentBlock = afterRight.blockIndex;

		final switch (a.op)
		{
			case AssignOp.opAssign:
				auto varExpr = cast(VariableExprNode*)a.left;
				store(currentBlock, varExpr.getSym.varDecl, a.right.irRef);
				break;
			case AssignOp.opIndexAssign:
				//auto indexExpr = cast(IndexExprNode*)a.left;
				//builder.emitInstr2(IrOpcode.o_store, indexExpr.type.irType(context), indexExpr.irRef, a.right.irRef);
				assert(false);
				break;
		}
		builder.addJumpToLabel(currentBlock, nextStmt);
	}

	void visitExprValue(VariableExprNode* v, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(IrGenPrint) writefln("[IR GEN] var expr value (%s) begin", v.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] var expr value (%s) end", v.loc);
		if (!v.isLvalue)
		{
			v.irRef = load(currentBlock, v.getSym.varDecl);
		}
		builder.addJumpToLabel(currentBlock, nextStmt);
	}
	void visitExprBranch(VariableExprNode* v, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit)
	{
		version(IrGenPrint) writefln("[IR GEN] var expr branch (%s) begin", v.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] var expr branch (%s) end", v.loc);
		v.irRef = load(currentBlock, v.getSym.varDecl);
		placeUnaryBranch(v.irRef, currentBlock, trueExit, falseExit);
	}
	void visitExprValue(LiteralExprNode* c, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(IrGenPrint) writefln("[IR GEN] literal value (%s) value %s", c.loc, c.value);
		c.irRef = context.addConstant(IrConstant(c.value));
		builder.addJumpToLabel(currentBlock, nextStmt);
	}
	void visitExprBranch(LiteralExprNode* c, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit)
	{
		version(IrGenPrint) writefln("[IR GEN] literal branch (%s) value %s", c.loc, c.value);
		if (c.value)
			builder.addJumpToLabel(currentBlock, trueExit);
		else
			builder.addJumpToLabel(currentBlock, falseExit);
	}
	void placeUnaryBranch(IrIndex value, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit)
	{

	}
	void placeBinaryBranch(BinOp op, IrIndex left, IrIndex right, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit)
	{

	}

	long calcBinOp(BinOp op, long a, long b)
	{
		switch(op)
		{
			case BinOp.EQUAL_EQUAL:   return a == b;
			case BinOp.NOT_EQUAL:     return a != b;
			case BinOp.GREATER:       return a >  b;
			case BinOp.GREATER_EQUAL: return a >= b;
			case BinOp.LESS:          return a <  b;
			case BinOp.LESS_EQUAL:    return a <= b;
			case BinOp.PLUS:          return a +  b;
			case BinOp.MINUS:         return a -  b;
			default:
				context.internal_error("Opcode `%s` is not implemented", op);
				assert(false);
		}
	}

	static IrBinaryCondition convertBinOpToIrCond(BinOp op)
	{
		assert(op >= BinOp.EQUAL_EQUAL && op <= BinOp.LESS_EQUAL);
		return cast(IrBinaryCondition)(op - BinOp.EQUAL_EQUAL);
	}


	void visitBinOpImpl(bool forValue)(BinaryExprNode* b, ref IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit)
	{
		IrLabel afterLeft = IrLabel(currentBlock);
		visitExprValue(b.left, currentBlock, afterLeft);
		IrLabel afterRight = IrLabel(afterLeft.blockIndex);
		visitExprValue(b.right, currentBlock, afterRight);
		currentBlock = afterRight.blockIndex;

		// constant folding
		if (b.left.irRef.isConstant && b.right.irRef.isConstant)
		{
			long arg0 = context.getConstant(cast(IrIndex)b.left.irRef).i64;
			long arg1 = context.getConstant(cast(IrIndex)b.right.irRef).i64;
			long value = calcBinOp(b.op, arg0, arg1);
			static if (forValue)
			{
				b.irRef = context.addConstant(IrConstant(value));
			}
			else
			{
				if (value)
					builder.addJumpToLabel(currentBlock, trueExit);
				else
					builder.addJumpToLabel(currentBlock, falseExit);
			}
			version(IrGenPrint) writefln("[IR GEN]   const expr %s", value);
			return;
		}

		auto lRef = b.left.irRef;
		auto rRef = b.right.irRef;

		static if (forValue)
		{
			switch(b.op) with(BinOp)
			{
				case EQUAL_EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL:
					version(IrGenPrint) writefln("[IR GEN]   rel op value %s", b.op);
					b.irRef = builder.emitBinaryInstr(currentBlock, convertBinOpToIrCond(b.op), lRef, rRef);
					break;

				// TODO
				case PLUS: b.irRef = builder.emitBinaryInstr(currentBlock, IrOpcode.add, lRef, rRef); break;
				case MINUS: b.irRef = builder.emitBinaryInstr(currentBlock, IrOpcode.sub, lRef, rRef); break;

				default: context.internal_error(b.loc, "Opcode `%s` is not implemented", b.op); break;
			}
			builder.addJumpToLabel(currentBlock, trueExit);
		}
		else
		{
			switch(b.op) with(BinOp)
			{
				case EQUAL_EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL:
					version(IrGenPrint) writefln("[IR GEN]   rel op branch %s", b.op);
					auto branch = builder.addBinBranch(currentBlock, convertBinOpToIrCond(b.op), lRef, rRef, trueExit, falseExit);
					builder.addUser(branch, lRef);
					builder.addUser(branch, rRef);
					break;

				// TODO && || !
				default: context.internal_error(b.loc, "Opcode `%s` is not implemented", b.op); break;
			}
		}
	}

	void visitExprBranch(BinaryExprNode* b, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit)
	{
		version(IrGenPrint) writefln("[IR GEN] bin expr branch (%s) begin", b.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] bin expr branch (%s) end", b.loc);
		visitBinOpImpl!false(b, currentBlock, trueExit, falseExit);
	}

	void visitExprValue(BinaryExprNode* b, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(IrGenPrint) writefln("[IR GEN] bin expr value (%s) begin", b.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] bin expr value (%s) end", b.loc);
		IrLabel fake;
		visitBinOpImpl!true(b, currentBlock, nextStmt, fake);
		assert(fake.numPredecessors == 0);
	}

	void visitExprBranch(CallExprNode* c, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit)
	{
		version(IrGenPrint) writefln("[IR GEN] call branch (%s) begin", c.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] call branch (%s) begin", c.loc);
	}
	void visitExprValue(CallExprNode* c, IrIndex currentBlock, ref IrLabel nextStmt)
	{
		version(IrGenPrint) writefln("[IR GEN] call value (%s) begin", c.loc);
		version(IrGenPrint) scope(success) writefln("[IR GEN] call value (%s) begin", c.loc);
		builder.addJumpToLabel(currentBlock, nextStmt);
	}
	void visitExprBranch(IndexExprNode* i, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit) {
	}
	void visitExprValue(IndexExprNode* i, IrIndex currentBlock, ref IrLabel nextStmt) {
		builder.addJumpToLabel(currentBlock, nextStmt);
	}
	void visitExprBranch(TypeConvExprNode* t, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit) {
	}
	void visitExprValue(TypeConvExprNode* t, IrIndex currentBlock, ref IrLabel nextStmt) {
		builder.addJumpToLabel(currentBlock, nextStmt);
	}
	void visit(BasicTypeNode* t) {}
	void visit(PtrTypeNode* t) {}
	void visit(StaticArrayTypeNode* t) {}
	void visit(UserTypeNode* t) {}
}
