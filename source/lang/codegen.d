/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lang.codegen;

import lang.ast;
import lang.semantics;

struct CodeGen
{
	import amd64asm;
	import utils;

	ubyte[] mem;
	ubyte[] code() { return gen.encoder.code; }

	void setup()
	{
		if (mem.length == 0) mem = alloc_executable_memory(4096 * 64);
		gen.encoder.setBuffer(mem);
	}

	// Stack structure
	// shadow space
	// return address
	// prev RBP <- RBP points here
	// local vars 0 - RBP - i * 8
	// local vars 1
	// local vars n <- RSP points here

	enum RET_REG = Register.AX;
	enum TEMP_REG_1 = Register.CX;
	enum TEMP_REG_2 = Register.DX;
	enum STACK_ITEM_SIZE = 8; // x86_64
	enum USE_FRAME_POINTER = true;
	CodeGen_x86_64 gen;

	static struct ReturnFixup {
		Fixup jmp_to_ret_fixup;
		PC jmp_pc_from;
	}

	private ReturnFixup[] returnFixups;

	private FunctionSemantics func;

	void* compileFunction(FunctionSemantics func)
	{
		this.func = func;
		void* funPtr = gen.pc;

		compileFuncProlog();
		compileFuncBody();
		compileFuncEpilog();

		return funPtr;
	}

	private int numParams;
	private int numLocals;
	private int numVars; // numLocals + numParams
	private int reservedBytes;

	void compileFuncProlog()
	{
		numParams = cast(int)func.node.parameters.length;
		numLocals = cast(int)(func.localVars.length - numParams);
		numVars = numLocals + numParams;

		// Copy parameters from registers to shadow space
		// TODO: check parameter type
		// parameter 5 RSP + 40
		if (numParams > 3) gen.movq(memAddrBaseDisp8(Register.SP, 32), Register.R9); // save fourth parameter
		if (numParams > 2) gen.movq(memAddrBaseDisp8(Register.SP, 24), Register.R8); // save third parameter
		if (numParams > 1) gen.movq(memAddrBaseDisp8(Register.SP, 16), Register.DX); // save second parameter
		if (numParams > 0) gen.movq(memAddrBaseDisp8(Register.SP,  8), Register.CX); // save first parameter
		// RSP + 0 points to RET

		// Establish frame pointer
		if (USE_FRAME_POINTER)
		{
			gen.pushq(Register.BP);
			gen.movq(Register.BP, Register.SP);
		}
		reservedBytes = cast(int)(numLocals * STACK_ITEM_SIZE);
		if (reservedBytes) // Reserve space for locals
		{
			if (reservedBytes > byte.max) gen.subq(Register.SP, Imm32(reservedBytes));
			else gen.subq(Register.SP, Imm8(cast(byte)reservedBytes));
		}
	}

	void compileFuncBody()
	{
		// init locals
		if (numLocals)
		{
			gen.xorq(RET_REG, RET_REG);
			foreach(i; 0..numLocals) gen.movq(varMemAddress(numParams+i), RET_REG);
		}

		returnFixups.length = 0;
		// body
		compileNode(func.node.statements);

		// fix jump to exit
		foreach(fixup; returnFixups) {
			fixup.jmp_to_ret_fixup.jmp(jumpOffset(fixup.jmp_pc_from, gen.pc));
		}
	}

	void compileFuncEpilog()
	{
		if (reservedBytes)
		{
			if (reservedBytes > byte.max) gen.addq(Register.SP, Imm32(reservedBytes));
			else gen.addq(Register.SP, Imm8(cast(byte)reservedBytes));
		}

		if (USE_FRAME_POINTER)
		{
			// Restore frame pointer
			gen.popq(Register.BP);
		}

		gen.ret();
	}

	MemAddress varMemAddress(int varIndex)
	{
		bool isParameter = varIndex < numParams;
		int displacement;
		Register baseReg;

		int index;
		if (USE_FRAME_POINTER)
		{
			// ++        varIndex
			// param2    1              \
			// param1    0  rbp + 2     / numParams = 2
			// ret addr     rbp + 1
			// rbp      <-- rbp + 0
			// local1    2  rbp - 1     \
			// local2    3  rbp - 2     / numLocals = 2
			// --
			if (isParameter) // parameter
			{
				index = 2 + varIndex;
			}
			else // local variable
			{
				index = -(varIndex - numParams + 1);
			}
			baseReg = Register.BP;
		}
		else
		{
			if (isParameter) // parameter
			{
				// Since return address is saved between locals and parameters, we need to add 1 to index for parameters
				index = numLocals + varIndex + 1;
			}
			else // local variable
			{
				// count from RSP, so last var has index of 0
				index = numVars - varIndex - 1;
			}
			baseReg = Register.SP;
		}
		displacement = index * STACK_ITEM_SIZE;
		if (displacement > byte.max) return memAddrBaseDisp32(baseReg, displacement);
		else return memAddrBaseDisp8(baseReg, cast(byte)displacement);
	}

	void compileNode(AstNode node)
	{
		// Statements
		if (auto b = cast(BlockStatement)node)
		{
			foreach(stmt; b.statements) compileNode(stmt);
		}
		else if (auto n = cast(IfStatement)node)
		{
			compileNode(n.condition); // paren_expr
				gen.testd(TEMP_REG_1, TEMP_REG_1);
				auto false_jump = gen.saveFixup();
				gen.jcc(Condition.Z, Imm32(0));
				auto nextInstrOff = gen.pc;
			compileNode(n.thenStatement);
				false_jump.jcc(Condition.Z, jumpOffset(nextInstrOff, gen.pc));
		}
		else if (auto n = cast(IfElseStatement)node) {
			compileNode(n.condition); // paren_expr
				gen.testd(TEMP_REG_1, TEMP_REG_1);
				auto else_jump_fix = gen.saveFixup();
				gen.jcc(Condition.Z, Imm32(0));
				auto else_pc = gen.pc;
			compileNode(n.thenStatement);
				auto end_jump = gen.saveFixup();
				gen.jmp(Imm32(0));
				auto then_pc = gen.pc;
				else_jump_fix.jcc(Condition.Z, jumpOffset(else_pc, gen.pc)); // fix cond -> else
			compileNode(n.elseStatement);
				end_jump.jmp(jumpOffset(then_pc, gen.pc)); // fix then -> end
		}
		else if (auto w = cast(WhileStatement)node)
		{
				auto condition_pc = gen.pc;
			compileNode(w.condition); // paren_expr
				gen.testd(TEMP_REG_1, TEMP_REG_1);
				auto break_fix = gen.saveFixup();
				gen.jcc(Condition.Z, Imm32(0)); // break
				auto break_pc = gen.pc;
			compileNode(w.statement); // statement
				auto continue_fix = gen.saveFixup();
				gen.jmp(Imm32(0));
				continue_fix.jmp(jumpOffset(gen.pc, condition_pc)); // continue
				break_fix.jcc(Condition.Z, jumpOffset(break_pc, gen.pc)); // fix cond -> end
		}
		else if (auto w = cast(DoWhileStatement)node)
		{
				auto do_pc = gen.pc;
			compileNode(w.statement); // do <statement>
			compileNode(w.condition); // while <paren_expr>
				gen.testd(TEMP_REG_1, TEMP_REG_1);
				auto continue_fix = gen.saveFixup();
				gen.jcc(Condition.NZ, Imm32(0));
				continue_fix.jcc(Condition.NZ, jumpOffset(gen.pc, do_pc)); // continue
		}
		else if (auto r = cast(ReturnStatement)node)
		{
			if (r.expression) {
				compileNode(r.expression);
				gen.movd(RET_REG, TEMP_REG_1);
			} else gen.movq(RET_REG, Imm32(0));

			auto return_fix = gen.saveFixup();
			gen.jmp(Imm32(0));
			returnFixups ~= ReturnFixup(return_fix, gen.pc);
		}
		else if (auto e = cast(ExpressionStatement)node)
		{
			compileNode(e.expression);
		}

		// Expressions
		else if (auto v = cast(VariableExpression)node)
		{
			gen.movd(TEMP_REG_1, varMemAddress(func.varIndex(v.id)));
		}
		else if (auto c = cast(ConstExpression)node)
		{
			gen.movd(TEMP_REG_1, Imm32(c.value));
		}
		else if (auto b = cast(BinaryExpression)node)
		{
			switch (b.op)
			{
				case BinOp.ADD   :
					compileNode(b.left); gen.pushq(TEMP_REG_1);
					compileNode(b.right); gen.popq(TEMP_REG_2);
					gen.addd(TEMP_REG_1, TEMP_REG_2);
					break;
				case BinOp.SUB   :
					compileNode(b.right); gen.pushq(TEMP_REG_1);
					compileNode(b.left); gen.popq(TEMP_REG_2);
					gen.subd(TEMP_REG_1, TEMP_REG_2);
					break;
				case BinOp.LT    :
					compileNode(b.left);
						gen.pushq(TEMP_REG_1);
					compileNode(b.right);
						gen.popq(TEMP_REG_2);
						gen.cmpd(TEMP_REG_2, TEMP_REG_1);
						gen.setcc(Condition.L, TEMP_REG_1);
						gen.movzx_btod(TEMP_REG_1, TEMP_REG_1);
					break;
				case BinOp.ASSIGN:
					compileNode(b.right);
					int varIndex = func.varIndex((cast(VariableExpression)b.left).id);
					gen.movd(varMemAddress(varIndex), TEMP_REG_1);
					break;
				default: assert(false, "Not implemented");
			}
		}
		else if (auto c = cast(CallExpression)node)
		{
			gen.movd(TEMP_REG_1, Imm32(42)); // TODO
		}
	}
}
