/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lang.codegen;

import lang.ast;
import lang.semantics;

alias JittedFunc = int function();

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
	// return address
	// prev RBP <- RBP points here
	// local vars 0 - RBP - i * 8
	// local vars 1
	// local vars n <- RSP points here
	// temp <- RSP temporarily points here
	// temp <- RSP temporarily points here

	enum LOCALS_REG = Register.BP;
	enum STACK_PTR_REG = Register.SP;

	enum RET_REG = Register.AX;
	enum TEMP_REG_1 = Register.CX;
	enum TEMP_REG_2 = Register.DX;
	enum STACK_ITEM_SIZE = 8; // x86_64
	CodeGen_x86_64 gen;

	static struct ReturnFixup {
		Fixup jmp_to_ret_fixup;
		PC jmp_pc_from;
	}

	private ReturnFixup[] returnFixups;

	private FunctionSemantics funcSemantics;

	JittedFunc compileFunction(FunctionSemantics func)
	{
		returnFixups.length = 0;

		funcSemantics = func;

		void* funPtr = gen.pc;

		// Prologue
		gen.pushq(Register.BP);
		gen.movq(Register.BP, Register.SP);
		int reservedBytes = cast(int)(func.localVars.length * STACK_ITEM_SIZE);

		if (func.localVars.length)
		{
			//gen.subq(STACK_PTR_REG, Imm32(reservedBytes)); // Reserve space for locals
			gen.movq(RET_REG, Imm32(0));
			//gen.movq(memAddrBaseDisp32(LOCALS_REG, Imm32()), RET_REG);
			foreach(_; 0..func.localVars.length) gen.pushq(RET_REG);
		}

		// body
		compileNode(func.node.statements);

		// fix jump to exit
		foreach(fixup; returnFixups) {
			fixup.jmp_to_ret_fixup.jmp(jumpOffset(fixup.jmp_pc_from, gen.pc));
		}

		// Epilogue
		if (func.localVars.length) gen.addq(STACK_PTR_REG, Imm32(reservedBytes));
		gen.popq(Register.BP);
		gen.ret();

		return cast(JittedFunc)funPtr;
	}

	MemAddress varMemAddress(VariableExpression var)
	{
		assert(var);
		int displacement = -(funcSemantics.varIndex(var.id)+1) * STACK_ITEM_SIZE;
		return memAddrBaseDisp32(LOCALS_REG, displacement);
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
			gen.movd(TEMP_REG_1, varMemAddress(v));
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
					gen.movd(varMemAddress(cast(VariableExpression)b.left), TEMP_REG_1);
					break;
				default: assert(false, "Not implemented");
			}
		}
	}
}
