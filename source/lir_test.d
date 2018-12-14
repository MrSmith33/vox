module lir_test;

import all;
import amd64asm;
import lir_amd64;

void main()
{
	// i32 sign(i32 number) {
	//     i32 result;
	//     if (number < 0) result = 0-1;
	//     else if (number > 0) result = 1;
	//     else result = 0;
	//     return result;
	// }

	// function sign() {
	//   @0
	//     v43 = parameter0 users [i62, i93]
	//     jmp @1
	//   @1
	//     if v43 < c1 then @2 else @3
	//   @2
	//     jmp @6
	//   @3
	//     if v43 > c5 then @4 else @5
	//   @4
	//     jmp @6
	//   @5
	//     jmp @6
	//   @6
	//     v157 = phi152(c7 @5, c6 @4, c4 @2) users [i26]
	//     jmp @7
	//   @7
	//     return v157
	// }

	// function sign() {
	//   @0
	//     mov vreg.0, ecx
	//     jmp @1
	//   @1
	//     cmp v43, c1
	//     jl @2
	//     jmp @3
	//   @2
	//     jmp @6
	//   @3
	//     cmp v43, c5
	//     jg @4
	//     jmp @5
	//   @4
	//     jmp @6
	//   @5
	//     jmp @6
	//   @6
	//     v157 = phi152(c7 @5, c6 @4, c4 @2)
	//     jmp @7
	//   @7
	//     return v157
	// }

	writefln("Lir example");
	Driver driver;
	driver.initialize(null);
	scope(exit) driver.releaseMemory;

	IrBuilder builder;
	IrFunction lir;

	builder.context = &driver.context;
	builder.ir = &lir;
	lir.name = driver.context.idMap.getOrReg("sign");

	lir.storage = driver.context.irBuffer.freePart;
	builder.setupEntryExitBlocks;

	IrIndex block1 = builder.addBasicBlock();
	IrIndex block2 = builder.addBasicBlock();
	IrIndex block3 = builder.addBasicBlock();
	IrIndex block4 = builder.addBasicBlock();
	IrIndex block5 = builder.addBasicBlock();
	IrIndex block6 = builder.addBasicBlock();

	builder.addBlockTarget(lir.entryBasicBlock, block1);
	builder.addBlockTarget(block1, block2);
	builder.addBlockTarget(block1, block3);
	builder.addBlockTarget(block2, block6);
	builder.addBlockTarget(block3, block4);
	builder.addBlockTarget(block3, block5);
	builder.addBlockTarget(block4, block6);
	builder.addBlockTarget(block5, block6);
	builder.addBlockTarget(block6, lir.exitBasicBlock);

	IrIndex const_0 = driver.context.addConstant(IrConstant(0));
	IrIndex const_1 = driver.context.addConstant(IrConstant(1));
	IrIndex const_m1 = driver.context.addConstant(IrConstant(-1));

	//     mov vreg.0, ecx
	IrIndex param0Value = builder.emitInstr!LirAmd64Instr_mov(lir.entryBasicBlock, amd64_reg.ax).result;
	//     jmp @1
	builder.emitInstr!LirAmd64Instr_jmp(lir.entryBasicBlock);
	//   @1
	//     cmp v43, c1
	//     jl @2
	builder.emitInstr!LirAmd64Instr_bin_branch(block1, IrBinaryCondition.l, param0Value, const_0);
	//     jmp @3
	builder.emitInstr!LirAmd64Instr_jmp(block1);
	//   @2
	//     jmp @6
	builder.emitInstr!LirAmd64Instr_jmp(block2);
	//   @3
	//     cmp v43, c5
	//     jg @4
	builder.emitInstr!LirAmd64Instr_bin_branch(block3, IrBinaryCondition.g, param0Value, const_0);
	//     jmp @5
	builder.emitInstr!LirAmd64Instr_jmp(block3);
	//   @4
	//     jmp @6
	builder.emitInstr!LirAmd64Instr_jmp(block4);
	//   @5
	//     jmp @6
	builder.emitInstr!LirAmd64Instr_jmp(block5);
	//   @6
	//     v157 = phi152(c7 @5, c6 @4, c4 @2)
	IrIndex phi = builder.addPhi(block6);
	IrIndex phiValue = lir.getPhi(phi).result;
	builder.addPhiArg(phi, block5, const_0);
	builder.addPhiArg(phi, block4, const_1);
	builder.addPhiArg(phi, block2, const_m1);
	builder.addUser(phi, const_0);
	builder.addUser(phi, const_1);
	builder.addUser(phi, const_m1);
	//     jmp @7
	builder.emitInstr!LirAmd64Instr_jmp(block6);
	//   @7
	//     return v157
	builder.emitInstr!LirAmd64Instr_mov(lir.exitBasicBlock, amd64_reg.ax, [phiValue]);
	builder.emitInstr!LirAmd64Instr_return(lir.exitBasicBlock);

	validateIrFunction(driver.context, lir);


	FuncDumpSettings dumpSettings;
	dumpSettings.dumper = &dumpAmd64Instr;
	dumpFunction(lir, driver.context, dumpSettings);
}
