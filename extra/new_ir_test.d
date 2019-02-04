module ir_test;

import std.stdio;
import all;

void main()
{
	// function i32 $sign () {
	//    |  @start:0
	//   1|    %0 = i32 o_param
	//   2|    %1 = i1  o_icmp   l i32 %0, i64 0
	//         branch i1 %1 @then_1, @else_1
	//    |  @then_1:1 in[@start]
	//         jmp @blk_2
	//    |  @else_1:2 in[@start]
	//   7|    %2 = i1  o_icmp   g i32 %0, i64 0
	//         branch i1 %2 @then_2, @else_2
	//    |  @then_2:3 in[@else_1]
	//         jmp @blk_1
	//    |  @else_2:4 in[@else_1]
	//         jmp @blk_1
	//    |  @blk_1:5 in[@then_2, @else_2]
	//  13|    %4 = i32 phi.1(i64 1 @3, i64 0 @4)
	//         jmp @blk_2
	//    |  @blk_2:6 in[@then_1, @blk_1]
	//  15|    %3 = i32 phi.0(i32 -1 @1, i32 %4 @5)
	//         return i32 %3
	// }

	// i32 sign(i32 number) {
	//     i32 result;
	//     if (number < 0) result = 0-1;
	//     else if (number > 0) result = 1;
	//     else result = 0;
	//     return result;
	// }
	writefln("start");
	Driver driver;
	driver.initialize(null);
	scope(exit) driver.releaseMemory;

	IrBuilder builder;
	IrFunction ir;

	ir.returnType = IrValueType.i32;
	ir.name = driver.context.idMap.getOrReg("sign");

	//i32 sign(i32 number)
	builder.begin(&ir, &driver.context);
	InstrWithResult param0Instr = builder.emitInstr!IrInstr_parameter(ir.entryBasicBlock);
	ir.get!IrInstr_parameter(param0Instr.instruction).index = 0;
	builder.addJump(ir.entryBasicBlock);
	//{
	IrIndex start_block = builder.addBasicBlock();
	builder.addBlockTarget(ir.entryBasicBlock, start_block);
	builder.sealBlock(start_block);
	//	i32 result;
	IrIndex zeroVal = driver.context.constants.add(IrConstant(0));
	IrIndex resultVar = builder.newIrVarIndex();
	builder.writeVariable(start_block, resultVar, zeroVal);
	IrLabel scope1ExitLabel = IrLabel(start_block);
	IrIndex then_1_block = builder.addBasicBlock();
	IrIndex else_1_block = builder.addBasicBlock();
	//	if (number < 0)
	auto branch1 = builder.addBinBranch(start_block, IrBinaryCondition.l, param0Instr.result, zeroVal);

	builder.addBlockTarget(start_block, then_1_block);
	builder.sealBlock(then_1_block);
	builder.addBlockTarget(start_block, else_1_block);
	builder.sealBlock(else_1_block);
	//		result = 0-1;
	IrIndex minusOneVal = driver.context.constants.add(IrConstant(-1));
	builder.writeVariable(then_1_block, resultVar, minusOneVal);
	builder.addJumpToLabel(then_1_block, scope1ExitLabel);
	//	else
	//	{
	//		if (number > 0)
	auto branch2 = builder.addBinBranch(else_1_block, IrBinaryCondition.g, param0Instr.result, zeroVal);

	IrIndex then_2_block = builder.addBasicBlock();
	IrIndex else_2_block = builder.addBasicBlock();
	builder.addBlockTarget(else_1_block, then_2_block);
	builder.sealBlock(then_2_block);
	builder.addBlockTarget(else_1_block, else_2_block);
	builder.sealBlock(else_2_block);
	//			result = 1;
	IrIndex oneVal = driver.context.constants.add(IrConstant(1));
	builder.writeVariable(then_2_block, resultVar, oneVal);
	builder.addJumpToLabel(then_2_block, scope1ExitLabel);
	//		else
	//			result = 0;
	builder.writeVariable(else_2_block, resultVar, zeroVal);
	builder.addJumpToLabel(else_2_block, scope1ExitLabel);
	//	}
	IrIndex currentBlock = scope1ExitLabel.blockIndex;
	builder.sealBlock(currentBlock);
	//	return result;
	builder.addReturn(currentBlock, builder.readVariable(currentBlock, resultVar));
	//}

	builder.sealBlock(ir.exitBasicBlock);

	FuncDumpSettings dumpSettings;
	dumpSettings.dumper = &dumpIrInstr;
	dumpFunction(ir, driver.context, dumpSettings);
}
