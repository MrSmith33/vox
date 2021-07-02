/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.stmt.break_stmt;

import all;


@(AstType.stmt_break)
struct BreakStmtNode {
	mixin AstNodeData!(AstType.stmt_break, 0, AstNodeState.type_check_done);
}

void print_break(BreakStmtNode* node, ref AstPrintState state)
{
	state.print("BREAK");
}

void ir_gen_break(ref IrGenState gen, IrIndex curBlock, ref IrLabel nextStmt, BreakStmtNode* b)
{
	if (gen.currentLoopEnd is null) gen.context.unrecoverable_error(b.loc, "break is not within the loop");
	gen.builder.addJumpToLabel(curBlock, *gen.currentLoopEnd);
}
