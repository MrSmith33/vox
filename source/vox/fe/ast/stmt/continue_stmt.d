/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.stmt.continue_stmt;

import vox.all;


@(AstType.stmt_continue)
struct ContinueStmtNode {
	mixin AstNodeData!(AstType.stmt_continue, 0, AstNodeState.type_check_done);
}

void print_continue(ContinueStmtNode* node, ref AstPrintState state)
{
	state.print("CONTINUE");
}

void ir_gen_continue(ref IrGenState gen, IrIndex curBlock, ref IrLabel nextStmt, ContinueStmtNode* c)
{
	if (gen.currentLoopHeader is null) gen.context.unrecoverable_error(c.loc, "continue is not within the loop");
	gen.builder.addJumpToLabel(curBlock, *gen.currentLoopHeader);
}
