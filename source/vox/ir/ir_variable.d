/**
Copyright: Copyright (c) 2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// IR Variable info struct
module vox.ir.ir_variable;

import vox.all;

/// Allocated in temp storage during IR generation
@(IrValueKind.variable)
struct IrVariableInfo
{
	IrIndex type;
}
