/**
Copyright: Copyright (c) 2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// Back-end
module vox.be;

public:
import vox.be.abi;
import vox.be.amd64asm;
import vox.be.emit_mc_amd64;
import vox.be.ir_lower;
import vox.be.ir_to_lir_amd64;
import vox.be.link;
import vox.be.link_jit;
import vox.be.lir_amd64;
import vox.be.make_exe;
import vox.be.obj;
import vox.be.optimize;
import vox.be.pecoff : WindowsSubsystem;
import vox.be.reg_alloc;
import vox.be.stack_layout;
