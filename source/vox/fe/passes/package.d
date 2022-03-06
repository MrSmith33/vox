/// Copyright: Copyright (c) 2021 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Front-end
module vox.fe.passes;

public import vox.fe.passes.ast_to_ir;
public import vox.fe.passes.dump;
public import vox.fe.passes.eval;
public import vox.fe.passes.lexer;
public import vox.fe.passes.names_register;
public import vox.fe.passes.names_resolve;
public import vox.fe.passes.parser;
public import vox.fe.passes.read_source;
public import vox.fe.passes.type_check;
