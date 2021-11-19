/// Copyright: Copyright (c) 2021 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Front-end
module fe.passes;

public import fe.passes.ast_to_ir;
public import fe.passes.dump;
public import fe.passes.eval;
public import fe.passes.lexer;
public import fe.passes.names_register;
public import fe.passes.names_resolve;
public import fe.passes.parser;
public import fe.passes.type_check;
