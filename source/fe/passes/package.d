/// Copyright: Copyright (c) 2021 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Front-end
module fe.passes;

public:
import fe.passes.ast_to_ir;
import fe.passes.dump;
import fe.passes.eval;
import fe.passes.lexer;
import fe.passes.names_register;
import fe.passes.names_resolve;
import fe.passes.parser;
import fe.passes.type_check;
