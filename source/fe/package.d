/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// Front-end
module fe;

public:
import fe.ast;
import fe.ast_to_ir;
import fe.basictype;
import fe.eval;
import fe.identifier;
import fe.names_register;
import fe.names_resolve;
import fe.type_check;
