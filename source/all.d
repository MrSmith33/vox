/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module all;

public:
import ast;
import ast_to_ir;
import compiler1;
import context;
import driver;
import identifier;
import ir;
import ir_to_lir_amd64;
import lir_amd64;
import parser;
import semantics;
import symbol;
import type;
import utils;
