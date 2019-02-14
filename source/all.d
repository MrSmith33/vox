/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module all;

public:
import ast;
import ast_to_ir;
import backend;
import basictype;
import context;
import driver;
import emit_mc_amd64;
import identifier;
import ir;
import ir_to_lir_amd64;
import lexer;
import lir_amd64;
import liveness;
import optimize;
import parser;
import pecoff : WindowsSubsystem;
import register_allocation;
import semantics;
import stack_layout;
import symbol;
import utils;
