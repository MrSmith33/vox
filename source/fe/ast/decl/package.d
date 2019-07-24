/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl;

public import fe.ast.decl.builtin;
public import fe.ast.decl.enum_;
public import fe.ast.decl.func;
public import fe.ast.decl.import_;
public import fe.ast.decl.module_;
public import fe.ast.decl.scope_;
public import fe.ast.decl.struct_;
public import fe.ast.decl.var;

import fe.ast;

ModuleDeclNode* cast_decl_module(AstNode* n) { if (n.astType == AstType.decl_module) return cast(ModuleDeclNode*)n; return null; }
StructDeclNode* cast_decl_struct(AstNode* n) { if (n.astType == AstType.decl_struct) return cast(StructDeclNode*)n; return null; }
FunctionDeclNode* cast_decl_function(AstNode* n) { if (n.astType == AstType.decl_function) return cast(FunctionDeclNode*)n; return null; }
VariableDeclNode* cast_decl_var(AstNode* n) { if (n.astType == AstType.decl_var) return cast(VariableDeclNode*)n; return null; }
EnumDeclaration* cast_decl_enum(AstNode* n) { if (n.astType == AstType.decl_enum) return cast(EnumDeclaration*)n; return null; }
EnumMemberDecl* cast_decl_enum_member(AstNode* n) { if (n.astType == AstType.decl_enum_member) return cast(EnumMemberDecl*)n; return null; }
