/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.visitor;

import all;

mixin template AstVisitorMixin() {
	void _visit(TypeNode* n) { _visit(cast(AstNode*)n); }
	void _visit(ExpressionNode* n) { _visit(cast(AstNode*)n); }
	void _visit(AstNode* n)
	{
		final switch(n.astType) with(AstType)
		{
			case error: context.internal_error(n.loc, "Visiting error node"); break;
			case abstract_node: context.internal_error(n.loc, "Visiting abstract node"); break;

			case decl_alias: visit(cast(AliasDeclNode*)n); break;
			case decl_builtin: break; // skip
			case decl_module: visit(cast(ModuleDeclNode*)n); break;
			case decl_import: visit(cast(ImportDeclNode*)n); break;
			case decl_function: visit(cast(FunctionDeclNode*)n); break;
			case decl_var: visit(cast(VariableDeclNode*)n); break;
			case decl_struct: visit(cast(StructDeclNode*)n); break;
			case decl_enum: visit(cast(EnumDeclaration*)n); break;
			case decl_enum_member: visit(cast(EnumMemberDecl*)n); break;
			case decl_static_if: visit(cast(StaticIfDeclNode*)n); break;
			case decl_template: visit(cast(TemplateDeclNode*)n); break;
			case decl_template_param: visit(cast(TemplateParamDeclNode*)n); break;

			case stmt_block: visit(cast(BlockStmtNode*)n); break;
			case stmt_if: visit(cast(IfStmtNode*)n); break;
			case stmt_while: visit(cast(WhileStmtNode*)n); break;
			case stmt_do_while: visit(cast(DoWhileStmtNode*)n); break;
			case stmt_for: visit(cast(ForStmtNode*)n); break;
			case stmt_switch: visit(cast(SwitchStmtNode*)n); break;
			case stmt_return: visit(cast(ReturnStmtNode*)n); break;
			case stmt_break: visit(cast(BreakStmtNode*)n); break;
			case stmt_continue: visit(cast(ContinueStmtNode*)n); break;

			case expr_name_use: visit(cast(NameUseExprNode*)n); break;
			case expr_member: visit(cast(MemberExprNode*)n); break;
			case expr_bin_op: visit(cast(BinaryExprNode*)n); break;
			case expr_un_op: visit(cast(UnaryExprNode*)n); break;
			case expr_call: visit(cast(CallExprNode*)n); break;
			case expr_index: visit(cast(IndexExprNode*)n); break;
			case expr_slice: visit(cast(SliceExprNode*)n); break;
			case expr_type_conv: visit(cast(TypeConvExprNode*)n); break;

			case literal_int: visit(cast(IntLiteralExprNode*)n); break;
			case literal_string: visit(cast(StringLiteralExprNode*)n); break;
			case literal_null: visit(cast(NullLiteralExprNode*)n); break;
			case literal_bool: visit(cast(BoolLiteralExprNode*)n); break;

			case type_basic: visit(cast(BasicTypeNode*)n); break;
			case type_func_sig: visit(cast(FunctionSignatureNode*)n); break;
			case type_ptr: visit(cast(PtrTypeNode*)n); break;
			case type_static_array: visit(cast(StaticArrayTypeNode*)n); break;
			case type_slice: visit(cast(SliceTypeNode*)n); break;
		}
	}
}

