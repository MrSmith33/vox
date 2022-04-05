/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module vox.fe.passes.dump;

import std.stdio;

import vox.all;

struct AstPrintState
{
	CompilationContext* context;

	int indentSize = 1;
	private int indent;

	void print(Args...)(Args args) {
		import std.range : repeat;
		write(' '.repeat(indent)); // indent
		writeln(args);
	}
}

void print_ast(AstIndex nodeIndex, CompilationContext* context, int indentSize = 1)
{
	auto state = AstPrintState(context, indentSize);
	state.indent = -state.indentSize;
	print_ast(nodeIndex, state);
}

void print_ast(AstNodes nodes, ref AstPrintState state)
{
	foreach(AstIndex item; nodes) print_ast(item, state);
}

void print_ast(AstIndex nodeIndex, ref AstPrintState state)
{
	if (nodeIndex.isUndefined) return;

	AstNode* node = state.context.getAstNode(nodeIndex);
	if (node.hasAttributes) {
		print_attributes(node.attributeInfo, state);
	}

	state.indent += state.indentSize;
	scope(exit) state.indent -= state.indentSize;

	final switch(node.astType) with(AstType)
	{
		case error: state.context.internal_error(node.loc, "Visiting error node");
		case abstract_node: state.context.internal_error(node.loc, "Visiting abstract node");

		case decl_alias: print_alias(cast(AliasDeclNode*)node, state); break;
		case decl_alias_array: print_alias_array(cast(AliasArrayDeclNode*)node, state); break;
		case decl_builtin: break; // skip
		case decl_builtin_attribute: print_builtin_attribute(cast(BuiltinAttribNode*)node, state); break;
		case decl_enum: print_enum(cast(EnumDeclaration*)node, state); break;
		case decl_enum_member: print_enum_member(cast(EnumMemberDecl*)node, state); break;
		case decl_function: print_func(cast(FunctionDeclNode*)node, state); break;
		case decl_import: print_import(cast(ImportDeclNode*)node, state); break;
		case decl_module: print_module(cast(ModuleDeclNode*)node, state); break;
		case decl_package: print_package(cast(PackageDeclNode*)node, state); break;
		case decl_static_assert: print_static_assert(cast(StaticAssertDeclNode*)node, state); break;
		case decl_static_foreach: print_static_foreach(cast(StaticForeachDeclNode*)node, state); break;
		case decl_static_if: print_static_if(cast(StaticIfDeclNode*)node, state); break;
		case decl_static_version: print_static_version(cast(StaticVersionDeclNode*)node, state); break;
		case decl_struct: print_struct(cast(StructDeclNode*)node, state); break;
		case decl_template: print_template(cast(TemplateDeclNode*)node, state); break;
		case decl_template_param: print_template_param(cast(TemplateParamDeclNode*)node, state); break;
		case decl_var: print_var(cast(VariableDeclNode*)node, state); break;

		case stmt_block: print_block(cast(BlockStmtNode*)node, state); break;
		case stmt_if: print_if(cast(IfStmtNode*)node, state); break;
		case stmt_while: print_while(cast(WhileStmtNode*)node, state); break;
		case stmt_do_while: print_do(cast(DoWhileStmtNode*)node, state); break;
		case stmt_for: print_for(cast(ForStmtNode*)node, state); break;
		case stmt_switch: print_switch(cast(SwitchStmtNode*)node, state); break;
		case stmt_return: print_return(cast(ReturnStmtNode*)node, state); break;
		case stmt_break: print_break(cast(BreakStmtNode*)node, state); break;
		case stmt_continue: print_continue(cast(ContinueStmtNode*)node, state); break;

		case expr_name_use: print_name_use(cast(NameUseExprNode*)node, state); break;
		case expr_member: print_member(cast(MemberExprNode*)node, state); break;
		case expr_bin_op: print_binary_op(cast(BinaryExprNode*)node, state); break;
		case expr_un_op: print_unary_op(cast(UnaryExprNode*)node, state); break;
		case expr_call: print_call(cast(CallExprNode*)node, state); break;
		case expr_named_argument: print_named_argument(cast(NamedArgumenExprNode*)node, state); break;
		case expr_index: print_index(cast(IndexExprNode*)node, state); break;
		case expr_slice: print_expr_slice(cast(SliceExprNode*)node, state); break;
		case expr_type_conv: print_type_conv(cast(TypeConvExprNode*)node, state); break;

		case literal_int: print_literal_int(cast(IntLiteralExprNode*)node, state); break;
		case literal_float: print_literal_float(cast(FloatLiteralExprNode*)node, state); break;
		case literal_string: print_literal_string(cast(StringLiteralExprNode*)node, state); break;
		case literal_null: print_literal_null(cast(NullLiteralExprNode*)node, state); break;
		case literal_bool: print_literal_bool(cast(BoolLiteralExprNode*)node, state); break;
		case literal_array: print_literal_array(cast(ArrayLiteralExprNode*)node, state); break;
		case literal_special: print_literal_special(cast(SpecialLiteralExprNode*)node, state); break;

		case type_basic: print_type_basic(cast(BasicTypeNode*)node, state); break;
		case type_func_sig: print_func_sig(cast(FunctionSignatureNode*)node, state); break;
		case type_ptr: print_ptr(cast(PtrTypeNode*)node, state); break;
		case type_static_array: print_static_array(cast(StaticArrayTypeNode*)node, state); break;
		case type_slice: print_slice(cast(SliceTypeNode*)node, state); break;
	}
}
