/// Copyright: Copyright (c) 2021 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.ast_node_type;

enum AstType : ubyte
{
	error,
	abstract_node,

	decl_alias,
	decl_alias_array,
	decl_builtin,
	decl_builtin_attribute,
	decl_enum,
	decl_enum_member,
	decl_function,
	decl_import,
	decl_module,
	decl_package,
	decl_static_assert,
	decl_static_foreach,
	decl_static_if,
	decl_static_version,
	decl_struct,
	decl_template,
	decl_template_param,
	decl_var,

	stmt_block,
	stmt_if,
	stmt_while,
	stmt_do_while,
	stmt_for,
	stmt_switch,
	stmt_return,
	stmt_break,
	stmt_continue,

	expr_name_use,
	expr_member,
	expr_call,
	expr_index,
	expr_slice,
	expr_bin_op,
	expr_un_op,
	expr_type_conv,

	literal_int,
	literal_float,
	literal_string,
	literal_null,
	literal_bool,
	literal_array,
	literal_special,

	type_basic,
	type_ptr,
	type_static_array,
	type_slice,
	type_func_sig,
}
