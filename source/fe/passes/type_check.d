/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module fe.passes.type_check;

import std.stdio;
import std.string : format;
import all;

void pass_type_check(ref CompilationContext context, CompilePassPerModule[] subPasses)
{
	auto state = TypeCheckState(&context);

	foreach (ref SourceFileInfo file; context.files.data) {
		AstIndex modIndex = file.mod.get_ast_index(&context);
		require_type_check(modIndex, state);
		assert(context.analisysStack.length == 0);

		if (context.printAstSema && modIndex) {
			writefln("// AST typed `%s`", file.name);
			print_ast(context.getAstNodeIndex(file.mod), &context, 2);
		}
	}
}

// TODO: remove later, when no calls that pass IsNested.no remain.
enum IsNested : bool {
	no = false,
	yes = true,
}

struct TypeCheckState
{
	CompilationContext* context;
	AstIndex parentType;
}

/// Type checking for static context
void require_type_check(ref AstIndex nodeIndex, CompilationContext* context, IsNested isNested = IsNested.yes)
{
	auto state = TypeCheckState(context);
	require_type_check(nodeIndex, state, isNested);
}

// Assumes IsNested.yes
void require_type_check(ref AstNodes items, ref TypeCheckState state, IsNested isNested = IsNested.yes)
{
	foreach(ref AstIndex item; items) require_type_check(item, state, isNested);
}

/// Annotates all expression nodes with their type
/// Type checking, casting
/// isNested:
///   If true, then type check must be performed. If check is already in progress then it is an error.
///   Indirect check requests over expr_name_use, expr_member pass false.
void require_type_check(ref AstIndex nodeIndex, ref TypeCheckState state, IsNested isNested = IsNested.yes)
{
	AstNode* node = state.context.getAstNode(nodeIndex);
	//writefln("require_type_check %s %s %s", node.astType, node.state, isNested);

	switch(node.state) with(AstNodeState)
	{
		case name_register_self, name_register_nested, name_resolve:
			state.context.push_analized_node(AnalysedNode(nodeIndex, NodeProperty.type_check));
			state.context.circular_dependency;
		case type_check:
			if (isNested == IsNested.no) {
				// this is allowed. We simply return.
				return;
			}
			state.context.push_analized_node(AnalysedNode(nodeIndex, NodeProperty.type_check));
			state.context.circular_dependency;
		case parse_done:
			auto name_state = NameRegisterState(state.context);
			require_name_register_self(0, nodeIndex, name_state);
			state.context.throwOnErrors;
			goto case;
		case name_register_self_done:
			auto name_state = NameRegisterState(state.context);
			require_name_register(nodeIndex, name_state);
			state.context.throwOnErrors;
			goto case;
		case name_register_nested_done:
			require_name_resolve(nodeIndex, state.context);
			state.context.throwOnErrors;
			break;
		case name_resolve_done: break; // all requirement are done
		case type_check_done, ir_gen_done: return; // already type checked
		default: state.context.internal_error(node.loc, "Node %s in %s state", node.astType, node.state);
	}

	state.context.push_analized_node(AnalysedNode(nodeIndex, NodeProperty.type_check));
	scope(success) state.context.pop_analized_node;

	if (node.hasAttributes) {
		type_check_attributes(node.attributeInfo, state);
	}

	final switch(node.astType) with(AstType)
	{
		case error: state.context.internal_error(node.loc, "Visiting error node");
		case abstract_node: state.context.internal_error(node.loc, "Visiting abstract node");

		case decl_alias: type_check_alias(cast(AliasDeclNode*)node, state); break;
		case decl_alias_array: assert(false);
		case decl_builtin: assert(false);
		case decl_builtin_attribute: type_check_builtin_attribute(cast(BuiltinAttribNode*)node, state); break;
		case decl_module: type_check_module(cast(ModuleDeclNode*)node, state); break;
		case decl_package: assert(false);
		case decl_import: assert(false);
		case decl_function: type_check_func(cast(FunctionDeclNode*)node, state); break;
		case decl_var: type_check_var(cast(VariableDeclNode*)node, state); break;
		case decl_struct: type_check_struct(cast(StructDeclNode*)node, state); break;
		case decl_enum: type_check_enum(cast(EnumDeclaration*)node, state); break;
		case decl_enum_member: type_check_enum_member(cast(EnumMemberDecl*)node, state); break;
		case decl_static_assert: type_check_static_assert(cast(StaticAssertDeclNode*)node, state); break;
		case decl_static_foreach: assert(false);
		case decl_static_if: assert(false);
		case decl_static_version: assert(false);
		case decl_template: assert(false);
		case decl_template_param: assert(false);

		case stmt_block: type_check_block(cast(BlockStmtNode*)node, state); break;
		case stmt_if: type_check_if(cast(IfStmtNode*)node, state); break;
		case stmt_while: type_check_while(cast(WhileStmtNode*)node, state); break;
		case stmt_do_while: type_check_do(cast(DoWhileStmtNode*)node, state); break;
		case stmt_for: type_check_for(cast(ForStmtNode*)node, state); break;
		case stmt_switch: type_check_switch(cast(SwitchStmtNode*)node, state); break;
		case stmt_return: type_check_return(cast(ReturnStmtNode*)node, state); break;
		case stmt_break: assert(false);
		case stmt_continue: assert(false);

		case expr_name_use: type_check_name_use(nodeIndex, cast(NameUseExprNode*)node, state); break;
		case expr_member: type_check_member(nodeIndex, cast(MemberExprNode*)node, state); break;
		case expr_bin_op: type_check_binary_op(cast(BinaryExprNode*)node, state); break;
		case expr_un_op: type_check_unary_op(cast(UnaryExprNode*)node, state); break;
		case expr_call: type_check_call(nodeIndex, cast(CallExprNode*)node, state); break;
		case expr_index: type_check_index(nodeIndex, cast(IndexExprNode*)node, state); break;
		case expr_slice: type_check_expr_slice(cast(SliceExprNode*)node, state); break;
		case expr_type_conv: type_check_type_conv(cast(TypeConvExprNode*)node, state); break;

		case literal_int: type_check_literal_int(cast(IntLiteralExprNode*)node, state); break;
		case literal_float: type_check_literal_float(cast(FloatLiteralExprNode*)node, state); break;
		case literal_string: type_check_literal_string(cast(StringLiteralExprNode*)node, state); break;
		case literal_null: type_check_literal_null(cast(NullLiteralExprNode*)node, state); break;
		case literal_bool: type_check_literal_bool(cast(BoolLiteralExprNode*)node, state); break;
		case literal_array: type_check_literal_array(cast(ArrayLiteralExprNode*)node, state); break;

		case type_basic: assert(false);
		case type_func_sig: type_check_func_sig(cast(FunctionSignatureNode*)node, state); break;
		case type_ptr: type_check_ptr(cast(PtrTypeNode*)node, state); break;
		case type_static_array: type_check_static_array(cast(StaticArrayTypeNode*)node, state); break;
		case type_slice: type_check_slice(cast(SliceTypeNode*)node, state); break;
	}
}

void require_type_check_expr(AstIndex targetType, ref AstIndex nodeIndex, CompilationContext* context)
{
	auto state = TypeCheckState(context);
	state.parentType = targetType;
	require_type_check(nodeIndex, state);
}

void require_type_check_expr(AstIndex targetType, ref AstIndex nodeIndex, ref TypeCheckState state)
{
	auto temp = state.parentType;
	state.parentType = targetType;
	require_type_check(nodeIndex, state);
	state.parentType = temp;
}

TypeConvResKind checkTypeConversion(AstIndex fromTypeIndex, AstIndex toTypeIndex, ref AstIndex expr, CompilationContext* c)
{
	if (same_type(fromTypeIndex, toTypeIndex, c)) return TypeConvResKind.no_i;

	if (fromTypeIndex == CommonAstNodes.type_error || toTypeIndex == CommonAstNodes.type_error)
		return TypeConvResKind.no_i;

	TypeNode* fromType = fromTypeIndex.get_type(c);
	TypeNode* toType = toTypeIndex.get_type(c);

	//writefln("checkTypeConversion %s -> %s", printer(fromTypeIndex, c), printer(toTypeIndex, c));
	//writefln("checkTypeConversion %s -> %s", printer(c.getAstNodeIndex(fromType), c), printer(c.getAstNodeIndex(toType), c));
	//writefln("checkTypeConversion %s", fromType.astType);

	switch (fromType.astType) with(AstType) {
		case type_basic: return type_conv_basic(fromType.as_basic, toTypeIndex, expr, c);
		case type_ptr: return type_conv_ptr(fromType.as_ptr, toTypeIndex, expr, c);
		case type_slice: return type_conv_slice(fromType.as_slice, toTypeIndex, expr, c);
		case type_static_array: return type_conv_static_array(fromType.as_static_array, toTypeIndex, expr, c);
		case decl_enum: return checkTypeConversion(fromType.as_enum.memberType, toTypeIndex, expr, c);
		case type_func_sig: return type_conv_func_sig(fromType.as_func_sig, toTypeIndex, expr, c);
		case decl_struct: return type_conv_struct(fromType.as_struct, toTypeIndex, expr, c);
		default:
			c.internal_error(expr.loc(c), "Unhandled type conversion %s, %s", cast(AstType)fromTypeIndex.astType(c), toType.astType);
	}
}

struct CommonTypeResult {
	AstIndex commonType;
	TypeConvResKind kindA;
	TypeConvResKind kindB;
}

CommonTypeResult calcCommonType(AstIndex indexA, AstIndex indexB, CompilationContext* c)
	out(res; res.commonType.isDefined)
{
	if (same_type(indexA, indexB, c)) return CommonTypeResult(indexA, TypeConvResKind.no_i, TypeConvResKind.no_i);

	TypeNode* typeA = indexA.get_type(c);
	TypeNode* typeB = indexB.get_type(c);

	if (typeB.astType == AstType.decl_enum) {
		indexB = typeB.as_enum.memberType;
		typeB = indexB.get_type(c);
	}

	restart_enum:

	switch (typeA.astType) with(AstType) {
		case type_basic: return common_type_basic(typeA.as_basic, indexB, c);
		case type_slice: return CommonTypeResult(CommonAstNodes.type_error);
		case type_ptr: return common_type_ptr(typeA.as_ptr, indexB, c);
		case decl_enum:
			indexA = typeA.as_enum.memberType;
			typeA = indexA.get_type(c);
			goto restart_enum;
		default:
			return CommonTypeResult(CommonAstNodes.type_error);
	}
}

void autoconvToBool(ref AstIndex exprIndex, CompilationContext* context)
{
	ExpressionNode* expr = exprIndex.get_expr(context);
	if (expr.type.isErrorType) return;
	if (!autoconvTo(exprIndex, CommonAstNodes.type_bool, context))
		context.error(expr.loc, "Cannot implicitly convert `%s` to bool",
			expr.type.typeName(context));
}

void insertCast(ref AstIndex exprIndex, AstIndex typeIndex, TypeConvResKind kind, CompilationContext* c)
{
	//writefln("cast %s", kind);
	final switch(kind) with(TypeConvResKind)
	{
		case fail: assert(false);
		case no_e, no_i: return;
		case ii_e, ii_i, if_e, if_i, ff_e, ff_i, fi_e, fi_i, string_literal_to_u8_ptr:
			exprIndex = c.appendAst!TypeConvExprNode(exprIndex.loc(c), typeIndex, exprIndex);
			exprIndex.setState(c, AstNodeState.type_check_done);
			exprIndex.get_node(c).subType = kind;
			return;
		case override_expr_type_e, override_expr_type_i:
			exprIndex.get_expr(c).type = typeIndex;
			return;
		case array_literal_to_slice:
			ExpressionNode* expr = exprIndex.get_expr(c);
			exprIndex = c.appendAst!UnaryExprNode(expr.loc, typeIndex, UnOp.staticArrayToSlice, exprIndex);
			return;
	}
}

bool autoconvToCommonType(ref AstIndex leftIndex, ref AstIndex rightIndex, CompilationContext* c)
{
	AstIndex leftType = leftIndex.get_expr_type(c);
	AstIndex rightType = rightIndex.get_expr_type(c);
	CommonTypeResult res = calcCommonType(leftType, rightType, c);
	//writefln("autoconvToCommonType %s %s %s", printer(leftType, c), printer(rightType, c), res);
	if (res.commonType == CommonAstNodes.type_error) return false;
	insertCast(leftIndex, res.commonType, res.kindA, c);
	insertCast(rightIndex, res.commonType, res.kindB, c);
	return true;
}

/// Returns true if conversion was successful. False otherwise
bool autoconvTo(ref AstIndex exprIndex, AstIndex typeIndex, CompilationContext* c)
{
	AstIndex exprType = exprIndex.get_expr_type(c);
	//writefln("autoconvTo %s -> %s", printer(exprType, c), printer(typeIndex, c));
	if (exprType == typeIndex) return true;

	TypeConvResKind res = checkTypeConversion(exprType, typeIndex, exprIndex, c);
	//writefln("autoconvTo %s %s %s", printer(exprType, c), printer(typeIndex, c), res);
	if (res.canConvertImplicitly)
	{
		insertCast(exprIndex, typeIndex, res, c);
		return true;
	}
	return false;
}
