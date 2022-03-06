/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.fe.ast.expr.literal;

import vox.all;

@(AstType.literal_int)
struct IntLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_int, 0, AstNodeState.name_resolve_done);
	ulong value;

	private enum Flags : ushort
	{
		isNegative = AstFlags.userFlag
	}
	bool isNegative() { return cast(bool)(flags & Flags.isNegative); }
	IsSigned isSigned() { return cast(IsSigned)isNegative; }

	void negate(TokenIndex pos, ref CompilationContext context) {
		if (isNegative) {
			value = -(cast(long)value);
			flags &= ~cast(int)Flags.isNegative;
		} else {
			if (value <= 0x8000_0000_0000_0000) {
				value = -(cast(long)value);
				flags |= Flags.isNegative;
			}
			else {
				context.error(pos, "`-%s` results in signed integer overflow", value);
			}
		}
	}
}

void print_literal_int(IntLiteralExprNode* node, ref AstPrintState state)
{
	if (node.isSigned)
		state.print("LITERAL int ", node.type.printer(state.context), " ", cast(long)node.value);
	else
		state.print("LITERAL int ", node.type.printer(state.context), " ", node.value);
}

void type_check_literal_int(IntLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	if (node.type.isUndefined) {
		if (node.isSigned) {
			BasicType t = minSignedIntType(node.value);
			t = max(t, BasicType.t_i32);
			node.type = state.context.basicTypeNodes(t);
		} else {
			BasicType t = minUnsignedIntType(node.value);
			if (cast(uint)(node.value & 0x7FFF_FFFF) == node.value) t = BasicType.t_i32;
			node.type = state.context.basicTypeNodes(t);
		}
	}
	node.state = AstNodeState.type_check_done;
}

IrIndex ir_gen_literal_int(CompilationContext* context, IntLiteralExprNode* n)
{
	CompilationContext* c = context;
	return c.constants.add(n.type.gen_ir_type(c), n.value);
}


@(AstType.literal_float)
struct FloatLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_float, 0, AstNodeState.name_resolve_done);
	double value;
	void negate(TokenIndex pos, ref CompilationContext context) {
		value = -value;
	}
}

void print_literal_float(FloatLiteralExprNode* node, ref AstPrintState state)
{
	state.print("LITERAL float ", node.type.printer(state.context), " ", node.value);
}

void type_check_literal_float(FloatLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	if (node.type.isUndefined) node.type = CommonAstNodes.type_f64;
	node.state = AstNodeState.type_check_done;
}

IrIndex ir_gen_literal_float(CompilationContext* context, FloatLiteralExprNode* node)
{
	if (node.type == CommonAstNodes.type_f32) {
		return context.constants.add(float(node.value));
	} else {
		assert(node.type == CommonAstNodes.type_f64);
		return context.constants.add(double(node.value));
	}
}


@(AstType.literal_null)
struct NullLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_null, 0, AstNodeState.name_resolve_done);
}

void print_literal_null(NullLiteralExprNode* node, ref AstPrintState state)
{
	state.print("LITERAL null");
}

void type_check_literal_null(NullLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	node.type = CommonAstNodes.type_null;
	node.state = AstNodeState.type_check_done;
}

IrIndex ir_gen_literal_null(CompilationContext* context, NullLiteralExprNode* n)
{
	CompilationContext* c = context;
	if (n.type.get_type(c).isPointer) {
		return c.constants.addZeroConstant(makeIrType(IrBasicType.i64));
	} else if (n.type.get_type(c).isSlice) {
		return c.constants.addZeroConstant(n.type.gen_ir_type(c));
	} else if (n.type.get_type(c).isTypeofNull) {
		return c.constants.addZeroConstant(makeIrType(IrBasicType.i64));
	} else c.internal_error(n.loc, "%s", n.type.printer(c));
}


@(AstType.literal_bool)
struct BoolLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_bool, 0, AstNodeState.name_resolve_done);
	bool value;
}

void print_literal_bool(BoolLiteralExprNode* node, ref AstPrintState state)
{
	if (node.value) state.print("TRUE ", node.type.printer(state.context));
	else state.print("FALSE ", node.type.printer(state.context));
}

void type_check_literal_bool(BoolLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	node.type = CommonAstNodes.type_bool;
	node.state = AstNodeState.type_check_done;
}

IrIndex ir_gen_literal_bool(CompilationContext* context, BoolLiteralExprNode* n)
{
	CompilationContext* c = context;
	return c.constants.add(makeIrType(IrBasicType.i8), (n.value != 0));
}

void ir_gen_branch_literal_bool(ref IrGenState gen, IrIndex currentBlock, ref IrLabel trueExit, ref IrLabel falseExit, BoolLiteralExprNode* n)
{
	if (n.value)
		gen.builder.addJumpToLabel(currentBlock, trueExit);
	else
		gen.builder.addJumpToLabel(currentBlock, falseExit);
}


@(AstType.literal_string)
struct StringLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_string, 0, AstNodeState.name_resolve_done);
	IrIndex irValue;
	string value;
}

IrIndex makeStringLiteralIrConstant(string data, LinkIndex moduleSymIndex, CompilationContext* c)
{
	IrIndex irValue;
	// dont create empty global for empty string. Globals are required to have non-zero length
	if (data.length == 0)
	{
		irValue = c.constants.addZeroConstant(makeIrType(IrBasicType.i64)); // null ptr
	}
	else
	{
		irValue = c.globals.add();
		IrGlobal* global = c.globals.get(irValue);
		global.type = CommonAstNodes.type_u8Ptr.gen_ir_type(c);

		ObjectSymbol sym = {
			kind : ObjectSymbolKind.isLocal,
			sectionIndex : c.builtinSections[ObjectSectionType.ro_data],
			moduleIndex : moduleSymIndex,
			flags : ObjectSymbolFlags.needsZeroTermination | ObjectSymbolFlags.isString,
			id : c.idMap.getOrRegNoDup(c, ":string"),
		};
		global.objectSymIndex = c.objSymTab.addSymbol(sym);

		ObjectSymbol* globalSym = c.objSymTab.getSymbol(global.objectSymIndex);
		globalSym.setInitializer(cast(ubyte[])data);
	}
	IrIndex irValueLength = c.constants.add(makeIrType(IrBasicType.i64), data.length);
	return c.constants.addAggrecateConstant(CommonAstNodes.type_u8Slice.gen_ir_type(c), irValueLength, irValue);
}

void print_literal_string(StringLiteralExprNode* node, ref AstPrintState state)
{
	state.print("LITERAL string ", node.type.printer(state.context), " ", node.value);
}

void type_check_literal_string(StringLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	// done in parser
	node.state = AstNodeState.type_check_done;
}

IrIndex ir_gen_literal_string(CompilationContext* context, StringLiteralExprNode* n)
{
	return n.irValue;
}


@(AstType.literal_array)
struct ArrayLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_array, 0, AstNodeState.name_resolve_done);
	AstNodes items;
	IrIndex irValue;
}

void print_literal_array(ArrayLiteralExprNode* node, ref AstPrintState state)
{
	state.print("LITERAL array ", node.type.printer(state.context), " ", node.items);
}

void type_check_literal_array(ArrayLiteralExprNode* node, ref TypeCheckState state)
{
	node.state = AstNodeState.type_check;
	require_type_check(node.items, state);
	node.state = AstNodeState.type_check_done;
}



AstIndex[SpecialKeyword.max+1] specialKeywordType = [
	SpecialKeyword.file          : CommonAstNodes.type_u8Slice,
	SpecialKeyword.line          : CommonAstNodes.type_u64,
	SpecialKeyword.function_name : CommonAstNodes.type_u8Slice,
	SpecialKeyword.module_name   : CommonAstNodes.type_u8Slice,
];

string[SpecialKeyword.max+1] specialKeywordName = [
	"__FILE__",
	"__LINE__",
	"__FUNCTION_NAME__",
	"__MODULE_NAME__",
];

/// __FILE__, __LINE__, __FUNCTION_NAME__, __MODULE_NAME__
@(AstType.literal_special)
struct SpecialLiteralExprNode {
	mixin ExpressionNodeData!(AstType.literal_special, 0, AstNodeState.type_check_done);
	IrIndex irValue;

	this(TokenIndex loc, IrIndex irValue, SpecialKeyword subType)
	{
		this.loc = loc;
		this.astType = AstType.literal_special;
		this.state = AstNodeState.type_check_done;
		this.subType = subType;
		this.irValue = irValue;
		this.type = specialKeywordType[subType];
	}
}

IrIndex eval_literal_special(SpecialKeyword kw, TokenIndex tok, ScopeIndex parentScope, CompilationContext* c) {
	final switch(kw) with(SpecialKeyword) {
		case file:
			ModuleDeclNode* mod = c.getModuleFromToken(tok);
			return c.get_file_name_constant(c.getAstNodeIndex(mod));
		case line:
			SourceLocation loc = c.tokenLoc(tok);
			return c.constants.add(makeIrType(IrBasicType.i64), loc.line+1);
		case function_name:
			AstIndex owner = find_innermost_owner(parentScope, AstType.decl_function, c);
			if (owner.isUndefined) return c.constants.addZeroConstant(CommonAstNodes.type_u8Slice.gen_ir_type(c));
			return c.get_function_name_constant(owner);
		case module_name:
			AstIndex owner = find_innermost_owner(parentScope, AstType.decl_module, c);
			c.assertf(owner.isDefined, "Can't find the module");
			return c.get_module_name_constant(owner);
	}
}

void print_literal_special(SpecialLiteralExprNode* node, ref AstPrintState state)
{
	state.print("LITERAL ", specialKeywordName[node.subType], " ", node.type.printer(state.context));
}

IrIndex ir_gen_literal_special(CompilationContext* context, SpecialLiteralExprNode* n)
{
	return n.irValue;
}
