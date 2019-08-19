/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.decl.var;

import all;

enum VariableFlags : ubyte {
	forceMemoryStorage = 1 << 0,
	isParameter        = 1 << 1,
	isAddressTaken     = 1 << 2,
}

struct VariableDeclNode
{
	mixin AstNodeData!(AstType.decl_var, AstFlags.isDeclaration | AstFlags.isStatement);
	AstIndex type;
	AstIndex initializer; // may be null
	Identifier id;
	ubyte varFlags; // VariableFlags
	ushort scopeIndex; // stores index of parameter or index of member (for struct fields)
	IrIndex irValue; // kind is variable or stackSlot, unique id of variable within a function
	bool forceMemoryStorage() { return cast(bool)(varFlags & VariableFlags.forceMemoryStorage); }
	bool isParameter() { return cast(bool)(varFlags & VariableFlags.isParameter); }
	bool isAddressTaken() { return cast(bool)(varFlags & VariableFlags.isAddressTaken); }
}

void name_register_var(AstIndex nodeIndex, VariableDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	state.insert(node.id, nodeIndex);
	if (node.initializer) require_name_register(node.initializer, state);
	node.state = AstNodeState.name_register_done;
}

void name_resolve_var(VariableDeclNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.type, state);
	if (node.initializer) require_name_resolve(node.initializer, state);
	node.state = AstNodeState.name_resolve_done;
}

void type_check_var(VariableDeclNode* node, ref TypeCheckState state)
{
	CompilationContext* c = state.context;

	TypeNode* type = node.type.get_type(c);
	node.state = AstNodeState.type_check;
	require_type_check(node.type, state);

	if (type.isOpaqueStruct(c)) {
		if (node.isParameter) {
			c.error(node.loc,
				"cannot declare parameter of opaque type `%s`",
				type.printer(c));
		} else {
			c.error(node.loc,
				"cannot declare variable `%s` of opaque type `%s`",
				c.idString(node.id),
				type.printer(c));
		}
	}

	if (node.initializer) {
		require_type_check(node.initializer, state);
		autoconvTo(node.initializer, node.type, c);
	}

	if (!node.isParameter)
	{
		switch (type.astType) with(AstType)
		{
			case type_static_array, decl_struct, type_slice:
				node.varFlags |= VariableFlags.forceMemoryStorage;
				break;

			default: break;
		}
	}
	node.state = AstNodeState.type_check_done;
}