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
	TypeNode* type;
	ExpressionNode* initializer; // may be null
	Identifier id;
	ubyte varFlags; // VariableFlags
	ushort scopeIndex; // stores index of parameter or index of member (for struct fields)
	IrIndex irValue; // kind is variable or stackSlot, unique id of variable within a function
	bool forceMemoryStorage() { return cast(bool)(varFlags & VariableFlags.forceMemoryStorage); }
	bool isParameter() { return cast(bool)(varFlags & VariableFlags.isParameter); }
	bool isAddressTaken() { return cast(bool)(varFlags & VariableFlags.isAddressTaken); }
}

void name_register_var(VariableDeclNode* node, ref NameRegisterState state) {
	node.state = AstNodeState.name_register;
	state.insert(node.id, node.as_node);
	if (node.initializer) require_name_register(node.initializer.as_node, state);
	node.state = AstNodeState.name_register_done;
}

void name_resolve_var(VariableDeclNode* node, ref NameResolveState state) {
	node.state = AstNodeState.name_resolve;
	require_name_resolve(node.type.as_node, state);
	if (node.initializer) require_name_resolve(node.initializer.as_node, state);
	node.state = AstNodeState.name_resolve_done;
}
