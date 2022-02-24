/// Copyright: Copyright (c) 2021 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module fe.ast.ast_node_state;

/// Invariant: child.state >= parent.state
enum AstNodeState : ubyte
{
	// initial state
	parse_done,
	// is set after scope gathered all named entities
	name_register_self,
	name_register_self_done,
	// is set after scope gathered all named entities
	name_register_nested,
	name_register_nested_done,
	// is set after name uses resolved identifier
	name_resolve,
	name_resolve_done,
	// is set after type checking
	type_check,
	type_check_done,
	// is set after IR representation was created
	ir_gen,
	ir_gen_done,
}
static assert(AstNodeState.max <= 15, "Assumed to fit in 4 bits");

// Used for fine-grained dependency tracking
// Instead of depending on the whole AST node being in some state
// node can depend on exact property it intends to read after request
// All property readers need to check the property state and calculate it if needed
enum PropertyState : ubyte
{
	not_calculated,
	calculating,
	calculated,
	//error_calculating, // no need for now
}

bool isCalculated(PropertyState s) {
	return s == PropertyState.calculated;
}

enum NodeProperty : ubyte {
	name_register_self,
	name_register_nested,
	name_resolve,
	type_check,
	type,
	ir_header,
	ir_body,
	init_value,
}
static assert(NodeProperty.max <= 15, "Max number of properties per uint is 16 (2 bits per property)");
