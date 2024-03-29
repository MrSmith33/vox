/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// Grammar
/// Lexer
/// Recursive descent parser
/// For expressions pratt parser is used
///   Copyright (c) 2017, Jean-Marc Bourguet
///   https://github.com/bourguet/operator_precedence_parsing/blob/86c11baa737673da521c9cb488fdc3b25d73f0b6/pratt_tdop_parser.py
module vox.fe.passes.parser;

import std.format : formattedWrite;
import std.string : format;
import std.range : repeat;
import std.stdio;
import std.conv : to;

import vox.all;


// Grammar
/**
	<module> = <declaration>* EOF
	<declaration> = <alias_decl> / <func_decl> / <var_decl> / <struct_decl> / <enum_decl>

	<alias_decl> = "alias" <id> "=" <expr> ";"
	<func_decl> = <type> <identifier> ("[" <template_args> "]")? "(" <param_list> ")" (<block_statement> / ';')
	<param_list> = <parameter> "," <parameter_list> / <parameter>?
	<parameter> = <type> <identifier>?

	<var_decl> = <type> <identifier> ("=" <expression>)? ";"
	<struct_decl> = "struct" <identifier> ("[" <template_args> "]")? "{" <declaration>* "}"
	<enum_decl> = <enum_decl_single> / <enum_decl_multi>
	<enum_decl_multi> = "enum" [<identifier>] [":" <type>] "{" (<identifier> ["=" <expr>] ",") * "}"
	<enum_decl_single> = "enum" <identifier> [ "=" <expr> ] ";"

	<statement> = "if" <paren_expression> <statement> ("else" <statement>)?
				  "while" <paren_expression> <statement> /
				  "do" <statement> "while" <paren_expression> ";" /
				  "return" <expression>? ";" /
				  "continue" ";" /
				  "break" ";" /
				  <block_statement> /
				  <expression> ("=" <expression>)? ";" /
				  <declaration_statement>

	<declaration_statement> = <declaration>
	<block_statement> = "{" <statement>* "}"

	<expression> = <test>
	<test> = <sum> | <sum> ("=="|"!="|"<"|">"|"<="|">=") <sum>
	<sum> = <term> / <sum> ("+"|"-") <term>
	<term> = <identifier> "(" <expression_list> ")" / <identifier> "[" <expression> "]" / <identifier> / <int_literal> / <string_literal> / <paren_expression>
	<paren_expression> = "(" <expression> ")"

	<expression_list> = (<expression> ",")*
	<identifier> = [_a-zA-Z] [_a-zA-Z0-9]*

	<type> = (<type_basic> / <type_struct>) <type_specializer>*
	<type_specializer> = "*" / "[" <expression> "]" / "[" "]" / "function" "(" <param_list> ")"
	<type_basic> = ("i8" | "i16" | "i32" | "i64" |
		"u8" | "u16" | "u32" | "u64" | "void" | "f32" | "f64")

	<type_struct> = <identifier>

	<int_literal> = <literal_dec_int> / <literal_hex_int> / <literal_bin_int>
	<literal_dec_int> = 0|[1-9][0-9_]*
	<literal_hex_int> = ("0x"|"0X")[0-9A-Fa-f_]+
	<literal_bin_int> = ("0b"|"0B")[01_]+
*/

void pass_parser(ref CompilationContext ctx, CompilePassPerModule[] subPasses) {
	Parser parser = Parser(&ctx);

	foreach (ref SourceFileInfo file; ctx.files.data)
	{
		parser.parseModule(file.mod, file.firstTokenIndex);

		if (ctx.printAstFresh) {
			writefln("// AST fresh `%s`", file.name);
			print_ast(ctx.getAstNodeIndex(file.mod), &ctx, 2);
		}
	}
}

// Attribute stack has the following structure at runtime
//
// - uneffective attributes (0 or more)
// - broadcasted attributes (0 or more)
// - immediate attributes   (0 or more) top of the stack
//
// when a new attribute is parsed it is added to the top of the attribute stack
//
struct BroadcastedAttribState
{
	// Number of attributes on the top of `attributeStack`, but below immediate attributes,
	// that will be applied to the next declaration
	// after application they will remain on the stack
	ushort numBroadcastedAttributes;

	// one bit per attribute in BuiltinFlagAttrib set
	// will be added to the next declaration
	// and will remain until the end of the scope
	ushort broadcastedFlagAttributes;
}

struct ImmediateAttribState
{
	// Number of attributes on the top of `attributeStack`, that
	// will be added to the next declaration
	// and will be dropped from the stack
	ushort numImmediateAttributes;

	// one bit per attribute in BuiltinFlagAttrib set
	// will be added to the next declaration
	// and will be zeroed
	ushort immediateFlagAttributes;
}

struct AttribState
{
	BroadcastedAttribState broadcasted;
	ImmediateAttribState immediate;

	ushort numEffectiveAttributes() { return cast(ushort)(broadcasted.numBroadcastedAttributes + immediate.numImmediateAttributes); }
	ushort effectiveFlagAttributes() { return broadcasted.broadcastedFlagAttributes | immediate.immediateFlagAttributes; }

	bool isStaticDecl() {
		return cast(bool)(effectiveFlagAttributes & BuiltinFlagAttrib.isStatic);
	}
}

struct ScopeTempData
{
	BroadcastedAttribState prevBroadcasted;
	bool isNonScope;
}

//version = print_parse;
struct Parser
{
	CompilationContext* context;
	ModuleDeclNode* currentModule;
	/// For member functions
	/// module, struct or function
	AstIndex declarationOwner;
	ScopeIndex currentScopeIndex;

	// Current token
	Token tok;

	// Saved to ScopeTempData on scope push/pop
	AttribState attribState;
	// Attributes affecting next declaration
	AstNodes attributeStack;

	// Allocates AttributeInfo in AST arena, so it will be before next AST node allocated.
	// returns 0 or AstFlags.hasAttributes flag. Must be added to node flags.
	ushort createAttributeInfo() {
		ushort declFlags;
		//writefln("%s %s", attribState.immediateFlagAttributes, attribState.broadcastedFlagAttributes);
		if (attribState.isStaticDecl) {
			declFlags |= AstFlags.isGlobal;
		} else {
			switch (declarationOwner.astType(context)) {
				case AstType.decl_struct:
					declFlags |= AstFlags.isMember;
					break;
				case AstType.decl_module:
					declFlags |= AstFlags.isGlobal;
					break;
				default: break;
			}
		}

		attribState.immediate.immediateFlagAttributes = 0;

		if (attribState.numEffectiveAttributes == 0) return declFlags;

		declFlags |= AstFlags.hasAttributes;

		AstNodes attributes;
		attributes.voidPut(context.arrayArena, attribState.numEffectiveAttributes);
		uint attribFlags;

		auto offset = attributeStack.length - attribState.numEffectiveAttributes;
		foreach(i; 0..attribState.numEffectiveAttributes) {
			AstIndex attrib = attributeStack[offset + i];
			attributes[i] = attrib;
			attribFlags |= calcAttribFlags(attrib, context);
		}

		attributeStack.unput(attribState.immediate.numImmediateAttributes);
		attribState.immediate.numImmediateAttributes = 0;

		context.appendAst!AttributeInfo(attributes, attribFlags);
		return declFlags;
	}

	SourceLocation loc() {
		return context.tokenLocationBuffer[tok.index];
	}

	int nesting;
	auto indent(uint var) { return ' '.repeat(var*2); }
	struct PrintScope { Parser* p; ~this(){--p.nesting;}}
	PrintScope scop(Args...)(string name, Args args) { write(indent(nesting)); writefln(name, args); ++nesting; return PrintScope(&this); }

	void nextToken()
	{
		do {
			++tok.index;
			tok.type = context.tokenBuffer[tok.index];
		}
		while (tok.type == TokenType.COMMENT);
	}

	void skipPast(TokenType tokType)
	{
		while (tok.type != TokenType.EOI)
		{
			++tok.index;
			tok.type = context.tokenBuffer[tok.index];
			if (tok.type == tokType) {
				nextToken;
				break;
			}
		}
	}

	bool hasMoreTokens() {
		return tok.type != TokenType.EOI;
	}

	AstIndex make(T, Args...)(TokenIndex start, Args args) {
		return context.appendAst!T(start, args);
	}
	// Will attach attributes if present
	AstIndex makeDecl(T, Args...)(TokenIndex start, Args args) {
		auto flags = createAttributeInfo;
		AstIndex declIndex = context.appendAst!T(start, args);
		if (flags) {
			AstNode* declNode = context.getAstNode(declIndex);
			declNode.flags |= flags;
		}
		return declIndex;
	}
	AstIndex makeExpr(T, Args...)(TokenIndex start, Args args) {
		return context.appendAst!T(start, AstIndex.init, args);
	}

	void expect(TokenType type, string after = null) {
		if (tok.type != type) {
			const(char)[] tokenString = context.getTokenString(tok.index);
			if (after)
				context.unrecoverable_error(tok.index, "Expected `%s` after %s, while got `%s` token '%s'",
					type, after, tok.type, tokenString);
			else
				context.unrecoverable_error(tok.index, "Expected `%s` token, while got `%s` token '%s'",
					type, tok.type, tokenString);
		}
	}

	void expectAndConsume(TokenType type, string after = null) {
		expect(type, after);
		nextToken;
	}

	Identifier makeIdentifier(TokenIndex index)
	{
		const(char)[] str = context.getTokenString(index);
		return context.idMap.getOrReg(context, str);
	}

	Identifier expectIdentifier(string after = null)
	{
		TokenIndex index = tok.index;
		expectAndConsume(TokenType.IDENTIFIER, after);
		Identifier id = makeIdentifier(index);
		return id;
	}

	// temp data is used instead of separate stack to push/pop
	ScopeTempData pushScope(string name, ScopeKind kind)
	{
		ScopeTempData temp;
		temp.prevBroadcasted = attribState.broadcasted;

		context.assertf(attribState.immediate == ImmediateAttribState.init,
			"No immediate attributes should remain before new scope");

		if (kind == ScopeKind.no_scope)
		{
			// no_scope scopes do not effectively introduce a new scope for attributes, nor for declarations/statements
			temp.isNonScope = true;

			// Outer effective attributes are effective inside no_scope scopes
			attribState = AttribState(attribState.broadcasted);
		}
		else
		{
			// Outer attributes are not visible inside
			attribState = AttribState.init;

			ScopeIndex newScopeIndex = context.appendScope;
			Scope* newScope = context.getAstScope(newScopeIndex);
			newScope.debugName = name;
			newScope.kind = kind;
			newScope.owner = declarationOwner;

			newScope.parentScope = currentScopeIndex;
			currentScopeIndex = newScopeIndex;
		}

		return temp;
	}

	void popScope(ScopeTempData temp)
	{
		context.assertf(attribState.immediate == ImmediateAttribState.init,
			"No immediate attributes should remain at the end of the scope");

		// How many attributes were introduced in the current scope compared to previous
		ushort numScopeAttributes = 0;
		if (attribState.broadcasted.numBroadcastedAttributes > temp.prevBroadcasted.numBroadcastedAttributes)
			numScopeAttributes = cast(ushort)(attribState.broadcasted.numBroadcastedAttributes - temp.prevBroadcasted.numBroadcastedAttributes);

		// mark the rest of effective nodes as broadcasted
		auto offset = attributeStack.length - numScopeAttributes;
		foreach(i; 0..numScopeAttributes) {
			AstIndex attrib = attributeStack[offset + i];
			attrib.flags(context) |= AnyAttributeFlags.isBroadcasted;
		}

		// drop broadcasted attributes introduced by this scope at the end of the scope
		attributeStack.unput(numScopeAttributes);

		if (temp.isNonScope)
		{
			// no scope was introduced
		}
		else
		{
			currentScopeIndex = currentScopeIndex.get_scope(context).parentScope;
		}

		attribState = AttribState(temp.prevBroadcasted);
	}

	// ------------------------------ PARSING ----------------------------------

	void parseModule(ModuleDeclNode* mod, TokenIndex firstTokenIndex) { // <module> ::= <declaration>*
		currentModule = mod;
		scope(exit) currentModule = null;

		version(print_parse) auto s1 = scop("parseModule");
		tok.index = firstTokenIndex;
		tok.type = context.tokenBuffer[tok.index];
		expectAndConsume(TokenType.SOI);
		mod.loc = tok.index;
		mod.state = AstNodeState.name_register_self_done;
		declarationOwner = context.getAstNodeIndex(mod);

		ScopeTempData scope_temp = pushScope("Module", ScopeKind.global);
			mod.memberScope = currentScopeIndex;
			parse_module_decl();
			parse_declarations(mod.declarations, TokenType.EOI);
		popScope(scope_temp);
	}

	void parse_declarations(ref AstNodes declarations, TokenType until) { // <declaration>*
		while (tok.type != until)
		{
			if (tok.type == TokenType.EOI) break;
			parse_declaration(declarations);
		}
	}

	void parse_declaration(ref AstNodes items) // <declaration> ::= <func_declaration> / <var_declaration> / <struct_declaration>
	{
		version(print_parse) auto s1 = scop("parse_declaration %s", loc);

		switch(tok.type) with(TokenType)
		{
			case ALIAS_SYM: // <alias_decl> ::= "alias" <id> "=" <expr> ";"
				AstIndex declIndex = parse_alias();
				items.put(context.arrayArena, declIndex);
				return;
			case STRUCT_SYM, UNION_SYM: // <struct_declaration> ::= "struct" <id> "{" <declaration>* "}"
				AstIndex declIndex = parse_struct();
				items.put(context.arrayArena, declIndex);
				return;
			case ENUM:
				AstIndex declIndex = parse_enum();
				items.put(context.arrayArena, declIndex);
				return;
			case IMPORT_SYM:
				AstIndex declIndex = parse_import();
				items.put(context.arrayArena, declIndex);
				return;
			case MODULE_SYM:
				context.error(tok.index, "Module declaration can only occur as first declaration of the module");
				skipPast(TokenType.SEMICOLON);
				AstIndex declIndex = context.getAstNodeIndex(currentModule);
				items.put(context.arrayArena, declIndex);
				return;
			case HASH_IF, HASH_VERSION:
				AstIndex declIndex = parse_hash_if();
				items.put(context.arrayArena, declIndex);
				return;
			case HASH_ASSERT:
				AstIndex declIndex = parse_hash_assert();
				items.put(context.arrayArena, declIndex);
				return;
			case HASH_FOREACH:
				AstIndex declIndex = parse_hash_foreach();
				items.put(context.arrayArena, declIndex);
				return;
			case AT:
				parse_attribute(items);
				return;
			case TYPE_AUTO:
				parse_auto_decl(items);
				return;
			default: // <func_declaration> / <var_declaration>
				TokenIndex start = tok.index;
				AstIndex body_start = AstIndex(context.astBuffer.uintLength);
				AstIndex typeIndex = expr(PreferType.yes, 0);
				AstIndex nodeIndex = parse_var_func_declaration_after_type(start, body_start, typeIndex, ConsumeTerminator.yes, TokenType.SEMICOLON);
				AstIndex declIndex = nodeIndex;
				items.put(context.arrayArena, declIndex);
				return;
		}
		assert(false);
	}

	void parse_single_attribute(TokenIndex start)
	{
		Identifier attributeId = expectIdentifier("@");

		switch(attributeId.index) {
			case CommonIds.id_extern.index:
				parse_extern_attribute(start);
				break;
			case CommonIds.id_static.index:
				attribState.immediate.immediateFlagAttributes |= BuiltinFlagAttrib.isStatic;
				break;
			default:
				context.unrecoverable_error(start, "Unknown built-in attribute `%s`", context.idString(attributeId));
		}
	}

	// parses all attributes and one or more declarations that it is attached too
	void parse_attribute(ref AstNodes declarations)
	{
		while(true)
		{
			TokenIndex start = tok.index;
			nextToken; // skip @
			parse_single_attribute(start);

			switch(tok.type)
			{
			case TokenType.AT:
				break; // next attribute
			case TokenType.COLON:
				nextToken; // skip :
				// Mark all immediate attributes as scoped attributes.
				// They will be effective until the end of current scope, then the will be popped by popScope
				attribState.broadcasted.numBroadcastedAttributes += attribState.immediate.numImmediateAttributes;
				attribState.immediate.numImmediateAttributes = 0;
				// Mark all immediate flag attributes as scoped attributes.
				attribState.broadcasted.broadcastedFlagAttributes |= attribState.immediate.immediateFlagAttributes;
				attribState.immediate.immediateFlagAttributes = 0;
				// next comes another attribute or declaration
				if (tok.type == TokenType.AT) break; // next attribute
				return; // back to declaration/statement parsing
			case TokenType.LCURLY:
				nextToken; // skip {
				// all attributes before {} will be applied to all declarations in {}
				// attributes before {} are initially classified as immediate
				// Remove them, and then move them into the scope
				// @attr1 @attr2 { // number of attributes == numImmediateAttributes
				//     decl1;
				//     decl2;
				// }
				// is the same as:
				// {
				//     @attr1 @attr2:
				//     decl1;
				//     decl2;
				// }

				// number of attributes
				ushort numImmediateAttributes = attribState.immediate.numImmediateAttributes;
				// save immediate flag attributes
				auto immediateFlagAttributes = attribState.immediate.immediateFlagAttributes;

				// remove attributes from current scope
				attribState.immediate.numImmediateAttributes = 0;

				// remove flag attributes from current scope
				attribState.immediate.immediateFlagAttributes = 0;

				// enter the scope
				ScopeTempData scope_temp = pushScope("@{}", ScopeKind.no_scope);

				// restore flag attributes, but now they are in the nested scope
				attribState.immediate.immediateFlagAttributes = immediateFlagAttributes;

				// add them as @: attributes
				attribState.broadcasted.numBroadcastedAttributes += numImmediateAttributes;

				// declarations
				parse_declarations(declarations, TokenType.RCURLY);

				// @: attributes are cleared by the scope end inside popScope
				popScope(scope_temp);

				expectAndConsume(TokenType.RCURLY, "@{");
				return;
			default:
				parse_declaration(declarations);
				return;
			}
		}
	}

	void parse_extern_attribute(TokenIndex start)
	{
		expectAndConsume(TokenType.LPAREN, "@extern");

		Identifier externKindId;
		if (tok.type == TokenType.MODULE_SYM) {
			externKindId = CommonIds.id_module;
			expectAndConsume(TokenType.MODULE_SYM, "@extern(");
		} else {
			externKindId = expectIdentifier("@extern(");
		}

		AstIndex attribute;
		switch(externKindId.index) {
			case CommonIds.id_syscall.index:
				expectAndConsume(TokenType.COMMA, "@extern(syscall");
				expect(TT.INT_DEC_LITERAL, "@extern(syscall,");
				string value = cast(string)context.getTokenString(tok.index);
				import std.algorithm.iteration : filter;
				uint syscallNumber = value.filter!(c => c != '_').to!uint;
				nextToken; // skip integer
				attribute = make!BuiltinAttribNode(start, BuiltinAttribSubType.extern_syscall, syscallNumber);
				break;
			case CommonIds.id_module.index:
				expectAndConsume(TokenType.COMMA, "@extern(module");
				expect(TT.STRING_LITERAL, "@extern(module,");
				string value = cast(string)context.getTokenString(tok.index);
				nextToken; // skip lib name
				Identifier moduleId = context.idMap.getOrReg(context, value[1..$-1]);
				attribute = make!BuiltinAttribNode(start, BuiltinAttribSubType.extern_module, moduleId.index);
				break;
			default:
				context.unrecoverable_error(start, "Unknown @extern kind `%s`", context.idString(externKindId));
		}

		expectAndConsume(TokenType.RPAREN, "@extern(...");
		++attribState.immediate.numImmediateAttributes;
		attributeStack.put(context.arrayArena, attribute);
	}

	/// <alias_decl> ::= "alias" <id> "=" <expr> ";"
	AstIndex parse_alias()
	{
		TokenIndex start = tok.index;
		nextToken; // skip "alias"

		Identifier aliasId = expectIdentifier();
		expectAndConsume(TokenType.EQUAL);

		AstIndex initializerIndex = expr(PreferType.no);
		expectAndConsume(TokenType.SEMICOLON);

		return makeDecl!AliasDeclNode(start, currentScopeIndex, aliasId, initializerIndex);
	}

	/// Parses expression preferring types, if identifier follows, parses as var/func declaration
	AstIndex parse_expr_or_id_decl(ConsumeTerminator consume_terminator, TokenType var_terminator = TokenType.init)
	{
		TokenIndex start = tok.index;
		AstIndex body_start = AstIndex(context.astBuffer.uintLength);
		AstIndex expr_or_type = expr(PreferType.yes);

		if (tok.type == TokenType.IDENTIFIER)
		{
			// declaration
			return parse_var_func_declaration_after_type(start, body_start, expr_or_type, consume_terminator, var_terminator);
		}
		else
		{
			// expression
			AstNode* statementNode = context.getAstNode(expr_or_type);
			if (consume_terminator) expectAndConsume(var_terminator);
			return expr_or_type;
		}
	}

	enum ConsumeTerminator : bool { no, yes }

	bool canBeType(AstIndex someExpr)
	{
		switch(someExpr.astType(context)) with(AstType)
		{
			case type_basic, type_ptr, type_static_array, type_slice, type_func_sig:
			case expr_name_use, expr_member, expr_index, expr_slice:
				return true;
			default:
				return false;
		}
	}

	AstIndex parse_var_func_declaration_after_type(TokenIndex start, AstIndex body_start, AstIndex typeIndex, ConsumeTerminator consume_terminator, TokenType var_terminator)
	{
		version(print_parse) auto s2 = scop("<func_declaration> / <var_declaration> %s", start);

		TokenIndex idPos = tok.index;
		Identifier declarationId = expectIdentifier();

		if (!canBeType(typeIndex))
		{
			context.error(idPos,
				"Invalid expression. Missing `;` before `%s`", context.idString(declarationId));
		}

		AstIndex initializerIndex;
		if (tok.type == TokenType.EQUAL) // "=" <expression>
		{
			// <var_decl> = <type> <identifier> ("=" <expression>)? ";"
			nextToken; // skip "="
			initializerIndex = expr(PreferType.no);
		}

		if (tok.type == TokenType.SEMICOLON || tok.type == TokenType.COMMA) // <var_declaration> ::= <type> <id> (";" / ",")
		{
			// variable
			version(print_parse) auto s3 = scop("<var_declaration> %s", start);
			if (consume_terminator) expectAndConsume(var_terminator);
			// leave ";" or "," for parent to decide
			AstIndex varIndex = makeDecl!VariableDeclNode(idPos, currentScopeIndex, typeIndex, initializerIndex, declarationId);
			return varIndex;
		}
		else if (tok.type == TokenType.LBRACKET) // <func_declaration> ::= <type> <id> "[" <template_params> "]" "(" <param_list> ")" (<block_statement> / ';')
		{
			// templated function
			AstNodes template_params;
			ushort numParamsBeforeVariadic;
			parse_template_parameters(template_params, numParamsBeforeVariadic);
			AstIndex body = parse_func(idPos, typeIndex, declarationId);
			AstIndex after_body = AstIndex(context.astBuffer.uintLength);
			return makeDecl!TemplateDeclNode(idPos, currentScopeIndex, template_params, body, body_start, after_body, declarationId, numParamsBeforeVariadic);
		}
		else if (tok.type == TokenType.LPAREN) // <func_declaration> ::= <type> <id> "(" <param_list> ")" (<block_statement> / ';')
		{
			// function
			return parse_func(idPos, typeIndex, declarationId);
		}
		else
		{
			context.unrecoverable_error(tok.index, "Expected '(' or ';', while got '%s'", context.getTokenString(tok.index));
		}
	}

	AstIndex parse_func(TokenIndex start, AstIndex typeIndex, Identifier declarationId)
	{
		version(print_parse) auto s3 = scop("<func_declaration> %s", start);
		AstNodes params;
		ushort funcFlags;

		// make all attributes apply to both function and the signature
		auto beforeAttribState = attribState;

		// make all attributes broadcasted, so that they are not removed when function is created
		attribState.broadcasted.numBroadcastedAttributes += attribState.immediate.numImmediateAttributes;
		attribState.immediate.numImmediateAttributes = 0;
		attribState.broadcasted.broadcastedFlagAttributes |= attribState.immediate.immediateFlagAttributes;
		attribState.immediate.immediateFlagAttributes = 0;

		AstIndex funcIndex = makeDecl!FunctionDeclNode(start, context.getAstNodeIndex(currentModule), ScopeIndex(), AstIndex(), declarationId);
		auto func = funcIndex.get!FunctionDeclNode(context);

		// no attributes go to the function body or to the parameters
		attribState = AttribState.init;

		AstIndex prevOwner = declarationOwner;
		declarationOwner = funcIndex;
		scope(exit) declarationOwner = prevOwner;

		ScopeTempData scope_temp = pushScope(context.idString(declarationId), ScopeKind.local);
		scope(exit) popScope(scope_temp);

		// set scope for a function
		ScopeIndex parameterScope = currentScopeIndex;
		func.parameterScope = parameterScope;

		// add this pointer parameter
		if (func.isMember)
		{
			AstIndex structName = make!NameUseExprNode(start, currentScopeIndex, prevOwner.get!StructDeclNode(context).id);
			NameUseExprNode* name = structName.get_name_use(context);
			name.resolve(prevOwner, context);
			name.flags |= AstFlags.isType;
			name.state = AstNodeState.name_resolve_done;
			AstIndex thisType = make!PtrTypeNode(start, CommonAstNodes.type_type, structName);

			// parameter
			AstIndex param = make!VariableDeclNode(start, currentScopeIndex, thisType, AstIndex.init, CommonIds.id_this, ushort(0));
			VariableDeclNode* paramNode = param.get!VariableDeclNode(context);
			paramNode.flags |= VariableFlags.isParameter;
			params.put(context.arrayArena, param);
		}

		// restore attributes for signature to consume
		attribState = beforeAttribState;

		CallConvention callConvention = context.defaultCallConvention;
		AstIndex signature = makeDecl!FunctionSignatureNode(start, typeIndex, params, callConvention);

		// store remaining state back
		scope_temp.prevBroadcasted = attribState.broadcasted;
		// no attributes go to the parameters and body
		attribState = AttribState.init;

		parseParameters(signature); // functions need to register their param names
		func.signature = signature;

		currentScopeIndex.get_scope(context).owner = funcIndex;

		if (tok.type == TokenType.HASH_INLINE)
		{
			nextToken; // skip #inline
			func.flags |= FuncDeclFlags.isInline;
		}

		if (tok.type != TokenType.SEMICOLON)
		{
			func.block_stmt = block_stmt();
			signature.flags(context) |= FuncSignatureFlags.attachedToFunctionWithBody;
		}
		else expectAndConsume(TokenType.SEMICOLON); // external function

		return funcIndex;
	}

	// I previously tried to not register parameters in the scope for function types
	// but with the introduction of named arguments they now must be registered
	void parseParameters(AstIndex signature)
	{
		auto sig = signature.get!FunctionSignatureNode(context);
		expectAndConsume(TokenType.LPAREN);

		ubyte numDefaultArgs = 0;
		while (tok.type != TokenType.RPAREN)
		{
			if (tok.type == TokenType.EOI) break;

			// <param> ::= <type> <identifier>?
			TokenIndex paramStart = tok.index;
			AstIndex paramType = expr(PreferType.yes, 0);
			Identifier paramId;
			ushort flags = VariableFlags.isParameter;
			size_t paramIndex = sig.parameters.length;
			AstIndex defaultValue;

			if (tok.type == TokenType.DOT_DOT_DOT) // expanded type
			{
				if (sig.hasExpandedParam) {
					context.error(tok.index, "Cannot have two expanded parameters");
				}
				nextToken; // skip ...
				flags |= VariableFlags.isVariadicParam;
				sig.flags |= FuncSignatureFlags.hasExpandedParam;
				sig.numParamsBeforeVariadic = cast(ushort)paramIndex;
			}

			if (tok.type == TokenType.IDENTIFIER) // named parameter
			{
				paramStart = tok.index;
				paramId = expectIdentifier();
			}
			else // anon parameter
			{
				flags |= VariableFlags.isAnonymous;
				paramId = context.idMap.getOrRegFormatted(context, "__param_%s", paramIndex);
			}

			// default argument
			if (tok.type == TokenType.EQUAL)
			{
				nextToken; // skip =
				defaultValue = expr(PreferType.yes, 0);

				++sig.numDefaultArgs;
			}
			else
			{
				// all default arguments must be at the end of param list
				if (sig.numDefaultArgs != 0)
					context.error(paramStart,
						"Default argument expected for %s", context.idString(paramId));
			}

			AstIndex param = makeDecl!VariableDeclNode(paramStart, currentScopeIndex, paramType, defaultValue, paramId);
			VariableDeclNode* paramNode = param.get!VariableDeclNode(context);
			if (paramNode.isGlobal) context.error(paramNode.loc, "Parameters cannot be @static");
			paramNode.flags |= flags;
			paramNode.scopeIndex = cast(typeof(paramNode.scopeIndex))paramIndex;

			sig.parameters.put(context.arrayArena, param);
			if (tok.type == TokenType.COMMA) nextToken; // skip ","
			else break;
		}
		if (!sig.hasExpandedParam)
			sig.numParamsBeforeVariadic = cast(ushort)sig.parameters.length;

		expectAndConsume(TokenType.RPAREN);
	}

	// 0+ non-variadic params, ? variadic params
	void parse_template_parameters(ref AstNodes params, out ushort numParamsBeforeVariadic)
	{
		expectAndConsume(TokenType.LBRACKET);

		bool hasVaridic = false;
		while (tok.type != TokenType.RBRACKET)
		{
			if (tok.type == TokenType.EOI) break;

			// <type_param> ::= <identifier>
			TokenIndex paramStart = tok.index;
			Identifier paramId = expectIdentifier();
			ushort paramIndex = cast(ushort)params.length;

			AstIndex param = makeDecl!TemplateParamDeclNode(paramStart, paramId, paramIndex);
			if (tok.type == TokenType.DOT_DOT_DOT) {
				nextToken; // skip "..."
				param.flags(context) |= TemplateParamDeclFlags.isVariadic;
				if (hasVaridic) {
					context.error(param.loc(context),
						"Only single variadic template parameter allowed");
				}
				hasVaridic = true;
				numParamsBeforeVariadic = paramIndex;
			}
			else
			{
				// In the future we may allow non-variadic template params after variadic one
				if (hasVaridic)
					context.error(param.loc(context),
						"Cannot have template parameters after variadic parameter (WIP)");
			}

			params.put(context.arrayArena, param);

			if (tok.type == TokenType.COMMA) nextToken; // skip ","
			else break;
		}

		if (!hasVaridic) numParamsBeforeVariadic = cast(ushort)params.length;

		expectAndConsume(TokenType.RBRACKET);
	}

	ushort parse_argument_list(ref AstNodes expressions)
	{
		ushort flags;
		// COMMA_PREC is used in `expr` because we don't want to grab the comma, e.g. it is NOT a sequence operator.
		while (tok.type != TokenType.RPAREN)
		{
			AstIndex exprIndex;
			if (tok.type == TokenType.IDENTIFIER)
			{
				Token idToken = tok;
				nextToken; // skip id
				if (tok.type == TokenType.COLON)
				{
					flags |= CallExprFlags.hasNamedArgs;
					// named argument
					nextToken; // skip :
					Identifier paramId = makeIdentifier(idToken.index);
					AstIndex child = expr(PreferType.no, COMMA_PREC);
					exprIndex = make!NamedArgumenExprNode(idToken.index, paramId, child);
				}
				else
				{
					// regular expression, undo identifier
					tok = idToken;
					exprIndex = expr(PreferType.no, COMMA_PREC);
				}
			}
			else
			{
				exprIndex = expr(PreferType.no, COMMA_PREC);
			}
			expressions.put(context.arrayArena, exprIndex);

			// allows trailing comma too
			if (tok.type == TokenType.COMMA)
				nextToken;
		}
		expectAndConsume(TokenType.RPAREN);
		return flags;
	}

	// <struct_declaration> ::= "struct" <id> ("[" <template_params> "]")? "{" <declaration>* "}" /
	//                          "struct" <id> ("[" <template_params> "]")? ";"
	AstIndex parse_struct()
	{
		ushort structFlags;
		if (tok.type == TokenType.UNION_SYM) {
			structFlags |= StructFlags.isUnion;
		}

		TokenIndex start = tok.index;
		AstIndex body_start = AstIndex(context.astBuffer.uintLength);

		version(print_parse) auto s2 = scop("struct %s", start);
		nextToken; // skip "struct"
		Identifier structId = expectIdentifier();

		AstIndex parse_rest()
		{
			if (tok.type == TokenType.SEMICOLON)
			{
				nextToken; // skip semicolon
				AstIndex structIndex = makeDecl!StructDeclNode(start, currentScopeIndex, ScopeIndex(), structId);
				StructDeclNode* s = structIndex.get!StructDeclNode(context);
				s.flags |= structFlags | StructFlags.isOpaque;
				return structIndex;
			}

			AstIndex structIndex = makeDecl!StructDeclNode(start, currentScopeIndex, ScopeIndex(), structId);
			StructDeclNode* s = structIndex.get!StructDeclNode(context);
			s.flags |= structFlags;

			ScopeTempData scope_temp = pushScope(context.idString(structId), ScopeKind.member);
			scope(exit) popScope(scope_temp);

			s.memberScope = currentScopeIndex;

			// no attributes go to the body
			attribState = AttribState.init;

			expectAndConsume(TokenType.LCURLY);
			{
				AstIndex prevOwner = declarationOwner;
				declarationOwner = structIndex;
				scope(exit) declarationOwner = prevOwner;

				parse_declarations(s.declarations, TokenType.RCURLY);

				ushort varIndex = 0;
				foreach(AstIndex declIndex; s.declarations) {
					AstNode* declNode = context.getAstNode(declIndex);
					if (declNode.astType == AstType.decl_var) {
						declNode.as!VariableDeclNode(context).scopeIndex = varIndex++;
					}
				}
			}
			expectAndConsume(TokenType.RCURLY);

			return structIndex;
		}

		AstNodes template_params;
		if (tok.type == TokenType.LBRACKET) // <func_declaration> ::= "struct" <id> "[" <template_params> "]"
		{
			ushort numParamsBeforeVariadic;
			parse_template_parameters(template_params, numParamsBeforeVariadic);
			AstIndex body = parse_rest();
			AstIndex after_body = AstIndex(context.astBuffer.uintLength);
			return makeDecl!TemplateDeclNode(start, currentScopeIndex, template_params, body, body_start, after_body, structId, numParamsBeforeVariadic);
		}

		return parse_rest();
	}

	// <enum_decl> = <enum_decl_single> / <enum_decl_multi>
	// <enum_decl_multi> = "enum" [<identifier>] [":" <type>] {" <identifier> ["=" <expr>] ,* "}"
	// <enum_decl_single> = "enum" <identifier> [ "=" <expr> ] ";"

	// enum i32 e2; // manifest constant, invalid, need initializer
	// enum e3 = 3; // manifest constant
	// enum i32 e4 = 4; // manifest constant

	// enum { e5 } // anon type
	// enum : i32 { e6 } // anon type

	// enum e1; // type
	// enum e7 : i32 { e7 } // type
	// enum e8 : i32; // type, body omitted
	// enum e9 { e9 } // type
	AstIndex parse_enum()
	{
		TokenIndex start = tok.index;
		nextToken; // slip `enum`

		AstIndex intType = CommonAstNodes.type_i32;

		AstIndex parseColonType()
		{
			nextToken; // skip ":"
			AstIndex type = expr(PreferType.yes, 0);
			if (!type)
				context.unrecoverable_error(tok.index,
					"Expected type after `enum :`, while got `%s`", context.getTokenString(tok.index));

			return type;
		}

		AstNodes tryParseEnumBody(AstIndex type)
		{
			if (tok.type == TokenType.SEMICOLON) {
				nextToken; // skip ";"
				return AstNodes();
			} else if (tok.type == TokenType.LCURLY) {
				return parse_enum_body(type);
			} else {
				context.unrecoverable_error(tok.index,
					"Expected `;` or `{` at the end of enum declaration, while got `%s`",
					context.getTokenString(tok.index));
			}
		}

		// enum T e4 = initializer;
		AstIndex parseTypeEnum()
		{
			AstIndex type = expr(PreferType.yes, 0);
			if (!type)
				context.unrecoverable_error(tok.index,
					"Expected type after `enum`, while got `%s`",
					context.getTokenString(tok.index));

			Identifier enumId = expectIdentifier;
			expectAndConsume(TokenType.EQUAL); // "="
			AstIndex value = expr(PreferType.no); // initializer

			auto member = makeDecl!EnumMemberDecl(start, currentScopeIndex, type, value, enumId);

			expectAndConsume(TokenType.SEMICOLON); // ";"

			// enum i32 e4 = 4;
			return member;
		}

		// can be both enum identifier and type identifier
		if (tok.type == TokenType.IDENTIFIER)
		{
			Token copy = tok; // save
			TokenIndex id = tok.index;
			nextToken; // skip identifier

			// enum type with no type or body
			// enum e1;
			if (tok.type == TokenType.SEMICOLON)
			{
				nextToken; // skip ";"
				Identifier enumId = makeIdentifier(id);
				ScopeIndex memberScope; // no scope

				return makeDecl!EnumDeclaration(start, currentScopeIndex, memberScope, AstNodes(), intType, enumId);
			}
			else if (tok.type == TokenType.EQUAL)
			{
				nextToken; // skip "="
				Identifier enumId = makeIdentifier(id);
				AstIndex value = expr(PreferType.no);
				// type will be taken from initializer
				auto member = make!EnumMemberDecl(start, currentScopeIndex, AstIndex.init, value, enumId);

				expectAndConsume(TokenType.SEMICOLON); // ";"

				// enum e3 = 3;
				return member;
			}
			// enum e7 : i32 ...
			else if (tok.type == TokenType.COLON)
			{
				Identifier enumId = makeIdentifier(id);
				AstIndex memberType = parseColonType;
				ScopeTempData scope_temp = pushScope(context.idString(enumId), ScopeKind.member);
				ScopeIndex memberScope = currentScopeIndex;
				AstIndex enumIndex = makeDecl!EnumDeclaration(start, ScopeIndex.init, memberScope, AstNodes.init, memberType, enumId);
				currentScopeIndex.get_scope(context).owner = enumIndex;
				auto enumNode = enumIndex.get!EnumDeclaration(context);
				AstNodes members = tryParseEnumBody(enumIndex);
				popScope(scope_temp);
				enumNode.declarations = members;
				enumNode.parentScope = currentScopeIndex;

				// enum e7 : i32 { e7 }
				// enum e8 : i32;
				return enumIndex;
			}
			else if (tok.type == TokenType.LCURLY)
			{
				Identifier enumId = makeIdentifier(id);
				AstIndex memberType = intType;
				ScopeTempData scope_temp = pushScope(context.idString(enumId), ScopeKind.member);
				ScopeIndex memberScope = currentScopeIndex;
				AstIndex enumIndex = makeDecl!EnumDeclaration(start, ScopeIndex.init, memberScope, AstNodes.init, memberType, enumId);
				currentScopeIndex.get_scope(context).owner = enumIndex;
				auto enumNode = enumIndex.get!EnumDeclaration(context);
				AstNodes members = parse_enum_body(enumIndex);
				popScope(scope_temp);
				enumNode.declarations = members;
				enumNode.parentScope = currentScopeIndex;

				// enum e9 { e9 }
				return enumIndex;
			}
			else
			{
				tok = copy; // restore
				return parseTypeEnum;
			}
		}
		else if (tok.type == TokenType.COLON)
		{
			AstIndex memberType = parseColonType;
			AstNodes members = parse_enum_body(memberType);
			ScopeIndex memberScope; // no scope

			// enum : i32 { e6 }
			return makeDecl!EnumDeclaration(start, currentScopeIndex, memberScope, members, memberType);
		}
		else if (tok.type == TokenType.LCURLY)
		{
			AstIndex memberType = intType;
			AstNodes members = parse_enum_body(memberType);
			ScopeIndex memberScope; // no scope

			// enum { e5 }
			return makeDecl!EnumDeclaration(start, currentScopeIndex, memberScope, members, memberType);
		}
		else if (isBasicTypeToken(tok.type))
		{
			return parseTypeEnum;
		}
		else
		{
			context.unrecoverable_error(tok.index,
				"Invalid enum declaration, got %s after `enum`",
				context.getTokenString(tok.index));
		}
	}

	AstIndex parse_import()
	{
		TokenIndex start = tok.index;
		version(print_parse) auto s = scop("import %s", start);
		nextToken; // skip "import"
		Identifier id;

		while (true) {
			string after = id.isDefined ? "import" : null;
			id = context.idMap.getOrRegFqn(context, FullyQualifiedName(id, expectIdentifier(after)));

			if (tok.type == TokenType.DOT) {
				nextToken; // skip "."
			} else if (tok.type == TokenType.SEMICOLON) {
				nextToken; // skip ";"
				break;
			} else {
				context.unrecoverable_error(tok.index,
					"Expected `;` or `.` after identifier, while got `%s`",
					context.getTokenString(tok.index));
			}
		}
		return makeDecl!ImportDeclNode(start, currentScopeIndex, id);
	}

	void parse_module_decl()
	{
		TokenIndex start = tok.index;
		AstIndex parentPackage = CommonAstNodes.node_root_package;
		AstIndex conflictingModule;
		AstIndex conflictingModPack;

		// module declaration
		if (tok.type == TokenType.MODULE_SYM)
		{
			nextToken; // skip "module"

			Identifier lastId;
			while (true) {
				Identifier newId = expectIdentifier();
				lastId = context.idMap.getOrRegFqn(context, FullyQualifiedName(lastId, newId));

				if (tok.type == TokenType.DOT) {
					nextToken; // skip "."
					auto parentPackageNode = parentPackage.get!PackageDeclNode(context);
					parentPackage = parentPackageNode.getOrCreateSubpackage(start, lastId, conflictingModule, context);
				} else if (tok.type == TokenType.SEMICOLON) {
					nextToken; // skip ";"
					currentModule.fqn = lastId;
					break;
				} else {
					context.unrecoverable_error(tok.index,
						"Expected `;` or `.` after identifier, while got `%s`",
						context.getTokenString(tok.index));
				}
			}

			currentModule.loc = start;
		}
		else
		{
			currentModule.loc = context.files[currentModule.moduleIndex.fileIndex].firstTokenIndex;
		}

		currentModule.parentPackage = parentPackage;
		auto parentPackageNode = parentPackage.get!PackageDeclNode(context);
		parentPackageNode.addModule(start, currentModule.fqn, context.getAstNodeIndex(currentModule), conflictingModPack, context);
		context.modules.put(context.arrayArena, currentModule.fqn, context.getAstNodeIndex(currentModule));
		void modConflict(ModuleDeclNode* newMod, ModuleDeclNode* oldMod)
		{
			context.error(newMod.loc,
				"Module `%s` in file %s conflicts with another module `%s` in file %s",
				newMod.fqn.pr(context),
				context.files[newMod.moduleIndex.fileIndex].name,
				oldMod.fqn.pr(context),
				context.files[oldMod.moduleIndex.fileIndex].name, );
		}

		void modPackConflict(ModuleDeclNode* newMod, PackageDeclNode* oldPack)
		{
			context.error(newMod.loc,
				"Module `%s` in file %s conflicts with package `%s` in files %s",
				newMod.fqn.pr(context),
				context.files[newMod.moduleIndex.fileIndex].name,
				oldPack.fqn.pr(context),
				PackageFilesPrinter(oldPack, context));
		}

		if (conflictingModule.isDefined) {
			modConflict(currentModule, conflictingModule.get!ModuleDeclNode(context));
		}

		if (conflictingModPack.isDefined) {
			AstNode* conflictingNode = conflictingModPack.get_node(context);
			if (conflictingNode.astType == AstType.decl_module) {
				modConflict(currentModule, conflictingNode.as!ModuleDeclNode(context));
				// module foo from file bar.d conflicts with another module foo from file foo.d
			} else {
				context.assertf(conflictingNode.astType == AstType.decl_package, "Must be package");
				modPackConflict(currentModule, conflictingNode.as!PackageDeclNode(context));
			}
		}
	}

	AstIndex parse_hash_assert() /* "#assert(" <condition>, <message> ");"*/
	{
		TokenIndex start = tok.index;
		nextToken; // skip "#assert"

		if (tok.type != TokenType.LPAREN)
			context.unrecoverable_error(tok.index,
				"Expected `(` after #assert, while got `%s`",
				context.getTokenString(tok.index));
		nextToken; // skip (

		AstIndex condition = expr(PreferType.no);
		AstIndex message;

		if (tok.type != TokenType.RPAREN)
		{
			if (tok.type != TokenType.COMMA)
				context.unrecoverable_error(tok.index,
					"Expected `,` after condition of #assert, while got `%s`",
					context.getTokenString(tok.index));

			nextToken; // skip ,

			message = expr(PreferType.no);
		}

		if (tok.type != TokenType.RPAREN)
			context.unrecoverable_error(tok.index,
				"Expected `)` after message of #assert, while got `%s`",
				context.getTokenString(tok.index));
		nextToken; // skip )

		if (tok.type != TokenType.SEMICOLON)
			context.unrecoverable_error(tok.index,
				"Expected `;` after #assert, while got `%s`",
				context.getTokenString(tok.index));
		nextToken; // skip ;

		return make!StaticAssertDeclNode(start, condition, message);
	}

	void parseItems(alias itemParser)(ref AstNodes items, ScopeKind scopeKind)
	{
		TokenIndex start = tok.index;
		if (tok.type == TokenType.LCURLY)
		{
			nextToken; // skip {
			ScopeTempData scope_temp = pushScope(null, scopeKind);
			while (tok.type != TokenType.RCURLY)
			{
				if (tok.type == TokenType.EOI) break;
				itemParser(items);
			}
			popScope(scope_temp);
			expectAndConsume(TokenType.RCURLY);
		}
		else
		{
			itemParser(items);
		}
	}

	void parseStaticIfThenElse(ref AstNodes thenStatements, ref AstNodes elseStatements)
	{
		if (declarationOwner.astType(context) == AstType.decl_function)
		{
			parseItems!statement(thenStatements, ScopeKind.no_scope);
			if (tok.type == TokenType.ELSE_SYM) { /* ... "else" <statement> */
				nextToken; // skip else
				parseItems!statement(elseStatements, ScopeKind.no_scope);
			}
		}
		else
		{
			parseItems!parse_declaration(thenStatements, ScopeKind.no_scope);
			if (tok.type == TokenType.ELSE_SYM) { /* ... "else" <decl> */
				nextToken; // skip else
				parseItems!parse_declaration(elseStatements, ScopeKind.no_scope);
			}
		}
	}

	AstIndex parse_hash_if() /* "#if/#version" <paren_expr> <statement/decl> */
	{
		TokenIndex start = tok.index;
		if (tok.type == TokenType.HASH_IF)
		{
			nextToken; // skip #if
			AstIndex condition = paren_expr();
			AstNodes thenStatements;
			AstNodes elseStatements;
			parseStaticIfThenElse(thenStatements, elseStatements);
			return make!StaticIfDeclNode(start, AstIndex.init, AstIndex.init, 0, condition, thenStatements, elseStatements);
		}
		else
		{
			nextToken; // skip #version
			expectAndConsume(TokenType.LPAREN, "#version");
			Identifier versionId = expectIdentifier("#version(");
			expectAndConsume(TokenType.RPAREN, "#version(id");
			AstNodes thenStatements;
			AstNodes elseStatements;
			parseStaticIfThenElse(thenStatements, elseStatements);
			return make!StaticVersionDeclNode(start, AstIndex.init, AstIndex.init, 0, versionId, thenStatements, elseStatements);
		}
	}

	AstIndex parse_hash_foreach() /* "#foreach" "(" [<index_id>], <val_id> ";" <ct_expr> ")" <statement> */
	{
		TokenIndex start = tok.index;
		nextToken; // skip "#foreach"

		expectAndConsume(TokenType.LPAREN); // (

		AstNodes init_statements;

		// <init>
		Identifier keyId = expectIdentifier;
		Identifier valId;
		if (tok.type == TokenType.COMMA)
		{
			nextToken; // skip ","
			valId = expectIdentifier;
			expectAndConsume(TokenType.SEMICOLON);
		}
		else if (tok.type == TokenType.SEMICOLON)
		{
			valId = keyId;
			keyId = Identifier.init;
			expectAndConsume(TokenType.SEMICOLON);
		}
		else
		{
			context.unrecoverable_error(tok.index,
				"Expected `;` after key and value of #foreach, instead got `%s`",
				context.getTokenString(tok.index));
		}

		// <ct_expr>
		AstIndex ct_expr = expr(PreferType.no);
		expectAndConsume(TokenType.RPAREN);

		AstIndex body_start = AstIndex(context.astBuffer.uintLength);
		AstNodes body;
		statement_as_array(body);
		AstIndex after_body = AstIndex(context.astBuffer.uintLength);
		return make!StaticForeachDeclNode(start, AstIndex.init, AstIndex.init, 0, currentScopeIndex, keyId, valId, ct_expr, body, body_start, after_body);
	}

	AstNodes parse_enum_body(AstIndex type) { // { id [= val], ... }
		expectAndConsume(TokenType.LCURLY);
		AstNodes members;
		ushort varIndex = 0;
		while (tok.type != TokenType.RCURLY)
		{
			if (tok.type == TokenType.EOI) break;

			TokenIndex start = tok.index;
			Identifier id = expectIdentifier;
			AstIndex value;

			if (tok.type == TokenType.EQUAL)
			{
				nextToken; // skip "="
				value = expr(PreferType.no);
			}

			auto member = makeDecl!EnumMemberDecl(start, currentScopeIndex, type, value, id);
			EnumMemberDecl* memberNode = context.getAst!EnumMemberDecl(member);
			memberNode.scopeIndex = varIndex++;
			members.put(context.arrayArena, member);

			if (tok.type == TokenType.COMMA) {
				nextToken; // skip ","
			} else break;
		}
		expectAndConsume(TokenType.RCURLY);
		return members;
	}

	void parse_block(ref AstNodes statements) // "{" <statement>* "}"
	{
		expectAndConsume(TokenType.LCURLY);
		while (tok.type != TokenType.RCURLY)
		{
			if (tok.type == TokenType.EOI) break;
			statement(statements);
		}
		expectAndConsume(TokenType.RCURLY);
	}

	AstIndex block_stmt() // <block_statement> ::= "{" <statement>* "}"
	{
		version(print_parse) auto s1 = scop("block_stmt %s", loc);
		TokenIndex start = tok.index;
		ScopeTempData scope_temp = pushScope("Block", ScopeKind.local);
		AstNodes statements;
		parse_block(statements);
		popScope(scope_temp);
		return make!BlockStmtNode(start, statements);
	}

	void statement_as_array(ref AstNodes statements)
	{
		if (tok.type == TokenType.LCURLY)
		{
			parse_block(statements);
		}
		else
		{
			statement(statements);
		}
	}

	void statement(ref AstNodes items)
	{
		version(print_parse) auto s1 = scop("statement %s", loc);
		TokenIndex start = tok.index;
		switch (tok.type)
		{
			// declarations
			case TokenType.ALIAS_SYM:
				items.put(context.arrayArena, parse_alias());
				return;
			case TokenType.STRUCT_SYM, TokenType.UNION_SYM:
				items.put(context.arrayArena, parse_struct());
				return;
			case TokenType.ENUM:
				items.put(context.arrayArena, parse_enum());
				return;
			case TokenType.IMPORT_SYM:
				items.put(context.arrayArena, parse_import());
				return;
			case TokenType.HASH_IF, TokenType.HASH_VERSION:
				items.put(context.arrayArena, parse_hash_if());
				return;
			case TokenType.HASH_ASSERT:
				items.put(context.arrayArena, parse_hash_assert());
				return;
			case TokenType.HASH_FOREACH:
				items.put(context.arrayArena, parse_hash_foreach());
				return;

			// statements
			case TokenType.IF_SYM: /* "if" <paren_expr> <statement> */
				nextToken;
				AstIndex condition = paren_expr();
				ScopeTempData scope_temp = pushScope("Then", ScopeKind.local);
				AstNodes thenStatements;
				statement_as_array(thenStatements);
				popScope(scope_temp);
				AstNodes elseStatements;
				if (tok.type == TokenType.ELSE_SYM) { /* ... "else" <statement> */
					nextToken;
					ScopeTempData scope_temp2 = pushScope("Else", ScopeKind.local);
					statement_as_array(elseStatements);
					popScope(scope_temp2);
				}
				AstIndex stmt = make!IfStmtNode(start, condition, thenStatements, elseStatements);
				items.put(context.arrayArena, stmt);
				return;
			case TokenType.WHILE_SYM:  /* "while" <paren_expr> <statement> */
				nextToken;
				ScopeTempData scope_temp = pushScope("While", ScopeKind.local);
				AstIndex condition = paren_expr();
				AstNodes statements;
				statement_as_array(statements);
				popScope(scope_temp);
				AstIndex stmt = make!WhileStmtNode(start, condition, statements);
				items.put(context.arrayArena, stmt);
				return;
			case TokenType.DO_SYM:  /* "do" <statement> "while" <paren_expr> ";" */
				nextToken;
				ScopeTempData scope_temp = pushScope("do", ScopeKind.local);
				AstNodes statements;
				statement_as_array(statements);
				expectAndConsume(TokenType.WHILE_SYM);
				AstIndex condition = paren_expr();
				popScope(scope_temp);
				expectAndConsume(TokenType.SEMICOLON);
				AstIndex stmt = make!DoWhileStmtNode(start, condition, statements);
				items.put(context.arrayArena, stmt);
				return;
			case TokenType.FOR_SYM:  /* "for" "(" <statement> ";" <statement> ";" "while" <paren_expr> ";" */
				items.put(context.arrayArena, parse_for());
				return;
			case TokenType.SWITCH_SYM:
				items.put(context.arrayArena, parse_switch());
				return;
			case TokenType.RETURN_SYM:  /* return <expr> */
				nextToken;
				AstIndex expression = tok.type != TokenType.SEMICOLON ? expr(PreferType.no) : AstIndex.init;
				expectAndConsume(TokenType.SEMICOLON);
				context.assertf(declarationOwner.isDefined && declarationOwner.astType(context) == AstType.decl_function, start, "Return statement is not inside function");
				AstIndex stmt = make!ReturnStmtNode(start, declarationOwner, expression);
				items.put(context.arrayArena, stmt);
				return;
			case TokenType.BREAK_SYM:  /* break; */
				nextToken;
				expectAndConsume(TokenType.SEMICOLON);
				AstIndex stmt = make!BreakStmtNode(start);
				items.put(context.arrayArena, stmt);
				return;
			case TokenType.CONTINUE_SYM:  /* continue; */
				nextToken;
				expectAndConsume(TokenType.SEMICOLON);
				AstIndex stmt = make!ContinueStmtNode(start);
				items.put(context.arrayArena, stmt);
				return;
			case TokenType.SEMICOLON:  /* ";" */
				context.error(tok.index, "Cannot use `;` as an empty statement. Use `{}` instead");
				nextToken;
				return;
			case TokenType.LCURLY:  /* "{" { <statement> } "}" */
				items.put(context.arrayArena, block_stmt());
				return;
			case TokenType.AT:
				parse_attribute(items);
				return;
			case TokenType.TYPE_AUTO:
				parse_auto_decl(items);
				return;
			default:
			{
				// expression or var/func declaration
				version(print_parse) auto s2 = scop("default %s", loc);
				// <expr> ";" / var decl / func decl
				AstIndex expression = parse_expr_or_id_decl(ConsumeTerminator.yes, TokenType.SEMICOLON);
				items.put(context.arrayArena, expression);
				return;
			}
		}
	}

	void parse_auto_decl(ref AstNodes items)
	{
		TokenIndex start = tok.index;
		AstIndex body_start = AstIndex(context.astBuffer.uintLength);
		nextToken; // skip auto
		AstIndex declIndex = parse_var_func_declaration_after_type(start, body_start, CommonAstNodes.type_auto, ConsumeTerminator.yes, TokenType.SEMICOLON);
		AstNode* declNode = declIndex.get_node(context);
		switch(declNode.astType)
		{
			case AstType.decl_var:
				if (declNode.as!VariableDeclNode(context).initializer.isUndefined) {
					context.error(declNode.loc, "variables declared as `auto` must have an initializer");
				}
				break;
			case AstType.decl_template:
			case AstType.decl_function:
				context.error(declNode.loc, "functions cannot return `auto`");
				break;
			default:
				context.internal_error(declNode.loc, "auto declaration of %s", declNode.astType);
		}
		items.put(context.arrayArena, declIndex);
	}

	AstIndex parse_for() // "for" "(" <init>,... ";" <cond> ";" <increment> ")" <statement>
	{
		TokenIndex start = tok.index;
		nextToken; // skip "for"

		expectAndConsume(TokenType.LPAREN); // (

		ScopeTempData scope_temp = pushScope("For", ScopeKind.local);
		scope(exit) popScope(scope_temp);

		AstNodes init_statements;

		// <init>
		while (tok.type != TokenType.SEMICOLON) // check after trailing comma
		{
			AstIndex init_stmt = parse_expr_or_id_decl(ConsumeTerminator.no);
			init_statements.put(context.arrayArena, init_stmt);

			if (tok.type == TokenType.COMMA)
				nextToken; // skip ","
			else break;
		}
		expectAndConsume(TokenType.SEMICOLON);

		// <cond>
		AstIndex condition;
		if (tok.type != TokenType.SEMICOLON) {
			condition = expr(PreferType.no);
		}
		expectAndConsume(TokenType.SEMICOLON);

		AstNodes increment_statements;
		// <increment>
		while (tok.type != TokenType.RPAREN) // check after trailing comma
		{
			AstIndex incExpr = expr(PreferType.no);
			AstNode* incExprNode = context.getAstNode(incExpr);
			increment_statements.put(context.arrayArena, incExpr);

			if (tok.type == TokenType.COMMA)
				nextToken; // skip ","
			else break;
		}
		expectAndConsume(TokenType.RPAREN);

		AstNodes statements;
		statement_as_array(statements);

		return make!ForStmtNode(start, init_statements, condition, increment_statements, statements);
	}

	AstIndex parse_switch() /* "switch" "(" <expr> ")" "{" <switch_case> "}" */
	{
		TokenIndex start = tok.index;
		nextToken; // skip "switch"

		AstIndex condition = paren_expr();

		Array!SwitchCase cases;
		AstIndex elseBlock;
		expectAndConsume(TokenType.LCURLY);
		while (tok.type != TokenType.RCURLY)
		{
			if (tok.type == TokenType.EOI) break;

			// case expression
			if (tok.type != TokenType.ELSE_SYM) /* <expr> <block> */
			{
				auto expr = expr(PreferType.no);
				auto block = block_stmt();
				cases.put(context.arrayArena, SwitchCase(expr, block));
			}
			else /* "else" <block> */
			{
				nextToken; // skip "else"
				if (elseBlock.isDefined)
				{
					// todo: error
					// must occur 0 or 1 times
				}
				elseBlock = block_stmt();
			}
		}
		expectAndConsume(TokenType.RCURLY);

		return make!SwitchStmtNode(start, condition, elseBlock, cases);
	}

	AstIndex paren_expr() { /* <paren_expr> ::= "(" <expr> ")" */
		version(print_parse) auto s1 = scop("paren_expr %s", loc);
		expectAndConsume(TokenType.LPAREN);
		auto res = expr(PreferType.no);
		expectAndConsume(TokenType.RPAREN);
		return res;
	}

	AstIndex expr(PreferType preferType, int rbp = 0)
	{
		Token t = tok;
		nextToken;

		NullInfo null_info = g_tokenLookups.null_lookup[t.type];
		AstIndex node = null_info.parser_null(this, preferType, t, null_info.rbp);
		int nbp = null_info.nbp; // next bp
		int lbp = g_tokenLookups.left_lookup[tok.type].lbp;
		//writefln("%s %s rbp %s lbp %s nbp %s", t, tok, rbp, lbp, nbp);

		while (rbp < lbp && lbp < nbp)
		{
			t = tok;
			nextToken;
			LeftInfo left_info = g_tokenLookups.left_lookup[t.type];
			nbp = left_info.nbp; // next bp
			// parser can modify nbp in case infix operator want to become postfix, like *
			node = left_info.parser_left(this, preferType, t, left_info.rbp, node, nbp);
			lbp = g_tokenLookups.left_lookup[tok.type].lbp;
			//writefln("%s %s rbp %s lbp %s nbp %s", t, tok, rbp, lbp, nbp);
		}

		return node;
	}
}

/// Controls the expression parser
/// Forces * expression to be parsed as pointer type
/// Disables slice parsing for [] expression
enum PreferType : bool {
	no = false,
	yes = true,
}

/// min and max binding powers
enum MIN_BP = 0;
enum MAX_BP = 10000;
enum COMMA_PREC = 10;

alias LeftParser = AstIndex function(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex left, ref int nbp);
alias NullParser = AstIndex function(ref Parser p, PreferType preferType, Token token, int rbp);

AstIndex left_error_parser(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex left, ref int nbp)
{
	if (token.type == TokenType.EOI)
		p.context.unrecoverable_error(token.index, "Unexpected end of input");
	else
		p.context.unrecoverable_error(token.index, "%s is not an expression", token.type);
}

AstIndex null_error_parser(ref Parser p, PreferType preferType, Token token, int rbp)
{
	if (token.type == TokenType.EOI)
		p.context.unrecoverable_error(token.index, "Unexpected end of input");
	else
		p.context.unrecoverable_error(token.index, "%s is not an expression", token.type);
}

struct LeftInfo
{
	LeftParser parser_left = &left_error_parser;
	int lbp = MIN_BP;
	int rbp = MIN_BP;
	int nbp = MIN_BP;
}

struct NullInfo
{
	NullParser parser_null = &null_error_parser;
	int lbp = MIN_BP;
	int rbp = MIN_BP;
	int nbp = MIN_BP;
}

struct TokenLookups
{
	LeftInfo[TokenType.max+1] left_lookup;
	NullInfo[TokenType.max+1] null_lookup;
}

__gshared immutable TokenLookups g_tokenLookups = cexp_parser();

private TokenLookups cexp_parser()
{
	TokenLookups res;

	TokenType strToTok(string str)
	{
		import std.algorithm.searching : countUntil;
		ptrdiff_t pos = countUntil(tokStrings, str);
		assert(pos != -1, str ~ " not found");
		return cast(TokenType)pos;
	}

	void _RegisterNull(int lbp, int rbp, int nbp, NullParser p, string[] tokens...) {
		foreach (string token; tokens) res.null_lookup[strToTok(token)] = NullInfo(p, lbp, rbp, nbp);
	}

	void _RegisterLeft(int lbp, int rbp, int nbp, LeftParser p, string[] tokens...) {
		foreach (string token; tokens) res.left_lookup[strToTok(token)] = LeftInfo(p, lbp, rbp, nbp);
	}

	void nilfix(int bp, NullParser nud, string[] tokens...) {
		_RegisterNull(MIN_BP, MIN_BP, MAX_BP, nud, tokens);
	}

	void prefix(int bp, NullParser nud, string[] tokens...) {
		_RegisterNull(MIN_BP, bp, MAX_BP, nud, tokens);
	}

	void suffix(int bp, LeftParser led, string[] tokens...) {
		_RegisterLeft(bp, MIN_BP, MAX_BP, led, tokens);
	}

	void infixL(int bp, LeftParser led, string[] tokens...) {
		_RegisterLeft(bp, bp, bp + 1, led, tokens);
	}

	void infixR(int bp, LeftParser led, string[] tokens...) {
		_RegisterLeft(bp, bp - 1, bp + 1, led, tokens);
	}

	void infixN(int bp, LeftParser led, string[] tokens...) {
		_RegisterLeft(bp, bp, bp, led, tokens);
	}

	// Compare the code below with this table of C operator precedence:
	// http://en.cppreference.com/w/c/language/operator_precedence

	suffix(310, &leftIncDec, ["++", "--"]);
	infixL(310, &leftFuncCall, "(");
	infixL(310, &leftIndex, "[");
	infixL(310, &leftOpDot, ".");
	//infixL(310, &leftBinaryOp, "->");

	// 29 -- binds to everything except function call, indexing, postfix ops
	prefix(290, &nullPrefixOp, ["+", "-", "!", "~", "*", "&", "++", "--", "@"]);
	prefix(290, &nullPrefixAttribute, ["@"]);
	prefix(290, &nullCast, "cast");

	infixL(250, &leftFunctionOp, ["function"]);
	infixL(250, &leftStarOp, ["*"]);
	infixL(250, &leftBinaryOp, ["/", "%"]);

	infixL(230, &leftBinaryOp, ["+", "-"]);
	infixL(210, &leftBinaryOp, ["<<", ">>", ">>>"]);
	infixL(190, &leftBinaryOp, ["<", ">", "<=", ">="]);
	infixL(170, &leftBinaryOp, ["!=", "=="]);

	infixL(150, &leftBinaryOp, "&");
	infixL(130, &leftBinaryOp, "^");
	infixL(110, &leftBinaryOp, "|");
	infixL(90, &leftBinaryOp, "&&");
	infixL(70, &leftBinaryOp, "||");

	// Right associative: a = b = 2 is a = (b = 2)
	infixR(30, &leftAssignOp, ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", ">>>=", "&=", "^=", "|="]);

	// 0 precedence -- doesn"t bind until )
	prefix(0, &nullParen, "("); // for grouping

	// 0 precedence -- never used
	nilfix(0, &nullLiteral, [
		"#id", "$id",
		"auto", "noreturn","void", "bool", "null",
		"i8", "i16", "i32", "i64",
		"u8", "u16", "u32", "u64",
		"f32", "f64",
		"$alias", "$type",
		"true", "false",
		"#special_kw",
		"#int_dec_lit", "#int_bin_lit", "#int_hex_lit", "#float_dec_lit", "#str_lit", "#char_lit"]
	);
	nilfix(0, &null_error_parser, [")", "]", ":", "#eoi", ";"]);
	return res;
}

// Null Denotations -- tokens that take nothing on the left

AstIndex parseIntLiteralType(string suffix, TokenIndex tok, CompilationContext* c) {
	if (suffix.length == 0) return AstIndex();
	switch(suffix) {
		case "i8":  return CommonAstNodes.type_i8;
		case "i16": return CommonAstNodes.type_i16;
		case "i32": return CommonAstNodes.type_i32;
		case "i64": return CommonAstNodes.type_i64;
		case "u8":  return CommonAstNodes.type_u8;
		case "u16": return CommonAstNodes.type_u16;
		case "u32": return CommonAstNodes.type_u32;
		case "u64": return CommonAstNodes.type_u64;
		default: c.internal_error(tok, "Unexpected type suffix `%s`", suffix);
	}
}

// id, int_literal, string_literal
AstIndex nullLiteral(ref Parser p, PreferType preferType, Token token, int rbp) {
	import std.algorithm : filter;
	import std.range : walkLength;
	import std.conv : parse;
	import std.typecons : Yes;
	CompilationContext* c = p.context;
	switch(token.type) with(TokenType)
	{
		case IDENTIFIER:
			Identifier id = p.makeIdentifier(token.index);
			return p.make!NameUseExprNode(token.index, p.currentScopeIndex, id);
		case CASH_IDENTIFIER:
			Identifier id = p.makeIdentifier(token.index);
			if (id.index >= commonId_builtin_func_first && id.index <= commonId_builtin_func_last)
			{
				uint builtinIndex = id.index - commonId_builtin_func_first;
				return builtinFuncsArray[builtinIndex];
			}
			c.error(token.index, "Invalid $ identifier %s", c.idString(id));
			return p.make!NameUseExprNode(token.index, p.currentScopeIndex, id);
		case SPECIAL_KW:
			Identifier id = p.makeIdentifier(token.index);
			c.assertf(id.index >= commonId_special_keyword_first && id.index <= commonId_special_keyword_last,
				"Invalid special keyword %s", c.idString(id));
			uint keywordIndex = id.index - commonId_special_keyword_first;
			SpecialKeyword kw = cast(SpecialKeyword)keywordIndex;
			IrIndex irValue = eval_literal_special(kw, token.index, p.currentScopeIndex, c);
			return c.appendAst!SpecialLiteralExprNode(token.index, irValue, kw);
		case NULL:
			return p.makeExpr!NullLiteralExprNode(token.index);
		case TRUE_LITERAL:
			return p.makeExpr!BoolLiteralExprNode(token.index, true);
		case FALSE_LITERAL:
			return p.makeExpr!BoolLiteralExprNode(token.index, false);
		case STRING_LITERAL:
			// omit " at the start and end of token
			string value = cast(string)c.getTokenString(token.index)[1..$-1];

			// handle escape sequences and copy string to RO buffer.
			value = handleEscapedString(c.roStaticDataBuffer, value);
			c.roStaticDataBuffer.put(0); // add zero terminator

			AstIndex type = CommonAstNodes.type_u8Slice;
			IrIndex irValue = makeStringLiteralIrConstant(value, p.currentModule.objectSymIndex, c);
			return p.make!StringLiteralExprNode(token.index, type, irValue, value);
		case CHAR_LITERAL:
			// omit ' at the start and end of token
			string value = cast(string)c.getTokenString(token.index)[1..$-1];
			dchar charVal = getCharValue(value);
			return p.makeExpr!IntLiteralExprNode(token.index, cast(uint)charVal);
		case INT_DEC_LITERAL:
			string value = cast(string)c.getTokenString(token.index);
			auto filtered = value.filter!(c => c != '_');
			auto result = filtered.parse!(ulong, typeof(filtered), Yes.doCount);
			AstIndex type = parseIntLiteralType(value[$-filtered.walkLength..$], token.index, c);
			return c.appendAst!IntLiteralExprNode(token.index, type, result.data);
		case INT_HEX_LITERAL:
			string value = cast(string)c.getTokenString(token.index);
			auto filtered = value[2..$].filter!(c => c != '_');
			auto result = filtered.parse!(ulong, typeof(filtered), Yes.doCount)(16); // skip 0x, 0X
			AstIndex type = parseIntLiteralType(value[$-filtered.walkLength..$], token.index, c);
			return c.appendAst!IntLiteralExprNode(token.index, type, result.data);
		case INT_BIN_LITERAL:
			string value = cast(string)c.getTokenString(token.index);
			auto filtered = value[2..$].filter!(c => c != '_');
			auto result = filtered.parse!(ulong, typeof(filtered), Yes.doCount)(2); // skip 0b, 0B
			AstIndex type = parseIntLiteralType(value[$-filtered.walkLength..$], token.index, c);
			return c.appendAst!IntLiteralExprNode(token.index, type, result.data);
		case FLOAT_DEC_LITERAL:
			string value = cast(string)c.getTokenString(token.index);
			auto filtered = value.filter!(c => c != '_');
			auto result = filtered.parse!(double, typeof(filtered), Yes.doCount);
			AstIndex type;
			if (!filtered.empty) {
				string suffix = value[$-filtered.walkLength..$];
				if (suffix == "f32")
					type = CommonAstNodes.type_f32;
				else {
					c.assertf(suffix == "f64", token.index, "Unexpected type suffix `%s`", suffix);
					type = CommonAstNodes.type_f64;
				}
			}
			return c.appendAst!FloatLiteralExprNode(token.index, type, result.data);
		case TYPE_AUTO:
			c.error(token.index, "`auto` can only be used to declare local varibles and globals");
			BasicType t = token.type.tokenTypeToBasicType;
			return c.basicTypeNodes(t);
		case TYPE_NORETURN, TYPE_VOID, TYPE_BOOL,
			TYPE_I8, TYPE_I16, TYPE_I32, TYPE_I64, TYPE_U8, TYPE_U16, TYPE_U32, TYPE_U64,
			TYPE_F32, TYPE_F64,
			TYPE_ALIAS, TYPE_TYPE:
			BasicType t = token.type.tokenTypeToBasicType;
			return c.basicTypeNodes(t);
		default:
			c.internal_error("nullLiteral %s", token.type);
	}
}

// Arithmetic grouping
AstIndex nullParen(ref Parser p, PreferType preferType, Token token, int rbp) {
	AstIndex r = p.expr(PreferType.no, rbp);
	p.expectAndConsume(TokenType.RPAREN);
	//r.flags |= NFLG.parenthesis; // NOTE: needed if ternary operator is needed
	return r;
}

AstIndex nullPrefixAttribute(ref Parser p, PreferType preferType, Token token, int rbp) {
	if (!preferType) p.context.unrecoverable_error(token.index, "Unexpected attribute");
	while(true) {
		p.parse_single_attribute(p.tok.index);
		if (p.tok.type == TokenType.AT) {
			p.nextToken; // skip @
			continue;
		}
		break;
	}
	return p.expr(preferType, rbp);
}

// Prefix operator
// ["+", "-", "!", "~", "*", "&", "++", "--"] <expr>
AstIndex nullPrefixOp(ref Parser p, PreferType preferType, Token token, int rbp) {
	AstIndex right = p.expr(PreferType.no, rbp);
	UnOp op;
	switch(token.type) with(TokenType)
	{
		case PLUS: return right;
		case MINUS:
			AstNode* rightNode = p.context.getAstNode(right);
			if (rightNode.astType == AstType.literal_int) {
				(cast(IntLiteralExprNode*)rightNode).negate(token.index, *p.context);
				return right;
			} else if (rightNode.astType == AstType.literal_float) {
				(cast(FloatLiteralExprNode*)rightNode).negate(token.index, *p.context);
				return right;
			}
			op = UnOp.GENERIC_MINUS;
			break;
		case NOT: op = UnOp.logicalNot; break;
		case TILDE: op = UnOp.bitwiseNot; break;
		case STAR: op = UnOp.deref; break;
		case AND: op = UnOp.addrOf; break;
		case PLUS_PLUS: op = UnOp.preIncrement; break;
		case MINUS_MINUS: op = UnOp.preDecrement; break;
		default: p.context.unreachable;
	}
	return p.makeExpr!UnaryExprNode(token.index, op, right);
}

// "cast" "(" <expr> ")" <expr>
AstIndex nullCast(ref Parser p, PreferType preferType, Token token, int rbp) {
	p.expectAndConsume(TokenType.LPAREN);
	AstIndex type = p.expr(PreferType.yes, 0);
	p.expectAndConsume(TokenType.RPAREN);
	AstIndex right = p.expr(PreferType.no, rbp);
	return p.make!TypeConvExprNode(token.index, type, right);
}

// Left Denotations -- tokens that take an expression on the left

// <expr> "++" / "--"
AstIndex leftIncDec(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex left, ref int nbp) {
	UnOp op;
	switch(token.type) with(TokenType)
	{
		case PLUS_PLUS: op = UnOp.postIncrement; break;
		case MINUS_MINUS: op = UnOp.postDecrement; break;
		default: p.context.unreachable;
	}
	return p.makeExpr!UnaryExprNode(token.index, op, left);
}

// <expr> "[" "]"
// <expr> "[" <expr> "," <expr>+ "]"
// <expr> "[" <expr> .. <expr> "]"
AstIndex leftIndex(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex array, ref int nbp) {
	AstNodes indices;
	if (p.tok.type == TokenType.RBRACKET)
	{
		p.nextToken;
		return p.makeExpr!IndexExprNode(token.index, p.currentScopeIndex, array, indices);
	}
	AstIndex index = p.expr(PreferType.no, 0);
	if (p.tok.type == TokenType.RBRACKET)
	{
		p.nextToken;
		indices.put(p.context.arrayArena, index);
		return p.makeExpr!IndexExprNode(token.index, p.currentScopeIndex, array, indices);
	}

	if (preferType == PreferType.yes)
	{
		// it is type
		p.expectAndConsume(TokenType.RBRACKET);
		assert(false);
	}
	else
	{
		// it is expression
		p.expectAndConsume(TokenType.DOT_DOT);
		AstIndex index2 = p.expr(PreferType.no, 0);
		p.expectAndConsume(TokenType.RBRACKET);
		return p.makeExpr!SliceExprNode(token.index, array, index, index2);
	}
}

// member access <expr> . <expr>
AstIndex leftOpDot(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex left, ref int nbp)
{
	Identifier id;
	if (p.tok.type == TokenType.IDENTIFIER)
	{
		id = p.makeIdentifier(p.tok.index);
		p.nextToken; // skip id
	}
	else
	{
		p.context.error(token.index,
			"Expected identifier after '.', while got '%s'",
			p.context.getTokenString(p.tok.index));
	}
	return p.make!MemberExprNode(token.index, p.currentScopeIndex, left, id);
}

AstIndex leftFunctionOp(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex returnType, ref int nbp) {
	CallConvention callConvention = p.context.defaultCallConvention;
	auto sig = p.makeDecl!FunctionSignatureNode(token.index, returnType, AstNodes.init, callConvention);
	AstIndex prevOwner = p.declarationOwner; // change the owner, so that parameters are inferred as local
	p.declarationOwner = sig;
	ScopeTempData scope_temp = p.pushScope(null, ScopeKind.local);
	p.currentScopeIndex.get_scope(p.context).owner = sig;
	p.parseParameters(sig);
	p.popScope(scope_temp);
	p.declarationOwner = prevOwner;
	// we don't have to register parameter names, since we have no body
	sig.setState(p.context, AstNodeState.name_register_nested_done);
	return p.make!PtrTypeNode(token.index, CommonAstNodes.type_type, sig);
}

// multiplication or pointer type
// <expr> * <expr> or <expr>*
AstIndex leftStarOp(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex left, ref int nbp) {
	switch (p.tok.type) with(TokenType)
	{
		case STAR, COMMA, RPAREN, RBRACKET, LBRACKET, SEMICOLON, FUNCTION_SYM /*,DELEGATE_SYM*/:
			// pointer
			nbp = 311; // make current node into a postfix op
			return p.make!PtrTypeNode(token.index, CommonAstNodes.type_type, left);
		case DOT:
			// hack for postfix star followed by dot
			AstIndex ptr = p.make!PtrTypeNode(token.index, CommonAstNodes.type_type, left);
			Token tok = p.tok;
			p.nextToken; // skip dot
			int nbpDot;
			return leftOpDot(p, PreferType.no, tok, 0, ptr, nbpDot);
		default:
			// otherwise it is multiplication
			break;
	}

	if (preferType)
	{
		// pointer
		return p.make!PtrTypeNode(token.index, CommonAstNodes.type_type, left);
	}

	// otherwise it is multiplication
	AstIndex right = p.expr(PreferType.no, rbp);
	BinOp op = BinOp.GENERIC_MUL;
	return p.makeExpr!BinaryExprNode(token.index, op, left, right);
}

// Normal binary operator <expr> op <expr>
AstIndex leftBinaryOp(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex left, ref int nbp) {
	AstIndex right = p.expr(PreferType.no, rbp);
	BinOp op;
	switch(token.type) with(TokenType)
	{
		// logic ops
		case AND_AND: op = BinOp.LOGIC_AND; break;                // &&
		case OR_OR: op = BinOp.LOGIC_OR; break;                   // ||
		case EQUAL_EQUAL: op = BinOp.EQUAL; break;                // ==
		case NOT_EQUAL: op = BinOp.NOT_EQUAL; break;              // !=
		case MORE: op = BinOp.GENERIC_GREATER; break;             // >
		case MORE_EQUAL: op = BinOp.GENERIC_GREATER_EQUAL; break; // >=
		case LESS: op = BinOp.GENERIC_LESS; break;                // <
		case LESS_EQUAL: op = BinOp.GENERIC_LESS_EQUAL; break;    // <=

		// arithmetic ops
		case AND: op = BinOp.BITWISE_AND; break;                  // &
		case OR: op = BinOp.BITWISE_OR; break;                    // |
		case PERCENT: op = BinOp.GENERIC_INT_REM; break;          // %
		case LESS_LESS: op = BinOp.SHL; break;                    // <<
		case MORE_MORE: op = BinOp.ASHR; break;                   // >>
		case MORE_MORE_MORE: op = BinOp.SHR; break;               // >>>
		case MINUS: op = BinOp.GENERIC_MINUS; break;              // -
		case PLUS: op = BinOp.GENERIC_PLUS; break;                // +
		case SLASH: op = BinOp.GENERIC_DIV; break;                // /
		case XOR: op = BinOp.XOR; break;                          // ^

		default:
			p.context.internal_error(token.index, "parse leftBinaryOp %s", token.type);
	}
	return p.makeExpr!BinaryExprNode(token.index, op, left, right);
}

// Binary assignment operator <expr> op= <expr>
AstIndex leftAssignOp(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex left, ref int nbp) {
	AstIndex right = p.expr(PreferType.no, rbp);
	BinOp op;
	switch(token.type) with(TokenType)
	{
		// arithmetic opEquals
		case EQUAL: op = BinOp.ASSIGN; break;                     // =
		case AND_EQUAL: op = BinOp.BITWISE_AND_ASSIGN; break;     // &=
		case OR_EQUAL: op = BinOp.BITWISE_OR_ASSIGN; break;       // |=
		case PERCENT_EQUAL: op = BinOp.GENERIC_INT_REM_ASSIGN; break; // %=
		case LESS_LESS_EQUAL: op = BinOp.SHL_ASSIGN; break;       // <<=
		case MORE_MORE_EQUAL: op = BinOp.ASHR_ASSIGN; break;      // >>=
		case MORE_MORE_MORE_EQUAL: op = BinOp.SHR_ASSIGN; break;  // >>>=
		case MINUS_EQUAL: op = BinOp.GENERIC_MINUS_ASSIGN; break; // -=
		case PLUS_EQUAL: op = BinOp.GENERIC_PLUS_ASSIGN; break;   // +=
		case SLASH_EQUAL: op = BinOp.GENERIC_DIV_ASSIGN; break;   // /=
		case STAR_EQUAL: op = BinOp.GENERIC_MUL_ASSIGN; break;    // *=
		case XOR_EQUAL: op = BinOp.XOR_ASSIGN; break;             // ^=
		default:
			p.context.internal_error(token.index, "parse leftAssignOp %s", token.type);
	}
	AstNode* leftNode = p.context.getAstNode(left);
	leftNode.flags |= AstFlags.isLvalue;

	AstIndex assignExpr = p.makeExpr!BinaryExprNode(token.index, op, left, right);
	AstNode* assignExprNode = p.context.getAstNode(assignExpr);
	assignExprNode.flags |= BinaryOpFlags.isAssignment;

	return assignExpr;
}

// <expr> "(" <expr_list> ")"
AstIndex leftFuncCall(ref Parser p, PreferType preferType, Token token, int unused_rbp, AstIndex callee, ref int nbp) {
	AstNodes args;
	ushort flags = p.parse_argument_list(args);
	AstIndex callExpr = p.makeExpr!CallExprNode(token.index, p.currentScopeIndex, callee, args);
	callExpr.flags(p.context) |= flags;
	return callExpr;
}
