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
module fe.ast.parser;

import std.format : formattedWrite;
import std.string : format;
import std.range : repeat;
import std.stdio;
import std.conv : to;

import all;


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

struct AttribState
{
	// Number of attributes on the top of `attributeStack`, that
	// will be added to the following declaration
	ushort numEffectiveAttributes;
	// Number of attributes on the top of `attributeStack`, that
	// will be dropped from the stack by the following declaration
	ushort numImmediateAttributes;
}

//version = print_parse;
struct Parser
{
	CompilationContext* context;
	ModuleDeclNode* currentModule;
	/// For member functions
	/// mmodule, struct or function
	AstIndex declarationOwner;
	Scope* currentScope;
	AstIndex currentScopeIndex() { return currentScope.get_ast_index(context); }

	Token tok;

	AttribState attribState;
	// Attributes affecting next declaration
	AstNodes attributeStack;

	// TODO: for now only immediate attributes are implemented
	// returns 0 or AstFlags.hasAttributes flag. Must be added to node flags.
	ushort createAttributeInfo() {
		if (attribState.numEffectiveAttributes == 0) return 0;

		AstNodes attributes;
		attributes.voidPut(context.arrayArena, attribState.numEffectiveAttributes);
		auto offset = attributeStack.length - attribState.numEffectiveAttributes;
		foreach(i; 0..attribState.numEffectiveAttributes) {
			attributes[i] = attributeStack[offset + i];
		}
		attributeStack.unput(attribState.numImmediateAttributes);
		attribState.numEffectiveAttributes -= attribState.numImmediateAttributes;
		attribState.numImmediateAttributes = 0;
		context.appendAst!AttributeInfo(attributes);
		return AstFlags.hasAttributes;
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
	// Will attach declaration if needed
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
		return context.idMap.getOrRegNoDup(context, str);
	}

	Identifier expectIdentifier(string after = null)
	{
		TokenIndex index = tok.index;
		expectAndConsume(TokenType.IDENTIFIER, after);
		Identifier id = makeIdentifier(index);
		return id;
	}

	// temp data is used instead of separate stack to push/pop
	AstIndex pushScope(string name, ScopeKind kind, ref ScopeTempData temp)
	{
		temp.prev = attribState;
		attribState = AttribState(0, 0);

		AstIndex newScopeIndex = context.appendAst!Scope;
		Scope* newScope = context.getAst!Scope(newScopeIndex);
		newScope.debugName = name;
		newScope.kind = kind;

		if (currentScope)
			newScope.parentScope = currentScope.get_ast_index(context);
		currentScope = newScope;

		return newScopeIndex;
	}

	void popScope(ScopeTempData temp)
	{
		attribState = temp.prev;

		if (currentScope.parentScope) {
			currentScope = currentScope.parentScope.get_scope(context);
		}
		else
			currentScope = null;
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

		ScopeTempData scope_temp;
		mod.memberScope = pushScope("Module", ScopeKind.global, scope_temp);
			parse_module();
			mod.declarations = parse_declarations(TokenType.EOI, AstFlags.isGlobal);
		popScope(scope_temp);
	}

	AstNodes parse_declarations(TokenType until, ushort declFlags = 0) { // <declaration>*
		AstNodes declarations;
		ushort varIndex = 0;
		while (tok.type != until)
		{
			if (tok.type == TokenType.EOI) break;
			AstIndex declIndex = parse_declaration;
			AstNode* declNode = context.getAstNode(declIndex);
			declNode.flags |= declFlags;
			if (declNode.astType == AstType.decl_var)
			{
				auto var = declNode.as!VariableDeclNode(context);
				var.scopeIndex = varIndex++;
				if (declNode.isGlobal)
				{
					// register global variable, type is not set yet
					IrIndex globalIndex = context.globals.add();
					IrGlobal* global = context.globals.get(globalIndex);
					var.irValue = ExprValue(globalIndex, ExprValueKind.ptr_to_data, IsLvalue.yes);

					ObjectSymbol sym = {
						kind : ObjectSymbolKind.isLocal,
						sectionIndex : context.builtinSections[ObjectSectionType.rw_data],
						moduleIndex : currentModule.objectSymIndex,
						flags : ObjectSymbolFlags.isMutable,
						id : var.id,
					};

					global.objectSymIndex = context.objSymTab.addSymbol(sym);
				}
			}
			declarations.put(context.arrayArena, declIndex);
		}
		return declarations;
	}

	AstIndex parse_declaration() // <declaration> ::= <func_declaration> / <var_declaration> / <struct_declaration>
	{
		version(print_parse) auto s1 = scop("parse_declaration %s", loc);

		loop:
		while(true)
		switch(tok.type) with(TokenType)
		{
			case ALIAS_SYM: // <alias_decl> ::= "alias" <id> "=" <expr> ";"
				return parse_alias();
			case STRUCT_SYM: // <struct_declaration> ::= "struct" <id> "{" <declaration>* "}"
				return parse_struct();
			case ENUM:
				return parse_enum();
			case IMPORT_SYM:
				return parse_import();
			case MODULE_SYM:
				context.error(tok.index, "Module declaration can only occur as first declaration of the module");
				skipPast(TokenType.SEMICOLON);
				return context.getAstNodeIndex(currentModule);
			case HASH_IF:
				return parse_hash_if();
			case HASH_ASSERT:
				return parse_hash_assert();
			case HASH_FOREACH:
				return parse_hash_foreach();
			case AT:
				TokenIndex start = tok.index;
				nextToken; // skip @
				Identifier attributeId = expectIdentifier("@");
				switch(attributeId.index) {
					case CommonIds.id_extern.index:
						parse_extern_attribute(start);
						break;
					default:
						context.unrecoverable_error(start, "Unknown attribute name `%s`", context.idString(attributeId));
				}
				continue loop;
			default: // <func_declaration> / <var_declaration>
				AstIndex funcIndex = parse_var_func_declaration(ConsumeTerminator.yes, TokenType.SEMICOLON);
				AstNode* node = context.getAstNode(funcIndex);
				return funcIndex;
		}
		assert(false);
	}

	void parse_extern_attribute(TokenIndex start)
	{
		expectAndConsume(TokenType.LPAREN, "@extern");
		Identifier externKindId = expectIdentifier("@extern(");
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
			default:
				context.unrecoverable_error(start, "Unknown @extern kind `%s`", context.idString(externKindId));
		}

		expectAndConsume(TokenType.RPAREN, "@extern(...");
		++attribState.numImmediateAttributes;
		++attribState.numEffectiveAttributes;
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
			statementNode.flags |= AstFlags.isStatement;
			if (consume_terminator) expectAndConsume(var_terminator);
			return expr_or_type;
		}
	}

	enum ConsumeTerminator : bool { no, yes }
	/// var_terminator can be ";" or ",". Used to parse var declaration
	/// or to parse var declaration in foreach
	AstIndex parse_var_func_declaration(ConsumeTerminator consume_terminator, TokenType var_terminator = TokenType.init)
	{
		TokenIndex start = tok.index;
		AstIndex body_start = AstIndex(context.astBuffer.uintLength);
		AstIndex typeIndex = expr(PreferType.yes, 0);
		return parse_var_func_declaration_after_type(start, body_start, typeIndex, consume_terminator, var_terminator);
	}

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

	AstIndex parse_var_func_declaration_after_type(TokenIndex start, AstIndex body_start, AstIndex typeIndex, ConsumeTerminator consume_terminator, TokenType var_terminator = TokenType.init)
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
			AstNode* initializerNode = context.getAstNode(initializerIndex);
			if (!initializerNode.isExpression) {
				const(char)[] tokenString = context.getTokenString(initializerNode.loc);
				context.unrecoverable_error(initializerNode.loc,
					"Variable declaration can be only initialized with expressions, not with %s, '%s'",
					initializerNode.astType, tokenString);
			}
		}

		if (tok.type == TokenType.SEMICOLON || tok.type == TokenType.COMMA) // <var_declaration> ::= <type> <id> (";" / ",")
		{
			version(print_parse) auto s3 = scop("<var_declaration> %s", start);
			if (consume_terminator) expectAndConsume(var_terminator);
			// leave ";" or "," for parent to decide
			return makeDecl!VariableDeclNode(start, currentScopeIndex, typeIndex, initializerIndex, declarationId);
		}
		else if (tok.type == TokenType.LBRACKET) // <func_declaration> ::= <type> <id> "[" <template_params> "]" "(" <param_list> ")" (<block_statement> / ';')
		{
			AstNodes template_params;
			ushort numParamsBeforeVariadic;
			parse_template_parameters(template_params, numParamsBeforeVariadic);
			AstIndex body = parse_func(start, typeIndex, declarationId);
			AstIndex after_body = AstIndex(context.astBuffer.uintLength);
			return makeDecl!TemplateDeclNode(start, currentScopeIndex, template_params, body, body_start, after_body, declarationId, numParamsBeforeVariadic);
		}
		else if (tok.type == TokenType.LPAREN) // <func_declaration> ::= <type> <id> "(" <param_list> ")" (<block_statement> / ';')
		{
			return parse_func(start, typeIndex, declarationId);
		}
		else
		{
			context.unrecoverable_error(tok.index, "Expected '(' or ';', while got '%s'", context.getTokenString(tok.index));
			assert(false);
		}
	}

	AstIndex parse_func(TokenIndex start, AstIndex typeIndex, Identifier declarationId)
	{
		version(print_parse) auto s3 = scop("<func_declaration> %s", start);
		AstNodes params;

		AstIndex parentScope = currentScopeIndex; // need to get parent before push scope
		ScopeTempData scope_temp;
		pushScope(context.idString(declarationId), ScopeKind.local, scope_temp);
		scope(exit) popScope(scope_temp);

		// add this pointer
		if (declarationOwner.astType(context) == AstType.decl_struct)
		{
			AstIndex structName = make!NameUseExprNode(start, currentScopeIndex, declarationOwner.get!StructDeclNode(context).id);
			NameUseExprNode* name = structName.get_name_use(context);
			name.resolve(declarationOwner, context);
			name.flags |= AstFlags.isType;
			name.state = AstNodeState.name_resolve_done;
			AstIndex thisType = make!PtrTypeNode(start, CommonAstNodes.type_type, structName);

			AstIndex param = make!VariableDeclNode(start, currentScopeIndex, thisType, AstIndex.init, CommonIds.id_this, ushort(0));
			VariableDeclNode* paramNode = param.get!VariableDeclNode(context);
			paramNode.flags |= VariableFlags.isParameter;
			params.put(context.arrayArena, param);
		}

		// restore attributes earlier, so signature receives attributes
		attribState = scope_temp.prev;

		CallConvention callConvention = context.defaultCallConvention;
		AstIndex signature = makeDecl!FunctionSignatureNode(start, typeIndex, params, callConvention);

		// store values back;
		scope_temp.prev = attribState;
		// no attributes go to the parameters and body
		attribState = AttribState(0, 0);

		parseParameters(signature, NeedRegNames.yes); // functions need to register their param names
		AstIndex func = makeDecl!FunctionDeclNode(start, context.getAstNodeIndex(currentModule), parentScope, signature, declarationId);

		if (tok.type == TokenType.HASH_INLINE)
		{
			nextToken; // skip #inline
			func.get!FunctionDeclNode(context).flags |= FuncDeclFlags.isInline;
		}

		if (tok.type != TokenType.SEMICOLON)
		{
			AstIndex prevOwner = declarationOwner;
			declarationOwner = func;
			scope(exit) declarationOwner = prevOwner;
			func.get!FunctionDeclNode(context).block_stmt = block_stmt();
		}
		else expectAndConsume(TokenType.SEMICOLON); // external function

		return func;
	}

	enum NeedRegNames : bool { no, yes }
	// if nameReg is `no` parameters are put in name_register_done state
	void parseParameters(AstIndex signature, NeedRegNames nameReg)
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
				paramId = expectIdentifier();
			else // anon parameter
			{
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
			paramNode.flags |= flags;
			paramNode.scopeIndex = cast(typeof(paramNode.scopeIndex))paramIndex;
			if (nameReg == NeedRegNames.no)
				paramNode.state = AstNodeState.name_register_nested_done;

			sig.parameters.put(context.arrayArena, param);
			if (tok.type == TokenType.COMMA) nextToken; // skip ","
			else break;
		}
		if (!sig.hasExpandedParam)
			sig.numParamsBeforeVariadic = cast(ushort)sig.parameters.length;

		expectAndConsume(TokenType.RPAREN);
	}

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

	void parse_expr_list(ref AstNodes expressions, TokenType terminator)
	{
		while (tok.type != terminator) {
			// We don't want to grab the comma, e.g. it is NOT a sequence operator.
			expressions.put(context.arrayArena, expr(PreferType.no, COMMA_PREC));
			// allows trailing comma too
			if (tok.type == TokenType.COMMA)
				nextToken;
		}
		expectAndConsume(terminator);
	}

	// <struct_declaration> ::= "struct" <id> ("[" <template_params> "]")? "{" <declaration>* "}" /
	//                          "struct" <id> ("[" <template_params> "]")? ";"
	AstIndex parse_struct()
	{
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
				AstIndex structIndex = makeDecl!StructDeclNode(start, currentScopeIndex, AstIndex(), structId);
				StructDeclNode* s = structIndex.get!StructDeclNode(context);
				s.flags |= StructFlags.isOpaque;
				return structIndex;
			}

			AstIndex parentScope = currentScopeIndex; // need to get parent before push scope
			ScopeTempData scope_temp;
			AstIndex memberScope = pushScope(context.idString(structId), ScopeKind.member, scope_temp);
			scope(exit) popScope(scope_temp);

			// restore attributes earlier, so signature receives attributes
			attribState = scope_temp.prev;

			AstIndex structIndex = makeDecl!StructDeclNode(start, parentScope, memberScope, structId);
			StructDeclNode* s = structIndex.get!StructDeclNode(context);

			// store values back;
			scope_temp.prev = attribState;
			// no attributes go to the body
			attribState = AttribState(0, 0);

			expectAndConsume(TokenType.LCURLY);
			{
				AstIndex prevOwner = declarationOwner;
				declarationOwner = structIndex;
				scope(exit) declarationOwner = prevOwner;

				s.declarations = parse_declarations(TokenType.RCURLY, AstFlags.isMember);
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
				return AstNodes();
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
				AstIndex memberScope; // no scope

				return makeDecl!EnumDeclaration(start, currentScopeIndex, memberScope, AstNodes(), intType, enumId);
			}
			else if (tok.type == TokenType.EQUAL)
			{
				nextToken; // skip "="
				Identifier enumId = makeIdentifier(id);
				AstIndex value = expr(PreferType.no);
				auto member = make!EnumMemberDecl(start, currentScopeIndex, intType, value, enumId);

				expectAndConsume(TokenType.SEMICOLON); // ";"

				// enum e3 = 3;
				return member;
			}
			// enum e7 : i32 ...
			else if (tok.type == TokenType.COLON)
			{
				Identifier enumId = makeIdentifier(id);
				AstIndex memberType = parseColonType;
				ScopeTempData scope_temp;
				AstIndex memberScope = pushScope(context.idString(enumId), ScopeKind.member, scope_temp);
				AstNodes members = tryParseEnumBody(memberType);
				popScope(scope_temp);

				// enum e7 : i32 { e7 }
				// enum e8 : i32;
				return makeDecl!EnumDeclaration(start, currentScopeIndex, memberScope, members, memberType, enumId);
			}
			else if (tok.type == TokenType.LCURLY)
			{
				Identifier enumId = makeIdentifier(id);
				AstIndex memberType = intType;
				ScopeTempData scope_temp;
				AstIndex memberScope = pushScope(context.idString(enumId), ScopeKind.member, scope_temp);
				AstNodes members = parse_enum_body(memberType);
				popScope(scope_temp);

				// enum e9 { e9 }
				return makeDecl!EnumDeclaration(start, currentScopeIndex, memberScope, members, memberType, enumId);
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
			AstIndex memberScope; // no scope

			// enum : i32 { e6 }
			return makeDecl!EnumDeclaration(start, currentScopeIndex, memberScope, members, memberType);
		}
		else if (tok.type == TokenType.LCURLY)
		{
			AstIndex memberType = intType;
			AstNodes members = parse_enum_body(memberType);
			AstIndex memberScope; // no scope

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
			assert(false);
		}
	}

	AstIndex parse_import()
	{
		TokenIndex start = tok.index;
		version(print_parse) auto s = scop("import %s", start);
		nextToken; // skip "import"
		Array!Identifier ids;

		while (true) {
			string after = ids.length == 0 ? "import" : null;
			ids.put(context.arrayArena, expectIdentifier(after));

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
		return makeDecl!ImportDeclNode(start, currentScopeIndex, ids);
	}

	void parse_module()
	{
		TokenIndex start = tok.index;
		AstIndex parentPackage = CommonAstNodes.node_root_package;
		AstIndex conflictingModule;
		AstIndex conflictingModPack;

		// module declaration
		if (tok.type == TokenType.MODULE_SYM)
		{
			nextToken; // skip "module"

			while (true) {
				Identifier lastId = expectIdentifier();

				if (tok.type == TokenType.DOT) {
					nextToken; // skip "."
					auto parentPackageNode = parentPackage.get!PackageDeclNode(context);
					parentPackage = parentPackageNode.getOrCreateSubpackage(start, lastId, conflictingModule, context);
				} else if (tok.type == TokenType.SEMICOLON) {
					nextToken; // skip ";"
					currentModule.id = lastId;
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
		parentPackageNode.addModule(start, currentModule.id, context.getAstNodeIndex(currentModule), conflictingModPack, context);

		void modConflict(ModuleDeclNode* newMod, ModuleDeclNode* oldMod)
		{
			context.error(newMod.loc,
				"Module `%s` in file %s conflicts with another module `%s` in file %s",
				ModuleNamePrinter(newMod, context),
				context.files[newMod.moduleIndex.fileIndex].name,
				ModuleNamePrinter(oldMod, context),
				context.files[oldMod.moduleIndex.fileIndex].name, );
		}

		void modPackConflict(ModuleDeclNode* newMod, PackageDeclNode* oldPack)
		{
			context.error(newMod.loc,
				"Module `%s` in file %s conflicts with package `%s` in files %s",
				ModuleNamePrinter(newMod, context),
				context.files[newMod.moduleIndex.fileIndex].name,
				PackageNamePrinter(context.getAstNodeIndex(oldPack), context),
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

	AstNodes parseItems(alias itemParser)()
	{
		AstNodes items;
		TokenIndex start = tok.index;
		if (tok.type == TokenType.LCURLY)
		{
			nextToken; // skip {
			while (tok.type != TokenType.RCURLY)
			{
				if (tok.type == TokenType.EOI) break;
				items.put(context.arrayArena, itemParser());
			}
			expectAndConsume(TokenType.RCURLY);
		}
		else
			items.put(context.arrayArena, itemParser());
		return items;
	}

	AstIndex parse_hash_if() /* "#if" <paren_expr> <statement/decl> */
	{
		TokenIndex start = tok.index;
		nextToken; // skip #if
		AstIndex condition = paren_expr();
		AstNodes thenStatements;
		AstNodes elseStatements;

		if (declarationOwner.isDeclaration(context))
		{
			thenStatements = parseItems!statement();
			if (tok.type == TokenType.ELSE_SYM) { /* ... "else" <statement/decl> */
				nextToken; // skip else
				elseStatements = parseItems!statement();
			}
		}
		else
		{
			thenStatements = parseItems!parse_declaration();
			if (tok.type == TokenType.ELSE_SYM) { /* ... "else" <statement/decl> */
				nextToken; // skip else
				elseStatements = parseItems!parse_declaration();
			}
		}
		return make!StaticIfDeclNode(start, condition, thenStatements, elseStatements);
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
		AstNodes body = statement_as_array;
		AstIndex after_body = AstIndex(context.astBuffer.uintLength);
		return make!StaticForeachDeclNode(start, currentScopeIndex, keyId, valId, ct_expr, body, body_start, after_body);
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

	AstNodes parse_block() // "{" <statement>* "}"
	{
		AstNodes statements;
		expectAndConsume(TokenType.LCURLY);
		while (tok.type != TokenType.RCURLY)
		{
			if (tok.type == TokenType.EOI) break;

			statements.put(context.arrayArena, statement());
		}
		expectAndConsume(TokenType.RCURLY);
		return statements;
	}

	AstIndex block_stmt() // <block_statement> ::= "{" <statement>* "}"
	{
		version(print_parse) auto s1 = scop("block_stmt %s", loc);
		TokenIndex start = tok.index;
		ScopeTempData scope_temp;
		pushScope("Block", ScopeKind.local, scope_temp);
		AstNodes statements = parse_block;
		popScope(scope_temp);
		return make!BlockStmtNode(start, statements);
	}

	AstNodes statement_as_array()
	{
		if (tok.type == TokenType.LCURLY)
		{
			return parse_block;
		}
		else
		{
			AstNodes items;
			items.put(context.arrayArena, statement);
			return items;
		}
	}

	AstIndex statement()
	{
		version(print_parse) auto s1 = scop("statement %s", loc);
		TokenIndex start = tok.index;
		switch (tok.type)
		{
			// declarations
			case TokenType.ALIAS_SYM:
				return parse_alias();
			case TokenType.STRUCT_SYM:
				return parse_struct();
			case TokenType.ENUM:
				return parse_enum();
			case TokenType.IMPORT_SYM:
				return parse_import();
			case TokenType.HASH_IF:
				return parse_hash_if();
			case TokenType.HASH_ASSERT:
				return parse_hash_assert();
			case TokenType.HASH_FOREACH:
				return parse_hash_foreach();

			// statements
			case TokenType.IF_SYM: /* "if" <paren_expr> <statement> */
				nextToken;
				AstIndex condition = paren_expr();
				ScopeTempData scope_temp;
				pushScope("Then", ScopeKind.local, scope_temp);
				AstNodes thenStatements = statement_as_array;
				popScope(scope_temp);
				AstNodes elseStatements;
				if (tok.type == TokenType.ELSE_SYM) { /* ... "else" <statement> */
					nextToken;
					ScopeTempData scope_temp2;
					pushScope("Else", ScopeKind.local, scope_temp2);
					elseStatements = statement_as_array;
					popScope(scope_temp2);
				}
				return make!IfStmtNode(start, condition, thenStatements, elseStatements);
			case TokenType.WHILE_SYM:  /* "while" <paren_expr> <statement> */
				nextToken;
				ScopeTempData scope_temp;
				pushScope("While", ScopeKind.local, scope_temp);
				AstIndex condition = paren_expr();
				AstNodes statements = statement_as_array;
				popScope(scope_temp);
				return make!WhileStmtNode(start, condition, statements);
			case TokenType.DO_SYM:  /* "do" <statement> "while" <paren_expr> ";" */
				nextToken;
				ScopeTempData scope_temp;
				pushScope("do", ScopeKind.local, scope_temp);
				AstNodes statements = statement_as_array;
				expectAndConsume(TokenType.WHILE_SYM);
				AstIndex condition = paren_expr();
				popScope(scope_temp);
				expectAndConsume(TokenType.SEMICOLON);
				return make!DoWhileStmtNode(start, condition, statements);
			case TokenType.FOR_SYM:  /* "for" "(" <statement> ";" <statement> ";" "while" <paren_expr> ";" */
				return parse_for;
			case TokenType.SWITCH_SYM:
				return parse_switch;
			case TokenType.RETURN_SYM:  /* return <expr> */
				nextToken;
				AstIndex expression = tok.type != TokenType.SEMICOLON ? expr(PreferType.no) : AstIndex.init;
				expectAndConsume(TokenType.SEMICOLON);
				return make!ReturnStmtNode(start, expression);
			case TokenType.BREAK_SYM:  /* break; */
				nextToken;
				expectAndConsume(TokenType.SEMICOLON);
				return make!BreakStmtNode(start);
			case TokenType.CONTINUE_SYM:  /* continue; */
				nextToken;
				expectAndConsume(TokenType.SEMICOLON);
				return make!ContinueStmtNode(start);
			case TokenType.SEMICOLON:  /* ";" */
				context.error(tok.index, "Cannot use `;` as an empty statement. Use `{}` instead");
				nextToken;
				return make!BlockStmtNode(start, AstNodes());
			case TokenType.LCURLY:  /* "{" { <statement> } "}" */
				return block_stmt();
			default:
			{
				// expression or var/func declaration
				version(print_parse) auto s2 = scop("default %s", loc);
				// <expr> ";" / var decl / func decl
				AstIndex expression = parse_expr_or_id_decl(ConsumeTerminator.yes, TokenType.SEMICOLON);
				return expression;
			}
		}
	}

	AstIndex parse_for() // "for" "(" <init>,... ";" <cond> ";" <increment> ")" <statement>
	{
		TokenIndex start = tok.index;
		nextToken; // skip "for"

		expectAndConsume(TokenType.LPAREN); // (

		ScopeTempData scope_temp;
		pushScope("For", ScopeKind.local, scope_temp);
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
			incExprNode.flags |= AstFlags.isStatement;
			increment_statements.put(context.arrayArena, incExpr);

			if (tok.type == TokenType.COMMA)
				nextToken; // skip ","
			else break;
		}
		expectAndConsume(TokenType.RPAREN);

		AstNodes statements = statement_as_array;

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

		while (rbp < lbp && lbp < nbp)
		{
			t = tok;
			nextToken;
			LeftInfo left_info = g_tokenLookups.left_lookup[t.type];
			node = left_info.parser_left(this, preferType, t, left_info.rbp, node);
			nbp = left_info.nbp; // next bp
			lbp = g_tokenLookups.left_lookup[tok.type].lbp;
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

alias LeftParser = AstIndex function(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex left);
alias NullParser = AstIndex function(ref Parser p, PreferType preferType, Token token, int rbp);

AstIndex left_error_parser(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex left)
{
	if (token.type == TokenType.EOI)
		p.context.unrecoverable_error(token.index, "Unexpected end of input");
	else
		p.context.unrecoverable_error(token.index, "%s is not an expression", token.type);
	assert(false);
}

AstIndex null_error_parser(ref Parser p, PreferType preferType, Token token, int rbp)
{
	if (token.type == TokenType.EOI)
		p.context.unrecoverable_error(token.index, "Unexpected end of input");
	else
		p.context.unrecoverable_error(token.index, "%s is not an expression", token.type);
	assert(false);
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
	prefix(290, &nullPrefixOp, ["+", "-", "!", "~", "*", "&", "++", "--"]);
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
		"noreturn","void", "bool", "null",
		"i8", "i16", "i32", "i64",
		"u8", "u16", "u32", "u64",
		"f32", "f64",
		"$alias", "$type",
		"true", "false",
		"#int_dec_lit", "#int_bin_lit", "#int_hex_lit", "#float_dec_lit", "#str_lit", "#char_lit"]
	);
	nilfix(0, &null_error_parser, [")", "]", ":", "#eoi", ";"]);
	return res;
}

// Null Denotations -- tokens that take nothing on the left

// id, int_literal, string_literal
AstIndex nullLiteral(ref Parser p, PreferType preferType, Token token, int rbp) {
	import std.algorithm.iteration : filter;
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
			p.context.error(token.index, "Invalid $ identifier %s", p.context.idString(id));
			return p.make!NameUseExprNode(token.index, p.currentScopeIndex, id);
		case NULL:
			return p.makeExpr!NullLiteralExprNode(token.index);
		case TRUE_LITERAL:
			return p.makeExpr!BoolLiteralExprNode(token.index, true);
		case FALSE_LITERAL:
			return p.makeExpr!BoolLiteralExprNode(token.index, false);
		case STRING_LITERAL:
			// omit " at the start and end of token
			string value = cast(string)p.context.getTokenString(token.index)[1..$-1];
			// handle escape sequences
			value = handleEscapedString(p.context.roStaticDataBuffer, value);
			p.context.roStaticDataBuffer.put(0); // add zero terminator
			AstIndex type = CommonAstNodes.type_u8Slice;

			IrIndex irValue;
			// dont create empty global for empty string. Globalsare required to have non-zero length
			if (value.length == 0)
			{
				irValue = p.context.constants.add(0, IsSigned.no, IrArgSize.size64); // null ptr
			}
			else
			{
				irValue = p.context.globals.add();
				IrGlobal* global = p.context.globals.get(irValue);
				global.type = CommonAstNodes.type_u8Ptr.gen_ir_type(p.context);

				ObjectSymbol sym = {
					kind : ObjectSymbolKind.isLocal,
					sectionIndex : p.context.builtinSections[ObjectSectionType.ro_data],
					moduleIndex : p.currentModule.objectSymIndex,
					flags : ObjectSymbolFlags.needsZeroTermination | ObjectSymbolFlags.isString,
					id : p.context.idMap.getOrRegNoDup(p.context, ":string"),
				};
				global.objectSymIndex = p.context.objSymTab.addSymbol(sym);

				ObjectSymbol* globalSym = p.context.objSymTab.getSymbol(global.objectSymIndex);
				globalSym.setInitializer(cast(ubyte[])value);
			}
			IrIndex irValueLength = p.context.constants.add(value.length, IsSigned.no, IrArgSize.size64);
			irValue = p.context.constants.addAggrecateConstant(type.gen_ir_type(p.context), irValueLength, irValue);

			return p.make!StringLiteralExprNode(token.index, type, irValue, value);
		case CHAR_LITERAL:
			// omit ' at the start and end of token
			string value = cast(string)p.context.getTokenString(token.index)[1..$-1];
			dchar charVal = getCharValue(value);
			return p.makeExpr!IntLiteralExprNode(token.index, cast(uint)charVal);
		case INT_DEC_LITERAL:
			string value = cast(string)p.context.getTokenString(token.index);
			long intValue = value.filter!(c => c != '_').to!ulong;
			return p.makeExpr!IntLiteralExprNode(token.index, intValue);
		case INT_HEX_LITERAL:
			string value = cast(string)p.context.getTokenString(token.index);
			long intValue = value[2..$].filter!(c => c != '_').to!ulong(16); // skip 0x, 0X
			return p.makeExpr!IntLiteralExprNode(token.index, intValue);
		case INT_BIN_LITERAL:
			string value = cast(string)p.context.getTokenString(token.index);
			long intValue = value[2..$].filter!(c => c != '_').to!ulong(2); // skip 0b, 0B
			return p.makeExpr!IntLiteralExprNode(token.index, intValue);
		case FLOAT_DEC_LITERAL:
			string value = cast(string)p.context.getTokenString(token.index);
			double floatValue = value.filter!(c => c != '_').to!double;
			return p.makeExpr!FloatLiteralExprNode(token.index, floatValue);
		case TYPE_NORETURN, TYPE_VOID, TYPE_BOOL,
			TYPE_I8, TYPE_I16, TYPE_I32, TYPE_I64, TYPE_U8, TYPE_U16, TYPE_U32, TYPE_U64,
			TYPE_F32, TYPE_F64,
			TYPE_ALIAS, TYPE_TYPE:
			BasicType t = token.type.tokenTypeToBasicType;
			return p.context.basicTypeNodes(t);
		default:
			p.context.internal_error("nullLiteral %s", token.type); assert(false);
	}
}

// Arithmetic grouping
AstIndex nullParen(ref Parser p, PreferType preferType, Token token, int rbp) {
	AstIndex r = p.expr(PreferType.no, rbp);
	p.expectAndConsume(TokenType.RPAREN);
	//r.flags |= NFLG.parenthesis; // NOTE: needed if ternary operator is needed
	return r;
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
			op = UnOp.minus;
			break;
		case NOT: op = UnOp.logicalNot; break;
		case TILDE: op = UnOp.bitwiseNot; break;
		case STAR: op = UnOp.deref; break;
		case AND: op = UnOp.addrOf; break;
		case PLUS_PLUS: op = UnOp.preIncrement; break;
		case MINUS_MINUS: op = UnOp.preDecrement; break;
		default:
			p.context.unreachable(); assert(false);
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
AstIndex leftIncDec(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex left) {
	UnOp op;
	switch(token.type) with(TokenType)
	{
		case PLUS_PLUS: op = UnOp.postIncrement; break;
		case MINUS_MINUS: op = UnOp.postDecrement; break;
		default:
			p.context.unreachable(); assert(false);
	}
	return p.makeExpr!UnaryExprNode(token.index, op, left);
}

// <expr> "[" "]"
// <expr> "[" <expr> "," <expr>+ "]"
// <expr> "[" <expr> .. <expr> "]"
AstIndex leftIndex(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex array) {
	AstNodes indicies;
	if (p.tok.type == TokenType.RBRACKET)
	{
		p.nextToken;
		return p.makeExpr!IndexExprNode(token.index, array, indicies);
	}
	AstIndex index = p.expr(PreferType.no, 0);
	if (p.tok.type == TokenType.RBRACKET)
	{
		p.nextToken;
		indicies.put(p.context.arrayArena, index);
		return p.makeExpr!IndexExprNode(token.index, array, indicies);
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
AstIndex leftOpDot(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex left)
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

AstIndex leftFunctionOp(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex returnType) {
	CallConvention callConvention = p.context.defaultCallConvention;
	auto sig = p.make!FunctionSignatureNode(token.index, returnType, AstNodes.init, callConvention);
	p.parseParameters(sig, p.NeedRegNames.no); // function types don't need to register their param names
	// we don't have to register parameter names, since we have no body
	sig.setState(p.context, AstNodeState.name_register_nested_done);
	return p.make!PtrTypeNode(token.index, CommonAstNodes.type_type, sig);
}

// multiplication or pointer type
// <expr> * <expr> or <expr>*
AstIndex leftStarOp(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex left) {
	switch (p.tok.type) with(TokenType)
	{
		case STAR, COMMA, RPAREN, RBRACKET, SEMICOLON, FUNCTION_SYM /*,DELEGATE_SYM*/:
			// pointer
			return p.make!PtrTypeNode(token.index, CommonAstNodes.type_type, left);
		case DOT:
			// hack for postfix star followed by dot
			AstIndex ptr = p.make!PtrTypeNode(token.index, CommonAstNodes.type_type, left);
			Token tok = p.tok;
			p.nextToken; // skip dot
			return leftOpDot(p, PreferType.no, tok, 0, ptr);
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
	BinOp op = BinOp.MULT;
	return p.makeExpr!BinaryExprNode(token.index, op, left, right);
}

// Normal binary operator <expr> op <expr>
AstIndex leftBinaryOp(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex left) {
	AstIndex right = p.expr(PreferType.no, rbp);
	BinOp op;
	switch(token.type) with(TokenType)
	{
		// logic ops
		case AND_AND: op = BinOp.LOGIC_AND; break;                // &&
		case OR_OR: op = BinOp.LOGIC_OR; break;                   // ||
		case EQUAL_EQUAL: op = BinOp.EQUAL; break;                // ==
		case NOT_EQUAL: op = BinOp.NOT_EQUAL; break;              // !=
		case MORE: op = BinOp.GREATER; break;                     // >
		case MORE_EQUAL: op = BinOp.GREATER_EQUAL; break;         // >=
		case LESS: op = BinOp.LESS; break;                        // <
		case LESS_EQUAL: op = BinOp.LESS_EQUAL; break;            // <=

		// arithmetic ops
		case AND: op = BinOp.BITWISE_AND; break;                  // &
		case OR: op = BinOp.BITWISE_OR; break;                    // |
		case PERCENT: op = BinOp.REMAINDER; break;                // %
		case LESS_LESS: op = BinOp.SHL; break;                    // <<
		case MORE_MORE: op = BinOp.ASHR; break;                   // >>
		case MORE_MORE_MORE: op = BinOp.SHR; break;               // >>>
		case MINUS: op = BinOp.MINUS; break;                      // -
		case PLUS: op = BinOp.PLUS; break;                        // +
		case SLASH: op = BinOp.DIV; break;                        // /
		case XOR: op = BinOp.XOR; break;                          // ^

		default:
			p.context.internal_error(token.index, "parse leftBinaryOp %s", token.type);
			assert(false);
	}
	return p.makeExpr!BinaryExprNode(token.index, op, left, right);
}

// Binary assignment operator <expr> op= <expr>
AstIndex leftAssignOp(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex left) {
	AstIndex right = p.expr(PreferType.no, rbp);
	BinOp op;
	switch(token.type) with(TokenType)
	{
		// arithmetic opEquals
		case EQUAL: op = BinOp.ASSIGN; break;                     // =
		case AND_EQUAL: op = BinOp.BITWISE_AND_ASSIGN; break;     // &=
		case OR_EQUAL: op = BinOp.BITWISE_OR_ASSIGN; break;       // |=
		case PERCENT_EQUAL: op = BinOp.REMAINDER_ASSIGN; break;   // %=
		case LESS_LESS_EQUAL: op = BinOp.SHL_ASSIGN; break;       // <<=
		case MORE_MORE_EQUAL: op = BinOp.ASHR_ASSIGN; break;      // >>=
		case MORE_MORE_MORE_EQUAL: op = BinOp.SHR_ASSIGN; break;  // >>>=
		case MINUS_EQUAL: op = BinOp.MINUS_ASSIGN; break;         // -=
		case PLUS_EQUAL: op = BinOp.PLUS_ASSIGN; break;           // +=
		case SLASH_EQUAL: op = BinOp.DIV_ASSIGN; break;           // /=
		case STAR_EQUAL: op = BinOp.MULT_ASSIGN; break;           // *=
		case XOR_EQUAL: op = BinOp.XOR_ASSIGN; break;             // ^=
		default:
			p.context.internal_error(token.index, "parse leftAssignOp %s", token.type);
			assert(false);
	}
	AstNode* leftNode = p.context.getAstNode(left);
	leftNode.flags |= AstFlags.isLvalue;

	AstIndex assignExpr = p.makeExpr!BinaryExprNode(token.index, op, left, right);
	AstNode* assignExprNode = p.context.getAstNode(assignExpr);
	assignExprNode.flags |= BinaryOpFlags.isAssignment;

	return assignExpr;
}

// <expr> "(" <expr_list> ")"
AstIndex leftFuncCall(ref Parser p, PreferType preferType, Token token, int unused_rbp, AstIndex callee) {
	AstNodes args;
	p.parse_expr_list(args, TokenType.RPAREN);
	return p.makeExpr!CallExprNode(token.index, callee, args);
}
