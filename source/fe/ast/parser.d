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
	<func_decl> = <type> <identifier> "(" <param_list> ")" (<block_statement> / ';')
	<param_list> = <parameter> "," <parameter_list> / <parameter>?
	<parameter> = <type> <identifier>?

	<var_decl> = <type> <identifier> ("=" <expression>)? ";"
	<struct_decl> = "struct" <identifier> "{" <declaration>* "}"
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
			auto astPrinter = AstPrinter(&ctx, 2);
			writefln("// AST fresh `%s`", file.name);
			astPrinter.printAst(cast(AstNode*)file.mod);
		}
	}
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

	bool hasMoreTokens() {
		return tok.type != TokenType.EOI;
	}

	AstIndex make(T, Args...)(TokenIndex start, Args args) {
		return context.appendAst!T(start, args);
	}
	AstIndex makeExpr(T, Args...)(TokenIndex start, Args args) {
		return context.appendAst!T(start, AstIndex.init, args);
	}

	void expect(TokenType type) {
		if (tok.type != type) {
			const(char)[] tokenString = context.getTokenString(tok.index);
			context.unrecoverable_error(tok.index, "Expected `%s` token, while got `%s` token '%s'",
				type, tok.type, tokenString);
		}
	}

	void expectAndConsume(TokenType type) {
		expect(type);
		nextToken;
	}

	Identifier makeIdentifier(TokenIndex index)
	{
		const(char)[] str = context.getTokenString(index);
		return context.idMap.getOrRegNoDup(str);
	}

	Identifier expectIdentifier()
	{
		TokenIndex index = tok.index;
		expectAndConsume(TokenType.IDENTIFIER);
		Identifier id = makeIdentifier(index);
		return id;
	}

	AstIndex pushScope(string name, ScopeKind kind)
	{
		AstIndex newScopeIndex = context.appendAst!Scope;
		Scope* newScope = context.getAst!Scope(newScopeIndex);
		newScope.debugName = name;
		newScope.kind = kind;

		if (currentScope)
			newScope.parentScope = currentScope.get_ast_index(context);
		currentScope = newScope;

		return newScopeIndex;
	}

	void popScope()
	{
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

		mod.memberScope = pushScope("Module", ScopeKind.global);
			mod.declarations = parse_declarations(TokenType.EOI, AstFlags.isGlobal);
		popScope;
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
			if (declNode.astType == AstType.decl_var) {
				auto var = declNode.as!VariableDeclNode(context);
				var.scopeIndex = varIndex++;
			}
			declarations.put(context.arrayArena, declIndex);
		}
		return declarations;
	}

	AstIndex parse_declaration() // <declaration> ::= <func_declaration> / <var_declaration> / <struct_declaration>
	{
		version(print_parse) auto s1 = scop("parse_declaration %s", loc);
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
			case HASH_IF:
				return parse_hash_if();
			default: // <func_declaration> / <var_declaration>
				AstIndex funcIndex = parse_var_func_declaration(ConsumeTerminator.yes, TokenType.SEMICOLON);
				AstNode* node = context.getAstNode(funcIndex);
				return funcIndex;
		}
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

		return make!AliasDeclNode(start, currentScopeIndex, aliasId, initializerIndex);
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

	AstIndex parse_var_func_declaration_after_type(TokenIndex start, AstIndex body_start, AstIndex typeIndex, ConsumeTerminator consume_terminator, TokenType var_terminator = TokenType.init)
	{
		version(print_parse) auto s2 = scop("<func_declaration> / <var_declaration> %s", start);
		Identifier declarationId = expectIdentifier();

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
			return make!VariableDeclNode(start, currentScopeIndex, typeIndex, initializerIndex, declarationId);
		}
		else if (tok.type == TokenType.LBRACKET) // <func_declaration> ::= <type> <id> "[" <template_params> "]" "(" <param_list> ")" (<block_statement> / ';')
		{
			AstNodes template_params;
			parse_template_parameters(template_params);
			AstIndex body = parse_func(start, typeIndex, declarationId);
			AstIndex after_body = AstIndex(context.astBuffer.uintLength);
			return make!TemplateDeclNode(start, currentScopeIndex, template_params, body, body_start, after_body, declarationId);
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
		pushScope(context.idString(declarationId), ScopeKind.local);
		scope(exit) popScope;

		// add this pointer
		if (declarationOwner.astType(context) == AstType.decl_struct)
		{
			AstIndex structName = make!NameUseExprNode(start, currentScopeIndex, declarationOwner.get!StructDeclNode(context).id);
			NameUseExprNode* name = structName.get_name_use(context);
			name.resolve(declarationOwner, context);
			name.flags |= AstFlags.isType;
			name.state = AstNodeState.name_resolve_done;
			AstIndex thisType = make!PtrTypeNode(start, structName);

			AstIndex param = make!VariableDeclNode(start, currentScopeIndex, thisType, AstIndex.init, CommonIds.id_this, ushort(0));
			VariableDeclNode* paramNode = param.get!VariableDeclNode(context);
			paramNode.flags |= VariableFlags.isParameter;
			params.put(context.arrayArena, param);
		}

		ubyte numDefaultArgs = parseParameters(params, NeedRegNames.yes); // functions need to register their param names

		AstIndex signature = make!FunctionSignatureNode(start, typeIndex, params, numDefaultArgs);
		AstIndex func = make!FunctionDeclNode(start, context.getAstNodeIndex(currentModule), parentScope, signature, declarationId);

		AstIndex block;
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
	// nameReg can put parameters in name_register_done state
	/// returns number of default args
	ubyte parseParameters(ref Array!AstIndex params, NeedRegNames nameReg)
	{
		expectAndConsume(TokenType.LPAREN);

		ubyte numDefaultArgs = 0;
		while (tok.type != TokenType.RPAREN)
		{
			if (tok.type == TokenType.EOI) break;

			// <param> ::= <type> <identifier>?
			TokenIndex paramStart = tok.index;
			AstIndex paramType = expr(PreferType.yes, 0);
			Identifier paramId;
			size_t paramIndex = params.length;
			AstIndex defaultValue;

			if (tok.type == TokenType.IDENTIFIER) // named parameter
				paramId = expectIdentifier();
			else // anon parameter
			{
				paramId = context.idMap.getOrRegFormatted("__param_%s", paramIndex);
			}

			// default argument
			if (tok.type == TokenType.EQUAL)
			{
				nextToken; // skip =
				defaultValue = expr(PreferType.yes, 0);

				++numDefaultArgs;
			}
			else
			{
				// all default arguments must be at the end of param list
				if (numDefaultArgs != 0)
					context.error(paramStart,
						"Default argument expected for %s", context.idString(paramId));
			}

			AstIndex param = make!VariableDeclNode(paramStart, currentScopeIndex, paramType, defaultValue, paramId);
			VariableDeclNode* paramNode = param.get!VariableDeclNode(context);
			paramNode.flags |= VariableFlags.isParameter;
			paramNode.scopeIndex = cast(typeof(paramNode.scopeIndex))paramIndex;
			if (nameReg == NeedRegNames.no)
				paramNode.state = AstNodeState.name_register_nested_done;

			params.put(context.arrayArena, param);
			if (tok.type == TokenType.COMMA) nextToken; // skip ","
			else break;
		}

		expectAndConsume(TokenType.RPAREN);

		return numDefaultArgs;
	}

	void parse_template_parameters(ref AstNodes params)
	{
		expectAndConsume(TokenType.LBRACKET);

		while (tok.type != TokenType.RBRACKET)
		{
			if (tok.type == TokenType.EOI) break;

			// <type_param> ::= <identifier>
			TokenIndex paramStart = tok.index;
			Identifier paramId = expectIdentifier();

			AstIndex param = make!TemplateParamDeclNode(paramStart, paramId);
			params.put(context.arrayArena, param);

			if (tok.type == TokenType.COMMA) nextToken; // skip ","
			else break;
		}

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
				AstIndex structIndex = make!StructDeclNode(start, currentScopeIndex, AstIndex(), structId);
				StructDeclNode* s = structIndex.get!StructDeclNode(context);
				s.flags |= StructFlags.isOpaque;
				return structIndex;
			}

			AstIndex parentScope = currentScopeIndex; // need to get parent before push scope
			AstIndex memberScope = pushScope(context.idString(structId), ScopeKind.member);
			scope(exit) popScope;

			AstIndex structIndex = make!StructDeclNode(start, parentScope, memberScope, structId);
			StructDeclNode* s = structIndex.get!StructDeclNode(context);

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
			parse_template_parameters(template_params);
			AstIndex body = parse_rest();
			AstIndex after_body = AstIndex(context.astBuffer.uintLength);
			return make!TemplateDeclNode(start, currentScopeIndex, template_params, body, body_start, after_body, structId);
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

		AstIndex intType = context.basicTypeNodes(BasicType.t_i32);

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

			auto member = make!EnumMemberDecl(start, currentScopeIndex, type, value, enumId);

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

				return make!EnumDeclaration(start, currentScopeIndex, memberScope, AstNodes(), intType, enumId);
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
				AstIndex type = parseColonType;
				AstIndex memberScope = pushScope(context.idString(enumId), ScopeKind.member);
				AstNodes members = tryParseEnumBody(type);
				popScope;

				// enum e7 : i32 { e7 }
				// enum e8 : i32;
				return make!EnumDeclaration(start, currentScopeIndex, memberScope, members, type, enumId);
			}
			else if (tok.type == TokenType.LCURLY)
			{
				Identifier enumId = makeIdentifier(id);
				AstIndex type = intType;
				AstIndex memberScope = pushScope(context.idString(enumId), ScopeKind.member);
				AstNodes members = parse_enum_body(type);
				popScope;

				// enum e9 { e9 }
				return make!EnumDeclaration(start, currentScopeIndex, memberScope, members, type, enumId);
			}
			else
			{
				tok = copy; // restore
				return parseTypeEnum;
			}
		}
		else if (tok.type == TokenType.COLON)
		{
			AstIndex type = parseColonType;
			AstNodes members = parse_enum_body(type);
			AstIndex memberScope; // no scope

			// enum : i32 { e6 }
			return make!EnumDeclaration(start, currentScopeIndex, memberScope, members, type);
		}
		else if (tok.type == TokenType.LCURLY)
		{
			AstIndex type = intType;
			AstNodes members = parse_enum_body(type);
			AstIndex memberScope; // no scope

			// enum { e5 }
			return make!EnumDeclaration(start, currentScopeIndex, memberScope, members, type);
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
		Identifier moduleId = expectIdentifier();
		expectAndConsume(TokenType.SEMICOLON);
		return make!ImportDeclNode(start, currentScopeIndex, moduleId);
	}

	AstIndex parse_hash_if() /* "#if" <paren_expr> <statement/decl> */
	{
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

			auto member = make!EnumMemberDecl(start, currentScopeIndex, type, value, id);
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
		pushScope("Block", ScopeKind.local);
		AstNodes statements = parse_block;
		popScope;
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

			// statements
			case TokenType.IF_SYM: /* "if" <paren_expr> <statement> */
				nextToken;
				AstIndex condition = paren_expr();
				pushScope("Then", ScopeKind.local);
				AstNodes thenStatements = statement_as_array;
				popScope;
				AstNodes elseStatements;
				if (tok.type == TokenType.ELSE_SYM) { /* ... "else" <statement> */
					nextToken;
					pushScope("Else", ScopeKind.local);
					elseStatements = statement_as_array;
					popScope;
				}
				return make!IfStmtNode(start, condition, thenStatements, elseStatements);
			case TokenType.WHILE_SYM:  /* "while" <paren_expr> <statement> */
				nextToken;
				pushScope("While", ScopeKind.local);
				AstIndex condition = paren_expr();
				AstNodes statements = statement_as_array;
				popScope;
				return make!WhileStmtNode(start, condition, statements);
			case TokenType.DO_SYM:  /* "do" <statement> "while" <paren_expr> ";" */
				nextToken;
				pushScope("do", ScopeKind.local);
				AstNodes statements = statement_as_array;
				expectAndConsume(TokenType.WHILE_SYM);
				AstIndex condition = paren_expr();
				popScope;
				expectAndConsume(TokenType.SEMICOLON);
				return make!DoWhileStmtNode(start, condition, statements);
			case TokenType.FOR_SYM:  /* "for" "(" <statement> ";" <statement> ";" "while" <paren_expr> ";" */
				return parse_for;
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

		pushScope("For", ScopeKind.local);
		scope(exit) popScope;

		Array!AstIndex init_statements;

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

		Array!AstIndex increment_statements;
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
		"#id", "void", "bool", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "null",
		"true", "false", "#num_dec_lit", "#num_bin_lit", "#num_hex_lit",
		"#str_lit", "#char_lit"]);
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
		case NULL:
			return p.makeExpr!NullLiteralExprNode(token.index);
		case TRUE_LITERAL:
			return p.makeExpr!BoolLiteralExprNode(token.index, true);
		case FALSE_LITERAL:
			return p.makeExpr!BoolLiteralExprNode(token.index, false);
		case STRING_LITERAL:
			// omit " at the start and end of token
			string value = cast(string)p.context.getTokenString(token.index)[1..$-1];
			AstIndex type = p.context.u8Slice;

			IrIndex irValue;
			// dont create empty global for empty string. Globalsare required to have non-zero length
			if (value.length == 0)
			{
				irValue = p.context.constants.add(0, IsSigned.no, IrArgSize.size64); // null ptr
			}
			else
			{
				irValue = p.context.globals.add();
				IrGlobal* global = &p.context.globals.get(irValue);
				global.setInitializer(cast(ubyte[])value);
				global.flags |= IrGlobalFlags.needsZeroTermination | IrGlobalFlags.isString;
				global.type = p.context.u8Ptr.gen_ir_type(p.context);
				global.moduleSymIndex = p.currentModule.objectSymIndex;
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
		case TYPE_VOID, TYPE_BOOL, TYPE_I8, TYPE_I16, TYPE_I32, TYPE_I64, TYPE_U8, TYPE_U16, TYPE_U32, TYPE_U64:
			BasicType t = token.type.tokenTypeToBasicType;
			return p.context.basicTypeNodes(t);
		default:
			p.context.unreachable(); assert(false);
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
	Array!AstIndex params;
	ubyte numDefaultArgs = p.parseParameters(params, p.NeedRegNames.no); // function types don't need to register their param names
	auto sig = p.make!FunctionSignatureNode(token.index, returnType, params, numDefaultArgs);
	// we don't have to register parameter names, since we have no body
	sig.setState(p.context, AstNodeState.name_register_nested_done);
	return p.make!PtrTypeNode(token.index, sig);
}

// multiplication or pointer type
// <expr> * <expr> or <expr>*
AstIndex leftStarOp(ref Parser p, PreferType preferType, Token token, int rbp, AstIndex left) {
	switch (p.tok.type) with(TokenType)
	{
		case STAR, COMMA, RPAREN, RBRACKET, SEMICOLON, FUNCTION_SYM /*,DELEGATE_SYM*/:
			// pointer
			return p.make!PtrTypeNode(token.index, left);
		case DOT:
			// hack for postfix star followed by dot
			AstIndex ptr = p.make!PtrTypeNode(token.index, left);
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
		return p.make!PtrTypeNode(token.index, left);
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
	assignExprNode.flags |= AstFlags.isAssignment;

	return assignExpr;
}

// <id> "(" <expr_list> ")"
AstIndex leftFuncCall(ref Parser p, PreferType preferType, Token token, int unused_rbp, AstIndex callee) {
	AstNodes args;
	p.parse_expr_list(args, TokenType.RPAREN);
	return p.makeExpr!CallExprNode(token.index, callee, args);
}
