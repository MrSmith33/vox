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
	<declaration> = <func_decl> / <var_decl> / <struct_decl> / <enum_decl>

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
	<type_specializer> = "*" / "[" <expression> "]" / "[" "]"
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
	Token tok;
	SourceLocation loc() {
		return context.tokenLocationBuffer[tok.index];
	}

	int nesting;
	auto indent() { return ' '.repeat(nesting*2); }
	struct Scope { Parser* p; ~this(){--p.nesting;}}
	Scope scop(Args...)(string name, Args args) { write(indent); writefln(name, args); ++nesting; return Scope(&this); }

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
		return context.appendAst!T(start, AstIndex.init, IrIndex(), args);
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
		Identifier id = makeIdentifier(tok.index);
		expectAndConsume(TokenType.IDENTIFIER);
		return id;
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
		mod.declarations = parse_declarations(TokenType.EOI);
	}

	AstNodes parse_declarations(TokenType until) { // <declaration>*
		AstNodes declarations;
		ushort varIndex = 0;
		while (tok.type != until)
		{
			if (tok.type == TokenType.EOI) break;
			AstIndex declIndex = parse_declaration;
			if (declIndex.isUndefined)
			{
				const(char)[] tokenString = context.getTokenString(tok.index);
				context.unrecoverable_error(tok.index, "Expected declaration, while got `%s` tok '%s'",
					tok, tokenString);
			}

			AstNode* declNode = context.getAstNode(declIndex);
			if (declNode.astType == AstType.decl_var) {
				declNode.cast_decl_var.scopeIndex = varIndex++;
			}
			declarations.put(context.arrayArena, declIndex);
		}
		return declarations;
	}

	/// Can return null
	AstIndex parse_declaration() // <declaration> ::= <func_declaration> / <var_declaration> / <struct_declaration>
	{
		version(print_parse) auto s1 = scop("parse_declaration %s", loc);
		if (tok.type == TokenType.STRUCT_SYM) // <struct_declaration> ::= "struct" <id> "{" <declaration>* "}"
		{
			return parse_struct();
		}
		else if (tok.type == TokenType.ENUM)
		{
			return parse_enum();
		}
		else if (tok.type == TokenType.IMPORT_SYM)
		{
			return parse_import();
		}
		else // <func_declaration> / <var_declaration>
		{
			AstIndex funcIndex = parse_var_func_declaration();
			AstNode* node = context.getAstNode(funcIndex);
			if (node) {
				bool needSemicolon = node.astType == AstType.decl_var ||
					node.cast_decl_function.isExternal;

				if (needSemicolon) expectAndConsume(TokenType.SEMICOLON);
			}
			return funcIndex;
		}
	}

	/// var_terminator can be ";" or ",". Used to parse regular var/func declaration
	/// or to parse var declaration in foreach
	AstIndex parse_var_func_declaration()
	{
		TokenIndex start = tok.index;
		version(print_parse) auto s2 = scop("<func_declaration> / <var_declaration> %s", start);
		AstIndex typeIndex = parse_type();

		if (typeIndex.isUndefined)
		{
			version(print_parse) auto s3 = scop("<type> is null %s", loc);
			return AstIndex.init;
		}
		Identifier declarationId = expectIdentifier();

		AstIndex initializerIndex;
		if (tok.type == TokenType.EQUAL) // "=" <expression>
		{
			// <var_decl> = <type> <identifier> ("=" <expression>)? ";"
			nextToken; // skip "="
			initializerIndex = expr();
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
			// leave ";" or "," for parent to decide
			return make!VariableDeclNode(start, typeIndex, initializerIndex, declarationId);
		}
		else if (tok.type == TokenType.LPAREN) // <func_declaration> ::= <type> <id> "(" <param_list> ")" (<block_statement> / ';')
		{
			version(print_parse) auto s3 = scop("<func_declaration> %s", start);
			expectAndConsume(TokenType.LPAREN);

			Array!AstIndex params;

			while (tok.type != TokenType.RPAREN)
			{
				if (tok.type == TokenType.EOI) break;

				// <param> ::= <type> <identifier>?
				TokenIndex paramStart = tok.index;
				AstIndex paramType = parse_type_expected();
				Identifier paramId;
				size_t paramIndex = params.length;

				if (tok.type == TokenType.IDENTIFIER) // named parameter
					paramId = expectIdentifier();
				else // anon parameter
				{
					paramId = context.idMap.getOrRegWithSuffix("__param_", paramIndex);
				}

				AstIndex param = make!VariableDeclNode(paramStart, paramType, AstIndex.init, paramId);
				VariableDeclNode* paramNode = context.getAst!VariableDeclNode(param);
				paramNode.varFlags |= VariableFlags.isParameter;
				paramNode.scopeIndex = cast(typeof(paramNode.scopeIndex))paramIndex;
				params.put(context.arrayArena, param);
				if (tok.type == TokenType.COMMA) nextToken; // skip ","
				else break;
			}
			expectAndConsume(TokenType.RPAREN);

			AstIndex block;
			if (tok.type != TokenType.SEMICOLON)
			{
				block = block_stmt();
			}
			else expect(TokenType.SEMICOLON); // external function

			auto func = make!FunctionDeclNode(start, typeIndex, params, block, declarationId);
			currentModule.addFunction(func, context);
			return func;
		}
		else
		{
			context.unrecoverable_error(tok.index, "Expected '(' or ';', while got '%s'", context.getTokenString(tok.index));
			assert(false);
		}
	}

	// <struct_declaration> ::= "struct" <id> "{" <declaration>* "}" /
	//                          "struct" <id> ";"
	AstIndex parse_struct()
	{
		TokenIndex start = tok.index;
		version(print_parse) auto s2 = scop("struct %s", start);
		nextToken; // skip "struct"
		Identifier structId = expectIdentifier();

		if (tok.type == TokenType.SEMICOLON)
		{
			nextToken; // skip semicolon
			return make!StructDeclNode(start, AstNodes(), structId, true);
		}

		expectAndConsume(TokenType.LCURLY);
		AstNodes declarations = parse_declarations(TokenType.RCURLY);
		expectAndConsume(TokenType.RCURLY);
		return make!StructDeclNode(start, declarations, structId, false);
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
			AstIndex type = parse_type();
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
			AstIndex type = parse_type();
			if (!type)
				context.unrecoverable_error(tok.index,
					"Expected type after `enum`, while got `%s`",
					context.getTokenString(tok.index));

			Identifier enumId = expectIdentifier;
			expectAndConsume(TokenType.EQUAL); // "="
			AstIndex value = expr; // initializer
			auto member = make!EnumMemberDecl(start, type, value, enumId);

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

				return make!EnumDeclaration(start, AstNodes(), intType, enumId);
			}
			else if (tok.type == TokenType.EQUAL)
			{
				nextToken; // skip "="
				Identifier enumId = makeIdentifier(id);
				AstIndex value = expr;
				auto member = make!EnumMemberDecl(start, intType, value, enumId);

				expectAndConsume(TokenType.SEMICOLON); // ";"

				// enum e3 = 3;
				return member;
			}
			// enum e7 : i32 ...
			else if (tok.type == TokenType.COLON)
			{
				Identifier enumId = makeIdentifier(id);
				AstIndex type = parseColonType;
				AstNodes members = tryParseEnumBody(type);

				// enum e7 : i32 { e7 }
				// enum e8 : i32;
				return make!EnumDeclaration(start, members, type, enumId);
			}
			else if (tok.type == TokenType.LCURLY)
			{
				Identifier enumId = makeIdentifier(id);
				AstIndex type = intType;
				AstNodes members = parse_enum_body(type);

				// enum e9 { e9 }
				return make!EnumDeclaration(start, members, type, enumId);
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

			// enum : i32 { e6 }
			return make!EnumDeclaration(start, members, type);
		}
		else if (tok.type == TokenType.LCURLY)
		{
			AstIndex type = intType;
			AstNodes members = parse_enum_body(type);

			// enum { e5 }
			return make!EnumDeclaration(start, members, type);
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
		return make!ImportDeclNode(start, moduleId);
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
				value = expr;
			}

			auto member = make!EnumMemberDecl(start, type, value, id);
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

	AstIndex parse_type_expected()
	{
		version(print_parse) auto s1 = scop("parse_type_expected %s", tok.index);
		auto type = parse_type();
		if (!type) context.unrecoverable_error(tok.index, "Expected basic type, while got '%s'", context.getTokenString(tok.index));
		return type;
	}

	/// Can return null
	AstIndex parse_type() // <type> = (<type_basic> / <type_struct>) <type_specializer>*
	{
		version(print_parse) auto s1 = scop("parse_type %s %s", loc, tok.type);
		TokenIndex start = tok.index;
		AstIndex base;
		if (tok.type == TokenType.IDENTIFIER) {
			Identifier id = expectIdentifier();
			base = make!NameUseExprNode(start, id);
		} else if (isBasicTypeToken(tok.type)) {
			base = context.basicTypeNodes(parse_type_basic());
		}

		if (base) // <type_specializer> = '*' / '[' <expression> ']'
		{
			while (true)
			{
				if (tok.type == TokenType.STAR) { // '*' pointer
					nextToken;
					base = make!PtrTypeNode(start, base);
				} else if (tok.type == TokenType.LBRACKET) {
					nextToken;
					if (tok.type == TokenType.RBRACKET) // '[' ']' slice
					{
						nextToken; // skip ']'
						base = make!SliceTypeNode(start, base);
					}
					else // '[' <expression> ']' static array
					{
						AstIndex length_expr = expr();
						expectAndConsume(TokenType.RBRACKET);
						base = make!StaticArrayTypeNode(start, base, length_expr);
					}
				}
				else break;
			}
		}

		return base;
	}

	BasicType parse_type_basic()
	{
		version(print_parse) auto s1 = scop("parse_type_basic %s", loc);
		if (isBasicTypeToken(tok.type))
		{
			auto res = tokenTypeToBasicType(tok.type);
			nextToken;
			return res;
		}
		context.unrecoverable_error(tok.index, "Expected basic type, while got '%s'", context.getTokenString(tok.index));
		assert(false);
	}

	AstIndex block_stmt() // <block_statement> ::= "{" <statement>* "}"
	{
		version(print_parse) auto s1 = scop("block_stmt %s", loc);
		AstNodes statements;
		TokenIndex start = tok.index;
		expectAndConsume(TokenType.LCURLY);
		while (tok.type != TokenType.RCURLY)
		{
			if (tok.type == TokenType.EOI) break;

			statements.put(context.arrayArena, statement());
		}
		expectAndConsume(TokenType.RCURLY);
		return make!BlockStmtNode(start, statements);
	}

	AstIndex statement()
	{
		version(print_parse) auto s1 = scop("statement %s", loc);
		TokenIndex start = tok.index;
		switch (tok.type)
		{
			case TokenType.IF_SYM: /* "if" <paren_expr> <statement> */
				nextToken;
				AstIndex condition = paren_expr();
				AstIndex thenStatement = statement();
				AstIndex elseStatement;
				if (tok.type == TokenType.ELSE_SYM) { /* ... "else" <statement> */
					nextToken;
					elseStatement = statement();
				}
				return make!IfStmtNode(start, condition, thenStatement, elseStatement);
			case TokenType.WHILE_SYM:  /* "while" <paren_expr> <statement> */
				nextToken;
				AstIndex condition = paren_expr();
				AstIndex statement = statement();
				return make!WhileStmtNode(start, condition, statement);
			case TokenType.DO_SYM:  /* "do" <statement> "while" <paren_expr> ";" */
				nextToken;
				AstIndex statement = statement();
				expectAndConsume(TokenType.WHILE_SYM);
				AstIndex condition = paren_expr();
				expectAndConsume(TokenType.SEMICOLON);
				return make!DoWhileStmtNode(start, condition, statement);
			case TokenType.FOR_SYM:  /* "for" "(" <statement> ";" <statement> ";" "while" <paren_expr> ";" */
				return parse_for;
			case TokenType.RETURN_SYM:  /* return <expr> */
				nextToken;
				AstIndex expression = tok.type != TokenType.SEMICOLON ? expr() : AstIndex.init;
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
				nextToken;
				return make!BlockStmtNode(start, AstNodes()); // TODO: make this an error
			case TokenType.LCURLY:  /* "{" { <statement> } "}" */
				return block_stmt();
			default:
			{
				version(print_parse) auto s2 = scop("default %s", loc);

				if (isBasicTypeToken(tok.type) ||
					tok.type == TokenType.STRUCT_SYM ||
					tok.type == TokenType.ENUM ||
					tok.type == TokenType.IMPORT_SYM) // declaration
				{
					return parse_declaration;
				}

				Token copy = tok; // save
				if (is_declaration)
				{
					tok = copy;
					AstIndex decl = parse_declaration;
					return decl;
				}
				tok = copy;

				// <expr> ";"
				AstIndex expression = expr();
				AstNode* expressionNode = context.getAstNode(expression);
				expressionNode.flags |= AstFlags.isStatement;
				expectAndConsume(TokenType.SEMICOLON);

				return expression;
			}
		}
	}

	AstIndex parse_for() // "for" "(" <init> ";" <cond> ";" <increment> ")" <statement>
	{
		TokenIndex start = tok.index;
		nextToken; // skip "for"

		expectAndConsume(TokenType.LPAREN); // (

		Array!AstIndex init_statements;

		// <init>
		while (tok.type != TokenType.SEMICOLON) // check after trailing comma
		{
			AstIndex init_stmt;

			Token copy = tok; // save

			if (is_declaration)
			{
				tok = copy;
				init_stmt = parse_var_func_declaration();
			}
			else
			{
				tok = copy; // restore

				// <expr>
				AstIndex expression = expr();
				AstNode* expressionNode = context.getAstNode(expression);
				expressionNode.flags |= AstFlags.isStatement;
				init_stmt = expression;
			}

			init_statements.put(context.arrayArena, init_stmt);

			if (tok.type == TokenType.COMMA)
				nextToken; // skip ","
			else break;
		}
		expectAndConsume(TokenType.SEMICOLON);

		// <cond>
		AstIndex condition;
		if (tok.type != TokenType.SEMICOLON) {
			condition = expr();
		}
		expectAndConsume(TokenType.SEMICOLON);

		Array!AstIndex increment_statements;
		// <increment>
		while (tok.type != TokenType.RPAREN) // check after trailing comma
		{
			AstIndex incExpr = expr();
			AstNode* incExprNode = context.getAstNode(incExpr);
			incExprNode.flags |= AstFlags.isStatement;
			increment_statements.put(context.arrayArena, incExpr);

			if (tok.type == TokenType.COMMA)
				nextToken; // skip ","
			else break;
		}
		expectAndConsume(TokenType.RPAREN);

		AstIndex statement = statement();

		return make!ForStmtNode(start, init_statements, condition, increment_statements, statement);
	}

	// scans forward to check if current token starts new declaration
	private bool is_declaration()
	{
		if (tok.type == TokenType.IDENTIFIER || isBasicTypeToken(tok.type))
		{
			nextToken;
			while(is_declarator)
			{}

			if (tok.type == TokenType.IDENTIFIER)
			{
				return true;
			}
		}

		return false;
	}

	// scans forward to check if current token starts new declarator
	// doesn't restore in a case of failure
	// [], *, [expr]
	private bool is_declarator()
	{
		if (tok.type == TokenType.STAR) { // *
			nextToken;
			return true;
		}
		if (tok.type == TokenType.LBRACKET) { // [
			skip_brackets;
			return true;
		}
		return false;
	}

	void skip_brackets() {
		skip!(TokenType.LBRACKET, TokenType.RBRACKET, '[')();
	}

	void skip(TokenType Open, TokenType Close, char sym)() {
		Token start = tok;
		expectAndConsume(Open);
		int depth = 1;
		while (hasMoreTokens) {
			switch (tok.type) {
				case Close:
					nextToken;
					--depth;
					if (depth <= 0) {
						return;
					}
					break;
				case Open:
					++depth;
					nextToken;
					break;
				default:
					nextToken;
					break;
			}
		}
		context.unrecoverable_error(start.index, "Unclosed `%s`", sym);
	}

	AstIndex paren_expr() { /* <paren_expr> ::= "(" <expr> ")" */
		version(print_parse) auto s1 = scop("paren_expr %s", loc);
		expectAndConsume(TokenType.LPAREN);
		auto res = expr();
		expectAndConsume(TokenType.RPAREN);
		return res;
	}

	AstIndex expr(int rbp = 0)
	{
		Token t = tok;
		nextToken;

		NullInfo null_info = g_tokenLookups.null_lookup[t.type];
		AstIndex node = null_info.parser_null(this, t, null_info.rbp);
		int nbp = null_info.nbp; // next bp
		int lbp = g_tokenLookups.left_lookup[tok.type].lbp;

		while (rbp < lbp && lbp < nbp)
		{
			t = tok;
			nextToken;
			LeftInfo left_info = g_tokenLookups.left_lookup[t.type];
			node = left_info.parser_left(this, t, left_info.rbp, node);
			nbp = left_info.nbp; // next bp
			lbp = g_tokenLookups.left_lookup[tok.type].lbp;
		}

		return node;
	}
}

/// min and max binding powers
enum MIN_BP = 0;
enum MAX_BP = 10000;
enum COMMA_PREC = 10;

alias LeftParser = AstIndex function(ref Parser p, Token token, int rbp, AstIndex left);
alias NullParser = AstIndex function(ref Parser p, Token token, int rbp);

AstIndex left_error_parser(ref Parser p, Token token, int rbp, AstIndex left)
{
	if (token.type == TokenType.EOI)
		p.context.unrecoverable_error(token.index, "Unexpected end of input");
	else
		p.context.unrecoverable_error(token.index, "%s is not an expression", token.type);
	assert(false);
}

AstIndex null_error_parser(ref Parser p, Token token, int rbp)
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
	infixL(310, &leftBinaryOp, ".");
	//infixL(310, &leftBinaryOp, "->");

	// 29 -- binds to everything except function call, indexing, postfix ops
	prefix(290, &nullPrefixOp, ["+", "-", "!", "~", "*", "&", "++", "--"]);
	prefix(290, &nullCast, "cast");

	infixL(250, &leftBinaryOp, ["*", "/", "%"]);

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
		"#id", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "null",
		"true", "false", "#num_dec_lit", "#num_bin_lit", "#num_hex_lit",
		"#str_lit", "#char_lit"]);
	nilfix(0, &null_error_parser, [")", "]", ":", "#eoi", ";"]);
	return res;
}

// Null Denotations -- tokens that take nothing on the left

// id, int_literal, string_literal
AstIndex nullLiteral(ref Parser p, Token token, int rbp) {
	import std.algorithm.iteration : filter;
	switch(token.type) with(TokenType)
	{
		case IDENTIFIER:
			Identifier id = p.makeIdentifier(token.index);
			return p.make!NameUseExprNode(token.index, id);
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

			IrIndex irValue = p.context.globals.add();
			IrGlobal* global = &p.context.globals.get(irValue);
			global.setInitializer(cast(ubyte[])value);
			global.flags |= IrGlobalFlags.needsZeroTermination | IrGlobalFlags.isString;
			global.type = p.context.u8Ptr.gen_ir_type(p.context);
			global.moduleSymIndex = p.currentModule.objectSymIndex;
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
		case TYPE_I8, TYPE_I16, TYPE_I32, TYPE_I64, TYPE_U8, TYPE_U16, TYPE_U32, TYPE_U64:
			BasicType t = token.type.tokenTypeToBasicType;
			return p.context.basicTypeNodes(t);
		default:
			p.context.unreachable(); assert(false);
	}
}

// Arithmetic grouping
AstIndex nullParen(ref Parser p, Token token, int rbp) {
	AstIndex r = p.expr(rbp);
	p.expectAndConsume(TokenType.RPAREN);
	//r.flags |= NFLG.parenthesis; // NOTE: needed if ternary operator is needed
	return r;
}

// Prefix operator
// ["+", "-", "!", "~", "*", "&", "++", "--"] <expr>
AstIndex nullPrefixOp(ref Parser p, Token token, int rbp) {
	AstIndex right = p.expr(rbp);
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
AstIndex nullCast(ref Parser p, Token token, int rbp) {
	p.expectAndConsume(TokenType.LPAREN);
	AstIndex type = p.parse_type_expected();
	p.expectAndConsume(TokenType.RPAREN);
	AstIndex right = p.expr(rbp);
	return p.make!TypeConvExprNode(token.index, type, IrIndex(), right);
}

// Left Denotations -- tokens that take an expression on the left

// <expr> "++" / "--"
AstIndex leftIncDec(ref Parser p, Token token, int rbp, AstIndex left) {
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

// <expr> "[" <expr> "]"
AstIndex leftIndex(ref Parser p, Token token, int rbp, AstIndex array) {
	AstIndex index = p.expr(0);
	p.expectAndConsume(TokenType.RBRACKET);
	return p.makeExpr!IndexExprNode(token.index, array, index);
}

// Normal binary operator <expr> op <expr>
AstIndex leftBinaryOp(ref Parser p, Token token, int rbp, AstIndex left) {
	AstIndex right = p.expr(rbp);
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
		case STAR: op = BinOp.MULT; break;                        // *
		case XOR: op = BinOp.XOR; break;                          // ^

		// member access
		case DOT:                                                 // .
			AstIndex name;
			AstNode* rightNode = p.context.getAstNode(right);
			if (rightNode.astType != AstType.expr_name_use) {
				p.context.error(token.index,
					"Expected identifier after '.', while got '%s'",
					p.context.getTokenString(token.index));
			}
			else
				name = right;
			return p.makeExpr!MemberExprNode(token.index, left, name);

		default:
			p.context.internal_error(token.index, "parse leftBinaryOp %s", token.type);
			assert(false);
	}
	return p.makeExpr!BinaryExprNode(token.index, op, left, right);
}

// Binary assignment operator <expr> op= <expr>
AstIndex leftAssignOp(ref Parser p, Token token, int rbp, AstIndex left) {
	AstIndex right = p.expr(rbp);
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
AstIndex leftFuncCall(ref Parser p, Token token, int unused_rbp, AstIndex callee) {
	Array!AstIndex args;
	while (p.tok.type != TokenType.RPAREN) {
		// We don't want to grab the comma, e.g. it is NOT a sequence operator.
		args.put(p.context.arrayArena, p.expr(COMMA_PREC));
		// allows trailing comma too
		if (p.tok.type == TokenType.COMMA)
			p.nextToken;
	}
	p.expectAndConsume(TokenType.RPAREN);
	return p.makeExpr!CallExprNode(token.index, callee, args);
}