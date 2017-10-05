/* file: "tinyc.d" */

/* Copyright (C) 2001 by Marc Feeley, All Rights Reserved. */
// D port by MrSmith33 (C) 2014
module tinyc;


import std.stdio;
import std.algorithm : equal;

/*
 * This is a compiler for the Tiny-C language.  Tiny-C is a
 * considerably stripped down version of C and it is meant as a
 * pedagogical tool for learning about compilers.  The integer global
 * variables "a" to "z" are predefined and initialized to zero, and it
 * is not possible to declare new variables.  The compiler reads the
 * program from standard input and prints out the value of the
 * variables that are not zero.  The grammar of Tiny-C in EBNF is:
 *
 *  <program> ::= <statement>
 *  <statement> ::= "if" <paren_expr> <statement> |
 *                  "if" <paren_expr> <statement> "else" <statement> |
 *                  "while" <paren_expr> <statement> |
 *                  "do" <statement> "while" <paren_expr> ";" |
 *                  "{" { <statement> } "}" |
 *                  <expr> ";" |
 *                  ";"
 *  <paren_expr> ::= "(" <expr> ")"
 *  <expr> ::= <test> | <id> "=" <expr>
 *  <test> ::= <sum> | <sum> "<" <sum>
 *  <sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term>
 *  <term> ::= <id> | <int> | <paren_expr>
 *  <id> ::= "a" | "b" | "c" | "d" | ... | "z"
 *  <int> ::= <an_unsigned_decimal_integer>
 *
 * Here are a few invocations of the compiler:
 *
 * % echo "a=b=c=2<3;" | ./a.out
 * a = 1
 * b = 1
 * c = 1
 * % echo "{ i=1; while (i<100) i=i+i; }" | ./a.out
 * i = 128
 * % echo "{ i=125; j=100; while (i-j) if (i<j) j=j-i; else i=i-j; }" | ./a.out
 * i = 25
 * j = 25
 * % echo "{ i=1; do i=i+10; while (i<50); }" | ./a.out
 * i = 51
 * % echo "{ i=1; while ((i=i+10)<50) ; }" | ./a.out
 * i = 51
 * % echo "{ i=7; if (i<5) n=1; if (i<10) y=2; }" | ./a.out
 * i = 7
 * y = 2
 *
 * The compiler does a minimal amount of error checking to help
 * highlight the structure of the compiler.
 */


/*---------------------------------------------------------------------------*/

/* Lexer. */

enum { DO_SYM, ELSE_SYM, IF_SYM, WHILE_SYM, LBRA, RBRA, LPAR, RPAR,
			 PLUS, MINUS, LESS, SEMI, EQUAL, INT, ID, EOI };
string[] words = [ "do", "else", "if", "while", null ];

void syntax_error() { assert(false, "syntax error"); }

struct Lexer
{
	char delegate() charSource;

	char ch = ' ';
	int sym;
	int int_val;
	char[100] id_name;

	void next_ch() { ch = charSource(); }

	void next_sym()
	{
		while(ch == ' ' || ch == '\n') next_ch();

		switch (ch)
		{
			case 255: sym = EOI; break; //EOF
			case '{': next_ch(); sym = LBRA; break;
			case '}': next_ch(); sym = RBRA; break;
			case '(': next_ch(); sym = LPAR; break;
			case ')': next_ch(); sym = RPAR; break;
			case '+': next_ch(); sym = PLUS; break;
			case '-': next_ch(); sym = MINUS; break;
			case '<': next_ch(); sym = LESS; break;
			case ';': next_ch(); sym = SEMI; break;
			case '=': next_ch(); sym = EQUAL; break;
			default:
			{
				if (ch >= '0' && ch <= '9')
				{
					int_val = 0; /* missing overflow check */
					while (ch >= '0' && ch <= '9')
					{
						int_val = int_val*10 + (ch - '0');
						next_ch();
					}
					sym = INT;
				}
				else if (ch >= 'a' && ch <= 'z')
				{
					size_t i = 0; /* missing overflow check */
					while ((ch >= 'a' && ch <= 'z') || ch == '_')
					{
						id_name[i++] = ch;
						next_ch();
					}
					id_name[i] = '\0';
					sym = 0;
					while (words[sym] !is null && !equal(words[sym], id_name[0..i]))
					{
						sym = sym + 1;
					}
					if (words[sym] is null)
					{
						if (id_name[1] == '\0')
							sym = ID;
						else
							syntax_error();
					}
				}
				else
					syntax_error();
			}
		}
	}
}

/*---------------------------------------------------------------------------*/

/* Parser. */

enum { VAR, CST, ADD, SUB, LT, SET,
			 IF1, IF2, WHILE, DO, EMPTY, SEQ, EXPR, PROG }
struct Node
{
	int kind;
	Node* o1, o2, o3;
	int val;
}

struct Parser
{
	Lexer* lexer;

	Node* new_Node(int k)
	{
		Node* n = new Node;
		n.kind = k;
		return n;
	}

	Node* term()  /* <term> ::= <id> | <int> | <paren_expr> */
	{
		Node* n;
		if (lexer.sym == ID) { n=new_Node(VAR); n.val=lexer.id_name[0]-'a'; lexer.next_sym(); }
		else if (lexer.sym == INT) { n=new_Node(CST); n.val=lexer.int_val; lexer.next_sym(); }
		else n = paren_expr();
		return n;
	}

	Node* sum()  /* <sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term> */
	{
		Node* t, n = term();
		while (lexer.sym == PLUS || lexer.sym == MINUS)
		{
			t=n; n=new_Node(lexer.sym==PLUS?ADD:SUB); lexer.next_sym(); n.o1=t; n.o2=term();
		}
		return n;
	}

	Node* test()  /* <test> ::= <sum> | <sum> "<" <sum> */
	{
		Node* t, n = sum();
		if (lexer.sym == LESS)
		{
			t=n; n=new_Node(LT); lexer.next_sym(); n.o1=t; n.o2=sum();
		}
		return n;
	}

	Node* expr()  /* <expr> ::= <test> | <id> "=" <expr> */
	{
		Node* t, n;
		if (lexer.sym != ID) return test();
		n = test();
		if (n.kind == VAR && lexer.sym == EQUAL)
		{
			t=n; n=new_Node(SET); lexer.next_sym(); n.o1=t; n.o2=expr();
		}
		return n;
	}

	Node* paren_expr()  /* <paren_expr> ::= "(" <expr> ")" */
	{
		Node* n;
		if (lexer.sym == LPAR) lexer.next_sym(); else syntax_error();
		n = expr();
		if (lexer.sym == RPAR) lexer.next_sym(); else syntax_error();
		return n;
	}

	Node* statement()
	{
		Node* n;

		if (lexer.sym == IF_SYM)  /* "if" <paren_expr> <statement> */
		{
			n = new_Node(IF1);
			lexer.next_sym();
			n.o1 = paren_expr();
			n.o2 = statement();
			if (lexer.sym == ELSE_SYM)  /* ... "else" <statement> */
			{
				n.kind = IF2;
				lexer.next_sym();
				n.o3 = statement();
			}
		}
		else if (lexer.sym == WHILE_SYM)  /* "while" <paren_expr> <statement> */
		{
			n = new_Node(WHILE);
			lexer.next_sym();
			n.o1 = paren_expr();
			n.o2 = statement();
		}
		else if (lexer.sym == DO_SYM)  /* "do" <statement> "while" <paren_expr> ";" */
		{
			n = new_Node(DO);
			lexer.next_sym();
			n.o1 = statement();
			if (lexer.sym == WHILE_SYM)
				lexer.next_sym();
			else
				syntax_error();
			n.o2 = paren_expr();
			if (lexer.sym == SEMI)
				lexer.next_sym();
			else
				syntax_error();
		}
		else if (lexer.sym == SEMI)  /* ";" */
		{
			n = new_Node(EMPTY);
			lexer.next_sym();
		}
		else if (lexer.sym == LBRA)  /* "{" { <statement> } "}" */
		{
			n = new_Node(EMPTY);
			lexer.next_sym();
			while (lexer.sym != RBRA)
			{
				Node* temp = n;
				n = new_Node(SEQ);
				n.o1 = temp;
				n.o2 = statement();
			}
			lexer.next_sym();
		}
		else  /* <expr> ";" */
		{
			n = new_Node(EXPR);
			n.o1 = expr();
			if (lexer.sym == SEMI)
				lexer.next_sym();
			else
				syntax_error();
		}
		return n;
	}

	Node* program()  /* <program> ::= <statement> */
	{
		Node* n = new_Node(PROG);
		lexer.next_sym();
		n.o1 = statement();
		if (lexer.sym != EOI)
			syntax_error();
		return n;
	}
}


/*---------------------------------------------------------------------------*/

/* Code generator. */

enum { IFETCH, ISTORE, IPUSH, IPOP, IADD, ISUB, ILT, JZ, JNZ, JMP, HALT };
string[] opCodes = ["IFETCH", "ISTORE", "IPUSH", "IPOP", "IADD", "ISUB", "ILT", "JZ", "JNZ", "JMP", "HALT"];

string opCodeToStr(byte op) {
	if (op < opCodes.length)
		return opCodes[op];
	return "INVALID";
}

alias code = byte;

struct CodeGenerator
{
	byte* pc;

	void put(byte c) { *pc++ = c; } /* missing overflow check */
	byte* hole() { return pc++; }
	void fix(byte* src, byte* dst) { *src = cast(byte)(dst - src); } /* missing overflow check */

	void compile(Node* n)
	{
		byte* p1, p2;
		switch (n.kind)
		{
			case VAR  : put(IFETCH); put(cast(byte)n.val); break;
			case CST  : put(IPUSH); put(cast(byte)n.val); break;
			case ADD  : compile(n.o1); compile(n.o2); put(IADD); break;
			case SUB  : compile(n.o1); compile(n.o2); put(ISUB); break;
			case LT   : compile(n.o1); compile(n.o2); put(ILT); break;
			case SET  : compile(n.o2); put(ISTORE); put(cast(byte)n.o1.val); break;
			case IF1  : compile(n.o1); put(JZ); p1=hole(); compile(n.o2); fix(p1,pc); break;
			case IF2  : compile(n.o1); put(JZ); p1=hole(); compile(n.o2); put(JMP); p2=hole();
									fix(p1,pc); compile(n.o3); fix(p2,pc); break;
			case WHILE: p1=pc; compile(n.o1); put(JZ); p2=hole(); compile(n.o2);
									put(JMP); fix(hole(),p1); fix(p2,pc); break;
			case DO   : p1=pc; compile(n.o1); compile(n.o2); put(JNZ); fix(hole(),p1); break;
			case EMPTY: break;
			case SEQ  : compile(n.o1); compile(n.o2); break;
			case EXPR : compile(n.o1); put(IPOP); break;
			case PROG : compile(n.o1); put(HALT); break;
			default: break;
		}
	}
}

void printAST(Node* n, string indent = "")
{
	switch (n.kind)
	{
		case VAR  : writeln(indent, "VAR ", cast(char)(n.val+'a'));   break;
		case CST  : writeln(indent, "CST");   break;
		case ADD  : writeln(indent, "ADD");   printAST(n.o1, indent~"  "); printAST(n.o2, indent~"  "); break;
		case SUB  : writeln(indent, "SUB");   printAST(n.o1, indent~"  "); printAST(n.o2, indent~"  "); break;
		case LT   : writeln(indent, "LT");    printAST(n.o1, indent~"  "); printAST(n.o2, indent~"  "); break;
		case SET  : writeln(indent, "SET");   printAST(n.o2, indent~"  "); break;
		case IF1  : writeln(indent, "IF1");   printAST(n.o1, indent~"  "); printAST(n.o2, indent~"  "); break;
		case IF2  : writeln(indent, "IF2");   printAST(n.o1, indent~"  "); printAST(n.o2, indent~"  "); printAST(n.o3, indent~"  "); break;
		case WHILE: writeln(indent, "WHILE"); printAST(n.o1, indent~"  "); printAST(n.o2, indent~"  "); break;
		case DO   : writeln(indent, "DO");    printAST(n.o1, indent~"  "); printAST(n.o2, indent~"  "); break;
		case EMPTY: writeln(indent, "EMPTY"); break;
		case SEQ  : writeln(indent, "SEQ");   printAST(n.o1, indent~"  "); printAST(n.o2, indent~"  "); break;
		case EXPR : writeln(indent, "EXPR");  printAST(n.o1, indent~"  "); break;
		case PROG : writeln(indent, "PROG");  printAST(n.o1, indent~"  "); break;
		default: break;
	}
}

/*---------------------------------------------------------------------------*/

/* Jit compiler */

struct JitVM
{
	import amd64asm;
	import utils;

	ubyte[] mem;
	ubyte[] code;

	int[26] globals;

	void reset()
	{
		globals[] = 0;
	}

	void free()
	{
		free_executable_memory(mem);
		mem = null;
	}

	void run()
	{
		alias JittedFunc = void function(void*);
		JittedFunc func = cast(JittedFunc)mem.ptr;
		//assert(false);
		func(&globals[0]);
	}

	/// RAX stores ptr to variables int[26]
	enum VARS_REG = Register.AX;
	/// RCX temp
	enum TEMP_REG_1 = Register.CX;
	enum TEMP_REG_2 = Register.DX;
	CodeGen_x86_64 gen;

	void compile(Node* n)
	{
		if (mem.length == 0) mem = alloc_executable_memory(4096 * 64);
		gen.encoder.setBuffer(mem);

		gen.movq(VARS_REG, Register.CX); // Mov address into VARS_REG
		compileNode(n);
		code = gen.encoder.code;

		gen.encoder.resetPC();
	}

	void compileNode(Node* n)
	{
		switch (n.kind)
		{
			case VAR  :
				gen.movd(TEMP_REG_1, memAddrBaseDisp8(VARS_REG, cast(ubyte)(n.val * 4))); // IFETCH
				break;
			case CST  : // constant
				gen.movd(TEMP_REG_1, Imm32(n.val)); // n.val == literal value
				break;
			case ADD  :
				compileNode(n.o1);
					gen.pushq(TEMP_REG_1);
				compileNode(n.o2);
					gen.popq(TEMP_REG_2); // o2
					gen.addd(TEMP_REG_1, TEMP_REG_2); // o1 += o2
				break;
			case SUB  : // o1 - o2
				compileNode(n.o2);
					gen.pushq(TEMP_REG_1);
				compileNode(n.o1);
					gen.popq(TEMP_REG_2); // o2
					gen.subd(TEMP_REG_1, TEMP_REG_2); // o1 = o1 - o2
				break;
			case LT   : // o1 < o2
				compileNode(n.o1);
					gen.pushq(TEMP_REG_1);
				compileNode(n.o2);
					gen.popq(TEMP_REG_2); // o1
					gen.cmpd(TEMP_REG_2, TEMP_REG_1); // cmp(o1, o2) // memAddrBase(Register.SP)
					gen.setcc(Condition.L, TEMP_REG_1);
					gen.movzx_btod(TEMP_REG_1, TEMP_REG_1);
				break;
			case SET  :
				compileNode(n.o2); // result in TEMP_REG_1
					gen.movd(memAddrBaseDisp8(VARS_REG, cast(ubyte)(n.o1.val * 4)), TEMP_REG_1); // ISTORE;
				break;
			case IF1  :
				compileNode(n.o1); // paren_expr
					gen.testd(TEMP_REG_1, TEMP_REG_1);
					auto false_jump = gen.saveFixup();
					gen.jcc(Condition.Z, Imm32(0));
					auto nextInstrOff = gen.pc;
				compileNode(n.o2);
					false_jump.jcc(Condition.Z, jumpOffset(nextInstrOff, gen.pc));
				break;
			case IF2  :
				compileNode(n.o1); // paren_expr
					gen.testd(TEMP_REG_1, TEMP_REG_1);
					auto else_jump_fix = gen.saveFixup();
					gen.jcc(Condition.Z, Imm32(0));
					auto else_pc = gen.pc;
				compileNode(n.o2);
					auto end_jump = gen.saveFixup();
					gen.jmp(Imm32(0));
					auto then_pc = gen.pc;
					else_jump_fix.jcc(Condition.Z, jumpOffset(else_pc, gen.pc)); // fix cond -> else
				compileNode(n.o3);
					end_jump.jmp(jumpOffset(then_pc, gen.pc)); // fix then -> end
				break;
			case WHILE:
					auto condition_pc = gen.pc;
				compileNode(n.o1); // paren_expr
					gen.testd(TEMP_REG_1, TEMP_REG_1);
					auto break_fix = gen.saveFixup();
					gen.jcc(Condition.Z, Imm32(0)); // break
					auto break_pc = gen.pc;
				compileNode(n.o2); // statement
					auto continue_fix = gen.saveFixup();
					gen.jmp(Imm32(0));
					continue_fix.jmp(jumpOffset(gen.pc, condition_pc)); // continue
					break_fix.jcc(Condition.Z, jumpOffset(break_pc, gen.pc)); // fix cond -> end
				break;
			case DO   :
					auto do_pc = gen.pc;
				compileNode(n.o1); // do <statement>
				compileNode(n.o2); // while <paren_expr>
					gen.testd(TEMP_REG_1, TEMP_REG_1);
					auto continue_fix = gen.saveFixup();
					gen.jcc(Condition.NZ, Imm32(0));
					continue_fix.jcc(Condition.NZ, jumpOffset(gen.pc, do_pc)); // continue
				break;
			case EMPTY: break;
			case SEQ  : compileNode(n.o1); compileNode(n.o2); break;
			case EXPR : compileNode(n.o1); break;
			case PROG : compileNode(n.o1); gen.ret(); break;
			default: break;
		}
	}
}

/*---------------------------------------------------------------------------*/
char[1000] buf;
size_t buf_index;
struct Putter {
	static void reset() {
		buf_index = 0;
	}
	static void put(const char chr) {
		buf[buf_index++] = chr;
	}
}
char[] print_stack(int[] stack, int* stack_ptr)
{
	ptrdiff_t size = stack_ptr - stack.ptr;
	if (size > 0)
	{
		import std.format : formattedWrite;
		const(char)[] temp = buf;
		int[] stack_vals = stack[0..size];
		Putter.reset();
		foreach(int val; stack_vals)
		{
			formattedWrite(Putter(), "%s ", val);
		}
		return buf[0..buf_index];
	}
	return null;
}

/* Virtual machine. */
struct VM
{
	int[26] globals;

	void run(code[] _object)
	{
		int[1000] stack = void;
		int* sp = stack.ptr;
		code* pc = _object.ptr;

		loop: while(true)
		{
			//writefln("exec\t%s\t%s\t% 6s\t%s\t%s", pc - _object.ptr, *pc, opCodeToStr(*pc), sp - stack.ptr, print_stack(stack, sp));
			final switch (*pc++)
			{
				case IFETCH: *sp++ = globals[*pc++];               break;
				case ISTORE: globals[*pc++] = sp[-1];              break;
				case IPUSH : *sp++ = *pc++;                        break;
				case IPOP  : --sp;                                 break;
				case IADD  : sp[-2] = sp[-2] + sp[-1]; --sp;       break;
				case ISUB  : sp[-2] = sp[-2] - sp[-1]; --sp;       break;
				case ILT   : sp[-2] = sp[-2] < sp[-1]; --sp;       break;
				case JMP   : pc += *pc;                            break;
				case JZ    : if (*--sp == 0) pc += *pc; else pc++; break;
				case JNZ   : if (*--sp != 0) pc += *pc; else pc++; break;
				case HALT: break loop;
			}
		}
	}
}
