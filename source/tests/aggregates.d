/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module tests.aggregates;

import std.stdio;
import tester;

Test[] aggregatesTests() { return collectTests!(tests.aggregates)(); }


@TestInfo(&tester64)
immutable aggr64 = q{--- aggr64
	// Test structs
	struct Big {
		i64 a;
		i64 b;
	}
	struct Big2
	{
		u8 r;
		u8 g;
		u8 b;
		u8 a;
		u8 r2;
		u8 g2;
		u8 b2;
		u8 a2;
	}
	struct Small {
		i32 a;
		i32 b;
	}
	struct Small3 {
		i32 a;
		i16 b;
		i16 c;
	}
	struct Micro {
		u8 a;
		u8 b;
	}
	struct Mini {
		Micro a;
		Micro b;
	}
	struct Single_u8  { u8  a; }
	struct Single_u16 { u16 a; }
	struct Single_u32 { u32 a; }
	struct Single_u64 { u64 a; }
	// constructor is a function (expression) that returns struct type
	// can compile it into create_aggregate instruction
	// - default initialization of members
	// + return result (by ptr)
	Small returnSmallStruct() {
		return Small(10, 42);
	}
	Single_u8  return_Single_u8_const () { return Single_u8 (42); }
	Single_u16 return_Single_u16_const() { return Single_u16(42); }
	Single_u32 return_Single_u32_const() { return Single_u32(42); }
	Single_u64 return_Single_u64_const() { return Single_u64(42); }

	Single_u8  return_Single_u8 (u8  val) { return Single_u8 (val); }
	Single_u16 return_Single_u16(u16 val) { return Single_u16(val); }
	Single_u32 return_Single_u32(u32 val) { return Single_u32(val); }
	Single_u64 return_Single_u64(u64 val) { return Single_u64(val); }
	// return aggregate by storing into hidden first parameter
	Big returnBigStruct() {
		return Big(10, 42);
	}
	Big returnBigStruct2() {
		Big res = Big(10, 42);
		return res;
	}
	Small buildSmallStruct(i32 a, i32 b) {
		return Small(a, b);
	}
	Small3 buildSmall3Struct(i32 a, i16 b, i16 c) {
		return Small3(a, b, c);
	}
	Mini buildMiniStruct(u8 a, u8 b, u8 c, u8 d) {
		return Mini(Micro(a, b), Micro(c, d));
	}
	Big2 buildBig2Struct(u8 r, u8 g, u8 b, u8 a) {
		return Big2(r, 42, b, 42, 42, g, 42, a);
	}
	Mini returnMiniStruct() {
		return Mini(Micro(10, 42), Micro(120, 3));
	}
	// - pass as arg (fits in register)
	Small passArgSmallStruct() {
		return receiveArgSmallStruct(Small(10, 42));
	}
	Small passArgSmallStructVar(i32 x, i32 y) {
		return receiveArgSmallStruct(Small(x+1, y+1));
	}
	// - pass as arg (fits in register, pushed)
	Small passArgSmallStructPush() {
		return receiveArgSmallStructPush(1,2,3,4,Small(10, 42));
	}
	// - pass as arg (by ptr)
	Big passArgBigStruct() {
		return receiveArgBigStruct(Big(10, 42));
	}
	// - pass as arg (by ptr, pushed)
	Big passArgBigStructPush() {
		return receiveArgBigStructPush(1,2,3,4,Big(10, 42));
	}
	// - receive parameter (fits in register)
	Small receiveArgSmallStruct(Small arg) { return arg; }
	Small receiveArgSmallStructPush(i32,i32,i32,i32,Small arg) { return arg; }
	// - receive parameter (by ptr)
	Big receiveArgBigStruct(Big arg) { return arg; }
	Big receiveArgBigStructPush(i32,i32,i32,i32,Big arg) { return arg; }
	// - pass member as arg (by ptr)
	// - pass member as arg (fits in register)
	// - receive result (fits in register)
	// - receive result (by ptr)
	// - return result (fits in register)
	// - store in memory
	// - load from memory
	// - set member
	// - get member
	// - get member ptr
	// - get ptr
};
void tester64(ref TestContext ctx) {
	static struct Big {
		long a;
		long b;
	}
	static struct Big2
	{
		ubyte r;
		ubyte g;
		ubyte b;
		ubyte a;
		ubyte r2;
		ubyte g2;
		ubyte b2;
		ubyte a2;
	}
	static struct Small {
		int a;
		int b;
	}
	static struct Small3 {
		int a;
		short b;
		short c;
	}
	static struct Micro {
		ubyte a;
		ubyte b;
	}
	static struct Mini {
		Micro a;
		Micro b;
	}
	static struct Single_u8  { ubyte  a; }
	static struct Single_u16 { ushort a; }
	static struct Single_u32 { uint   a; }
	static struct Single_u64 { ulong  a; }

	auto returnSmallStruct = ctx.getFunctionPtr!(Small)("returnSmallStruct");
	assert(returnSmallStruct() == Small(10, 42));

	auto return_Single_u8_const = ctx.getFunctionPtr!(Single_u8)("return_Single_u8_const");
	assert(return_Single_u8_const() == Single_u8(42));
	auto return_Single_u16_const = ctx.getFunctionPtr!(Single_u16)("return_Single_u16_const");
	assert(return_Single_u16_const() == Single_u16(42));
	auto return_Single_u32_const = ctx.getFunctionPtr!(Single_u32)("return_Single_u32_const");
	assert(return_Single_u32_const() == Single_u32(42));
	auto return_Single_u64_const = ctx.getFunctionPtr!(Single_u64)("return_Single_u64_const");
	assert(return_Single_u64_const() == Single_u64(42));

	auto return_Single_u8 = ctx.getFunctionPtr!(Single_u8, ubyte)("return_Single_u8");
	assert(return_Single_u8(42) == Single_u8(42));
	auto return_Single_u16 = ctx.getFunctionPtr!(Single_u16, ushort)("return_Single_u16");
	assert(return_Single_u16(42) == Single_u16(42));
	auto return_Single_u32 = ctx.getFunctionPtr!(Single_u32, uint)("return_Single_u32");
	assert(return_Single_u32(42) == Single_u32(42));
	auto return_Single_u64 = ctx.getFunctionPtr!(Single_u64, ulong)("return_Single_u64");
	assert(return_Single_u64(42) == Single_u64(42));

	auto returnBigStruct = ctx.getFunctionPtr!(Big)("returnBigStruct");
	assert(returnBigStruct() == Big(10, 42));

	auto returnBigStruct2 = ctx.getFunctionPtr!(Big)("returnBigStruct2");
	assert(returnBigStruct2() == Big(10, 42));

	auto passArgBigStruct = ctx.getFunctionPtr!(Big)("passArgBigStruct");
	assert(passArgBigStruct() == Big(10, 42));

	auto passArgBigStructPush = ctx.getFunctionPtr!(Big)("passArgBigStructPush");
	assert(passArgBigStructPush() == Big(10, 42));

	auto passArgSmallStruct = ctx.getFunctionPtr!(Small)("passArgSmallStruct");
	assert(passArgSmallStruct() == Small(10, 42));

	auto passArgSmallStructVar = ctx.getFunctionPtr!(Small, int, int)("passArgSmallStructVar");
	assert(passArgSmallStructVar(10, 42) == Small(11, 43));

	auto passArgSmallStructPush = ctx.getFunctionPtr!(Small)("passArgSmallStructPush");
	assert(passArgSmallStructPush() == Small(10, 42));

	auto buildSmallStruct = ctx.getFunctionPtr!(Small, int, int)("buildSmallStruct");
	assert(buildSmallStruct(10, 42) == Small(10, 42));

	auto buildSmall3Struct = ctx.getFunctionPtr!(Small3, int, short, short)("buildSmall3Struct");
	assert(buildSmall3Struct(10, 42, 120) == Small3(10, 42, 120));

	auto buildMiniStruct = ctx.getFunctionPtr!(Mini, ubyte, ubyte, ubyte, ubyte)("buildMiniStruct");
	assert(buildMiniStruct(10, 42, 120, 3) == Mini(Micro(10, 42), Micro(120, 3)));

	auto buildBig2Struct = ctx.getFunctionPtr!(Big2, ubyte, ubyte, ubyte, ubyte)("buildBig2Struct");
	assert(buildBig2Struct(10, 42, 120, 3) == Big2(10, 42, 120, 42, 42, 42, 42, 3));

	auto returnMiniStruct = ctx.getFunctionPtr!(Mini)("returnMiniStruct");
	assert(returnMiniStruct() == Mini(Micro(10, 42), Micro(120, 3)));
}


@TestInfo(&tester129)
immutable aggr129 = q{--- aggr129
	// Extract member from small struct (0 offset)
	struct Point { i32 x; i32 y; }
	void run(Point* points, Point neighbor)
	{
		Point* t = &points[neighbor.x]; // neighbor.x is 0th member
		t.x = 42;
	}
};
void tester129(ref TestContext ctx) {
	static struct Point { int x; int y; }
	auto run = ctx.getFunctionPtr!(void, Point*, Point)("run");
	Point point;
	run(&point, Point(0, 0));
	assert(point == Point(42, 0));
}


@TestInfo(&tester130)
immutable aggr130 = q{--- aggr130
	// Extract member from small struct (1 offset)
	struct Point { i32 x; i32 y; }
	void run(Point* points, Point neighbor)
	{
		Point* t = &points[neighbor.y]; // neighbor.y is 1st member
		t.y = 42;
	}
};
void tester130(ref TestContext ctx) {
	static struct Point { int x; int y; }
	auto run = ctx.getFunctionPtr!(void, Point*, Point)("run");
	Point point;
	run(&point, Point(0, 0));
	assert(point == Point(0, 42));
}


@TestInfo(&tester131)
immutable aggr131 = q{--- aggr131
	// Construct and store into ptr
	struct Point { i32 x; i32 y; }
	void run(Point* point, i32 x, i32 y)
	{
		*point = Point(x, y);
	}
};
void tester131(ref TestContext ctx) {
	static struct Point { int x; int y; }
	auto run = ctx.getFunctionPtr!(void, Point*, int, int)("run");
	Point point;
	run(&point, 42, 90);
	assert(point == Point(42, 90));
}


@TestInfo(&tester132, [HostSymbol("consume", cast(void*)&aggr132_external_consume)])
immutable aggr132 = q{--- aggr132
	// Bug. Wrong size of shl used when building small aggregate
	struct Point { i32 x; i32 y; }
	@extern(module, "host")
	void consume(i32, i32);
	void run(Point* player)
	{
		Point point;
		Point* ptr = &point;
		*ptr = Point(player.x, player.y);
		consume(point.x, point.y);
	}
};
extern(C) void aggr132_external_consume(int x, int y) {
	assert(x == 42);
	assert(y == 90);
}
void tester132(ref TestContext ctx) {
	static struct Point { int x; int y; }
	auto run = ctx.getFunctionPtr!(void, Point*)("run");
	Point p = Point(42, 90);
	run(&p);
}


@TestInfo(&tester133, [HostSymbol("consume", cast(void*)&aggr133_external_consume)])
immutable aggr133 = q{--- aggr133
	// Bug. Wrong size of shr used when deconstructing small aggregate
	struct Point { i32 x; i32 y; }
	@extern(module, "host")
	void consume(i32, i32);
	void run(Point point)
	{
		consume(point.x, point.y);
	}
};
extern(C) void aggr133_external_consume(int x, int y) {
	assert(x == 42);
	assert(y == 90);
}
void tester133(ref TestContext ctx) {
	static struct Point { int x; int y; }
	auto run = ctx.getFunctionPtr!(void, Point)("run");
	Point p = Point(42, 90);
	run(p);
}


@TestInfo(&tester134)
immutable aggr134 = q{--- aggr134
	// SysV ABI
	struct Struct { i64 x; i64 y; }
	Struct run(Struct s) { return s; }
};
void tester134(ref TestContext ctx) {
	static struct Point { long x; long y; }
	auto run = ctx.getFunctionPtr!(Point, Point)("run");
	assert(run(Point(1, 2)) == Point(1, 2));
}


@TestInfo(&tester135)
immutable aggr135 = q{--- aggr135
	// SysV ABI
	struct vec1 { f32 x; }
	struct vec2 { f32 x; f32 y; }
	struct vec3 { f32 x; f32 y; f32 z; }
	struct vec4 { f32 x; f32 y; f32 z; f32 w; }
	struct vec5 { f32 x; f32 y; f32 z; f32 w; f32 q; }
	vec1 pass_vec1(vec1 v) { return v; }
	vec2 pass_vec2(vec2 v) { return v; }
	vec3 pass_vec3(vec3 v) { return v; }
	void pass_vec3_ptr(vec3* res, vec3 v) {
		*res = pass_vec3(v);
	}
	vec4 pass_vec4(vec4 v) { return v; }
	vec5 pass_vec5(vec5 v) { return v; }
};
void tester135(ref TestContext ctx) {
	static struct vec1 { float x; }
	static struct vec2 { float x; float y; }
	static struct vec3 { float x; float y; float z; }
	static struct vec4 { float x; float y; float z; float w; }
	static struct vec5 { float x; float y; float z; float w; float q; }

	auto pass_vec1 = ctx.getFunctionPtr!(vec1, vec1)("pass_vec1");
	assert(pass_vec1(vec1(1)) == vec1(1));

	auto pass_vec2 = ctx.getFunctionPtr!(vec2, vec2)("pass_vec2");
	assert(pass_vec2(vec2(1, 2)) == vec2(1, 2));

	auto pass_vec3_ptr = ctx.getFunctionPtr!(void, vec3*, vec3)("pass_vec3_ptr");
	vec3 r1;
	pass_vec3_ptr(&r1, vec3(1, 2, 3));
	assert(r1 == vec3(1, 2, 3));

	auto pass_vec3 = ctx.getFunctionPtr!(vec3, vec3)("pass_vec3");
	assert(pass_vec3(vec3(1, 2, 3)) == vec3(1, 2, 3));

	auto pass_vec4 = ctx.getFunctionPtr!(vec4, vec4)("pass_vec4");
	assert(pass_vec4(vec4(1, 2, 3, 4)) == vec4(1, 2, 3, 4));

	auto pass_vec5 = ctx.getFunctionPtr!(vec5, vec5)("pass_vec5");
	assert(pass_vec5(vec5(1, 2, 3, 4, 5)) == vec5(1, 2, 3, 4, 5));
}


@TestInfo(&tester136)
immutable aggr136 = q{--- aggr136
	// SysV ABI
	struct vec1 { f32[1] x; }
	struct vec2 { f32[2] x; }
	struct vec3 { f32[3] x; }
	struct vec4 { f32[4] x; }
	struct vec5 { f32[5] x; }
	vec1 pass_vec1(vec1 v) { return v; }
	vec2 pass_vec2(vec2 v) { return v; }
	vec3 pass_vec3(vec3 v) { return v; }
	vec4 pass_vec4(vec4 v) { return v; }
	vec5 pass_vec5(vec5 v) { return v; }
};
void tester136(ref TestContext ctx) {
	static struct vec1 { float[1] x; }
	static struct vec2 { float[2] x; }
	static struct vec3 { float[3] x; }
	static struct vec4 { float[4] x; }
	static struct vec5 { float[5] x; }

	auto pass_vec1 = ctx.getFunctionPtr!(vec1, vec1)("pass_vec1");
	assert(pass_vec1(vec1([1])) == vec1([1]));

	auto pass_vec2 = ctx.getFunctionPtr!(vec2, vec2)("pass_vec2");
	assert(pass_vec2(vec2([1, 2])) == vec2([1, 2]));

	auto pass_vec3 = ctx.getFunctionPtr!(vec3, vec3)("pass_vec3");
	assert(pass_vec3(vec3([1, 2, 3])) == vec3([1, 2, 3]));

	auto pass_vec4 = ctx.getFunctionPtr!(vec4, vec4)("pass_vec4");
	assert(pass_vec4(vec4([1, 2, 3, 4])) == vec4([1, 2, 3, 4]));

	auto pass_vec5 = ctx.getFunctionPtr!(vec5, vec5)("pass_vec5");
	assert(pass_vec5(vec5([1, 2, 3, 4, 5])) == vec5([1, 2, 3, 4, 5]));
}


@TestInfo(&tester137)
immutable aggr137 = q{--- aggr137
	// SysV ABI
	struct vec1 { f32 x; i32 y; }
	struct vec2 { i32 x; f32 y; }
	struct vec3 { i32 x; f32 y; f32 z; i32 w; }
	struct vec4 { f32 x; i32 y; i32 z; f32 w; }
	struct vec5 { i32 x; i32 y; f32 z; f32 w; }
	struct vec6 { f32 x; f32 y; i32 z; i32 w; }
	vec1 pass_vec1(vec1 v) { return v; }
	vec2 pass_vec2(vec2 v) { return v; }
	vec3 pass_vec3(vec3 v) { return v; }
	vec4 pass_vec4(vec4 v) { return v; }
	vec5 pass_vec5(vec5 v) { return v; }
	vec6 pass_vec6(vec6 v) { return v; }
};
void tester137(ref TestContext ctx) {
	static struct vec1 { float x; int y; }
	static struct vec2 { int x; float y; }
	static struct vec3 { int x; float y; float z; int w; }
	static struct vec4 { float x; int y; int z; float w; }
	static struct vec5 { int x; int y; float z; float w; }
	static struct vec6 { float x; float y; int z; int w; }

	auto pass_vec1 = ctx.getFunctionPtr!(vec1, vec1)("pass_vec1");
	assert(pass_vec1(vec1(1, 2)) == vec1(1, 2));

	auto pass_vec2 = ctx.getFunctionPtr!(vec2, vec2)("pass_vec2");
	assert(pass_vec2(vec2(1, 2)) == vec2(1, 2));

	auto pass_vec3 = ctx.getFunctionPtr!(vec3, vec3)("pass_vec3");
	assert(pass_vec3(vec3(1, 2, 3, 4)) == vec3(1, 2, 3, 4));

	auto pass_vec4 = ctx.getFunctionPtr!(vec4, vec4)("pass_vec4");
	assert(pass_vec4(vec4(1, 2, 3, 4)) == vec4(1, 2, 3, 4));

	auto pass_vec5 = ctx.getFunctionPtr!(vec5, vec5)("pass_vec5");
	assert(pass_vec5(vec5(1, 2, 3, 4)) == vec5(1, 2, 3, 4));

	auto pass_vec6 = ctx.getFunctionPtr!(vec6, vec6)("pass_vec6");
	assert(pass_vec6(vec6(1, 2, 3, 4)) == vec6(1, 2, 3, 4));
}


@TestInfo(&tester138)
immutable aggr138 = q{--- aggr138
	// lowering
	struct Point { i32 x; i32 y; }
	i32 getX(Point neighbor) {
		return neighbor.x; // neighbor.x is member 0
	}
	i32 getY(Point neighbor) {
		return neighbor.y; // neighbor.y is member 1
	}
};
void tester138(ref TestContext ctx) {
	static struct Point { int x; int y; }
	auto getX = ctx.getFunctionPtr!(int, Point)("getX");
	auto getY = ctx.getFunctionPtr!(int, Point)("getY");
	assert(getX(Point(1, 2)) == 1);
	assert(getY(Point(1, 2)) == 2);
}


@TestInfo(&tester139, [HostSymbol("external", cast(void*)&aggr139_external)])
immutable aggr139 = q{--- aggr139
	// passing address of member var
	struct Struct {
		u64 var2;
		u64 var;
		u64 fun() {
			external(&var);
			return var;
		}
	}
	u64 run() {
		Struct s;
		return s.fun();
	}
	@extern(module, "host")
	void external(u64*);
};
extern(C) void aggr139_external(ulong* ptr) {
	*ptr = 42;
}
void tester139(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(ulong)("run")() == 42);
}
