/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module tests.stashed;

import tester;

immutable input1 = q{
	i32 a;
	struct structWIP {
		i64 e;
		i32 member(i32 param) {
			i32 c;
			i32 d;
			a = b + c + d + e + g;
		}
		i32 g;
	}
	i32 b;
};

immutable input2 = q{void e() {
	i32 a;
	i32 b;
	i32 c;

	void f() {
		i32 a;
		i32 b;
		i32 c;
		a = b + c;
	}

	void g() {
		i32 a;
		i32 b;

		void h() {
			i32 c;
			i32 d;
			c = a + d;
		}

		void i() {
			i32 b;
			i32 d;
			b = a + c;
		}

		b = a + c;
	}

	a = b + c;
}};

immutable input3 = q{
	A b;
	struct A{
		void fun(i32 param) {
			//a = 1;
			i32 a;
			a = (param + 1) - var + fun42();
		}
	}
	A a;
	i32 var;
	i32 fun42() { return 42; }
};

immutable input4 = q{
	struct A {
		int x;
		struct B { int y; }
		B b;
	}

	int i=0;
	int j=0;

	void f() {
		A a;
		a.x = 1+i*j;
		a.b.y = 2;
		bool b = 3 == a.x;
		if ( i < j ) f();
	}
};

// test implicit casting
immutable input5 = q{void f() {
	//struct A{}
	//A a;
	//if (a){} // error
	f32 var_f32;
	f64 var_f64;
	//var_f32 = var_f64; // error
	var_f64 = var_f32;

	i8 var_i8;
	if (var_i8){}
	i16 var_i16;
	if (var_i16){}
	i32 var_i32;
	if (var_i32){}
	i64 var_i64;
	if (var_i64){}

	u8 var_u8;
	if (var_u8){}
	u16 var_u16;
	if (var_u16){}
	u32 var_u32;
	if (var_u32){}
	u64 var_u64;
	if (var_u64){}
}};

immutable input6 = q{void f() {
	f32 var_f32;
	f64 var_f64;
	var_f64 = var_f32;
	i8 var_i8;
	i16 var_i16;
	i32 var_i32;
	i64 var_i64;
	u8 var_u8;
	u16 var_u16;
	u32 var_u32;
	u64 var_u64;

	var_i8 + var_i16;
	var_i8 + var_i32;
	var_i8 + var_i64;
	var_i8 + var_u8;
	var_i8 + var_u16;
	var_i8 + var_u32;
	var_i8 + var_u64;
	var_i8 + var_f32;
	var_i8 + var_f64;

	var_i16 + var_i32;
	var_i16 + var_i64;
	var_i16 + var_u8;
	var_i16 + var_u16;
	var_i16 + var_u32;
	var_i16 + var_u64;
	var_i16 + var_f32;
	var_i16 + var_f64;

	var_i32 + var_i32;
	var_i32 + var_i64;
	var_i32 + var_u8;
	var_i32 + var_u16;
	var_i32 + var_u32;
	var_i32 + var_u64;
	var_i32 + var_f32;
	var_i32 + var_f64;

	var_i64 + var_i64;
	var_i64 + var_u8;
	var_i64 + var_u16;
	var_i64 + var_u32;
	var_i64 + var_u64;
	var_i64 + var_f32;
	var_i64 + var_f64;

	var_u8 + var_u8;
	var_u8 + var_u16;
	var_u8 + var_u32;
	var_u8 + var_u64;
	var_u8 + var_f32;
	var_u8 + var_f64;

	var_u16 + var_u16;
	var_u16 + var_u32;
	var_u16 + var_u64;
	var_u16 + var_f32;
	var_u16 + var_f64;

	var_u32 + var_u32;
	var_u32 + var_u64;
	var_u32 + var_f32;
	var_u32 + var_f64;

	var_u64 + var_u64;
	var_u64 + var_f32;
	var_u64 + var_f64;

	var_f32 + var_f32;
	var_f32 + var_f64;
}};

immutable inputX = q{
	#pragma(lib, "kernel32")
	u8 WriteConsoleA(
		void* hConsoleOutput,
		void* lpBuffer,
		u32 nNumberOfCharsToWrite,
		u32* lpNumberOfCharsWritten,
		void* lpReserved
	);
	#pragma(lib, "kernel32")
	void* GetStdHandle(u32 nStdHandle);
	enum : u32 {
		STD_INPUT_HANDLE  = 0xFFFFFFF6,
		STD_OUTPUT_HANDLE = 0xFFFFFFF5,
		STD_ERROR_HANDLE  = 0xFFFFFFF4
	}
	void main(void* hInstance, void* hPrevInstance, u8* lpCmdLine, i32 nShowCmd) {
		u8[] array = "Hello world";
		u32 numWritten;
		void* handle = GetStdHandle(STD_OUTPUT_HANDLE);
		WriteConsoleA(handle, array.ptr, array.length, &numWritten, null);
	}
};

immutable input30 = q{--- test30
	void SDL_SetMainReady();
	i32 SDL_Init(u32);
	void SDL_Quit();
	void* SDL_CreateWindow(u8* title, i32 x, i32 y, i32 w, i32 h, u32 flags);
	void* SDL_CreateRenderer(void* window, i32 index, u32 flags);
	void SDL_DestroyRenderer(void* renderer);
	void SDL_DestroyWindow(void* renderer);
	i32 SDL_PollEvent(SDL_Event* event);
	struct SDL_Event
	{
		u32 type;
		u8[52] padding;
	}
	void ExitProcess(u32 uExitCode);

	i32 main(void* hInstance, void* hPrevInstance, u8* lpCmdLine, i32 nShowCmd) {
		SDL_SetMainReady();
		if(SDL_Init(0x00000020) < 0) return 1;
		void* window = SDL_CreateWindow("SDL test via tiny_jit", 0x1FFF0000, 0x1FFF0000, 300, 100, 4);
		void* renderer = SDL_CreateRenderer(window, 0xFFFF_FFFF, 2);
		SDL_Event e;
		while (1)
		{
			SDL_PollEvent(&e);
			if (e.type == 0x100) // SDL_QUIT
				break;
		}
		SDL_DestroyRenderer(renderer);
		SDL_DestroyWindow(window);
		SDL_Quit();
		ExitProcess(0);
		return 0;
	}
};
auto test30 = Test("exe SDL", input30, null, null, null, [
	DllModule("SDL2", ["SDL_SetMainReady", "SDL_Init", "SDL_Quit",
		"SDL_CreateWindow", "SDL_CreateRenderer", "SDL_PollEvent",
		"SDL_DestroyRenderer", "SDL_DestroyWindow"]),
	DllModule("kernel32", ["ExitProcess"])]
);
