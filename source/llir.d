import std.stdio;
import std.bitmanip : bitfields;

void main()
{
	LirFunction fun;
}

struct LirFunction
{
	LirInstruction[] instructions;
	LirMemAddress[] addresses;
	IrConstant[] constants;
}

enum LirValueKind : ubyte
{
	virtReg,
	physReg,
	phi,
	con,
	param
}

enum LirConstKind : ubyte
{
	literal,
	stackSlotId
}

enum LirValueType : ubyte
{
	b,w,d,q
}

struct LirInstruction
{
	ushort op;
	LirRef result;
	union {
		struct { LirRef arg0, arg1, arg2, arg3; }
		LirRef[4] args;
	}
}

struct SibScale { ubyte bits; ubyte value() { return cast(ubyte)(1 << bits); } }
enum MemAddrType : ubyte {
	disp32,           // [                     disp32]
	indexDisp32,      // [       (index * s) + disp32]
	base,             // [base                       ]
	baseDisp32,       // [base +             + disp32]
	baseIndex,        // [base + (index * s)         ]
	baseIndexDisp32,  // [base + (index * s) + disp32]
	baseDisp8,        // [base +             + disp8 ]
	baseIndexDisp8,   // [base + (index * s) + disp8 ]
	ripDisp32         // [RIP  +             + disp32]
}
struct LirMemAddress
{
	ubyte typeStorage; // MemAddrType | 0b1_0000;
	SibScale scale;
	LirRef baseReg;
	LirRef indexReg;
	LirRef disp; // disp8 is stored here too

	MemAddrType type() { return cast(MemAddrType)(typeStorage & 0b1111); }
}

/// Stores numeric constant data
struct IrConstant
{
	this(long value) {
		this.i64 = value;

		if (cast(byte)(value & 0xFF) == value)
			numSignedBytes = 1;
		else if (cast(short)(value & 0xFFFF) == value)
			numSignedBytes = 2;
		else if (cast(int)(value & 0xFFFF_FFFF) == value)
			numSignedBytes = 4;
		else
			numSignedBytes = 8;

		if (cast(ubyte)(value & 0xFF) == value)
			numUnsignedBytes = 1;
		else if (cast(ushort)(value & 0xFFFF) == value)
			numUnsignedBytes = 2;
		else if (cast(uint)(value & 0xFFFF_FFFF) == value)
			numUnsignedBytes = 4;
		else
			numUnsignedBytes = 8;
	}
	ubyte numSignedBytes;
	ubyte numUnsignedBytes;
	ushort numUses;
	union {
		bool i1;
		byte i8;
		short i16;
		int i32;
		long i64;
	}

	void addUser() { ++numUses; }
}

struct LirRef
{
	this(uint idx, LirValueKind k, LirValueType t) {
		index = idx; kind = k; type = t;
	}
	this(StackSlotId idx) {
		constIndex = idx; constKind = LirConstKind.stackSlotId; kind = LirValueKind.con; type = LirValueType.q;
	}
	this(uint idx, LirConstKind c, LirValueType t) {
		constIndex = idx; constKind = c; kind = LirValueKind.con; type = t;
	}
	union
	{
		uint _payload = uint.max;
		mixin(bitfields!(
			uint,         "index",     27, // instruction/phi index
			LirValueKind, "kind",       3,
			LirValueType, "type",       2
		));
		mixin(bitfields!(
			uint,        "constIndex",     25,
			LirConstKind, "constKind",      2, // 2 bits are taken out of `index`
			uint,        "",                5  // kind and type
		));
	}
	static assert(LirConstKind.max <= 0b11, "2 bits are reserved");
	static assert(LirValueType.max <= 0b11, "2 bits are reserved");
	static assert(LirValueKind.max <= 0b111, "3 bits are reserved");
	bool isCon() { return kind == LirValueKind.con; }
	bool isLiteral() { return kind == LirValueKind.con && constKind == LirConstKind.literal; }
	bool isDefined() { return _payload != uint.max; }
}

enum Amd64Op : ushort {
	add,
	sub,
	mov,
	imul,
	or,
	and,
	xor,
	not,
	cmp,
	inc,
	dec,
	neg,
	mul,
	div,
	jmp,
	call,
	jcc,
	setcc,
	test,
	movsx,
	movzx,
	ret,
	pop,
	push,
	lea
}

struct StackSlotId
{
	uint id = uint.max;
	alias id this;
	bool isNull() { return id == uint.max; }
}


struct Buffer(T)
{
	import std.experimental.allocator.gc_allocator;
	alias allocator = GCAllocator.instance;

	T[] buf;
	// Must be kept private since it can be used to check for avaliable space
	// when used as output range
	private size_t length;

	bool empty() { return length == 0; }

	void put(T[] items ...)
	{
		reserve(items.length);
		buf[length..length+items.length] = items;
		length += items.length;
	}

	void put(R)(R itemRange)
	{
		foreach(item; itemRange)
			put(item);
	}

	void stealthPut(T item)
	{
		reserve(1);
		buf[length] = item;
	}

	ref T opIndex(size_t at)
	{
		return buf[at];
	}

	ref T back() { return buf[length-1]; }

	inout(T[]) data() inout {
		return buf[0..length];
	}

	void clear() nothrow {
		length = 0;
	}

	size_t capacity() const @property {
		return buf.length;
	}

	void reserve(size_t items)
	{
		if (buf.length - length < items)
		{
			import core.memory;
			GC.removeRange(buf.ptr);
			size_t newCapacity = nextPOT(buf.length + items);
			void[] tmp = buf;
			allocator.reallocate(tmp, newCapacity*T.sizeof);
			buf = cast(T[])tmp;
			GC.addRange(buf.ptr, buf.length * T.sizeof, typeid(T));
		}
	}

	void removeInPlace(size_t index)
	{
		if (index+1 != length)
		{
			buf[index] = buf[length-1];
		}
		--length;
	}

	void unput(size_t numItems)
	{
		length -= numItems;
	}
}

T nextPOT(T)(T x)
{
	--x;
	x |= x >> 1;  // handle  2 bit numbers
	x |= x >> 2;  // handle  4 bit numbers
	x |= x >> 4;  // handle  8 bit numbers
	static if (T.sizeof >= 2) x |= x >> 8;  // handle 16 bit numbers
	static if (T.sizeof >= 4) x |= x >> 16; // handle 32 bit numbers
	static if (T.sizeof >= 8) x |= x >> 32; // handle 64 bit numbers
	++x;

	return x;
}
