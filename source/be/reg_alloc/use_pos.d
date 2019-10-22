/// Copyright: Copyright (c) 2018-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module be.reg_alloc.use_pos;

import std.bitmanip : bitfields;

// position 0 is owned by entry block and can't be owned by any phi function
// so all use positions are > 0
enum MIN_USE_POS = 0;
enum MAX_USE_POS = (1 << 28) - 1;
enum ENUM_STEP = 2;

enum UseKind : ubyte {
	instruction,
	phi
}

struct UsePosition
{
	this(uint _pos, UseKind _kind)
	{
		pos = _pos;
		kind = _kind;
	}

	mixin(bitfields!(
		uint,    "pos",  28,
		UseKind, "kind",  4
	));

	void toString(scope void delegate(const(char)[]) sink) {
		import std.format : formattedWrite;
		final switch (kind) {
			case UseKind.instruction: sink.formattedWrite("(%s instr)", pos); break;
			case UseKind.phi: sink.formattedWrite("(%s phi)", pos); break;
		}
	}
}
