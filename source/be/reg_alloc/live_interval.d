/// Copyright: Copyright (c) 2018-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module be.reg_alloc.live_interval;

import all;

struct LiveInterval
{
	Array!LiveRange ranges;
	UsePosition[] uses;
	// hint tells from which range to start searching
	// Allows to restart search from the position we previously ended on.
	// On big functions allows to reduce O(n^2) search complexity to 2 iterations per search.
	LiveRangeIndex lastHit = LiveRangeIndex(0);

	uint from() {
		if (ranges.length > 0) return ranges[0].from;
		return MAX_USE_POS;
	}
	uint to() {
		if (ranges.length > 0) return ranges.back.to;
		return MAX_USE_POS;
	}

	IrIndex reg;
	IrIndex definition; // phys or virt reg
	IrIndex storageHint;
	IntervalIndex parent; // prev split interval to the left
	IntervalIndex child; // next split interval to the right
	ubyte regClass;

	bool isFixed() { return definition.isPhysReg; }
	bool isSplitChild() { return !parent.isNull; }
	bool isSplit() { return !child.isNull; }

	void toString(scope void delegate(const(char)[]) sink) {
		import std.format : formattedWrite;
		sink.formattedWrite("int(");
		if (definition.isDefined) sink.formattedWrite("%s, ", definition);
		sink.formattedWrite("%s, %s", ranges, uses);
		if (!parent.isNull) sink.formattedWrite(", par %s", parent);
		if (!child.isNull) sink.formattedWrite(", child %s", child);
		sink(")");
	}

	/// Set hint for register allocator
	void setVirtRegHint(IrIndex hint) {
		storageHint = hint;
	}

	// Skips all ranges to the left of the position, and returns first range after that
	// Range before returned does not contain position
	// Returned range.to > position
	// Returned range.from may be <= position or may be > position
	// If all ranges were skipped then returns NULL
	LiveRangeIndex getRightmostRange(uint position)
	{
		if (ranges.length == 0) return LiveRangeIndex.NULL;
		LiveRangeIndex hint = lastHit;
		if (lastHit >= ranges.length) hint = ranges.length - 1;
		if (ranges[hint].to > position) {
			if (hint == 0) {
				lastHit = LiveRangeIndex(0);
				return LiveRangeIndex(0);
			}
			foreach_reverse(i, range; ranges[0..hint]) {
				if (range.to <= position) {
					return LiveRangeIndex(i+1);
				}
				lastHit = LiveRangeIndex(i);
			}
		} else {
			foreach(i, range; ranges[hint..$]) {
				lastHit = LiveRangeIndex(i+hint);
				if (range.to > position) {
					return lastHit;
				}
			}
		}

		// Unoptimized
		//foreach(i, range; ranges) {
		//	if (position < range.to) return LiveRangeIndex(i);
		//}
		return LiveRangeIndex.NULL;
	}

	// returns rangeId pointing to range covering position or one to the left of pos.
	// returns NULL if empty interval or no ranges to the left
	LiveRangeIndex getLeftRange(uint position)
	{
		LiveRangeIndex result = LiveRangeIndex.NULL;
		foreach(i, range; ranges) {
			if (position >= range.from)
				return result;
			result = LiveRangeIndex(i);
		}
		return result;
	}

	// sets the definition position
	void setFrom(CompilationContext* context, uint from) {
		version(LivePrint) writefln("[LIVE] setFrom vreg.%s from %s", virtReg, from);

		if (ranges.empty) { // can happen if vreg had no uses (it is probably dead or used in phi above definition)
			addRange(context, from, from);
		} else {
			ranges[0].from = from;
		}
	}

	void prependUse(UsePosition use) {
		//writefln("prependUse %s %s", definition, use);
		uses[$-1] = use;
		--uses.length;
	}

	// bounds are from block start to block end of the same block
	// from is always == to block start for virtual intervals
	void addRange(CompilationContext* context, uint from, uint to)
	{
		version(LivePrint) writefln("[LIVE] addRange %s [%s; %s)", definition, from, to);
		LiveRange newRange = LiveRange(from, to);

		size_t cur = 0;
		size_t len = ranges.length;

		while (cur < len)
		{
			LiveRange* r = &ranges[cur];

			if (r.canBeMergedWith(newRange))
			{
				// merge all intersecting ranges into one
				r.merge(newRange);

				++cur;
				size_t firstToRemove = cur;

				while (cur < len && r.canBeMergedWith(ranges[cur])) {
					r.merge(ranges[cur]);
					++cur;
				}
				ranges.removeByShift(firstToRemove, cur-firstToRemove);

				return;
			}
			else if (to < r.from)
			{
				// we found insertion point before cur
				ranges.putAt(context.arrayArena, cur, newRange);
				return;
			}

			++cur;
		}

		// insert after last, no merge/insertion was made
		ranges.put(context.arrayArena, newRange);
	}

	bool hasUseAt(uint pos) {
		foreach (UsePosition use; uses) {
			if (use.pos == pos) return true;
		}
		return false;
	}

	uint firstUse() {
		// don't require uses by phi functions to be in register
		foreach(UsePosition use; uses)
			if (use.kind == UseKind.instruction) return use.pos;
		return MAX_USE_POS;
	}

	// incudes after
	uint nextUseAfter(uint after)
	{
		uint closest = MAX_USE_POS;
		foreach_reverse (UsePosition use; uses)
		{
			if (use.pos < after) break;
			// don't require uses by phi functions to be in register
			if (use.kind == UseKind.instruction)
				closest = use.pos;
		}
		return closest;
	}

	// others intervals can't use interval's register in this pos
	bool exclusivePosition(uint position)
	{
		foreach(range; ranges) {
			if (position < range.from) return false;
			if (position == range.from) return true;
			if (position < range.to) return true;
			// position >= to
		}
		return false;
	}

	// interval's register contains valid value at this position
	bool coversPosition(uint position)
	{
		foreach(range; ranges) {
			if (position < range.from)
				return false;
			else if (position <= range.to)
				return true;
			// position >= to
		}
		return false;
	}

	// retains uses before `pos`, returns uses >= `pos`
	UsePosition[] splitUsesBefore(uint pos) {
		foreach(i, use; uses) {
			if (use.pos >= pos) {
				UsePosition[] copy = uses;
				uses = uses[0..i];
				return copy[i..$];
			}
		}
		return null;
	}
}

uint firstIntersection(LiveInterval* a, LiveInterval* b)
{
	size_t len_a = a.ranges.length;
	if (len_a == 0) return MAX_USE_POS;

	size_t len_b = b.ranges.length;
	if (len_b == 0) return MAX_USE_POS;

	size_t i_a = 0;
	size_t i_b = 0;

	LiveRange r_a = a.ranges[i_a];
	LiveRange r_b = b.ranges[i_b];

	while (true)
	{
		if (r_a.intersectsWith(r_b)) {
			return max(r_a.from, r_b.from);
		} else if (r_a.from < r_b.from) {
			if (i_a == len_a) return MAX_USE_POS;
			r_a = a.ranges[i_a];
			++i_a;
		} else { // r_b.from > r_a.from
			if (i_b == len_b) return MAX_USE_POS;
			r_b = b.ranges[i_b];
			++i_b;
		}
	}
}

struct IntervalIndex
{
	this(size_t idx) { index = cast(uint)idx; }
	uint index = uint.max;
	alias index this;
	enum NULL = IntervalIndex(uint.max);
	bool isNull() { return index == uint.max; }
	void toString(scope void delegate(const(char)[]) sink) {
		import std.format : formattedWrite;
		if (isNull) sink("it_null");
		else sink.formattedWrite("it%s", index);
	}
}
