/// Copyright: Copyright (c) 2018-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module be.reg_alloc.live_range;

import all;

/// exclusive end doesn't prevent register from being used by another interval
/// ranges within one interval never have ranges[i].to == ranges[i+1].from
/// When split prev range's end get's offset by 1 so [0; 10) becomes [0; 3) [4; 10)
/// When looking if instruction is covered by range the end position is inclusive
/// Splits must always occur on odd positions
/// We forbid creation of [10; 10) ranges
/// invariant: from < to
/// [from; to)
struct LiveRange
{
	uint from;
	uint to;

	bool contains(uint pos) {
		if (pos < from) return false;
		if (pos >= to) return false;
		return true;
	}
	void merge(LiveRange other) {
		from = min(from, other.from);
		to = max(to, other.to);
	}
	bool canBeMergedWith(const LiveRange other) {
		if (to + 2 < other.from) return false;
		if (from > other.to + 2) return false;
		return true;
	}

	bool intersectsWith(const LiveRange other) {
		if (to <= other.from) return false;
		if (from >= other.to) return false;
		return true;
	}

	void toString(scope void delegate(const(char)[]) sink) {
		import std.format : formattedWrite;
		sink.formattedWrite("[%s; %s)", from, to);
	}
}

struct LiveRangeIndex {
	this(size_t id) { this.id = cast(uint)id; }
	enum LiveRangeIndex NULL = LiveRangeIndex(uint.max);
	uint id = uint.max;
	bool isNull() { return id == NULL; }
	alias id this;

	void toString(scope void delegate(const(char)[]) sink) {
		import std.format : formattedWrite;
		if (id == uint.max) sink("max");
		else sink.formattedWrite("%s", id);
	}
}
