import std.stdio;
import std.algorithm;
import std.format : formattedWrite;

void main()
{
	test1;
	test2;
	test3;
}

void tester(ref Solver solver)
{
	solver.resolve2;
	//writeln("instrs");
	//foreach (instr; solver.instructions)
	//	writeln(instr);
	//writeln("values");
	//foreach (reg; solver.registers)
	//	writefln("%s <- %s", reg.loc, reg.currentValue);
	foreach(ref reg; solver.registers)
		assert(reg.currentValue == reg.initialReadFrom.loc, "err");
}

void test1()
{
	Solver solver;
	solver.registers.length = 5;
	solver.addMove2(regLoc(1), regLoc(0));
	solver.addMove2(regLoc(1), regLoc(2));
	solver.addMove2(regLoc(0), regLoc(3));
	solver.addMove2(regLoc(3), regLoc(4));
	solver.addMove2(regLoc(3), regLoc(1));
	tester(solver);
}

void test2()
{
	Solver solver;
	solver.registers.length = 4;
	solver.addMove2(regLoc(0), regLoc(1));
	solver.addMove2(regLoc(1), regLoc(2));
	solver.addMove2(regLoc(2), regLoc(3));
	tester(solver);
}

void test3()
{
	Solver solver;
	solver.registers.length = 6;
	solver.addMove2(regLoc(0), regLoc(1));
	solver.addMove2(regLoc(1), regLoc(2));
	solver.addMove2(regLoc(2), regLoc(0));
	solver.addMove2(regLoc(0), regLoc(3));
	solver.addMove2(regLoc(1), regLoc(4));
	solver.addMove2(regLoc(2), regLoc(5));
	tester(solver);
}

struct Solver
{
	// We need full set of all locations, so we can store info there in O(1) time
	ValueInfo[] constants;
	ValueInfo[] registers;
	ValueInfo[] stackSlots;
	ValueInfo*[] writtenNodes;
	uint numWriteOnlyValues;

	Instruction[] instructions;

	ref ValueInfo getInfo(ValueLocation loc) {
		final switch(loc.kind) {
			case ValueLocationKind.con:  return  constants[loc.index];
			case ValueLocationKind.reg:  return  registers[loc.index];
			case ValueLocationKind.stack: return stackSlots[loc.index];
		}
	}

	// assumes 1 or 0 write to each ValueLocation
	void addMove(ValueLocation fromLoc, ValueLocation toLoc)
	{
		ValueInfo* from = &getInfo(fromLoc);
		ValueInfo* to = &getInfo(toLoc);

		from.setup(fromLoc);
		to.setup(toLoc);

		from.onRead;
		to.onWrite(from);

		writtenNodes ~= to;
	}

	// O(n^2)
	void resolve()
	{
		size_t i;
		while (writtenNodes.length)
		{
			ValueInfo* to = writtenNodes[i];
			ValueInfo* from = to.readFrom;

			if (from is null)
			{
				// swapped node, skip
				removeInPlace(i);
			}
			else if (to.numReads == 0)
			{
				instructions ~= Instruction(Instruction.Type.move, to.loc, from.loc);
				writefln("move %s %s(%s)", to.loc, from.loc, from.currentValue);
				to.currentValue = from.currentValue;
				--from.numReads;
				writefln("%s (%s reads)", from.loc, from.numReads);
				removeInPlace(i);
			}
			else
			{
				if (from.readFrom !is null && from.numReads == 1 && from.readFrom.numReads == 1)
				{
					// to <-- from <-- from.readFrom
					// delete from and rewrite as:
					// to <-- from.readFrom

					writefln("swap %s(%s) %s(%s)", from.loc, from.currentValue, from.readFrom.loc, from.readFrom.currentValue);
					instructions ~= Instruction(Instruction.Type.swap, from.loc, from.readFrom.loc);
					swap(from.currentValue, from.readFrom.currentValue);

					if (from.readFrom == to)
					{
						// handle case when from.readFrom == to
						// to <-- from <-- to <-- from
						removeInPlace(i);
					}
					else
					{
						to.readFrom = from.readFrom;
						--from.numReads;
						writefln("%s (%s reads)", from.loc, from.numReads);
						++i;
					}

					// remove node from processing (we can't use removeInPlace, because we have no index)
					from.readFrom = null;
				}
				else
					++i;
			}

			//if (i >= writtenNodes.length) break;
			if (i >= writtenNodes.length) i = 0;
		}
	}

	// Invariant: all values with 0 reads are at the end of move array
	void addMove2(ValueLocation fromLoc, ValueLocation toLoc)
	{
		ValueInfo* from = &getInfo(fromLoc);
		ValueInfo* to = &getInfo(toLoc);

		from.setup(fromLoc);
		to.setup(toLoc);

		from.onRead;
		if (from.numReads == 1 && from.readFrom != null) {
			wo_to_rw(from.arrayPos);
		}

		to.onWrite(from);
		to.arrayPos = cast(uint)writtenNodes.length;
		writtenNodes ~= to;
		++numWriteOnlyValues;

		writefln("add move %s -> %s", fromLoc, toLoc);

		if (to.numReads > 0) {
			wo_to_rw(to.arrayPos);
		}
	}

	void wo_to_rw(uint arrayPos)
	{
		assert(numWriteOnlyValues > 0);
		size_t from = arrayPos;
		size_t to = writtenNodes.length - numWriteOnlyValues;
		if (from != to) {
			swap(writtenNodes[from], writtenNodes[to]);
			writtenNodes[from].arrayPos = cast(uint)from;
			writtenNodes[to].arrayPos = cast(uint)to;
		}
		--numWriteOnlyValues;
	}
	void rw_to_wo(uint arrayPos)
	{
		++numWriteOnlyValues;
		size_t from = arrayPos;
		size_t to = writtenNodes.length - numWriteOnlyValues;
		if (from != to) {
			swap(writtenNodes[from], writtenNodes[to]);
			writtenNodes[from].arrayPos = cast(uint)from;
			writtenNodes[to].arrayPos = cast(uint)to;
		}
	}

	void removeInPlace(size_t index)
	{
		if (index+1 != writtenNodes.length)
		{
			writtenNodes[index] = writtenNodes[$-1];
			writtenNodes[index].arrayPos = cast(uint)index;
		}
		--writtenNodes.length;
	}

	void removeItem(ValueInfo* item)
	{
		size_t from = writtenNodes.length-1;
		size_t to = item.arrayPos;
		if (from != to) {
			writtenNodes[to] = writtenNodes[from];
			writtenNodes[to].arrayPos = cast(uint)to;
		}
		--writtenNodes.length;
	}

	// we process items from last to first. Starting from items with zero reads
	// as we process unread items, read items can become unread
	// in the end either all items are processed, or read items that left are in a read cycle(s)
	// we perform O(n) iterations
	void resolve2()
	{
		writefln("--- resolve ---");
		while (writtenNodes.length)
		{
			ValueInfo* to = writtenNodes[$-1];
			ValueInfo* from = to.readFrom;

			if (to.numReads > 0)
			{
				// process cycled items
				// to <-- from <-- from.readFrom
				// delete from and rewrite as:
				// to <-- from.readFrom

				writefln("insert swap %s(%s) %s(%s)", from.loc, from.currentValue, from.readFrom.loc, from.readFrom.currentValue);
				instructions ~= Instruction(Instruction.Type.swap, from.loc, from.readFrom.loc);
				swap(from.currentValue, from.readFrom.currentValue);

				if (from.readFrom == to)
				{
					// handle case when from.readFrom == to
					// ... to <-- from <-- to ...
					removeItem(to);
				}
				else
				{
					to.readFrom = from.readFrom;
					--from.numReads;
					writefln("%s (%s reads)", from.loc, from.numReads);
				}
				removeItem(from);
			}
			else
			{
				// WO item
				instructions ~= Instruction(Instruction.Type.move, to.loc, from.loc);
				writefln("insert move %s <- %s(%s)", to.loc, from.loc, from.currentValue);
				to.currentValue = from.currentValue;
				--from.numReads;
				--numWriteOnlyValues;
				removeItem(to);

				if (from.numReads == 0 && from.readFrom != null)
					rw_to_wo(from.arrayPos);
			}
		}
	}
}

struct Instruction
{
	enum Type
	{
		move,
		swap
	}
	Type type;
	ValueLocation arg0, arg1;
	void toString(scope void delegate(const(char)[]) sink) const {
		sink.formattedWrite("%s %s, %s", type, arg0, arg1);
	}
}

struct ValueInfo
{
	uint numReads;
	uint arrayPos; // index into writtenNodes
	ValueLocation loc;
	ValueInfo* readFrom; // can be null
	ValueInfo* initialReadFrom; // can be null, for debug. An immutable copy of readFrom
	ValueLocation currentValue; // for debug, show what is stored here now

	void setup(ValueLocation self)
	{
		loc = self;
		currentValue = self;
	}

	void onRead()
	{
		++numReads;
		writefln("read %s (%s reads)", loc, numReads);
	}

	void onWrite(ValueInfo* from)
	{
		assert(readFrom is null, "Second write detected");
		readFrom = from;
		initialReadFrom = from;
	}
}

ValueLocation regLoc(uint index) { return ValueLocation(index, ValueLocationKind.reg); }
ValueLocation conLoc(uint index) { return ValueLocation(index, ValueLocationKind.con); }
ValueLocation memLoc(uint index) { return ValueLocation(index, ValueLocationKind.stack); }

struct ValueLocation
{
	uint index = uint.max;
	ValueLocationKind kind;
	bool isNull() { return index == uint.max; }
	void toString(scope void delegate(const(char)[]) sink) const {
		sink.formattedWrite("%s.%s", kind, index);
	}
}

enum ValueLocationKind : ubyte
{
	con,
	reg,
	stack
}
