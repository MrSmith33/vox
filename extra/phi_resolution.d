import std.stdio;
import std.algorithm;
import std.format : formattedWrite;
void main()
{
	Solver solver;
	solver.registers.length = 5;
	solver.addMove(regLoc(1), regLoc(0));
	solver.addMove(regLoc(1), regLoc(2));
	solver.addMove(regLoc(0), regLoc(3));
	solver.addMove(regLoc(3), regLoc(4));
	solver.addMove(regLoc(3), regLoc(1));
	solver.resolve;
	writeln("instrs");
	foreach (instr; solver.instructions)
		writeln(instr);
	writeln("values");
	foreach (reg; solver.registers)
		writefln("%s <- %s", reg.info.loc, reg.info.currentValue);
}

struct Solver
{
	// We need full set of all locations, so we can store info there in O(1) time
	Constant[] constants;
	Register[] registers;
	StackSlot[] stackSlots;
	//MoveData[] moves;
	ValueInfo*[] writtenNodes;

	Instruction[] instructions;

	ref ValueInfo getInfo(ValueLocation loc) {
		final switch(loc.kind) {
			case ValueLocationKind.constant:  return  constants[loc.index].info;
			case ValueLocationKind.register:  return  registers[loc.index].info;
			case ValueLocationKind.stackSlot: return stackSlots[loc.index].info;
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
				removeInPlace(writtenNodes, i);
			}
			else if (to.numReads == 0)
			{
				instructions ~= Instruction(Instruction.Type.move, to.loc, from.loc);
				writefln("move %s %s(%s)", to.loc, from.loc, from.currentValue);
				to.currentValue = from.currentValue;
				--from.numReads;
				writefln("%s (%s reads)", from.loc, from.numReads);
				removeInPlace(writtenNodes, i);
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
						removeInPlace(writtenNodes, i);
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
	ValueLocation loc;
	ValueInfo* readFrom; // can be null
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
	}
}

struct MoveData
{
	ValueLocation from;
	ValueLocation to;
}

ValueLocation regLoc(uint index) { return ValueLocation(index, ValueLocationKind.register); }
ValueLocation conLoc(uint index) { return ValueLocation(index, ValueLocationKind.constant); }
ValueLocation memLoc(uint index) { return ValueLocation(index, ValueLocationKind.stackSlot); }

struct ValueLocation
{
	uint index = uint.max;
	ValueLocationKind kind;
	bool isNull() { return index == uint.max; }
	void toString(scope void delegate(const(char)[]) sink) const {
		sink.formattedWrite("%s.%s", kind, index);
	}
}

struct Constant  { ValueInfo info; }
struct Register  { ValueInfo info; }
struct StackSlot { ValueInfo info; }

enum ValueLocationKind : ubyte
{
	constant,
	register,
	stackSlot
}

void removeInPlace(T)(ref T[] array, size_t index)
{
	if (index+1 != array.length)
	{
		array[index] = array[$-1];
	}
	--array.length;
}
