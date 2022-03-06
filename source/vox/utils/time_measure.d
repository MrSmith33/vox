/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.utils.time_measure;

import std.stdio : writeln, write, writef, writefln;
import vox.all;

struct PerPassTimeMeasurements
{
	TimeMeasurements totalTimes;
	TimeMeasurements[] passTimes;
	CompilePassGlobal[] passes;
	size_t numPasses; // number of passes in the tree of passes

	this(size_t numIters, CompilePassGlobal[] passes)
	{
		this.passes = passes;
		totalTimes = TimeMeasurements(numIters);
		foreach (passIndex, string name, Duration dur; PassMetaIterator(passes)) ++numPasses;
		passTimes = new TimeMeasurements[numPasses];
		foreach (ref times; passTimes) times = TimeMeasurements(numIters);
	}

	void onIteration(size_t iterIndex, Duration iterTime)
	{
		totalTimes.onIteration(iterIndex, iterTime);
		foreach (passIndex, string name, Duration dur; PassMetaIterator(passes))
			passTimes[passIndex].onIteration(iterIndex, dur);
	}

	void print()
	{
		void printRow(string name, ref TimeMeasurements times)
		{
			writef("%- 20s", name);
			times.print;
			writeln;
		}

		writef("Iterations % 5.0s    ", scaledNumberFmt(totalTimes.numIters));
		totalTimes.printHeader; writeln;
		foreach (passIndex, string name, Duration dur; PassMetaIterator(passes))
			if (name)
				printRow(name, passTimes[passIndex]);
		printRow("Total", totalTimes);
	}

	void printTsv()
	{
		void printRow(string name, ref TimeMeasurements times)
		{
			writef("%s", name);
			times.printTsv;
			writeln;
		}

		writef("Iterations %s", scaledNumberFmt(totalTimes.numIters));
		totalTimes.printHeaderTsv; writeln;
		foreach (passIndex, string name, Duration dur; PassMetaIterator(passes))
			if (name)
				printRow(name, passTimes[passIndex]);
		printRow("Total", totalTimes);
	}
}

struct TimeMeasurements
{
	size_t numIters;
	Duration[] iterTimes;
	Duration totalTime;
	Duration avgTime() { return totalTime/numIters; }
	Duration minTime = Duration.max;
	Duration maxTime = Duration.min;

	this(size_t numIters)
	{
		this.numIters = numIters;
		iterTimes = new Duration[numIters];
	}

	void onIteration(size_t iterIndex, Duration iterTime)
	{
		iterTimes[iterIndex] = iterTime;
		totalTime += iterTime;
		minTime = min(iterTime, minTime);
		maxTime = max(iterTime, maxTime);
	}

	enum showNumFirstIters = 3;

	void printHeader()
	{
		if (numIters == 1) {
			write("    time");
		} else {
			foreach (i; 0..min(numIters, showNumFirstIters))
				writef("  iter %s", i);
			write("   total     avg     min     max");
		}
	}

	void print()
	{
		if (numIters == 1) {
			writef(" % 6.2ss", scaledNumberFmt(iterTimes[0]));
		} else {
			foreach (i; 0..min(numIters, showNumFirstIters))
				writef(" % 6.2ss", scaledNumberFmt(iterTimes[i]));
			writef(" % 6.2ss", scaledNumberFmt(totalTime));
			writef(" % 6.2ss", scaledNumberFmt(avgTime));
			writef(" % 6.2ss", scaledNumberFmt(minTime));
			writef(" % 6.2ss", scaledNumberFmt(maxTime));
		}
	}

	void printHeaderTsv()
	{
		if (numIters == 1) {
			write("\ttime");
		} else {
			foreach (i; 0..min(numIters, showNumFirstIters))
				writef("\titer %s", i);
			write("\ttotal\tavg\tmin\tmax");
		}
	}

	void printTsv()
	{
		if (numIters == 1) {
			writef("\t%ss", scaledNumberFmt(iterTimes[0]));
		} else {
			foreach (i; 0..min(numIters, showNumFirstIters))
				writef("\t%#ss", scaledNumberFmt(iterTimes[i]));
			writef("\t%#ss", scaledNumberFmt(totalTime));
			writef("\t%#ss", scaledNumberFmt(avgTime));
			writef("\t%#ss", scaledNumberFmt(minTime));
			writef("\t%#ss", scaledNumberFmt(maxTime));
		}
	}
}
