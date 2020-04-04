/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module utils.numfmt;

import core.time : Duration;

/// Use 'i' format char to get binary prefixes (like Ki, instead of K), only for integers
/// Use '#' flag to get greek letter in the output (not compatible with 'i')
struct ScaledNumberFmt(T)
{
	import std.algorithm : min, max;
	import std.format : formattedWrite, FormatSpec;
	T value;
	void toString(scope void delegate(const(char)[]) sink, const ref FormatSpec!char fmt)
	{
		if (fmt.spec == 'i') {
			// Use binary prefixes instead of decimal prefixes
			long intVal = cast(long)value;
			int scale = calcScale2(intVal);
			double scaledValue = scaled2(value, scale);
			int digits = numDigitsInNumber10(scaledValue);
			string prefix = scalePrefixesAscii[scaleToScaleIndex2(scale)]; // length is 1 or 0
			int width = max(fmt.width - (cast(int)prefix.length * 2), 0); // account for 'i' prefix
			int precision = max(min(3-digits, fmt.precision), 0); // gives 0 or 1
			string fmtString = (scale == 0) ? "%*.*f%s" : "%*.*f%si";
			sink.formattedWrite(fmtString, width, precision, scaledValue, prefix);
		} else {
			int scale = calcScale10(value);
			auto scaledValue = scaled10(value, -scale);
			int digits = numDigitsInNumber10(scaledValue);
			immutable string[] prefixes = (fmt.flHash) ? scalePrefixesGreek : scalePrefixesAscii;
			string prefix = prefixes[scaleToScaleIndex10(scale)]; // length is 1 or 0
			int width = max(fmt.width - cast(int)prefix.length, 0);
			int precision = max(min(3-digits, fmt.precision), 0); // gives 0 or 1
			sink.formattedWrite("%*.*f%s", width, precision, scaledValue, prefix);
		}
	}
}

auto scaledNumberFmt(T)(T value)
{
	return ScaledNumberFmt!T(value);
}

auto scaledNumberFmt(Duration value, double scale = 1)
{
	double seconds = value.total!"nsecs" / 1_000_000_000.0;
	return ScaledNumberFmt!double(seconds * scale);
}

// -30 .. 30, with step of 3. Or -10 to 10 with step of 1
immutable string[] scalePrefixesAscii = ["q","r","y","z","a","f","p","n","u","m","","K","M","G","T","P","E","Z","Y","R","Q"];
immutable string[] scalePrefixesGreek = ["q","r","y","z","a","f","p","n","µ","m","","K","M","G","T","P","E","Z","Y","R","Q"];
enum NUM_SCALE_PREFIXES = 10;
enum MIN_SCALE_PREFIX = -30;
enum MAX_SCALE_PREFIX = 30;


int numDigitsInNumber10(Num)(const Num val)
{
	import std.math: abs, round;
	ulong absVal = cast(ulong)val.abs.round;
	int numDigits = 1;

	while (absVal >= 10)
	{
		absVal /= 10;
		++numDigits;
	}

	return numDigits;
}

private int signum(T)(const T x) pure nothrow
{
	return (x > 0) - (x < 0);
}

/// Returns number in range of [-30; 30]
int calcScale10(Num)(Num val)
{
	import std.algorithm: clamp;
	import std.math: abs, round, log10;

	// cast to double is necessary in case of long.min, which overflows integral abs
	auto lg = log10(abs(cast(double)val));

	// handle very small values and zero
	if (lg == -double.infinity) return 0;

	double absLog = abs(lg);
	int scale = cast(int)(round(absLog/3.0))*3;

	int logSign = signum(lg);
	int clampedScale = scale * logSign;

	// we want
	//  0.9994 to be formatted as 999m
	//  0.9995 to be formatted as 1.0
	//  0.9996 to be formatted as 1.0
	if (abs(scaled10(val, -clampedScale)) < 0.9995) clampedScale -= 3;

	if (clampedScale < MIN_SCALE_PREFIX)
		clampedScale = 0; // prevent zero, or values smaller that min scale to display with min scale
	else if (clampedScale > MAX_SCALE_PREFIX)
		clampedScale = MAX_SCALE_PREFIX;

	return clampedScale;
}

/// Returns number in range of [0; 100]
int calcScale2(Num)(Num val)
{
	import std.algorithm: clamp;
	import std.math: abs, round, log2;

	auto lg = log2(abs(val));
	double absLog = abs(lg);

	int scale = cast(int)(round(absLog/10.0))*10;

	int logSign = signum(lg);
	int clampedScale = scale * logSign;

	// we want
	//  0.9994 to be formatted as 999m
	//  0.9995 to be formatted as 1.0
	//  0.9996 to be formatted as 1.0
	if (abs(scaled2(val, clampedScale)) < 0.9995) clampedScale -= 10;

	if (clampedScale < 0)
		clampedScale = 0; // negative scale should not happen for binary numbers
	else if (clampedScale > MAX_SCALE_PREFIX)
		clampedScale = MAX_SCALE_PREFIX;

	return clampedScale;
}

int scaleToScaleIndex10(int scale) {
	return scale / 3 + NUM_SCALE_PREFIXES; // -30...30 -> -10...10 -> 0...20
}

int scaleToScaleIndex2(int scale) {
	return scale / 10 + NUM_SCALE_PREFIXES; // -100...100 -> -10...10 -> 0...20
}

double scaled10(Num)(Num num, int scale)
{
	import std.math: pow;
	return num * pow(10.0, scale);
}

double scaled2(Num)(Num num, int scale)
{
	double divisor = 1 << scale;
	return num / divisor;
}

// Criteria:
// Should not produce `0.xx`
// must be `xxxm` instead
unittest
{
	void test(T)(T num, string expected, string file = __MODULE__, int line = __LINE__) {
		import std.format;
		string res = format("%s", scaledNumberFmt(num));
		assert(res == expected,
			format("%s:%s %s != %s", file, line, res, expected));
	}

	test(-10_000_000_000_000_000_000_000_000_000_000_000.0, "-10000Q");
	test(-1_000_000_000_000_000_000_000_000_000_000_000.0, "-1000Q");
	test(-100_000_000_000_000_000_000_000_000_000_000.0, "-100Q");
	test(-10_000_000_000_000_000_000_000_000_000_000.0, "-10.0Q");
	test(-1_000_000_000_000_000_000_000_000_000_000.0, "-1.00Q");
	test(-100_000_000_000_000_000_000_000_000_000.0, "-100R");
	test(-10_000_000_000_000_000_000_000_000_000.0, "-10.0R");
	test(-1_000_000_000_000_000_000_000_000_000.0, "-1.00R");
	test(-100_000_000_000_000_000_000_000_000.0, "-100Y");
	test(-10_000_000_000_000_000_000_000_000.0, "-10.0Y");
	test(-1_000_000_000_000_000_000_000_000.0, "-1.00Y");
	test(-100_000_000_000_000_000_000_000.0, "-100Z");
	test(-10_000_000_000_000_000_000_000.0, "-10.0Z");
	test(-1_000_000_000_000_000_000_000.0, "-1.00Z");
	test(-100_000_000_000_000_000_000.0, "-100E");
	test(-10_000_000_000_000_000_000.0, "-10.0E");

	test(long.min, "-9.22E");
	test(-9_223_372_036_854_775_807, "-9.22E");
	test(-1_000_000_000_000_000_000, "-1.00E");
	test(-100_000_000_000_000_000, "-100P");
	test(-10_000_000_000_000_000, "-10.0P");
	test(-1_000_000_000_000_000, "-1.00P");
	test(-100_000_000_000_000, "-100T");
	test(-10_000_000_000_000, "-10.0T");
	test(-1_000_000_000_000, "-1.00T");
	test(-100_000_000_000, "-100G");
	test(-10_000_000_000, "-10.0G");
	test(-1_000_000_000, "-1.00G");
	test(-100_000_000, "-100M");
	test(-10_000_000, "-10.0M");
	test(-1_000_000, "-1.00M");
	test(-100_000, "-100K");
	test(-10_000, "-10.0K");
	test(-1_000, "-1.00K");
	test(-100, "-100");
	test(-10, "-10.0");
	test(-1, "-1.00");
	test(0, "0.00");
	test(1, "1.00");
	test(10, "10.0");
	test(100, "100");
	test(1_000, "1.00K");
	test(10_000, "10.0K");
	test(100_000, "100K");
	test(1_000_000, "1.00M");
	test(10_000_000, "10.0M");
	test(100_000_000, "100M");
	test(1_000_000_000, "1.00G");
	test(10_000_000_000, "10.0G");
	test(100_000_000_000, "100G");
	test(1_000_000_000_000, "1.00T");
	test(10_000_000_000_000, "10.0T");
	test(100_000_000_000_000, "100T");
	test(1_000_000_000_000_000, "1.00P");
	test(10_000_000_000_000_000, "10.0P");
	test(100_000_000_000_000_000, "100P");
	test(1_000_000_000_000_000_000, "1.00E");
	test(ulong.max, "18.4E");

	test(10_000_000_000_000_000_000.0, "10.0E");
	test(100_000_000_000_000_000_000.0, "100E");
	test(1_000_000_000_000_000_000_000.0, "1.00Z");
	test(10_000_000_000_000_000_000_000.0, "10.0Z");
	test(100_000_000_000_000_000_000_000.0, "100Z");
	test(1_000_000_000_000_000_000_000_000.0, "1.00Y");
	test(10_000_000_000_000_000_000_000_000.0, "10.0Y");
	test(100_000_000_000_000_000_000_000_000.0, "100Y");
	test(1_000_000_000_000_000_000_000_000_000.0, "1.00R");
	test(10_000_000_000_000_000_000_000_000_000.0, "10.0R");
	test(100_000_000_000_000_000_000_000_000_000.0, "100R");
	test(1_000_000_000_000_000_000_000_000_000_000.0, "1.00Q");
	test(10_000_000_000_000_000_000_000_000_000_000.0, "10.0Q");
	test(100_000_000_000_000_000_000_000_000_000_000.0, "100Q");
	test(1_000_000_000_000_000_000_000_000_000_000_000.0, "1000Q");
	test(10_000_000_000_000_000_000_000_000_000_000_000.0, "10000Q");

	// numbers less than 1.0 or close to 1
	test(1.234, "1.23");
	test(1.000, "1.00");
	test(0.9994, "999m");
	test(0.9995, "1.00");
	test(0.9996, "1.00");
	test(0.1234, "123m");
	test(0.01234, "12.3m");
	test(0.001234, "1.23m");
	test(0.0001234, "123u");

	test(-1.234, "-1.23");
	test(-1.000, "-1.00");
	test(-0.9994, "-999m");
	test(-0.9995, "-1.00");
	test(-0.9996, "-1.00");
	test(-0.1234, "-123m");
	test(-0.01234, "-12.3m");
	test(-0.001234, "-1.23m");
	test(-0.0001234, "-123u");
}
