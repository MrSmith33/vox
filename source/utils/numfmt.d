/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module utils.numfmt;

import utils : Duration;

///
struct ScaledNumberFmt(T)
{
	import utils : min;
	import std.format : formattedWrite, FormatSpec;
	T value;
	void toString(scope void delegate(const(char)[]) sink, const ref FormatSpec!char fmt)
	{
		int scale = calcScale(value);
		auto scaledValue = scaled(value, -scale);
		int digits = numDigitsInNumber(scaledValue);
		string suffix = scaleSuffixes[scaleToScaleIndex(scale)]; // length is 1 or 0
		int width = fmt.width - cast(int)suffix.length;
		int precision = min(3-digits, fmt.precision); // gives 0 or 1
		sink.formattedWrite("%*.*f%s", width, precision, scaledValue, suffix);
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
immutable string[] scaleSuffixes = ["q","r","y","z","a","f","p","n","u","m","","K","M","G","T","P","E","Z","Y","R","Q"];
enum NUM_SCALE_SUFFIXES = 10;
enum MIN_SCALE_SUFFIX = -30;
enum MAX_SCALE_SUFFIX = 30;


int numDigitsInNumber(Num)(const Num val)
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

int calcScale(Num)(Num val)
{
	import std.algorithm: clamp;
	import std.math: abs, floor, ceil, log10;
	static int signum(T)(const T x) pure nothrow
	{
		return (x > 0) - (x < 0);
	}

	auto lg = log10(abs(val));
	int logSign = signum(lg);
	double absLog = abs(lg);

	int scale;
	if (lg < 0)
		scale = cast(int)(ceil(absLog/3.0))*3;
	else
		scale = cast(int)(floor(absLog/3.0))*3;

	int clampedScale = scale * logSign;
	if (clampedScale < MIN_SCALE_SUFFIX)
		clampedScale = 0; // prevent zero, or values smaller that min scale to display with min scale
	else if (clampedScale > MAX_SCALE_SUFFIX)
		clampedScale = MAX_SCALE_SUFFIX;

	return clampedScale;
}

int scaleToScaleIndex(int scale)
{
	return scale / 3 + NUM_SCALE_SUFFIXES; // -30...30 -> -10...10 -> 0...20
}

double scaled(Num)(Num num, int scale)
{
	import std.math: pow;
	return num * pow(10.0, scale);
}
