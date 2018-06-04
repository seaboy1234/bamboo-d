module bamboo.util;

import bamboo.types;

/// Generates a hashmap of builtin types.
Type[string] genbuiltins()
{
    import std.algorithm : map;
    import std.conv : to;
    import std.traits : EnumMembers;
    import std.typecons : tuple;

    Type[string] types;

    enum arr = [EnumMembers!Type].map!(x => tuple(x, x.to!string));

    foreach (item; arr)
    {
        types[item[1]] = item[0];
    }

    return types;
}

/++
Interpolated string (ie, variable expansion).
Any D expression can be placed inside ${ and }. Everything between the curly
braces will be evaluated inside your current scope, and passed as a parameter
(or parameters) to std.conv.text.
The curly braces do NOT nest, so variable expansion will end at the first
closing brace. If the closing brace is missing, an Exception will be thrown
at compile-time.
Author: https://github.com/Abscissa
License: zlib/libpng
Example:
------------
// Output: The number 21 doubled is 42!
int num = 21;
writeln( mixin(interp!"The number ${num} doubled is ${num * 2}!") );
// Output: Empty braces output nothing.
writeln( mixin(interp!"Empty ${}braces ${}output nothing.") );
// Output: Multiple params: John Doe.
auto first = "John", last = "Doe";
writeln( mixin(interp!`Multiple params: ${first, " ", last}.`) );
------------
+/
string interp(string str)()
{
	enum State
	{
		normal,
		dollar,
		code,
	}

	auto state = State.normal;

	string buf;
	buf ~= '`';

	foreach(char c; str)
	final switch(state)
	{
	case State.normal:
		if(c == '$')
			// Delay copying the $ until we find out whether it's
			// the start of an escape sequence.
			state = State.dollar;
		else if(c == '`')
			buf ~= "`~\"`\"~`";
		else
			buf ~= c;
		break;

	case State.dollar:
		if(c == '{')
		{
			state = State.code;
			buf ~= "`~_interp_text(";
		}
		else if(c == '$')
			buf ~= '$'; // Copy the previous $
		else
		{
			buf ~= '$'; // Copy the previous $
			buf ~= c;
			state = State.normal;
		}
		break;

	case State.code:
		if(c == '}')
		{
			buf ~= ")~`";
			state = State.normal;
		}
		else
			buf ~= c;
		break;
	}
	
	// Finish up
	final switch(state)
	{
	case State.normal:
		buf ~= '`';
		break;

	case State.dollar:
		buf ~= "$`"; // Copy the previous $
		break;

	case State.code:
		throw new Exception(
			"Interpolated string contains an unterminated expansion. "~
			"You're missing a closing curly brace."
		);
	}

	return buf;
}
string _interp_text(T...)(T args)
{
    static import std.conv;
	static if(T.length == 0)
		return null;
	else
		return std.conv.text(args);
}
