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

