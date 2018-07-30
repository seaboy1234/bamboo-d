module bamboo.codegen.helpers;

import bamboo.codegen;

mixin template ParentConstructors()
{
    static foreach(ctor; __traits(getOverloads, typeof(super), "__ctor", true))
    {
        this(Parameters!ctor args)
        {
            super(args);
        }
    }
}

string generateDefinition(Parameter parameter)
{
    switch (parameter.syntaxKind)
    {
    case SyntaxKind.StructParameter:
        return generateDefinition(cast(StructParameter) parameter);
    case SyntaxKind.ArrayParameter:
        return generateDefinition(cast(ArrayParameter) parameter);
    case SyntaxKind.SizedParameter:
        return generateDefinition(cast(SizedParameter) parameter);
    case SyntaxKind.NumericParameter:
        return generateDefinition(cast(NumericParameter) parameter);
    default:
        assert(0);
    }
}

string generateDefinition(StructParameter parameter)
{
    enum string format = `${type} ${name}`;

    string type = mapType(parameter.type.symbol);
    string name = generateName(parameter.symbol, type, 1);

    parameter.symbol = name;

    return mixin(interp!format);
}

string generateDefinition(ArrayParameter array)
{
    enum string format = `${type}[${maxLength}] ${name}`;

    string type = mapType(array.elementType.symbol);
    string name = generateName(array.symbol, type, 2);
    string maxLength = "";

    version (RespectFixedLength)
    {
        if (array.hasRange && array.size.isFixedLength)
        {
            maxLength = array.size.maxLength.to!string;
        }
    }

    array.symbol = name;

    return mixin(interp!format);
}

string generateDefinition(SizedParameter array)
{
    enum string format = `${type} ${name}`;

    string type;
    string maxLength = "";
    string defaultVal = "";

    version (RespectFixedLength)
    {
        if (array.hasRange && array.size.isFixedLength)
        {
            maxLength = array.size.maxLength.to!string;
        }

        switch (array.parameterType) with (Type)
        {
        case string_:
        case varstring:
            type = "immutable char[${maxLength}]";
            break;
        case blob:
        case varblob:
            type = "immutable ubyte[${maxLength}]";
            break;
        default:
            assert(0);
        }
        type = mixin(interp!type);
    }
    else
    {
        switch (array.parameterType) with (Type)
        {
        case string_:
        case varstring:
            type = "string";
            break;
        case blob:
        case varblob:
            type = "blob";
            break;
        default:
            assert(0);
        }
    }

    type = mapType(type);

    string name = generateName(array.symbol, type, 3);

    if (array.defaultVal.length > 0)
    {
        defaultVal = mixin(interp!" = \"${array.defaultVal}\"");
    }

    array.symbol = name;

    return mixin(interp!format);
}

string generateDefinition(NumericParameter numeric)
{
    enum string format = `${type} ${name}`;

    string type = mapType(numeric.type.to!string());
    string name = generateName(numeric.symbol, type, 4);

    numeric.symbol = name;

    return mixin(interp!format);
}

string mapType(string type)
{
    // dfmt off
    enum types = [
            "int8": "byte", 
            "int16": "short",
            "int32": "int", 
            "int64": "long", 
            "uint8": "ubyte", 
            "uint16": "ushort", 
            "uint32": "uint", 
            "uint64": "ulong", 
            "float32": "float", 
            "float64": "double", 
            "char_": "char",
            "varstring": "string", 
            "blob": "ubyte[]", 
            "varblob": "ubyte[]", 
        ];
    // dfmt on

    if (auto ret = type in types)
    {
        return *ret;
    }
    return type;
}

string generateContractFor(Parameter parameter)
{
    if (auto array = cast(SizedParameter) parameter)
    {
        return generateContract(array);
    }
    else if (auto array = cast(ArrayParameter) parameter)
    {
        return generateContract(array);
    }
    else if (auto numeric = cast(NumericParameter) parameter)
    {
        return generateContract(numeric);
    }
    return "";
}

string generateContract(SizedParameter array)
{
    enum string format = `
    assert(${parameter}.length <= ${maxLength}, "${parameter} is oversized!");
    assert(${parameter}.length >= ${minLength}, "${parameter} is undersized!");
    `;

    if (!array.hasRange)
    {
        return "";
    }

    string parameter = array.symbol;
    string maxLength = array.size.maxSize.to!string;
    string minLength = array.size.minSize.to!string;

    return mixin(interp!format);
}

string generateContract(ArrayParameter array)
{
    enum string format = `
    assert(${parameter}.length <= ${maxLength}, "${parameter} is oversized!");
    assert(${parameter}.length >= ${minLength}, "${parameter} is undersized!");
    `;

    if (!array.hasRange)
    {
        return "";
    }

    string parameter = array.symbol;
    string maxLength = array.size.maxLength.to!string;
    string minLength = array.size.minLength.to!string;

    return mixin(interp!format);
}

string generateContract(NumericParameter numeric)
{
    string formatStr(string parameter, string op, string value, string msg)
    {
        return `assert(` ~ parameter ~ ` ` ~ op ~ ` ` ~ value ~ `, "` ~ parameter
            ~ ` is too ` ~ msg ~ `!");`;
    }

    string format;

    if (!numeric.hasRange)
    {
        return "";
    }

    string parameter = numeric.symbol;
    string max = numeric.range.max.to!string;
    string min;

    format ~= formatStr(parameter, "<=", max, "large");

    if (!isNaN(numeric.range.min))
    {
        min = numeric.range.min.to!string;
        format ~= formatStr(parameter, ">=", min, "small");
    }

    return format;
}

string generateName(string name, string type, int counter)
{
    if (name.length == 0)
    {
        name = mixin(interp!"_${type.toLower}${counter}");
    }
    return name;
}

