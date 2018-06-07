module bamboo.codegen;

import std.algorithm;
import std.array;
import std.conv;
import std.math;
import std.string;
import std.uni;

import bamboo.hashgen;
import bamboo.types;
import bamboo.util;

/// Aliases for dclass int type names.
enum intTypes = `
alias int8  = byte;
alias int16 = short;
alias int32 = int;
alias int64 = long;

alias uint8  = ubyte;
alias uint16 = ushort;
alias uint32 = uint;
alias uint64 = long;
`;

/// Aliases for dclass float type names.
enum floatTypes = `
alias float32 = float;
alias float64 = double;
`;

// NB: string and char are properly aliased in D.

/// Alias for the dclass `blob` type.
enum blobTypes = `
alias blob = immutable ubyte[];
`;

/// Handy mixin for all dclass primitives.
enum Primitives = `
mixin(intTypes);
mixin(floatTypes);
mixin(blobTypes);
`;

/// Provides metadata for D introspection on dclass types.
struct TypeId
{
    int id;
    string name;
}

/// Provides metadata for D introspection on dclass fields.
struct FieldId
{
    int id;
}

/// Provides metadata for D introspection on dclass fields.
struct FieldType(T)
{
    T typeInit = T.init;
}

/// Generates D source code from a given module.  It will optionally declare a D module.
/// This utility function also imports codegen helpers and privately aliases dclass primitives.
/// This function is suitable for creating a self-contained file.
/// See_Also: $(D generateModule).
string generateFile(Module file, string moduleName = "",
        string distributedObjectModule = "libastrond", bool generateStubs = true)
{
    string format;
    format ~= "// This code was generated by a tool.\n";
    if (moduleName.length > 0)
    {
        format ~= "module ";
        format ~= moduleName;
        format ~= ";";
    }
    format ~= "import std.exception;";
    format ~= "import bamboo.codegen;";
    format ~= "import " ~ distributedObjectModule ~ " : DistributedObject;";
    format ~= "private { mixin(Primitives); }";
    format ~= generateModule(file, generateStubs);

    return format;
}

/// Generates D source code from a given module. This function is 
/// suitable for mixing into a larger D source file.
/// See_Also: $(D generateFile).
string generateModule(Module file, bool generateStubs = false)
{
    string format;

    foreach (type; file.typesById)
    {
        if (auto cls = cast(ClassDeclaration) type)
        {
            format ~= generateClass(cls, generateStubs);
        }
        else if (auto strct = cast(StructDeclaration) type)
        {
            format ~= generateStruct(strct);
        }
    }

    return format;
}

private:

string generateClass(ClassDeclaration cls, bool generateStubs)
{
    string format;
    format ~= "@TypeId(" ~ cls.id.to!string ~ ", `" ~ cls.symbol ~ "`) ";

    if (generateStubs)
    {
        format ~= "abstract ";
    }

    format ~= "class ";
    format ~= cls.symbol;

    format ~= " : ";
    if (cls.hasSuperclass)
    {
        format ~= cls.parents[0].symbol;
    }
    else
    {
        format ~= "DistributedObject";
    }

    format ~= "{";

    if (cls.hasConstructor)
    {
        assert(0, "Constructors are not supported.");
    }

    foreach (field; cls.fields)
    {
        format ~= generateField(field, generateStubs);
    }

    format ~= "}";
    return format;
}

string generateStruct(StructDeclaration strct)
{
    string format;
    format ~= "@TypeId(" ~ strct.id.to!string ~ ", `" ~ strct.symbol ~ "`)";
    format ~= "struct ";
    format ~= strct.symbol;
    format ~= " {";
    foreach (field; strct.parameters)
    {
        format ~= generateParameterField(field, true);
    }
    format ~= "}";
    return format;
}

string generateField(FieldDeclaration field, bool generateStubs)
{
    string format;
    if (auto molecular = cast(MolecularField) field)
    {
        format ~= generateMolecular(molecular);
    }
    else if (auto atomic = cast(AtomicField) field)
    {
        format ~= generateAtomic(atomic, generateStubs);
    }
    else if (auto parameter = cast(ParameterField) field)
    {
        format ~= generateParameterField(parameter, generateStubs);
    }
    else
    {
        assert(0);
    }
    return format;
}

string generateMolecular(MolecularField field)
{
    string format;
    format ~= "@FieldId(" ~ field.id.to!string ~ ")";
    format ~= "void ";
    format ~= field.symbol;

    format ~= "(";

    foreach (reference; field.references)
    {
        format ~= generateParameterListFor(reference);
    }

    format ~= ") {";
    foreach (reference; field.references)
    {
        format ~= generateCallFor(reference) ~ ";";
    }

    format ~= "}";

    return format;
}

/**
 * This function generates parameter names from a field's name.
 * 
 * Notes: 
 *      Some fields follow the pattern of, e.g., `setXYZ(float32, float32, float32)`.
 *      Using the name generator, ugly parameter names will be generated when 
 *      enough semantic information is already available to properly generate
 *      parameter names.
 *
 *      This method would take `setXYZ(float32, float32, float32)` and transform
 *      it to `setXYZ(float32 x, float32, y, float32 z)`, for example.
 */
void autogenParameterNames(AtomicField field)
{
    if (!field.symbol.startsWith("set"))
    {
        return;
    }

    foreach (parameter; field.parameters)
    {
        if (parameter.symbol.length > 0)
        {
            return;
        }
    }

    string name = field.symbol[3 .. $];
    if (name.filter!(a => isUpper(cast(dchar) a))().array.length != field.parameters.length)
    {
        return;
    }

    int start;
    int index = 1;
    int param;

    string[] names;

    void appendName()
    {
        if (index - start == 1)
        {
            names ~= [cast(char)(name[start].toLower)];
        }
        else
        {
            names ~= cast(char)(name[start].toLower) ~ name[start + 1 .. index];
        }

    }

    foreach (value; name[1 .. $])
    {
        if (isUpper(cast(dchar) value))
        {
            appendName();
            start = index;
        }
        index++;
    }
    if (names.length != field.parameters.length)
    {
        appendName();
    }

    foreach (i, para; field.parameters)
    {
        para.symbol = names[i];
    }
}

string generateAtomic(AtomicField field, bool stub)
{
    string format;

    bool isComplex = field.parameters.length > 1;
    bool isProperty = field.name.startsWith("set");
    string name;

    autogenParameterNames(field);

    if (isProperty)
    {
        name = field.name[3 .. $];
        if (!stub)
        {
        if (isComplex)
        {
            format ~= "struct " ~ name ~ "_t {";
        foreach (parameter; field.parameters)
        {
                format ~= generateDefinition(parameter) ~ ";";
            }
            format ~= "}";
            format ~= "private " ~ name ~ "_t _" ~ name ~ "; ";
        }
        else if (field.parameters.length == 1)
        {
            auto parameter = field.parameters[0];
            format ~= "private " ~ generateDefinition(parameter) ~ "_" ~ name ~ ";";
        }
        else
        {
            assert(0, name ~ " is a setter with no value!");
        }
        }
    }
    else
    {
        name = field.name;
    }
    format ~= "@FieldId(" ~ field.id.to!string ~ ") ";
    
    if (isProperty && !stub)
    {
        string fieldType;
        if (isComplex)
        {
            fieldType = name ~ "_t";
        }
        else
        {
            fieldType = generateDefinition(field.parameters[0]).split(' ')[0];
        }
        format ~= "@FieldType!(" ~ fieldType ~ ") ";
    }

    foreach (keyword; field.keywords)
    {
        format ~= "@" ~ keyword ~ " ";
    }

    if (stub)
    {
        format ~= " abstract ";
    }
    format ~= " void ";
    format ~= field.symbol;

    format ~= "(";
    format ~= generateParameterListFor(field);
    format ~= ")";

    string contracts;

    foreach (parameter; field.parameters)
    {
        contracts ~= generateContractFor(parameter);
    }
    if (contracts.length > 0)
    {
        format ~= " in {";
        format ~= contracts;
        format ~= "}";
        if(!stub)
        {
        format ~= "body";

    }
    }

    if (!stub)
    {
        format ~= "{";
    if (isProperty)
    {
        if (isComplex)
        {
        foreach (parameter; field.parameters)
        {
                format ~= "_" ~ name ~ "." ~ parameter.symbol;
                format ~= "=" ~ parameter.symbol ~ ";";
            }
        }
        else if (field.parameters.length == 1)
        {
            auto parameter = field.parameters[0];
            format ~= parameter.symbol ~ "_" ~ name;
            format ~= "=" ~ parameter.symbol ~ ";";
        }
    }
    else
    {
        format ~= `assert(0, "Override body not defined for ` ~ field.name ~ `!");`;
    }

    format ~= "}";
    }
    else
    {
        format ~= ";";
    }

    return format;
}

string generateParameterField(ParameterField field, bool stub)
{
    string format;
    format ~= "@FieldId(" ~ field.id.to!string ~ ") ";
    format ~= "@FieldType!(" ~ generateDefinition(field.parameter).split(' ')[0] ~ ") ";
    
    foreach (keyword; field.keywords)
    {
        format ~= " @" ~ keyword ~ " ";
    }

    if (stub)
    {
        string def = generateDefinition(field.parameter);
        format ~= "abstract " ~ def ~ "();";
        format ~= "abstract " ~ def ~ "(" ~ def.split(' ')[0] ~ ") @property;";
    }
    else
    {
    format ~= generateDefinition(field.parameter) ~ ";";
    }

    return format;
}

string generateCallFor(FieldDeclaration field)
{
    if (auto molecular = cast(MolecularField) field)
    {
        return generateMolecularCall(molecular);
    }
    else if (auto atomic = cast(AtomicField) field)
    {
        return generateAtomicCall(atomic);
    }
    else if (auto parameter = cast(ParameterField) field)
    {
        return generateParameterFieldCall(parameter);
    }
    else
    {
        assert(0);
    }

}

string[] flattenArgs(MolecularField field)
{
    string[] args;
    foreach (reference; field.references)
    {
        if (auto molecular = cast(MolecularField) field)
        {
            args ~= flattenArgs(molecular);
        }
        else if (auto atomic = cast(AtomicField) field)
        {
            foreach (arg; atomic.parameters)
            {
                args ~= arg.symbol;
            }
        }
        else if (auto paraField = cast(ParameterField) field)
        {
            args ~= paraField.parameter.symbol;
        }
    }
    return args;
}

string generateMolecularCall(MolecularField field)
{
    string[] args = flattenArgs(field);
    string format;
    format ~= field.symbol;
    format ~= "(";
    foreach (arg; args)
    {
        format ~= arg ~ ",";
    }
    format ~= ")";

    return format;
}

string generateAtomicCall(AtomicField field)
{
    string format;
    format ~= field.symbol;
    format ~= "(";
    foreach (arg; field.parameters)
    {
        format ~= arg.symbol ~ ",";
    }
    format ~= ")";

    return format;
}

string generateParameterFieldCall(ParameterField field)
{
    return "this." ~ field.name ~ "=" ~ field.name;
}

string generateParameterListFor(FieldDeclaration field)
{
    if (auto molecular = cast(MolecularField) field)
    {
        string format;
        foreach (reference; molecular.references)
        {
            format ~= generateParameterListFor(reference);
        }
        return format;
    }
    else if (auto atomic = cast(AtomicField) field)
    {
        return generateParameterList(atomic.parameters);
    }
    else if (auto parameter = cast(ParameterField) field)
    {
        return generateParameterList([parameter.parameter]);
    }
    return "";
}

string generateParameterList(Parameter[] parameters)
{
    string format;

    foreach (parameter; parameters)
    {
        format ~= generateDefinition(parameter) ~ ",";
    }

    return format;
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

    string type = parameter.type.symbol;
    string name = generateName(parameter.symbol, type);

    parameter.symbol = name;

    return mixin(interp!format);
}

string generateDefinition(ArrayParameter array)
{
    enum string format = `${type}[${maxLength}] ${name}`;

    string type = array.elementType.symbol;
    string name = generateName(array.symbol, type);
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

    string name = generateName(array.symbol, type);

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

    string type = numeric.type.to!string();
    string name = generateName(numeric.symbol, type);

    numeric.symbol = name;

    return mixin(interp!format);
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

private:

static int g_nameCounter;

string generateName(string name, string type)
{
    if (name.length == 0)
    {
        HashGenerator gen;
        gen.addInt(g_nameCounter++);
        gen.addString(type);
        name = mixin(interp!"_${type.toLower}${gen.hash}");
    }
    return name;
}