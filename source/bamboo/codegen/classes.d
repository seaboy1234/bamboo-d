module bamboo.codegen.classes;

import bamboo.codegen;

string generateClass(ClassDeclaration cls, string baseType, bool generateStubs)
{
    string format;
    string firstFieldId = "0";
    if (cls.fields)
    {
        firstFieldId = cls.fields[0].id.to!string;
    }
    format ~= "@TypeId(" ~ cls.id.to!string ~ ", `" ~ cls.symbol ~ "`, " ~ firstFieldId ~ ") ";

    if (!generateStubs)
    {
        format ~= "abstract ";
    }

    format ~= "class ";
    format ~= cls.symbol;

    if (cls.hasSuperclass)
    {
        format ~= " : ";
        format ~= cls.parents[0].symbol;
    }
    else if (baseType.length > 0)
    {
        format ~= " : ";
        format ~= baseType;
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
    format ~= "@TypeId(" ~ strct.id.to!string ~ ", `" ~ strct.symbol ~ "`, "
        ~ strct.parameters[0].id.to!string ~ ")";
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
