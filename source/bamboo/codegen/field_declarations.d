module bamboo.codegen.field_declarations;

import bamboo.codegen;

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

string generateAtomic(AtomicField field, bool stub)
{
    string format;

    bool isComplex = field.parameters.length > 1;
    bool isProperty = field.name.startsWith("set");
    string name;

    autogenParameterNames(field);

    if (isProperty)
    {
        name = cast(char)(field.name[3].toLower) ~ field.name[4 .. $];
        if (name[1 .. $].count!(x => isUpper(cast(dchar) x)) == name.length - 1)
        {
            name = field.name[3 .. $];
        }
        if (stub)
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
                format ~= "private " ~ generateDefinition(parameter).split(' ')[0] ~ " _"
                    ~ name ~ ";";
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
    string fieldType;

    if (isProperty)
    {
        if (isComplex && stub)
        {
            fieldType = name ~ "_t";
        }
        else if (!isComplex)
        {
            fieldType = generateDefinition(field.parameters[0]).split(' ')[0];
        }
        if (stub)
        {
            format ~= "@FieldType!(" ~ fieldType ~ ") ";
        }
    }

    foreach (keyword; field.keywords)
    {
        format ~= "@" ~ keyword ~ " ";
    }

    if (!stub)
    {
        format ~= " abstract ";
    }
    format ~= " void ";

    if (isComplex)
    {
        format ~= field.symbol;
    }
    else
    {
        format ~= name;
    }

    format ~= "(";
    format ~= generateParameterListFor(field);
    format ~= ") ";

    if (!isComplex && isProperty)
    {
        format ~= "@property ";
    }

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
        if (stub)
        {
            format ~= "body";
        }
    }

    if (stub)
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
                format ~= "_" ~ name;
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

    if (isProperty)
    {
        format ~= "@FieldId(" ~ field.id.to!string ~ ") ";

        if (!isComplex)
        {
            if (!stub)
            {
                format ~= "abstract " ~ fieldType ~ " " ~ name ~ "() inout @property;";
            }
            else
            {
                format ~= fieldType ~ " " ~ name ~ "() inout @property { ";
                format ~= "return _" ~ name ~ ";";
                format ~= "}";
            }
        }
        else
        {
            if (stub)
            {
                format ~= "auto " ~ name ~ "() inout @property {";
                format ~= "return _" ~ name ~ ";";
                format ~= "}";
            }
        }
    }

    return format;
}

string generateParameterField(ParameterField field, bool stub)
{
    string format;
    string def = generateDefinition(field.parameter);
    string type = def.split(' ')[0];
    string name = def.split(' ')[1];

    format ~= "@FieldId(" ~ field.id.to!string ~ ") ";
    format ~= "@FieldType!(" ~ generateDefinition(field.parameter).split(' ')[0] ~ ") ";

    foreach (keyword; field.keywords)
    {
        format ~= " @" ~ keyword ~ " ";
    }

    if (!stub)
    {
        format ~= "abstract void " ~ name ~ "(" ~ type ~ " value) @property;";
        format ~= "abstract " ~ def ~ "();";
    }
    else
    {
        format ~= "void " ~ name ~ "(" ~ type ~ " value) @property {";
        format ~= "_" ~ name ~ " = value;";
        format ~= "}";

        format ~= "@FieldId(" ~ field.id.to!string ~ ") ";

        format ~= type ~ " " ~ name ~ "() inout @property {";
        format ~= "return _" ~ name ~ ";";
        format ~= "}";
        format ~= type ~ " _" ~ name ~ ";";
    }

    return format;
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

