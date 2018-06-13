module bamboo.codegen.field_declarations;

public import bamboo.codegen;
public import bamboo.codegen.field_declarations.atomic;
public import bamboo.codegen.field_declarations.molecular;
public import bamboo.codegen.field_declarations.parameter;

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

