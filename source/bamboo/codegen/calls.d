module bamboo.codegen.calls;

import bamboo.codegen;

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
    if (field.parameters.length > 1)
    {
        format ~= field.symbol;
    }
    else
    {
        format ~= "this." ~ cast(char)(field.name[3].toLower) ~ field.name[4 .. $];
    }
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
