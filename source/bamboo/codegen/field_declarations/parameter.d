module bamboo.codegen.field_declarations.parameter;

import std.format;

import bamboo.codegen.field_declarations;

private enum parameterFieldStubbed = q{
    void %1$s (%2$s value) @FieldId(%3$s) %4$-(@%s %) @property
    {
        _%1$s = value;
    }
    inout(%2$s) %1$s() inout @property
    {
        return _%1$s;
    }
};

private enum parameterFieldAbstract = q{
    abstract void %1$s(%2$s value) @FieldId(%3$s) %4$-(@%s %) @property;
    abstract %2$s %1$s() @property;
};

string generateParameterField(ParameterField field, bool stub)
{
    string code;
    string def = generateDefinition(field.parameter);
    string type = def.split(' ')[0];
    string name = def.split(' ')[1];

    if (!stub)
    {
        return parameterFieldAbstract.format(name, type, field.id, field.keywords.keywords);
    }
    
    return parameterFieldStubbed.format(name, type, field.id, field.keywords.keywords);
}
