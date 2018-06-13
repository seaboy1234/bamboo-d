module bamboo.codegen.field_declarations.parameter;

import bamboo.codegen.field_declarations;



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
