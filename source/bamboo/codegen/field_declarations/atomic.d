module bamboo.codegen.field_declarations.atomic;

import bamboo.codegen.field_declarations;

string generateAtomic(AtomicField field, bool stub)
{
    string generated;
    string fieldType;

    bool isComplex = field.parameters.length > 1;
    bool isProperty = field.name.startsWith("set");
    string name;

    string generateUnderlyingAtomicField()
    {
        string format;

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
            format ~= "private " ~ generateDefinition(parameter).split(' ')[0] ~ " _" ~ name ~ ";";
        }
        else
        {
            assert(0, name ~ " is a setter with no value!");
        }

        return format;

    }

    string getFieldName()
    {
        string name;

        if (!isProperty || isComplex)
        {
            return field.name;
        }

        name = cast(char)(field.name[3].toLower) ~ field.name[4 .. $];

        if (name[1 .. $].count!(x => isUpper(cast(dchar) x)) == name.length - 1)
        {
            name = field.name[3 .. $];
        }

        return name;
    }

    string generateAtomicDeclaration(out string fieldType)
    {
        string format;

        format ~= "@FieldId(" ~ field.id.to!string ~ ") ";

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

        format ~= name;

        format ~= "(";
        format ~= generateParameterListFor(field);
        format ~= ") ";

        if (!isComplex && isProperty)
        {
            format ~= "@property ";
        }

        return format;
    }

    string generateContracts()
    {
        string contracts;
        string format;

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

        return format;
    }

    string generateBody()
    {
        string format;

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

        return format;
    }

    string generateGetter()
    {
        string format;

        format ~= "@FieldId(" ~ field.id.to!string ~ ") ";

        if (!isComplex)
        {
            if (!stub)
            {
                format ~= "abstract " ~ fieldType ~ " " ~ name ~ "() @property;";
            }
            else
            {
                format ~= fieldType ~ " " ~ name ~ "() @property { ";
                format ~= "return _" ~ name ~ ";";
                format ~= "}";
            }
        }
        else
        {
            if (stub)
            {
                format ~= "auto " ~ name ~ "() {";
                format ~= "return _" ~ name ~ ";";
                format ~= "}";
            }
        }

        return format;
    }

    autogenParameterNames(field);

    name = getFieldName();

    if (stub)
    {
        generated ~= generateUnderlyingAtomicField();
    }

    generated ~= generateAtomicDeclaration(fieldType);
    generated ~= generateContracts();
    generated ~= generateBody();

    if (isProperty)
    {
        generated ~= generateGetter();
    }

    return generated;
}

private:

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
