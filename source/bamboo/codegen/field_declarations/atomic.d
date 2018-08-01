module bamboo.codegen.field_declarations.atomic;

import bamboo.codegen.field_declarations;

string generateAtomic(AtomicField field, bool stub)
{
    string generated;
    string fieldType;

    bool isComplex = field.parameters.length > 1;
    bool isProperty = field.name.startsWith("set");

    string name = (() @trusted{
        if (!isProperty)
        {
            return field.name;
        }

        if (field.name[3 .. $].all!(x => isUpper(x)))
        {
            return field.name[3 .. $].toLower;
        }

        return cast(char)(field.name[3].toLower) ~ field.name[4 .. $];
    })();

    int counter = field.id;

    foreach (parameter; field.parameters)
    {
        counter++;
        parameter.symbol = generateName(parameter.symbol, parameter.parameterTypeName, counter);
    }

    string generateUnderlyingAtomicField()
    {
        string format;

        if (isComplex)
        {
            format ~= "private Tuple!(";
            foreach (parameter; field.parameters)
            {
                string[] parts = generateDefinition(parameter).split(' ');
                string type = parts[0];
                string param = parts[1];

                format ~= type ~ "," ~ "`" ~ param ~ "`,";
            }
            format ~= ") _" ~ name ~ "; ";
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

    string generateAtomicDeclaration(out string fieldType)
    {
        string format;

        format ~= "@FieldId(" ~ field.id.to!string ~ ") ";

        if (isProperty)
        {
            if (isComplex && stub)
            {
                fieldType = "typeof(_" ~ name ~ ")";
            }
            else if (!isComplex)
            {
                fieldType = generateDefinition(field.parameters[0]).split(' ')[0];
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
            format ~= "@property";
        }

        return format;
    }

    string generateComplexSetter()
    {
        return q{
            final void %1$s(%2$s value)
            {
                %1$s(value.expand);
            }

            final void %1$s(Tuple!(%2$s.Types) value)
            {
                %1$s(value.expand);
            }
        }.format(name, fieldType);
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
            format ~= "@property ";
        }

        if (!stub)
        {
            format ~= "abstract " ~ fieldType ~ " " ~ name ~ "();";
        }
        else
        {
            format ~= fieldType ~ " " ~ name ~ "() { ";
            format ~= "return _" ~ name ~ ";";
            format ~= "}";
        }

        return format;
    }

    autogenParameterNames(field);

    if (stub && isProperty)
    {
        generated ~= generateUnderlyingAtomicField();
    }

    generated ~= generateAtomicDeclaration(fieldType);
    generated ~= generateContracts();
    generated ~= generateBody();

    if (isProperty)
    {
        if (isComplex)
        {
            generated ~= generateComplexSetter();
        }
        generated ~= generateGetter();
        generated ~= text("alias ", field.name, " = ", name, ";"); // setField
        generated ~= text("alias get", field.name[3 .. $], " = ", name, ";"); // getField
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

    if (field.parameters.length == 1)
    {
        string name = field.symbol[3 .. $];
        field.parameters[0].symbol = cast(char) name[0].toLower ~ name[1 .. $];
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
