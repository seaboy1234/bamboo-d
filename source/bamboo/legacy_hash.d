module bamboo.legacy_hash;

import std.conv;
import bamboo.hashgen;
import bamboo.types;

/// Hashes a module.
void hashModule(ref HashGenerator gen, Module mod)
{
    gen.addInt(1);
    gen.addInt(mod.structs.length + mod.classes.length);

    foreach (type; mod.typesById)
    {
        if (type.syntaxKind == SyntaxKind.ClassDeclaration)
        {
            hashClass(gen, cast(ClassDeclaration) type);
        }
        else
        {
            hashStruct(gen, cast(StructDeclaration) type);
        }
    }
}

private:

/// Represents legacy types supported by Panda3D.
enum LegacyType
{
    int8,
    int16,
    int32,
    int64,

    uint8,
    uint16,
    uint32,
    uint64,

    float64,

    string_,
    blob,

    char_ = 19,
    invalid = 20
}

void hashClass(ref HashGenerator gen, ClassDeclaration type)
{
    gen.addString(type.symbol);

    // Bug(?) in astron: doesn't get the full class graph.
    version (broken_parents)
    {
        gen.addInt(type.parents.length != 0 ? 1 : 0);
        if (type.parents.length != 0)
        {
            gen.addInt(type.parents[0].id);
        }
    }
    else
    {
        gen.addInt(type.parents.length);
        foreach (parent; type.parents)
        {
            gen.addInt(parent.id);
        }
    }

    if (type.hasConstructor)
    {
        hashField(gen, type.constructor);
    }

    gen.addInt(type.fields.length);
    foreach (field; type.fields)
    {
        hashField(gen, field);
    }
}

void hashStruct(ref HashGenerator gen, StructDeclaration type)
{
    gen.addString(type.symbol);
    gen.addInt(1);
    gen.addInt(0);

    gen.addInt(type.parameters.length);

    foreach (field; type.parameters)
    {
        hashField(gen, field);
    }
}

void hashField(ref HashGenerator gen, FieldDeclaration field)
{
    if (auto molecular = cast(MolecularField) field)
    {
        gen.addString(molecular.symbol);
        gen.addInt(molecular.id);

        gen.addInt(molecular.references.length);

        foreach (decl; molecular.references)
        {
            hashField(gen, decl);
        }
        return;
    }

    if (auto atomic = cast(AtomicField) field)
    {
        gen.addString(atomic.symbol);
        gen.addInt(atomic.id);

        gen.addInt(atomic.parameters.length);
        foreach (parameter; atomic.parameters)
        {
            hashParameter(gen, parameter);
        }

        hashKeywords(gen, atomic.keywords);
        return;
    }
    if (auto para = cast(ParameterField) field)
    {
        if (para.keywords.length > 0)
        {
            hashKeywords(gen, para.keywords);
        }
        hashLegacyType(gen, para.parameter);
        return;
    }

    assert(0);
}

void hashParameter(ref HashGenerator gen, Parameter parameter)
{
    hashLegacyType(gen, parameter);
}

void hashKeywords(ref HashGenerator gen, KeywordList list)
{
    // dfmt off
    immutable int[string] legacyKeywords = [
        "required": 0x0001,
        "broadcast": 0x0002,
        "ownrecv": 0x0004,
        "ram": 0x0008,
        "db": 0x0010,
        "clsend": 0x0020,
        "clrecv": 0x0040,
        "ownsend": 0x0080,
        "airecv": 0x0100,
        "": 0,
    ];
    // dfmt on

    int flags;
    foreach (keyword; list.keywords)
    {
        if (auto value = keyword in legacyKeywords)
        {
            flags |= *value;
        }
        else
        {
            // detected nonlegacy keyword.
            flags = ~0;
            break;
        }
    }

    // Everything is a legacy keyword.
    if (flags != ~0)
    {
        gen.addInt(flags);
    }
    else
    {
        gen.addInt(list.keywords.length);
        foreach (keyword; list.keywords)
        {
            gen.addString(keyword);
        }
    }
}

void hashLegacyType(T)(ref HashGenerator gen, T thing)
{
    LegacyType toLegacy(Type type)
    {
        switch (type) with (Type)
        {
        case blob:
        case varblob:
            return LegacyType.blob;
        case string_:
        case varstring:
            return LegacyType.string_;
        case int8:
            return LegacyType.int8;
        case int16:
            return LegacyType.int16;
        case int32:
            return LegacyType.int32;
        case int64:
            return LegacyType.int64;
        case uint8:
            return LegacyType.uint8;
        case uint16:
            return LegacyType.uint16;
        case uint32:
            return LegacyType.uint32;
        case uint64:
            return LegacyType.uint64;
        case char_:
            return LegacyType.char_;
        case float64:
            return LegacyType.float64;

        case float32:
        default:
            return LegacyType.invalid;
        }
    }

    Type type;
    LegacyType legacy;

    static if (is(typeof(thing.type) == Type))
    {
        type = thing.type;
    }
    else static if (is(typeof(thing.parameterType) == Type))
    {
        type = thing.parameterType;
    }
    else
    {
        static assert(0, "Cannot deduce type from " ~ T.stringof);
    }

    legacy = toLegacy(type);

    switch (type) with (Type)
    {
    case struct_:
        if (auto structPara = cast(StructParameter) thing)
        {
            if (auto cls = cast(ClassDeclaration) structPara.type)
            {
                hashClass(gen, cls);
            }
            else if (auto strct = cast(StructDeclaration) structPara.type)
            {
                hashStruct(gen, strct);
            }
            else
            {
                assert(0, "StructParameter is invalid type: " ~ structPara.type.symbol);
            }
        }
        else if (auto cls = cast(ClassDeclaration) thing)
        {
            hashClass(gen, cls);
        }
        else if (auto strct = cast(StructDeclaration) thing)
        {
            hashStruct(gen, strct);
        }
        else
        {
            assert(0, "Error: " ~ typeid(thing).to!string ~ " is not valid for this parameter.");
        }
        break;
    case array:
    case vararray:
        auto array = cast(ArrayParameter) thing;
        hashLegacyType(gen, array.elementType);

        if (array.hasRange)
        {
            ArrayRange range = array.size;
            gen.addInt(1);
            gen.addInt(range.minLength);
            gen.addInt(range.maxLength);
        }
        break;
    case blob:
    case varblob:
        auto blob = cast(SizedParameter) thing;
        // TODO: Sometimes this should be uint8?
        gen.addInt(LegacyType.blob);
        gen.addInt(1);

        if (blob.hasRange)
        {
            SizeConstraint range = blob.size;
            gen.addInt(1);
            gen.addInt(range.minSize);
            gen.addInt(range.maxSize);
        }
        break;
    case string_:
    case varstring:
        auto strblob = cast(SizedParameter) thing;
        // TODO: Sometimes this should be char?
        gen.addInt(LegacyType.string_);
        gen.addInt(1);

        if (strblob.hasRange)
        {
            SizeConstraint range = strblob.size;
            gen.addInt(1);
            gen.addInt(range.minSize);
            gen.addInt(range.maxSize);
        }
        break;

    case int8:
    case int16:
    case int32:
    case int64:
    case uint8:
    case uint16:
    case uint32:
    case uint64:
    case char_:
        auto intpara = cast(NumericParameter) thing;
        gen.addInt(legacy);
        hashIntType(gen, intpara);
        break;
    case float64:
        auto floatpara = cast(NumericParameter) thing;
        gen.addInt(LegacyType.float64);
        gen.addInt(floatpara.divisor);
        if (floatpara.hasModulus)
        {
            gen.addInt(cast(int)(floatpara.modulus * floatpara.divisor));
        }
        if (floatpara.hasRange)
        {
            gen.addInt(1);
            gen.addInt(cast(int)(floatpara.range.min * floatpara.divisor));
            gen.addInt(cast(int)(floatpara.range.max * floatpara.divisor));
        }
        break;
    case float32:
    case invalid:
        break;
    default:
        assert(0);
    }
}

void hashIntType(ref HashGenerator gen, NumericParameter numeric)
{
    import std.math : floor;

    gen.addInt(numeric.divisor);
    if (numeric.hasModulus)
    {
        uint modulus = cast(uint) floor(numeric.modulus * numeric.divisor + 0.5);
        gen.addInt(modulus);
    }
    if (numeric.hasRange)
    {
        gen.addInt(1);
        gen.addInt(cast(int)(numeric.range.min * numeric.divisor));
        gen.addInt(cast(int)(numeric.range.max * numeric.divisor));
    }
}
