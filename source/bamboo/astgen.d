module bamboo.astgen;
import std.conv;
import pegged.peg;
import bamboo.parser;
import bamboo.types;
import bamboo.util;

Module parseString(string source)
{
    auto node = DClass(source);
    auto completed = transform(node);
    return completed;
}

package Module transformParseTree(ParseTree tree)
{
    return transform(tree);
}

/// Given a path, parses a DC file.
/// Returns: a `Module`, representing the parsed file.
Module parseModule(string file)
{
    import std.file : readText;

    return parseString(readText(file));
}

/// Destructively mutates `file`, appending members from `other`.
void combineWith(Module file, Module other)
{
    ushort typeId = file.lastTypeId;
    ushort fieldId = file.lastFieldId;

    foreach (i, type; other.typesById)
    {
        type.id = ++typeId;
        if (auto cls = cast(ClassDeclaration) type)
        {
            foreach (field; cls.fields)
            {
                field.id = ++fieldId;
            }
            file.classes ~= cls;
        }
        else if (auto strct = cast(StructDeclaration) type)
        {
            foreach (field; strct.parameters)
            {
                field.id = ++fieldId;
            }
            file.structs ~= strct;
        }
        else if (auto def = cast(AliasDeclaration) type)
        {
            file.aliases ~= def;
        }
        else if (auto keyword = cast(KeywordDeclaration) type)
        {
            file.keywords ~= keyword;
        }
    }
    foreach (imprt; other.importDeclarations)
    {
        file.importDeclarations ~= imprt;
    }

    foreach (keyword; other.keywords)
    {
        file.keywords ~= keyword;
    }

    file.lastTypeId = typeId;
    file.lastFieldId = fieldId;
}

private:

class SyntaxResolver : Visitor
{
    enum Type[string] builtins = genbuiltins;
    TypeDeclaration[string] aliases;

    Module _module;

    override void visit(Module file)
    {
        _module = file;
        foreach (decl; file.aliases)
        {
            decl.visit(this);
        }
        foreach (type; file.typesById)
        {
            type.visit(this);
        }
    }

    TypeDeclaration resolve(string type)
    {
        if (auto ret = type in builtins)
        {
            return new BuiltinType(-1, type, *ret);
        }
        if (auto ret = type in aliases)
        {
            return *ret;
        }
        return _module.findType(type);
    }

    Parameter createParameter(TypeDeclaration type, string name)
    {
        import std.conv : to;

        switch (type.type) with (Type)
        {
        case int8:
        case int16:
        case int32:
        case int64:
        case uint8:
        case uint16:
        case uint32:
        case uint64:
        case char_:
        case float32:
        case float64:
            return new NumericParameter(name, type.type.to!string, 1, double.nan, null, null);
        case string_:
        case blob:
            return new SizedParameter(name, type.type.to!string, new SizeConstraint(0, 0), "");
        case struct_:
            return new StructParameter(name, type.symbol);
        case array:
            return new ArrayParameter(name, type.type.to!string, new ArrayRange(0, 0));
        default:
            break;
        }
        return null;
    }

    override void visit(ImportDeclaration node)
    {
    }

    override void visit(KeywordList node)
    {
    }

    override void visit(KeywordDeclaration node)
    {
    }

    override void visit(BuiltinType node)
    {
    }

    override void visit(StructDeclaration node)
    {
        foreach (field; node.parameters)
        {
            field.visit(this);
        }
    }

    override void visit(ClassDeclaration node)
    {
        node.resolve(_module);
        foreach (field; node.fields)
        {
            if (auto molecular = cast(MolecularField) field)
            {
                FieldDeclaration[] refs;
                foreach (reference; molecular.referenceNames)
                {
                    refs ~= node.getField(reference);
                }
                molecular.references = refs;
            }
            field.visit(this);
        }
        if (node.hasConstructor)
        {
            node.constructor.visit(this);
        }
    }

    override void visit(AliasDeclaration node)
    {
        node.aliasedType = resolve(node.aliasedTypeName);
        aliases[node.symbol] = node.aliasedType;
    }

    override void visit(MolecularField node)
    {

    }

    override void visit(AtomicField node)
    {
        Parameter[] parameters;

        foreach (parameter; node.parameters)
        {
            parameter.visit(this);
            if (auto structPara = cast(StructParameter) parameter)
            {
                if (auto type = cast(BuiltinType) structPara.type)
                {
                    auto newPara = createParameter(structPara.type, structPara.symbol);
                    if (newPara !is null)
                    {
                        parameters ~= newPara;
                        continue;
                    }
                }
            }
            parameters ~= parameter;
        }

        node.parameters = parameters;
    }

    override void visit(ParameterField node)
    {
        node.parameter.visit(this);
        if (StructParameter strct = cast(StructParameter) node.parameter)
        {
            import std.algorithm : canFind;

            auto numerics = [
                Type.int8, Type.int16, Type.int32, Type.int64, Type.uint8,
                Type.uint16, Type.uint32, Type.uint64, Type.char_, Type.float32, Type.float64
            ];

            if (numerics.canFind(strct.parameterType))
            {
                node.parameter = createParameter(strct.type, strct.symbol);
                node.visit(this);
            }
        }
    }

    override void visit(NumericParameter node)
    {
    }

    override void visit(NumericRange node)
    {
    }

    override void visit(NumericTransform node)
    {
    }

    override void visit(NumericConstant node)
    {
    }

    override void visit(SizedParameter node)
    {
    }

    override void visit(SizeConstraint node)
    {
    }

    override void visit(StructParameter node)
    {
        node.type = resolve(node.typeName);
    }

    override void visit(ArrayParameter node)
    {
        node.elementType = resolve(node.type);
        node.element = createParameter(node.elementType, "");
        node.element.visit(this);
    }

    override void visit(ArrayRange node)
    {
    }

}

Module transform(ParseTree node)
{
    assert(node.name == "DClass");

    return transformModule(node.children[0]);
}

//dfmt off
enum SyntaxType
{
    Module              = "DClass.Module",
    DCFile              = "DClass.DCFile",
    ParseDirective      = "DClass.ParseDirective",
    ImportDecl          = "DClass.ImportDecl",
    ImportList          = "DClass.ImportList",
    TypeDecl            = "DClass.TypeDecl",
    AliasType           = "DClass.AliasType",
    KeywordType         = "DClass.KeywordType",
    KeywordList         = "DClass.KeywordList",
    StructType          = "DClass.StructType",
    ClassType           = "DClass.ClassType",
    FieldDecl           = "DClass.FieldDecl",
    MolecularField      = "DClass.MolecularField",
    AtomicField         = "DClass.AtomicField",
    ParameterField      = "DClass.ParameterField",
    Parameter           = "DClass.Parameter",
    ParameterList       = "DClass.ParameterList",
    CharParameter       = "DClass.CharParameter",
    IntParameter        = "DClass.IntParameter",
    IntConstant         = "DClass.IntConstant",
    IntTransform        = "DClass.IntTransform",
    IntRange            = "DClass.IntRange",
    FloatParameter      = "DClass.FloatParameter",
    FloatConstant       = "DClass.FloatConstant",
    FloatTransform      = "DClass.FloatTransform",
    FloatRange          = "DClass.FloatRange",
    SizedParameter      = "DClass.SizedParameter",
    SizeConstraint      = "DClass.SizeConstraint",
    StructParameter     = "DClass.StructParameter",
    ArrayParameter      = "DClass.ArrayParameter",
    ArrayRange          = "DClass.ArrayRange",
    InterfaceMarker     = "DClass.InterfaceMarker",
    Identifier          = "DClass.Identifier",
    QualifiedIdentifier = "DClass.QualifiedIdentifier",
    IdentifierList      = "DClass.IdentifierList",
    dataType            = "DClass.dataType",
}

enum LiteralType
{
    charLiteral     = "DClass.charLiteral",
    intLiteral      = "DClass.intLiteral",
    floatLiteral    = "DClass.floatLiteral",
    stringLiteral   = "DClass.stringLiteral",
}
//dfmt on

ParseTree child(ParseTree node, int index)
{
    if (node.children.length <= index)
    {
        return ParseTree("", false, [], "", 0, 0, []);
    }
    return node.children[index];
}

Module transformModule(ParseTree node)
{
    assert(node.name == SyntaxType.Module);

    auto file = transformFile(node.children[0]);

    SyntaxResolver resolver = new SyntaxResolver();
    resolver.visit(file);

    return file;
}

Module transformFile(ParseTree node)
{
    assert(node.name == SyntaxType.DCFile);

    ushort id;
    ushort fieldId;
    int skip;
    string name;

    ImportDeclaration[] imports;

    ClassDeclaration[] classes;
    StructDeclaration[] structs;
    KeywordDeclaration[] keywords;
    AliasDeclaration[] aliases;

    // Check if this module has a name.
    if (node.children[0].name == SyntaxType.ParseDirective)
    {
        ParseTree child = node.children[0];
        string directive = transformIdentifier(child.children[0]);
        string value = child.children[1].matches[0];

        if (directive == "module")
        {
            skip = 1;
            name = value;
        }
    }

    foreach (child; node.children[skip .. $])
    {
        switch (cast(SyntaxType) child.name) with (SyntaxType)
        {
        case ParseDirective:
            {
                string directive = transformIdentifier(child.children[0]);
                string value = child.children[1].matches[0];

                if (directive == "typeid")
                {
                    id = value.to!ushort;
                }
            }
            break;
        case ImportDecl:
            imports ~= transformImport(child);
            break;
        case TypeDecl:
            child = child.children[0];
            switch (cast(SyntaxType) child.name) with (SyntaxType)
            {
            case KeywordType:
                keywords ~= transformKeywordType(child, 0);
                break;
            case StructType:
                structs ~= transformStructType(child, id++, fieldId);
                break;
            case ClassType:
                classes ~= transformClassType(child, id++, fieldId);
                break;
            case AliasType:
                aliases ~= transformAliasType(child, 0);
                break;
            default:
                assert(0);
            }
            break;
        default:
            assert(0, "Expected ImportDecl or TypeDecl. Got " ~ child.name);
        }
    }

    return new Module(name, imports, aliases, classes, structs, keywords, id, fieldId);
}

string transformIdentifier(SyntaxType type = SyntaxType.Identifier)(ParseTree node)
{
    assert(node.name == type, node.name ~ " != " ~ type);
    return node.matches[0];
}

ImportDeclaration transformImport(ParseTree node)
{
    assert(node.name == SyntaxType.ImportDecl);
    string module_ = transformIdentifier!(SyntaxType.QualifiedIdentifier)(node.children[0]);

    string[] symbols = transformImportList(node.children[1]);

    return new ImportDeclaration(module_, symbols);
}

string[] transformImportList(ParseTree node)
{
    assert(node.name == SyntaxType.ImportList);
    // Expand symbol list from, e.g., LoginManager/AI/UD
    // to LoginManager, LoginManagerAI, LoginManagerUD
    string firstSymbol = transformIdentifier(node.children[0]);
    string[] symbols;

    symbols ~= firstSymbol;
    foreach (child; node.children[1 .. $])
    {
        symbols ~= firstSymbol ~ transformIdentifier(child);
    }

    return symbols;
}

AliasDeclaration transformAliasType(ParseTree node, ushort id)
{
    assert(node.name == SyntaxType.AliasType);
    string symbol = transformIdentifier(node.children[1]);
    string type = node.children[0].matches[0];

    return new AliasDeclaration(id, symbol, type);
}

KeywordDeclaration transformKeywordType(ParseTree node, ushort id)
{
    assert(node.name == SyntaxType.KeywordType);
    string symbol = transformIdentifier(node.children[0]);
    return new KeywordDeclaration(id, symbol);
}

KeywordList transformKeywordList(ParseTree node)
{
    assert(node.name == SyntaxType.KeywordList);

    string[] symbols;

    foreach (child; node.children)
    {
        symbols ~= transformIdentifier(child);
    }

    return new KeywordList(symbols);
}

StructDeclaration transformStructType(ParseTree node, ushort id, ref ushort fieldId)
{
    assert(node.name == SyntaxType.StructType);

    string symbol = transformIdentifier(node.children[0]);

    ParameterField[] members;

    foreach (child; node.children[1 .. $])
    {
        members ~= new ParameterField(fieldId++, transformParameter(child), new KeywordList([]));
    }

    return new StructDeclaration(id, symbol, members);
}

ClassDeclaration transformClassType(ParseTree node, ushort id, ref ushort fieldId)
{
    assert(node.name == SyntaxType.ClassType);

    int cur = 0;
    bool isInterface;

    if (node.child(cur).name == SyntaxType.InterfaceMarker)
    {
        cur++;
        isInterface = true;
    }

    string symbol = transformIdentifier(node.children[cur++]);
    string[] superclasses;

    if (node.child(cur).name == SyntaxType.IdentifierList)
    {
        foreach (child; node.children[cur].children)
        {
            superclasses ~= transformIdentifier(child);
        }
        cur++;
    }

    FieldDeclaration[] members = [];
    FieldDeclaration cotr;

    if (cur < node.children.length - 1)
    {
        foreach (child; node.children[cur .. $])
        {
            auto field = transformFieldDecl(child, fieldId++);
            if (is(field == AtomicField))
            {
                cotr = field;
            }
            else
            {
                members ~= field;
            }
        }
    }

    return new ClassDeclaration(id, symbol, superclasses, isInterface, cotr, members);
}

FieldDeclaration transformFieldDecl(ParseTree node, ushort id)
{
    assert(node.name == SyntaxType.FieldDecl);

    ParseTree child = node.children[0];

    switch (cast(SyntaxType) child.name) with (SyntaxType)
    {
    case AtomicField:
        return transformAtomicField(child, id);
    case MolecularField:
        return transformMolecularField(child, id);
    case ParameterField:
        return transformParameterField(child, id);
    default:
        assert(0);
    }
}

MolecularField transformMolecularField(ParseTree node, ref ushort id)
{
    assert(node.name == SyntaxType.MolecularField);

    string symbol = transformIdentifier(node.children[0]);
    string[] references;

    foreach (child; node.children[1].children)
    {
        references ~= transformIdentifier(child);
    }

    return new MolecularField(id, symbol, references);
}

AtomicField transformAtomicField(ParseTree node, ref ushort id)
{
    assert(node.name == SyntaxType.AtomicField);

    string symbol = transformIdentifier(node.children[0]);

    Parameter[] parameters;
    KeywordList keywords = new KeywordList([]);

    int cur = 1;

    if (node.child(cur).name == SyntaxType.ParameterList)
    {
        parameters = transformParameterList(node.children[cur++]);
    }
    if (node.child(cur).name == SyntaxType.KeywordList)
    {
        keywords = transformKeywordList(node.child(cur));
    }

    return new AtomicField(id, symbol, parameters, keywords);
}

Parameter[] transformParameterList(ParseTree node)
{
    assert(node.name == SyntaxType.ParameterList);

    Parameter[] parameters;

    foreach (child; node.children)
    {
        parameters ~= transformParameter(child);
    }

    return parameters;
}

ParameterField transformParameterField(ParseTree node, ref ushort id)
{
    assert(node.name == SyntaxType.ParameterField);

    Parameter parameter = transformParameter(node.children[0]);
    KeywordList keywords = new KeywordList([]);

    if (node.children.length == 2)
    {
        keywords = transformKeywordList(node.children[1]);
    }

    return new ParameterField(id, parameter, keywords);
}

Parameter transformParameter(ParseTree node)
{
    assert(node.name == SyntaxType.Parameter);

    ParseTree child = node.children[0];

    switch (child.name) with (SyntaxType)
    {
    case CharParameter:
        return transformCharParameter(child);
    case IntParameter:
    case FloatParameter:
        return transformNumericParameter(child);
    case SizedParameter:
        return transformSizedParameter(child);
    case ArrayParameter:
        return transformArrayParameter(child);
    case StructParameter:
        return transformStructParameter(child);
    default:
        assert(0);
    }
}

//dfmt off
T readLiteral(T)(ParseTree node)
    if(is(T == char)
    || is(T == string)
    || is(T == int)
    || is(T == double)
    )
{
    import std.conv : to;

    static if (is(T == char))
    {
        assert(node.name == LiteralType.charLiteral);
        return node.children[0].matches[0][0];
    }
    else static if (is(T == string))
    {
        assert(node.name == LiteralType.stringLiteral);
        return node.children[0].matches[0];
    }
    else static if (is(T == int))
    {
        assert(node.name == LiteralType.intLiteral);
        return node.matches[0].to!int();
    }
    else static if (is(T == double))
    {
        //assert(node.name == LiteralType.floatLiteral);
        return node.matches[0].to!double();
    }
    else
    {
        static assert(0, "?");
    }
}
//dfmt on

NumericParameter transformCharParameter(ParseTree node)
{
    assert(node.name == SyntaxType.CharParameter);

    string symbol;
    char defaultVal;
    int valIndex;

    if (node.children.length == 0)
    {
        return new NumericParameter(symbol, "char", 1, double.nan, null, null);
    }

    if (node.child(valIndex).name == SyntaxType.Identifier)
    {
        symbol = transformIdentifier(node.children[valIndex++]);
    }

    if (node.child(valIndex).name == LiteralType.charLiteral)
    {
        defaultVal = readLiteral!char(node.children[valIndex]);
    }

    return new NumericParameter(symbol, "char", 1, double.nan, null,
            new NumericConstant(cast(double) defaultVal, null));
}

NumericParameter transformNumericParameter(ParseTree node)
{
    //assert(node.name == SyntaxType.IntParameter);

    immutable string type = node.children[0].matches[0];
    int cur = 1;

    NumericRange range;
    NumericTransform transform;
    string symbol;
    NumericConstant defaultValue;

    // dfmt off
    if (node.child(cur).name == SyntaxType.IntRange
     || node.child(cur).name == SyntaxType.FloatRange)
    {
        range = transformNumericRange(node.children[cur++]);
    }
    if (node.child(cur).name == SyntaxType.IntTransform
     || node.child(cur).name == SyntaxType.FloatTransform)
    {
        transform = transformNumericTransform(node.children[cur++]);
    }
    if (node.child(cur).name == SyntaxType.Identifier)
    {
        symbol = transformIdentifier(node.children[cur++]);
    }
    if (node.child(cur).name == SyntaxType.IntConstant
     || node.child(cur).name == SyntaxType.FloatConstant)
    {
        defaultValue = transformNumericConstant(node.children[cur++]);
    }
    // dfmt on

    auto parameter = new NumericParameter(symbol, type, 1, double.nan, range, defaultValue);
    if (transform !is null)
    {
        if (transform.operator == Operator.modulo)
        {
            parameter.modulus = transform.value;
        }
        else if (transform.operator == Operator.divide)
        {
            parameter.divisor = cast(uint) transform.value;
        }
        else if (transform.operator == Operator.multiply)
        {
            //parameter.divisor = (1 / transform.value);
        }
    }
    return parameter;
}

NumericConstant transformNumericConstant(ParseTree node)
{
    //assert(node.name == SyntaxType.IntConstant);

    immutable double value = readLiteral!double(node.children[0]);
    NumericTransform transform;

    if (node.children.length > 1)
    {
        transform = transformNumericTransform(node.children[1]);
    }

    return new NumericConstant(value, transform);
}

NumericTransform transformNumericTransform(ParseTree node)
{
    //assert(node.name == SyntaxType.IntTransform);

    Operator op = cast(Operator) node.children[0].matches[0];
    immutable double value = readLiteral!double(node.children[1]);
    NumericTransform next;

    if (node.children.length > 2)
    {
        next = transformNumericTransform(node.children[2]);
    }

    return new NumericTransform(op, value, next);
}

NumericRange transformNumericRange(ParseTree node)
{
    // assert(node.name == SyntaxType.IntRange);

    int lower;
    int upper = readLiteral!int(node.children[0]);

    if (node.children.length == 2)
    {
        lower = upper;
        upper = readLiteral!int(node.children[1]);
    }

    return new NumericRange(lower, upper);
}

void swap(T)(T* left, T* right)
{
    const auto tmp = left;
    left = right;
    right = tmp;
}

SizedParameter transformSizedParameter(ParseTree node)
{
    assert(node.name == SyntaxType.SizedParameter);

    string type = node.children[0].matches[0];
    SizeConstraint size;
    string symbol;
    string defaultVal;

    int cur = 1;

    if (node.child(cur).name == SyntaxType.SizeConstraint)
    {
        size = transformSizeConstraint(node.children[cur++]);
    }
    if (node.child(cur).name == SyntaxType.Identifier)
    {
        symbol = transformIdentifier(node.children[cur++]);
    }
    if (node.child(cur).name == LiteralType.stringLiteral)
    {
        defaultVal = readLiteral!string(node.child(cur));
    }

    return new SizedParameter(symbol, type, size, defaultVal);
}

SizeConstraint transformSizeConstraint(ParseTree node)
{
    assert(node.name == SyntaxType.SizeConstraint);

    int upper = readLiteral!int(node.children[0]);
    immutable int lower = upper;

    if (node.children.length == 2)
    {
        upper = readLiteral!int(node.children[1]);
    }

    return new SizeConstraint(lower, upper);
}

StructParameter transformStructParameter(ParseTree node)
{
    assert(node.name == SyntaxType.StructParameter);

    string type = transformIdentifier(node.children[0]);
    string symbol;

    if (node.children.length > 1)
    {
        symbol = transformIdentifier(node.children[1]);
    }

    return new StructParameter(symbol, type);
}

ArrayParameter transformArrayParameter(ParseTree node)
{
    assert(node.name == SyntaxType.ArrayParameter);

    string type = node.children[0].matches[0];
    string symbol;
    ArrayRange size;

    int cur = 1;
    if (node.child(cur).name == SyntaxType.Identifier)
    {
        symbol = transformIdentifier(node.children[cur++]);
    }

    size = transformArrayRange(node.child(cur));

    return new ArrayParameter(symbol, type, size);
}

ArrayRange transformArrayRange(ParseTree node)
{
    assert(node.name == SyntaxType.ArrayRange);

    if (node.children.length == 0)
    {
        return new ArrayRange(0, 0);
    }

    int minLength;
    int maxLength;

    if (node.children.length == 1)
    {
        maxLength = readLiteral!int(node.children[0]);
        minLength = maxLength;
    }
    else if (node.children.length == 2)
    {
        minLength = readLiteral!int(node.children[0]);
        maxLength = readLiteral!int(node.children[1]);
    }

    return new ArrayRange(minLength, maxLength);
}
