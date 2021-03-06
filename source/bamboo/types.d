module bamboo.types;

import std.array;
import std.algorithm;
import std.conv;
import std.range;

import boilerplate;

import bamboo.hashgen;
import bamboo.util;

private
{
    enum string generateVisit = q{
        override SyntaxKind syntaxKind() @property
        {
            return mixin("SyntaxKind." ~ (typeof(this).stringof));
        }
        override void visit(Visitor visitor)
        {
            visitor.visit(this);
        }
    };

    enum string generateSyntaxNode = q{
        mixin(GenerateThis);
        mixin(generateVisit);

        static if(is(typeof(symbol)))
        {
            override string toString()
            {
                return typeid(this).name ~ " " ~ symbol;
            }
        }
    };
}

/// Represents the type of a SyntaxNode.
enum SyntaxKind
{
    Module,
    ImportDeclaration,
    KeywordList,
    BuiltinType,
    KeywordDeclaration,
    StructDeclaration,
    ClassDeclaration,
    AliasDeclaration,
    MolecularField,
    AtomicField,
    ParameterField,
    NumericParameter,
    NumericRange,
    NumericTransform,
    NumericConstant,
    SizedParameter,
    SizeConstraint,
    StructParameter,
    ArrayParameter,
    ArrayRange,
}

//dfmt off

/// Represents an operator in an $(D IntTransformation) or a $(D FloatTransformation).
enum Operator
{
    modulo      = "%",
    multiply    = "*",
    add         = "+",
    subtract    = "-",
    divide      = "/",
}
//dfmt on

// dfmt off
/// Represents the fundamental type of a node.
enum Type
{
    int8, int16, int32, int64,
    uint8, uint16, uint32, uint64,
    char_,
    float32, float64,

    string_,
    varstring,
    blob,
    varblob,
    array,
    vararray,

    struct_,
    method,
    
    invalid,
}
// dfmt on

/// Represents an object which can act upon a $(D SyntaxNode).
abstract class Visitor
{
    /// Visits the given $(D SyntaxNode).
    void visitNode(SyntaxNode node)
    {
        final switch (node.syntaxKind)
        {
        case SyntaxKind.Module:
            return visit(cast(Module) node);
        case SyntaxKind.ImportDeclaration:
            return visit(cast(ImportDeclaration) node);
        case SyntaxKind.KeywordList:
            return visit(cast(KeywordList) node);
        case SyntaxKind.BuiltinType:
            return visit(cast(BuiltinType) node);
        case SyntaxKind.KeywordDeclaration:
            return visit(cast(KeywordDeclaration) node);
        case SyntaxKind.StructDeclaration:
            return visit(cast(StructDeclaration) node);
        case SyntaxKind.ClassDeclaration:
            return visit(cast(ClassDeclaration) node);
        case SyntaxKind.AliasDeclaration:
            return visit(cast(AliasDeclaration) node);
        case SyntaxKind.MolecularField:
            return visit(cast(MolecularField) node);
        case SyntaxKind.AtomicField:
            return visit(cast(AtomicField) node);
        case SyntaxKind.ParameterField:
            return visit(cast(ParameterField) node);
        case SyntaxKind.NumericParameter:
            return visit(cast(NumericParameter) node);
        case SyntaxKind.NumericRange:
            return visit(cast(NumericRange) node);
        case SyntaxKind.NumericTransform:
            return visit(cast(NumericTransform) node);
        case SyntaxKind.NumericConstant:
            return visit(cast(NumericConstant) node);
        case SyntaxKind.SizedParameter:
            return visit(cast(SizedParameter) node);
        case SyntaxKind.SizeConstraint:
            return visit(cast(SizeConstraint) node);
        case SyntaxKind.StructParameter:
            return visit(cast(StructParameter) node);
        case SyntaxKind.ArrayParameter:
            return visit(cast(ArrayParameter) node);
        case SyntaxKind.ArrayRange:
            return visit(cast(ArrayRange) node);
        }
    }

    /// Visits the given $(D Module).
    abstract void visit(Module file);

    /// Visits the given $(D ImportDeclaration).    
    abstract void visit(ImportDeclaration node);

    /// Visits the given $(D KeywordList).    
    abstract void visit(KeywordList node);

    /// Visits the given $(D BuiltinType).    
    abstract void visit(BuiltinType node);

    /// Visits the given $(D KeywordDeclaration).    
    abstract void visit(KeywordDeclaration node);

    /// Visits the given $(D StructDeclaration).    
    abstract void visit(StructDeclaration node);

    /// Visits the given $(D ClassDeclaration).    
    abstract void visit(ClassDeclaration node);

    /// Visits the given $(D AliasDeclaration).    
    abstract void visit(AliasDeclaration node);

    /// Visits the given $(D MolecularField).    
    abstract void visit(MolecularField node);

    /// Visits the given $(D AtomicField).    
    abstract void visit(AtomicField node);

    /// Visits the given $(D ParameterField).    
    abstract void visit(ParameterField node);

    /// Visits the given $(D IntParameter).    
    abstract void visit(NumericParameter node);

    /// Visits the given $(D IntRange).    
    abstract void visit(NumericRange node);

    /// Visits the given $(D IntTransform).    
    abstract void visit(NumericTransform node);

    /// Visits the given $(D IntConstant).    
    abstract void visit(NumericConstant node);

    /// Visits the given $(D SizedParameter).    
    abstract void visit(SizedParameter node);

    /// Visits the given $(D SizeConstraint).    
    abstract void visit(SizeConstraint node);

    /// Visits the given $(D StructParameter).    
    abstract void visit(StructParameter node);

    /// Visits the given $(D ArrayParameter).    
    abstract void visit(ArrayParameter node);

    /// Visits the given $(D ArrayRange).    
    abstract void visit(ArrayRange node);

}

/// Represents an abstract syntax node.
abstract class SyntaxNode
{
    /// Gets the $(D SyntaxKind) for this type of node.
    abstract SyntaxKind syntaxKind() @property;

    /// Calls the $(S Visitor)'s analogous $(D Visitor.visit) method.
    abstract void visit(Visitor visitor);
}

/// Represents a module in the dclass system.
class Module : SyntaxNode
{
    /// The optional name of this module.
    string symbol;
    /// Get the import statements in this module.
    ImportDeclaration[] importDeclarations;

    /// The aliases defined in this module.
    AliasDeclaration[] aliases;

    /// The classes defined in this module.
    ClassDeclaration[] classes;

    /// The structs defined in this module.
    StructDeclaration[] structs;

    /// The keywords defined in this module.
    KeywordDeclaration[] keywords;

    /// The id of the last type defined in this module.
    ushort lastTypeId;

    /// The id of the last field defined in this module.
    ushort lastFieldId;

    mixin(generateSyntaxNode);

    /// Gets an array of `StructDeclaration` and `ClassDeclaration` objects
    /// sorted by their id.
    TypeDeclaration[] typesById() pure @safe nothrow
    {
        import std.array : array;
        import std.conv : to;
        import std.range : chain;

        // dfmt off
        auto result = chain(classes, structs)
                      .array // BAD: for some reason, we need this call to `array`...
                      .sort!"a.id < b.id"
                      .array;
        // dfmt on

        return result;
    }

    /// Finds a `ClassDeclaration` or a `StructDeclaration` with the given name.
    TypeDeclaration findType(in string name) pure @safe nothrow
    {
        auto candidate = typesById.chain(aliases).filter!(x => x.symbol == name).takeOne;
        if (candidate.empty)
        {
            return null;
        }
        return candidate.front;
    }

    TypeDeclaration findType(in int id) pure @safe nothrow
    {
        auto candidate = typesById.chain(aliases).filter!(x => x.id == id).takeOne;
        if (candidate.empty)
        {
            return null;
        }
        return candidate.front;
    }
}

/// Represents an import statement.
class ImportDeclaration : SyntaxNode
{
    /// The package from which to import.
    string packageName;

    /// The symbols to import.
    string[] symbols;

    mixin(generateSyntaxNode);
}

/// Base class for module-level type declarations.
/// A type is a keyword declaration, a class declaration,
/// a struct declaration, or an alias/typedef declaration.
abstract class TypeDeclaration : SyntaxNode
{
    /// Represents a type of TypeDeclaration.
    enum Kind
    {
        keywordDeclaration,
        classDeclaration,
        structDeclaration,
        aliasDeclaration,
        builtin,
    }

    /// The id of this Type.
    int id;

    /// The symbol name of this type.
    string symbol;

    /// Gets the kind of TypeDeclaration this object represents.
    abstract Kind kind();

    mixin(GenerateThis);

    /// Gets the `Type` associated with this `TypeDeclaration`.
    abstract inout(Type) type() inout @safe pure nothrow @property;

    /// Gets all fields declared on this type.
    abstract inout(FieldDeclaration[]) getFields() inout @safe pure nothrow @property;

    /// Find a field by its id.
    final FieldDeclaration getField(int id)
    {
        return getFields.filter!(x => x.id == id).takeOne.front;
    }

    /// Find a field with its name.
    final FieldDeclaration getField(string symbol)
    {
        return getFields.filter!(x => x.name == symbol).takeOne.front;
    }
}

/// Represents a fundamental type.
class BuiltinType : TypeDeclaration
{
    /// The type of this declaration.
    Type _type;

    /// Ditto
    override TypeDeclaration.Kind kind()
    {
        return TypeDeclaration.Kind.builtin;
    }

    /// Ditto
    override inout(Type) type() inout @safe pure nothrow @property
    {
        return _type;
    }

    /// Ditto.
    override inout(FieldDeclaration[]) getFields() inout @safe pure nothrow @property
    {
        return [];
    }

    mixin(generateSyntaxNode);
}

/// Represents a list of keywords attached to a field.
class KeywordList : SyntaxNode
{
    /// The list of keywords.
    string[] keywords;

    alias keywords this;

    mixin(generateSyntaxNode);
}

/// Represents the value of a `keyword` statement.
class KeywordDeclaration : TypeDeclaration
{
    /// Ditto
    override TypeDeclaration.Kind kind()
    {
        return TypeDeclaration.Kind.keywordDeclaration;
    }

    mixin(generateSyntaxNode);

    override inout(FieldDeclaration[]) getFields() inout @safe pure nothrow @property
    {
        return [];
    }

    /// Ditto
    override inout(Type) type() inout @safe pure nothrow @property
    {
        return Type.invalid;
    }
}

/// Represents a struct declaration.
class StructDeclaration : TypeDeclaration
{
    override TypeDeclaration.Kind kind()
    {
        return TypeDeclaration.Kind.structDeclaration;
    }

    /// The atomic parts of this struct.
    ParameterField[] parameters;

    mixin(generateSyntaxNode);

    /// Ditto
    override inout(Type) type() inout @safe pure nothrow @property
    {
        return Type.struct_;
    }

    override inout(FieldDeclaration[]) getFields() inout @safe pure nothrow @property
    {
        return (() @trusted => cast(FieldDeclaration[]) parameters)();
    }
}

/// Represents a dclass declaration.
class ClassDeclaration : TypeDeclaration
{
    override TypeDeclaration.Kind kind()
    {
        return TypeDeclaration.Kind.classDeclaration;
    }

    /// The superclasses for this type.
    @(This.Exclude)
    ClassDeclaration[] superclasses;

    string[] superclassNames;

    /// Whether this $(D ClassDeclaration) should be generated as an
    /// `interface`.
    bool isInterface;

    /// The constructor for this type.
    FieldDeclaration constructor;

    /// The fields and RPC methods defined on this class.
    FieldDeclaration[] fields;

    mixin(generateSyntaxNode);

    /// Ditto
    override inout(Type) type() inout @safe pure nothrow @property
    {
        return Type.struct_;
    }

    /// Resolves superclasses.
    void resolve(Module file) @safe nothrow
    {
        if (hasSuperclass())
        {
            foreach (superclass; superclassNames)
            {
                superclasses ~= (() @trusted => cast(ClassDeclaration) file.findType(superclass))();
            }
        }
    }

    /// Checks whether this ClassDeclaration has a super class.
    bool hasSuperclass() pure nothrow inout @nogc @safe @property
    {
        return superclassNames.length > 0;
    }

    /// Get the direct parents of this dclass.
    ClassDeclaration[] parents() pure @safe @property
    {
        return superclasses;
    }

    /// Gets whether this class has a constructor.
    bool hasConstructor() pure @nogc @property
    {
        return constructor !is null;
    }

    /// Ditto.
    override inout(FieldDeclaration[]) getFields() inout @safe pure nothrow @property
    {
        return fields;
    }
}

/// Represents a typedef statement.
class AliasDeclaration : TypeDeclaration
{
    override TypeDeclaration.Kind kind()
    {
        return TypeDeclaration.Kind.aliasDeclaration;
    }

    /// The type to substitute the symbol for.
    /// For example, `typedef doId uint32` would
    /// set the aliasedTypeName to uint32.
    string aliasedTypeName;

    @(This.Exclude)
    TypeDeclaration aliasedType;

    mixin(generateSyntaxNode);

    /// Ditto
    override inout(Type) type() inout @safe pure nothrow @property
    {
        enum types = genbuiltins;

        if (auto ret = aliasedTypeName in types)
        {
            return *ret;
        }

        return Type.invalid;
    }

    /// Ditto.
    override inout(FieldDeclaration[]) getFields() inout @safe pure nothrow @property
    {
        return [];
    }
}

/// Abstract class representing a field.
abstract class FieldDeclaration : SyntaxNode
{
    /// The id of this field.
    int id;
    mixin(GenerateThis);

    /// Gets the symbol name of this field.
    abstract string name() @property;

    abstract string[] attributes() @property;

    final bool hasKeyword(string keyword)
    {
        return attributes.canFind(keyword);
    }
}

/// Represents a field which swizzles atomic fields.
class MolecularField : FieldDeclaration
{
    /// The name of this field.
    string symbol;

    /// The atomic fields which make up this field.
    @(This.Exclude)
    FieldDeclaration[] references;

    /// The names of the references. Used for resolution.
    string[] referenceNames;

    mixin(generateSyntaxNode);

    /// Ditto
    override string name() @property
    {
        return symbol;
    }

    override string[] attributes() @property
    {
        return [];
    }
}

/// Represents a method.
class AtomicField : FieldDeclaration
{
    /// The name of this field.
    string symbol;

    /// The parameters for this field.
    Parameter[] parameters;

    /// The keywords applied to this field.
    KeywordList keywords;
    mixin(generateSyntaxNode);

    /// Get the fundamental type of this field.
    /// Returns: `Type.method`.
    Type type()
    {
        return Type.method;
    }

    /// Ditto
    override string name() @property
    {
        return symbol;
    }

    override string[] attributes() @property
    {
        return keywords;
    }
}

/// Represents a plain field.
class ParameterField : FieldDeclaration
{
    /// Get the parameter for this field.
    Parameter parameter;

    /// Get the keywords on this field.
    KeywordList keywords;
    mixin(generateSyntaxNode);

    /// Ditto
    override string name() @property
    {
        return parameter.symbol;
    }

    override string[] attributes() @property
    {
        return keywords;
    }
}

/// Abstract class representing a parameter.
class Parameter : SyntaxNode
{
    /// Get the name of this parameter.
    string symbol;

    /// Get the fundamental type of this parameter.
    abstract Type parameterType() @property;
    abstract string parameterTypeName() @property;

    mixin(GenerateThis);
}

/// Represents a parameter of a numeric type.
class NumericParameter : Parameter
{
    private @(This.Exclude)
    {
        double _origMod;
    }

    /**
     * The type of this parameter.
     * 
     * Remarks:
     * May be `char`, `int8`, `int16`, `int32`,
     * `int64`, `uint8`, `uint16`, `uint32`,
     * `uint64`, `float32`, or `float64`.
     */
    string type;

    /// The divisor 
    uint divisor = 1;

    private double _modulus;

    /// The possible range of this parameter.
    /// This may be null.
    NumericRange range;

    version (none)
    {
        // Represents any transforms present on this parameter.
        // This may be null.    
        NumericTransform transform;
    }

    /// The default value of this parameter.
    /// This may be null.    
    NumericConstant defaultValue;

    mixin(generateSyntaxNode);

    bool hasRange()
    {
        import std.math : isNaN;

        return range !is null;
    }

    double modulus() @property
    {
        return _modulus;
    }

    bool modulus(double value) @property
    {
        import std.math : floor;

        if (value == double.nan)
        {
            _modulus = value;
            return true;
        }

        if (modulus <= 0.0)
        {
            return false;
        }

        auto floatModulus = value * divisor;
        auto uintModulus = cast(uint) floor(value * divisor + 0.5);

        switch (parameterType) with (Type)
        {
        case int8:
            if (uintModulus < 1 || cast(ushort)(ubyte.max + 1 < uintModulus))
            {
                return false;
            }
            _modulus = uintModulus;
            break;
        case int16:
            if (uintModulus < 1 || cast(uint)(ushort.max + 1 < uintModulus))
            {
                return false;
            }
            _modulus = uintModulus;
            break;
        case int32:
            if (uintModulus < 1 || cast(ulong)(uint.max + 1 < uintModulus))
            {
                return false;
            }
            _modulus = uintModulus;
            break;
        case int64:
            if (uintModulus < 1)
            {
                return false;
            }
            _modulus = uintModulus;
            break;
        case uint8:
            if (uintModulus < 1 || cast(ushort)(ubyte.max + 1 < uintModulus))
            {
                return false;
            }
            _modulus = uintModulus;
            break;
        case uint16:
            if (uintModulus < 1 || cast(uint)(ushort.max + 1 < uintModulus))
            {
                return false;
            }
            _modulus = uintModulus;
            break;
        case uint32:
            if (uintModulus < 1 || cast(ulong)(uint.max + 1 < uintModulus))
            {
                return false;
            }
            _modulus = uintModulus;
            break;
        case uint64:
            if (uintModulus < 1)
            {
                return false;
            }
            _modulus = uintModulus;
            break;
        case float32:
        case float64:
            _modulus = floatModulus;
            break;
        default:
            assert(0);
        }
        _origMod = value;
        return true;
    }

    bool hasModulus()
    {
        import std.math : isNaN;

        return !isNaN(_modulus);
    }

    override Type parameterType() @property
    {
        switch (type)
        {
        case "char":
            return Type.char_;
        case "int8":
            return Type.int8;
        case "int16":
            return Type.int16;
        case "int32":
            return Type.int32;
        case "int64":
            return Type.int64;
        case "uint8":
            return Type.uint8;
        case "uint16":
            return Type.uint16;
        case "uint32":
            return Type.uint32;
        case "uint64":
            return Type.uint64;
        case "float32":
            return Type.float32;
        case "float64":
            return Type.float64;
        default:
            assert(0, "Invalid type " ~ type);
        }
    }

    override string parameterTypeName() @property
    {
        return type;
    }
}

/// Represents the minimum and maximum value of a NumericParameter.
class NumericRange : SyntaxNode
{
    /// The minimum value of this range.
    double min;

    /// The maximum value of this range.
    double max;

    mixin(generateSyntaxNode);
}

/// Represents a transformation on a NumericParameter or IntConstant.
class NumericTransform : SyntaxNode
{
    /// The operator to apply.
    Operator operator;

    /// The value to apply.
    double value;

    /// The next transformation.
    /// This may be null.    
    NumericTransform next;

    /// Instantiates an IntTransform.
    this(Operator op, double value, NumericTransform next)
    {
        operator = op;
        this.value = value;
        this.next = next;
    }

    mixin(generateVisit);
}

/// Represents an integer constant.
class NumericConstant : SyntaxNode
{
    /// The value of this constant.
    double value;

    /// Any transformations which need to be applied.
    NumericTransform transform;

    mixin(generateSyntaxNode);
}

/// Represents a blob or string parameter.
class SizedParameter : Parameter
{
    /// The type of this parameter.
    string type;

    /// The size constraints of this parameter.
    /// If the minimum value equals the maximum value,
    /// This is a fixed-sized parameter.  Otherwise,
    /// it's a variable-sized parameter.
    SizeConstraint size;

    /// The default value of this parameter.
    /// This may be null.  This is only valid
    /// on string-type parameters.
    string defaultVal;

    mixin(generateSyntaxNode);

    bool hasRange()
    {
        // dfmt off
        return size !is null 
            && size.maxSize != 0;
        // dfmt on
    }

    override Type parameterType() @property
    {
        if (hasRange && size.isFixedLength)
        {
            if (type == "string")
            {
                return Type.string_;
            }
            else if (type == "blob")
            {
                return Type.blob;
            }
        }
        else
        {
            if (type == "string")
            {
                return Type.varstring;
            }
            else if (type == "blob")
            {
                return Type.varblob;
            }
        }
        assert(0);
    }

    override string parameterTypeName() @property
    {
        return type;
    }
}

/// Represents the constraints on a SizedParameter.
class SizeConstraint : SyntaxNode
{
    /// The minimum size of this parameter.
    int minSize;

    /// The maximum size of this parameter.    
    int maxSize;

    mixin(generateSyntaxNode);

    /// Gets whether this parameter is fixed-length.
    bool isFixedLength() pure @property
    {
        return minSize == maxSize && maxSize > 1;
    }
}

class StructParameter : Parameter
{
    string typeName;

    @(This.Exclude)
    TypeDeclaration type;

    mixin(generateSyntaxNode);

    TypeDeclaration resolveType(Module mod)
    {
        return mod.findType(typeName);
    }

    override Type parameterType() @property
    {
        return type.type;
    }

    override string parameterTypeName() @property
    {
        return typeName;
    }
}

class ArrayParameter : Parameter
{
    string type;
    ArrayRange size;

    @(This.Exclude)
    TypeDeclaration elementType;

    @(This.Exclude)
    Parameter element;

    mixin(generateSyntaxNode);

    TypeDeclaration resolveType(Module mod)
    {
        return mod.findType(type);
    }

    bool hasRange()
    {
        // dfmt off
        return size !is null 
            && size.maxLength != 0;
        // dfmt on
    }

    override Type parameterType() @property
    {
        if (size.isFixedLength)
        {
            return Type.array;
        }
        else
        {
            return Type.vararray;
        }
    }

    override string parameterTypeName() @property
    {
        return type;
    }

}

class ArrayRange : SyntaxNode
{
    int minLength;
    int maxLength;

    mixin(generateSyntaxNode);

    bool isFixedLength() pure @property
    {
        return minLength == maxLength && maxLength > 1;
    }
}
