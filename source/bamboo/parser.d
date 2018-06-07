module bamboo.parser;

import pegged.grammar;
import pegged.tohtml;

mixin(grammar(`
DClass:
    Module < DCFile

    ######################
    ## Parser Utilities ##
    ######################

    List(Elem, Sep) <  Elem (Sep Elem)*

    ######################
    ## Lexical Elements ##
    ######################

    Spacing <- :(' ' / '\t' / '\r' / '\n' / '\r\n' / comment)*

    # Letters and Digits
    letter <- [A-Za-z]
    decDigit <- [0-9]
    octDigit <- [0-7]
    hexDigit <- [0-9A-Fa-f]
    binDigit <- "0" / "1"

    #Operators
    operator < "%" / "*" / "+" / "-" / "/"

    #Delimiters
    delimiter < "(" / ")" / "{" / "}" / "[" / "]" / "," / ";" / "=" / ":" / operator

    #Number Literals
    numLiteral < intLiteral / floatLiteral

    #Integers
    intLiteral <- 
        / ;decLiteral
        / ;octLiteral
        / ;hexLiteral
        / ;binLiteral

    decLiteral <~ ([1-9] decDigit*) / "0"
    octLiteral <~ ^"0" octDigit*
    hexLiteral <~ ^"0" [Xx] hexDigit*
    binLiteral <~ ^"0" [Bb] binDigit*

    #Floats
    floatLiteral <~ (decimals / (decimals "." decimals) / ("." decimals)) "f"?
    decimals <~ decDigit+

    #Text Literals
    charLiteral <- :quote (escapeSequence / nonSingleQuote) :quote
    stringLiteral <- :doublequote stringCharacters :doublequote
    stringCharacters <~ (escapeSequence / nonDoubleQuote)*
    nonSingleQuote <- (!quote .)*
    nonDoubleQuote <- (!doublequote .)*
    escapeSequence <- ^"\\" (~("x" hexDigit+) / .)

    # Identifiers
    Identifier < ;identifier

    #Keywords
    keyword < "dclass" / "struct" / "keyword"

    #Data Types
    dataType < ;charType / ;intType / ;floatType / ;sizedType
    charType <- ;"char"
    intType <- 
            / ;"int8" 
            / ;"int16"
            / ;"int32"
            / ;"int64"
            / ;"uint8"
            / ;"uint16"
            / ;"uint32"
            / ;"uint64"
    floatType <- ;"float64"
    sizedType <- ;"string" / ;"blob"

    # Comments
    comment <:  lineComment / blockComment 
    lineComment <: (("//" !'#') (!endOfLine .)* endOfLine)
    blockComment <: ("/*" (blockComment / (!("/*"/"*/") .))* "*/")

    #############
    ## Grammar ##
    #############

    # DC File

    DCFile < (ImportDecl / ParseDirective / TypeDecl / :comment)+ eoi

    # Special parse directives: e.g:
    # //# typeid = 01
    ParseDirective < "//#" Identifier "=" (QualifiedIdentifier / numLiteral) ";"

    ImportDecl < "from" QualifiedIdentifier "import" ImportList
    QualifiedIdentifier <~ Identifier ("." Identifier)*
    ImportList < Identifier ("/" Identifier)*

    TypeDecl < (KeywordType / StructType / ClassType / AliasType) ";"

    # Alias Types
    AliasType < "typedef" (dataType / Identifier) Identifier

    # Keywords
    KeywordType < "keyword" Identifier
    KeywordList < Identifier+

    # Struct Type
    StructType < "struct" Identifier "{" (Parameter ";")+ "}"

    # Class Type
    ClassType < "dclass" Identifier (":" Identifier)? "{" (FieldDecl ";")+ "}"

    # Field Types
    FieldDecl < (MolecularField / AtomicField / ParameterField)

    MolecularField < Identifier ":" MolecularFieldMembers
    AtomicField < Identifier "(" ParameterList? ")" KeywordList?
    ParameterField < Parameter KeywordList?

    MolecularFieldMembers < (Identifier) ("," (Identifier))*

    # Parameter Types
    ParameterList < Parameter ("," Parameter)*
    Parameter <
              / ArrayParameter
              / CharParameter 
              / IntParameter 
              / FloatParameter 
              / SizedParameter 
              / StructParameter 
    
    # Char Parameter
    CharParameter < :charType Identifier? ("=" charLiteral)?

    # Integer Parameter
    IntParameter < intType IntRange? IntTransform? Identifier? ("=" IntConstant)?
    IntConstant < intLiteral / "{" intLiteral IntTransform "}"
    IntTransform < operator intLiteral (( "(" IntTransform ")" ) / IntTransform)*
    IntRange < "(" intLiteral "-" intLiteral ")"

    # Float Parameter
    FloatParameter < floatType FloatRange? FloatTransform? Identifier? ("=" FloatConstant)?
    FloatConstant < floatLiteral / ( "{" floatLiteral FloatTransform "}" )
    FloatTransform < operator floatLiteral (FloatTransform / ( "(" FloatTransform ")" ))*
    FloatRange < "(" floatLiteral "-" floatLiteral ")"

    # Sized Parameter
    SizedParameter < sizedType SizeConstraint? Identifier? ("=" stringLiteral)?
    SizeConstraint < "(" intLiteral ("-" intLiteral)? ")"

    # Struct Parameter
    StructParameter < Identifier Identifier?

    # Array Parameter
    ArrayParameter < (dataType / Identifier) Identifier? ArrayRange
    ArrayRange < 
        / "[" "]"
        / "[" intLiteral ("-" intLiteral)? "]"
`));

unittest
{
    import std.file;

    assert(identifier("simple_example").successful);

    assert(DClass.AtomicField("db_complex(Block named[], Block[3]) db;").successful);

    assert(DClass.floatLiteral("0").successful);
    assert(DClass.floatLiteral("1").successful);
    assert(DClass.floatLiteral("10.5").successful);
    assert(DClass.floatLiteral(".2").successful);
    assert(DClass.FloatParameter("float64(0-10) / 100 optional = 5").successful);
    assert(DClass.Parameter(`char a = 'a'`).successful);
    assert(DClass.Parameter(`int32(0-10) / 100 optional = 5`).successful);
    assert(DClass.Parameter(`float64(0-10) / 100 optional = 5`).successful);
    assert(DClass.Parameter(`string(0-10) optional = "default"`).successful);
    assert(DClass.Parameter(`Location loc`).successful);
    assert(DClass.Parameter(`int32 optional[1-10]`).successful);

    assert(DClass.FieldDecl(`test(char a = 'a',
         int32(0-10) / 100 optional = 5,
         float64(0-10) / 100 optional = 5,
         string(0-10) optional = "default",
         Location loc,
         int32 optional[1-10]
    ) airecv`).successful);

    assert(DClass.ClassType(`dclass DistributedTestObject {
        test(char a = 'a',
            int32(0-10) / 100 optional = 5,
            float64(0-10) / 100 optional = 5,
            string(0-10) optional = "default",
            Location loc,
            int32 optional[1-10]
        ) airecv;
    };`).successful);

    ParseTree simpleExample = DClass(readText("tests/simple_example.dc"));

    assert(simpleExample.successful);

    auto astronTest = DClass(readText("tests/astron_example.dc"));
    assert(astronTest.successful);

    ParseTree example = DClass(readText("tests/example.dc"));

    assert(example.successful);
}
