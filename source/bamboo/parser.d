/++
This module was automatically generated from the following grammar:

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

+/
module bamboo.parser;

public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

struct GenericDClass(TParseTree)
{
    import std.functional : toDelegate;
    import pegged.dynamic.grammar;
    static import pegged.peg;
    struct DClass
    {
    enum name = "DClass";
    static ParseTree delegate(ParseTree)[string] before;
    static ParseTree delegate(ParseTree)[string] after;
    static ParseTree delegate(ParseTree)[string] rules;
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static this()
    {
        rules["Module"] = toDelegate(&Module);
        rules["Spacing"] = toDelegate(&Spacing);
    }

    template hooked(alias r, string name)
    {
        static ParseTree hooked(ParseTree p)
        {
            ParseTree result;

            if (name in before)
            {
                result = before[name](p);
                if (result.successful)
                    return result;
            }

            result = r(p);
            if (result.successful || name !in after)
                return result;

            result = after[name](p);
            return result;
        }

        static ParseTree hooked(string input)
        {
            return hooked!(r, name)(ParseTree("",false,[],input));
        }
    }

    static void addRuleBefore(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar name
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(ruleName,rule; dg.rules)
            if (ruleName != "Spacing") // Keep the local Spacing rule, do not overwrite it
                rules[ruleName] = rule;
        before[parentRule] = rules[dg.startingRule];
    }

    static void addRuleAfter(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar named
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(name,rule; dg.rules)
        {
            if (name != "Spacing")
                rules[name] = rule;
        }
        after[parentRule] = rules[dg.startingRule];
    }

    static bool isRule(string s)
    {
		import std.algorithm : startsWith;
        return s.startsWith("DClass.");
    }
    mixin decimateTree;

    static TParseTree Module(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, DCFile, Spacing), "DClass.Module")(p);
        }
        else
        {
            if (auto m = tuple(`Module`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, DCFile, Spacing), "DClass.Module"), "Module")(p);
                memo[tuple(`Module`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Module(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, DCFile, Spacing), "DClass.Module")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, DCFile, Spacing), "DClass.Module"), "Module")(TParseTree("", false,[], s));
        }
    }
    static string Module(GetName g)
    {
        return "DClass.Module";
    }

    template List(alias Elem, alias Sep)
    {
    static TParseTree List(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Elem, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Sep, Spacing), pegged.peg.wrapAround!(Spacing, Elem, Spacing)), Spacing))), "DClass.List!(" ~ pegged.peg.getName!(Elem)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("List!(" ~ pegged.peg.getName!(Elem)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Elem, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Sep, Spacing), pegged.peg.wrapAround!(Spacing, Elem, Spacing)), Spacing))), "DClass.List!(" ~ pegged.peg.getName!(Elem)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")"), "List_2")(p);
                memo[tuple("List!(" ~ pegged.peg.getName!(Elem)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree List(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Elem, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Sep, Spacing), pegged.peg.wrapAround!(Spacing, Elem, Spacing)), Spacing))), "DClass.List!(" ~ pegged.peg.getName!(Elem)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Elem, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Sep, Spacing), pegged.peg.wrapAround!(Spacing, Elem, Spacing)), Spacing))), "DClass.List!(" ~ pegged.peg.getName!(Elem)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")"), "List_2")(TParseTree("", false,[], s));
        }
    }
    static string List(GetName g)
    {
        return "DClass.List!(" ~ pegged.peg.getName!(Elem)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")";
    }

    }
    static TParseTree Spacing(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), pegged.peg.literal!("\r"), pegged.peg.literal!("\n"), pegged.peg.literal!("\r\n"), comment))), "DClass.Spacing")(p);
        }
        else
        {
            if (auto m = tuple(`Spacing`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), pegged.peg.literal!("\r"), pegged.peg.literal!("\n"), pegged.peg.literal!("\r\n"), comment))), "DClass.Spacing"), "Spacing")(p);
                memo[tuple(`Spacing`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Spacing(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), pegged.peg.literal!("\r"), pegged.peg.literal!("\n"), pegged.peg.literal!("\r\n"), comment))), "DClass.Spacing")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.literal!(" "), pegged.peg.literal!("\t"), pegged.peg.literal!("\r"), pegged.peg.literal!("\n"), pegged.peg.literal!("\r\n"), comment))), "DClass.Spacing"), "Spacing")(TParseTree("", false,[], s));
        }
    }
    static string Spacing(GetName g)
    {
        return "DClass.Spacing";
    }

    static TParseTree letter(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z')), "DClass.letter")(p);
        }
        else
        {
            if (auto m = tuple(`letter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z')), "DClass.letter"), "letter")(p);
                memo[tuple(`letter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree letter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z')), "DClass.letter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('a', 'z')), "DClass.letter"), "letter")(TParseTree("", false,[], s));
        }
    }
    static string letter(GetName g)
    {
        return "DClass.letter";
    }

    static TParseTree decDigit(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "DClass.decDigit")(p);
        }
        else
        {
            if (auto m = tuple(`decDigit`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "DClass.decDigit"), "decDigit")(p);
                memo[tuple(`decDigit`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree decDigit(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "DClass.decDigit")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "DClass.decDigit"), "decDigit")(TParseTree("", false,[], s));
        }
    }
    static string decDigit(GetName g)
    {
        return "DClass.decDigit";
    }

    static TParseTree octDigit(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.charRange!('0', '7'), "DClass.octDigit")(p);
        }
        else
        {
            if (auto m = tuple(`octDigit`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.charRange!('0', '7'), "DClass.octDigit"), "octDigit")(p);
                memo[tuple(`octDigit`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree octDigit(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.charRange!('0', '7'), "DClass.octDigit")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.charRange!('0', '7'), "DClass.octDigit"), "octDigit")(TParseTree("", false,[], s));
        }
    }
    static string octDigit(GetName g)
    {
        return "DClass.octDigit";
    }

    static TParseTree hexDigit(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'F'), pegged.peg.charRange!('a', 'f')), "DClass.hexDigit")(p);
        }
        else
        {
            if (auto m = tuple(`hexDigit`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'F'), pegged.peg.charRange!('a', 'f')), "DClass.hexDigit"), "hexDigit")(p);
                memo[tuple(`hexDigit`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree hexDigit(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'F'), pegged.peg.charRange!('a', 'f')), "DClass.hexDigit")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('A', 'F'), pegged.peg.charRange!('a', 'f')), "DClass.hexDigit"), "hexDigit")(TParseTree("", false,[], s));
        }
    }
    static string hexDigit(GetName g)
    {
        return "DClass.hexDigit";
    }

    static TParseTree binDigit(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("0", "1"), "DClass.binDigit")(p);
        }
        else
        {
            if (auto m = tuple(`binDigit`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("0", "1"), "DClass.binDigit"), "binDigit")(p);
                memo[tuple(`binDigit`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree binDigit(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("0", "1"), "DClass.binDigit")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("0", "1"), "DClass.binDigit"), "binDigit")(TParseTree("", false,[], s));
        }
    }
    static string binDigit(GetName g)
    {
        return "DClass.binDigit";
    }

    static TParseTree operator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing)), "DClass.operator")(p);
        }
        else
        {
            if (auto m = tuple(`operator`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing)), "DClass.operator"), "operator")(p);
                memo[tuple(`operator`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree operator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing)), "DClass.operator")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("%"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("+"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing)), "DClass.operator"), "operator")(TParseTree("", false,[], s));
        }
    }
    static string operator(GetName g)
    {
        return "DClass.operator";
    }

    static TParseTree delimiter(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, operator, Spacing)), "DClass.delimiter")(p);
        }
        else
        {
            if (auto m = tuple(`delimiter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, operator, Spacing)), "DClass.delimiter"), "delimiter")(p);
                memo[tuple(`delimiter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree delimiter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, operator, Spacing)), "DClass.delimiter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, operator, Spacing)), "DClass.delimiter"), "delimiter")(TParseTree("", false,[], s));
        }
    }
    static string delimiter(GetName g)
    {
        return "DClass.delimiter";
    }

    static TParseTree numLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing)), "DClass.numLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`numLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing)), "DClass.numLiteral"), "numLiteral")(p);
                memo[tuple(`numLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree numLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing)), "DClass.numLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing)), "DClass.numLiteral"), "numLiteral")(TParseTree("", false,[], s));
        }
    }
    static string numLiteral(GetName g)
    {
        return "DClass.numLiteral";
    }

    static TParseTree intLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.drop!(decLiteral), pegged.peg.drop!(octLiteral), pegged.peg.drop!(hexLiteral), pegged.peg.drop!(binLiteral)), "DClass.intLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`intLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.drop!(decLiteral), pegged.peg.drop!(octLiteral), pegged.peg.drop!(hexLiteral), pegged.peg.drop!(binLiteral)), "DClass.intLiteral"), "intLiteral")(p);
                memo[tuple(`intLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree intLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.drop!(decLiteral), pegged.peg.drop!(octLiteral), pegged.peg.drop!(hexLiteral), pegged.peg.drop!(binLiteral)), "DClass.intLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.drop!(decLiteral), pegged.peg.drop!(octLiteral), pegged.peg.drop!(hexLiteral), pegged.peg.drop!(binLiteral)), "DClass.intLiteral"), "intLiteral")(TParseTree("", false,[], s));
        }
    }
    static string intLiteral(GetName g)
    {
        return "DClass.intLiteral";
    }

    static TParseTree decLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.charRange!('1', '9'), pegged.peg.zeroOrMore!(decDigit)), pegged.peg.literal!("0"))), "DClass.decLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`decLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.charRange!('1', '9'), pegged.peg.zeroOrMore!(decDigit)), pegged.peg.literal!("0"))), "DClass.decLiteral"), "decLiteral")(p);
                memo[tuple(`decLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree decLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.charRange!('1', '9'), pegged.peg.zeroOrMore!(decDigit)), pegged.peg.literal!("0"))), "DClass.decLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.charRange!('1', '9'), pegged.peg.zeroOrMore!(decDigit)), pegged.peg.literal!("0"))), "DClass.decLiteral"), "decLiteral")(TParseTree("", false,[], s));
        }
    }
    static string decLiteral(GetName g)
    {
        return "DClass.decLiteral";
    }

    static TParseTree octLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.literal!("0")), pegged.peg.zeroOrMore!(octDigit))), "DClass.octLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`octLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.literal!("0")), pegged.peg.zeroOrMore!(octDigit))), "DClass.octLiteral"), "octLiteral")(p);
                memo[tuple(`octLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree octLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.literal!("0")), pegged.peg.zeroOrMore!(octDigit))), "DClass.octLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.literal!("0")), pegged.peg.zeroOrMore!(octDigit))), "DClass.octLiteral"), "octLiteral")(TParseTree("", false,[], s));
        }
    }
    static string octLiteral(GetName g)
    {
        return "DClass.octLiteral";
    }

    static TParseTree hexLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.literal!("0")), pegged.peg.or!(pegged.peg.literal!("X"), pegged.peg.literal!("x")), pegged.peg.zeroOrMore!(hexDigit))), "DClass.hexLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`hexLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.literal!("0")), pegged.peg.or!(pegged.peg.literal!("X"), pegged.peg.literal!("x")), pegged.peg.zeroOrMore!(hexDigit))), "DClass.hexLiteral"), "hexLiteral")(p);
                memo[tuple(`hexLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree hexLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.literal!("0")), pegged.peg.or!(pegged.peg.literal!("X"), pegged.peg.literal!("x")), pegged.peg.zeroOrMore!(hexDigit))), "DClass.hexLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.literal!("0")), pegged.peg.or!(pegged.peg.literal!("X"), pegged.peg.literal!("x")), pegged.peg.zeroOrMore!(hexDigit))), "DClass.hexLiteral"), "hexLiteral")(TParseTree("", false,[], s));
        }
    }
    static string hexLiteral(GetName g)
    {
        return "DClass.hexLiteral";
    }

    static TParseTree binLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.literal!("0")), pegged.peg.or!(pegged.peg.literal!("B"), pegged.peg.literal!("b")), pegged.peg.zeroOrMore!(binDigit))), "DClass.binLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`binLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.literal!("0")), pegged.peg.or!(pegged.peg.literal!("B"), pegged.peg.literal!("b")), pegged.peg.zeroOrMore!(binDigit))), "DClass.binLiteral"), "binLiteral")(p);
                memo[tuple(`binLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree binLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.literal!("0")), pegged.peg.or!(pegged.peg.literal!("B"), pegged.peg.literal!("b")), pegged.peg.zeroOrMore!(binDigit))), "DClass.binLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.literal!("0")), pegged.peg.or!(pegged.peg.literal!("B"), pegged.peg.literal!("b")), pegged.peg.zeroOrMore!(binDigit))), "DClass.binLiteral"), "binLiteral")(TParseTree("", false,[], s));
        }
    }
    static string binLiteral(GetName g)
    {
        return "DClass.binLiteral";
    }

    static TParseTree floatLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.or!(decimals, pegged.peg.and!(decimals, pegged.peg.literal!("."), decimals), pegged.peg.and!(pegged.peg.literal!("."), decimals)), pegged.peg.option!(pegged.peg.literal!("f")))), "DClass.floatLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`floatLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.or!(decimals, pegged.peg.and!(decimals, pegged.peg.literal!("."), decimals), pegged.peg.and!(pegged.peg.literal!("."), decimals)), pegged.peg.option!(pegged.peg.literal!("f")))), "DClass.floatLiteral"), "floatLiteral")(p);
                memo[tuple(`floatLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree floatLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.or!(decimals, pegged.peg.and!(decimals, pegged.peg.literal!("."), decimals), pegged.peg.and!(pegged.peg.literal!("."), decimals)), pegged.peg.option!(pegged.peg.literal!("f")))), "DClass.floatLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.or!(decimals, pegged.peg.and!(decimals, pegged.peg.literal!("."), decimals), pegged.peg.and!(pegged.peg.literal!("."), decimals)), pegged.peg.option!(pegged.peg.literal!("f")))), "DClass.floatLiteral"), "floatLiteral")(TParseTree("", false,[], s));
        }
    }
    static string floatLiteral(GetName g)
    {
        return "DClass.floatLiteral";
    }

    static TParseTree decimals(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(decDigit)), "DClass.decimals")(p);
        }
        else
        {
            if (auto m = tuple(`decimals`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(decDigit)), "DClass.decimals"), "decimals")(p);
                memo[tuple(`decimals`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree decimals(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(decDigit)), "DClass.decimals")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(decDigit)), "DClass.decimals"), "decimals")(TParseTree("", false,[], s));
        }
    }
    static string decimals(GetName g)
    {
        return "DClass.decimals";
    }

    static TParseTree charLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(quote), pegged.peg.or!(escapeSequence, nonSingleQuote), pegged.peg.discard!(quote)), "DClass.charLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`charLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(quote), pegged.peg.or!(escapeSequence, nonSingleQuote), pegged.peg.discard!(quote)), "DClass.charLiteral"), "charLiteral")(p);
                memo[tuple(`charLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree charLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(quote), pegged.peg.or!(escapeSequence, nonSingleQuote), pegged.peg.discard!(quote)), "DClass.charLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(quote), pegged.peg.or!(escapeSequence, nonSingleQuote), pegged.peg.discard!(quote)), "DClass.charLiteral"), "charLiteral")(TParseTree("", false,[], s));
        }
    }
    static string charLiteral(GetName g)
    {
        return "DClass.charLiteral";
    }

    static TParseTree stringLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(doublequote), stringCharacters, pegged.peg.discard!(doublequote)), "DClass.stringLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`stringLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(doublequote), stringCharacters, pegged.peg.discard!(doublequote)), "DClass.stringLiteral"), "stringLiteral")(p);
                memo[tuple(`stringLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree stringLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(doublequote), stringCharacters, pegged.peg.discard!(doublequote)), "DClass.stringLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(doublequote), stringCharacters, pegged.peg.discard!(doublequote)), "DClass.stringLiteral"), "stringLiteral")(TParseTree("", false,[], s));
        }
    }
    static string stringLiteral(GetName g)
    {
        return "DClass.stringLiteral";
    }

    static TParseTree stringCharacters(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(escapeSequence, nonDoubleQuote))), "DClass.stringCharacters")(p);
        }
        else
        {
            if (auto m = tuple(`stringCharacters`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(escapeSequence, nonDoubleQuote))), "DClass.stringCharacters"), "stringCharacters")(p);
                memo[tuple(`stringCharacters`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree stringCharacters(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(escapeSequence, nonDoubleQuote))), "DClass.stringCharacters")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(escapeSequence, nonDoubleQuote))), "DClass.stringCharacters"), "stringCharacters")(TParseTree("", false,[], s));
        }
    }
    static string stringCharacters(GetName g)
    {
        return "DClass.stringCharacters";
    }

    static TParseTree nonSingleQuote(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), pegged.peg.any)), "DClass.nonSingleQuote")(p);
        }
        else
        {
            if (auto m = tuple(`nonSingleQuote`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), pegged.peg.any)), "DClass.nonSingleQuote"), "nonSingleQuote")(p);
                memo[tuple(`nonSingleQuote`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree nonSingleQuote(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), pegged.peg.any)), "DClass.nonSingleQuote")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(quote), pegged.peg.any)), "DClass.nonSingleQuote"), "nonSingleQuote")(TParseTree("", false,[], s));
        }
    }
    static string nonSingleQuote(GetName g)
    {
        return "DClass.nonSingleQuote";
    }

    static TParseTree nonDoubleQuote(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "DClass.nonDoubleQuote")(p);
        }
        else
        {
            if (auto m = tuple(`nonDoubleQuote`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "DClass.nonDoubleQuote"), "nonDoubleQuote")(p);
                memo[tuple(`nonDoubleQuote`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree nonDoubleQuote(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "DClass.nonDoubleQuote")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any)), "DClass.nonDoubleQuote"), "nonDoubleQuote")(TParseTree("", false,[], s));
        }
    }
    static string nonDoubleQuote(GetName g)
    {
        return "DClass.nonDoubleQuote";
    }

    static TParseTree escapeSequence(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.literal!("\\")), pegged.peg.or!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.oneOrMore!(hexDigit))), pegged.peg.any)), "DClass.escapeSequence")(p);
        }
        else
        {
            if (auto m = tuple(`escapeSequence`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.literal!("\\")), pegged.peg.or!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.oneOrMore!(hexDigit))), pegged.peg.any)), "DClass.escapeSequence"), "escapeSequence")(p);
                memo[tuple(`escapeSequence`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree escapeSequence(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.literal!("\\")), pegged.peg.or!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.oneOrMore!(hexDigit))), pegged.peg.any)), "DClass.escapeSequence")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keep!(pegged.peg.literal!("\\")), pegged.peg.or!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.oneOrMore!(hexDigit))), pegged.peg.any)), "DClass.escapeSequence"), "escapeSequence")(TParseTree("", false,[], s));
        }
    }
    static string escapeSequence(GetName g)
    {
        return "DClass.escapeSequence";
    }

    static TParseTree Identifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, identifier, Spacing)), "DClass.Identifier")(p);
        }
        else
        {
            if (auto m = tuple(`Identifier`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, identifier, Spacing)), "DClass.Identifier"), "Identifier")(p);
                memo[tuple(`Identifier`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Identifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, identifier, Spacing)), "DClass.Identifier")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, identifier, Spacing)), "DClass.Identifier"), "Identifier")(TParseTree("", false,[], s));
        }
    }
    static string Identifier(GetName g)
    {
        return "DClass.Identifier";
    }

    static TParseTree keyword(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dclass"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("keyword"), Spacing)), "DClass.keyword")(p);
        }
        else
        {
            if (auto m = tuple(`keyword`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dclass"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("keyword"), Spacing)), "DClass.keyword"), "keyword")(p);
                memo[tuple(`keyword`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree keyword(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dclass"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("keyword"), Spacing)), "DClass.keyword")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dclass"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("keyword"), Spacing)), "DClass.keyword"), "keyword")(TParseTree("", false,[], s));
        }
    }
    static string keyword(GetName g)
    {
        return "DClass.keyword";
    }

    static TParseTree dataType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, charType, Spacing)), pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, intType, Spacing)), pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, floatType, Spacing)), pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, sizedType, Spacing))), "DClass.dataType")(p);
        }
        else
        {
            if (auto m = tuple(`dataType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, charType, Spacing)), pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, intType, Spacing)), pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, floatType, Spacing)), pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, sizedType, Spacing))), "DClass.dataType"), "dataType")(p);
                memo[tuple(`dataType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree dataType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, charType, Spacing)), pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, intType, Spacing)), pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, floatType, Spacing)), pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, sizedType, Spacing))), "DClass.dataType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, charType, Spacing)), pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, intType, Spacing)), pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, floatType, Spacing)), pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, sizedType, Spacing))), "DClass.dataType"), "dataType")(TParseTree("", false,[], s));
        }
    }
    static string dataType(GetName g)
    {
        return "DClass.dataType";
    }

    static TParseTree charType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.drop!(pegged.peg.literal!("char")), "DClass.charType")(p);
        }
        else
        {
            if (auto m = tuple(`charType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.drop!(pegged.peg.literal!("char")), "DClass.charType"), "charType")(p);
                memo[tuple(`charType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree charType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.drop!(pegged.peg.literal!("char")), "DClass.charType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.drop!(pegged.peg.literal!("char")), "DClass.charType"), "charType")(TParseTree("", false,[], s));
        }
    }
    static string charType(GetName g)
    {
        return "DClass.charType";
    }

    static TParseTree intType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.drop!(pegged.peg.literal!("int8")), pegged.peg.drop!(pegged.peg.literal!("int16")), pegged.peg.drop!(pegged.peg.literal!("int32")), pegged.peg.drop!(pegged.peg.literal!("int64")), pegged.peg.drop!(pegged.peg.literal!("uint8")), pegged.peg.drop!(pegged.peg.literal!("uint16")), pegged.peg.drop!(pegged.peg.literal!("uint32")), pegged.peg.drop!(pegged.peg.literal!("uint64"))), "DClass.intType")(p);
        }
        else
        {
            if (auto m = tuple(`intType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.drop!(pegged.peg.literal!("int8")), pegged.peg.drop!(pegged.peg.literal!("int16")), pegged.peg.drop!(pegged.peg.literal!("int32")), pegged.peg.drop!(pegged.peg.literal!("int64")), pegged.peg.drop!(pegged.peg.literal!("uint8")), pegged.peg.drop!(pegged.peg.literal!("uint16")), pegged.peg.drop!(pegged.peg.literal!("uint32")), pegged.peg.drop!(pegged.peg.literal!("uint64"))), "DClass.intType"), "intType")(p);
                memo[tuple(`intType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree intType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.drop!(pegged.peg.literal!("int8")), pegged.peg.drop!(pegged.peg.literal!("int16")), pegged.peg.drop!(pegged.peg.literal!("int32")), pegged.peg.drop!(pegged.peg.literal!("int64")), pegged.peg.drop!(pegged.peg.literal!("uint8")), pegged.peg.drop!(pegged.peg.literal!("uint16")), pegged.peg.drop!(pegged.peg.literal!("uint32")), pegged.peg.drop!(pegged.peg.literal!("uint64"))), "DClass.intType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.drop!(pegged.peg.literal!("int8")), pegged.peg.drop!(pegged.peg.literal!("int16")), pegged.peg.drop!(pegged.peg.literal!("int32")), pegged.peg.drop!(pegged.peg.literal!("int64")), pegged.peg.drop!(pegged.peg.literal!("uint8")), pegged.peg.drop!(pegged.peg.literal!("uint16")), pegged.peg.drop!(pegged.peg.literal!("uint32")), pegged.peg.drop!(pegged.peg.literal!("uint64"))), "DClass.intType"), "intType")(TParseTree("", false,[], s));
        }
    }
    static string intType(GetName g)
    {
        return "DClass.intType";
    }

    static TParseTree floatType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.drop!(pegged.peg.literal!("float64")), "DClass.floatType")(p);
        }
        else
        {
            if (auto m = tuple(`floatType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.drop!(pegged.peg.literal!("float64")), "DClass.floatType"), "floatType")(p);
                memo[tuple(`floatType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree floatType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.drop!(pegged.peg.literal!("float64")), "DClass.floatType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.drop!(pegged.peg.literal!("float64")), "DClass.floatType"), "floatType")(TParseTree("", false,[], s));
        }
    }
    static string floatType(GetName g)
    {
        return "DClass.floatType";
    }

    static TParseTree sizedType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.drop!(pegged.peg.literal!("string")), pegged.peg.drop!(pegged.peg.literal!("blob"))), "DClass.sizedType")(p);
        }
        else
        {
            if (auto m = tuple(`sizedType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.drop!(pegged.peg.literal!("string")), pegged.peg.drop!(pegged.peg.literal!("blob"))), "DClass.sizedType"), "sizedType")(p);
                memo[tuple(`sizedType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree sizedType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.drop!(pegged.peg.literal!("string")), pegged.peg.drop!(pegged.peg.literal!("blob"))), "DClass.sizedType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.drop!(pegged.peg.literal!("string")), pegged.peg.drop!(pegged.peg.literal!("blob"))), "DClass.sizedType"), "sizedType")(TParseTree("", false,[], s));
        }
    }
    static string sizedType(GetName g)
    {
        return "DClass.sizedType";
    }

    static TParseTree comment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.or!(lineComment, blockComment)), "DClass.comment")(p);
        }
        else
        {
            if (auto m = tuple(`comment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.or!(lineComment, blockComment)), "DClass.comment"), "comment")(p);
                memo[tuple(`comment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree comment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.or!(lineComment, blockComment)), "DClass.comment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.or!(lineComment, blockComment)), "DClass.comment"), "comment")(TParseTree("", false,[], s));
        }
    }
    static string comment(GetName g)
    {
        return "DClass.comment";
    }

    static TParseTree lineComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.negLookahead!(pegged.peg.literal!("#"))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine)), "DClass.lineComment")(p);
        }
        else
        {
            if (auto m = tuple(`lineComment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.negLookahead!(pegged.peg.literal!("#"))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine)), "DClass.lineComment"), "lineComment")(p);
                memo[tuple(`lineComment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree lineComment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.negLookahead!(pegged.peg.literal!("#"))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine)), "DClass.lineComment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.negLookahead!(pegged.peg.literal!("#"))), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine)), "DClass.lineComment"), "lineComment")(TParseTree("", false,[], s));
        }
    }
    static string lineComment(GetName g)
    {
        return "DClass.lineComment";
    }

    static TParseTree blockComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.or!(blockComment, pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/*", "*/")), pegged.peg.any))), pegged.peg.literal!("*/"))), "DClass.blockComment")(p);
        }
        else
        {
            if (auto m = tuple(`blockComment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.or!(blockComment, pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/*", "*/")), pegged.peg.any))), pegged.peg.literal!("*/"))), "DClass.blockComment"), "blockComment")(p);
                memo[tuple(`blockComment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree blockComment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.or!(blockComment, pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/*", "*/")), pegged.peg.any))), pegged.peg.literal!("*/"))), "DClass.blockComment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.or!(blockComment, pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("/*", "*/")), pegged.peg.any))), pegged.peg.literal!("*/"))), "DClass.blockComment"), "blockComment")(TParseTree("", false,[], s));
        }
    }
    static string blockComment(GetName g)
    {
        return "DClass.blockComment";
    }

    static TParseTree DCFile(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ImportDecl, Spacing), pegged.peg.wrapAround!(Spacing, ParseDirective, Spacing), pegged.peg.wrapAround!(Spacing, TypeDecl, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, comment, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "DClass.DCFile")(p);
        }
        else
        {
            if (auto m = tuple(`DCFile`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ImportDecl, Spacing), pegged.peg.wrapAround!(Spacing, ParseDirective, Spacing), pegged.peg.wrapAround!(Spacing, TypeDecl, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, comment, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "DClass.DCFile"), "DCFile")(p);
                memo[tuple(`DCFile`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DCFile(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ImportDecl, Spacing), pegged.peg.wrapAround!(Spacing, ParseDirective, Spacing), pegged.peg.wrapAround!(Spacing, TypeDecl, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, comment, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "DClass.DCFile")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ImportDecl, Spacing), pegged.peg.wrapAround!(Spacing, ParseDirective, Spacing), pegged.peg.wrapAround!(Spacing, TypeDecl, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, comment, Spacing))), Spacing)), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "DClass.DCFile"), "DCFile")(TParseTree("", false,[], s));
        }
    }
    static string DCFile(GetName g)
    {
        return "DClass.DCFile";
    }

    static TParseTree ParseDirective(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("//#"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, QualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, numLiteral, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "DClass.ParseDirective")(p);
        }
        else
        {
            if (auto m = tuple(`ParseDirective`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("//#"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, QualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, numLiteral, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "DClass.ParseDirective"), "ParseDirective")(p);
                memo[tuple(`ParseDirective`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ParseDirective(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("//#"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, QualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, numLiteral, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "DClass.ParseDirective")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("//#"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, QualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, numLiteral, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "DClass.ParseDirective"), "ParseDirective")(TParseTree("", false,[], s));
        }
    }
    static string ParseDirective(GetName g)
    {
        return "DClass.ParseDirective";
    }

    static TParseTree ImportDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("from"), Spacing), pegged.peg.wrapAround!(Spacing, QualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing)), "DClass.ImportDecl")(p);
        }
        else
        {
            if (auto m = tuple(`ImportDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("from"), Spacing), pegged.peg.wrapAround!(Spacing, QualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing)), "DClass.ImportDecl"), "ImportDecl")(p);
                memo[tuple(`ImportDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ImportDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("from"), Spacing), pegged.peg.wrapAround!(Spacing, QualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing)), "DClass.ImportDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("from"), Spacing), pegged.peg.wrapAround!(Spacing, QualifiedIdentifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("import"), Spacing), pegged.peg.wrapAround!(Spacing, ImportList, Spacing)), "DClass.ImportDecl"), "ImportDecl")(TParseTree("", false,[], s));
        }
    }
    static string ImportDecl(GetName g)
    {
        return "DClass.ImportDecl";
    }

    static TParseTree QualifiedIdentifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Identifier, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!("."), Identifier)))), "DClass.QualifiedIdentifier")(p);
        }
        else
        {
            if (auto m = tuple(`QualifiedIdentifier`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Identifier, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!("."), Identifier)))), "DClass.QualifiedIdentifier"), "QualifiedIdentifier")(p);
                memo[tuple(`QualifiedIdentifier`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree QualifiedIdentifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Identifier, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!("."), Identifier)))), "DClass.QualifiedIdentifier")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Identifier, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!("."), Identifier)))), "DClass.QualifiedIdentifier"), "QualifiedIdentifier")(TParseTree("", false,[], s));
        }
    }
    static string QualifiedIdentifier(GetName g)
    {
        return "DClass.QualifiedIdentifier";
    }

    static TParseTree ImportList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing))), "DClass.ImportList")(p);
        }
        else
        {
            if (auto m = tuple(`ImportList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing))), "DClass.ImportList"), "ImportList")(p);
                memo[tuple(`ImportList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ImportList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing))), "DClass.ImportList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("/"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing))), "DClass.ImportList"), "ImportList")(TParseTree("", false,[], s));
        }
    }
    static string ImportList(GetName g)
    {
        return "DClass.ImportList";
    }

    static TParseTree TypeDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, KeywordType, Spacing), pegged.peg.wrapAround!(Spacing, StructType, Spacing), pegged.peg.wrapAround!(Spacing, ClassType, Spacing), pegged.peg.wrapAround!(Spacing, AliasType, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "DClass.TypeDecl")(p);
        }
        else
        {
            if (auto m = tuple(`TypeDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, KeywordType, Spacing), pegged.peg.wrapAround!(Spacing, StructType, Spacing), pegged.peg.wrapAround!(Spacing, ClassType, Spacing), pegged.peg.wrapAround!(Spacing, AliasType, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "DClass.TypeDecl"), "TypeDecl")(p);
                memo[tuple(`TypeDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TypeDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, KeywordType, Spacing), pegged.peg.wrapAround!(Spacing, StructType, Spacing), pegged.peg.wrapAround!(Spacing, ClassType, Spacing), pegged.peg.wrapAround!(Spacing, AliasType, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "DClass.TypeDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, KeywordType, Spacing), pegged.peg.wrapAround!(Spacing, StructType, Spacing), pegged.peg.wrapAround!(Spacing, ClassType, Spacing), pegged.peg.wrapAround!(Spacing, AliasType, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "DClass.TypeDecl"), "TypeDecl")(TParseTree("", false,[], s));
        }
    }
    static string TypeDecl(GetName g)
    {
        return "DClass.TypeDecl";
    }

    static TParseTree AliasType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typedef"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, dataType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "DClass.AliasType")(p);
        }
        else
        {
            if (auto m = tuple(`AliasType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typedef"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, dataType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "DClass.AliasType"), "AliasType")(p);
                memo[tuple(`AliasType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AliasType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typedef"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, dataType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "DClass.AliasType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("typedef"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, dataType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "DClass.AliasType"), "AliasType")(TParseTree("", false,[], s));
        }
    }
    static string AliasType(GetName g)
    {
        return "DClass.AliasType";
    }

    static TParseTree KeywordType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("keyword"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "DClass.KeywordType")(p);
        }
        else
        {
            if (auto m = tuple(`KeywordType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("keyword"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "DClass.KeywordType"), "KeywordType")(p);
                memo[tuple(`KeywordType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree KeywordType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("keyword"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "DClass.KeywordType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("keyword"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "DClass.KeywordType"), "KeywordType")(TParseTree("", false,[], s));
        }
    }
    static string KeywordType(GetName g)
    {
        return "DClass.KeywordType";
    }

    static TParseTree KeywordList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "DClass.KeywordList")(p);
        }
        else
        {
            if (auto m = tuple(`KeywordList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "DClass.KeywordList"), "KeywordList")(p);
                memo[tuple(`KeywordList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree KeywordList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "DClass.KeywordList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), "DClass.KeywordList"), "KeywordList")(TParseTree("", false,[], s));
        }
    }
    static string KeywordList(GetName g)
    {
        return "DClass.KeywordList";
    }

    static TParseTree StructType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "DClass.StructType")(p);
        }
        else
        {
            if (auto m = tuple(`StructType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "DClass.StructType"), "StructType")(p);
                memo[tuple(`StructType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "DClass.StructType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "DClass.StructType"), "StructType")(TParseTree("", false,[], s));
        }
    }
    static string StructType(GetName g)
    {
        return "DClass.StructType";
    }

    static TParseTree ClassType(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dclass"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FieldDecl, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "DClass.ClassType")(p);
        }
        else
        {
            if (auto m = tuple(`ClassType`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dclass"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FieldDecl, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "DClass.ClassType"), "ClassType")(p);
                memo[tuple(`ClassType`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ClassType(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dclass"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FieldDecl, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "DClass.ClassType")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("dclass"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.oneOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, FieldDecl, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "DClass.ClassType"), "ClassType")(TParseTree("", false,[], s));
        }
    }
    static string ClassType(GetName g)
    {
        return "DClass.ClassType";
    }

    static TParseTree FieldDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MolecularField, Spacing), pegged.peg.wrapAround!(Spacing, AtomicField, Spacing), pegged.peg.wrapAround!(Spacing, ParameterField, Spacing)), Spacing), "DClass.FieldDecl")(p);
        }
        else
        {
            if (auto m = tuple(`FieldDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MolecularField, Spacing), pegged.peg.wrapAround!(Spacing, AtomicField, Spacing), pegged.peg.wrapAround!(Spacing, ParameterField, Spacing)), Spacing), "DClass.FieldDecl"), "FieldDecl")(p);
                memo[tuple(`FieldDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FieldDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MolecularField, Spacing), pegged.peg.wrapAround!(Spacing, AtomicField, Spacing), pegged.peg.wrapAround!(Spacing, ParameterField, Spacing)), Spacing), "DClass.FieldDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, MolecularField, Spacing), pegged.peg.wrapAround!(Spacing, AtomicField, Spacing), pegged.peg.wrapAround!(Spacing, ParameterField, Spacing)), Spacing), "DClass.FieldDecl"), "FieldDecl")(TParseTree("", false,[], s));
        }
    }
    static string FieldDecl(GetName g)
    {
        return "DClass.FieldDecl";
    }

    static TParseTree MolecularField(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, MolecularFieldMembers, Spacing)), "DClass.MolecularField")(p);
        }
        else
        {
            if (auto m = tuple(`MolecularField`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, MolecularFieldMembers, Spacing)), "DClass.MolecularField"), "MolecularField")(p);
                memo[tuple(`MolecularField`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MolecularField(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, MolecularFieldMembers, Spacing)), "DClass.MolecularField")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, MolecularFieldMembers, Spacing)), "DClass.MolecularField"), "MolecularField")(TParseTree("", false,[], s));
        }
    }
    static string MolecularField(GetName g)
    {
        return "DClass.MolecularField";
    }

    static TParseTree AtomicField(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, KeywordList, Spacing))), "DClass.AtomicField")(p);
        }
        else
        {
            if (auto m = tuple(`AtomicField`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, KeywordList, Spacing))), "DClass.AtomicField"), "AtomicField")(p);
                memo[tuple(`AtomicField`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AtomicField(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, KeywordList, Spacing))), "DClass.AtomicField")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, ParameterList, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, KeywordList, Spacing))), "DClass.AtomicField"), "AtomicField")(TParseTree("", false,[], s));
        }
    }
    static string AtomicField(GetName g)
    {
        return "DClass.AtomicField";
    }

    static TParseTree ParameterField(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, KeywordList, Spacing))), "DClass.ParameterField")(p);
        }
        else
        {
            if (auto m = tuple(`ParameterField`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, KeywordList, Spacing))), "DClass.ParameterField"), "ParameterField")(p);
                memo[tuple(`ParameterField`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ParameterField(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, KeywordList, Spacing))), "DClass.ParameterField")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, KeywordList, Spacing))), "DClass.ParameterField"), "ParameterField")(TParseTree("", false,[], s));
        }
    }
    static string ParameterField(GetName g)
    {
        return "DClass.ParameterField";
    }

    static TParseTree MolecularFieldMembers(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.wrapAround!(Spacing, Identifier, Spacing), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.wrapAround!(Spacing, Identifier, Spacing), Spacing)), Spacing))), "DClass.MolecularFieldMembers")(p);
        }
        else
        {
            if (auto m = tuple(`MolecularFieldMembers`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.wrapAround!(Spacing, Identifier, Spacing), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.wrapAround!(Spacing, Identifier, Spacing), Spacing)), Spacing))), "DClass.MolecularFieldMembers"), "MolecularFieldMembers")(p);
                memo[tuple(`MolecularFieldMembers`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MolecularFieldMembers(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.wrapAround!(Spacing, Identifier, Spacing), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.wrapAround!(Spacing, Identifier, Spacing), Spacing)), Spacing))), "DClass.MolecularFieldMembers")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.wrapAround!(Spacing, Identifier, Spacing), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.wrapAround!(Spacing, Identifier, Spacing), Spacing)), Spacing))), "DClass.MolecularFieldMembers"), "MolecularFieldMembers")(TParseTree("", false,[], s));
        }
    }
    static string MolecularFieldMembers(GetName g)
    {
        return "DClass.MolecularFieldMembers";
    }

    static TParseTree ParameterList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Parameter, Spacing)), Spacing))), "DClass.ParameterList")(p);
        }
        else
        {
            if (auto m = tuple(`ParameterList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Parameter, Spacing)), Spacing))), "DClass.ParameterList"), "ParameterList")(p);
                memo[tuple(`ParameterList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ParameterList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Parameter, Spacing)), Spacing))), "DClass.ParameterList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Parameter, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, Parameter, Spacing)), Spacing))), "DClass.ParameterList"), "ParameterList")(TParseTree("", false,[], s));
        }
    }
    static string ParameterList(GetName g)
    {
        return "DClass.ParameterList";
    }

    static TParseTree Parameter(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ArrayParameter, Spacing), pegged.peg.wrapAround!(Spacing, CharParameter, Spacing), pegged.peg.wrapAround!(Spacing, IntParameter, Spacing), pegged.peg.wrapAround!(Spacing, FloatParameter, Spacing), pegged.peg.wrapAround!(Spacing, SizedParameter, Spacing), pegged.peg.wrapAround!(Spacing, StructParameter, Spacing)), "DClass.Parameter")(p);
        }
        else
        {
            if (auto m = tuple(`Parameter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ArrayParameter, Spacing), pegged.peg.wrapAround!(Spacing, CharParameter, Spacing), pegged.peg.wrapAround!(Spacing, IntParameter, Spacing), pegged.peg.wrapAround!(Spacing, FloatParameter, Spacing), pegged.peg.wrapAround!(Spacing, SizedParameter, Spacing), pegged.peg.wrapAround!(Spacing, StructParameter, Spacing)), "DClass.Parameter"), "Parameter")(p);
                memo[tuple(`Parameter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Parameter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ArrayParameter, Spacing), pegged.peg.wrapAround!(Spacing, CharParameter, Spacing), pegged.peg.wrapAround!(Spacing, IntParameter, Spacing), pegged.peg.wrapAround!(Spacing, FloatParameter, Spacing), pegged.peg.wrapAround!(Spacing, SizedParameter, Spacing), pegged.peg.wrapAround!(Spacing, StructParameter, Spacing)), "DClass.Parameter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ArrayParameter, Spacing), pegged.peg.wrapAround!(Spacing, CharParameter, Spacing), pegged.peg.wrapAround!(Spacing, IntParameter, Spacing), pegged.peg.wrapAround!(Spacing, FloatParameter, Spacing), pegged.peg.wrapAround!(Spacing, SizedParameter, Spacing), pegged.peg.wrapAround!(Spacing, StructParameter, Spacing)), "DClass.Parameter"), "Parameter")(TParseTree("", false,[], s));
        }
    }
    static string Parameter(GetName g)
    {
        return "DClass.Parameter";
    }

    static TParseTree CharParameter(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, charType, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, charLiteral, Spacing)), Spacing))), "DClass.CharParameter")(p);
        }
        else
        {
            if (auto m = tuple(`CharParameter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, charType, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, charLiteral, Spacing)), Spacing))), "DClass.CharParameter"), "CharParameter")(p);
                memo[tuple(`CharParameter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CharParameter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, charType, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, charLiteral, Spacing)), Spacing))), "DClass.CharParameter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, charType, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, charLiteral, Spacing)), Spacing))), "DClass.CharParameter"), "CharParameter")(TParseTree("", false,[], s));
        }
    }
    static string CharParameter(GetName g)
    {
        return "DClass.CharParameter";
    }

    static TParseTree IntParameter(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, intType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, IntRange, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, IntTransform, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, IntConstant, Spacing)), Spacing))), "DClass.IntParameter")(p);
        }
        else
        {
            if (auto m = tuple(`IntParameter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, intType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, IntRange, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, IntTransform, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, IntConstant, Spacing)), Spacing))), "DClass.IntParameter"), "IntParameter")(p);
                memo[tuple(`IntParameter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IntParameter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, intType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, IntRange, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, IntTransform, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, IntConstant, Spacing)), Spacing))), "DClass.IntParameter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, intType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, IntRange, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, IntTransform, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, IntConstant, Spacing)), Spacing))), "DClass.IntParameter"), "IntParameter")(TParseTree("", false,[], s));
        }
    }
    static string IntParameter(GetName g)
    {
        return "DClass.IntParameter";
    }

    static TParseTree IntConstant(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "DClass.IntConstant")(p);
        }
        else
        {
            if (auto m = tuple(`IntConstant`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "DClass.IntConstant"), "IntConstant")(p);
                memo[tuple(`IntConstant`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IntConstant(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "DClass.IntConstant")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), "DClass.IntConstant"), "IntConstant")(TParseTree("", false,[], s));
        }
    }
    static string IntConstant(GetName g)
    {
        return "DClass.IntConstant";
    }

    static TParseTree IntTransform(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, operator, Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IntTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, IntTransform, Spacing)), Spacing))), "DClass.IntTransform")(p);
        }
        else
        {
            if (auto m = tuple(`IntTransform`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, operator, Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IntTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, IntTransform, Spacing)), Spacing))), "DClass.IntTransform"), "IntTransform")(p);
                memo[tuple(`IntTransform`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IntTransform(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, operator, Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IntTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, IntTransform, Spacing)), Spacing))), "DClass.IntTransform")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, operator, Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, IntTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, IntTransform, Spacing)), Spacing))), "DClass.IntTransform"), "IntTransform")(TParseTree("", false,[], s));
        }
    }
    static string IntTransform(GetName g)
    {
        return "DClass.IntTransform";
    }

    static TParseTree IntRange(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "DClass.IntRange")(p);
        }
        else
        {
            if (auto m = tuple(`IntRange`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "DClass.IntRange"), "IntRange")(p);
                memo[tuple(`IntRange`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IntRange(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "DClass.IntRange")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "DClass.IntRange"), "IntRange")(TParseTree("", false,[], s));
        }
    }
    static string IntRange(GetName g)
    {
        return "DClass.IntRange";
    }

    static TParseTree FloatParameter(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, floatType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FloatRange, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FloatTransform, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, FloatConstant, Spacing)), Spacing))), "DClass.FloatParameter")(p);
        }
        else
        {
            if (auto m = tuple(`FloatParameter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, floatType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FloatRange, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FloatTransform, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, FloatConstant, Spacing)), Spacing))), "DClass.FloatParameter"), "FloatParameter")(p);
                memo[tuple(`FloatParameter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FloatParameter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, floatType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FloatRange, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FloatTransform, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, FloatConstant, Spacing)), Spacing))), "DClass.FloatParameter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, floatType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FloatRange, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, FloatTransform, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, FloatConstant, Spacing)), Spacing))), "DClass.FloatParameter"), "FloatParameter")(TParseTree("", false,[], s));
        }
    }
    static string FloatParameter(GetName g)
    {
        return "DClass.FloatParameter";
    }

    static TParseTree FloatConstant(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, FloatTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), Spacing)), "DClass.FloatConstant")(p);
        }
        else
        {
            if (auto m = tuple(`FloatConstant`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, FloatTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), Spacing)), "DClass.FloatConstant"), "FloatConstant")(p);
                memo[tuple(`FloatConstant`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FloatConstant(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, FloatTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), Spacing)), "DClass.FloatConstant")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, FloatTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), Spacing)), "DClass.FloatConstant"), "FloatConstant")(TParseTree("", false,[], s));
        }
    }
    static string FloatConstant(GetName g)
    {
        return "DClass.FloatConstant";
    }

    static TParseTree FloatTransform(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, operator, Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, FloatTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, FloatTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), Spacing))), "DClass.FloatTransform")(p);
        }
        else
        {
            if (auto m = tuple(`FloatTransform`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, operator, Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, FloatTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, FloatTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), Spacing))), "DClass.FloatTransform"), "FloatTransform")(p);
                memo[tuple(`FloatTransform`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FloatTransform(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, operator, Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, FloatTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, FloatTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), Spacing))), "DClass.FloatTransform")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, operator, Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, FloatTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, FloatTransform, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing)), Spacing))), "DClass.FloatTransform"), "FloatTransform")(TParseTree("", false,[], s));
        }
    }
    static string FloatTransform(GetName g)
    {
        return "DClass.FloatTransform";
    }

    static TParseTree FloatRange(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "DClass.FloatRange")(p);
        }
        else
        {
            if (auto m = tuple(`FloatRange`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "DClass.FloatRange"), "FloatRange")(p);
                memo[tuple(`FloatRange`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FloatRange(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "DClass.FloatRange")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, floatLiteral, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "DClass.FloatRange"), "FloatRange")(TParseTree("", false,[], s));
        }
    }
    static string FloatRange(GetName g)
    {
        return "DClass.FloatRange";
    }

    static TParseTree SizedParameter(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, sizedType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, SizeConstraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, stringLiteral, Spacing)), Spacing))), "DClass.SizedParameter")(p);
        }
        else
        {
            if (auto m = tuple(`SizedParameter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, sizedType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, SizeConstraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, stringLiteral, Spacing)), Spacing))), "DClass.SizedParameter"), "SizedParameter")(p);
                memo[tuple(`SizedParameter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SizedParameter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, sizedType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, SizeConstraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, stringLiteral, Spacing)), Spacing))), "DClass.SizedParameter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, sizedType, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, SizeConstraint, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, stringLiteral, Spacing)), Spacing))), "DClass.SizedParameter"), "SizedParameter")(TParseTree("", false,[], s));
        }
    }
    static string SizedParameter(GetName g)
    {
        return "DClass.SizedParameter";
    }

    static TParseTree SizeConstraint(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "DClass.SizeConstraint")(p);
        }
        else
        {
            if (auto m = tuple(`SizeConstraint`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "DClass.SizeConstraint"), "SizeConstraint")(p);
                memo[tuple(`SizeConstraint`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SizeConstraint(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "DClass.SizeConstraint")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "DClass.SizeConstraint"), "SizeConstraint")(TParseTree("", false,[], s));
        }
    }
    static string SizeConstraint(GetName g)
    {
        return "DClass.SizeConstraint";
    }

    static TParseTree StructParameter(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "DClass.StructParameter")(p);
        }
        else
        {
            if (auto m = tuple(`StructParameter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "DClass.StructParameter"), "StructParameter")(p);
                memo[tuple(`StructParameter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructParameter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "DClass.StructParameter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing))), "DClass.StructParameter"), "StructParameter")(TParseTree("", false,[], s));
        }
    }
    static string StructParameter(GetName g)
    {
        return "DClass.StructParameter";
    }

    static TParseTree ArrayParameter(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, dataType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, ArrayRange, Spacing)), "DClass.ArrayParameter")(p);
        }
        else
        {
            if (auto m = tuple(`ArrayParameter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, dataType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, ArrayRange, Spacing)), "DClass.ArrayParameter"), "ArrayParameter")(p);
                memo[tuple(`ArrayParameter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArrayParameter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, dataType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, ArrayRange, Spacing)), "DClass.ArrayParameter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, dataType, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, ArrayRange, Spacing)), "DClass.ArrayParameter"), "ArrayParameter")(TParseTree("", false,[], s));
        }
    }
    static string ArrayParameter(GetName g)
    {
        return "DClass.ArrayParameter";
    }

    static TParseTree ArrayRange(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "DClass.ArrayRange")(p);
        }
        else
        {
            if (auto m = tuple(`ArrayRange`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "DClass.ArrayRange"), "ArrayRange")(p);
                memo[tuple(`ArrayRange`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ArrayRange(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "DClass.ArrayRange")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("-"), Spacing), pegged.peg.wrapAround!(Spacing, intLiteral, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing))), "DClass.ArrayRange"), "ArrayRange")(TParseTree("", false,[], s));
        }
    }
    static string ArrayRange(GetName g)
    {
        return "DClass.ArrayRange";
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(Module(p));
        result.children = [result];
        result.name = "DClass";
        return result;
    }

    static TParseTree opCall(string input)
    {
        if(__ctfe)
        {
            return DClass(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            forgetMemo();
            return DClass(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "DClass";
    }


    static void forgetMemo()
    {
        memo = null;
    }
    }
}

alias GenericDClass!(ParseTree).DClass DClass;

