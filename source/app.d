import std.algorithm;
import std.file;
import std.getopt;
import std.stdio;

import bamboo.astgen;
import bamboo.codegen;
import bamboo.hashgen;
import bamboo.parser;
import bamboo.types;

import pegged.peg;
import pegged.tohtml;

version (application)
{
    void main(string[] args)
    {
        string[] files;
        string output;
        string parseTreeOutput = "parse-tree.html";

        bool generateStubs = true;
        bool saveParseTree = false;
        string[] imports = ["libastrond"];
        string base = "DistributedObject";
        string name;

        //dfmt off
        auto helpInformation = getopt(args, 
            "file|f", "The file(s) to transpile.", &files,
            "output|o", "The file to output.", &output,
            "module|m", "The name of the module to generate.", &name,
            "import|i", "Modules to import.", &imports,
            "stubs|s", "Whether to generate stubs.", &generateStubs,
            "base|b", "The desired base type.", &base,
            "save-parse-tree", "Whether to save the initial parse tree", &saveParseTree,
            "parse-tree-output", "The location to save the initial parse tree", &parseTreeOutput,
        );
        // dfmt on

        if(!args.length)
        {
            helpInformation.helpWanted = true;
        }

        if (!helpInformation.helpWanted)
        {
            if (!files.length)
            {
                helpInformation.helpWanted = true;
                writeln("No files specified!");
            }

            if (!output.length)
            {
                helpInformation.helpWanted = true;
                writeln("No output specified!");
            }
        }

        if (helpInformation.helpWanted)
        {
            defaultGetoptPrinter("Transpiler for Astron dc files.", helpInformation.options);
            return;
        }

        string source = joiner(files.map!(x => x.readText()), lineSep.to!string).to!string;

        ParseTree tree = DClass(source);

        if(saveParseTree)
        {
            toHTML(tree, parseTreeOutput);
        }

        if(!tree.successful)
        {
            writeln("Files did not parse correctly.");
        }

        Module mod = parseString(source);

        if (!name.length && mod.symbol.length)
        {
            name = mod.symbol;
        }

        if (!name.length)
        {
            writeln("Generating anonymous module!");
        }

        string dfile = generateFile(mod, name, imports.join("; "), base, generateStubs);

        File f = File(output, "w+");
        f.write(dfile);
    }
}
