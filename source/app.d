import std.algorithm;
import std.file;
import std.getopt;
import std.path;
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
        auto helpInformation = getopt(args, config.passThrough,
            "output|o", "The file to output.", &output,
            "module|m", "The name of the module to generate.", &name,
            "import|i", "Modules to import.", &imports,
            "stubs|s", "Whether to generate stubs.", &generateStubs,
            "base|b", "The desired base type.", &base,
            "save-parse-tree", "Whether to save the initial parse tree", &saveParseTree,
            "parse-tree-output", "The location to save the initial parse tree", &parseTreeOutput,
        );
        // dfmt on

        files = args[1 .. $];
        if (!output.length && files.length == 1)
        {
            output = text(stripExtension(files[0]), ".d");
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
            writeln("Usage: bamboo [options] [files]");
            defaultGetoptPrinter("Transpiler for Astron dc files.", helpInformation.options);
            return;
        }

        string source = joiner(files.map!(x => x.readText())).to!string;

        ParseTree tree = DClass(source);

        if (saveParseTree)
        {
            toHTML(tree, parseTreeOutput);
        }

        if (!tree.successful)
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
