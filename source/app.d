import std.getopt;
import std.stdio;
import bamboo.astgen;
import bamboo.codegen;
import bamboo.hashgen;
import bamboo.parser;
import bamboo.types;

version (application)
{
    void main(string[] args)
    {
        string[] files;
        string output;

        bool generateStubs = true;
        string[] imports = ["libastrond"];
        string base = "DistributedObject";
        string name;

        //dfmt off
        auto helpInformation = getopt(args, 
            "file|f", "The file(s) to transpile.", &files,
            "output|out|o", "The file to output.", &output,
            "module|mod|m", "The name of the module to generate.", &name,
            "import|i", "Modules to import.", &imports,
            "stubs|s", "Whether to generate stubs.", &generateStubs,
            "base|b", "The desired base type.", &base
        );
        // dfmt on
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

        auto source = joiner(files, lineSep);

        auto mod = parseString(source);

        if (!name.length && mod.symbol.length)
        {
            name = mod.symbol;
        }

        if (!name.length)
        {
            writeln("Generating anonymous module!");
        }

        string dfile = generateFile(mod, name, imports.join("; "), base, generateStubs);
    }
}
