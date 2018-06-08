module bamboo.parsertests;

import bamboo.parser;

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
