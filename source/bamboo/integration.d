module bamboo.integration;

version (unittest)  :  // Example
import std.conv;
import std.file;
import std.stdio;
import pegged.parser;
import bamboo.astgen;
import bamboo.parser;
import bamboo.types;
import bamboo.hashgen;
import bamboo.legacy_hash;

unittest
{

    Module file = parseModule("tests/astron_example.dc");

    assert(file !is null);
    HashGenerator gen;

    hashModule(gen, file);

    auto hash = gen.hash;

    assert(hash == 205_656_144, hash.to!string() ~ " != 205,656,144");

    file = parseModule("tests/simple_example.dc");
    hash = computeHash(file);

    assert(hash == 4_275_592, hash.to!string() ~ " != 4,275,592");
}