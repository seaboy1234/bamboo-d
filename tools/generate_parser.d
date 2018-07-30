#!/usr/bin/env dub
/+ dub.sdl:
	name "generate_parser"
    dependency "pegged" version="~>0.4.3"
+/
module tools.generate_parser;

import std.file;
import pegged.grammar;

void main()
{
    asModule("bamboo.parser", "../source/bamboo/parser", readText("../source/bamboo/parser.peg"));
}
