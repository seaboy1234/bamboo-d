module bamboo.integration;

version (unittest)  :  // Example
import std.conv;
import std.file;
import std.stdio;
import pegged.parser;
import bamboo.astgen;
import bamboo.codegen;
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

enum simple_example = `
// Some abstractions for readability.
typedef uint32 doId;
typedef uint32 zoneId;
typedef uint64 channel;

// The Python Views for these distributed classes.
from simple_example.objects import LoginManager/AI/UD
from simple_example.objects import DistributedMaproot/AI/UD
from simple_example.objects import DistributedAvatar/AI/OV

// A simple DOG for username/password authentication and handoff to
// the DistributedMaproot for avatar creation.
dclass LoginManager {
  setMaproot(doId maproot);
  login(string username, string password) clsend;
};

// The root object of the map, container for the DistributedAvatars.
dclass DistributedMaproot {
  createAvatar(channel client) airecv;
};

// The actual class for avatar control.
// The idea is that a Client can set its intention for its heading
// and speed, but only the controlling AI can set its actual
// position and heading.
dclass DistributedAvatar {
  string name broadcast required;

  setXYZH(float32, float32, float32, int16 % 360) broadcast required;
  indicateIntent(float32 x, float32 y) ownsend airecv;
};
`;

unittest
{
    import std.stdio : File;

    Module file = parseModule("tests/simple_example.dc");

    string generated = generateFile(file, "simple_example");

    // Write to file
    //File output = File("tests/simple_example.d", "w");
    //output.writeln(generated);

    // These are needed for the dclass system to work.

    class DistributedObject
    {
    }

    enum Keyword
    {
        required,
        broadcast,
        ownrecv,
        ram,
        db,
        clsend,
        clrecv,
        ownsend,
        airecv,
    }

    alias required = Keyword.required;
    alias broadcast = Keyword.broadcast;
    alias ownrecv = Keyword.ownrecv;
    alias ram = Keyword.ram;
    alias db = Keyword.db;
    alias clsend = Keyword.clsend;
    alias clrecv = Keyword.clrecv;
    alias ownsend = Keyword.ownsend;
    alias airecv = Keyword.airecv;

    // Mix in the module.

    enum str = generateModule(parseString(simple_example));
    mixin(str);

    // Test that the types exist.
    static assert(is(LoginManager));
    static assert(is(DistributedMaproot));
    static assert(is(DistributedAvatar));

    // Can we override one?
    class DistributedAvatarTest : DistributedAvatar
    {
        private
        {
            string _name;
            float _x;
            float _y;
            float _z;
            short _h;

            float _tx;
            float _ty;
        }

        override inout(string) name() inout
        {
            return _name;
        }

        override void name(string value) @property
        {
            _name = value;
        }

        override void setXYZH(float x, float y, float z, short h)
        {
            _x = x;
            _y = y;
            _z = z;
            _h = h;
        }

        override void indicateIntent(float x, float y)
        {
            _tx = x;
            _ty = y;
        }
    }
}

unittest
{
    import std.stdio : File;

    Module file = parseModule("tests/direct.dc");

    string generated = generateFile(file);

    assert(file.symbol == "direct.distributed");
}
