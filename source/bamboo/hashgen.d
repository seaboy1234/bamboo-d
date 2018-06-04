module bamboo.hashgen;

import std.traits;
import bamboo.legacy_hash;
import bamboo.types;

/// Computes a hash using `bamboo.legacy_hash`.
uint computeHash(Module file)
{
    HashGenerator gen;
    hashModule(gen, file);
    return gen.hash;
}

/// Provides mechanisms for generating hashes for DClasses.
struct HashGenerator
{
    private size_t _hash;
    private size_t _index;
    private PrimeGenerator!size_t _primes;

nothrow @nogc @safe:

    /// Adds an int to the accumulator.
    void addInt(size_t num)
    {
        _index++;
        _hash += _primes.front * num;
        _primes.popFront;
    }

    /// Adds a string to the accumulator.
    void addString(string str)
    {
        addInt(str.length);

        foreach (c; str)
        {
            addInt(cast(size_t) c);
        }
    }

    /// Gets the current hash.
    uint hash() inout @property
    {
        return cast(uint)(_hash & 0xffffffff);
    }
}

/// Utility for generating prime numers.
struct PrimeGenerator(T) if (isIntegral!T)
{
    private T _current = 2;

pure nothrow @nogc @safe:

    ///
    enum bool empty = false;

    /// Returns: the current prime.
    T front() inout @property
    {
        return _current;
    }

    /// Advances the generator.
    void popFront()
    {
        if (_current == 2)
        {
            ++_current;
            return;
        }

        // This is not at all efficient.
        while (!isPrime(_current += 2))
        {
        }
    }

    /// Creates a copy of the generator.
    typeof(this) save()
    {
        return PrimeGenerator(_current);
    }
}

///
pure nothrow @nogc @safe unittest
{
    import std.algorithm : equal;
    import std.range : take;

    // dfmt off
    static immutable earlyPrimes = 
       [ 2,   3,   5,   7,  11,   13,  17,  19,  23,  29,
        31,  37,  41,  43,  47,   53,  59,  61,  67,  71,
        73,  79,  83,  89,  97,  101, 103, 107, 109, 113,
        127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 
        179, 181, 191, 193, 197, 199, 211, 223, 227, 229];
    // dfmt on

    auto primes = PrimeGenerator!int();

    assert(equal(earlyPrimes, primes.take(earlyPrimes.length)));
}

/// Checks if a number is prime.
/// Returns: `true` if `n` is prime.
bool isPrime(T)(T n) if (isIntegral!T)
{
    if (n == 2)
    {
        return true;
    }
    if (n == 1 || n < 1 || (n & 1) == 0)
    {
        return false;
    }
    if (n > 3)
    {
        for (auto i = 3; i * i <= n; i += 2)
        {
            if ((n % i) == 0)
                return false;
        }
    }
    return true;
}

///
pure nothrow @nogc @safe unittest
{
    assert(isPrime(2));
    assert(isPrime(3));
    assert(isPrime(5));
    assert(isPrime(7));
    assert(isPrime(199UL));
    assert(isPrime(104_729)); // 10,000th prime.

    assert(!isPrime(-199));
    assert(!isPrime(-1));
    assert(!isPrime(1)); // 1 is not a prime number: https://primes.utm.edu/notes/faq/one.html
    assert(!isPrime(4));
}
