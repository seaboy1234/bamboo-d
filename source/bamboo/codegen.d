module bamboo.codegen;

/// Aliases for dclass int type names.
enum intTypes = `
alias int8  = byte;
alias int16 = short;
alias int32 = int;
alias int64 = long;

alias uint8  = ubyte;
alias uint16 = ushort;
alias uint32 = uint;
alias uint64 = long;
`;

/// Aliases for dclass float type names.
enum floatTypes = `
alias float32 = float;
alias float64 = double;
`;

// NB: string and char are properly aliased in D.

/// Alias for the dclass `blob` type.
enum blobTypes = `
alias blob = immutable ubyte[];
`;

/// Handy mixin for all dclass primitives.
enum Primitives = `
mixin(intTypes);
mixin(floatTypes);
mixin(blobTypes);
`;
