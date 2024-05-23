const std = @import("std");
const builtins = @import("builtins");
const build_options = @import("build_options");
const AnyWriter = std.io.AnyWriter;

const Tuple = std.meta.Tuple;
const StructField = std.builtin.Type.StructField;

fn ComptimeMapTuple(comptime T: type, comptime len: comptime_int) type {
    return Tuple(&([_]type{ComptimeMapEntry(T)} ** len));
}

fn ComptimeMapEntry(comptime T: type) type {
    return Tuple(&[_]type{ []const u8, T });
}

// construct a tuple that contains a tuple for each enum value, with the first entry being
// the enums name as a string and the second entry being the enums value
fn enumNameTuples(comptime Enum: type) ComptimeMapTuple(Enum, @typeInfo(Enum).Enum.fields.len) {
    const typeInfo = @typeInfo(Enum).Enum;

    const MapTuple = ComptimeMapTuple(Enum, typeInfo.fields.len);
    const MapEntry = ComptimeMapEntry(Enum);

    var tuple: MapTuple = undefined;

    for (typeInfo.fields, 0..) |field, i| {
        var entry: MapEntry = undefined;
        entry[0] = field.name;
        entry[1] = @enumFromInt(field.value);
        tuple[i] = entry;
    }

    return tuple;
}

// map comamnds/segments to their string representations
pub const command_token_map = std.ComptimeStringMap(Command, enumNameTuples(Command));
pub const segment_token_map = std.ComptimeStringMap(Segment, enumNameTuples(Segment));

// map segments to their pointer labels
pub const segment_ptr_map = std.EnumMap(Segment, []const u8).init(.{
    .local = "LCL",
    .argument = "ARG",
    .this = "THIS",
    .that = "THAT",
    .constant = null,
    .pointer = null,
    .static = null,
    .temp = null,
});

pub const Command = enum {
    // push/pop
    push,
    pop,
    // arithmeticc
    add,
    sub,
    neg,
    eq,
    gt,
    lt,
    @"and",
    @"or",
    not,
    // branching
    label,
    goto,
    @"if-goto",
    // function
    function,
    call,
    @"return",
};

pub const Segment = enum {
    local,
    argument,
    this,
    that,
    constant,
    static,
    temp,
    pointer,
};

pub const SegmentWithConstant = struct {
    segment: Segment,
    constant: u16,
};

pub const LabelWithConstant = struct {
    label: []const u8,
    constant: u16,
};

pub const Instruction = union(Command) {
    // push/pop
    push: SegmentWithConstant,
    pop: SegmentWithConstant,
    // arithmetic
    add,
    sub,
    neg,
    eq,
    gt,
    lt,
    @"and",
    @"or",
    not,
    // branching
    label: []const u8,
    goto: []const u8,
    @"if-goto": []const u8,
    // function
    function: LabelWithConstant,
    call: LabelWithConstant,
    @"return",
};
