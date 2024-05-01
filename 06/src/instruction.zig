pub const Jump = enum(u3) {
    none = 0,
    jump = 0b111,
    not_equal = 0b101,
    less = 0b100,
    less_equal = 0b110,
    equal = 0b010,
    greater = 0b001,
    greater_equal = 0b011,
};

pub const Destination = packed struct(u3) {
    m: bool = false,
    d: bool = false,
    a: bool = false,
};

// zig fmt: off
pub const Computation = enum(u7) {
    // a = 0
    zero        = 0b0101010,
    one         = 0b0111111,
    minus_one   = 0b0111010,
    d           = 0b0001100,
    a           = 0b0110000,
    not_d       = 0b0001101,
    not_a       = 0b0110001,
    negative_d  = 0b0001111,
    negative_a  = 0b0110011,
    d_plus_one  = 0b0011111,
    a_plus_one  = 0b0110111,
    d_minus_one = 0b0001110,
    a_minus_one = 0b0110010,
    d_plus_a    = 0b0000010,
    d_minus_a   = 0b0010011,
    a_minus_d   = 0b0000111,
    d_and_a     = 0b0000000,
    d_or_a      = 0b0010101,

    // a = 1
    m           = 0b1110000,
    not_m       = 0b1110001,
    negative_m  = 0b1110011,
    m_plus_one  = 0b1110111,
    m_minus_one = 0b1110010,
    d_plus_m    = 0b1000010,
    d_minus_m   = 0b1010011,
    m_minus_d   = 0b1000111,
    d_and_m     = 0b1000000,
    d_or_m      = 0b1010101,
};
// zig fmt: on

pub const AInstruction = union(enum) {
    label: []const u8,
    number: u15,
};

pub const CInstruction = packed struct(u13) {
    jump: Jump = .none,
    dest: Destination = .{},
    comp: Computation,
};

// because of packed struct magic, this type now has same in memory representation as the instruction
// it doesnt have the c instructions padding or the a/c indicator, but we'll patch that in later
pub const Instruction = union(enum) {
    a: AInstruction,
    c: CInstruction,
};

const std = @import("std");
const testing = std.testing;

test "c instruction correct layout" {
    const inst = CInstruction{
        .comp = .one,
        .dest = .{
            .a = true,
            .m = true,
        },
        .jump = .greater,
    };

    const raw: u13 = @bitCast(inst);
    try testing.expectEqual(0b0111111101001, raw);
}
