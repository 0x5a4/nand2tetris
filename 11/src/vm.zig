const std = @import("std");
const AnyWriter = std.io.AnyWriter;
const StringLocation = @import("Ast.zig").StringLocation;

const Tuple = std.meta.Tuple;

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
    constant: i16,
};

pub const LabelWithConstant = struct {
    label: []const u8,
    constant: u16,
};

pub const Command = @typeInfo(Instruction).Union.tag_type.?;

pub const Instruction = union(enum) {
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

pub fn emitInstruction(writer: AnyWriter, inst: Instruction) !void {
    switch (inst) {
        // push/pop
        .push => |x| try writer.print("push {s} {}\n", .{ @tagName(x.segment), x.constant }),
        .pop => |x| try writer.print("pop {s} {}\n", .{ @tagName(x.segment), x.constant }),
        // arithmetic
        .add => try writer.writeAll("add\n"),
        .sub => try writer.writeAll("sub\n"),
        .neg => try writer.writeAll("neg\n"),
        .eq => try writer.writeAll("eq\n"),
        .gt => try writer.writeAll("gt\n"),
        .lt => try writer.writeAll("lt\n"),
        .@"and" => try writer.writeAll("and\n"),
        .@"or" => try writer.writeAll("or\n"),
        .not => try writer.writeAll("not\n"),
        // branching
        .label => |x| try writer.print("label {s}\n", .{x}),
        .goto => |x| try writer.print("goto {s}\n", .{x}),
        .@"if-goto" => |x| try writer.print("if-goto {s}\n", .{x}),
        // functiono
        .function => |x| try writer.print("function {s} {}\n", .{ x.label, x.constant }),
        .call => |x| try writer.print("call {s} {}\n", .{ x.label, x.constant }),
        .@"return" => try writer.writeAll("return\n"),
    }
}
