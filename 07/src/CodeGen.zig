const std = @import("std");
const build_options = @import("build_options");
const AnyWriter = std.io.AnyWriter;
const instruction = @import("instruction.zig");
const Instruction = instruction.Instruction;
const Command = instruction.Command;
const Segment = instruction.Segment;
const SegmentWithConstant = instruction.SegmentWithConstant;

const CodeGen = @This();

class_name: []const u8,
label_counter: u16 = 0,

pub fn emit(self: *CodeGen, inst: *const Instruction, writer: AnyWriter) !void {
    return switch (inst.*) {
        .push => |segment_const| self.emitPush(segment_const, writer),
        .pop => |segment_const| self.emitPop(segment_const, writer),
        inline .neg, .not => |_, tag| self.emitUnary(tag, writer),
        inline .add, .sub, .@"and", .@"or" => |_, tag| self.emitArithmetic(tag, writer),
        inline .eq, .gt, .lt => |_, tag| self.emitComparison(tag, writer),
        else => std.debug.panic("instruction is unimplemented '{}'", .{@as(Command, inst.*)}),
    };
}

fn emitPush(self: *CodeGen, args: SegmentWithConstant, writer: AnyWriter) !void {
    if (!build_options.pure_asm)
        try writer.print("// push {s} {}\n", .{ @tagName(args.segment), args.constant });

    switch (args.segment) {
        .constant => try writer.print(
            \\@{}
            \\D=A
            \\
        , .{args.constant}),
        .static => try writer.print(
            \\@{s}.{}
            \\D=M
            \\
        , .{ self.class_name, args.constant }),
        .temp => {
            if (args.constant > 7) {
                std.log.err("temp segment only supports offsets 0-7", .{});
                return error.InvalidCommand;
            }

            const temp_addr = 5 + args.constant;

            try writer.print(
                \\@{}
                \\D=M
                \\
            , .{temp_addr});
        },
        .pointer => {
            if (args.constant > 2) {
                std.log.err("pointer segment only supports values of 0 or 1", .{});
                return error.InvalidCommand;
            }

            const label = if (args.constant == 0)
                "THIS"
            else
                "THAT";

            try writer.print(
                \\@{s}
                \\D=M
                \\
            , .{label});
        },
        inline .local,
        .argument,
        .this,
        .that,
        => |tag| {
            const segment_ptr = instruction.segment_ptr_map.getAssertContains(tag);

            try writer.print(
                \\@{}
                \\D=A
                \\@{s}
                \\A=D+M
                \\D=M
                \\
            , .{ args.constant, segment_ptr });
        },
    }

    // loads the value from D onto the stack
    try writer.print(
        \\@SP
        \\AM=M+1
        \\A=A-1
        \\M=D
        \\
    , .{});
}

fn emitPop(self: *CodeGen, args: SegmentWithConstant, writer: AnyWriter) !void {
    if (!build_options.pure_asm)
        try writer.print("// pop {s} {}\n", .{ @tagName(args.segment), args.constant });

    switch (args.segment) {
        .static => try writer.print(
            \\@{s}.{}
            \\D=A
            \\@R13
            \\M=D
            \\
        , .{ self.class_name, args.constant }),
        .temp => {
            if (args.constant > 7) {
                std.log.err("temp segment only supports offsets 0-7", .{});
                return error.InvalidCommand;
            }

            const temp_addr = 5 + args.constant;

            try writer.print(
                \\@{}
                \\D=A
                \\@R13
                \\A=D
                \\
            , .{temp_addr});
        },
        .pointer => {
            if (args.constant > 2) {
                std.log.err("pointer segment only supports values of 0 or 1", .{});
                return error.InvalidCommand;
            }

            const label = if (args.constant == 0)
                "THIS"
            else
                "THAT";

            try writer.print(
                \\@{s}
                \\D=A
                \\@R13
                \\A=D
                \\
            , .{label});
        },
        inline .local,
        .argument,
        .this,
        .that,
        => |tag| {
            const segment_ptr = instruction.segment_ptr_map.getAssertContains(tag);

            try writer.print(
                \\@{}
                \\D=A
                \\@{s}
                \\D=D+M
                \\@R13
                \\M=D
                \\
            , .{ args.constant, segment_ptr });
        },
        .constant => {
            std.log.err("pushing to segment 'constant' is unsupported", .{});
            return error.InvalidCommand;
        },
    }

    // loads the top of the stack into the address located at R13
    try writer.print(
        \\@SP
        \\AM=M-1
        \\D=M
        \\@R13
        \\A=M
        \\M=D
        \\
    , .{});
}

fn emitUnary(_: *const CodeGen, comptime op: Command, writer: AnyWriter) !void {
    if (!build_options.pure_asm)
        try writer.print("// {s}\n", .{@tagName(op)});

    try writer.writeAll("@SP\n");

    switch (op) {
        .neg => try writer.writeAll("M=-M\n"),
        .not => try writer.writeAll("M=!M\n"),
        else => @compileError("unsupported unary operation " ++ @tagName(op)),
    }
}

fn emitArithmetic(_: *const CodeGen, comptime op: Command, writer: AnyWriter) !void {
    if (!build_options.pure_asm)
        try writer.print("// {s}\n", .{@tagName(op)});

    // pop D and set A to SP
    try writer.print(
        \\@SP
        \\AM=M-1
        \\D=M
        \\
    , .{});

    switch (op) {
        .add => try writer.writeAll("M=M+D\n"),
        .sub => try writer.writeAll("M=M-D\n"),
        .@"and" => try writer.writeAll("M=M&D\n"),
        .@"or" => try writer.writeAll("M=M|D\n"),
        else => @compileError("unsupported arithmetic operation " ++ @tagName(op)),
    }
}

fn emitComparison(_: *const CodeGen, comptime op: Command, writer: AnyWriter) !void {
    if (!build_options.pure_asm)
        try writer.print("// {s}\n", .{@tagName(op)});

    switch (op) {
        .eq, .lt, .gt => {},
        else => @compileError("unsupported comparison operation " ++ @tagName(op)),
    }
}

// unit tests yay!

const testing = std.testing;

fn testEmitPush(expected: []const u8, args: SegmentWithConstant) !void {
    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    var codegen = CodeGen{ .class_name = "class" };

    try codegen.emitPush(args, buffer.writer().any());

    try testing.expectEqualStrings(expected, buffer.items);
}

test emitPush {
    try testEmitPush(
        \\// push constant 42
        \\@42
        \\D=A
        \\@SP
        \\AM=M+1
        \\A=A-1
        \\M=D
        \\
    ,
        .{ .segment = .constant, .constant = 42 },
    );

    try testEmitPush(
        \\// push local 42
        \\@42
        \\D=A
        \\@LCL
        \\A=D+M
        \\D=M
        \\@SP
        \\AM=M+1
        \\A=A-1
        \\M=D
        \\
    ,
        .{ .segment = .local, .constant = 42 },
    );

    try testEmitPush(
        \\// push static 1
        \\@class.1
        \\D=M
        \\@SP
        \\AM=M+1
        \\A=A-1
        \\M=D
        \\
    ,
        .{ .segment = .static, .constant = 1 },
    );

    try testEmitPush(
        \\// push temp 1
        \\@6
        \\D=M
        \\@SP
        \\AM=M+1
        \\A=A-1
        \\M=D
        \\
    ,
        .{ .segment = .temp, .constant = 1 },
    );
    
    try testEmitPush(
        \\// push pointer 0
        \\@THIS
        \\D=M
        \\@SP
        \\AM=M+1
        \\A=A-1
        \\M=D
        \\
    ,
        .{ .segment = .pointer, .constant = 0 },
    );
    
    try testEmitPush(
        \\// push pointer 1
        \\@THAT
        \\D=M
        \\@SP
        \\AM=M+1
        \\A=A-1
        \\M=D
        \\
    ,
        .{ .segment = .pointer, .constant = 1 },
    );
}

fn testEmitPop(expected: []const u8, args: SegmentWithConstant) !void {
    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    var codegen = CodeGen{ .class_name = "class" };

    try codegen.emitPop(args, buffer.writer().any());

    try testing.expectEqualStrings(expected, buffer.items);
}

test emitPop {
    try testEmitPop(
        \\// pop local 2
        \\@2
        \\D=A
        \\@LCL
        \\D=D+M
        \\@R13
        \\M=D
        \\@SP
        \\AM=M-1
        \\D=M
        \\@R13
        \\A=M
        \\M=D
        \\
    ,
        .{ .segment = .local, .constant = 2 },
    );

    try testEmitPop(
        \\// pop static 2
        \\@class.2
        \\D=A
        \\@R13
        \\M=D
        \\@SP
        \\AM=M-1
        \\D=M
        \\@R13
        \\A=M
        \\M=D
        \\
    ,
        .{ .segment = .static, .constant = 2 },
    );
}
