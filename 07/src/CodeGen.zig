const std = @import("std");
const build_options = @import("build_options");
const AnyWriter = std.io.AnyWriter;
const instruction = @import("instruction.zig");
const Instruction = instruction.Instruction;
const Command = instruction.Command;
const Segment = instruction.Segment;
const SegmentWithConstant = instruction.SegmentWithConstant;

const CodeGen = @This();

writer: AnyWriter,

class_name: []const u8,
// used to generate unique labels in compare instructions
label_counter: u16 = 0,

pub fn init(writer: AnyWriter, class_name: []const u8) CodeGen {
    return .{
        .writer = writer,
        .class_name = class_name,
    };
}

pub fn emit(self: *CodeGen, inst: *const Instruction) !void {
    return switch (inst.*) {
        .push => |segment_const| self.emitPush(segment_const),
        .pop => |segment_const| self.emitPop(segment_const),
        inline .neg, .not => |_, tag| self.emitUnary(tag),
        inline .add, .sub, .@"and", .@"or" => |_, tag| self.emitArithmetic(tag),
        inline .eq, .gt, .lt => |_, tag| self.emitComparison(tag),
        else => std.debug.panic("instruction is unimplemented '{}'", .{@as(Command, inst.*)}),
    };
}

pub fn emitBootstrap(self: *CodeGen) !void {
    try self.writer.print(
        \\@256
        \\D=A
        \\@SP
        \\M=D
        \\
    , .{});
}

fn emitPush(self: *CodeGen, args: SegmentWithConstant) !void {
    if (!build_options.pure_asm)
        try self.writer.print("// push {s} {}\n", .{ @tagName(args.segment), args.constant });

    switch (args.segment) {
        .constant => try self.writer.print(
            \\@{}
            \\D=A
            \\
        , .{args.constant}),
        .static => try self.writer.print(
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

            try self.writer.print(
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

            try self.writer.print(
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
            const segment_ptr = comptime instruction.segment_ptr_map.getAssertContains(tag);

            try self.writer.print(
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
    try self.writer.print(
        \\@SP
        \\AM=M+1
        \\A=A-1
        \\M=D
        \\
    , .{});
}

fn emitPop(self: *CodeGen, args: SegmentWithConstant) !void {
    if (!build_options.pure_asm)
        try self.writer.print("// pop {s} {}\n", .{ @tagName(args.segment), args.constant });

    switch (args.segment) {
        .static => try self.writer.print(
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

            try self.writer.print(
                \\@{}
                \\D=A
                \\@R13
                \\M=D
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

            try self.writer.print(
                \\@{s}
                \\D=A
                \\@R13
                \\M=D
                \\
            , .{label});
        },
        inline .local,
        .argument,
        .this,
        .that,
        => |tag| {
            const segment_ptr = comptime instruction.segment_ptr_map.getAssertContains(tag);

            try self.writer.print(
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
            std.log.err("popping from segment 'constant' is unsupported", .{});
            return error.InvalidCommand;
        },
    }

    // loads the top of the stack into the address located at R13
    try self.writer.print(
        \\@SP
        \\AM=M-1
        \\D=M
        \\@R13
        \\A=M
        \\M=D
        \\
    , .{});
}

fn emitUnary(self: *const CodeGen, comptime op: Command) !void {
    if (!build_options.pure_asm)
        try self.writer.print("// {s}\n", .{@tagName(op)});

    try self.writer.writeAll(
        \\@SP
        \\A=M-1
        \\
    );

    switch (op) {
        .neg => try self.writer.writeAll("M=-M\n"),
        .not => try self.writer.writeAll("M=!M\n"),
        else => @compileError("unsupported unary operation " ++ @tagName(op)),
    }
}

fn emitArithmetic(self: *const CodeGen, comptime op: Command) !void {
    if (!build_options.pure_asm)
        try self.writer.print("// {s}\n", .{@tagName(op)});

    // pop D and set A to SP
    try self.writer.print(
        \\@SP
        \\AM=M-1
        \\D=M
        \\A=A-1
        \\
    , .{});

    switch (op) {
        .add => try self.writer.writeAll("M=M+D\n"),
        .sub => try self.writer.writeAll("M=M-D\n"),
        .@"and" => try self.writer.writeAll("M=M&D\n"),
        .@"or" => try self.writer.writeAll("M=M|D\n"),
        else => @compileError("unsupported arithmetic operation " ++ @tagName(op)),
    }
}

fn emitComparison(self: *CodeGen, comptime op: Command) !void {
    if (!build_options.pure_asm)
        try self.writer.print("// {s}\n", .{@tagName(op)});

    try self.writer.print(
        \\@SP
        \\AM=M-1
        \\D=M
        \\A=A-1
        \\D=M-D
        \\@CMP_TRUE_{s}_{}
        \\
    , .{ self.class_name, self.label_counter });

    switch (op) {
        .eq => try self.writer.writeAll("D;JEQ\n"),
        .gt => try self.writer.writeAll("D;JGT\n"),
        .lt => try self.writer.writeAll("D;JLT\n"),
        else => @compileError("unsupported comparison operation " ++ @tagName(op)),
    }

    try self.writer.print(
        \\@CMP_END_{s}_{}
        \\D=0;JMP
        \\(CMP_TRUE_{s}_{})
        \\D=-1
        \\(CMP_END_{s}_{})
        \\@SP
        \\A=M-1
        \\M=D
        \\
    , .{
        self.class_name, self.label_counter,
        self.class_name, self.label_counter,
        self.class_name, self.label_counter,
    });

    self.label_counter += 1;
}

// unit tests yay!

const testing = std.testing;

fn testEmitPush(expected: []const u8, args: SegmentWithConstant) !void {
    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    var codegen = CodeGen.init(buffer.writer().any(), "class");

    try codegen.emitPush(args);

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

    var codegen = CodeGen.init(buffer.writer().any(), "class");

    try codegen.emitPop(args);

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
