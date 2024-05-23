const std = @import("std");
const AnyReader = std.io.AnyReader;
const instruction = @import("instruction.zig");
const Instruction = instruction.Instruction;
const Segment = instruction.Segment;
const SegmentWithConstant = instruction.SegmentWithConstant;
const LabelWithConstant = instruction.LabelWithConstant;

const MAX_TOKEN_LENGTH = 256;

const Parser = @This();

// buffer holding the last parsed token
token_buffer: [MAX_TOKEN_LENGTH]u8 = [_]u8{0} ** MAX_TOKEN_LENGTH,

// buffer holding the current label, if any
label_buffer: [MAX_TOKEN_LENGTH]u8 = [_]u8{0} ** MAX_TOKEN_LENGTH,

current_line: usize = 0,

reader: AnyReader,

pub fn init(reader: AnyReader) Parser {
    return .{ .reader = reader };
}

// parse the next instruction.
//
// calling this method invalidates all previously generated instructions.
// destroying the parser also does this
//
// returns null if there are no more bytes to read
pub fn next(self: *Parser) !?Instruction {
    const command_token = try self.readToken() orelse return null;

    const command = instruction.command_token_map.get(command_token) orelse
        return self.invalidCommand("expected command found '{s}'", .{command_token});

    const inst = switch (command) {
        inline .pop,
        .push,
        => |tag| @unionInit(
            Instruction,
            @tagName(tag),
            try self.parseSegmentWithConstant(),
        ),
        inline .add,
        .sub,
        .neg,
        .eq,
        .gt,
        .lt,
        .@"and",
        .@"or",
        .not,
        .@"return",
        => |tag| @unionInit(
            Instruction,
            @tagName(tag),
            {},
        ),
        inline .label,
        .goto,
        .@"if-goto",
        => |tag| @unionInit(Instruction, @tagName(tag), try self.parseLabel()),
        inline .call,
        .function,
        => |tag| @unionInit(Instruction, @tagName(tag), try self.parseLabelWithConstant()),
    };

    return inst;
}

fn parseSegmentWithConstant(self: *Parser) !SegmentWithConstant {
    const segment_token = try self.readToken() orelse
        return self.invalidCommand("expected a segment but found eof instead", .{});

    const segment = instruction.segment_token_map.get(segment_token) orelse
        return self.invalidCommand("expected segment found '{s}'", .{segment_token});

    const constant_token = try self.readToken() orelse
        return self.invalidCommand("expected a number but found eof instead", .{});

    const constant = std.fmt.parseUnsigned(u16, constant_token, 0) catch |err| switch (err) {
        error.Overflow => return self.invalidCommand(
            "numbers must fit into 16-bit, but {s} doesnt",
            .{constant_token},
        ),
        error.InvalidCharacter => return self.invalidCommand(
            "expected a number but found '{s}' instead",
            .{constant_token},
        ),
    };

    return .{
        .segment = segment,
        .constant = constant,
    };
}

fn parseLabel(self: *Parser) ![]const u8 {
    const token = try self.readToken() orelse
        return error.InvalidCommand;

    @memcpy(self.label_buffer[0..token.len], token);

    return self.label_buffer[0..token.len];
}

fn parseLabelWithConstant(self: *Parser) !LabelWithConstant {
    const label = try self.parseLabel();

    const constant_token = try self.readToken() orelse
        return self.invalidCommand("expected a number but found eof instead", .{});

    const constant = std.fmt.parseUnsigned(u16, constant_token, 0) catch |err| switch (err) {
        error.Overflow => return self.invalidCommand(
            "numbers must fit into 16-bit, but {s} doesnt",
            .{constant_token},
        ),
        error.InvalidCharacter => return self.invalidCommand(
            "expected a number but found '{s}' instead",
            .{constant_token},
        ),
    };

    return .{
        .label = label,
        .constant = constant,
    };
}

fn invalidCommand(self: *const Parser, comptime fmt: []const u8, args: anytype) anyerror {
    std.log.err("line {}: invalid command, " ++ fmt, .{self.current_line} ++ args);
    return error.InvalidCommand;
}

// read the next token (words between spaces/newlines/comment) into the internal buffer
// and return a slice to it
//
// returns null if no more bytes could be read
fn readToken(
    self: *Parser,
) !?[]const u8 {
    const TokenizeState = enum { start, token, comment };

    var state = TokenizeState.start;

    var index: u8 = 0;
    while (true) {
        const byte = self.reader.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };

        switch (byte) {
            ' ', '\t', '\r' => switch (state) {
                .token => break,
                else => {},
            },
            '\n' => {
                self.current_line += 1;
                switch (state) {
                    .token => break,
                    .comment => state = .start,
                    else => {},
                }
            },
            '/' => switch (state) {
                .token => break,
                else => state = .comment,
            },
            else => switch (state) {
                .comment => continue,
                else => {
                    state = .token;
                    self.token_buffer[index] = byte;
                    index +%= 1; // too long tokens just get fucked up hehe
                },
            },
        }
    }

    if (index == 0) {
        return null;
    }

    return self.token_buffer[0..index];
}

// unit tests

const testing = std.testing;

fn testTokenizer(expected: []const u8, input: []const u8) !void {
    var fbs = std.io.fixedBufferStream(input);

    var parser = Parser.init(fbs.reader().any());
    const actual = try parser.readToken() orelse return error.TestUnexpectedResult;

    try testing.expectEqualStrings(expected, actual);
}

test "parser.readToken" {
    try testTokenizer("token", "token");
    try testTokenizer("token", "   token   garbage");
    try testTokenizer("token", "// i am a comment\n   token");
    try testTokenizer("token", "\n\ntoken\n");
    try testTokenizer("token", "\r\ntoken\r\n"); // diesesmal hab ich tests dafür ;)
    try testTokenizer("1343", "1343");
    try testTokenizer("1343", " 1343 ");
}

fn testParser(expected: Instruction, input: []const u8) !void {
    var fbs = std.io.fixedBufferStream(input);

    var parser = Parser.init(fbs.reader().any());
    const actual = try parser.next() orelse return error.TestUnexpectedResult;

    try testing.expectEqualDeep(expected, actual);
}

test "parser.next push/pop" {
    try testParser(
        .{ .push = .{ .segment = .constant, .constant = 42 } },
        "push constant 42",
    );
    try testParser(
        .{ .pop = .{ .segment = .local, .constant = 1 } },
        "pop local 1",
    );
}

test "parser.next parseLabel" {
    try testParser(
        .{ .label = "mylabel" },
        "label mylabel",
    );
}

test "parser.next function" {
    try testParser(
        .{ .function = .{ .label = "mylabel", .constant = 42 } },
        "function mylabel 42",
    );
}
