const std = @import("std");
const instruction = @import("instruction.zig");

const Allocator = std.mem.Allocator;
const AInstruction = instruction.AInstruction;
const CInstruction = instruction.CInstruction;
const Computation = instruction.Computation;
const Destination = instruction.Destination;
const Instruction = instruction.Instruction;
const Jump = instruction.Jump;

pub const SymbolTable = std.StringHashMap(?u16);

pub const ParseError = error{
    IllegalInstruction,
} || Allocator.Error;

// map expressions to their binary representation in a quick and dirty way
// so we dont have to do any actual parsing
pub const computation_map = std.ComptimeStringMap(Computation, .{
    .{ "0", .zero },
    .{ "1", .one },
    .{ "-1", .minus_one },
    .{ "D", .d },
    .{ "A", .a },
    .{ "M", .m },
    .{ "!D", .not_d },
    .{ "!A", .not_a },
    .{ "!M", .not_m },
    .{ "-D", .negative_d },
    .{ "-A", .negative_a },
    .{ "-M", .negative_m },
    .{ "D+1", .d_plus_one },
    .{ "A+1", .a_plus_one },
    .{ "M+1", .m_plus_one },
    .{ "D-1", .d_minus_one },
    .{ "A-1", .a_minus_one },
    .{ "M-1", .m_minus_one },
    .{ "D+A", .d_plus_a },
    .{ "D+M", .d_plus_m },
    .{ "D-A", .d_minus_a },
    .{ "D-M", .d_minus_m },
    .{ "A-D", .a_minus_d },
    .{ "M-D", .m_minus_d },
    .{ "D&A", .d_and_a },
    .{ "D&M", .d_and_m },
    .{ "D|A", .d_or_a },
    .{ "D|M", .d_or_m },
});

pub const jump_map = std.ComptimeStringMap(Jump, .{
    .{ "JMP", .jump },
    .{ "JNE", .not_equal },
    .{ "JLT", .less },
    .{ "JLE", .less_equal },
    .{ "JEQ", .equal },
    .{ "JGE", .greater_equal },
    .{ "JGT", .greater },
});

/// state used during parsing, mostly for labels
const ParserState = struct {
    labels: SymbolTable,
    instruction_pointer: u16 = 0,

    pub fn init(alloc: Allocator) !@This() {
        return .{
            .labels = try buildLabelTable(alloc),
        };
    }
    
    pub fn deinit(self: *@This()) void {
        self.labels.deinit();
        self.* = undefined;
    }
};

pub const ParseResult = struct {
    labels: SymbolTable,
    instructions: []const Instruction,

    pub fn deinit(self: *@This(), alloc: Allocator) void {
        self.labels.deinit();
        alloc.free(self.instructions);
        self.* = undefined;
    }
};

pub fn parse(alloc: Allocator, text: []const u8) !ParseResult {
    var state = try ParserState.init(alloc);
    errdefer state.deinit();

    var instructions = std.ArrayList(Instruction).init(alloc);
    defer instructions.deinit();

    var spliterator = std.mem.splitScalar(u8, text, '\n');

    while (spliterator.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " ");

        // strip trailing whitespace / comments
        const end = std.mem.indexOfAny(u8, trimmed, " /") orelse trimmed.len;
        const instruction_string = trimmed[0..end];

        // skip empty lines
        if (instruction_string.len == 0) {
            continue;
        }

        switch (instruction_string[0]) {
            '(' => try parseLabel(&state, instruction_string),
            '@' => {
                const x = try parseAInstruction(&state, instruction_string);
                try instructions.append(x);
            },
            else => {
                const x = try parseCInstruction(&state, instruction_string);
                try instructions.append(x);
            },
        }
    }

    return .{
        .labels = state.labels,
        .instructions = try instructions.toOwnedSlice(),
    };
}

fn parseAInstruction(state: *ParserState, line: []const u8) !Instruction {
    state.instruction_pointer += 1;

    // requires at least @x
    if (line.len < 2) {
        return error.IllegalInstruction;
    }

    const value = line[1..];

    // if value starts with a letter its a label, otherwise its
    // a number literal
    const inst = blk: {
        if (std.ascii.isAlphabetic(value[0])) {
            break :blk AInstruction{
                .label = value,
            };
        } else {
            const num = std.fmt.parseInt(u15, value, 10) catch
                return error.IllegalInstruction;

            break :blk AInstruction{
                .number = num,
            };
        }
    };

    return Instruction{
        .a = inst,
    };
}

fn parseCInstruction(state: *ParserState, line: []const u8) !Instruction {
    std.debug.assert(line.len != 0); // empty lines were already skipped

    state.instruction_pointer += 1;

    var pointer: usize = 0;
    var result = CInstruction{
        .comp = undefined,
    };

    // parse destination if an equal sign exists.
    // also advance the pointer to the char after it
    if (std.mem.indexOf(u8, line, "=")) |index| {
        for (line) |char| {
            switch (char) {
                'A' => result.dest.a = true,
                'M' => result.dest.m = true,
                'D' => result.dest.d = true,
                '=', ' ' => break,
                else => return error.IllegalInstruction,
            }
        }

        pointer += index + 1;
    }

    // parse computation
    result.comp = try parseComputation(line[pointer..]);

    // parse jump
    if (std.mem.indexOf(u8, line, ";")) |index| {
        const jump_expr = std.mem.trim(u8, line[(index + 1)..], " ");
        result.jump = jump_map.get(jump_expr) orelse return error.IllegalInstruction;
    }

    return Instruction{ .c = result };
}

fn parseComputation(line: []const u8) !Computation {
    var pointer: usize = 0;
    var buffer = [_]u8{0} ** 3;

    for (line) |char| {
        const x = switch (char) {
            ' ' => continue,
            ';' => break,
            else => char,
        };

        if (pointer >= buffer.len) {
            return error.IllegalInstruction;
        }

        buffer[pointer] = x;
        pointer += 1;
    }

    return computation_map.get(buffer[0..pointer]) orelse return error.IllegalInstruction;
}

fn parseLabel(state: *ParserState, line: []const u8) !void {
    const end = std.mem.indexOf(u8, line, ")") orelse return ParseError.IllegalInstruction;
    const label = line[1..end];

    try state.labels.put(label, state.instruction_pointer);
}

pub fn buildLabelTable(alloc: Allocator) !SymbolTable {
    var table = SymbolTable.init(alloc);
    errdefer table.deinit();

    try table.put("R0", 0);
    try table.put("R1", 1);
    try table.put("R2", 2);
    try table.put("R3", 3);
    try table.put("R4", 4);
    try table.put("R5", 5);
    try table.put("R6", 6);
    try table.put("R7", 7);
    try table.put("R8", 8);
    try table.put("R9", 9);
    try table.put("R10", 10);
    try table.put("R11", 11);
    try table.put("R12", 12);
    try table.put("R13", 13);
    try table.put("R14", 14);
    try table.put("R15", 15);
    try table.put("SCREEN", 0x4000);
    try table.put("KBD", 0x6000);
    try table.put("SP", 0);
    try table.put("LCL", 1);
    try table.put("ARG", 2);
    try table.put("THIS", 3);
    try table.put("THAT", 4);

    return table;
}

// unit tests

const testing = std.testing;

fn expectAInstruction(input: []const u8, comptime expected: AInstruction) !void {
    var state = .{
        .labels = undefined,
    };

    const actual = try parseAInstruction(&state, input);

    try testing.expectEqualDeep(expected, actual.a);
}

fn expectCInstruction(input: []const u8, comptime expected: CInstruction) !void {
    var state = .{
        .labels = undefined,
    };

    const actual = try parseCInstruction(&state, input);

    try testing.expectEqualDeep(expected, actual.c);
}

test "parse a instruction" {
    try expectAInstruction("@17", .{ .number = 17 });
    try expectAInstruction("@index", .{ .label = "index" });
}

test "parse computation" {
    try testing.expectEqual(try parseComputation("0"), .zero);
    try testing.expectEqual(try parseComputation("D   & A"), .d_and_a);
    try testing.expectEqual(try parseComputation("A-D; JIBBERISH"), .a_minus_d);
    try testing.expectError(error.IllegalInstruction, parseComputation("D+A+1"));
    try testing.expectError(error.IllegalInstruction, parseComputation(""));
    try testing.expectError(error.IllegalInstruction, parseComputation("   "));
}

test "parse c instruction" {
    try expectCInstruction("D", .{ .comp = .d });
    try expectCInstruction("D=M", .{ .dest = .{ .d = true }, .comp = .m });
    try expectCInstruction("D =  M", .{ .dest = .{ .d = true }, .comp = .m });
    try expectCInstruction("D;JEQ", .{ .comp = .d, .jump = .equal });
    try expectCInstruction("D = 0; JMP", .{ .dest = .{ .d = true }, .comp = .zero, .jump = .jump });
}
