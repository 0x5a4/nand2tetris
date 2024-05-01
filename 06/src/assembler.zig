const std = @import("std");
const parser = @import("parser.zig");

const Allocator = std.mem.Allocator;
const SymbolTable = parser.SymbolTable;
const ParseResult = parser.ParseResult;
const Instruction = @import("instruction.zig").Instruction;

pub fn assemble(alloc: Allocator, text: []const u8, writer: anytype) !void {
    var parsed = try parser.parse(alloc, text);
    defer parsed.deinit(alloc);

    var symbol_counter: u15 = 16;
    var symbols = SymbolTable.init(alloc);
    defer symbols.deinit();

    for (parsed.instructions) |instruction| {
        const raw = try as_bytes(
            &instruction,
            &parsed.labels,
            &symbols,
            &symbol_counter,
        );

        try writer.print("{b:0>16}\n", .{raw});
    }
}

fn as_bytes(
    instruction: *const Instruction,
    labels: *const SymbolTable,
    symbols: *SymbolTable,
    symbol_counter: *u15,
) !u16 {
    switch (instruction.*) {
        Instruction.c => |inst| {
            const raw: u13 = @bitCast(inst);
            return 0b111 << 13 | @as(u16, raw);
        },
        Instruction.a => |inst| {
            const addr = switch (inst) {
                .number => |num| num,
                .label => |label| try resolve_label(
                    label,
                    labels,
                    symbols,
                    symbol_counter,
                ),
            };

            return addr;
        },
    }
}

fn resolve_label(
    label: []const u8,
    labels: *const SymbolTable,
    symbols: *SymbolTable,
    symbol_counter: *u15,
) !u16 {
    if (labels.get(label)) |addr| {
        return addr.?;
    }

    // its a symbol. if we know it, retrieve it otherwise
    // create a new one
    const entry = try symbols.getOrPut(label);

    if (!entry.found_existing) {
        entry.value_ptr.* = symbol_counter.*;
        symbol_counter.* += 1;
    }

    return entry.value_ptr.*.?;
}

const testing = std.testing;

fn expectHack(assembly: []const u8, expected: []const u8) !void {
    var result = std.ArrayList(u8).init(testing.allocator);
    defer result.deinit();

    try assemble(testing.allocator, assembly, result.writer());

    try testing.expectEqualStrings(expected, result.items);
}

test "assemble add.hack" {
    const assembly =
        \\// Computes R0 = 2 + 3  (R0 refers to RAM[0])
        \\
        \\@2
        \\D=A
        \\@3
        \\D=D+A
        \\@0
        \\M=D
    ;

    const hack =
        \\0000000000000010
        \\1110110000010000
        \\0000000000000011
        \\1110000010010000
        \\0000000000000000
        \\1110001100001000
        \\
    ;

    try expectHack(assembly, hack);
}

test "assemble max.hack" {
    const assembly =
        \\  // D = R0 - R1
        \\  @R0
        \\  D=M
        \\  @R1
        \\  D=D-M
        \\  // If (D > 0) goto ITSR0
        \\  @ITSR0
        \\  D;JGT
        \\  // Its R1
        \\  @R1
        \\  D=M
        \\  @OUTPUT_D
        \\  0;JMP
        \\(ITSR0)
        \\  @R0
        \\  D=M
        \\(OUTPUT_D)
        \\  @R2
        \\  M=D
        \\(END)
        \\  @END
        \\  0;JMP
    ;

    const hack =
        \\0000000000000000
        \\1111110000010000
        \\0000000000000001
        \\1111010011010000
        \\0000000000001010
        \\1110001100000001
        \\0000000000000001
        \\1111110000010000
        \\0000000000001100
        \\1110101010000111
        \\0000000000000000
        \\1111110000010000
        \\0000000000000010
        \\1110001100001000
        \\0000000000001110
        \\1110101010000111
        \\
    ;

    try expectHack(assembly, hack);
}

test "assemble rect.hack" {
    const assembly =
        \\// If (R0 <= 0) goto END else n = R0
        \\   @R0
        \\   D=M
        \\   @END
        \\   D;JLE 
        \\   @n
        \\   M=D
        \\   // addr = base address of first screen row
        \\   @SCREEN
        \\   D=A
        \\   @addr
        \\   M=D
        \\(LOOP)
        \\   // RAM[addr] = -1
        \\   @addr
        \\   A=M
        \\   M=-1
        \\   // addr = base address of next screen row
        \\   @addr
        \\   D=M
        \\   @32
        \\   D=D+A
        \\   @addr
        \\   M=D
        \\   // decrements n and loops
        \\   @n
        \\   MD=M-1
        \\   @LOOP
        \\   D;JGT
        \\(END)
        \\   @END
        \\   0;JMP
    ;

    const hack =
        \\0000000000000000
        \\1111110000010000
        \\0000000000010111
        \\1110001100000110
        \\0000000000010000
        \\1110001100001000
        \\0100000000000000
        \\1110110000010000
        \\0000000000010001
        \\1110001100001000
        \\0000000000010001
        \\1111110000100000
        \\1110111010001000
        \\0000000000010001
        \\1111110000010000
        \\0000000000100000
        \\1110000010010000
        \\0000000000010001
        \\1110001100001000
        \\0000000000010000
        \\1111110010011000
        \\0000000000001010
        \\1110001100000001
        \\0000000000010111
        \\1110101010000111
        \\
    ;

    try expectHack(assembly, hack);
}
