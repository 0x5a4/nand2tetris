const std = @import("std");
const path = std.fs.path;

const instruction = @import("instruction.zig");
const assembler = @import("assembler.zig");
const parser = @import("parser.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();

    var args = std.process.args();
    _ = args.skip(); // skip binary name

    const firstArg = args.next() orelse {
        std.debug.print("usage: hackas <asm file>", .{});
        return;
    };

    const cwd = std.fs.cwd();

    const inputFile = try cwd.readFileAlloc(alloc, firstArg, 1024 * 1024 * 4);
    defer alloc.free(inputFile);

    const outputFile = try cwd.createFile("out.hack", .{});
    defer outputFile.close();

    try assembler.assemble(alloc, inputFile, outputFile.writer());
}

test {
    _ = std.testing.refAllDecls(parser);
    _ = std.testing.refAllDecls(assembler);
    _ = std.testing.refAllDecls(instruction);
}
