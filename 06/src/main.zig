const std = @import("std");
const path = std.fs.path;

const instruction = @import("instruction.zig");
const assembler = @import("assembler.zig");
const parser = @import("parser.zig");

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();

    var args = std.process.args();
    _ = args.skip(); // skip binary name

    const firstArg = args.next() orelse {
        std.debug.print("usage: hackas <asm file>", .{});
        return 1;
    };

    const realpath = try std.fs.realpathAlloc(alloc, firstArg);
    defer alloc.free(realpath);
    std.debug.print("assembling {s}\n", .{realpath});

    const cwd = std.fs.cwd();

    const inputFile = try cwd.readFileAlloc(alloc, realpath, 1024 * 1024 * 4);
    defer alloc.free(inputFile);

    const dirname = path.dirname(realpath) orelse return 1;
    
    const stem = path.stem(realpath);

    const output_name = try std.mem.join(alloc, "", &[_][]const u8{stem, ".hack"});
    defer alloc.free(output_name);

    const output_path = try path.join(alloc, &[_][]const u8{dirname, output_name});
    defer alloc.free(output_path);
    std.debug.print("output file: {s}\n", .{output_path});

    const outputFile = try cwd.createFile(output_path, .{});
    defer outputFile.close();

    try assembler.assemble(alloc, inputFile, outputFile.writer());

    return 0;
}

test {
    _ = std.testing.refAllDecls(parser);
    _ = std.testing.refAllDecls(assembler);
    _ = std.testing.refAllDecls(instruction);
}
