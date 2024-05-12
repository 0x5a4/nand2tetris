const std = @import("std");
const io = std.io;
const fs = std.fs;
const instruction = @import("instruction.zig");
const CodeGen = @import("CodeGen.zig");
const Parser = @import("Parser.zig");

pub fn main() !u8 {
    var args = std.process.args();
    _ = args.skip(); // skip binary name

    const inputFileName = args.next() orelse {
        std.debug.print("usage: hackvm <vm file>", .{});
        return 1;
    };

    const outputFileName = args.next() orelse {
        std.debug.print("usage: hackvm <vm file> <output file>", .{});
        return 1;
    };

    const cwd = fs.cwd();

    const inputFile = cwd.openFile(inputFileName, .{}) catch |err| {
        std.log.err("input file could not be read: {}", .{err});
        return 1;
    };
    defer inputFile.close();

    const outputFile = cwd.createFile(outputFileName, .{}) catch |err| {
        std.log.err("output file could not be created: {}", .{err});
        return 1;
    };
    defer outputFile.close();

    var bufferedReader = io.bufferedReader(inputFile.reader());
    const reader = bufferedReader.reader().any();

    var bufferedWriter = io.bufferedWriter(outputFile.writer());
    defer bufferedWriter.flush() catch {};
    const writer = bufferedWriter.writer().any();

    const basename = std.fs.path.basename(inputFileName);

    var parser = Parser.init(reader);
    var codegen = CodeGen{ .class_name = basename };

    while (true) {
        const maybe_inst = try parser.next();

        if (maybe_inst) |inst| {
            try codegen.emit(&inst, writer);
        } else {
            break;
        }
    }

    return 0;
}

test {
    std.testing.refAllDecls(instruction);
    std.testing.refAllDecls(Parser);
    std.testing.refAllDecls(CodeGen);
}
