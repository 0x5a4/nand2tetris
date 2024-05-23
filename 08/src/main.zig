const std = @import("std");
const io = std.io;
const fs = std.fs;
const Dir = fs.Dir;
const AnyWriter = io.AnyWriter;

const instruction = @import("instruction.zig");
const CodeGen = @import("CodeGen.zig");
const Parser = @import("Parser.zig");

pub const std_options: std.Options = .{
    .log_level = .info,
};

pub fn main() !u8 {
    // woopsie, this doesnt work on windows
    var args = std.process.args();
    _ = args.skip(); // skip binary name

    const inputPath = args.next() orelse return usage();
    const outputPath = args.next() orelse return usage();
    const cwd = fs.cwd();

    // open output file
    const outputFile = cwd.createFile(outputPath, .{}) catch |err| {
        std.log.err("unable to create output file: {}", .{err});
        return 1;
    };
    defer outputFile.close();

    var bufferedWriter = io.bufferedWriter(outputFile.writer());
    defer bufferedWriter.flush() catch {};
    const writer = bufferedWriter.writer().any();

    // emit bootstrap code
    emitBootstrap(writer) catch |err| {
        std.log.err("unable to write bootstrap code: {}", .{err});
        return 1;
    };

    const input_stat = cwd.statFile(inputPath) catch |err| {
        std.log.err("while stating input path: {}", .{err});
        return 1;
    };

    std.log.info("output file {s}", .{fs.path.basename(outputPath)});

    switch (input_stat.kind) {
        .file => compileFile(&cwd, inputPath, writer) catch |err| {
            std.log.err("while compiling input file: {}", .{err});
            return 1;
        },
        .directory => compileDir(&cwd, inputPath, writer) catch |err| {
            std.log.err("while compiling input folder: {}", .{err});
            return 1;
        },
        // symlinks? what is that?
        else => unreachable,
    }

    return 0;
}

fn compileDir(base_dir: *const Dir, path: []const u8, writer: AnyWriter) !void {
    // yes this is kind of a race condition
    const dir = try base_dir.openDir(path, .{
        .iterate = true,
    });

    var dir_iter = dir.iterateAssumeFirstIteration();

    while (try dir_iter.next()) |entry| {
        if (entry.kind != .file) {
            continue;
        }

        const extension = fs.path.extension(entry.name);

        if (!std.mem.eql(u8, extension, ".vm")) {
            continue;
        }

        compileFile(&dir, entry.name, writer) catch |err| {
            std.log.err("while compiling file {s}: {}", .{ entry.name, err });
            return err;
        };
    }
}

fn compileFile(dir: *const Dir, path: []const u8, writer: AnyWriter) !void {
    std.log.info("compiling file {s}", .{path});

    const inputFile = try dir.openFile(path, .{});
    defer inputFile.close();

    var bufferedReader = io.bufferedReader(inputFile.reader());
    const reader = bufferedReader.reader().any();

    const basename = fs.path.basename(path);
    const class_name = fs.path.stem(basename);

    var parser = Parser.init(reader);
    var codegen = CodeGen.init(writer, class_name);
    try codegen.beginClass();

    while (try parser.next()) |inst| {
        try codegen.emit(&inst);
    }
}

fn emitBootstrap(writer: AnyWriter) !void {
    // zig fmt: off
    try writer.writeAll(
        // since we are going to call Sys.init, we need a frame.
        // but we dont have anything useful to put there, so we just
        // put -1 as the return address to guarantee a crash. technically we
        // could just ignore it, but the tests dont seem to like that
        \\@256
        \\M=-1
        // set SP and LCL to 261, to pretend like we have a proper call stack
        \\@261
        \\D=A
        \\@SP
        \\M=D
        \\@LCL
        \\M=D
        //and jump!
        \\@FUNC_Sys.init
        \\0;JMP
        \\
    );
    // zig fmt: on
}

fn usage() u8 {
    std.debug.print(
        \\usage: hackvmc <INPUT> <OUTPUT>
        \\ 
        \\<INPUT>  - can be either be a folder or a file. for a given folder,
        \\           all .vm files will be compiled.
        \\
        \\<OUTPUT> - path of the output file
        \\
    , .{});
    return 1;
}

test {
    std.testing.refAllDecls(instruction);
    std.testing.refAllDecls(Parser);
    std.testing.refAllDecls(CodeGen);
}
