const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Ast = @import("Ast.zig");
const Parser = @import("Parser.zig");
const Serializer = @import("Serializer.zig");
const Compilation = @import("Compilation.zig");

const Allocator = std.mem.Allocator;

pub const std_options: std.Options = .{
    .log_level = .info,
};

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const arena_alloc = arena.allocator();
    defer arena.deinit();

    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len != 2) {
        return usage();
    }

    const cwd = std.fs.cwd();
    const inputPath = args[1];

    var comp = Compilation.init(alloc, arena_alloc);
    defer comp.deinit();

    const input_stat = cwd.statFile(inputPath) catch |err| {
        std.log.err("while stating input path: {}", .{err});
        return 1;
    };

    switch (input_stat.kind) {
        .file => comp.compile(&cwd, inputPath) catch |err| {
            comp.printErrors();
            return err;
        },
        .directory => try compileDir(&comp, &cwd, inputPath),
        else => unreachable,
    }

    return 0;
}

fn compileDir(
    compilation: *Compilation,
    base: *const std.fs.Dir,
    inputPath: []const u8,
) !void {
    const dir = try base.openDir(inputPath, .{
        .iterate = true,
    });

    var dir_iter = dir.iterateAssumeFirstIteration();

    while (try dir_iter.next()) |entry| {
        if (entry.kind != .file) {
            continue;
        }

        const extension = std.fs.path.extension(entry.name);

        if (!std.mem.eql(u8, extension, ".jack")) {
            continue;
        }

        compilation.compile(&dir, entry.name) catch |err| {
            compilation.printErrors();
            return err;
        };
    }
}

fn usage() u8 {
    std.debug.print(
        \\usage: jackc <INPUT>
        \\ 
        \\<INPUT>  - can be either be a folder or a file. for a given folder,
        \\           all .jack files will be compiled.
        \\
    , .{});
    return 1;
}

test {
    std.testing.refAllDecls(Tokenizer);
    std.testing.refAllDecls(Ast);
    std.testing.refAllDecls(Parser);
}
