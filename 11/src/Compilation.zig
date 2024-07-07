const std = @import("std");

const Allocator = std.mem.Allocator;
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const Serializer = @import("Serializer.zig");
const CodeGen = @import("CodeGen.zig");
const vm = @import("vm.zig");

const Self = @This();

alloc: Allocator,
errors: std.ArrayList(Error),

pub fn init(alloc: Allocator) Self {
    return .{
        .alloc = alloc,
        .errors = std.ArrayList(Error).init(alloc),
    };
}

pub fn deinit(self: *Self) void {
    self.errors.deinit();
    self.* = undefined;
}

pub const SrcLoc = struct {
    line: u32,
    char: u32,
    file: []const u8,
};

pub const Error = struct {
    tag: Tag,
    msg: []const u8,
    src_loc: SrcLoc,

    pub const Tag = Parser.Error;
};

pub fn hasError(self: *const Self) bool {
    return self.errors.items.len != 0;
}

pub fn printErrors(self: *const Self) void {
    for (self.errors.items) |e| {
        std.log.err("{s}:{}:{} - {} {s}", .{
            e.src_loc.file,
            e.src_loc.line,
            e.src_loc.char,
            e.tag,
            e.msg,
        });
    }
}

pub fn dumpAst(self: *Self, dir: *const std.fs.Dir, path: []const u8) !void {
    const basename = std.fs.path.basename(path);
    const stem = std.fs.path.stem(basename);
    const outputPath = try std.mem.concat(self.alloc, u8, &.{ stem, ".xml" });
    defer self.alloc.free(outputPath);

    std.log.info("Ast '{s}' -> '{s}'", .{ basename, outputPath });

    const text = try dir.readFileAlloc(self.alloc, path, 4e+9);
    defer self.alloc.free(text);

    const outputFile = try dir.createFile(outputPath, .{});
    defer outputFile.close();

    var writer = std.io.bufferedWriter(outputFile.writer());
    const anyWriter = writer.writer().any();

    var ast = try Parser.parse(self, basename, text);
    defer ast.deinit();

    try Serializer.serialize(&ast, anyWriter, text);

    try writer.flush();
}

pub fn compile(self: *Self, dir: *const std.fs.Dir, path: []const u8) !void {
    const basename = std.fs.path.basename(path);
    const stem = std.fs.path.stem(basename);
    const outputPath = try std.mem.concat(self.alloc, u8, &.{ stem, ".out.vm" });
    defer self.alloc.free(outputPath);

    std.log.info("Compile '{s}' -> '{s}'", .{ basename, outputPath });

    const text = try dir.readFileAlloc(self.alloc, path, 4e+9);
    defer self.alloc.free(text);

    const outputFile = try dir.createFile(outputPath, .{});
    defer outputFile.close();

    var writer = std.io.bufferedWriter(outputFile.writer());
    const anyWriter = writer.writer().any();

    var ast = try Parser.parse(self, basename, text);
    defer ast.deinit();

    var codegen = CodeGen.init(self, &ast, text);
    defer codegen.deinit();

    try codegen.emitClass();

    for (codegen.instructions.items) |inst| {
        try vm.emitInstruction(anyWriter, inst);
    }

    try writer.flush();
}
