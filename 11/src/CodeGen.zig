const std = @import("std");
const Ast = @import("Ast.zig");
const Compilation = @import("Compilation.zig");
const vm = @import("vm.zig");

const Self = @This();

// testing
const testing = std.testing;

compilation: *Compilation,

text: []const u8,
tree: *const Ast,
node_pointer: u32 = 0,

class_symbols: SymbolTable,
method_symbols: SymbolTable,

// this is so ugly :(
class_name: []const u8 = undefined,

this_counter: u32 = 0,
static_counter: u32 = 0,
local_counter: u32 = 0,
label_counter: u32 = 0,

instructions: std.ArrayList(vm.Instruction),

const SymbolTable = std.ArrayList(Symbol);

const Symbol = struct {
    name: []const u8,
    type: []const u8,
    segment: vm.Segment,
    idx: u32,
};

const Error = std.mem.Allocator.Error;

pub fn init(compilation: *Compilation, tree: *const Ast, text: []const u8) Self {
    return .{
        .compilation = compilation,
        .text = text,
        .tree = tree,
        .class_symbols = SymbolTable.init(compilation.alloc),
        .method_symbols = SymbolTable.init(compilation.alloc),
        .instructions = std.ArrayList(vm.Instruction).init(compilation.alloc),
    };
}

pub fn deinit(self: *Self) void {
    self.instructions.deinit();
    self.class_symbols.deinit();
    self.method_symbols.deinit();

    self.* = undefined;
}

fn peekNode(self: *Self) Ast.Node {
    return self.tree.nodes.items[self.node_pointer];
}

fn popNode(self: *Self) Ast.Node {
    defer self.node_pointer += 1;
    return self.peekNode();
}

fn readStringLoc(self: *Self, index: u32) []const u8 {
    const strLoc = self.tree.strings.items[index];
    return self.text[strLoc.start..(strLoc.start + strLoc.len)];
}

fn lookupSymbol(self: *Self, name: []const u8) ?Symbol {
    for (self.method_symbols.items) |method_symbol| {
        if (std.mem.eql(u8, name, method_symbol.name)) {
            return method_symbol;
        }
    }

    for (self.class_symbols.items) |class_symbol| {
        if (std.mem.eql(u8, name, class_symbol.name)) {
            return class_symbol;
        }
    }

    return null;
}

pub fn emitClass(self: *Self) Error!void {
    const root_node = self.popNode();
    std.debug.assert(root_node.tag == .class_decl);

    // pfusch
    const identifer_node = self.popNode();
    self.class_name = self.readStringLoc(identifer_node.data.identifier);

    var child_count = root_node.data.class_decl - 1;

    while (child_count > 0) : (child_count -= 1) {
        const node = self.peekNode();

        switch (node.tag) {
            .static_decl, .field_decl => try self.emitClassVar(),
            .function_decl, .method_decl, .constructor_decl => try self.emitFunctionDecl(),
            else => unreachable,
        }
    }
}

fn emitClassVar(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .field_decl or node.tag == .static_decl);

    var child_count = switch (node.tag) {
        .field_decl => node.data.field_decl,
        .static_decl => node.data.static_decl,
        else => unreachable,
    } - 1;

    const segment: vm.Segment = switch (node.tag) {
        .field_decl => .this,
        .static_decl => .static,
        else => unreachable,
    };

    const segmentCounter = switch (node.tag) {
        .field_decl => &self.this_counter,
        .static_decl => &self.static_counter,
        else => unreachable,
    };

    const typeNode = self.popNode();
    const varType = self.readStringLoc(typeNode.data.identifier);

    while (child_count > 0) : (child_count -= 1) {
        const identifierNode = self.popNode();
        const identifier = self.readStringLoc(identifierNode.data.identifier);

        const symbol = Symbol{
            .name = identifier,
            .type = varType,
            .segment = segment,
            .idx = segmentCounter.*,
        };

        segmentCounter.* += 1;

        try self.class_symbols.append(symbol);
    }
}

fn emitFunctionDecl(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .constructor_decl or node.tag == .function_decl or node.tag == .method_decl);

    var child_count = switch (node.tag) {
        .constructor_decl => node.data.constructor_decl,
        .method_decl => node.data.method_decl,
        .function_decl => node.data.function_decl,
        else => unreachable,
    } - 3;

    _ = self.popNode(); // pop return type

    const identifier_node = self.popNode();
    const func_identifier = self.readStringLoc(identifier_node.data.identifier);
    const func_name = try std.fmt.allocPrint(
        self.compilation.arena,
        "{s}.{s}",
        .{ self.class_name, func_identifier },
    );

    var arg_counter: u32 = 0;

    // push this if necessary
    if (node.tag == .method_decl) {
        const thisSymbol = Symbol{
            .name = "this",
            .type = self.class_name,
            .idx = 0,
            .segment = .argument,
        };

        try self.method_symbols.append(thisSymbol);

        arg_counter += 1;
    }

    // traverse parameters
    while (child_count > 0) : (child_count -= 2) {
        const typeNode = self.popNode();
        const parameterType = self.readStringLoc(typeNode.data.identifier);

        const identifierNode = self.popNode();
        const identifier = self.readStringLoc(identifierNode.data.identifier);

        const symbol = Symbol{
            .name = identifier,
            .type = parameterType,
            .segment = .argument,
            .idx = arg_counter,
        };

        try self.method_symbols.append(symbol);

        arg_counter += 1;
    }

    // emit the function declaration and store its index, because we retroactively patch
    // in the amount of local variables
    const func_instruction_index = self.instructions.items.len;
    try self.instructions.append(.{
        .function = .{
            .label = func_name,
            .constant = 0,
        },
    });

    // if its a method, load this into pointer 0
    if (node.tag == .method_decl) {
        try self.instructions.appendSlice(&.{
            .{
                .push = .{
                    .segment = .argument,
                    .constant = 0,
                },
            },
            .{
                .pop = .{
                    .segment = .pointer,
                    .constant = 0,
                },
            },
        });
    }

    // if its a constructor, create this object
    if (node.tag == .constructor_decl) {
        const thisSymbol = Symbol{
            .name = "this",
            .type = self.class_name,
            .segment = .local,
            .idx = @intCast(self.method_symbols.items.len),
        };
        try self.method_symbols.append(thisSymbol);

        try self.instructions.appendSlice(&.{
            .{
                .push = .{
                    .segment = .constant,
                    .constant = @intCast(self.class_symbols.items.len),
                },
            },
            .{
                .call = .{
                    .label = "Memory.alloc",
                    .constant = 1,
                },
            },
            .{
                .pop = .{
                    .segment = .pointer,
                    .constant = 0,
                },
            },
        });
    }

    try self.emitFunctionBody();

    // patch in local variable count
    const func_instruction = &self.instructions.items[func_instruction_index];
    func_instruction.*.function.constant = @intCast(self.local_counter);

    self.method_symbols.clearRetainingCapacity();
    self.local_counter = 0;
}

fn emitFunctionBody(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .function_body);

    var child_count = node.data.function_body - 1;
    while (child_count > 0) : (child_count -= 1) {
        try self.emitVarDecl();
    }

    try self.emitStatementBlock();
}

fn emitVarDecl(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .var_decl);

    var child_count = node.data.var_decl - 1;

    const typeNode = self.popNode();
    const varType = self.readStringLoc(typeNode.data.identifier);

    while (child_count > 0) : (child_count -= 1) {
        const identifierNode = self.popNode();
        const identifier = self.readStringLoc(identifierNode.data.identifier);

        const symbol = Symbol{
            .name = identifier,
            .type = varType,
            .segment = .local,
            .idx = self.local_counter,
        };

        self.local_counter += 1;

        try self.method_symbols.append(symbol);
    }
}

fn emitStatementBlock(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .statement_block);

    var child_count = node.data.statement_block;

    while (child_count > 0) : (child_count -= 1) {
        const statementNode = self.peekNode();

        switch (statementNode.tag) {
            .let_statement => try self.emitLetStatement(),
            .do_statement => try self.emitDoStatement(),
            .if_statement => try self.emitIfStatement(),
            .while_statement => try self.emitWhileStatement(),
            .return_statement => try self.emitReturnStatement(),
            else => unreachable,
        }
    }
}

fn emitReturnStatement(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .return_statement);

    if (node.data.return_statement) {
        try self.emitExpression();
    } else {
        try self.instructions.append(.{
            .push = .{
                .segment = .constant,
                .constant = 0,
            },
        });
    }

    try self.instructions.append(.{ .@"return" = {} });
}

fn emitLetStatement(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .let_statement);

    const lhs = self.popNode();
    switch (lhs.tag) {
        .identifier => {
            const identifier = self.readStringLoc(lhs.data.identifier);
            const symbol = self.lookupSymbol(identifier) orelse
                unreachable; // TODO: handle the error

            try self.emitExpression();

            try self.instructions.append(.{
                .pop = .{
                    .segment = symbol.segment,
                    .constant = @intCast(symbol.idx),
                },
            });
        },
        .array_access => {
            const identifier = self.readStringLoc(lhs.data.array_access);
            const symbol = self.lookupSymbol(identifier) orelse
                unreachable; // TODO: handle the error

            try self.emitExpression();

            try self.instructions.append(.{
                .push = .{
                    .segment = symbol.segment,
                    .constant = @intCast(symbol.idx),
                },
            });

            try self.instructions.appendSlice(&.{
                .{ .add = {} },
            });

            try self.emitExpression();

            // load temp 0 into pointer 1, then load the result
            // into that
            try self.instructions.appendSlice(&.{
                .{
                    .pop = .{
                        .segment = .temp,
                        .constant = 0,
                    },
                },
                .{
                    .pop = .{
                        .segment = .pointer,
                        .constant = 1,
                    },
                },
                .{
                    .push = .{
                        .segment = .temp,
                        .constant = 0,
                    },
                },
                .{
                    .pop = .{
                        .segment = .that,
                        .constant = 0,
                    },
                },
            });
        },
        else => unreachable,
    }
}

fn emitDoStatement(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .do_statement);

    try self.emitFunctionCall();

    try self.instructions.append(.{
        .pop = .{
            .segment = .temp,
            .constant = 0,
        },
    });
}

fn emitIfStatement(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .if_statement);

    try self.emitExpression();

    // construct a bunch of labels
    const true_branch = try std.fmt.allocPrint(
        self.compilation.arena,
        "{s}_IF_{}_TRUE",
        .{ self.class_name, self.label_counter },
    );

    const false_branch = try std.fmt.allocPrint(
        self.compilation.arena,
        "{s}_IF_{}_FALSE",
        .{ self.class_name, self.label_counter },
    );

    self.label_counter += 1;

    try self.instructions.appendSlice(&.{
        .{ .@"if-goto" = true_branch },
        .{ .goto = false_branch },
        .{ .label = true_branch },
    });

    try self.emitStatementBlock();

    // we have an else
    if (node.data.if_statement) {
        const end_branch = try std.fmt.allocPrint(
            self.compilation.arena,
            "{s}_IF_{}_END",
            .{ self.class_name, self.label_counter },
        );

        try self.instructions.appendSlice(&.{
            .{ .goto = end_branch },
            .{ .label = false_branch },
        });

        try self.emitStatementBlock();

        try self.instructions.append(.{
            .label = end_branch,
        });
    } else {
        try self.instructions.append(.{
            .label = false_branch,
        });
    }
}

fn emitWhileStatement(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .while_statement);

    const while_label = try std.fmt.allocPrint(
        self.compilation.arena,
        "{s}_WHILE_{}",
        .{ self.class_name, self.label_counter },
    );

    self.label_counter += 1;

    const break_label = try std.mem.concat(
        self.compilation.arena,
        u8,
        &.{
            while_label,
            "_BREAK",
        },
    );

    try self.instructions.append(.{
        .label = while_label,
    });

    try self.emitExpression();

    try self.instructions.appendSlice(&.{
        .{ .not = {} },
        .{ .@"if-goto" = break_label },
    });

    try self.emitStatementBlock();

    try self.instructions.appendSlice(&.{
        .{ .goto = while_label },
        .{ .label = break_label },
    });
}

fn emitExpression(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .expression);

    var child_count = node.data.expression;

    try self.emitTerm();

    while (child_count > 1) : (child_count -= 2) {
        // the ast stores operators in between their operand, which we cant use.
        // since the position of the next node is implied through the node pointer,
        // we need to manually move it around a bit.
        const op_pointer = self.node_pointer;
        self.node_pointer += 1; // skip the operator for now

        try self.emitTerm();
        const after_term_pointer = self.node_pointer;

        self.node_pointer = op_pointer;
        try self.emitOperator();

        self.node_pointer = after_term_pointer; // restore the pointer as if nothing ever happened
    }
}

fn emitTerm(self: *Self) Error!void {
    const node = self.peekNode();

    switch (node.tag) {
        .integer_literal => try self.emitIntegerLiteral(),
        .string_literal => try self.emitStringLiteral(),
        .constant => try self.emitConstant(),
        .identifier => try self.emitReadIdentifier(),
        .array_access => try self.emitReadArrayAccess(),
        .unary_operator => try self.emitUnaryOperator(),
        .call => try self.emitFunctionCall(),
        .expression => try self.emitExpression(),
        else => unreachable,
    }
}

fn emitUnaryOperator(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .unary_operator);

    try self.emitTerm();

    const instruction: vm.Instruction = switch (node.data.unary_operator) {
        .not => .{ .not = {} },
        .neg => .{ .neg = {} },
    };

    try self.instructions.append(instruction);
}

fn emitFunctionCall(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .call);

    var parameter_count: u32 = 0;

    const func_name_node = self.popNode();
    const func_name = switch (func_name_node.tag) {
        .identifier => blk: {
            const identifier = self.readStringLoc(func_name_node.data.identifier);
            const name = try std.mem.concat(
                self.compilation.arena,
                u8,
                &.{
                    self.class_name,
                    ".",
                    identifier,
                },
            );

            parameter_count = 1;

            try self.instructions.append(.{
                .push = .{
                    .segment = .pointer,
                    .constant = 0,
                },
            });

            break :blk name;
        },
        .member_access => blk: {
            const obj_name = self.readStringLoc(func_name_node.data.member_access);
            const maybe_obj = self.lookupSymbol(obj_name);

            const method_name_node = self.popNode();
            const method_name = self.readStringLoc(method_name_node.data.identifier);

            if (maybe_obj) |obj| {
                // method call!
                parameter_count = 1;

                try self.instructions.appendSlice(&.{
                    .{
                        .push = .{
                            .segment = obj.segment,
                            .constant = @intCast(obj.idx),
                        },
                    },
                });

                const name = try std.mem.concat(
                    self.compilation.arena,
                    u8,
                    &.{ obj.type, ".", method_name },
                );

                break :blk name;
            } else {
                // hmmm, maybe a function then?
                const name = try std.mem.concat(
                    self.compilation.arena,
                    u8,
                    &.{ obj_name, ".", method_name },
                );

                break :blk name;
            }
        },
        else => unreachable,
    };

    var child_count = node.data.call - 1;

    parameter_count += child_count;

    while (child_count > 0) : (child_count -= 1) {
        try self.emitExpression();
    }

    try self.instructions.append(.{
        .call = .{
            .label = func_name,
            .constant = @intCast(parameter_count),
        },
    });
}

fn emitReadIdentifier(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .identifier);

    const identifier = self.readStringLoc(node.data.identifier);
    const symbol = self.lookupSymbol(identifier) orelse
        unreachable; // TODO: handle the error

    try self.instructions.append(.{
        .push = .{
            .segment = symbol.segment,
            .constant = @intCast(symbol.idx),
        },
    });
}

fn emitReadArrayAccess(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .array_access);

    const identifier = self.readStringLoc(node.data.array_access);
    const symbol = self.lookupSymbol(identifier) orelse
        unreachable; // TODO: handle the error

    try self.emitExpression();

    try self.instructions.appendSlice(&.{
        .{
            .push = .{
                .segment = symbol.segment,
                .constant = @intCast(symbol.idx),
            },
        },
        .{ .add = {} },
        .{
            .pop = .{
                .segment = .pointer,
                .constant = 1,
            },
        },
        .{
            .push = .{
                .segment = .that,
                .constant = 0,
            },
        },
    });
}

fn emitIntegerLiteral(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .integer_literal);

    const inst = vm.Instruction{
        .push = .{
            .segment = .constant,
            .constant = node.data.integer_literal,
        },
    };

    try self.instructions.append(inst);
}

fn emitStringLiteral(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .string_literal);

    const string = self.readStringLoc(node.data.string_literal);

    // create a new string
    try self.instructions.appendSlice(&.{
        .{
            .push = .{
                .segment = .constant,
                .constant = @intCast(string.len),
            },
        },
        .{
            .call = .{
                .constant = 1,
                .label = "String.new",
            },
        },
    });

    // push the strings "value"
    try self.instructions.ensureUnusedCapacity(string.len * 2);

    for (string) |char| {
        self.instructions.appendSliceAssumeCapacity(&.{
            .{
                .push = .{
                    .segment = .constant,
                    .constant = char,
                },
            },
            .{
                .call = .{
                    .label = "String.appendChar",
                    .constant = 2,
                },
            },
        });
    }
}

fn emitOperator(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .operator);

    switch (node.data.operator) {
        .add => try self.instructions.append(.{ .add = {} }),
        .sub => try self.instructions.append(.{ .sub = {} }),
        .@"and" => try self.instructions.append(.{ .@"and" = {} }),
        .@"or" => try self.instructions.append(.{ .@"or" = {} }),
        .equal => try self.instructions.append(.{ .eq = {} }),
        .less_than => try self.instructions.append(.{ .lt = {} }),
        .greater_than => try self.instructions.append(.{ .gt = {} }),
        .mul => try self.instructions.append(
            .{
                .call = .{
                    .label = "Math.multiply",
                    .constant = 2,
                },
            },
        ),
        .div => try self.instructions.append(
            .{
                .call = .{
                    .label = "Math.divide",
                    .constant = 2,
                },
            },
        ),
    }
}

fn emitConstant(self: *Self) Error!void {
    const node = self.popNode();
    std.debug.assert(node.tag == .constant);

    switch (node.data.constant) {
        .true => try self.instructions.appendSlice(&.{
            .{
                .push = .{
                    .segment = .constant,
                    .constant = 0,
                },
            },
            .{
                .not = {},
            },
        }),
        .false, .null => try self.instructions.append(.{
            .push = .{
                .segment = .constant,
                .constant = 0,
            },
        }),
        .this => try self.instructions.append(.{
            .push = .{
                .segment = .pointer,
                .constant = 0,
            },
        }),
    }
}
