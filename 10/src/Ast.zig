const std = @import("std");
const Allocator = std.mem.Allocator;

const Self = @This();

pub const NodeIndex = u32;
pub const StringIndex = u32;

nodes: std.ArrayList(Node),
strings: std.ArrayList(StringLocation),

pub const StringLocation = struct {
    start: u32,
    len: u32,
};

pub const Constant = enum {
    true,
    false,
    null,
    this,
};

pub const Operator = enum {
    add,
    sub,
    mul,
    div,
    @"and",
    @"or",
    less_than,
    greater_than,
    equal,
};

pub const UnaryOperator = enum {
    not,
    neg,
};

pub const Node = struct {
    /// what is this node?
    tag: Tag,
    /// extra data associated with this node, also has different meaning depending on the tag.
    data: Data,

    pub const Tag = @typeInfo(Data).Union.tag_type.?;

    pub const Data = union(enum) {
        /// index into strings array
        identifier: u32,

        /// how many children this has. class var decls and function decls
        class_decl: u32,
        /// how many children. followed by 2 identifiers for name and type,
        /// n - 3 identifiers for name and type pairs of parameters and a function_body
        constructor_decl: u32,
        /// how many children. followed by 2 identifiers for name and type,
        /// n - 3 identifiers for name and type pairs of parameters and a function_body
        function_decl: u32,
        /// how many children. followed by 2 identifiers for name and type,
        /// n - 3 identifiers for name and type pairs of parameters and a function_body
        method_decl: u32,
        /// how many children. followed by n - 1 var statements and a statement_block
        function_body: u32,

        /// how many child nodes this has, number of identifers including type
        var_decl: u32,
        /// how many child nodes this has, number of identifers including type
        field_decl: u32,
        /// how many child nodes this has, number of identifers including type
        static_decl: u32,

        /// followed by identifier or arrayAccess and then an expression
        let_statement: void,
        /// followed by a call node
        do_statement: void,
        /// how many child nodes this has, number of statements
        statement_block: u32,
        /// whether this has an else branch. followed by an expression
        /// and a statement_block. if an else block exists, followed by another statement_block
        if_statement: bool,
        /// whether this has a return expression. if so, followed by an expression
        return_statement: bool,
        /// followed by an expression and a statement_block
        while_statement: void,

        /// how many children
        expression: u32,
        /// index into strings array where the class name is. followed by an identifier node
        member_access: u32,
        /// points to the identifier in the strings array. followed by an expression
        array_access: u32,
        /// value of the literal
        integer_literal: i16,
        /// index into the strings array
        string_literal: u32,
        /// what constant is this?
        constant: Constant,
        /// what operator is this?
        operator: Operator,
        /// what unary is this? followed by a "term"
        unary_operator: UnaryOperator,
        /// how many children. followed by identifier/member_access and n - 1 expressions
        call: u32,
    };
};

pub fn init(alloc: Allocator) Self {
    return .{
        .nodes = std.ArrayList(Node).init(alloc),
        .strings = std.ArrayList(StringLocation).init(alloc),
    };
}

pub fn deinit(self: *Self) void {
    self.nodes.deinit();
    self.strings.deinit();
    self.* = undefined;
}

pub fn pushString(self: *Self, start: u32, len: u32) !StringIndex {
    const str_loc: StringLocation = .{
        .start = start,
        .len = len,
    };

    const index: StringIndex = @intCast(self.strings.items.len);
    try self.strings.append(str_loc);
    return index;
}

pub fn pushNode(self: *Self, comptime node_tag: Node.Tag, data: anytype) !NodeIndex {
    const node: Node = .{
        .tag = node_tag,
        .data = @unionInit(Node.Data, @tagName(node_tag), data),
    };

    const index: NodeIndex = @intCast(self.nodes.items.len);
    try self.nodes.append(node);

    return index;
}

pub fn DataType(tag: Node.Tag) type {
    const dataInfo = @typeInfo(Node.Data).Union;

    for (dataInfo.fields) |field| {
        if (std.mem.eql(u8, @tagName(tag), field.name))
            return field.type;
    }

    unreachable;
}

pub fn dataPtr(
    self: *Self,
    index: NodeIndex,
    comptime node_tag: Node.Tag,
) *DataType(node_tag) {
    const nodeptr = &self.nodes.items[index];

    return @ptrCast(&nodeptr.data);
}

const testing = std.testing;

test "read data ptr" {
    var ast = init(testing.allocator);
    defer ast.deinit();

    _ = try ast.pushNode(.var_decl, 3);

    const node = ast.nodes.items[0];
    const actualData = node.data.var_decl;

    const ptr = ast.dataPtr(0, .var_decl);

    try testing.expectEqual(actualData, ptr.*);
}
