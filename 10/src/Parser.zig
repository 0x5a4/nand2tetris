const std = @import("std");
const Compilation = @import("Compilation.zig");
const Tokenizer = @import("Tokenizer.zig");
const Ast = @import("Ast.zig");
const NodeIndex = Ast.NodeIndex;

const AllocError = std.mem.Allocator.Error;

const Self = @This();

pub const ParseFn = *const fn (self: *Self) Error!NodeIndex;

tokenizer: Tokenizer,
compilation: *Compilation,
file_name: []const u8,

tree: Ast,

pub const ParseError = error{
    UnexpectedToken,
    IntegerOutOfBounds,
} || Tokenizer.Error;

pub const Error = ParseError || AllocError;

fn init(compilation: *Compilation, file_name: []const u8, text: []const u8) Self {
    return .{
        .tokenizer = Tokenizer.init(text),
        .compilation = compilation,
        .file_name = file_name,
        .tree = Ast.init(compilation.alloc),
    };
}

pub fn parse(compilation: *Compilation, file_name: []const u8, text: []const u8) !Ast {
    var parser = Self{
        .tokenizer = Tokenizer.init(text),
        .compilation = compilation,
        .file_name = file_name,
        .tree = Ast.init(compilation.alloc),
    };

    _ = try parser.parseClassDecl();

    return parser.tree;
}

pub fn abort(self: *Self) void {
    self.tree.deinit();
    self.* = undefined;
}

/// peek at the next token. If it has one of the expected tags, return it
///
/// resets the tokenizer, so subsequent calls will still produce that token
fn tasteToken(self: *Self, comptime expect: []const Tokenizer.Token.Tag) Error!?Tokenizer.Token {
    const t = self.tokenizer.next() catch |e|
        return self.err(self.tokenizer.index, e, "what the fuck is that");
    // reset the tokenizer
    defer self.tokenizer.index = t.start;

    for (expect) |expect_tok| {
        if (t.tag == expect_tok) return t;
    }

    return null;
}

/// consume a token and require it to have one of the given tags.
fn eatToken(self: *Self, comptime expect: []const Tokenizer.Token.Tag) Error!Tokenizer.Token {
    const t = try self.tasteToken(expect) orelse
        return self.tokenErr(expect);

    // "commit" the token
    self.tokenizer.index = t.start + t.len;

    return t;
}

/// compute the in-source location (line, column) from the given index
fn srcLoc(self: *const Self, index: u32) Compilation.SrcLoc {
    var line: u32 = 0;
    var char: u32 = 0;

    for (self.tokenizer.text[0..index]) |c| {
        if (c == '\n') {
            line += 1;
            char = 0;
        } else {
            char += 1;
        }
    }

    return .{
        .line = line,
        .char = char,
        .file = self.file_name,
    };
}

/// produce a compilation error
fn err(
    self: *Self,
    src_index: u32,
    tag: Compilation.Error.Tag,
    msg: []const u8,
) Compilation.Error.Tag {
    const e = Compilation.Error{
        .tag = tag,
        .msg = msg,
        .src_loc = self.srcLoc(src_index),
    };

    try self.compilation.errors.append(e);

    return tag;
}

/// produce an "unexpected token" compilation error
fn tokenErr(self: *Self, comptime expect: []const Tokenizer.Token.Tag) Compilation.Error.Tag {
    const token_names_type = std.meta.Tuple(&[_]type{[]const u8} ** expect.len);
    comptime var token_names = std.mem.zeroes(token_names_type);

    inline for (expect, 0..expect.len) |t, i| {
        token_names[i] = @tagName(t)[0..];
    }

    const msg_prefix = if (expect.len > 1)
        "expected one of "
    else
        "expected ";
    const msg_fmt = msg_prefix ++ ("{s}, " ** (expect.len - 1)) ++ "{s}";
    const msg = std.fmt.comptimePrint(msg_fmt, token_names);

    return self.err(self.tokenizer.index, Error.UnexpectedToken, msg);
}

// actual parsing stuff

fn parseIdentifier(self: *Self) Error!NodeIndex {
    const token = try self.eatToken(&.{.identifier});
    const identifierIndex = try self.tree.pushString(token.start, token.len);

    return self.tree.pushNode(.identifier, identifierIndex);
}

fn parseMemberAccess(self: *Self) Error!NodeIndex {
    const identifier = try self.eatToken(&.{.identifier});
    const identifierIndex = try self.tree.pushString(identifier.start, identifier.len);

    const rootNode = try self.tree.pushNode(.member_access, identifierIndex);

    _ = try self.eatToken(&.{.dot});

    const memberToken = try self.eatToken(&.{.identifier});
    const memberIndex = try self.tree.pushString(memberToken.start, memberToken.len);

    _ = try self.tree.pushNode(.identifier, memberIndex);

    return rootNode;
}

fn parseStringLiteral(self: *Self) Error!NodeIndex {
    const token = try self.eatToken(&.{.string_literal});

    const string_index = try self.tree.pushString(token.start + 1, token.len - 2);

    return self.tree.pushNode(.string_literal, string_index);
}

fn parseIntegerLiteral(self: *Self) Error!NodeIndex {
    const t = try self.eatToken(&.{.integer_literal});

    const value = std.fmt.parseInt(
        i16,
        self.tokenizer.text[t.start..(t.start + t.len)],
        0,
    ) catch |e| switch (e) {
        error.Overflow => return self.err(
            t.start,
            error.IntegerOutOfBounds,
            "integers must fit into 16 bits",
        ),
        error.InvalidCharacter => return self.err(
            t.start,
            error.InvalidToken,
            "was not a valid integer",
        ),
    };
    return self.tree.pushNode(.integer_literal, value);
}

fn parseConstant(self: *Self) Error!NodeIndex {
    const t = try self.eatToken(&.{
        .keyword_true,
        .keyword_false,
        .keyword_null,
        .keyword_this,
    });

    const value: Ast.Constant = switch (t.tag) {
        .keyword_true => .true,
        .keyword_false => .false,
        .keyword_null => .null,
        .keyword_this => .this,
        else => unreachable,
    };

    return self.tree.pushNode(.constant, value);
}

fn parseOperator(self: *Self) Error!NodeIndex {
    const t = try self.eatToken(&.{
        .plus,
        .minus,
        .asterisk,
        .slash,
        .ampersand,
        .pipe,
        .l_diamond,
        .r_diamond,
        .equal_sign,
    });

    const value: Ast.Operator = switch (t.tag) {
        .plus => .add,
        .minus => .sub,
        .asterisk => .mul,
        .slash => .div,
        .ampersand => .@"and",
        .pipe => .@"or",
        .l_diamond => .less_than,
        .r_diamond => .greater_than,
        .equal_sign => .equal,
        else => unreachable,
    };

    return self.tree.pushNode(.operator, value);
}

fn parseUnaryOperator(self: *Self) Error!NodeIndex {
    const t = try self.eatToken(&.{
        .minus,
        .tilde,
    });

    const value: Ast.UnaryOperator = switch (t.tag) {
        .minus => .neg,
        .tilde => .not,
        else => unreachable,
    };

    const rootNode = self.tree.pushNode(.unary_operator, value);

    _ = try self.parseTerm();

    return rootNode;
}

fn parseTerm(self: *Self) Error!NodeIndex {
    const expectedTokens = &.{
        .integer_literal,
        .string_literal,
        .keyword_true,
        .keyword_false,
        .keyword_null,
        .keyword_this,
        .identifier,
        .l_bracket,
        .minus,
        .tilde,
    };

    const lhsToken = try self.tasteToken(expectedTokens) orelse
        return self.tokenErr(expectedTokens);

    // parse lhs
    return switch (lhsToken.tag) {
        .integer_literal => try self.parseIntegerLiteral(),
        .string_literal => try self.parseStringLiteral(),
        .keyword_true,
        .keyword_false,
        .keyword_null,
        .keyword_this,
        => try self.parseConstant(),
        .identifier => blk: {
            // first, find out if this might actually be a function call or an array access
            const identifierToken = self.tokenizer.next() catch unreachable; // no error can occur, because we tokenized this before

            const maybeNextToken = try self.tasteToken(&.{
                .l_square_bracket,
                .l_bracket,
                .dot,
            });

            // we need to do this shit because we actually consumed the identifier
            // while trying to find out if it was actually a function call or array access,
            // and we'll now need it again
            self.tokenizer.index = identifierToken.start;

            break :blk if (maybeNextToken) |nextToken|
                switch (nextToken.tag) {
                    .l_bracket,
                    .dot,
                    => try self.parseFunctionCall(),
                    .l_square_bracket => try self.parseArrayAccess(),
                    else => unreachable,
                }
            else
                try self.parseIdentifier();
        },
        .l_bracket => blk: {
            _ = self.eatToken(&.{.l_bracket}) catch unreachable;

            const node = try self.parseExpression();

            _ = try self.eatToken(&.{.r_bracket});
            break :blk node;
        },
        .minus,
        .tilde,
        => try self.parseUnaryOperator(),
        else => unreachable,
    };
}

fn parseExpression(self: *Self) Error!NodeIndex {
    const rootNode = try self.tree.pushNode(.expression, 0);

    var childCount: u32 = 0;

    while (true) {
        _ = try self.parseTerm();

        childCount += 1;

        // do we have a rhs
        const opToken = try self.tasteToken(&.{
            .plus,
            .minus,
            .asterisk,
            .slash,
            .ampersand,
            .pipe,
            .l_diamond,
            .r_diamond,
            .equal_sign,
        });

        if (opToken != null) {
            _ = try self.parseOperator();
            childCount += 1;
        } else {
            break;
        }
    }

    const dataPtr = self.tree.dataPtr(rootNode, .var_decl);
    dataPtr.* = childCount;

    return rootNode;
}

fn parseArrayAccess(self: *Self) Error!NodeIndex {
    const identifierToken = try self.eatToken(&.{.identifier});

    const identifierIndex = try self.tree.pushString(identifierToken.start, identifierToken.len);

    const rootNode = try self.tree.pushNode(.array_access, identifierIndex);

    _ = try self.eatToken(&.{.l_square_bracket});
    _ = try self.parseExpression();
    _ = try self.eatToken(&.{.r_square_bracket});

    return rootNode;
}

fn parseFunctionCall(self: *Self) Error!NodeIndex {
    const rootNode = try self.tree.pushNode(.call, 0);

    const identifierToken = try self.eatToken(&.{.identifier});

    const maybeDot = try self.tasteToken(&.{.dot});

    self.tokenizer.index = identifierToken.start;

    var childCount: u32 = 1;

    if (maybeDot != null) {
        _ = try self.parseMemberAccess();
    } else {
        _ = try self.parseIdentifier();
    }

    _ = try self.eatToken(&.{.l_bracket});

    const maybeClosingBracket = try self.tasteToken(&.{.r_bracket});

    if (maybeClosingBracket == null) {
        while (true) {
            childCount += 1;

            _ = try self.parseExpression();

            const maybeComma = try self.tasteToken(&.{.comma});

            if (maybeComma != null) {
                _ = try self.eatToken(&.{.comma});
            } else {
                break;
            }
        }
    }

    const dataPtr = self.tree.dataPtr(rootNode, .call);
    dataPtr.* = childCount;

    _ = try self.eatToken(&.{.r_bracket});

    return rootNode;
}

fn parseVarDecl(self: *Self) Error!NodeIndex {
    _ = try self.eatToken(&.{.keyword_var});

    const rootNode = try self.tree.pushNode(.var_decl, 0);

    _ = try self.parseIdentifier();

    var childCount: u32 = 1;

    while (true) {
        _ = try self.parseIdentifier();

        childCount += 1;

        // have we reached the ending?
        const commaToken = try self.tasteToken(&.{.comma});

        if (commaToken != null) {
            _ = try self.eatToken(&.{.comma});
        } else {
            break;
        }
    }

    _ = try self.eatToken(&.{.semicolon});

    const dataPtr = self.tree.dataPtr(rootNode, .var_decl);
    dataPtr.* = childCount;

    return rootNode;
}

fn parseLetStatement(self: *Self) Error!NodeIndex {
    _ = try self.eatToken(&.{.keyword_let});

    const rootNode = try self.tree.pushNode(.let_statement, {});

    // identifier or array access?
    const identifierToken = try self.eatToken(&.{.identifier});
    const maybeBracket = try self.tasteToken(&.{.l_square_bracket});

    self.tokenizer.index = identifierToken.start;

    if (maybeBracket != null) {
        _ = try self.parseArrayAccess();
    } else {
        _ = try self.parseIdentifier();
    }

    _ = try self.eatToken(&.{.equal_sign});

    _ = try self.parseExpression();

    _ = try self.eatToken(&.{.semicolon});

    return rootNode;
}

fn parseDoStatement(self: *Self) Error!NodeIndex {
    _ = try self.eatToken(&.{.keyword_do});

    const rootNode = try self.tree.pushNode(.do_statement, {});

    _ = try self.parseFunctionCall();

    _ = try self.eatToken(&.{.semicolon});

    return rootNode;
}

fn parseReturnStatement(self: *Self) Error!NodeIndex {
    _ = try self.eatToken(&.{.keyword_return});

    const maybeSemicolon = try self.tasteToken(&.{.semicolon});
    const hasReturnExpr = maybeSemicolon == null;

    const rootNode = try self.tree.pushNode(.return_statement, hasReturnExpr);

    if (hasReturnExpr) {
        _ = try self.parseExpression();
    }

    _ = try self.eatToken(&.{.semicolon});

    return rootNode;
}

fn parseIfStatement(self: *Self) Error!NodeIndex {
    _ = try self.eatToken(&.{.keyword_if});

    const rootNode = try self.tree.pushNode(.if_statement, false);

    _ = try self.eatToken(&.{.l_bracket});
    _ = try self.parseExpression();
    _ = try self.eatToken(&.{.r_bracket});

    _ = try self.eatToken(&.{.l_curly_bracket});
    _ = try self.parseStatementBlock();
    _ = try self.eatToken(&.{.r_curly_bracket});

    // if an else is found continue, otherwise return immediatly
    _ = try self.tasteToken(&.{.keyword_else}) orelse return rootNode;
    _ = self.tokenizer.next() catch unreachable;

    _ = try self.eatToken(&.{.l_curly_bracket});
    _ = try self.parseStatementBlock();
    _ = try self.eatToken(&.{.r_curly_bracket});

    const dataPtr = self.tree.dataPtr(rootNode, .if_statement);
    dataPtr.* = true;

    return rootNode;
}

fn parseWhileStatement(self: *Self) Error!NodeIndex {
    _ = try self.eatToken(&.{.keyword_while});

    const rootNode = try self.tree.pushNode(.while_statement, {});

    _ = try self.eatToken(&.{.l_bracket});
    _ = try self.parseExpression();
    _ = try self.eatToken(&.{.r_bracket});

    _ = try self.eatToken(&.{.l_curly_bracket});
    _ = try self.parseStatementBlock();
    _ = try self.eatToken(&.{.r_curly_bracket});

    return rootNode;
}

fn parseStatementBlock(self: *Self) Error!NodeIndex {
    const rootNode = try self.tree.pushNode(.statement_block, 0);

    var childCount: u32 = 0;

    while (true) : (childCount += 1) {
        const expectedTokens = &.{
            .keyword_let,
            .keyword_do,
            .keyword_while,
            .keyword_if,
            .keyword_return,
        };
        const nextStatementToken = try self.tasteToken(expectedTokens) orelse
            break;

        _ = switch (nextStatementToken.tag) {
            .keyword_let => try self.parseLetStatement(),
            .keyword_do => try self.parseDoStatement(),
            .keyword_return => try self.parseReturnStatement(),
            .keyword_while => try self.parseWhileStatement(),
            .keyword_if => try self.parseIfStatement(),
            else => unreachable,
        };
    }

    const dataPtr = self.tree.dataPtr(rootNode, .statement_block);
    dataPtr.* = childCount;

    return rootNode;
}

fn parseClassVarDecl(self: *Self) Error!NodeIndex {
    const mainToken = try self.eatToken(&.{ .keyword_static, .keyword_field });

    const rootNode = switch (mainToken.tag) {
        .keyword_static => try self.tree.pushNode(.static_decl, 0),
        .keyword_field => try self.tree.pushNode(.field_decl, 0),
        else => unreachable,
    };

    _ = try self.parseIdentifier();

    var childCount: u32 = 1;

    while (true) {
        _ = try self.parseIdentifier();

        childCount += 1;

        // have we reached the ending?
        const commaToken = try self.tasteToken(&.{.comma});

        if (commaToken != null) {
            _ = try self.eatToken(&.{.comma});
        } else {
            break;
        }
    }

    _ = try self.eatToken(&.{.semicolon});

    const dataPtr = switch (mainToken.tag) {
        .keyword_static => self.tree.dataPtr(rootNode, .static_decl),
        .keyword_field => self.tree.dataPtr(rootNode, .field_decl),
        else => unreachable,
    };
    dataPtr.* = childCount;

    return rootNode;
}

fn parseFunctionDecl(self: *Self) Error!NodeIndex {
    const mainToken = try self.eatToken(&.{
        .keyword_function,
        .keyword_method,
        .keyword_constructor,
    });

    const rootNode = switch (mainToken.tag) {
        .keyword_function => try self.tree.pushNode(.function_decl, 0),
        .keyword_method => try self.tree.pushNode(.method_decl, 0),
        .keyword_constructor => try self.tree.pushNode(.constructor_decl, 0),
        else => unreachable,
    };

    // type
    _ = try self.parseIdentifier();

    // name
    _ = try self.parseIdentifier();

    _ = try self.eatToken(&.{.l_bracket});

    var childCount: u32 = 3;

    const maybeClosingBracket = try self.tasteToken(&.{.r_bracket});

    if (maybeClosingBracket == null) {
        while (true) {
            _ = try self.parseIdentifier();
            _ = try self.parseIdentifier();

            childCount += 2;

            const maybeComma = try self.tasteToken(&.{.comma});

            if (maybeComma != null) {
                _ = try self.eatToken(&.{.comma});
            } else {
                break;
            }
        }
    }

    _ = try self.eatToken(&.{.r_bracket});

    _ = try self.eatToken(&.{.l_curly_bracket});
    _ = try self.parseFunctionBody();
    _ = try self.eatToken(&.{.r_curly_bracket});

    const dataPtr = switch (mainToken.tag) {
        .keyword_function => self.tree.dataPtr(rootNode, .function_decl),
        .keyword_method => self.tree.dataPtr(rootNode, .method_decl),
        .keyword_constructor => self.tree.dataPtr(rootNode, .constructor_decl),
        else => unreachable,
    };

    dataPtr.* = childCount;

    return rootNode;
}

fn parseFunctionBody(self: *Self) Error!NodeIndex {
    const rootNode = try self.tree.pushNode(.function_body, 0);

    var childCount: u32 = 1; // starts at one because of statements

    while (true) : (childCount += 1) {
        _ = try self.tasteToken(&.{.keyword_var}) orelse break;

        _ = try self.parseVarDecl();
    }

    _ = try self.parseStatementBlock();

    const dataPtr = self.tree.dataPtr(rootNode, .function_body);
    dataPtr.* = childCount;

    return rootNode;
}

fn parseClassDecl(self: *Self) Error!NodeIndex {
    _ = try self.eatToken(&.{.keyword_class});

    const rootNode = try self.tree.pushNode(.class_decl, 0);
    var childCount: u32 = 1;

    _ = try self.parseIdentifier();

    _ = try self.eatToken(&.{.l_curly_bracket});

    while (true) : (childCount += 1) {
        const expectedTokens = &.{
            .keyword_static,
            .keyword_field,
            .keyword_function,
            .keyword_method,
            .keyword_constructor,
        };

        const nextToken = try self.tasteToken(expectedTokens) orelse
            break;

        _ = switch (nextToken.tag) {
            .keyword_static,
            .keyword_field,
            => try self.parseClassVarDecl(),
            .keyword_function,
            .keyword_method,
            .keyword_constructor,
            => try self.parseFunctionDecl(),
            else => unreachable,
        };
    }

    _ = try self.eatToken(&.{.r_curly_bracket});

    const dataPtr = self.tree.dataPtr(rootNode, .class_decl);
    dataPtr.* = childCount;

    return rootNode;
}

// testing

const testing = std.testing;

fn expectNode(parser: *Self, pos: u32, comptime expect: anytype) !void {
    const tag: Ast.Node.Tag = expect[0];
    const data = expect[1];

    const node = parser.tree.nodes.items[pos];

    try testing.expectEqual(tag, node.tag);

    const actualData = parser.tree.dataPtr(pos, tag);

    try testing.expectEqual(data, actualData.*);
}

fn expectString(parser: *Self, pos: u32, expected: []const u8) !void {
    const loc = parser.tree.strings.items[pos];

    const actual = parser.tokenizer.text[loc.start..(loc.start + loc.len)];
    try testing.expectEqualStrings(expected, actual);
}

fn testParser(
    comptime func: ParseFn,
    input: []const u8,
    comptime nodes: anytype,
    comptime strings: anytype,
) !void {
    var compilation = Compilation.init(testing.allocator);
    defer compilation.deinit();
    var parser = init(&compilation, "testing.jack", input);
    defer parser.abort();

    _ = try func(&parser);

    try testing.expectEqual(nodes.len, parser.tree.nodes.items.len);
    try testing.expectEqual(strings.len, parser.tree.strings.items.len);
    try testing.expect(!compilation.hasError());

    inline for (nodes, 0..) |expected, i| {
        try expectNode(&parser, i, expected);
    }

    inline for (strings, 0..) |expected, i| {
        try expectString(&parser, i, expected);
    }

    const nextToken = try parser.tokenizer.next();
    try testing.expectEqual(nextToken.tag, .eof);
}

test "parse identifier" {
    try testParser(
        parseIdentifier,
        "identifier",
        .{.{ .identifier, 0 }},
        .{"identifier"},
    );
}

test "parse member access" {
    try testParser(
        parseMemberAccess,
        "Class.func",
        .{ .{ .member_access, 0 }, .{ .identifier, 1 } },
        .{ "Class", "func" },
    );
}

test "parse integer literal" {
    try testParser(
        parseIntegerLiteral,
        "1337",
        .{.{ .integer_literal, 1337 }},
        .{},
    );
}

test "parse constant" {
    try testParser(
        parseConstant,
        "true",
        .{.{ .constant, .true }},
        .{},
    );
    try testParser(
        parseConstant,
        "false",
        .{.{ .constant, .false }},
        .{},
    );
    try testParser(
        parseConstant,
        "null",
        .{.{ .constant, .null }},
        .{},
    );
    try testParser(
        parseConstant,
        "this",
        .{.{ .constant, .this }},
        .{},
    );
}

test "parse operator" {
    try testParser(
        parseOperator,
        "+",
        .{.{ .operator, .add }},
        .{},
    );
    try testParser(
        parseOperator,
        "-",
        .{.{ .operator, .sub }},
        .{},
    );
    try testParser(
        parseOperator,
        "*",
        .{.{ .operator, .mul }},
        .{},
    );
    try testParser(
        parseOperator,
        "/",
        .{.{ .operator, .div }},
        .{},
    );
    try testParser(
        parseOperator,
        "&",
        .{.{ .operator, .@"and" }},
        .{},
    );
    try testParser(
        parseOperator,
        "|",
        .{.{ .operator, .@"or" }},
        .{},
    );
    try testParser(
        parseOperator,
        "<",
        .{.{ .operator, .less_than }},
        .{},
    );
    try testParser(
        parseOperator,
        ">",
        .{.{ .operator, .greater_than }},
        .{},
    );
    try testParser(
        parseOperator,
        "=",
        .{.{ .operator, .equal }},
        .{},
    );
}

test "parse string literal" {
    try testParser(
        parseStringLiteral,
        "\"woah yeah\"",
        .{.{ .string_literal, 0 }},
        .{"woah yeah"},
    );
    try testParser(
        parseStringLiteral,
        "\"\"",
        .{.{ .string_literal, 0 }},
        .{""},
    );
}

test "parse var decl" {
    try testParser(
        parseVarDecl,
        "var int var1;",
        .{ .{ .var_decl, 2 }, .{ .identifier, 0 }, .{ .identifier, 1 } },
        .{ "int", "var1" },
    );
    try testParser(
        parseVarDecl,
        "var int var1, var2;",
        .{ .{ .var_decl, 3 }, .{ .identifier, 0 }, .{ .identifier, 1 }, .{ .identifier, 2 } },
        .{ "int", "var1", "var2" },
    );
}

test "parse class var decl" {
    try testParser(
        parseClassVarDecl,
        "field int var1;",
        .{ .{ .field_decl, 2 }, .{ .identifier, 0 }, .{ .identifier, 1 } },
        .{ "int", "var1" },
    );
    try testParser(
        parseClassVarDecl,
        "static int var1, var2;",
        .{ .{ .static_decl, 3 }, .{ .identifier, 0 }, .{ .identifier, 1 }, .{ .identifier, 2 } },
        .{ "int", "var1", "var2" },
    );
}

test "parse expression" {
    try testParser(
        parseExpression,
        "42",
        .{ .{ .expression, 1 }, .{ .integer_literal, 42 } },
        .{},
    );

    try testParser(
        parseExpression,
        "myvar",
        .{ .{ .expression, 1 }, .{ .identifier, 0 } },
        .{"myvar"},
    );

    try testParser(
        parseExpression,
        "21 + 21",
        .{
            .{ .expression, 3 },
            .{ .integer_literal, 21 },
            .{ .operator, .add },
            .{ .integer_literal, 21 },
        },
        .{},
    );

    try testParser(
        parseExpression,
        "true & true",
        .{
            .{ .expression, 3 },
            .{ .constant, .true },
            .{ .operator, .@"and" },
            .{ .constant, .true },
        },
        .{},
    );

    try testParser(
        parseExpression,
        "true & true | false",
        .{
            .{ .expression, 5 },
            .{ .constant, .true },
            .{ .operator, .@"and" },
            .{ .constant, .true },
            .{ .operator, .@"or" },
            .{ .constant, .false },
        },
        .{},
    );

    try testParser(
        parseExpression,
        "myvar[3]",
        .{
            .{ .expression, 1 },
            .{ .array_access, 0 },
            .{ .expression, 1 },
            .{ .integer_literal, 3 },
        },
        .{"myvar"},
    );

    try testParser(
        parseExpression,
        "func(1, 2)",
        .{
            .{ .expression, 1 },
            .{ .call, 3 },
            .{ .identifier, 0 },
            .{ .expression, 1 },
            .{ .integer_literal, 1 },
            .{ .expression, 1 },
            .{ .integer_literal, 2 },
        },
        .{"func"},
    );

    try testParser(
        parseExpression,
        "Class.func(1, 2)",
        .{
            .{ .expression, 1 },
            .{ .call, 3 },
            .{ .member_access, 0 },
            .{ .identifier, 1 },
            .{ .expression, 1 },
            .{ .integer_literal, 1 },
            .{ .expression, 1 },
            .{ .integer_literal, 2 },
        },
        .{ "Class", "func" },
    );

    try testParser(
        parseExpression,
        "func(1, 2) + 3",
        .{
            .{ .expression, 3 },
            .{ .call, 3 },
            .{ .identifier, 0 },
            .{ .expression, 1 },
            .{ .integer_literal, 1 },
            .{ .expression, 1 },
            .{ .integer_literal, 2 },
            .{ .operator, .add },
            .{ .integer_literal, 3 },
        },
        .{"func"},
    );

    try testParser(
        parseExpression,
        "1 + (2 - 3)",
        .{
            .{ .expression, 3 },
            .{ .integer_literal, 1 },
            .{ .operator, .add },
            .{ .expression, 3 },
            .{ .integer_literal, 2 },
            .{ .operator, .sub },
            .{ .integer_literal, 3 },
        },
        .{},
    );

    try testParser(
        parseExpression,
        "1 + -3",
        .{
            .{ .expression, 3 },
            .{ .integer_literal, 1 },
            .{ .operator, .add },
            .{ .unary_operator, .neg },
            .{ .integer_literal, 3 },
        },
        .{},
    );
}

test "parse unary operator " {
    try testParser(
        parseUnaryOperator,
        "~exit",
        .{
            .{ .unary_operator, .not },
            .{ .identifier, 0 },
        },
        .{"exit"},
    );
}

test "parse array access" {
    try testParser(
        parseArrayAccess,
        "myvar[42]",
        .{ .{ .array_access, 0 }, .{ .expression, 1 }, .{ .integer_literal, 42 } },
        .{"myvar"},
    );

    try testParser(
        parseArrayAccess,
        "myvar[1 + 2]",
        .{
            .{ .array_access, 0 },
            .{ .expression, 3 },
            .{ .integer_literal, 1 },
            .{ .operator, .add },
            .{ .integer_literal, 2 },
        },
        .{"myvar"},
    );
}

test "parse function call" {
    try testParser(
        parseFunctionCall,
        "func()",
        .{ .{ .call, 1 }, .{ .identifier, 0 } },
        .{"func"},
    );

    try testParser(
        parseFunctionCall,
        "func(1 + 2)",
        .{
            .{ .call, 2 },
            .{ .identifier, 0 },
            .{ .expression, 3 },
            .{ .integer_literal, 1 },
            .{ .operator, .add },
            .{ .integer_literal, 2 },
        },
        .{"func"},
    );

    try testParser(
        parseFunctionCall,
        "func(1 + 2, 42)",
        .{
            .{ .call, 3 },
            .{ .identifier, 0 },
            .{ .expression, 3 },
            .{ .integer_literal, 1 },
            .{ .operator, .add },
            .{ .integer_literal, 2 },
            .{ .expression, 1 },
            .{ .integer_literal, 42 },
        },
        .{"func"},
    );

    try testParser(
        parseFunctionCall,
        "Class.func(1 + 2, 42)",
        .{
            .{ .call, 3 },
            .{ .member_access, 0 },
            .{ .identifier, 1 },
            .{ .expression, 3 },
            .{ .integer_literal, 1 },
            .{ .operator, .add },
            .{ .integer_literal, 2 },
            .{ .expression, 1 },
            .{ .integer_literal, 42 },
        },
        .{ "Class", "func" },
    );
}

test "parse let statement" {
    try testParser(
        parseLetStatement,
        "let yomama = 42;",
        .{
            .{ .let_statement, {} },
            .{ .identifier, 0 },
            .{ .expression, 1 },
            .{ .integer_literal, 42 },
        },
        .{"yomama"},
    );

    try testParser(
        parseLetStatement,
        "let joe[biden] = 42;",
        .{
            .{ .let_statement, {} },
            .{ .array_access, 0 },
            .{ .expression, 1 },
            .{ .identifier, 1 },
            .{ .expression, 1 },
            .{ .integer_literal, 42 },
        },
        .{ "joe", "biden" },
    );

    try testParser(
        parseLetStatement,
        "let length = Keyboard.readInt(\"HOW MANY NUMBERS? \");",
        .{
            .{ .let_statement, {} },
            .{ .identifier, 0 },
            .{ .expression, 1 },
            .{ .call, 2 },
            .{ .member_access, 1 },
            .{ .identifier, 2 },
            .{ .expression, 1 },
            .{ .string_literal, 3 },
        },
        .{
            "length",
            "Keyboard",
            "readInt",
            "HOW MANY NUMBERS? ",
        },
    );
}

test "parse do statement" {
    try testParser(
        parseDoStatement,
        "do myfunc(42);",
        .{
            .{ .do_statement, {} },
            .{ .call, 2 },
            .{ .identifier, 0 },
            .{ .expression, 1 },
            .{ .integer_literal, 42 },
        },
        .{"myfunc"},
    );
}

test "parse return statement" {
    try testParser(
        parseReturnStatement,
        "return;",
        .{
            .{ .return_statement, false },
        },
        .{},
    );

    try testParser(
        parseReturnStatement,
        "return 1;",
        .{
            .{ .return_statement, true },
            .{ .expression, 1 },
            .{ .integer_literal, 1 },
        },
        .{},
    );
}

test "parse if statement" {
    try testParser(
        parseIfStatement,
        "if (true) { return; }",
        .{
            .{ .if_statement, false },
            .{ .expression, 1 },
            .{ .constant, .true },
            .{ .statement_block, 1 },
            .{ .return_statement, false },
        },
        .{},
    );

    try testParser(
        parseIfStatement,
        "if (true) { return; } else { return; }",
        .{
            .{ .if_statement, true },
            .{ .expression, 1 },
            .{ .constant, .true },
            .{ .statement_block, 1 },
            .{ .return_statement, false },
            .{ .statement_block, 1 },
            .{ .return_statement, false },
        },
        .{},
    );
}

test "parse while statement" {
    try testParser(
        parseWhileStatement,
        "while (true) { return; }",
        .{
            .{ .while_statement, {} },
            .{ .expression, 1 },
            .{ .constant, .true },
            .{ .statement_block, 1 },
            .{ .return_statement, false },
        },
        .{},
    );
}

test "parse statement block" {
    try testParser(
        parseStatementBlock,
        "let one = 1;",
        .{
            .{ .statement_block, 1 },
            .{ .let_statement, {} },
            .{ .identifier, 0 },
            .{ .expression, 1 },
            .{ .integer_literal, 1 },
        },
        .{"one"},
    );

    try testParser(
        parseStatementBlock,
        "do thing();",
        .{
            .{ .statement_block, 1 },
            .{ .do_statement, {} },
            .{ .call, 1 },
            .{ .identifier, 0 },
        },
        .{"thing"},
    );

    try testParser(
        parseStatementBlock,
        "return;",
        .{
            .{ .statement_block, 1 },
            .{ .return_statement, false },
        },
        .{},
    );
}

test "parse function body" {
    try testParser(
        parseFunctionBody,
        "return;",
        .{
            .{ .function_body, 1 },
            .{ .statement_block, 1 },
            .{ .return_statement, false },
        },
        .{},
    );

    try testParser(
        parseFunctionBody,
        "var int myvar; return;",
        .{
            .{ .function_body, 2 },
            .{ .var_decl, 2 },
            .{ .identifier, 0 },
            .{ .identifier, 1 },
            .{ .statement_block, 1 },
            .{ .return_statement, false },
        },
        .{ "int", "myvar" },
    );
}

test "parse function decl" {
    try testParser(
        parseFunctionDecl,
        \\function int myfunc() {
        \\    return;
        \\} 
    ,
        .{
            .{ .function_decl, 3 },
            .{ .identifier, 0 },
            .{ .identifier, 1 },
            .{ .function_body, 1 },
            .{ .statement_block, 1 },
            .{ .return_statement, false },
        },
        .{ "int", "myfunc" },
    );

    try testParser(
        parseFunctionDecl,
        \\function int myfunc(int a, int b) {
        \\    return;
        \\}
    ,
        .{
            .{ .function_decl, 7 },
            .{ .identifier, 0 },
            .{ .identifier, 1 },
            .{ .identifier, 2 },
            .{ .identifier, 3 },
            .{ .identifier, 4 },
            .{ .identifier, 5 },
            .{ .function_body, 1 },
            .{ .statement_block, 1 },
            .{ .return_statement, false },
        },
        .{ "int", "myfunc", "int", "a", "int", "b" },
    );
}

test "parse class decl" {
    try testParser(
        parseClassDecl,
        \\class Nand2 {
        \\  static int tetris;
        \\ 
        \\  function int myfunc(int a, int b) {
        \\    return;
        \\  }
        \\}
    ,
        .{
            .{ .class_decl, 3 },
            .{ .identifier, 0 },
            .{ .static_decl, 2 },
            .{ .identifier, 1 },
            .{ .identifier, 2 },
            .{ .function_decl, 7 },
            .{ .identifier, 3 },
            .{ .identifier, 4 },
            .{ .identifier, 5 },
            .{ .identifier, 6 },
            .{ .identifier, 7 },
            .{ .identifier, 8 },
            .{ .function_body, 1 },
            .{ .statement_block, 1 },
            .{ .return_statement, false },
        },
        .{ "Nand2", "int", "tetris", "int", "myfunc", "int", "a", "int", "b" },
    );

    try testParser(
        parseClassDecl,
        \\class Main {
        \\    function void main() {
        \\      let length = Keyboard.readInt("HOW MANY NUMBERS? ");
        \\      return;
        \\  }
        \\}
    ,
        .{
            .{ .class_decl, 2 },
            .{ .identifier, 0 },
            .{ .function_decl, 3 },
            .{ .identifier, 1 },
            .{ .identifier, 2 },
            .{ .function_body, 1 },
            .{ .statement_block, 2 },
            .{ .let_statement, {} },
            .{ .identifier, 3 },
            .{ .expression, 1 },
            .{ .call, 2 },
            .{ .member_access, 4 },
            .{ .identifier, 5 },
            .{ .expression, 1 },
            .{ .string_literal, 6 },
            .{ .return_statement, false },
        },
        .{
            "Main",
            "void",
            "main",
            "length",
            "Keyboard",
            "readInt",
            "HOW MANY NUMBERS? ",
        },
    );
}
