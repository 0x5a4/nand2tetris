const std = @import("std");
const ComptimeStringMap = std.ComptimeStringMap;

const Self = @This();

text: []const u8,
index: u32 = 0,

pub fn init(text: []const u8) Self {
    return .{
        .text = text,
    };
}

pub const Error = error{
    InvalidToken,
};

pub const Token = struct {
    start: u32,
    len: u32,
    tag: Tag,

    pub const Tag = enum {
        eof,
        identifier,
        integer_literal,
        string_literal,

        // symbols
        l_bracket,
        r_bracket,
        l_curly_bracket,
        r_curly_bracket,
        l_square_bracket,
        r_square_bracket,
        dot,
        comma,
        semicolon,
        plus,
        minus,
        asterisk,
        slash,
        ampersand,
        pipe,
        l_diamond,
        r_diamond,
        equal_sign,
        tilde,

        // keywords
        keyword_class,
        keyword_constructor,
        keyword_do,
        keyword_else,
        keyword_false,
        keyword_field,
        keyword_function,
        keyword_if,
        keyword_let,
        keyword_method,
        keyword_null,
        keyword_return,
        keyword_static,
        keyword_this,
        keyword_true,
        keyword_var,
        keyword_while,
    };
};

const State = enum {
    start,
    string_literal,
    integer_literal,
    identifier,
    slash,
    comment,
    multiline_comment,
    maybe_end_multiline_comment,
};

pub fn next(self: *Self) Error!Token {
    var state: State = .start;

    var start: u32 = self.index;

    while (self.index < self.text.len) : (self.index += 1) {
        const char = self.text[self.index];

        switch (state) {
            .start => switch (char) {
                ' ', '\t', '\n', '\r' => start += 1,
                '/' => state = .slash,
                '"' => state = .string_literal,

                'a'...'z', 'A'...'Z', '_' => state = .identifier,
                '0'...'9' => state = .integer_literal,

                // all of these are unambiguous
                '(' => return self.tokenEndNext(.l_bracket, start),
                ')' => return self.tokenEndNext(.r_bracket, start),
                '{' => return self.tokenEndNext(.l_curly_bracket, start),
                '}' => return self.tokenEndNext(.r_curly_bracket, start),
                '[' => return self.tokenEndNext(.l_square_bracket, start),
                ']' => return self.tokenEndNext(.r_square_bracket, start),
                '.' => return self.tokenEndNext(.dot, start),
                ',' => return self.tokenEndNext(.comma, start),
                ';' => return self.tokenEndNext(.semicolon, start),
                '+' => return self.tokenEndNext(.plus, start),
                '-' => return self.tokenEndNext(.minus, start),
                '*' => return self.tokenEndNext(.asterisk, start),
                '&' => return self.tokenEndNext(.ampersand, start),
                '|' => return self.tokenEndNext(.pipe, start),
                '<' => return self.tokenEndNext(.l_diamond, start),
                '>' => return self.tokenEndNext(.r_diamond, start),
                '=' => return self.tokenEndNext(.equal_sign, start),
                '~' => return self.tokenEndNext(.tilde, start),
                else => return error.InvalidToken,
            },
            .string_literal => switch (char) {
                '"' => return self.tokenEndNext(.string_literal, start),
                '\n' => return error.InvalidToken,
                else => continue,
            },
            .integer_literal => switch (char) {
                '0'...'9' => continue,
                else => return self.token(.integer_literal, start),
            },
            .identifier => switch (char) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => continue,
                else => return self.tokenIdentifier(start),
            },
            .slash => switch (char) {
                '/',
                => state = .comment,
                '*',
                => state = .multiline_comment,
                else => return self.token(.slash, start),
            },
            .comment => switch (char) {
                '\n' => {
                    start = self.index + 1;
                    state = .start;
                },
                else => start += 1,
            },
            .multiline_comment => switch (char) {
                '*' => state = .maybe_end_multiline_comment,
                else => start += 1,
            },
            .maybe_end_multiline_comment => switch (char) {
                '/' => {
                    start = self.index + 1;
                    state = .start;
                },
                else => {
                    state = .multiline_comment;
                    start += 1;
                },
            },
        }
    }

    switch (state) {
        .start,
        .comment,
        .multiline_comment,
        .maybe_end_multiline_comment,
        => return self.token(.eof, start),
        .integer_literal => return self.token(.integer_literal, start),
        .slash => return self.tokenEndNext(.slash, start),
        .identifier => return self.tokenIdentifier(start),
        .string_literal => return error.InvalidToken,
    }
}

const keyword_map = ComptimeStringMap(Token.Tag, .{
    .{ "class", .keyword_class },
    .{ "constructor", .keyword_constructor },
    .{ "function", .keyword_function },
    .{ "method", .keyword_method },
    .{ "field", .keyword_field },
    .{ "static", .keyword_static },
    .{ "var", .keyword_var },
    .{ "true", .keyword_true },
    .{ "false", .keyword_false },
    .{ "null", .keyword_null },
    .{ "this", .keyword_this },
    .{ "let", .keyword_let },
    .{ "do", .keyword_do },
    .{ "if", .keyword_if },
    .{ "else", .keyword_else },
    .{ "while", .keyword_while },
    .{ "return", .keyword_return },
});

fn tokenIdentifier(self: *const Self, start: u32) Token {
    if (keyword_map.get(self.text[start..self.index])) |keyword_tag| {
        return self.token(keyword_tag, start);
    } else {
        return self.token(.identifier, start);
    }
}

fn tokenEndNext(self: *Self, tag: Token.Tag, start: u32) Token {
    self.index += 1;
    return self.token(tag, start);
}

fn token(self: *const Self, tag: Token.Tag, start: u32) Token {
    return .{
        .start = start,
        .len = self.index - start,
        .tag = tag,
    };
}

// tests
const testing = std.testing;

fn expectTokens(text: []const u8, tokens: []const Token.Tag) !void {
    var tokenizer = Self.init(text);

    for (tokens) |tok| {
        const actual = try tokenizer.next();

        try testing.expectEqual(tok, actual.tag);
    }
}

test "tokenize symbols" {
    try expectTokens("(", &.{ .l_bracket, .eof });
    try expectTokens(")", &.{ .r_bracket, .eof });
    try expectTokens("{", &.{ .l_curly_bracket, .eof });
    try expectTokens("}", &.{ .r_curly_bracket, .eof });
    try expectTokens("[", &.{ .l_square_bracket, .eof });
    try expectTokens("]", &.{ .r_square_bracket, .eof });
    try expectTokens(".", &.{ .dot, .eof });
    try expectTokens(",", &.{ .comma, .eof });
    try expectTokens(";", &.{ .semicolon, .eof });
    try expectTokens("+", &.{ .plus, .eof });
    try expectTokens("-", &.{ .minus, .eof });
    try expectTokens("*", &.{ .asterisk, .eof });
    try expectTokens("&", &.{ .ampersand, .eof });
    try expectTokens("|", &.{ .pipe, .eof });
    try expectTokens("<", &.{ .l_diamond, .eof });
    try expectTokens(">", &.{ .r_diamond, .eof });
    try expectTokens("=", &.{ .equal_sign, .eof });
    try expectTokens("~", &.{ .tilde, .eof });
}

test "tokenize keywords" {
    try expectTokens("class", &.{ .keyword_class, .eof });
    try expectTokens("constructor", &.{ .keyword_constructor, .eof });
    try expectTokens("do", &.{ .keyword_do, .eof });
    try expectTokens("else", &.{ .keyword_else, .eof });
    try expectTokens("false", &.{ .keyword_false, .eof });
    try expectTokens("field", &.{ .keyword_field, .eof });
    try expectTokens("function", &.{ .keyword_function, .eof });
    try expectTokens("if", &.{ .keyword_if, .eof });
    try expectTokens("let", &.{ .keyword_let, .eof });
    try expectTokens("method", &.{ .keyword_method, .eof });
    try expectTokens("null", &.{ .keyword_null, .eof });
    try expectTokens("return", &.{ .keyword_return, .eof });
    try expectTokens("static", &.{ .keyword_static, .eof });
    try expectTokens("this", &.{ .keyword_this, .eof });
    try expectTokens("true", &.{ .keyword_true, .eof });
    try expectTokens("var", &.{ .keyword_var, .eof });
    try expectTokens("while", &.{ .keyword_while, .eof });
}

test "tokenize skip whitespace" {
    try expectTokens("   class", &.{ .keyword_class, .eof });
    try expectTokens("\tclass", &.{ .keyword_class, .eof });
    try expectTokens("\n class", &.{ .keyword_class, .eof });
    try expectTokens("\r\nclass", &.{ .keyword_class, .eof });
}

test "tokenize skip comments" {
    try expectTokens("class //comment", &.{ .keyword_class, .eof });
    try expectTokens("// more comment \n class //comment\n if", &.{
        .keyword_class,
        .keyword_if,
        .eof,
    });
    try expectTokens("/* invisible */ class //comment", &.{ .keyword_class, .eof });
    try expectTokens("/** invisible\n\n */ class //comment", &.{ .keyword_class, .eof });
}

test "tokenize slash" {
    try expectTokens("/", &.{ .slash, .eof });
    try expectTokens(
        "class / if",
        &.{ .keyword_class, .slash, .keyword_if, .eof },
    );
}

test "tokenize identifier" {
    try expectTokens("identifier", &.{ .identifier, .eof });
    try expectTokens("IDENTIFIER", &.{ .identifier, .eof });
    try expectTokens("IdEnTiFiEr", &.{ .identifier, .eof });
    try expectTokens("also_identifier", &.{ .identifier, .eof });
    try expectTokens("_identifier", &.{ .identifier, .eof });
    try expectTokens("identifier42", &.{ .identifier, .eof });
}

test "tokenize number" {
    try expectTokens("42", &.{ .integer_literal, .eof });
    try expectTokens("421111", &.{ .integer_literal, .eof });
}

test "tokenize string literal" {
    try expectTokens("\"string\"", &.{ .string_literal, .eof });
    try expectTokens("\" more string\"", &.{ .string_literal, .eof });
    try expectTokens("\" more string\"   ", &.{ .string_literal, .eof });
}

test "tokenize determine start and len" {
    {
        var tokenizer = Self.init("   class    ");
        const tok = try tokenizer.next();

        try std.testing.expectEqual(.keyword_class, tok.tag);
        try std.testing.expectEqual(3, tok.start);
        try std.testing.expectEqual(5, tok.len);
    }

    {
        var tokenizer = Self.init("   class");
        const tok = try tokenizer.next();

        try std.testing.expectEqual(.keyword_class, tok.tag);
        try std.testing.expectEqual(3, tok.start);
        try std.testing.expectEqual(5, tok.len);
    }

    {
        var tokenizer = Self.init("   1337");
        const tok = try tokenizer.next();

        try std.testing.expectEqual(.integer_literal, tok.tag);
        try std.testing.expectEqual(3, tok.start);
        try std.testing.expectEqual(4, tok.len);
    }
}

test "tokenize var decl" {
    try expectTokens("var int var1;", &.{
        .keyword_var,
        .identifier,
        .identifier,
        .semicolon,
    });
}

test "tokenize array access" {
    try expectTokens("myvar[42]", &.{
        .identifier,
        .l_square_bracket,
        .integer_literal,
        .r_square_bracket,
    });
}

test "tokenizer let statement" {
    try expectTokens("let length = Keyboard.readInt(\"HOW MANY NUMBERS? \");", &.{
        .keyword_let,
        .identifier,
        .equal_sign,
        .identifier,
        .dot,
        .identifier,
        .l_bracket,
        .string_literal,
        .r_bracket,
        .semicolon,
    });
}

test "tokenize ExpressionLessSquare/Main.jack" {
    try expectTokens(
        \\/** Expressionless version of projects/10/Square/Main.jack. */
        \\
        \\class Main {
        \\}
    , &.{
        .keyword_class,
        .identifier,
        .l_curly_bracket,
        .r_curly_bracket,
    });
}
