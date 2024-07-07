const std = @import("std");
const Ast = @import("Ast.zig");
const Tokenizer = @import("Tokenizer.zig");
const TokenTag = Tokenizer.Token.Tag;

const AnyWriter = std.io.AnyWriter;

const Self = @This();

tree: *const Ast,
writer: AnyWriter,
text: []const u8,
nodePointer: u32,

pub fn serialize(tree: *const Ast, writer: AnyWriter, text: []const u8) anyerror!void {
    var serializer = Self{
        .tree = tree,
        .writer = writer,
        .text = text,
        .nodePointer = 0,
    };

    try serializer.serializeClass();
}

fn serializeClass(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .class_decl);

    std.log.debug("writing {}", .{node.tag});

    try self.writer.writeAll("<class>\n");

    try self.writeToken(.keyword_class);

    // write class children
    var child_count = node.data.class_decl - 1; // one less because of the identifier

    self.nodePointer += 1;

    // write class name
    try self.writeIdentifier();

    try self.writeToken(.l_curly_bracket);

    while (child_count > 0) : (child_count -= 1) {
        const nextNode = self.tree.nodes.items[self.nodePointer];

        switch (nextNode.tag) {
            .static_decl,
            .field_decl,
            => try self.writeClassVarDecl(),
            .function_decl,
            .method_decl,
            .constructor_decl,
            => try self.writeFunctionDecl(),
            else => {
                std.log.debug("unexpected {}", .{nextNode.tag});
                unreachable;
            },
        }
    }

    try self.writeToken(.r_curly_bracket);
    try self.writer.writeAll("</class>\n");
}

fn writeClassVarDecl(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .static_decl or node.tag == .field_decl);

    std.log.debug("writing {}", .{node.tag});

    try self.writer.writeAll("<classVarDec>\n");

    switch (node.tag) {
        .static_decl => try self.writeToken(.keyword_static),
        .field_decl => try self.writeToken(.keyword_field),
        else => unreachable,
    }

    var child_count = switch (node.tag) {
        .static_decl => node.data.static_decl,
        .field_decl => node.data.field_decl,
        else => unreachable,
    };

    self.nodePointer += 1;

    try self.writeType();

    child_count -= 1;

    while (child_count > 1) : (child_count -= 1) {
        try self.writeIdentifier();
        try self.writeToken(.comma);
    }

    try self.writeIdentifier();

    try self.writeToken(.semicolon);

    try self.writer.writeAll("</classVarDec>\n");
}

fn writeVarDecl(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .var_decl);

    std.log.debug("writing {}", .{node.tag});

    try self.writer.writeAll("<varDec>\n");

    var child_count = node.data.var_decl - 1;

    try self.writeToken(.keyword_var);

    self.nodePointer += 1;

    try self.writeType();

    while (child_count > 1) : (child_count -= 1) {
        try self.writeIdentifier();
        try self.writeToken(.comma);
    }

    try self.writeIdentifier();

    try self.writeToken(.semicolon);

    try self.writer.writeAll("</varDec>\n");
}

fn writeFunctionDecl(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .function_decl or node.tag == .method_decl or node.tag == .constructor_decl);

    std.log.debug("writing {}", .{node.tag});

    try self.writer.writeAll("<subroutineDec>\n");

    switch (node.tag) {
        .function_decl => try self.writeToken(.keyword_function),
        .method_decl => try self.writeToken(.keyword_method),
        .constructor_decl => try self.writeToken(.keyword_constructor),
        else => unreachable,
    }

    var child_count = switch (node.tag) {
        .function_decl => node.data.function_decl,
        .method_decl => node.data.method_decl,
        .constructor_decl => node.data.constructor_decl,
        else => unreachable,
    };

    child_count -= 3;

    self.nodePointer += 1;

    try self.writeType();
    try self.writeIdentifier();
    try self.writeToken(.l_bracket);

    try self.writer.writeAll("<parameterList>\n");
    std.log.debug("begin parameter list", .{});

    while (child_count > 0) : (child_count -= 2) {
        std.log.debug("begin parameter", .{});
        try self.writeType();
        try self.writeIdentifier();

        if (child_count > 2)
            try self.writeToken(.comma);
    }

    try self.writer.writeAll("</parameterList>\n");

    try self.writeToken(.r_bracket);

    try self.writeFunctionBody();

    try self.writer.writeAll("</subroutineDec>\n");
}

fn writeFunctionBody(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .function_body);

    std.log.debug("writing {}", .{node.tag});

    try self.writer.writeAll("<subroutineBody>\n");
    try self.writeToken(.l_curly_bracket);

    var child_count = node.data.function_body - 1;

    self.nodePointer += 1;

    while (child_count > 0) : (child_count -= 1) {
        try self.writeVarDecl();
    }

    try self.writeStatementBlock();

    try self.writeToken(.r_curly_bracket);
    try self.writer.writeAll("</subroutineBody>\n");
}

fn writeLetStatement(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .let_statement);

    std.log.debug("writing {}", .{node.tag});

    try self.writer.writeAll("<letStatement>\n");

    try self.writeToken(.keyword_let);

    self.nodePointer += 1;

    const nextNode = self.tree.nodes.items[self.nodePointer];
    switch (nextNode.tag) {
        .identifier => try self.writeIdentifier(),
        .array_access => try self.writeArrayAccess(),
        else => unreachable,
    }

    try self.writeToken(.equal_sign);

    try self.writeExpression();

    try self.writeToken(.semicolon);

    try self.writer.writeAll("</letStatement>\n");
}

fn writeDoStatement(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .do_statement);

    std.log.debug("writing {}", .{node.tag});

    try self.writer.writeAll("<doStatement>\n");

    try self.writeToken(.keyword_do);

    self.nodePointer += 1;

    try self.writeFunctionCall();

    try self.writeToken(.semicolon);

    try self.writer.writeAll("</doStatement>\n");
}

fn writeStatementBlock(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .statement_block);

    std.log.debug("writing {}", .{node.tag});

    self.nodePointer += 1;

    try self.writer.writeAll("<statements>\n");

    var child_count = node.data.statement_block;
    while (child_count > 0) : (child_count -= 1) {
        const statementNode = self.tree.nodes.items[self.nodePointer];

        const prevPointer = self.nodePointer;

        switch (statementNode.tag) {
            .let_statement => try self.writeLetStatement(),
            .do_statement => try self.writeDoStatement(),
            .if_statement => try self.writeIfStatement(),
            .while_statement => try self.writeWhileStatement(),
            .return_statement => try self.writeReturnStatement(),
            else => {
                std.log.debug("unexprected {}", .{statementNode.tag});
                unreachable;
            },
        }

        if (prevPointer == self.nodePointer) {
            std.log.debug("node pointer did not advance on node {}", .{statementNode.tag});
        }
    }

    try self.writer.writeAll("</statements>\n");
}

fn writeIfStatement(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .if_statement);

    std.log.debug("writing {}", .{node.tag});

    try self.writer.writeAll("<ifStatement>\n");

    try self.writeToken(.keyword_if);

    const hasElseBlock = node.data.if_statement;

    self.nodePointer += 1;

    try self.writeToken(.l_bracket);
    try self.writeExpression();
    try self.writeToken(.r_bracket);

    try self.writeToken(.l_curly_bracket);
    try self.writeStatementBlock();
    try self.writeToken(.r_curly_bracket);

    if (hasElseBlock) {
        try self.writeToken(.keyword_else);

        try self.writeToken(.l_curly_bracket);
        try self.writeStatementBlock();
        try self.writeToken(.r_curly_bracket);
    }

    try self.writer.writeAll("</ifStatement>\n");
}

fn writeWhileStatement(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .while_statement);

    std.log.debug("writing {}", .{node.tag});

    try self.writer.writeAll("<whileStatement>\n");

    try self.writeToken(.keyword_while);

    self.nodePointer += 1;

    try self.writeToken(.l_bracket);
    try self.writeExpression();
    try self.writeToken(.r_bracket);

    try self.writeToken(.l_curly_bracket);
    try self.writeStatementBlock();
    try self.writeToken(.r_curly_bracket);

    try self.writer.writeAll("</whileStatement>\n");
}

fn writeReturnStatement(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .return_statement);

    std.log.debug("writing {}", .{node.tag});

    const hasReturnExpr = node.data.return_statement;

    try self.writer.writeAll("<returnStatement>\n");

    try self.writeToken(.keyword_return);

    self.nodePointer += 1;

    if (hasReturnExpr) {
        try self.writeExpression();
    }

    try self.writeToken(.semicolon);

    try self.writer.writeAll("</returnStatement>\n");
}

fn writeExpression(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .expression);

    std.log.debug("writing {}", .{node.tag});

    try self.writer.writeAll("<expression>\n");

    self.nodePointer += 1;

    var child_count = node.data.expression;
    while (child_count > 0) : (child_count -= 1) {
        const nextNode = self.tree.nodes.items[self.nodePointer];

        const prevPointer = self.nodePointer;

        switch (nextNode.tag) {
            .operator => try self.writeOperator(),
            else => try self.writeTerm(),
        }

        if (prevPointer == self.nodePointer) {
            std.log.debug("node pointer did not advance on node {}", .{nextNode.tag});
        }
    }

    try self.writer.writeAll("</expression>\n");
}

fn writeTerm(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .integer_literal or
        node.tag == .string_literal or
        node.tag == .constant or
        node.tag == .identifier or
        node.tag == .array_access or
        node.tag == .call or
        node.tag == .expression or
        node.tag == .unary_operator);

    std.log.debug("writing {}", .{node.tag});

    try self.writer.writeAll("<term>\n");

    switch (node.tag) {
        .integer_literal => try self.writeIntegerLiteral(),
        .string_literal => try self.writeStringLiteral(),
        .constant => try self.writeConstant(),
        .identifier => try self.writeIdentifier(),
        .array_access => try self.writeArrayAccess(),
        .call => try self.writeFunctionCall(),
        .expression => {
            try self.writeToken(.l_bracket);
            try self.writeExpression();
            try self.writeToken(.r_bracket);
        },
        .unary_operator => try self.writeUnaryOperator(),
        else => unreachable,
    }

    try self.writer.writeAll("</term>\n");
}

fn writeFunctionCall(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .call);

    std.log.debug("writing {}", .{node.tag});

    self.nodePointer += 1;

    const nextNode = self.tree.nodes.items[self.nodePointer];
    switch (nextNode.tag) {
        .identifier => try self.writeIdentifier(),
        .member_access => try self.writeMemberAccess(),
        else => unreachable,
    }

    try self.writeToken(.l_bracket);

    try self.writer.writeAll("<expressionList>\n");

    var child_count = node.data.call - 1;
    while (child_count > 0) : (child_count -= 1) {
        try self.writeExpression();
        if (child_count > 1) {
            try self.writeToken(.comma);
        }
    }

    try self.writer.writeAll("</expressionList>\n");

    try self.writeToken(.r_bracket);
}

fn writeUnaryOperator(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .unary_operator);

    std.log.debug("writing {}", .{node.tag});

    switch (node.data.unary_operator) {
        .not => try self.writeToken(.tilde),
        .neg => try self.writeToken(.minus),
    }

    self.nodePointer += 1;

    try self.writeTerm();
}

fn writeArrayAccess(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .array_access);

    std.log.debug("writing {}", .{node.tag});

    const stringIndex = node.data.array_access;
    const stringLoc = self.tree.strings.items[stringIndex];
    const string = self.text[stringLoc.start..(stringLoc.start + stringLoc.len)];

    try self.writer.print("<identifier> {s} </identifier>\n", .{string});

    self.nodePointer += 1;

    try self.writeToken(.l_square_bracket);
    try self.writeExpression();
    try self.writeToken(.r_square_bracket);
}

fn writeMemberAccess(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .member_access);

    std.log.debug("writing {}", .{node.tag});

    const stringIndex = node.data.member_access;
    const stringLoc = self.tree.strings.items[stringIndex];
    const string = self.text[stringLoc.start..(stringLoc.start + stringLoc.len)];

    try self.writer.print("<identifier> {s} </identifier>\n", .{string});

    try self.writeToken(.dot);

    self.nodePointer += 1;

    try self.writeIdentifier();
}

fn writeOperator(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .operator);

    std.log.debug("writing {}", .{node.tag});

    switch (node.data.operator) {
        .add => try self.writeToken(.plus),
        .sub => try self.writeToken(.minus),
        .mul => try self.writeToken(.asterisk),
        .div => try self.writeToken(.slash),
        .@"and" => try self.writeToken(.ampersand),
        .@"or" => try self.writeToken(.pipe),
        .less_than => try self.writeToken(.l_diamond),
        .greater_than => try self.writeToken(.r_diamond),
        .equal => try self.writeToken(.equal_sign),
    }

    self.nodePointer += 1;
}

fn writeConstant(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .constant);

    std.log.debug("writing {}", .{node.tag});

    switch (node.data.constant) {
        .true => try self.writeToken(.keyword_true),
        .false => try self.writeToken(.keyword_false),
        .null => try self.writeToken(.keyword_null),
        .this => try self.writeToken(.keyword_this),
    }

    self.nodePointer += 1;
}

fn writeIdentifier(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .identifier);

    std.log.debug("writing {}", .{node.tag});

    const stringIndex = node.data.identifier;
    const stringLoc = self.tree.strings.items[stringIndex];
    const string = self.text[stringLoc.start..(stringLoc.start + stringLoc.len)];

    try self.writer.print("<identifier> {s} </identifier>\n", .{string});

    self.nodePointer += 1;
}

fn writeStringLiteral(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .string_literal);

    std.log.debug("writing {}", .{node.tag});

    const stringIndex = node.data.string_literal;
    const stringLoc = self.tree.strings.items[stringIndex];
    const stringEnd = stringLoc.start + stringLoc.len;
    const string = self.text[stringLoc.start..stringEnd];

    try self.writer.writeAll("<stringConstant> ");

    for (string) |c| {
        // ljhsddl i hate xml asdasdasdasdsasd
        switch (c) {
            '\'' => try self.writer.writeAll("&apos;"),
            '&' => try self.writer.writeAll("&amp;"),
            '<' => try self.writer.writeAll("&lt;"),
            '>' => try self.writer.writeAll("&gt;"),
            else => try self.writer.writeByte(c),
        }
    }

    try self.writer.writeAll(" </stringConstant>\n");

    self.nodePointer += 1;
}

fn writeIntegerLiteral(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .integer_literal);

    std.log.debug("writing {}", .{node.tag});

    try self.writer.print(
        "<integerConstant> {} </integerConstant>\n",
        .{node.data.integer_literal},
    );

    self.nodePointer += 1;
}

fn writeType(self: *Self) anyerror!void {
    const node = self.tree.nodes.items[self.nodePointer];
    std.debug.assert(node.tag == .identifier);

    std.log.debug("writing {}", .{node.tag});

    const textLoc = self.tree.strings.items[node.data.identifier];
    const identifier = self.text[textLoc.start..(textLoc.start + textLoc.len)];

    if (std.mem.eql(u8, identifier, "int")) {
        try self.writer.writeAll("<keyword> int </keyword>");
        self.nodePointer += 1;
    } else if (std.mem.eql(u8, identifier, "boolean")) {
        try self.writer.writeAll("<keyword> boolean </keyword>");
        self.nodePointer += 1;
    } else if (std.mem.eql(u8, identifier, "char")) {
        try self.writer.writeAll("<keyword> char </keyword>");
        self.nodePointer += 1;
    } else if (std.mem.eql(u8, identifier, "void")) {
        try self.writer.writeAll("<keyword> void </keyword>");
        self.nodePointer += 1;
    } else {
        try self.writeIdentifier();
    }
}

fn writeToken(self: *Self, token: TokenTag) anyerror!void {
    switch (token) {
        .l_bracket => try self.writer.writeAll("<symbol> ( </symbol>"),
        .r_bracket => try self.writer.writeAll("<symbol> ) </symbol>"),
        .l_curly_bracket => try self.writer.writeAll("<symbol> { </symbol>"),
        .r_curly_bracket => try self.writer.writeAll("<symbol> } </symbol>"),
        .l_square_bracket => try self.writer.writeAll("<symbol> [ </symbol>"),
        .r_square_bracket => try self.writer.writeAll("<symbol> ] </symbol>"),
        .dot => try self.writer.writeAll("<symbol> . </symbol>"),
        .comma => try self.writer.writeAll("<symbol> , </symbol>"),
        .semicolon => try self.writer.writeAll("<symbol> ; </symbol>"),
        .plus => try self.writer.writeAll("<symbol> + </symbol>"),
        .minus => try self.writer.writeAll("<symbol> - </symbol>"),
        .asterisk => try self.writer.writeAll("<symbol> * </symbol>"),
        .slash => try self.writer.writeAll("<symbol> / </symbol>"),
        .ampersand => try self.writer.writeAll("<symbol>&amp;</symbol>"),
        .pipe => try self.writer.writeAll("<symbol> | </symbol>"),
        .l_diamond => try self.writer.writeAll("<symbol> &lt; </symbol>"),
        .r_diamond => try self.writer.writeAll("<symbol> &gt; </symbol>"),
        .equal_sign => try self.writer.writeAll("<symbol> = </symbol>"),
        .tilde => try self.writer.writeAll("<symbol> ~ </symbol>"),

        .keyword_class => try self.writer.writeAll("<keyword> class </keyword>"),
        .keyword_constructor => try self.writer.writeAll("<keyword> constructor </keyword>"),
        .keyword_do => try self.writer.writeAll("<keyword> do </keyword>"),
        .keyword_else => try self.writer.writeAll("<keyword> else </keyword>"),
        .keyword_false => try self.writer.writeAll("<keyword> false </keyword>"),
        .keyword_field => try self.writer.writeAll("<keyword> field </keyword>"),
        .keyword_function => try self.writer.writeAll("<keyword> function </keyword>"),
        .keyword_if => try self.writer.writeAll("<keyword> if </keyword>"),
        .keyword_let => try self.writer.writeAll("<keyword> let </keyword>"),
        .keyword_method => try self.writer.writeAll("<keyword> method </keyword>"),
        .keyword_null => try self.writer.writeAll("<keyword> null </keyword>"),
        .keyword_return => try self.writer.writeAll("<keyword> return </keyword>"),
        .keyword_static => try self.writer.writeAll("<keyword> static </keyword>"),
        .keyword_this => try self.writer.writeAll("<keyword> this </keyword>"),
        .keyword_true => try self.writer.writeAll("<keyword> true </keyword>"),
        .keyword_var => try self.writer.writeAll("<keyword> var </keyword>"),
        .keyword_while => try self.writer.writeAll("<keyword> while </keyword>"),
        else => unreachable,
    }

    try self.writer.writeByte('\n');
}
