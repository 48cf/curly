const std = @import("std");

const isa = @import("isa");

const TokenKind = enum {
    ident,
    integer,
    paren_open,
    paren_close,
    comma,
    colon,
};

const Token = struct {
    kind: TokenKind,
    start: usize,
    end: usize,
};

const Tokenizer = struct {
    source: []const u8,
    offset: usize,

    fn init(source: []const u8) Tokenizer {
        return .{
            .source = source,
            .offset = 0,
        };
    }

    fn isEof(self: *const Tokenizer) bool {
        return self.offset >= self.source.len;
    }

    fn next(self: *Tokenizer) ?Token {
        while (!self.isEof() and std.ascii.isSpace(self.source[self.offset])) {
            self.offset += 1;
        }

        if (self.isEof()) {
            return null;
        }

        var start = self.offset;
        var ch = self.source[start];

        self.offset += 1;

        switch (ch) {
            '(' => return Token{ .kind = .paren_open, .start = start, .end = self.offset },
            ')' => return Token{ .kind = .paren_close, .start = start, .end = self.offset },
            ',' => return Token{ .kind = .comma, .start = start, .end = self.offset },
            ':' => return Token{ .kind = .colon, .start = start, .end = self.offset },
            else => {},
        }

        if (std.ascii.isAlpha(ch) or ch == '_' or ch == '.' or ch == '$') {
            while (!self.isEof()) {
                const next_ch = self.source[self.offset];

                if (!std.ascii.isAlNum(next_ch) and next_ch != '_' and next_ch != '.' and next_ch != '$') {
                    break;
                }

                self.offset += 1;
            }

            return Token{ .kind = .ident, .start = start, .end = self.offset };
        } else if (std.ascii.isDigit(ch) or ch == '-') {
            if (ch == '-') {
                ch = self.source[start];
                self.offset += 1;
            }

            if (ch == '0' and !self.isEof()) {
                const next_ch = self.source[self.offset];

                if (next_ch != 'b' and next_ch != 'o' and next_ch != 'x' and !std.ascii.isXDigit(next_ch)) {
                    std.debug.panic("Invalid character in integer literal: {c}", .{next_ch});
                }

                self.offset += 1;
            }

            while (!self.isEof()) {
                const next_ch = self.source[self.offset];

                if (!std.ascii.isXDigit(next_ch)) {
                    break;
                }

                self.offset += 1;
            }

            return Token{ .kind = .integer, .start = start, .end = self.offset };
        }

        std.debug.panic("Invalid input character: {c}", .{ch});
    }
};

fn handleSourceFile(input: []const u8, writer: anytype) !void {
    _ = writer;

    var tokenizer = Tokenizer.init(input);

    while (tokenizer.next()) |token| {
        std.log.info("{} '{s}'", .{ token, input[token.start..token.end] });
    }
}

pub fn main() !void {
    var args = try std.process.argsWithAllocator(std.heap.page_allocator);
    _ = args.next();

    const source_path = args.next() orelse @panic("Expected source path as second argument");
    const output_path = args.next() orelse @panic("Expected output path as third argument");

    const source_file = try std.fs.cwd().openFile(source_path, .{});
    defer source_file.close();

    const output_file = try std.fs.cwd().createFile(output_path, .{ .truncate = true });
    defer output_file.close();

    const source = try source_file.readToEndAlloc(std.heap.page_allocator, 1024 * 1024 * 32);
    defer std.heap.page_allocator.free(source);

    try handleSourceFile(source, &output_file.writer());
}
