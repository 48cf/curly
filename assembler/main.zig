const std = @import("std");

const isa = @import("isa");

const TokenValue = union(enum) {
    eof,
    ident,
    integer: u64,
    string: []const u8,
    paren_open,
    paren_close,
    comma,
    colon,
    register: isa.Register,
    msr: isa.Msr,
    mnemonic: Mnemonic,
    keyword: Keyword,
};

const TokenKind = @typeInfo(TokenValue).Union.tag_type.?;

const Token = struct {
    value: TokenValue,
    start: usize,
    source_bytes: []const u8,
};

fn charsToEnum(comptime E: type, chars: []const u8) ?E {
    inline for (@typeInfo(E).Enum.fields) |field| {
        if (std.mem.eql(u8, chars, field.name)) {
            return @field(E, field.name);
        }
    }
    return null;
}

const Mnemonic = enum {
    @"add",
    @"sub",
    @"mul",
    @"div",
    @"mod",

    @"and",
    @"or",
    @"xor",
    @"sex",
    @"shl",
    @"lsr",
    @"asr",

    @"jeq",
    @"jne",
    @"jlt",
    @"jle",
    @"jgt",
    @"jge",
    @"jz",
    @"jnz",

    @"jlr",
    @"adr",

    @"rmsr",
    @"wmsr",

    @"mcp",
    @"mst",

    @"hlt",
    @"udi",

    @"ld",
    @"ld.b",
    @"ld.w",
    @"ld.d",
    @"ld.q",
    @"ldp",
    @"ldp.b",
    @"ldp.w",
    @"ldp.d",
    @"ldp.q",

    @"st",
    @"st.b",
    @"st.w",
    @"st.d",
    @"st.q",
    @"stp",
    @"stp.b",
    @"stp.w",
    @"stp.d",
    @"stp.q",

    // Pseudoinstructions
    @"ret",
    @"call",
    @"jmp",

    @"ldi",
};

const Keyword = enum {
    @".align",
    @".include",
    @".db",
    @".dw",
    @".dd",
    @".dq",
};

fn hexValue(ch: u8) u4 {
    if ('a' <= ch and ch <= 'f') {
        return @intCast(u4, (ch - 'a') + 10);
    } else if ('A' <= ch and ch <= 'F') {
        return @intCast(u4, (ch - 'A') + 10);
    } else if ('0' <= ch and ch <= '9') {
        return @intCast(u4, ch - '0');
    } else {
        unreachable;
    }
}

const Tokenizer = struct {
    source: []const u8,
    offset: usize,
    next_token: ?Token,

    fn init(source: []const u8) @This() {
        return .{
            .source = source,
            .offset = 0,
            .next_token = null,
        };
    }

    fn isEof(self: *const @This()) bool {
        return self.offset >= self.source.len;
    }

    fn expect(self: *@This(), kind: TokenKind) Token {
        const token = self.next();
        if (token.value != kind) {
            std.debug.panic("Error: expected {s}, found {}!", .{ @tagName(kind), token.value });
        }
        return token;
    }

    fn peek(self: *@This()) Token {
        if (self.next_token) |token| {
            return token;
        }

        self.next_token = self.makeToken();
        return self.next_token.?;
    }

    fn next(self: *@This()) Token {
        const result = self.peek();
        self.next_token = null;
        return result;
    }

    fn makeToken(self: *@This()) Token {
        while (!self.isEof() and std.ascii.isSpace(self.source[self.offset])) {
            self.offset += 1;
        }

        if (self.isEof()) {
            return .{ .value = .eof, .start = self.offset, .source_bytes = self.source[self.offset..self.offset] };
        }

        var start = self.offset;
        var ch = self.source[start];

        self.offset += 1;

        switch (ch) {
            '(' => return .{ .value = .paren_open, .start = start, .source_bytes = self.source[start..self.offset] },
            ')' => return .{ .value = .paren_close, .start = start, .source_bytes = self.source[start..self.offset] },
            ',' => return .{ .value = .comma, .start = start, .source_bytes = self.source[start..self.offset] },
            ':' => return .{ .value = .colon, .start = start, .source_bytes = self.source[start..self.offset] },
            else => {},
        }

        if (ch == '"') {
            var value = std.ArrayList(u8).init(std.heap.page_allocator);

            while (!self.isEof() and self.source[self.offset] != '"') {
                value.append(self.parseLiteralChar()) catch unreachable;
            }

            self.offset += 1;

            return .{ .value = .{ .string = value.toOwnedSlice() }, .start = start, .source_bytes = self.source[start..self.offset] };
        }  else if (std.ascii.isAlpha(ch) or ch == '_' or ch == '.' or ch == '$') {
            while (!self.isEof()) {
                const next_ch = self.source[self.offset];

                if (!std.ascii.isAlNum(next_ch) and next_ch != '_' and next_ch != '.' and next_ch != '$') {
                    break;
                }

                self.offset += 1;
            }

            if (charsToEnum(Mnemonic, self.source[start..self.offset])) |m| {
                return .{ .value = .{ .mnemonic = m }, .start = start, .source_bytes = self.source[start..self.offset] };
            } else if (charsToEnum(Keyword, self.source[start..self.offset])) |kw| {
                return .{ .value = .{ .keyword = kw }, .start = start, .source_bytes = self.source[start..self.offset] };
            } else if (charsToEnum(isa.Register, self.source[start..self.offset])) |reg| {
                return .{ .value = .{ .register = reg }, .start = start, .source_bytes = self.source[start..self.offset] };
            } else if (charsToEnum(isa.Msr, self.source[start..self.offset])) |msr| {
                return .{ .value = .{ .msr = msr }, .start = start, .source_bytes = self.source[start..self.offset] };
            }

            return .{ .value = .ident, .start = start, .source_bytes = self.source[start..self.offset] };
        } else if (std.ascii.isDigit(ch) or ch == '-' or ch == '~') {
            const value = blk: {
                switch (ch) {
                    '-' => break :blk @bitCast(u64, -@bitCast(i64, self.parseUnsigned())),
                    '~' => break :blk ~self.parseUnsigned(),
                    else => {
                        self.offset -= 1;
                        break :blk self.parseUnsigned();
                    },
                }
            };

            return .{ .value = .{ .integer = value }, .start = start, .source_bytes = self.source[start..self.offset] };
        }

        std.debug.panic("Invalid input character: {c}", .{ch});
    }

    fn parseUnsignedBase(self: *@This(), base: u64) u64 {
        var result: u64 = 0;
        while (!self.isEof()) {
            const next_ch = self.source[self.offset];
            if (std.ascii.isXDigit(next_ch)) {
                result *= base;
                result += hexValue(next_ch);
            } else {
                return result;
            }
            self.offset += 1;
        }
        unreachable;
    }

    fn parseUnsigned(self: *@This()) u64 {
        var ch = self.source[self.offset];
        if (ch == '0' and !self.isEof()) {
            switch (self.source[self.offset + 1]) {
                'b' => {
                    self.offset += 2;
                    return self.parseUnsignedBase(2);
                },
                'o' => {
                    self.offset += 2;
                    return self.parseUnsignedBase(8);
                },
                'x' => {
                    self.offset += 2;
                    return self.parseUnsignedBase(16);
                },
                else => {},
            }
        }
        return self.parseUnsignedBase(10);
    }

    fn readRegister(self: *@This()) isa.Register {
        return self.expect(.register).value.register;
    }

    fn parseLiteralChar(self: *@This()) u8 {
        switch (self.source[self.offset]) {
            '\\' => {
                switch (self.source[self.offset + 1]) {
                    'x' => {
                        const result = std.fmt.parseUnsigned(u8, self.source[self.offset + 2..][0..2], 16) catch unreachable;
                        self.offset += 4;
                        return result;
                    },
                    't' => {
                        self.offset += 2;
                        return '\t';
                    },
                    'n' => {
                        self.offset += 2;
                        return '\n';
                    },
                    'r' => {
                        self.offset += 2;
                        return '\r';
                    },
                    'e' => {
                        self.offset += 2;
                        return '\x1b';
                    },
                    else => {
                        const result = self.source[self.offset + 1];
                        self.offset += 2;
                        return result;
                    }
                }
            },
            0 => @panic("Whoops"),
            else => {
                const result = self.source[self.offset];
                self.offset += 1;
                return result;
            },
        }
    }
};

const RelocationKind = enum {
    abs64,
    m_imm_pcrel,
    m_imm_pcrel_div4,
    l_imm_pcrel_div4,
};

const Relocation = struct {
    write_offset: usize,
    value_offset: usize,
    source_bytes: []const u8,
    kind: RelocationKind,

    fn patch(self: @This(), data: []u8, value: usize, base_address: u64) void {
        const offset = value +% self.value_offset -% (self.write_offset + 4);
        const old_value = std.mem.readIntLittle(u32, data[self.write_offset..][0..4]);

        switch (self.kind) {
            .abs64 => std.mem.writeIntLittle(
                u64,
                data[self.write_offset..][0..8],
                value + self.value_offset + base_address,
            ),
            .m_imm_pcrel => std.mem.writeIntLittle(
                u32,
                data[self.write_offset..][0..4],
                (old_value & 0x0000FFFF) | @truncate(u32, offset << 16),
            ),
            .m_imm_pcrel_div4 => std.mem.writeIntLittle(
                u32,
                data[self.write_offset..][0..4],
                (old_value & 0x0000FFFF) | @truncate(u32, (offset >> 2) << 16),
            ),
            .l_imm_pcrel_div4 => std.mem.writeIntLittle(
                u32,
                data[self.write_offset..][0..4],
                (old_value & 0x000007FF) | @truncate(u32, (offset >> 2) << 11),
            ),
        }
    }
};

const Writer = struct {
    base_address: u64,
    output_data: *std.ArrayList(u8),

    relocations: std.ArrayList(Relocation),
    labels: std.StringHashMap(usize),

    fn init(base_addr: u64, output: *std.ArrayList(u8)) @This() {
        return .{
            .base_address = base_addr,
            .output_data = output,
            .relocations = std.ArrayList(Relocation).init(output.allocator),
            .labels = std.StringHashMap(usize).init(output.allocator),
        };
    }

    fn addLabel(self: *@This(), label: Token) !void {
        try self.alignTo(4, 0x00);
        var i: usize = 0;
        while (i < self.relocations.items.len) {
            if (std.mem.eql(u8, self.relocations.items[i].source_bytes, label.source_bytes)) {
                self.relocations.items[i].patch(self.output_data.items, self.output_data.items.len, self.base_address);
                _ = self.relocations.swapRemove(i);
            } else {
                i += 1;
            }
        }
        try self.labels.put(label.source_bytes, self.output_data.items.len);
    }

    fn alignTo(self: *@This(), alignment: usize, align_byte: u8) !void {
        if (self.output_data.items.len % alignment != 0) {
            try self.output_data.appendNTimes(align_byte, alignment - (self.output_data.items.len % alignment));
        }
    }

    fn embedBytes(self: *@This(), bytes: []const u8) !void {
        try self.output_data.appendSlice(bytes);
    }

    fn embedInt(self: *@This(), comptime T: type, value: T) !void {
        try self.embedBytes(&std.mem.toBytes(value));
    }

    fn getLabelValue(self: *@This(), tok: Token) ?usize {
        var it = self.labels.iterator();
        while (it.next()) |label| {
            if (std.mem.eql(u8, label.key_ptr.*, tok.source_bytes)) {
                return label.value_ptr.*;
            }
        }
        return null;
    }

    fn maybeRelocLastInstr(self: *@This(), label: Token, kind: RelocationKind, offset: u64) !void {
        const reloc = Relocation{
            .write_offset = self.output_data.items.len - 4,
            .source_bytes = label.source_bytes,
            .kind = kind,
            .value_offset = offset,
        };

        if (self.getLabelValue(label)) |value| {
            reloc.patch(self.output_data.items, value, self.base_address);
        } else {
            try self.relocations.append(reloc);
        }
    }

    fn r(
        self: *@This(),
        opcode: isa.RTypeCode,
        size: isa.OperandSize,
        dest: isa.Register,
        lhs: isa.Register,
        rhs: isa.Register,
        imm: u6,
    ) !void {
        try self.alignTo(4, 0x00);
        try self.embedInt(u32, isa.Instruction.encode(.{ .r = .{
            .code = opcode,
            .size = size,
            .dest = dest,
            .lhs = lhs,
            .rhs = rhs,
            .imm = imm,
        } }));
    }

    fn m_label_pcrel(
        self: *@This(),
        code: isa.MTypeCode,
        reg1: isa.Register,
        reg2: isa.Register,
        label: Token,
    ) !void {
        try self.m(code, reg1, reg2, 0);
        try self.maybeRelocLastInstr(label, .m_imm_pcrel_div4, 0);
    }

    fn m(
        self: *@This(),
        code: isa.MTypeCode,
        reg1: isa.Register,
        reg2: isa.Register,
        imm: u16,
    ) !void {
        try self.alignTo(4, 0x00);
        try self.embedInt(u32, isa.Instruction.encode(.{ .m = .{
            .code = code,
            .reg1 = reg1,
            .reg2 = reg2,
            .imm = imm,
        } }));
    }

    fn l_signed(
        self: *@This(),
        code: isa.LTypeCode,
        reg: isa.Register,
        imm: i21,
    ) !void {
        try self.alignTo(4, 0x00);
        try self.embedInt(u32, isa.Instruction.encode(.{ .l = .{
            .code = code,
            .reg = reg,
            .imm = imm,
        } }));
    }

    fn l_unsigned(
        self: *@This(),
        code: isa.LTypeCode,
        reg: isa.Register,
        imm: u21,
    ) !void {
        try self.l_signed(code, reg, @bitCast(i21, imm));
    }

    fn l_label_pcrel(
        self: *@This(),
        code: isa.LTypeCode,
        reg: isa.Register,
        label: Token,
    ) !void {
        try self.l_unsigned(code, reg, 0);
        try self.maybeRelocLastInstr(label, .l_imm_pcrel_div4, 0);
    }
};

const AssemblerError = std.fs.File.OpenError || std.fs.File.ReadError || std.mem.Allocator.Error;

fn handleSourceFile(path: []const u8, input: []const u8, writer: *Writer) AssemblerError!void {
    var tokenizer = Tokenizer.init(input);

    while (true) {
        const token = tokenizer.next();

        // std.log.info("Token: {}", .{token});

        switch (token.value) {
            .eof => return,
            .mnemonic => |m| switch (m) {
                .@"jlr" => {
                    const link = tokenizer.readRegister();
                    _ = tokenizer.expect(.comma);

                    const target = tokenizer.next();
                    switch (target.value) {
                        .register => |dest| {
                            try writer.r(
                                .@"jlr",
                                .qword,
                                link,
                                dest,
                                .zero, // unused
                                0, // unused
                            );
                        },
                        .ident => {
                            try writer.l_label_pcrel(
                                .@"jlr",
                                link,
                                target,
                            );
                        },
                        else => @panic("Expected register or label!"),
                    }
                },
                .@"jeq", .@"jne", .@"jlt", .@"jle", .@"jgt", .@"jge" => {
                    const m_opcode: isa.MTypeCode = switch (m) {
                        .@"jeq" => .@"jeq",
                        .@"jne" => .@"jne",
                        .@"jlt" => .@"jlt",
                        .@"jle" => .@"jle",
                        .@"jgt" => .@"jgt",
                        .@"jge" => .@"jge",
                        else => unreachable,
                    };

                    const lhs = tokenizer.readRegister();
                    _ = tokenizer.expect(.comma);
                    const rhs = tokenizer.readRegister();
                    _ = tokenizer.expect(.comma);
                    const label = tokenizer.expect(.ident);

                    try writer.m_label_pcrel(m_opcode, lhs, rhs, label);
                },
                .@"jz", .@"jnz" => {
                    const l_opcode: isa.LTypeCode = switch (m) {
                        .@"jz" => .@"jz",
                        .@"jnz" => .@"jnz",
                        else => unreachable,
                    };

                    const lhs = tokenizer.readRegister();
                    _ = tokenizer.expect(.comma);
                    const label = tokenizer.expect(.ident);

                    try writer.l_label_pcrel(l_opcode, lhs, label);
                },
                .@"rmsr", .@"wmsr" => {
                    const reg = tokenizer.readRegister();
                    _ = tokenizer.expect(.comma);
                    const msr = tokenizer.expect(.msr);

                    try writer.l_unsigned(
                        switch (m) {
                            .@"rmsr" => .@"rmsr",
                            .@"wmsr" => .@"wmsr",
                            else => unreachable,
                        },
                        reg,
                        @enumToInt(msr.value.msr),
                    );
                },
                .@"hlt" => try writer.r(.@"hlt", .qword, .zero, .zero, .zero, 0),
                .@"udi" => try writer.r(.@"udi", .qword, .zero, .zero, .zero, 0),
                .@"ld", .@"ld.b", .@"ld.w", .@"ld.d", .@"ld.q", .@"st", .@"st.b", .@"st.w", .@"st.d", .@"st.q" => {
                    const value_reg = tokenizer.readRegister();
                    _ = tokenizer.expect(.comma);

                    const m_opcode: isa.MTypeCode = switch (m) {
                        .@"ld.b" => .@"ld.b",
                        .@"st.b" => .@"st.b",
                        .@"ld.w" => .@"ld.w",
                        .@"st.w" => .@"st.w",
                        .@"ld.d" => .@"ld.d",
                        .@"st.d" => .@"st.d",
                        .@"ld", .@"ld.q" => .@"ld.q",
                        .@"st", .@"st.q" => .@"st.q",
                        else => unreachable,
                    };

                    switch (tokenizer.next().value) {
                        .paren_open => {
                            // M-type reg relative load/store
                            const label = tokenizer.next();
                            switch (label.value) {
                                .ident => {
                                    // PC-Relative label access
                                    try writer.m(
                                        m_opcode,
                                        value_reg,
                                        .pc,
                                        0, // relocated
                                    );
                                    try writer.maybeRelocLastInstr(label, .m_imm_pcrel, 0);
                                },
                                .register => |memory_reg| {
                                    // Register relative immediate access
                                    try writer.m(m_opcode, value_reg, memory_reg, 0);
                                },
                                else => @panic("Bad load/store memory operand!"),
                            }
                            _ = tokenizer.expect(.paren_close);
                        },
                        .integer => |imm| {
                            // Do we have a register operand too?
                            if (tokenizer.peek().value == .paren_open) {
                                // M-type reg relative load/store
                                _ = tokenizer.expect(.paren_open);
                                const label = tokenizer.next();
                                switch (label.value) {
                                    .ident => {
                                        // PC-Relative label access
                                        try writer.m(
                                            m_opcode,
                                            value_reg,
                                            .pc,
                                            0, // relocated
                                        );
                                        try writer.maybeRelocLastInstr(label, .m_imm_pcrel, imm);
                                    },
                                    .register => |memory_reg| {
                                        // Register relative immediate access
                                        try writer.m(m_opcode, value_reg, memory_reg, @truncate(u16, imm));
                                    },
                                    else => @panic("Bad load/store memory operand!"),
                                }
                                _ = tokenizer.expect(.paren_close);
                            } else {
                                // L-type load/store
                                const opcode: isa.LTypeCode = switch (m) {
                                    .@"ld.d" => .@"ld.d",
                                    .@"ld.q" => .@"ld.q",
                                    .@"st.d" => .@"st.d",
                                    .@"st.q" => .@"st.q",
                                    else => unreachable,
                                };

                                try writer.l_unsigned(
                                    opcode,
                                    value_reg,
                                    @truncate(u21, imm),
                                );
                            }
                        },
                        else => @panic("Bad load/store second operand"),
                    }
                },
                // zig fmt: off
                .@"add", .@"sub", .@"mul", .@"div", .@"mod",
                .@"and", .@"or", .@"xor", .@"sex", .@"shl", .@"lsr", .@"asr",
                .@"ldp.b", .@"ldp.w", .@"ldp.d", .@"ldp.q", .@"ldp",
                .@"stp.b", .@"stp.w", .@"stp.d", .@"stp.q", .@"stp",
                .@"mcp", .@"mst",
                // zig fmt: on
                => {
                    const dest = tokenizer.readRegister();
                    _ = tokenizer.expect(.comma);
                    const lhs = tokenizer.readRegister();
                    _ = tokenizer.expect(.comma);

                    switch (tokenizer.next().value) {
                        .integer => |imm| {
                            const opcode: isa.MTypeCode = switch (m) {
                                .@"add" => .@"add",
                                .@"sub" => .@"sub",
                                else => unreachable,
                            };

                            try writer.m(
                                opcode,
                                dest,
                                lhs,
                                @intCast(u16, imm),
                            );
                        },
                        .register => |rhs| {
                            const opcode: isa.RTypeCode = switch (m) {
                                .@"add" => .@"add",
                                .@"sub" => .@"sub",
                                .@"mul" => .@"mul",
                                .@"div" => .@"div",
                                .@"mod" => .@"mod",
                                .@"and" => .@"and",
                                .@"or" => .@"or",
                                .@"xor" => .@"xor",
                                .@"sex" => .@"sex",
                                .@"shl" => .@"shl",
                                .@"lsr" => .@"lsr",
                                .@"asr" => .@"asr",
                                .@"mcp" => .@"mcp",
                                .@"mst" => .@"mst",
                                else => unreachable,
                            };

                            try writer.r(
                                opcode,
                                .qword, // TODO: Truncating versions of these mnemonics??
                                dest,
                                lhs,
                                rhs,
                                0, // TODO: Immediate/shamts
                            );
                        },
                        .paren_open => {
                            const memory_reg = tokenizer.readRegister();
                            _ = tokenizer.expect(.paren_close);

                            const opcode: isa.RTypeCode = switch (m) {
                                .@"ldp.b", .@"ldp.w", .@"ldp.d", .@"ldp.q", .@"ldp" => .@"ldp",
                                .@"stp.b", .@"stp.w", .@"stp.d", .@"stp.q", .@"stp" => .@"stp",
                                else => unreachable,
                            };

                            const size: isa.OperandSize = switch (m) {
                                .@"ldp.b", .@"stp.b" => .byte,
                                .@"ldp.w", .@"stp.w" => .word,
                                .@"ldp.d", .@"stp.d" => .dword,
                                .@"ldp", .@"stp", .@"ldp.q", .@"stp.q" => .qword,
                                else => unreachable,
                            };

                            try writer.r(
                                opcode,
                                size,
                                dest,
                                lhs,
                                memory_reg,
                                0,
                            );
                        },
                        else => @panic("Expected integer or register!"),
                    }
                },
                .@"ret" => {
                    try writer.r(
                        .@"jlr",
                        .qword,
                        .zero,
                        .ra,
                        .zero, // unused
                        0, // unused
                    );
                },
                .@"adr" => {
                    const reg = tokenizer.expect(.register).value.register;
                    _ = tokenizer.expect(.comma);
                    const label = tokenizer.expect(.ident);
                    try writer.l_label_pcrel(.@"adr", reg, label);
                },
                .@"call" => {
                    const next = tokenizer.next();

                    switch (next.value) {
                        .ident => try writer.l_label_pcrel(.@"jlr", .ra, next),
                        .register => |reg| try writer.r(.@"jlr", .qword, .ra, reg, .zero, 0),
                        else => unreachable,
                    }
                },
                .@"jmp" => {
                    try writer.l_label_pcrel(
                        .@"jlr",
                        .zero,
                        tokenizer.expect(.ident),
                    );
                },
                .@"ldi" => {
                    const dest = tokenizer.readRegister();
                    _ = tokenizer.expect(.comma);
                    const imm = tokenizer.expect(.integer).value.integer;

                    if (@bitCast(i64, imm) < 0) {
                        try writer.m(.@"sub", dest, .zero, @bitCast(u16, @truncate(i16, -@bitCast(i64, imm))));
                    } else {
                        try writer.m(.@"add", dest, .zero, @truncate(u16, imm));
                    }
                },
            },
            .keyword => |kw| switch (kw) {
                .@".align" => {
                    var alignment = tokenizer.expect(.integer);
                    var align_value: u8 = 0x00;

                    if (tokenizer.peek().value == .comma) {
                        _ = tokenizer.expect(.comma);
                        align_value = @truncate(u8, tokenizer.expect(.integer).value.integer);
                    }

                    try writer.alignTo(alignment.value.integer, align_value);
                },
                .@".include" => {
                    const file_name = tokenizer.expect(.ident); // FIXME: Use proper string literals there
                    const current_source_dir = std.fs.path.dirname(path) orelse ".";
                    const include_path = try std.fs.path.join(std.heap.page_allocator, &.{ current_source_dir, file_name.source_bytes });

                    const source_file = try std.fs.cwd().openFile(include_path, .{});
                    defer source_file.close();

                    const source = try source_file.readToEndAlloc(std.heap.page_allocator, std.math.maxInt(usize));

                    try handleSourceFile(include_path, source, writer);
                },
                .@".db", .@".dw", .@".dd", .@".dq" => {
                    while (true) {
                        const operand = tokenizer.next();

                        switch (operand.value) {
                            .integer => |imm| switch (kw) {
                                .@".db" => try writer.embedInt(u8, @truncate(u8, imm)),
                                .@".dw" => try writer.embedInt(u16, @truncate(u16, imm)),
                                .@".dd" => try writer.embedInt(u32, @truncate(u32, imm)),
                                .@".dq" => try writer.embedInt(u64, @truncate(u64, imm)),
                                else => unreachable,
                            },
                            .string => |value| switch (kw) {
                                .@".db" => try writer.embedBytes(value),
                                else => unreachable,
                            },
                            .ident => {
                                if (kw != .@".dq") {
                                    @panic("Can't use label reference with .db, .dw or .dd!");
                                }

                                if (writer.getLabelValue(operand)) |value| {
                                    try writer.embedInt(u64, value);
                                } else {
                                    try writer.embedInt(u64, 0x00);
                                    try writer.relocations.append(.{
                                        .write_offset = writer.output_data.items.len - 8,
                                        .source_bytes = operand.source_bytes,
                                        .kind = .abs64,
                                        .value_offset = 0,
                                    });
                                }
                            },
                            else => std.debug.panic("Expected integer, string or ident, got {}", .{operand}),
                        }

                        if (tokenizer.peek().value != .comma) {
                            break;
                        }

                        _ = tokenizer.expect(.comma);
                    }
                },
            },
            .ident => {
                try writer.addLabel(token);
                _ = tokenizer.expect(.colon);
            },
            else => std.debug.panic("Expected mnemonic, directive or label, got {}", .{token}),
        }
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

    const source = try source_file.readToEndAlloc(std.heap.page_allocator, std.math.maxInt(usize));
    defer std.heap.page_allocator.free(source);

    var output_bytes = std.ArrayList(u8).init(std.heap.page_allocator);
    var writer = Writer.init(0x10000000, &output_bytes);

    try handleSourceFile(source_path, source, &writer);
    try output_file.writeAll(output_bytes.items);

    for (writer.relocations.items) |reloc| {
        std.log.err("Undefined relocation to '{s}' at 0x{X:0>8}", .{ reloc.source_bytes, reloc.write_offset });
    }
}
