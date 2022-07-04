const std = @import("std");

const registers = @import("registers.zig");

pub const Encoding = enum(u2) { r, m, l };

pub const OperandSize = enum(u2) {
    byte = 0b00,
    word = 0b01,
    dword = 0b10,
    qword = 0b11,
};

pub const RTypeCode = enum(u7) {
    @"add" = 0b0000000,
    @"sub" = 0b0000001,
    @"mul" = 0b0000010,
    @"div" = 0b0000011,
    @"mod" = 0b0000100,
    // ...
    @"and" = 0b0010000,
    @"or" = 0b0010001,
    @"xor" = 0b0010010,
    @"sex" = 0b0010011,
    @"shl" = 0b0010100,
    @"lsr" = 0b0010101,
    @"asr" = 0b0010110,
    // ...
    @"jlr" = 0b1000000,
    // ...
    @"ldp" = 0b1110000,
    @"stp" = 0b1110001,
    @"mcp" = 0b1110010,
    @"mst" = 0b1110011,
    // ...
    @"hlt" = 0b1111110,
    @"udi" = 0b1111111,
};

pub const MTypeCode = enum(u4) {
    @"add" = 0b0000,
    @"sub" = 0b0001,
    @"beq" = 0b0010,
    @"bne" = 0b0011,
    @"blt" = 0b0100,
    @"ble" = 0b0101,
    @"bgt" = 0b0110,
    @"bge" = 0b0111,
    @"ld.b" = 0b1000,
    @"ld.w" = 0b1001,
    @"ld.d" = 0b1010,
    @"ld.q" = 0b1011,
    @"st.b" = 0b1100,
    @"st.w" = 0b1101,
    @"st.d" = 0b1110,
    @"st.q" = 0b1111,
};

pub const LTypeCode = enum(u4) {
    @"ld.d" = 0b0000,
    @"ld.q" = 0b0001,
    @"st.d" = 0b0010,
    @"st.q" = 0b0011,
    @"and.d" = 0b0100,
    @"or.d" = 0b0101,
    @"xor.d" = 0b0110,
    // ...
    @"jz" = 0b1000,
    @"jnz" = 0b1001,
    @"jlr" = 0b1100,
    @"adr" = 0b1101,
    @"rmsr" = 0b1110,
    @"wmsr" = 0b1111,
};

pub const Instruction = union(Encoding) {
    r: packed struct {
        code: RTypeCode,
        size: OperandSize,
        dest: registers.Register,
        lhs: registers.Register,
        rhs: registers.Register,
        imm: u6,
    },
    m: packed struct {
        code: MTypeCode,
        reg1: registers.Register,
        reg2: registers.Register,
        imm: u16,
    },
    l: packed struct {
        code: LTypeCode,
        reg: registers.Register,
        imm: i21,
    },

    pub fn decode(instr: u32) !Instruction {
        const encoding = truncateIntoEnum(Encoding, instr) catch return error.InvalidEncoding;

        return switch (encoding) {
            .r => {
                const code = truncateIntoEnum(RTypeCode, instr >> 2) catch return error.InvalidOpcode;
                const size = truncateIntoEnum(OperandSize, instr >> 9) catch unreachable;
                const dest = truncateIntoEnum(registers.Register, instr >> 11) catch unreachable;
                const lhs = truncateIntoEnum(registers.Register, instr >> 16) catch unreachable;
                const rhs = truncateIntoEnum(registers.Register, instr >> 21) catch unreachable;
                const imm = @intCast(u6, instr >> 26);

                return Instruction{ .r = .{ .code = code, .size = size, .dest = dest, .lhs = lhs, .rhs = rhs, .imm = imm } };
            },
            .m => {
                const code = truncateIntoEnum(MTypeCode, instr >> 2) catch return error.InvalidOpcode;
                const reg1 = truncateIntoEnum(registers.Register, instr >> 6) catch unreachable;
                const reg2 = truncateIntoEnum(registers.Register, instr >> 11) catch unreachable;
                const imm = @intCast(u16, instr >> 16);

                return Instruction{ .m = .{ .code = code, .reg1 = reg1, .reg2 = reg2, .imm = imm } };
            },
            .l => {
                const code = truncateIntoEnum(LTypeCode, instr >> 2) catch return error.InvalidOpcode;
                const reg = truncateIntoEnum(registers.Register, instr >> 6) catch unreachable;
                const imm = @bitCast(i21, @intCast(u21, instr >> 11));

                return Instruction{ .l = .{ .code = code, .reg = reg, .imm = imm } };
            },
        };
    }

    pub fn encode(self: Instruction) u32 {
        var result: u32 = @enumToInt(self);

        switch (self) {
            .r => |encoded| {
                result |= @as(u32, @enumToInt(encoded.code)) << 2;
                result |= @as(u32, @enumToInt(encoded.size)) << 9;
                result |= @as(u32, @enumToInt(encoded.dest)) << 11;
                result |= @as(u32, @enumToInt(encoded.lhs)) << 16;
                result |= @as(u32, @enumToInt(encoded.rhs)) << 21;
                result |= @as(u32, encoded.imm) << 26;
            },
            .m => |encoded| {
                result |= @as(u32, @enumToInt(encoded.code)) << 2;
                result |= @as(u32, @enumToInt(encoded.reg1)) << 6;
                result |= @as(u32, @enumToInt(encoded.reg2)) << 11;
                result |= @as(u32, encoded.imm) << 16;
            },
            .l => |encoded| {
                result |= @as(u32, @enumToInt(encoded.code)) << 2;
                result |= @as(u32, @enumToInt(encoded.reg)) << 6;
                result |= @as(u32, @bitCast(u21, encoded.imm)) << 11;
            },
        }

        return result;
    }
};

fn truncateIntoEnum(comptime T: type, value: anytype) !T {
    return std.meta.intToEnum(T, @truncate(@typeInfo(T).Enum.tag_type, value)) catch return error.InvalidValue;
}
