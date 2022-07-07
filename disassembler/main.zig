const std = @import("std");

const isa = @import("isa");

fn prettyPrintInstruction(instr: isa.Instruction, addr: u64, writer: anytype) !void {
    // zig fmt: off
    switch(instr) {
        .l => |i| {
            const target_addr = (addr + 4) +% @bitCast(u64, @as(i64, i.imm) * 4);
            const imm_unsigned = @bitCast(u21, i.imm);

            if(i.code == .@"jlr") {
                if(i.reg == .ra)
                    return writer.print("call 0x{X}", .{target_addr});
                if(i.reg == .zero)
                    return writer.print("jmp 0x{X}", .{target_addr});
            }

            switch(i.code) {
                .@"adr", .@"jz", .@"jnz", .@"jlr",
                => return writer.print("{s} {s}, 0x{X}", .{@tagName(i.code), @tagName(i.reg), target_addr}),
                .@"ld.d", .@"ld.q",
                .@"st.d", .@"st.q",
                .@"and.d", .@"or.d", .@"xor.d",
                => return writer.print("{s} {s}, (0x{X})", .{@tagName(i.code), @tagName(i.reg), imm_unsigned}),
                .@"wmsr", .@"rmsr" => {
                    const msr = std.meta.intToEnum(isa.Msr, imm_unsigned) catch .invalid_msr;
                    return writer.print("{s} {s}, {s}", .{@tagName(i.code), @tagName(i.reg), @tagName(msr)});
                },
            }
        },
        .m => |i| {
            const target_branch = (addr + 4) +% @bitCast(u64, @as(i64, i.imm) * 4);
            const target_memory = (addr + 4) +% @bitCast(u64, @as(i64, i.imm));
            switch(i.code) {
                .@"add", .@"sub", => if(i.reg2 == .zero) {
                    const value: i64 = if(i.code == .@"sub")
                        -@as(i64, @bitCast(i16, i.imm)) else @intCast(i64, i.imm);
                    return writer.print("li {s}, {d}", .{@tagName(i.reg1), value});
                },
                .@"ld.b", .@"ld.w", .@"ld.d", .@"ld.q",
                .@"st.b", .@"st.w", .@"st.d", .@"st.q",
                => if(i.reg2 == .pc)
                    return writer.print("{s} {s}, (0x{X})", .{@tagName(i.code), @tagName(i.reg1), target_memory}),
                else => {},
            }
            switch(i.code) {
                .@"add", .@"sub",
                => return writer.print("{s} {s}, {s}, 0x{X}", .{@tagName(i.code), @tagName(i.reg1), @tagName(i.reg2), i.imm}),
                .@"jeq", .@"jne", .@"jlt", .@"jle", .@"jge", .@"jgt",
                => return writer.print("{s} {s}, {s}, 0x{X}", .{@tagName(i.code), @tagName(i.reg1), @tagName(i.reg2), target_branch}),
                //else => try writer.print("{}\n", .{i}),
                .@"ld.b", .@"ld.w", .@"ld.d", .@"ld.q",
                .@"st.b", .@"st.w", .@"st.d", .@"st.q", => {
                    const sign_ext = @as(i64, @bitCast(i16, i.imm));
                    return writer.print("{s} {s}, {d}({s})", .{@tagName(i.code), @tagName(i.reg1), sign_ext, @tagName(i.reg2)});
                },
            }
        },
        .r => |i| switch(i.code) {
            .@"jlr" => {
                if(i.size != .qword) return error.@"noncanonical r-type jlr size";
                if(i.imm != 0)       return error.@"noncanonical r-type jlr imm";
                if(i.rhs != .zero)   return error.@"noncanonical r-type jlr rhs";
                if(i.dest == .zero) {
                    if(i.lhs == .ra) {
                        return writer.print("ret", .{});
                    }
                    return writer.print("jmp {s}", .{@tagName(i.lhs)});
                }
                if(i.dest == .ra) {
                    return writer.print("call {s}", .{@tagName(i.lhs)});
                }
                return writer.print("jlr {s}, {s}", .{@tagName(i.dest), @tagName(i.lhs)});
            },
            .@"add", .@"sub", .@"mul", .@"div", .@"mod",
            .@"and", .@"or",  .@"xor",
            .@"sex", .@"shl", .@"asr", .@"lsr",
            => {
                if(i.imm != 0) return error.@"noncanonical r-type arithmetic imm";
                const op_size: []const u8 = switch(i.size) {
                    .byte => ".b",
                    .word => ".w",
                    .dword => ".d",
                    .qword => "",
                };
                return writer.print("{s}{s} {s}, {s}, {s}", .{@tagName(i.code), op_size, @tagName(i.dest), @tagName(i.lhs), @tagName(i.rhs)});
            },
            .@"mcp", .@"mst", => {
                if(i.imm != 0) return error.@"noncanonical r-type memory imm";
                const op_size: []const u8 = switch(i.size) {
                    .byte => "",
                    .word => ".w",
                    .dword => ".d",
                    .qword => ".b",
                };
                return writer.print("{s}{s} {s}, {s}, {s}", .{@tagName(i.code), op_size, @tagName(i.dest), @tagName(i.lhs), @tagName(i.rhs)});
            },
            .@"stp", .@"ldp", => {
                if(i.imm != 0) return error.@"noncanonical r-type pair imm";
                const op_size: []const u8 = switch(i.size) {
                    .byte => ".b",
                    .word => ".w",
                    .dword => ".d",
                    .qword => "",
                };
                return writer.print("{s}{s} {s}, {s}, ({s})", .{@tagName(i.code), op_size, @tagName(i.dest), @tagName(i.lhs), @tagName(i.rhs)});
            },
            .@"udi", .@"hlt", => {
                if(i.size != .qword) return error.@"noncanonical r-type special size";
                if(i.dest != .zero)  return error.@"noncanonical r-type special dest";
                if(i.lhs != .zero)   return error.@"noncanonical r-type special lhs";
                if(i.rhs != .zero)   return error.@"noncanonical r-type special rhs";
                if(i.imm != 0) return error.@"noncanonical r-type special imm";
                return writer.print("{s}", .{@tagName(i.code)});
            }
        },
    }
    // zig fmt: on
}

fn printInstruction(opcode: u32, addr: u64, writer: anytype) !void {
    try writer.print("{X:0>8} {X:0>8}: ", .{ addr, opcode });

    const instr = isa.Instruction.decode(opcode) catch {
        try writer.print("???\n", .{});
        return;
    };

    prettyPrintInstruction(instr, addr, writer) catch |err| {
        writer.print("{s}: {}", .{ @errorName(err), instr }) catch return;
    };

    try writer.print("\n", .{});
}

pub fn main() !void {
    var args = try std.process.argsWithAllocator(std.heap.page_allocator);
    _ = args.next();

    const binary_path = args.next() orelse @panic("Expected binary path as first argument");
    const binary_file = try std.fs.cwd().openFile(binary_path, .{});
    const bytes = try binary_file.readToEndAlloc(std.heap.page_allocator, std.math.maxInt(usize));
    const encoded = std.mem.bytesAsSlice(u32, bytes);
    const writer = std.io.getStdOut().writer();

    for (encoded) |encoded_instr, i| {
        try printInstruction(encoded_instr, 0x10000000 + i * 4, writer);
    }
}
