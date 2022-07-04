const std = @import("std");

const isa = @import("isa");

fn printInstruction(opcode: u32, writer: anytype) !void {
    try writer.print("{X:0>8}: ", .{opcode});

    const instr = isa.Instruction.decode(opcode) catch {
        try writer.print("???\n", .{});
        return;
    };

    try writer.print("{}\n", .{instr});
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
        try writer.print("{X:0>8}: ", .{i * 4});
        try printInstruction(encoded_instr, writer);
    }
}
