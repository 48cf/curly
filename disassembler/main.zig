const std = @import("std");

const isa = @import("isa");

pub const log_level = std.log.Level.debug;

pub fn main() !void {
    var args = try std.process.argsWithAllocator(std.heap.page_allocator);
    _ = args.next();

    const binary_path = args.next() orelse @panic("Expected binary path as first argument");
    const binary_file = try std.fs.cwd().openFile(binary_path, .{});
    const bytes = try binary_file.readToEndAlloc(std.heap.page_allocator, std.math.maxInt(usize));
    const encoded = std.mem.bytesAsSlice(u32, bytes);

    for (encoded) |encoded_instr, i| {
        const instr = isa.Instruction.decode(encoded_instr) catch {
            std.log.info("{X:0>8}: ???", .{i * 4});
            continue;
        };

        std.log.info("{X:0>8}: {}", .{ i * 4, instr });
    }
}
