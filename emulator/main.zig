const std = @import("std");

const isa = @import("isa");

pub const log_level = std.log.Level.debug;

const MemoryBus = struct {
    dram_base: u64,
    dram: std.ArrayListAligned(u8, 0x1000),

    fn init(allocator: std.mem.Allocator, dram_base: u64) !MemoryBus {
        var dram = std.ArrayListAligned(u8, 0x1000).init(allocator);
        try dram.resize(1024 * 1024 * 32);

        std.mem.set(u8, dram.items, 0);

        return MemoryBus{
            .dram_base = dram_base,
            .dram = dram,
        };
    }

    fn load(self: *MemoryBus, comptime T: type, addr: u64) T {
        if (addr >= self.dram_base and addr <= self.dram_base + self.dram.items.len - @sizeOf(T)) {
            return std.mem.readIntLittle(T, self.dram.items[addr - self.dram_base ..][0..@sizeOf(T)]);
        }

        @panic("eeeweajkdasd");
    }

    fn store(self: *MemoryBus, comptime T: type, addr: u64, value: T) void {
        if (addr >= self.dram_base and addr <= self.dram_base + self.dram.items.len - @sizeOf(T)) {
            return std.mem.writeIntLittle(T, self.dram.items[addr - self.dram_base ..][0..@sizeOf(T)], value);
        } else if (T == u32 and addr == 0x10000) {
            return std.io.getStdOut().writer().writeByte(@truncate(u8, value)) catch unreachable;
        }

        std.debug.panic("eeeweajkdasd {s} {X} {X}", .{ @typeName(T), addr, value });
    }
};

const Cpu = struct {
    regs: [31]u64 = std.mem.zeroes([31]u64),
    msrs: std.EnumArray(isa.Msr, u64) = std.EnumArray(isa.Msr, u64).initFill(0),

    fn trap(self: *Cpu, cause: u64, addr: u64) void {
        self.msrs.set(.trap_cause, cause);
        self.msrs.set(.trap_pc, self.load(.pc) -% 4);
        self.msrs.set(.trap_addr, addr);

        self.store(.pc, self.msrs.get(.trap_vec));
    }

    fn load(self: *const Cpu, reg: isa.Register) u64 {
        return switch (reg) {
            .zero => 0,
            else => self.regs[@enumToInt(reg)],
        };
    }

    fn store(self: *Cpu, reg: isa.Register, value: u64) void {
        return switch (reg) {
            else => self.regs[@enumToInt(reg)] = value,
            .zero => {},
        };
    }

    fn loadMemory(self: *Cpu, comptime T: type, bus: *MemoryBus, addr: u64) T {
        _ = self;
        return bus.load(T, addr);
    }

    fn storeMemory(self: *Cpu, comptime T: type, bus: *MemoryBus, addr: u64, value: T) void {
        _ = self;
        bus.store(T, addr, value);
    }

    // TODO: Replace MemoryBus.load/store calls with loadMemory/storeMemory
    fn execute(self: *Cpu, bus: *MemoryBus) void {
        const opcode = self.loadMemory(u32, bus, self.load(.pc));
        const instr = isa.Instruction.decode(opcode) catch return self.trap(0x0, 0x0);

        // std.log.debug("{b:0>32} - {}", .{ opcode, instr });

        self.store(.pc, self.load(.pc) +% 4);

        switch (instr) {
            .r => |encoded| switch (encoded.code) {
                .@"jlr" => {
                    self.store(encoded.dest, self.load(.pc));
                    self.store(.pc, self.load(encoded.lhs));
                },
                .@"udi" => return self.trap(0x0, 0x0),
                else => std.debug.panic("Unhandled R-type opcode: {s}", .{@tagName(encoded.code)}),
            },
            .m => |encoded| switch (encoded.code) {
                .@"add" => self.store(encoded.reg1, self.load(encoded.reg2) +% @as(u64, encoded.imm)),
                .@"sub" => self.store(encoded.reg1, self.load(encoded.reg2) -% @as(u64, encoded.imm)),
                .@"ld.b", .@"ld.w", .@"ld.d", .@"ld.q", .@"st.b", .@"st.w", .@"st.d", .@"st.q" => {
                    const address = self.load(encoded.reg2) + encoded.imm;

                    switch (encoded.code) {
                        .@"ld.b" => self.store(encoded.reg1, bus.load(u8, address)),
                        .@"ld.w" => self.store(encoded.reg1, bus.load(u16, address)),
                        .@"ld.d" => self.store(encoded.reg1, bus.load(u32, address)),
                        .@"ld.q" => self.store(encoded.reg1, bus.load(u64, address)),
                        .@"st.b" => bus.store(u8, address, @truncate(u8, self.load(encoded.reg1))),
                        .@"st.w" => bus.store(u16, address, @truncate(u16, self.load(encoded.reg1))),
                        .@"st.d" => bus.store(u32, address, @truncate(u32, self.load(encoded.reg1))),
                        .@"st.q" => bus.store(u64, address, self.load(encoded.reg1)),
                        else => unreachable,
                    }
                },
                else => std.debug.panic("Unhandled M-type opcode: {s}", .{@tagName(encoded.code)}),
            },
            .l => |encoded| switch (encoded.code) {
                .@"ld.d", .@"ld.q", .@"st.d", .@"st.q" => {
                    const address = @as(u64, @bitCast(u21, encoded.imm));

                    switch (encoded.code) {
                        .@"ld.d" => self.store(encoded.reg, bus.load(u32, address)),
                        .@"ld.q" => self.store(encoded.reg, bus.load(u64, address)),
                        .@"st.d" => bus.store(u32, address, @truncate(u32, self.load(encoded.reg))),
                        .@"st.q" => bus.store(u64, address, self.load(encoded.reg)),
                        else => unreachable,
                    }
                },
                .@"jz", .@"jnz", .@"jlr" => {
                    const target = self.load(.pc) +% @bitCast(u64, @as(i64, encoded.imm) * 4);

                    if (encoded.code == .@"jlr") {
                        self.store(encoded.reg, self.load(.pc));
                        self.store(.pc, target);
                    } else {
                        const value = self.load(encoded.reg);

                        if ((value == 0) == (encoded.code == .@"jz")) {
                            self.store(.pc, target);
                        }
                    }
                },
                .@"rmsr", .@"wmsr" => {
                    const msr_u21 = @bitCast(u21, encoded.imm);
                    const msr = truncateIntoEnum(isa.Msr, msr_u21) catch std.debug.panic("Invalid MSR: 0x{X:0>5}", .{msr_u21});

                    switch (encoded.code) {
                        .@"rmsr" => self.store(encoded.reg, self.msrs.get(msr)),
                        .@"wmsr" => self.msrs.set(msr, self.load(encoded.reg)),
                        else => unreachable,
                    }
                },
                else => std.debug.panic("Unhandled L-type opcode: {s}", .{@tagName(encoded.code)}),
            },
        }
    }
};

const Machine = struct {
    cpu: Cpu,
    memory: MemoryBus,

    fn step(self: *Machine) void {
        self.cpu.execute(&self.memory);
    }
};

fn truncateIntoEnum(comptime T: type, value: anytype) !T {
    return std.meta.intToEnum(T, @truncate(@typeInfo(T).Enum.tag_type, value)) catch return error.InvalidValue;
}

pub fn main() !void {
    var machine: Machine = .{
        .memory = try MemoryBus.init(std.heap.page_allocator, 0x10000000),
        .cpu = .{},
    };

    machine.cpu.store(.pc, machine.memory.dram_base);

    var args = try std.process.argsWithAllocator(std.heap.page_allocator);
    _ = args.next();

    const binary_path = args.next() orelse @panic("Expected binary path as first argument");
    const binary_file = try std.fs.cwd().openFile(binary_path, .{});

    _ = try binary_file.readAll(machine.memory.dram.items);

    var last_pc = machine.cpu.load(.pc);
    while (true) {
        machine.step();

        const curr_pc = machine.cpu.load(.pc);

        if (last_pc == curr_pc) {
            break;
        }

        last_pc = curr_pc;
    }
}
