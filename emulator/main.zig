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

const AccessType = enum {
    Read,
    Write,
    Execute,
};

const Cpu = struct {
    regs: [31]u64 = std.mem.zeroes([31]u64),
    msrs: std.EnumArray(isa.Msr, u64) = std.EnumArray(isa.Msr, u64).initFill(0),
    last_pc: u64 = 0,

    fn msrInfo(msr: isa.Msr) isa.MSRInfo {
        const privileged_rw = isa.MSRInfo {
            .privileged_readable = true,
            .privileged_writable = true,
            .unprivileged_readable = false,
            .unprivileged_writable = false,
        };

        return switch(msr) {
            .paging_config,
            .kernel_base,
            .kernel_pt_addr,
            .user_pt_addr,
            .trap_vec,
            .trap_info,
            .trap_cause,
            .trap_pc,
            .trap_addr,
            => privileged_rw,

            else => .{
                .privileged_readable = false,
                .privileged_writable = false,
                .unprivileged_readable = false,
                .unprivileged_writable = false,
            },
        };
    }

    fn privilegedAddr(self: Cpu, addr: u64) bool {
        return addr >= self.msrs.get(.kernel_base);
    }

    fn privileged(self: Cpu) bool {
        return self.privilegedAddr(self.last_pc);
    }

    fn trap(self: *Cpu, cause: isa.TrapCause, info: u64, addr: u64) void {
        self.msrs.set(.trap_cause, @enumToInt(cause));
        self.msrs.set(.trap_info, info);
        self.msrs.set(.trap_pc, self.last_pc);
        self.msrs.set(.trap_addr, addr);

        self.storeDirect(.pc, self.msrs.get(.trap_vec));
    }

    fn load(self: *const Cpu, reg: isa.Register) u64 {
        return switch (reg) {
            .zero => 0,
            else => self.regs[@enumToInt(reg)],
        };
    }

    fn storeDirect(self: *Cpu, reg: isa.Register, value: u64) void {
        return switch (reg) {
            else => self.regs[@enumToInt(reg)] = value,
            .zero => {},
        };
    }

    fn storeChecked(self: *Cpu, reg: isa.Register, value: u64) ?void {
        if(reg == .pc) {
            return self.jump(value);
        }
        return self.storeDirect(reg, value);
    }

    fn loadPhysical(self: *Cpu, comptime T: type, bus: *MemoryBus, addr: u64) ?T {
        _ = self;
        return bus.load(T, addr);
    }

    fn loadLinear(self: *Cpu, comptime T: type, bus: *MemoryBus, addr: u64) ?T {
        if(self.privilegedAddr(addr) and !self.privileged()) {
            self.trap(.Privilege, 0, addr);
            return null;
        }
        // TODO: Do something about loads across page boundaries
        return self.loadPhysical(T, bus, self.resolvePhys(addr, bus, .Read) orelse return null);
    }

    fn loadInstrLinear(self: *Cpu, comptime T: type, bus: *MemoryBus, addr: u64) ?T {
        if(self.privilegedAddr(addr) and !self.privileged()) {
            self.trap(.Privilege, 0, addr);
            return null;
        }
        // TODO: Do something about loads across page boundaries
        return self.loadPhysical(T, bus, self.resolvePhys(addr, bus, .Execute) orelse return null);
    }

    fn storePhysical(self: *Cpu, comptime T: type, bus: *MemoryBus, addr: u64, value: T) ?void {
        _ = self;
        bus.store(T, addr, value);
        return {};
    }

    fn storeLinear(self: *Cpu, comptime T: type, bus: *MemoryBus, addr: u64, value: T) ?void {
        if(self.privilegedAddr(addr) and !self.privileged()) {
            self.trap(.Privilege, 1, addr);
            return null;
        }
        // TODO: Do something about stores across page boundaries
        return self.storePhysical(T, bus, self.resolvePhys(addr, bus, .Write) orelse return null, value);
    }

    fn resolvePhys(self: *Cpu, vaddr: u64, bus: *MemoryBus, access_type: AccessType) ?u64 {
        _ = bus;
        _ = access_type;
        _ = self;
        // TODO: Implement paging
        return vaddr;
    }

    fn jump(self: *Cpu, addr: u64) ?void {
        if(self.privilegedAddr(addr) and !self.privileged()) {
            self.trap(.Privilege, 2, addr);
            return null;
        }
        self.storeDirect(.pc, addr);
        return {};
    }

    fn execute(self: *Cpu, bus: *MemoryBus) void {
        const opcode = self.loadInstrLinear(u32, bus, self.load(.pc)) orelse return;
        const instr = isa.Instruction.decode(opcode) catch return self.trap(.UndefinedInstruction, opcode, 0);

        // std.log.debug("{b:0>32} - {}", .{ opcode, instr });

        self.jump(self.load(.pc) +% 4) orelse return;

        switch (instr) {
            .r => |encoded| switch (encoded.code) {
                .@"jlr" => {
                    const old_pc = self.load(.pc);
                    self.jump(self.load(encoded.lhs)) orelse return;
                    _ = self.storeChecked(encoded.dest, old_pc);
                },
                .@"add", .@"sub", .@"mul", .@"div", .@"mod" => {
                    const mask: u64 = switch (encoded.size) {
                        .byte => 0xFF,
                        .word => 0xFFFF,
                        .dword => 0xFFFFFFFF,
                        .qword => 0xFFFFFFFFFFFFFFFF,
                    };

                    const lhs = self.load(encoded.lhs);
                    const rhs = self.load(encoded.rhs);
                    const value = switch (encoded.code) {
                        .@"add" => lhs + rhs,
                        .@"sub" => lhs - rhs,
                        .@"mul" => lhs * rhs,
                        .@"div" => if (rhs != 0) lhs / rhs else return self.trap(.DivisionByZero, 0, 0),
                        .@"mod" => if (rhs != 0) lhs % rhs else return self.trap(.DivisionByZero, 0, 0),
                        else => unreachable,
                    } & mask;

                    _ = self.storeChecked(encoded.dest, value);
                },
                .@"udi" => return self.trap(.UndefinedInstruction, opcode, 0),
                else => std.debug.panic("Unhandled R-type opcode: {s}", .{@tagName(encoded.code)}),
            },
            .m => |encoded| switch (encoded.code) {
                .@"add" => _ = self.storeChecked(encoded.reg1, self.load(encoded.reg2) +% @as(u64, encoded.imm)),
                .@"sub" => _ = self.storeChecked(encoded.reg1, self.load(encoded.reg2) -% @as(u64, encoded.imm)),
                .@"ld.b", .@"ld.w", .@"ld.d", .@"ld.q", .@"st.b", .@"st.w", .@"st.d", .@"st.q" => {
                    const address = self.load(encoded.reg2) +% @bitCast(u64, @as(i64, @bitCast(i16, encoded.imm)));

                    switch (encoded.code) {
                        .@"ld.b" => _ = self.storeChecked(encoded.reg1, self.loadLinear(u8, bus, address) orelse return),
                        .@"ld.w" => _ = self.storeChecked(encoded.reg1, self.loadLinear(u16, bus, address) orelse return),
                        .@"ld.d" => _ = self.storeChecked(encoded.reg1, self.loadLinear(u32, bus, address) orelse return),
                        .@"ld.q" => _ = self.storeChecked(encoded.reg1, self.loadLinear(u64, bus, address) orelse return),
                        .@"st.b" => _ = self.storeLinear(u8, bus, address, @truncate(u8, self.load(encoded.reg1))),
                        .@"st.w" => _ = self.storeLinear(u16, bus, address, @truncate(u16, self.load(encoded.reg1))),
                        .@"st.d" => _ = self.storeLinear(u32, bus, address, @truncate(u32, self.load(encoded.reg1))),
                        .@"st.q" => _ = self.storeLinear(u64, bus, address, self.load(encoded.reg1)),
                        else => unreachable,
                    }
                },
                else => std.debug.panic("Unhandled M-type opcode: {s}", .{@tagName(encoded.code)}),
            },
            .l => |encoded| switch (encoded.code) {
                .@"ld.d", .@"ld.q", .@"st.d", .@"st.q" => {
                    const address = @as(u64, @bitCast(u21, encoded.imm));

                    switch (encoded.code) {
                        .@"ld.d" => _ = self.storeChecked(encoded.reg, self.loadLinear(u32, bus, address) orelse return),
                        .@"ld.q" => _ = self.storeChecked(encoded.reg, self.loadLinear(u64, bus, address) orelse return),
                        .@"st.d" => _ = self.storeLinear(u32, bus, address, @truncate(u32, self.load(encoded.reg))),
                        .@"st.q" => _ = self.storeLinear(u64, bus, address, self.load(encoded.reg)),
                        else => unreachable,
                    }
                },
                .@"jz", .@"jnz", .@"jlr" => {
                    const target = self.load(.pc) +% @bitCast(u64, @as(i64, encoded.imm) * 4);

                    if (encoded.code == .@"jlr") {
                        self.storeChecked(encoded.reg, self.load(.pc)) orelse return;
                        _ = self.jump(target);
                    } else {
                        const value = self.load(encoded.reg);

                        if ((value == 0) == (encoded.code == .@"jz")) {
                            _ = self.jump(target);
                        }
                    }
                },
                .@"adr" => {
                    const address = self.load(.pc) +% @bitCast(u64, @as(i64, @bitCast(i21, encoded.imm)) * 4);
                    _ = self.storeChecked(encoded.reg, address);
                },
                .@"rmsr", .@"wmsr" => {
                    const msr_u21 = @bitCast(u21, encoded.imm);
                    const msr = truncateIntoEnum(isa.Msr, msr_u21) catch std.debug.panic("Invalid MSR: 0x{X:0>5}", .{msr_u21});
                    const info = msrInfo(msr);
                    const fault_info = info.encode() |
                        (@as(u64, @boolToInt(encoded.code == .@"wmsr")) << 62) | (@as(u64, @boolToInt(self.privileged())) << 63);

                    switch (encoded.code) {
                        .@"rmsr" => {
                            if(self.privileged()) {
                                if(!info.privileged_readable) return self.trap(.InvalidMSR, fault_info, msr_u21);
                            } else {
                                if(!info.unprivileged_readable) return self.trap(.InvalidMSR, fault_info, msr_u21);
                            }
                            _ = self.storeChecked(encoded.reg, self.msrs.get(msr));
                        },
                        .@"wmsr" => {
                            if(self.privileged()) {
                                if(!info.privileged_writable) return self.trap(.InvalidMSR, fault_info, msr_u21);
                            } else {
                                if(!info.unprivileged_writable) return self.trap(.InvalidMSR, fault_info, msr_u21);
                            }
                            self.msrs.set(msr, self.load(encoded.reg));
                        },
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

    var args = try std.process.argsWithAllocator(std.heap.page_allocator);
    _ = args.next();

    const binary_path = args.next() orelse @panic("Expected binary path as first argument");
    const binary_file = try std.fs.cwd().openFile(binary_path, .{});
    const bytes_read = try binary_file.readAll(machine.memory.dram.items);

    std.mem.copy(u8, machine.memory.dram.items[0x8000..], "Hello, world!\n\x00");

    machine.cpu.storeDirect(.pc, machine.memory.dram_base + bytes_read - 4);
    machine.cpu.storeDirect(.sp, machine.memory.dram_base + machine.memory.dram.items.len);

    var last_pc = machine.cpu.load(.pc);

    while (true) {
        // const old_regs = machine.cpu.regs;

        machine.step();

        // for (machine.cpu.regs) |value, reg_i| {
        //     const reg = @intToEnum(isa.Register, reg_i);

        //     if ((reg == .pc and value - 4 != old_regs[reg_i]) or (reg != .pc and value != old_regs[reg_i])) {
        //         std.io.getStdOut().writer().print("   {s:>8} {X:0>16} -> {X:0>16}\n", .{ @tagName(reg), old_regs[reg_i], value }) catch {};
        //     }
        // }

        const pc = machine.cpu.load(.pc);

        if (pc == last_pc) {
            break;
        }

        last_pc = pc;
    }
}
