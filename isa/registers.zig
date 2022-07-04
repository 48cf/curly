pub const Register = enum(u5) {
    r0,
    r1,
    r2,
    r3,
    r4,
    r5,
    r6,
    r7,
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15,
    r16,
    r17,
    r18,
    r19,
    r20,
    pc = 27,
    ra = 28,
    sp = 29,
    fp = 30,
    zero = 31,
};

pub const Msr = enum(u21) {
    // Paging
    paging_config = 0x00000,
    kernel_base,
    kernel_pt_addr,
    user_pt_addr,

    // Traps
    trap_vec = 0x00100,
    trap_info,
    trap_cause,
    trap_pc,
    trap_addr,

    invalid_msr = ~@as(u21, 0),
};

pub const TrapCause = enum(u64) {
    /// An undefined instruction fault occurs when an unknown opcode is found
    ///
    ///   trap_info: Opcode of the unknown instruction
    ///   trap_pc: Address of the unknown instruction
    UndefinedInstruction = 0,

    /// A privilege fault fault occurs when an unprivileged instruction
    ///   attepmts to read, write or execute a privileged instruction.
    ///
    ///   trap_info: Access type:
    ///       0: Read
    ///       1: Write
    ///       2: Execution
    ///   trap_pc: Address of the violating instruction
    ///   trap_addr: Address of the attempted access
    Privilege = 1,

    /// An invalid combination of MSR number, MSR readonlyness and code privilege level occured.
    ///   trap_info: Following bits bitwise-or with the MSRInfo of the MSR accessed
    ///       bit 62:
    ///           0: Attempted MSR read
    ///           1: Attempted MSR write
    ///       bit 63:
    ///           0: Access from unprivileged code
    ///           1: Access from privileged code
    ///   trap_pc: Address of the violating instruction
    ///   trap_addr: MSR number
    InvalidMSR = 2,
};

/// Bitfields of the following
///     bit 0: Readable from unprivileged code
///     bit 1: Writable from unprivileged code
///     bit 2: Readable from privileged code
///     bit 3: Writable from privileged code
/// All bits being 0 implies the MSR is not known
pub const MSRInfo = packed struct {
    unprivileged_readable: bool,
    unprivileged_writable: bool,
    privileged_readable: bool,
    privileged_writable: bool,

    pub fn encode(value: @This()) u64 {
        var result: u64 = 0;
        if(value.unprivileged_readable) result |= (1 << 0);
        if(value.unprivileged_writable) result |= (1 << 1);
        if(value.privileged_readable) result |= (1 << 2);
        if(value.privileged_writable) result |= (1 << 3);
        return result;
    }

    pub fn decode(value: u64) @This() {
        return .{
            .unprivileged_readable = (value & (1 << 0)) != 0,
            .unprivileged_writable = (value & (1 << 1)) != 0,
            .privileged_readable = (value & (1 << 2)) != 0,
            .privileged_writable = (value & (1 << 3)) != 0,
        };
    }
};