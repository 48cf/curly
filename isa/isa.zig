pub usingnamespace @import("instructions.zig");
pub usingnamespace @import("registers.zig");

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

    /// An invalid MSR fault occurs when an invalid MSR number is supplied to `rmsr`
    /// Invalidness of the MSR may depend on if the `rmsr` is privileged or not
    InvalidMSR = 2,

    /// A readonly MSR fault occurs when an readonly MSR number is supplied to `wmsr`
    /// Readonlyness of the MSR may depend on if the `wmsr` is privileged or not
    ReadonlyMSR = 3,
};
