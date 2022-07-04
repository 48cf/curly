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
};
