start:
    adr r0, trap_handler
    wmsr r0, trap_vec

    adr r0, hello_world
    call putstr

    udi

inf_loop:
    jmp inf_loop

trap_handler:
    adr r0, trap_message
    call putstr
    jmp inf_loop

hello_world:
    .db 72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108, 100, 33, 10, 0

trap_message:
    .db 84, 114, 97, 112, 112, 101, 100, 33, 10, 0

.include uart.asm
