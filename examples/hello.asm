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

.include uart.asm

hello_world:
    .db "Hello, world!\n", 0

trap_message:
    .db "Trapped!\n", 0

jmp start
