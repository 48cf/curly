start:
    ldi r0, 72
    call output_ch

    ldi r0, 101
    call output_ch

    ldi r0, 108
    call output_ch
    call output_ch
    
    ldi r0, 111
    call output_ch

    ldi r0, 33
    call output_ch

    ldi r0, 10
    ld.q r1, -0x8(output_stub)
    call r1

loop:
    jmp loop

    .dq output_ch
output_stub:

.include uart.asm
