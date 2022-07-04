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
    call output_ch

loop:
    jmp loop

output_ch:
    st.d r0, 0x10000
    ret
