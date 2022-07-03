start:
    add r0, zero, 72
    call output_ch

    add r0, zero, 101
    call output_ch

    add r0, zero, 108
    call output_ch
    call output_ch
    
    add r0, zero, 111
    call output_ch

    add r0, zero, 33
    call output_ch

    add r0, zero, 10
    call output_ch

loop:
    jmp loop

output_ch:
    st.d r0, 0x10000
    ret
