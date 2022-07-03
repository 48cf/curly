start:
    add r0, zero, 72
    jlr ra, output_ch

    add r0, zero, 101
    jlr ra, output_ch

    add r0, zero, 108
    jlr ra, output_ch
    jlr ra, output_ch
    
    add r0, zero, 111
    jlr ra, output_ch

loop:
    jlr zero, loop

output_ch:
    st.d r0, 0x10000
    jlr zero, ra
