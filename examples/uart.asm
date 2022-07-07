putstr:
    ld.b r1, (r0)
    jz r1, .ret
    st.d r1, 0x10000
    add r0, r0, 1
    jmp putstr
.ret:
    ret
