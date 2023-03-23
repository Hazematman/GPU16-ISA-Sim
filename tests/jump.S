    loadi 123
    add.accum.low r1, r0
    loadi 133
    add.accum.low r2, r0
    load_addr JUMP
    add.accum.low r3, r0
    jump r3

    ; This instruction should be skipped
    add r5, r0, r1
JUMP:
    add r4, r0, r2
