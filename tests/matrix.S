; This function implements triangle transformation
; The transformation matrix for the triangle is located at mem[transform_matrix]
; The GPU16 archiecture has 4 simulatenous threads of execution from
; the same instruction stream, in the case of triangle transformation
; each thread is responsbile for transforming one whole triangle. And if the
; triangle is visible it will sent to rasterization engine.

; Matrix looks like the following (i.e 3x4 matrix)
; [ a b c d] [x]
; [ e f g h] [y]
; [ i j k l] [z]
;            [1]
; To calculate the vertex transformation, we need to calculate:
; x' = ax + by + cz + d
; y' = ex + fy + gz + h
; z' = ix + jy + kz + l
;
; Then we need to calculate the perspective divide
; x'' = x' / z'
; y'' = y' / z'

; r1 = vertex_addr
; r2 = matrix_addr
vertex_func:
    stride 6
    load r3, r1, 0 ; Load x high
    load r4, r1, 1 ; Load x low

    stride 0
    load r5, r2, 0 ; Load a high
    load r6, r2, 1 ; load a low

    ; Calculate ax
    mult.set.u16_u16.down r4, r6
    mult.accum.s16_u16.no r3, r6
    mult.accum.u16_u16.no r4, r5
    mult.accum.u16_u16.up r3, r5

    ; Store accumulator since we don't have enough registers
    ; to keep it in registers
    add.accum.high r5, r0
    add.accum.low r6, r0
    stride 6
    store r5, r1, 0
    store r6, r1, 1

    stride 0
    load r5, r2, 2
    load r6, r2, 3

    ; Calculate bx
    mult.set.u16_u16.down r4, r6
    mult.accum.s16_u16.no r3, r6
    mult.accum.u16_u16.no r4, r5
    mult.accum.u16_u16.up r3, r5

    ; Calculate ax + bx
    load r5, r1, 0
    load r6, r1, 1
    add.accum.low r6, r6
    add.accum.high r5, r5

    ; Store ax + bx
    stride 6
    store r5, r1, 0
    store r6, r1, 1

    stride 0
    load r5, r2, 4
    load r6, r2, 5

    ; Calculate cx
    mult.set.u16_u16.down r4, r6
    mult.accum.s16_u16.no r3, r6
    mult.accum.u16_u16.no r4, r5
    mult.accum.u16_u16.up r3, r5

    ; Calculate ax + bx + cx
    load r5, r1, 0
    load r6, r1, 1
    add.accum.low r6, r6
    add.accum.high r5, r5

    ; Load d, overwritting x in r3, r4
    stride 0
    load r3, r2, 6
    load r4, r2, 7

    ; Calculate ax + bx + cx + d
    add r6, r6, r4
    add r5, r5, r3

    ; Store new x value over old x value
    stride 6
    store r5, r1, 0
    store r6, r1, 1

done:
