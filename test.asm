@assert SubRoutine.length == 3

SubRoutine:
    nop
    nop
    nop
    rts
    @def .length, @here - SubRoutine
