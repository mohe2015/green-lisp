.text
.global _start

_start:
    mov r0, #1              @ stdout
    add r1, pc, #16         @ address of the string
    mov r2, #12             @ string length
    mov r7, #4              @ syscall for 'write'
    swi #0                  @ software interrupt

_exit:
    mov r7, #1              @ syscall for 'exit'
    swi #0                  @ software interrupt

_string:
.asciz "Hello world!!!\n"          @ our string, NULL terminated
