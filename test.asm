msg db "Hola Mundo", 0xA, 0x0

extern printf

print:
    mov  rax, 0x0
    mov  rdi, msg
    lea  rdi, [rip]
    lea  rdi, [0xA]
    lea  rdi, [rip + 0xA]
    lea  rdi, [rcx + 0xA]
    lea  rdi, [rcx + rdx * 0x1 + 0x0]
    call printf
    ret

end:
    mov  rax, 0x3c  ;; sys_exit
    mov  rdi, 10   ;; arg0 = 10
    syscall        ;; call

main:
    call print
    call end        ;; call end