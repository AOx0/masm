msg db "Hola Mundo", 0xA, 0x0

extern printf

print:
    mov rax, 0
    mov rdi, msg
    mov rdi, [rsp + rdi * 4]
    call printf
    ret

end:
    mov  rax, 0x3c  ;; sys_exit
    mov  rdi, 10   ;; arg0 = 10
    syscall        ;; call

main:
    call print
    call end        ;; call end