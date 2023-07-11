end:
    mov  rax, 0x3c  ;; sys_exit
    mov  rdi, 10   ;; arg0 = 10
    syscall        ;; call
    ret

main:
    call end        ;; call end