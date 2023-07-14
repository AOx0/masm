global exit

exit:
    mov rax, 0x3c
    mov rdi, 0
    syscall