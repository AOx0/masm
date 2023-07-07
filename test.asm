; Termina el programa con número de estado 10

end:
    mov rax, 0x3c ;; sys_exit
    mov rdi, 10   ;; arg0 = 10
    syscall       ;; call
    

main:
    call end