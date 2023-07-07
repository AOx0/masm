;; un test de chill

end:
    mov rax, 0x3c ;; aqui tambien
    mov rdi, 0x0   
    syscall  

main:
    call <end>

