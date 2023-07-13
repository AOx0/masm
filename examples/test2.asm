extern printf
extern scanf

numb dw 0x0
omsg db "The number is: %d", 0xa, 0
imsg db "Enter a number: ", 0
scan db "%d", 0

main:
    mov rax, 0
    lea rdi, [rip + imsg]
    call printf

    mov rax, 0
    lea rdi, [rip + scan]
    lea rsi, [rip + numb]
    call scanf

    mov rax, 0
    lea rdi, [rip + omsg]
    mov rsi, [rip + numb]
    call printf

    jmp exit


exit:
    mov rax, 0x3c
    mov rdi, 0
    syscall