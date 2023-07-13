extern printf

fmt db "%d-", 0x0

_start:
    mov rax, 10       ; numero decimal a convertir
    mov rdx, 2           ; base
    mov rcx, 1
    mov rbx, 0
    mov rsi, 1   ; nuestro contador para movernos por la memoria
    mov rdi, 0   ; nuestro contador de cuantas veces hemos sumado la base
    jmp p1

p1:
    cmp rdx, rax
    ja base_mayor_que_el_numero

    inc rcx
    add rbx, rdx
    cmp rbx, rax
    ja obtener_cociente_y_residuo

    inc rdi

    push rax
    mov rax, 0x0
    lea rdi, [rip + fmt]
    mov rsi, rcx
    call printf
    pop rax

    dec rcx
    jnz _start
    jmp exit


base_mayor_que_el_numero:
    mov [rsp], rax
    mov rax, 0x20
    mov rcx, rsi
    jmp imprimir

obtener_cociente_y_residuo:
    sub rbx, rdx
    sub rax, rbx
    push rax
    inc rsi
    mov rax, rdi
    mov rbx, 0
    mov rdi, 0
    jmp p1

es_mayor_a_diez:
    cmp rdx, 10
    je es_diez
    cmp rdx, 11
    je es_once
    cmp rdx, 12
    je es_doce
    cmp rdx, 13
    je es_trece
    cmp rdx, 14
    je es_catorce
    cmp rdx, 15
    je es_quince

es_diez:
    mov rdx, 'a'
    jmp imprimir

es_once:
    mov rdx, 'b'
    jmp imprimir

es_doce:
    mov rdx, 'c'
    jmp imprimir

es_trece:
    mov rdx, 'd'
    jmp imprimir

es_catorce:
    mov rdx, 'e'
    jmp imprimir

es_quince:
    mov rdx, 'f'
    jmp imprimir


imprimir:
    cmp rsi, 0
    jmp exit
    pop rdx
    dec rsi
    cmp rdx, 10
    jae es_mayor_a_diez
    add rdx, 48

    ;; printf
    push rax
    mov rax, 0x0
    lea rdi, [rip + fmt]
    mov rsi, rdx
    call printf
    pop rax

    dec rcx
    jnz imprimir

exit:
    mov rax, 0x3c
    mov rdi, 0
    syscall

