output db 0
base db 2
counter dw 1
sum dw 0
remainder db 0
quotient dw 0

_start:
    mov rax, 10000       ; numero decimal a convertir
    mov rdx, 2           ; base
    mov rcx, 1
    mov rbx, 0
    mov rsi, 1   ; nuestro contador para movernos por la memoria
    mov rdi, 0   ; nuestro contador de cuantas veces hemos sumado la base

p1:
    cmp rdx, rax
    ja base_mayor_que_el_numero

    inc rcx
    add rbx, rdx
    cmp rbx, rax
    ja obtener_cociente_y_residuo

    inc rdi

    loop p1
    call exit


base_mayor_que_el_numero:
    mov [rsp], rax
    mov rax, 0x20
    mov rcx, rsi
    call imprimir

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
    cmp rdl, 10
    je es_diez
    cmp rdl, 11
    je es_once
    cmp rdl, 12
    je es_doce
    cmp rdl, 13
    je es_trece
    cmp rdl, 14
    je es_catorce
    cmp rdl, 15
    je es_quince

es_diez:
    mov rdl, 'a'
    jmp imprimir

es_once:
    mov rdl, 'b'
    jmp imprimir

es_doce:
    mov rdl, 'c'
    jmp imprimir

es_trece:
    mov dl, 'd'
    jmp imprimir

es_catorce:
    mov dl, 'e'
    jmp imprimir

es_quince:
    mov dl, 'f'
    jmp imprimir


imprimir:
    cmp rsi, 0
    call exit
    pop rdl
    dec rsi
    cmp rdl, 10
    jae es_mayor_a_diez
    add rdl, 48

    ;; sys_write
    mov rax, 0x01
    mov rdi, rdl
    mov rsi, 1
    syscall
    loop imprimir


exit:
    mov rax, 0x3c
    mov rdi, 0
    syscall

