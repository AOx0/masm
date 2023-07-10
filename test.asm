; Termina el programa con número de estado 10

msg db 'H', "ola", 0xA, 0
msg2 db -1

end:
mov rax, rdi
mov edi, eax
mov  rax, [rsi + rdi * 0x1 + 4] ;; sys_exit
mov  rax, [rsi + rdi + 4] ;; sys_exit
mov  rax, [rcx + rdi + 4] ;; sys_exit
mov  rax, [rcx + rdi + 4] ;; sys_exit
mov  rax, [rsi + rdi + 0x0] ;; sys_exit
mov  rax, [rsi + rdi + 0xA] ;; sys_exit
mov  eax, [rsi + rdi + 0x0] ;; sys_exit
mov  eax, [rsi + rdi + 0xA] ;; sys_exit
mov  ax, [rsi + rdi + 0x0] ;; sys_exit
mov  ax, [rsi + rdi + 0xA] ;; sys_exit
mov  rcx, [rsi + rdi + 0x0] ;; sys_exit
mov  rcx, [rsi + rdi + 0xA] ;; sys_exit
mov  ecx, [rsi + rdi + 0x0] ;; sys_exit
mov  ecx, [rsi + rdi + 0xA] ;; sys_exit
mov  cx, [rsi + rdi * 2 + 0x0] ;; sys_exit
mov  cx, [rsi + rdi * 2  + 0xA] ;; sys_exit
mov  rax, [rsi + rdi* 2  + 0x0] ;; sys_exit
mov  rax, [rsi + rdi* 2  + 0xA] ;; sys_exit
mov  eax, [rsi + rdi* 2  + 0x0] ;; sys_exit
mov  eax, [rsi + rdi* 2  + 0xA] ;; sys_exit
mov  ax, [rsi + rdi* 2  + 0x0] ;; sys_exit
mov  ax, [rsi + rdi* 2  + 0xA] ;; sys_exit
mov  rcx, [rsi + rdi * 2 + 0x0] ;; sys_exit
mov  rcx, [rsi + rdi* 2  + 0xA] ;; sys_exit
mov  ecx, [rsi + rdi* 2  + 0x0] ;; sys_exit
mov  ecx, [rsi + rdi* 2  + 0xA] ;; sys_exit
mov  cx, [rsi + rdi + 0x0] ;; sys_exit
mov  cx, [rsi + rdi + 0xA] ;; sys_exit
mov  ax, 0x3   ;; sys_exit
mov  rdi, 10   ;; arg0 = 10
push rax
push rsi
push 4
push -1
pop rdx
syscall        ;; call
ret
    

main:
call end