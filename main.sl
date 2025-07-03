fun test(n){
    println(n);
    if(n == -1){
        println("match");
    } else {
        println("nope");
    }
}

test(-1);
println(-1);

section .bss
    buffer resb 32
section .data
    newline db 10
temp0_msg db 'match'
temp0_len equ $ - temp0_msg
temp1_msg db 'nope'
temp1_len equ $ - temp1_msg
section .text
global test
test:
    push rbp
    mov rbp, rsp
    sub rsp, 8





    mov [rbp-8], rdi
_start:
    push rbp
    mov rbp, rsp
    sub rsp, 1024
    mov rdi, [rbp-8]
    call print_number
    ; newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline
    mov rdx, 1
    syscall
    mov rax, [rbp-8]
    push rax
    mov rax, -1
    mov rbx, rax
    pop rax
    cmp rax, rbx
    sete al
    movzx rax, al
    cmp rax, 0
    je else_if_0_0
    mov rsi, temp0_msg
    mov rdx, temp0_len
    call print_string
    ; newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline
    mov rdx, 1
    syscall
    jmp endif_0
else_if_0_0:
    mov rsi, temp1_msg
    mov rdx, temp1_len
    call print_string
    ; newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline
    mov rdx, 1
    syscall
endif_0:
test_end:
    mov rax, -1
    leave
    ret
global _start
    mov rax, -1
    mov rdi, rax
    call test
    mov rdi, -1
    call print_number
    ; newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline
    mov rdx, 1
    syscall
    mov rax, 60
    xor rdi,rdi
    syscall
; ----------------------
; void print_number(int64_t value)
; number is passed in rdi
print_number:
    mov rax, rdi
    mov rbx, 10
    mov rsi, buffer + 32
    mov rcx, 0
    mov rdx, 0
    cmp rdi, 0
    jl .negative
.convert:
    xor rdx, rdx
    div rbx
    add dl, '0'
    dec rsi
    mov [rsi], dl
    inc rcx
    test rax, rax
    jnz .convert
    jmp .print
.negative:
    neg rax
    xor rdx, rdx
    .negconvert:
    div rbx
    add dl, '0'
    dec rsi
    mov [rsi], dl
    inc rcx
    test rax, rax
    jnz .negconvert
    dec rsi
    mov byte [rsi], '-'
    inc rcx
.print:
    mov rax, 1
    mov rdi, 1
    mov rdx, rcx
    syscall
    ret
; ----------------------
; atoi
; convert buffer (rsi) to integer in rax
atoi:
    xor rax, rax
    xor rbx, rbx
.loop:
    mov bl, byte [rsi]
    cmp bl, 10
    je .done
    cmp bl, '0'
    jl .done
    cmp bl, '9'
    jg .done
    sub bl, '0'
    imul rax, rax, 10
    add rax, rbx
    inc rsi
    jmp .loop
.done:
    ret
; ----------------------
; void print_string(char* str, uint64_t len)
; rsi = pointer, rdx = length
print_string:
    push rsi
    push rdx
    mov rax, 1
    mov rdi, 1
    syscall
    pop rdx
    pop rsi
    ret
