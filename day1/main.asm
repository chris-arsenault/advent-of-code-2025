; Day 1 solution in x86-64 assembly (SysV ABI)
; Uses C stdio/stdlib for file IO and parsing.

global main
extern clock_gettime
extern printf
extern perror
extern ns_since        ; from shared/utils.asm
extern read_file_all

%define CLOCK_MONOTONIC 1
%define BUF_SIZE 1048576

section .data
input_file:    db "input.txt", 0
fmt_out:       db "zero_landings=%d crossings=%d final_pos=%d elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

section .bss
buf:           resb BUF_SIZE
ts0:           resq 2                ; struct timespec { tv_sec, tv_nsec }
ts1:           resq 2
at_var:        resd 1
zero_var:      resd 1
cross_var:     resd 1

section .text

main:
    push    rbp
    mov     rbp, rsp
    push    r12
    push    r13
    push    r14
    push    r15

    ; read entire file into buffer
    lea     rdi, [rel input_file]
    lea     rsi, [rel buf]
    mov     rdx, BUF_SIZE
    call    read_file_all
    cmp     rax, 0
    jl      .open_fail
    mov     r13, rax                ; bytes read

    mov     dword [at_var], 50
    mov     dword [zero_var], 0
    mov     dword [cross_var], 0

    ; clock_gettime(CLOCK_MONOTONIC, &ts0)
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    cmp     r13, 0
    jle     .after_read
    lea     rsi, [rel buf]          ; ptr
    lea     r15, [rel buf]
    add     r15, r13                ; end pointer

.line_loop:
    cmp     rsi, r15
    je      .after_read

    ; sign
    mov     al, [rsi]
    cmp     al, 10                  ; newline -> skip
    je      .skip_line
    cmp     al, 13                  ; carriage -> skip
    je      .skip_line
    cmp     al, 'R'
    jne     .sign_neg
    mov     r8d, 1
    jmp     .sign_done
.sign_neg:
    mov     r8d, -1
.sign_done:
    inc     rsi

    ; parse magnitude
    xor     eax, eax                ; mag
.mag_loop:
    cmp     rsi, r15
    je      .mag_done
    mov     dl, [rsi]
    inc     rsi
    cmp     dl, 10                  ; '\n'
    je      .mag_done
    cmp     dl, 13                  ; '\r'
    je      .mag_done
    sub     dl, '0'
    movzx   edx, dl
    imul    eax, eax, 10
    add     eax, edx
    jmp     .mag_loop
.mag_done:
    mov     ebx, eax                ; mag in ebx

    ; first = sign==1 ? 100 - at : at
    cmp     r8d, 1
    jne     .first_neg
    mov     eax, 100
    sub     eax, [at_var]
    jmp     .first_done
.first_neg:
    mov     eax, [at_var]
.first_done:
    cmp     eax, 0
    jne     .first_nz
    mov     eax, 100
.first_nz:

    ; hits
    xor     edx, edx
    cmp     ebx, eax
    jl      .hits_skip
    mov     ecx, ebx
    sub     ecx, eax
    xor     edx, edx
    mov     eax, ecx
    mov     ecx, 100
    div     ecx                     ; eax = (mag-first)/100
    inc     eax
    mov     edx, eax
.hits_skip:
    add     [cross_var], edx

    ; sum = at + sign*mag
    mov     eax, [at_var]
    imul    ebx, r8d
    add     eax, ebx
    cdq
    mov     ecx, 100
    idiv    ecx                     ; edx = remainder
    mov     eax, edx
    cmp     eax, 0
    jge     .rem_ok
    add     eax, 100
.rem_ok:
    mov     [at_var], eax
    cmp     eax, 0
    jne     .line_loop
    inc     dword [zero_var]
    jmp     .line_loop

.skip_line:
    inc     rsi
    jmp     .line_loop

.open_fail:
    lea     rdi, [rel err_open]
    call    perror
    mov     eax, 1
    jmp     .exit

.after_read:
    ; clock_gettime(CLOCK_MONOTONIC, &ts1)
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts1]
    call    clock_gettime

    ; elapsed_ms = ns_since(&ts0, &ts1) / 1e6
    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since
    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    ; printf
    lea     rdi, [rel fmt_out]
    mov     esi, [zero_var]
    mov     edx, [cross_var]
    mov     ecx, [at_var]
    call    printf

    xor     eax, eax
.exit:
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbp
    ret

section .note.GNU-stack noalloc nobits progbits align=1
