; Day 1 solution in x86-64 assembly (SysV ABI)
; Uses branchless techniques where beneficial.
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

    ; Check for whitespace (newline/carriage return)
    mov     al, [rsi]
    cmp     al, 10                  ; newline -> skip
    je      .skip_line
    cmp     al, 13                  ; carriage -> skip
    je      .skip_line

    ; Branchless sign extraction: sign = (char == 'R') ? 1 : -1
    ; Uses cmov instead of conditional jump
    mov     r8d, 1                  ; assume 'R' (sign = 1)
    mov     ecx, -1                 ; alternative (sign = -1)
    cmp     al, 'R'
    cmovne  r8d, ecx                ; if not 'R', sign = -1
    inc     rsi

    ; parse magnitude (must be sequential - digit by digit)
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

    ; Branchless first calculation: first = (sign==1) ? 100-at : at
    mov     eax, [at_var]           ; eax = at (for sign == -1)
    mov     ecx, 100
    sub     ecx, eax                ; ecx = 100 - at (for sign == 1)
    cmp     r8d, 1
    cmove   eax, ecx                ; if sign==1, eax = 100-at

    ; Branchless: if first == 0, first = 100
    mov     ecx, 100
    test    eax, eax
    cmovz   eax, ecx                ; if first==0, first = 100
    mov     r9d, eax                ; save first in r9d

    ; Crossing hits calculation (keep branch - division is expensive)
    xor     edx, edx
    cmp     ebx, r9d                ; mag >= first?
    jl      .hits_skip
    mov     ecx, ebx
    sub     ecx, r9d                ; ecx = mag - first
    xor     edx, edx
    mov     eax, ecx
    mov     ecx, 100
    div     ecx                     ; eax = (mag-first)/100
    inc     eax
    mov     edx, eax
.hits_skip:
    add     [cross_var], edx

    ; Position update: pos = (at + sign*mag) % 100
    mov     eax, [at_var]
    imul    ebx, r8d                ; ebx = sign * mag
    add     eax, ebx                ; eax = at + sign*mag
    cdq
    mov     ecx, 100
    idiv    ecx                     ; edx = remainder (may be negative)

    ; Branchless negative modulo fix: if edx < 0, edx += 100
    mov     eax, edx
    add     edx, 100                ; edx = remainder + 100
    test    eax, eax
    cmovns  edx, eax                ; if original >= 0, use original
    mov     [at_var], edx

    ; Branchless zero counting: zero_var += (pos == 0)
    xor     eax, eax
    test    edx, edx
    setz    al                      ; al = 1 if pos==0, else 0
    add     [zero_var], eax
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
