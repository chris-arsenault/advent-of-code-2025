; Day 3: Maximum Digits Sum
; Part 1: Find best 2-digit number from each line (first digit before second)
; Part 2: Find best k-digit number using greedy stack algorithm

global main
extern clock_gettime
extern printf
extern perror
extern ns_since
extern read_file_all

%define CLOCK_MONOTONIC 1
%define BUF_SIZE 1048576
%define MAX_LINE 256
%define K_DIGITS 12

section .data
input_file:    db "input.txt", 0
fmt_out:       db "max-2-digit-sum=%llu max-12-digit-sum=%llu elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2
line_digits:   resb MAX_LINE      ; digits as bytes 0-9
suffix_max:    resb MAX_LINE      ; suffix max array
stack_buf:     resb MAX_LINE      ; stack for greedy algorithm

section .text

;------------------------------------------------------------------------------
; uint64_t best_two_digits(uint8_t *digits, int len)
; Find maximum 2-digit number where first comes before second
;------------------------------------------------------------------------------
best_two_digits:
    push    rbp
    mov     rbp, rsp
    push    rbx
    push    r12
    push    r13
    push    r14

    mov     r12, rdi            ; digits
    mov     r13d, esi           ; len

    cmp     r13d, 2
    jl      .btd_zero

    ; Build suffix_max array
    lea     r14, [rel suffix_max]
    mov     byte [r14 + r13], 0 ; suffix_max[len] = 0

    mov     ecx, r13d
    dec     ecx                 ; i = len - 1
.suffix_loop:
    cmp     ecx, 0
    jl      .suffix_done
    movzx   eax, byte [r12 + rcx]       ; digits[i]
    movzx   edx, byte [r14 + rcx + 1]   ; suffix_max[i+1]
    cmp     al, dl
    cmovl   eax, edx            ; max(digits[i], suffix_max[i+1])
    mov     [r14 + rcx], al
    dec     ecx
    jmp     .suffix_loop
.suffix_done:

    ; Find best 2-digit combination
    mov     ebx, -1             ; best = -1
    xor     ecx, ecx            ; i = 0
    mov     edx, r13d
    dec     edx                 ; len - 1
.find_best:
    cmp     ecx, edx
    jge     .btd_return
    movzx   eax, byte [r12 + rcx]       ; d = digits[i]
    imul    eax, eax, 10
    movzx   r8d, byte [r14 + rcx + 1]   ; suffix_max[i+1]
    add     eax, r8d                    ; candidate = 10*d + suffix_max[i+1]
    cmp     eax, ebx
    cmovg   ebx, eax
    inc     ecx
    jmp     .find_best

.btd_return:
    mov     eax, ebx
    cmp     eax, 0
    jge     .btd_done
    xor     eax, eax
.btd_done:
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

.btd_zero:
    xor     eax, eax
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

;------------------------------------------------------------------------------
; uint64_t best_k_digits(uint8_t *digits, int len, int k)
; Greedy monotonic stack algorithm for best k-digit number
;------------------------------------------------------------------------------
best_k_digits:
    push    rbp
    mov     rbp, rsp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15

    mov     r12, rdi            ; digits
    mov     r13d, esi           ; len
    mov     r14d, edx           ; k

    cmp     r13d, r14d
    jl      .bkd_zero           ; len < k, not enough digits

    lea     r15, [rel stack_buf]
    xor     ebx, ebx            ; stack_top = 0

    ; drop = len - k
    mov     r8d, r13d
    sub     r8d, r14d           ; drop count

    xor     ecx, ecx            ; i = 0
.greedy_loop:
    cmp     ecx, r13d
    jge     .greedy_done

    movzx   eax, byte [r12 + rcx]   ; d = digits[i]

    ; while drop > 0 and stack not empty and stack[-1] < d
.pop_loop:
    test    r8d, r8d
    jz      .push_digit
    test    ebx, ebx
    jz      .push_digit
    movzx   edx, byte [r15 + rbx - 1]   ; stack[-1]
    cmp     dl, al
    jge     .push_digit
    ; pop
    dec     ebx
    dec     r8d
    jmp     .pop_loop

.push_digit:
    mov     [r15 + rbx], al
    inc     ebx
    inc     ecx
    jmp     .greedy_loop

.greedy_done:
    ; Trim to k digits if needed
    cmp     ebx, r14d
    cmovg   ebx, r14d

    ; Convert stack to number
    xor     rax, rax            ; result = 0
    xor     ecx, ecx            ; i = 0
.convert_loop:
    cmp     ecx, ebx
    jge     .bkd_done
    imul    rax, rax, 10
    movzx   edx, byte [r15 + rcx]
    add     rax, rdx
    inc     ecx
    jmp     .convert_loop

.bkd_done:
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

.bkd_zero:
    xor     eax, eax
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

;------------------------------------------------------------------------------
; main
;------------------------------------------------------------------------------
main:
    push    rbp
    mov     rbp, rsp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, 40

    ; Read file
    lea     rdi, [rel input_file]
    lea     rsi, [rel file_buf]
    mov     rdx, BUF_SIZE
    call    read_file_all
    cmp     rax, 0
    jg      .file_ok
    lea     rdi, [rel err_open]
    call    perror
    mov     eax, 1
    jmp     .exit

.file_ok:
    mov     r14, rax            ; bytes read
    mov     qword [rbp-48], 0   ; p1_sum
    mov     qword [rbp-56], 0   ; p2_sum

    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; Parse lines
    lea     r12, [rel file_buf]
    lea     r13, [rel file_buf]
    add     r13, r14

.line_loop:
    cmp     r12, r13
    jge     .after_read

    ; Extract digits from line
    lea     r15, [rel line_digits]
    xor     ebx, ebx            ; digit count

.digit_loop:
    cmp     r12, r13
    jge     .process_line
    movzx   eax, byte [r12]
    cmp     al, 10              ; newline
    je      .end_line
    cmp     al, 13              ; CR
    je      .skip_char
    cmp     al, '0'
    jb      .skip_char
    cmp     al, '9'
    ja      .skip_char
    ; It's a digit
    sub     al, '0'
    mov     [r15 + rbx], al
    inc     ebx
.skip_char:
    inc     r12
    jmp     .digit_loop

.end_line:
    inc     r12                 ; skip newline
.process_line:
    test    ebx, ebx
    jz      .line_loop          ; empty line

    ; best_two_digits
    mov     rdi, r15
    mov     esi, ebx
    call    best_two_digits
    add     [rbp-48], rax

    ; best_k_digits
    mov     rdi, r15
    mov     esi, ebx
    mov     edx, K_DIGITS
    call    best_k_digits
    add     [rbp-56], rax

    jmp     .line_loop

.after_read:
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts1]
    call    clock_gettime

    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since
    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    mov     rsi, [rbp-48]
    mov     rdx, [rbp-56]
    lea     rdi, [rel fmt_out]
    call    printf
    xor     eax, eax

.exit:
    add     rsp, 40
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
