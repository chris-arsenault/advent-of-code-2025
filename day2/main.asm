; Day 2 pure x86-64 assembly using syscalls via shared helper.
; SysV ABI, x86_64, Linux/WSL2.

global main
extern clock_gettime
extern printf
extern perror

; Shared utilities from shared/utils.asm
extern ns_since
extern read_file_all
extern parse_uint64
extern skip_non_digits
extern uint64_digit_count
extern uint64_to_digits
extern pow10

%define CLOCK_MONOTONIC 1
%define BUF_SIZE 1048576

section .data
input_file:    db "input.txt", 0
fmt_out:       db "part1_sum=%llu part2_sum=%llu elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2
digit_buf:     resb 24          ; buffer for digit extraction

section .text

;------------------------------------------------------------------------------
; bool is_even_half(uint64_t n)
; Returns 1 if n has even digit count and first half equals second half.
; Uses shared: uint64_digit_count, pow10
;------------------------------------------------------------------------------
is_even_half:
    push    rbp
    mov     rbp, rsp
    push    rbx
    push    r12

    mov     rbx, rdi            ; save n

    ; Get digit count
    call    uint64_digit_count
    mov     r12d, eax           ; digit_count

    ; Check if even
    test    r12d, 1
    jnz     .eh_ret_zero

    ; half_exp = digit_count / 2
    mov     edi, r12d
    shr     edi, 1
    call    pow10               ; rax = 10^(digit_count/2)

    ; first_half = n / 10^half, second_half = n % 10^half
    mov     rcx, rax            ; divisor
    mov     rax, rbx            ; n
    xor     edx, edx
    div     rcx                 ; rax = first_half, rdx = second_half

    cmp     rax, rdx
    jne     .eh_ret_zero
    mov     eax, 1
    jmp     .eh_ret

.eh_ret_zero:
    xor     eax, eax
.eh_ret:
    pop     r12
    pop     rbx
    pop     rbp
    ret

;------------------------------------------------------------------------------
; bool is_periodic(uint64_t n)
; Returns 1 if decimal digits form a repeating pattern.
; Uses shared: uint64_to_digits
;------------------------------------------------------------------------------
is_periodic:
    push    rbp
    mov     rbp, rsp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, 8              ; align stack to 16 bytes

    mov     rbx, rdi            ; save n

    ; Extract digits to buffer
    lea     rsi, [rel digit_buf]
    call    uint64_to_digits
    mov     r8, rax             ; r8 = digit count
    lea     r9, [rel digit_buf] ; r9 = buffer pointer

    ; Need at least 2 digits to have a pattern
    cmp     r8, 2
    jb      .per_not_periodic

    ; Try each possible pattern length from 1 to len-1
    mov     r10, 1              ; substr_len = 1
.per_substr_loop:
    cmp     r10, r8
    jge     .per_not_periodic

    ; Check if len % substr_len == 0
    mov     rax, r8
    xor     edx, edx
    div     r10                 ; rax = len / substr_len, rdx = len % substr_len
    test    rdx, rdx
    jnz     .per_next_substr
    mov     r11, rax            ; reps = len / substr_len

    ; Check if pattern repeats
    xor     r12d, r12d          ; idx = 0
.per_idx_loop:
    cmp     r12, r10
    jge     .per_found_periodic

    ; first = buf[idx]
    movzx   r13d, byte [r9 + r12]
    mov     r14, 1              ; rep = 1
.per_rep_loop:
    cmp     r14, r11
    jge     .per_next_idx
    mov     rax, r14
    imul    rax, r10            ; rep * substr_len
    add     rax, r12            ; pos = rep * substr_len + idx
    movzx   r15d, byte [r9 + rax]
    cmp     r15d, r13d
    jne     .per_next_substr
    inc     r14
    jmp     .per_rep_loop

.per_next_idx:
    inc     r12
    jmp     .per_idx_loop

.per_found_periodic:
    mov     eax, 1
    jmp     .per_ret

.per_next_substr:
    inc     r10
    jmp     .per_substr_loop

.per_not_periodic:
    xor     eax, eax

.per_ret:
    add     rsp, 8
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

;------------------------------------------------------------------------------
; main: parse input and compute sums
; Register allocation for parse loop (callee-saved registers survive function calls):
;   rbx = current value in range loop
;   r12 = current parse position (ptr)
;   r13 = end pointer
;   r14 = first value of pair
;   r15 = second value of pair
; Stack locals:
;   [rbp-56] = part1 sum
;   [rbp-48] = part2 sum
;   [rbp-40] = state (0=need first, 1=need second)
;------------------------------------------------------------------------------
main:
    push    rbp
    mov     rbp, rsp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, 56             ; align 16

    ; Read entire file into buffer using shared helper
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
    mov     r14, rax            ; save bytes read
    mov     qword [rbp-56], 0   ; part1 sum
    mov     qword [rbp-48], 0   ; part2 sum
    mov     qword [rbp-40], 0   ; state = 0 (need first)

    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; Set up buffer pointers (use callee-saved registers)
    lea     r12, [rel file_buf] ; current position
    lea     r13, [rel file_buf]
    add     r13, r14            ; end pointer

    xor     r14, r14            ; first value

.parse_loop:
    cmp     r12, r13
    jge     .after_read

    ; Skip non-digit characters using shared function
    mov     rdi, r12
    mov     rsi, r13
    call    skip_non_digits
    mov     r12, rax            ; update position

    cmp     r12, r13
    jge     .after_read

    ; Parse number using shared function
    lea     rdi, [rbp-64]       ; temporary pointer storage
    mov     [rbp-64], r12       ; store current position
    mov     rsi, r13            ; end pointer
    call    parse_uint64
    mov     r15, rax            ; parsed number
    mov     r12, [rbp-64]       ; updated position

    ; Check state
    cmp     qword [rbp-40], 0
    jne     .have_second
    mov     r14, r15            ; first value
    mov     qword [rbp-40], 1   ; state = need second
    jmp     .parse_loop

.have_second:
    ; Process range [r14, r15]
    mov     rbx, r14
.range_loop:
    cmp     rbx, r15
    ja      .range_done

    mov     rdi, rbx
    call    is_even_half
    test    al, al
    jz      .skip_p1
    add     [rbp-56], rbx
.skip_p1:
    mov     rdi, rbx
    call    is_periodic
    test    al, al
    jz      .skip_p2
    add     [rbp-48], rbx
.skip_p2:
    inc     rbx
    jmp     .range_loop

.range_done:
    mov     qword [rbp-40], 0   ; reset state for next pair
    jmp     .parse_loop

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

    mov     rsi, [rbp-56]       ; part1
    mov     rdx, [rbp-48]       ; part2
    lea     rdi, [rel fmt_out]
    call    printf
    xor     eax, eax

.exit:
    add     rsp, 56
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

section .note.GNU-stack noalloc nobits progbits align=1
