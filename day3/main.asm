; Day 3: Maximum Digits Sum
;
; Hand-written ASM style:
; - No frame pointers - rsp-relative locals with symbolic offsets
; - Register-centric: hot pointers in registers, cold accumulators on stack
; - LEA-based multiply-by-10 (lower latency than IMUL)
; - Unsigned compares for digit data (0-9)
; - No defensive fixes for impossible conditions
;
; Algorithm (from ALGORITHMS.md):
; - Part 1: Suffix max array for O(1) best 2-digit lookup
; - Part 2: Greedy monotonic stack for k-digit selection

global main
extern printf
extern perror
extern read_file_all
extern clock_gettime
extern ns_since

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
line_digits:   resb MAX_LINE
suffix_max:    resb MAX_LINE
stack_buf:     resb MAX_LINE

section .text

;------------------------------------------------------------------------------
; uint64_t best_two_digits(uint8_t *digits, int len)
; Find maximum 2-digit number where first digit comes before second.
; Uses suffix max array for O(1) best-pair lookups.
;
; Register ownership:
;   r12 = digits pointer (constant)
;   r13d = len (constant)
;   r14 = suffix_max pointer (constant)
;   rbx = best result (accumulator)
;   ecx = loop index
;------------------------------------------------------------------------------
best_two_digits:
    push    rbx
    push    r12
    push    r13
    push    r14

    mov     r12, rdi
    mov     r13d, esi

    cmp     r13d, 2
    jl      .btd_zero

    ; Build suffix_max array (no sentinel needed)
    ; Initialize: suffix_max[len-1] = digits[len-1]
    lea     r14, [rel suffix_max]
    mov     ecx, r13d
    dec     ecx                         ; ecx = len-1
    movzx   eax, byte [r12 + rcx]
    mov     [r14 + rcx], al

    ; Loop from len-2 down to 0
    dec     ecx
.suffix_loop:
    cmp     ecx, 0
    jl      .suffix_done
    movzx   eax, byte [r12 + rcx]       ; digits[i]
    movzx   edx, byte [r14 + rcx + 1]   ; suffix_max[i+1]
    cmp     al, dl
    cmovb   eax, edx                    ; unsigned max
    mov     [r14 + rcx], al
    dec     ecx
    jmp     .suffix_loop
.suffix_done:

    ; Find best 2-digit combination: max(10*digits[i] + suffix_max[i+1])
    xor     ebx, ebx                    ; best = 0
    xor     ecx, ecx                    ; i = 0
    mov     edx, r13d
    dec     edx                         ; limit = len - 1
.find_best:
    cmp     ecx, edx
    jge     .btd_done
    movzx   eax, byte [r12 + rcx]       ; d = digits[i]
    lea     eax, [eax + eax*4]
    add     eax, eax                    ; eax = d * 10
    movzx   r8d, byte [r14 + rcx + 1]   ; suffix_max[i+1]
    add     eax, r8d                    ; candidate = 10*d + suffix_max[i+1]
    cmp     eax, ebx
    cmova   ebx, eax                    ; unsigned max
    inc     ecx
    jmp     .find_best

.btd_zero:
    xor     ebx, ebx
.btd_done:
    mov     eax, ebx
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; uint64_t best_k_digits(uint8_t *digits, int len, int k)
; Greedy monotonic stack algorithm for selecting k digits to form largest number.
;
; Algorithm:
;   drop = len - k (max digits we can discard)
;   For each digit d:
;     While drop > 0 and stack not empty and stack.top < d:
;       pop (discard smaller digit)
;       drop--
;     Push d
;   Stack contains exactly k digits (invariant)
;
; Register ownership:
;   r12 = digits pointer (constant)
;   r13d = len (constant)
;   r14d = k (constant)
;   r15 = stack_buf pointer (constant)
;   ebx = stack_top (accumulator)
;   r8d = drop count (decremented during pops)
;   ecx = input index
;   eax = current digit (during pop loop)
;------------------------------------------------------------------------------
best_k_digits:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15

    mov     r12, rdi
    mov     r13d, esi
    mov     r14d, edx

    cmp     r13d, r14d
    jl      .bkd_zero

    lea     r15, [rel stack_buf]
    xor     ebx, ebx                    ; stack_top = 0

    ; drop = len - k
    mov     r8d, r13d
    sub     r8d, r14d

    xor     ecx, ecx                    ; i = 0
.greedy_loop:
    cmp     ecx, r13d
    jge     .greedy_done

    movzx   eax, byte [r12 + rcx]       ; d = digits[i]

    ; Pop while: drop > 0 AND stack not empty AND stack.top < d
.pop_loop:
    test    r8d, r8d
    jz      .push_digit
    test    ebx, ebx
    jz      .push_digit
    movzx   edx, byte [r15 + rbx - 1]   ; stack.top
    cmp     edx, eax                    ; unsigned compare
    jae     .push_digit                 ; stack.top >= d
    dec     ebx
    dec     r8d
    jmp     .pop_loop

.push_digit:
    mov     [r15 + rbx], al
    inc     ebx
    inc     ecx
    jmp     .greedy_loop

.greedy_done:
    ; Truncate to k digits (needed when input is non-increasing)
    ; This is part of the algorithm, not a defensive check
    cmp     ebx, r14d
    cmovg   ebx, r14d

    ; Convert stack to number
    xor     rax, rax                    ; result = 0
    xor     ecx, ecx                    ; i = 0
.convert_loop:
    cmp     ecx, ebx
    jge     .bkd_done
    lea     rax, [rax + rax*4]
    add     rax, rax                    ; rax *= 10
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
    ret

.bkd_zero:
    xor     eax, eax
    jmp     .bkd_done

;------------------------------------------------------------------------------
; main
;
; Register ownership (hot loop optimized):
;   r15 = line_digits pointer (constant)
;   r14 = end pointer (constant, hot - checked every char)
;   r13 = current parse pointer (hot - updated every char)
;   r12 = digit count (hot - updated every digit)
;   rbx = scratch
;
; Stack frame (cold accumulators):
;------------------------------------------------------------------------------
%define MAIN_P1_SUM     0
%define MAIN_P2_SUM     8
%define MAIN_FRAME      32      ; 16 bytes used + 16 alignment

main:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, MAIN_FRAME

    ; --- Start timing ---
    mov     edi, 1
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; --- Read file ---
    lea     rdi, [rel input_file]
    lea     rsi, [rel file_buf]
    mov     edx, BUF_SIZE
    call    read_file_all
    test    rax, rax
    jg      .file_ok

    lea     rdi, [rel err_open]
    call    perror
    mov     eax, 1
    jmp     .exit

.file_ok:
    ; Cache pointers in registers (hot path optimization)
    lea     r13, [rel file_buf]         ; current pointer
    lea     r14, [rel file_buf]
    add     r14, rax                    ; end pointer (constant)
    lea     r15, [rel line_digits]      ; digits buffer (constant)

    ; Initialize accumulators on stack (cold, only touched per-line)
    mov     qword [rsp + MAIN_P1_SUM], 0
    mov     qword [rsp + MAIN_P2_SUM], 0

.line_loop:
    cmp     r13, r14
    jge     .after_read

    xor     r12d, r12d                  ; digit count = 0

.digit_loop:
    cmp     r13, r14
    jge     .process_line
    movzx   eax, byte [r13]
    cmp     al, 10
    je      .end_line
    cmp     al, 13
    je      .skip_char
    ; Check if digit '0'-'9'
    sub     al, '0'
    cmp     al, 9
    ja      .skip_char
    ; Store digit value (0-9)
    mov     [r15 + r12], al
    inc     r12d
    inc     r13
    jmp     .digit_loop

.skip_char:
    inc     r13
    jmp     .digit_loop

.end_line:
    inc     r13
.process_line:
    test    r12d, r12d
    jz      .line_loop                  ; empty line

    ; Part 1: best_two_digits
    mov     rdi, r15
    mov     esi, r12d
    call    best_two_digits
    add     [rsp + MAIN_P1_SUM], rax

    ; Part 2: best_k_digits
    mov     rdi, r15
    mov     esi, r12d
    mov     edx, K_DIGITS
    call    best_k_digits
    add     [rsp + MAIN_P2_SUM], rax

    jmp     .line_loop

.after_read:
    ; --- End timing ---
    mov     edi, 1
    lea     rsi, [rel ts1]
    call    clock_gettime

    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since
    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    ; --- Output ---
    lea     rdi, [rel fmt_out]
    mov     rsi, [rsp + MAIN_P1_SUM]
    mov     rdx, [rsp + MAIN_P2_SUM]
    mov     eax, 1                  ; one XMM arg
    call    printf

    xor     eax, eax

.exit:
    add     rsp, MAIN_FRAME
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

section .note.GNU-stack noalloc noexec nobits
