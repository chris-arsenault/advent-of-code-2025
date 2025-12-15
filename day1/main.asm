; Day 1 solution in x86-64 assembly (SysV ABI)
;
; Hand-written ASM style:
; - No frame pointer (rbp) - uses rsp-relative addressing only when needed
; - Register-centric: state variables kept in callee-saved registers, not memory
; - Branchless techniques for all conditional arithmetic
; - Clean, intentional register allocation documented at function entry
;
; Uses C stdio/stdlib for file IO and timing.

global main
extern printf
extern perror
extern read_file_all
extern clock_gettime
extern ns_since

%define BUF_SIZE 1048576

section .data
input_file:    db "input.txt", 0
fmt_out:       db "zero_landings=%d crossings=%d final_pos=%d elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

section .bss
buf:           resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2

section .text

;------------------------------------------------------------------------------
; main - entry point
;
; Register allocation (callee-saved, persist across calls):
;   r12d = at (current dial position, starts at 50)
;   r13d = zero_count (times dial lands on 0)
;   r14d = cross_count (times dial crosses 0)
;   r15  = end pointer (buf + bytes_read)
;   rsi  = current buffer pointer (caller-saved but no calls in hot loop)
;   ebx  = magnitude (caller-saved but no calls in hot loop)
;
; Scratch registers (reset each iteration):
;   eax, ecx, edx, r8d, r9d
;------------------------------------------------------------------------------
main:
    ; Save callee-saved registers (no frame pointer - hand-written style)
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    ; 5 pushes = 40 bytes, plus 8 for return addr = 48 bytes
    ; RSP is now 0 mod 16 (correct for function calls)

    ; start timing
    mov     edi, 1
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; read entire file into buffer
    lea     rdi, [rel input_file]
    lea     rsi, [rel buf]
    mov     rdx, BUF_SIZE
    call    read_file_all
    cmp     rax, 0
    jl      .open_fail
    mov     r15, rax                ; save bytes_read in r15 temporarily

    ; Initialize state in registers (no memory variables - hand-written style)
    mov     r12d, 50                ; at = 50 (starting dial position)
    xor     r13d, r13d              ; zero_count = 0
    xor     r14d, r14d              ; cross_count = 0

    ; Set up pointers
    test    r15, r15
    jle     .after_read
    lea     rsi, [rel buf]          ; ptr
    add     r15, rsi                ; r15 = buf + bytes_read (end pointer)

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
    mov     eax, r12d               ; eax = at (for sign == -1)
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
    add     r14d, edx               ; cross_count += hits

    ; Position update: pos = (at + sign*mag) % 100
    mov     eax, r12d               ; eax = at
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
    mov     r12d, edx               ; at = new position

    ; Branchless zero counting: zero_count += (pos == 0)
    xor     eax, eax
    test    edx, edx
    setz    al                      ; al = 1 if pos==0, else 0
    add     r13d, eax               ; zero_count += (pos == 0)
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
    ; end timing
    mov     edi, 1
    lea     rsi, [rel ts1]
    call    clock_gettime

    ; elapsed_ns = ns_since(&ts0, &ts1)
    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since

    ; convert to ms in xmm0
    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    ; printf (using register values directly)
    lea     rdi, [rel fmt_out]
    mov     esi, r13d               ; zero_count
    mov     edx, r14d               ; cross_count
    mov     ecx, r12d               ; at (final position)
    mov     eax, 1                  ; 1 XMM register used
    call    printf

    xor     eax, eax
.exit:
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

section .note.GNU-stack noalloc noexec nobits
