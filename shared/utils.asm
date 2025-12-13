; Shared helper functions (pure syscall based)

global ns_since
global read_file_all
global parse_uint64
global skip_non_digits
global uint64_digit_count
global uint64_to_digits
global pow10

section .text

;------------------------------------------------------------------------------
; uint64_t parse_uint64(const char **pptr, const char *end)
; Parses an unsigned 64-bit integer from string.
; Input:  rdi = pointer to pointer to current position (updated on return)
;         rsi = end pointer
; Output: rax = parsed value (0 if no digits found)
; Updates *pptr to point after the last digit parsed.
;------------------------------------------------------------------------------
parse_uint64:
    push    rbx
    mov     rbx, [rdi]          ; ptr = *pptr
    xor     eax, eax            ; result = 0
    xor     ecx, ecx            ; found_digit = 0

.parse_loop:
    cmp     rbx, rsi
    jge     .done
    movzx   edx, byte [rbx]
    cmp     dl, '0'
    jb      .done
    cmp     dl, '9'
    ja      .done
    ; Found a digit
    mov     ecx, 1              ; found_digit = true
    sub     dl, '0'
    imul    rax, rax, 10
    movzx   edx, dl
    add     rax, rdx
    inc     rbx
    jmp     .parse_loop

.done:
    mov     [rdi], rbx          ; *pptr = updated position
    pop     rbx
    ret

;------------------------------------------------------------------------------
; const char* skip_non_digits(const char *ptr, const char *end)
; Skips whitespace and common separators (space, comma, dash, newline, CR).
; Input:  rdi = current pointer
;         rsi = end pointer
; Output: rax = pointer to first digit or end
;------------------------------------------------------------------------------
skip_non_digits:
    mov     rax, rdi
.skip_loop:
    cmp     rax, rsi
    jge     .skip_done
    movzx   ecx, byte [rax]
    cmp     cl, ' '
    je      .skip_next
    cmp     cl, ','
    je      .skip_next
    cmp     cl, '-'
    je      .skip_next
    cmp     cl, 10              ; newline
    je      .skip_next
    cmp     cl, 13              ; carriage return
    je      .skip_next
    cmp     cl, 9               ; tab
    je      .skip_next
    ; Not a separator, check if digit
    cmp     cl, '0'
    jb      .skip_next          ; skip other non-digit chars
    cmp     cl, '9'
    ja      .skip_next
    ; Found a digit
    jmp     .skip_done
.skip_next:
    inc     rax
    jmp     .skip_loop
.skip_done:
    ret

;------------------------------------------------------------------------------
; uint64_t uint64_digit_count(uint64_t n)
; Returns the number of decimal digits in n.
; Input:  rdi = number
; Output: rax = digit count (1 for n=0)
;------------------------------------------------------------------------------
uint64_digit_count:
    mov     rax, rdi
    xor     ecx, ecx            ; count = 0
    test    rax, rax
    jnz     .count_loop
    mov     eax, 1              ; n=0 has 1 digit
    ret
.count_loop:
    test    rax, rax
    jz      .count_done
    inc     ecx
    xor     edx, edx
    mov     r8, 10
    div     r8                  ; rax = rax / 10
    jmp     .count_loop
.count_done:
    mov     eax, ecx
    ret

;------------------------------------------------------------------------------
; uint64_t uint64_to_digits(uint64_t n, uint8_t *buf)
; Extracts decimal digits of n into buf (most significant first).
; Input:  rdi = number
;         rsi = buffer pointer (must have space for 20 bytes)
; Output: rax = digit count
;         buf filled with digits as bytes 0-9 (not ASCII)
;------------------------------------------------------------------------------
uint64_to_digits:
    push    rbx
    push    r12
    push    r13
    mov     r12, rsi            ; save buf
    mov     rax, rdi            ; n
    xor     r13d, r13d          ; count = 0

    ; Handle n=0 special case
    test    rax, rax
    jnz     .extract_loop
    mov     byte [r12], 0
    mov     eax, 1
    pop     r13
    pop     r12
    pop     rbx
    ret

.extract_loop:
    ; Extract digits in reverse order onto stack area
    test    rax, rax
    jz      .reverse
    xor     edx, edx
    mov     rcx, 10
    div     rcx                 ; rax = n/10, rdx = n%10
    mov     [r12 + r13], dl     ; store digit (reversed)
    inc     r13
    jmp     .extract_loop

.reverse:
    ; Reverse the digits in place
    xor     ecx, ecx            ; i = 0
    mov     rdx, r13
    dec     rdx                 ; j = count - 1
.reverse_loop:
    cmp     rcx, rdx
    jge     .reverse_done
    mov     al, [r12 + rcx]
    mov     bl, [r12 + rdx]
    mov     [r12 + rcx], bl
    mov     [r12 + rdx], al
    inc     rcx
    dec     rdx
    jmp     .reverse_loop
.reverse_done:
    mov     rax, r13            ; return count
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; uint64_t pow10(uint32_t exp)
; Returns 10^exp.
; Input:  rdi = exponent (0-19 valid for 64-bit result)
; Output: rax = 10^exp
;------------------------------------------------------------------------------
pow10:
    mov     eax, 1
    test    edi, edi
    jz      .pow_done
    mov     ecx, edi
.pow_loop:
    imul    rax, rax, 10
    dec     ecx
    jnz     .pow_loop
.pow_done:
    ret

; long long ns_since(const struct timespec *start, const struct timespec *end)
; Returns nanoseconds between start and end.
ns_since:
    mov     rax, [rsi]        ; end.tv_sec
    sub     rax, [rdi]        ; end.tv_sec - start.tv_sec
    imul    rax, rax, 1000000000
    mov     rdx, [rsi + 8]    ; end.tv_nsec
    sub     rdx, [rdi + 8]    ; end.tv_nsec - start.tv_nsec
    add     rax, rdx
    ret

%define SYS_OPEN 2
%define SYS_READ 0
%define SYS_CLOSE 3

; ssize_t read_file_all(const char *path, void *buf, size_t bufsize)
; Returns bytes read or -1 on error.
; Note: syscall clobbers rcx and r11, so we use r14 for total.
read_file_all:
    push    rbp
    mov     rbp, rsp
    push    r12
    push    r13
    push    r14
    push    r15                 ; keep stack 16-byte aligned

    mov     r12, rsi            ; buf
    mov     r13, rdx            ; bufsize
    mov     r15, rdi            ; save path (rdi clobbered by syscall args)

    ; open(path, O_RDONLY, 0)
    mov     eax, SYS_OPEN
    ; rdi already contains path
    xor     esi, esi            ; O_RDONLY
    xor     edx, edx            ; mode = 0
    syscall
    cmp     rax, 0
    jl      .err
    mov     r10, rax            ; fd

    xor     r14d, r14d          ; total = 0 (use r14, not r11!)

.read_loop:
    cmp     r14, r13
    jge     .finish_ok

    mov     eax, SYS_READ
    mov     rdi, r10            ; fd
    lea     rsi, [r12 + r14]    ; buf + total
    mov     rdx, r13
    sub     rdx, r14            ; bufsize - total
    syscall

    test    rax, rax
    jl      .read_error
    je      .finish_ok

    add     r14, rax            ; accumulate total (r14 survives syscall)
    jmp     .read_loop

.finish_ok:
    mov     eax, SYS_CLOSE
    mov     rdi, r10
    syscall
    mov     rax, r14            ; return total bytes read
    jmp     .ret

.read_error:
    mov     eax, SYS_CLOSE
    mov     rdi, r10
    syscall
    mov     rax, -1
    jmp     .ret

.err:
    mov     rax, -1

.ret:
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbp
    ret

section .note.GNU-stack noalloc noexec nowrite progbits