; Day 7: Particle Splitting Simulation
; Part 1: Count total splits
; Part 2: Count total particles at end

global main
extern clock_gettime
extern printf
extern perror
extern ns_since
extern read_file_all

%define CLOCK_MONOTONIC 1
%define BUF_SIZE 1048576
%define MAX_ROWS 256
%define MAX_COLS 256

section .data
input_file:    db "input.txt", 0
fmt_out:       db "splits=%d timelines=%llu elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2
grid:          resb MAX_ROWS * MAX_COLS
active:        resb MAX_COLS              ; bitmap for Part 1
next_act:      resb MAX_COLS              ; bitmap for Part 1
counts:        resq MAX_COLS              ; particle counts for Part 2
next_cnt:      resq MAX_COLS              ; next particle counts

section .text

main:
    push    rbp
    mov     rbp, rsp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, 72

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
    mov     [rbp-48], rax               ; bytes read

    ; Parse grid and find S
    lea     r12, [rel file_buf]
    mov     r13, [rbp-48]
    add     r13, r12                    ; end ptr
    lea     r8, [rel grid]

    xor     ebx, ebx                    ; row
    xor     r14d, r14d                  ; col
    xor     r15d, r15d                  ; width
    mov     dword [rbp-52], -1          ; start_r
    mov     dword [rbp-56], -1          ; start_c

.parse_loop:
    cmp     r12, r13
    jge     .parse_done
    movzx   eax, byte [r12]
    inc     r12
    cmp     al, 10
    je      .parse_newline
    cmp     al, 13
    je      .parse_loop

    ; Store char
    mov     edx, ebx
    imul    edx, MAX_COLS
    add     edx, r14d
    mov     [r8 + rdx], al

    ; Check for S
    cmp     al, 'S'
    jne     .not_start
    mov     [rbp-52], ebx               ; start_r
    mov     [rbp-56], r14d              ; start_c

.not_start:
    inc     r14d
    cmp     r14d, r15d
    cmovg   r15d, r14d
    jmp     .parse_loop

.parse_newline:
    inc     ebx
    xor     r14d, r14d
    jmp     .parse_loop

.parse_done:
    ; Handle last row
    test    r14d, r14d
    jz      .no_last
    cmp     r14d, r15d
    cmovg   r15d, r14d
    inc     ebx
.no_last:
    mov     [rbp-60], ebx               ; height
    mov     [rbp-64], r15d              ; width

    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; Part 1: Count splits
    ; Initialize active bitmap
    lea     rdi, [rel active]
    xor     eax, eax
    mov     ecx, MAX_COLS
    rep     stosb

    ; Set starting position
    mov     eax, [rbp-56]               ; start_c
    lea     rdi, [rel active]
    mov     byte [rdi + rax], 1

    xor     r14d, r14d                  ; splits = 0
    mov     ebx, [rbp-52]               ; row = start_r

.p1_row_loop:
    cmp     ebx, [rbp-60]
    jge     .p1_done

    ; Clear next_act
    lea     rdi, [rel next_act]
    xor     eax, eax
    mov     ecx, MAX_COLS
    rep     stosb

    ; Process active columns
    lea     r8, [rel grid]
    lea     r9, [rel active]
    lea     r10, [rel next_act]
    xor     ecx, ecx                    ; col

.p1_col_loop:
    cmp     ecx, [rbp-64]
    jge     .p1_col_done

    ; Check if active
    movzx   eax, byte [r9 + rcx]
    test    al, al
    jz      .p1_next_col

    ; Get grid cell
    mov     eax, ebx
    imul    eax, MAX_COLS
    add     eax, ecx
    movzx   edx, byte [r8 + rax]

    cmp     dl, '^'
    jne     .p1_no_split

    ; Split
    inc     r14d                        ; splits++
    ; Add left
    test    ecx, ecx
    jz      .p1_try_right
    mov     eax, ecx
    dec     eax
    mov     byte [r10 + rax], 1
.p1_try_right:
    mov     eax, ecx
    inc     eax
    cmp     eax, [rbp-64]
    jge     .p1_next_col
    mov     byte [r10 + rax], 1
    jmp     .p1_next_col

.p1_no_split:
    mov     byte [r10 + rcx], 1

.p1_next_col:
    inc     ecx
    jmp     .p1_col_loop

.p1_col_done:
    ; Copy next_act to active
    lea     rdi, [rel active]
    lea     rsi, [rel next_act]
    mov     ecx, MAX_COLS
    rep     movsb

    inc     ebx
    jmp     .p1_row_loop

.p1_done:
    mov     [rbp-68], r14d              ; save splits

    ; Part 2: Count particles
    ; Clear counts
    lea     rdi, [rel counts]
    xor     eax, eax
    mov     ecx, MAX_COLS * 8
    rep     stosb

    ; Set starting position with count 1
    mov     eax, [rbp-56]               ; start_c
    lea     rdi, [rel counts]
    mov     qword [rdi + rax*8], 1

    mov     ebx, [rbp-52]               ; row = start_r

.p2_row_loop:
    cmp     ebx, [rbp-60]
    jge     .p2_done

    ; Clear next_cnt
    lea     rdi, [rel next_cnt]
    xor     eax, eax
    mov     ecx, MAX_COLS * 8
    rep     stosb

    ; Process columns with particles
    lea     r8, [rel grid]
    lea     r9, [rel counts]
    lea     r10, [rel next_cnt]
    xor     ecx, ecx                    ; col

.p2_col_loop:
    cmp     ecx, [rbp-64]
    jge     .p2_col_done

    ; Get particle count
    mov     r11, [r9 + rcx*8]
    test    r11, r11
    jz      .p2_next_col

    ; Get grid cell
    mov     eax, ebx
    imul    eax, MAX_COLS
    add     eax, ecx
    movzx   edx, byte [r8 + rax]

    cmp     dl, '^'
    jne     .p2_no_split

    ; Split: add count to left and right
    test    ecx, ecx
    jz      .p2_try_right2
    mov     eax, ecx
    dec     eax
    add     [r10 + rax*8], r11
.p2_try_right2:
    mov     eax, ecx
    inc     eax
    cmp     eax, [rbp-64]
    jge     .p2_next_col
    add     [r10 + rax*8], r11
    jmp     .p2_next_col

.p2_no_split:
    add     [r10 + rcx*8], r11

.p2_next_col:
    inc     ecx
    jmp     .p2_col_loop

.p2_col_done:
    ; Copy next_cnt to counts
    lea     rdi, [rel counts]
    lea     rsi, [rel next_cnt]
    mov     ecx, MAX_COLS * 8
    rep     movsb

    inc     ebx
    jmp     .p2_row_loop

.p2_done:
    ; Sum all counts
    xor     r15, r15                    ; total
    lea     r9, [rel counts]
    xor     ecx, ecx
.sum_loop:
    cmp     ecx, [rbp-64]
    jge     .sum_done
    add     r15, [r9 + rcx*8]
    inc     ecx
    jmp     .sum_loop

.sum_done:
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts1]
    call    clock_gettime

    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since
    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    mov     esi, [rbp-68]               ; splits
    mov     rdx, r15                    ; timelines
    lea     rdi, [rel fmt_out]
    mov     eax, 1
    call    printf
    xor     eax, eax

.exit:
    add     rsp, 72
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
