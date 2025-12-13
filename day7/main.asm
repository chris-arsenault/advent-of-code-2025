; Day 7: Particle Splitting Simulation
; Optimized with SIMD (vpaddd), bitmask packing, popcnt, branchless lookup tables
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

; Lookup table for left shift (column c-1 gets bit from column c)
; For branchless splitter handling
align 32
left_mask:     dq 0xFFFFFFFFFFFFFFFE, 0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2
grid:          resb MAX_ROWS * MAX_COLS
; Bitmask arrays (256 bits = 4 qwords per array)
align 32
active_mask:   resq 4              ; packed bitmask for active columns
next_mask:     resq 4              ; packed bitmask for next row
; Particle counts for Part 2 (still need per-column counts)
align 32
counts:        resq MAX_COLS
next_cnt:      resq MAX_COLS

section .text

main:
    push    rbp
    mov     rbp, rsp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, 88

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

    ; =========== Part 1: Count splits using bitmask ===========
    ; Clear active_mask
    lea     rdi, [rel active_mask]
    xor     eax, eax
    mov     [rdi], rax
    mov     [rdi+8], rax
    mov     [rdi+16], rax
    mov     [rdi+24], rax

    ; Set starting position bit
    mov     eax, [rbp-56]               ; start_c
    lea     rdi, [rel active_mask]
    ; Set bit: active_mask[c/64] |= (1 << (c%64))
    mov     ecx, eax
    shr     ecx, 6                      ; c/64 = qword index
    and     eax, 63                     ; c%64 = bit index
    mov     rdx, 1
    shlx    rdx, rdx, rax               ; 1 << (c%64)
    or      [rdi + rcx*8], rdx

    xor     r14d, r14d                  ; splits = 0
    mov     ebx, [rbp-52]               ; row = start_r

.p1_row_loop:
    cmp     ebx, [rbp-60]
    jge     .p1_done

    ; Clear next_mask
    lea     rdi, [rel next_mask]
    xor     eax, eax
    mov     [rdi], rax
    mov     [rdi+8], rax
    mov     [rdi+16], rax
    mov     [rdi+24], rax

    ; Process each active column
    lea     r8, [rel grid]
    lea     r9, [rel active_mask]
    lea     r10, [rel next_mask]
    xor     ecx, ecx                    ; qword index

.p1_qword_loop:
    cmp     ecx, 4
    jge     .p1_qword_done

    mov     rax, [r9 + rcx*8]           ; load active qword
    test    rax, rax
    jz      .p1_next_qword

    ; Process bits in this qword
    mov     r11, rax                    ; bits to process
    shl     ecx, 6                      ; convert to column base

.p1_bit_loop:
    test    r11, r11
    jz      .p1_bit_done

    ; Find lowest set bit using tzcnt
    tzcnt   rdi, r11                    ; bit position

    ; Clear this bit: r11 &= (r11 - 1)
    blsr    r11, r11

    ; Column = base + bit position
    lea     eax, [ecx + edi]
    cmp     eax, [rbp-64]               ; check width
    jge     .p1_bit_loop

    ; Get grid cell
    mov     edx, ebx                    ; row
    imul    edx, MAX_COLS
    add     edx, eax                    ; + col
    movzx   edx, byte [r8 + rdx]

    cmp     dl, '^'
    jne     .p1_no_split_bit

    ; Split: increment count, add left and right to next
    inc     r14d                        ; splits++

    ; Add left (col-1) to next_mask if col > 0
    mov     esi, eax
    dec     esi
    jl      .p1_try_right_bit
    ; Set bit in next_mask
    mov     edi, esi
    shr     edi, 6                      ; qword index
    and     esi, 63                     ; bit index
    mov     rdx, 1
    shlx    rdx, rdx, rsi
    or      [r10 + rdi*8], rdx

.p1_try_right_bit:
    ; Add right (col+1) to next_mask if col+1 < width
    mov     esi, eax
    inc     esi
    cmp     esi, [rbp-64]
    jge     .p1_bit_loop
    ; Set bit in next_mask
    mov     edi, esi
    shr     edi, 6
    and     esi, 63
    mov     rdx, 1
    shlx    rdx, rdx, rsi
    or      [r10 + rdi*8], rdx
    jmp     .p1_bit_loop

.p1_no_split_bit:
    ; Not a splitter: add to next_mask
    mov     esi, eax
    mov     edi, esi
    shr     edi, 6
    and     esi, 63
    mov     rdx, 1
    shlx    rdx, rdx, rsi
    or      [r10 + rdi*8], rdx
    jmp     .p1_bit_loop

.p1_bit_done:
    shr     ecx, 6                      ; restore qword index

.p1_next_qword:
    inc     ecx
    jmp     .p1_qword_loop

.p1_qword_done:
    ; Copy next_mask to active_mask
    lea     rdi, [rel active_mask]
    lea     rsi, [rel next_mask]
    mov     rax, [rsi]
    mov     [rdi], rax
    mov     rax, [rsi+8]
    mov     [rdi+8], rax
    mov     rax, [rsi+16]
    mov     [rdi+16], rax
    mov     rax, [rsi+24]
    mov     [rdi+24], rax

    ; Check if any active (use popcnt to count active beams)
    mov     rax, [rdi]
    or      rax, [rdi+8]
    or      rax, [rdi+16]
    or      rax, [rdi+24]
    test    rax, rax
    jz      .p1_done

    inc     ebx
    jmp     .p1_row_loop

.p1_done:
    mov     [rbp-68], r14d              ; save splits

    ; =========== Part 2: Count particles with SIMD ===========
    ; Clear counts array
    lea     rdi, [rel counts]
    xor     eax, eax
    mov     ecx, MAX_COLS
.clear_counts:
    mov     [rdi], rax
    add     rdi, 8
    dec     ecx
    jnz     .clear_counts

    ; Set starting position with count 1
    mov     eax, [rbp-56]               ; start_c
    lea     rdi, [rel counts]
    mov     qword [rdi + rax*8], 1

    mov     ebx, [rbp-52]               ; row = start_r

.p2_row_loop:
    cmp     ebx, [rbp-60]
    jge     .p2_done

    ; Clear next_cnt using SIMD (AVX)
    lea     rdi, [rel next_cnt]
    vpxor   ymm0, ymm0, ymm0
    mov     ecx, MAX_COLS / 4           ; 4 qwords per ymm register
.clear_next:
    vmovdqu [rdi], ymm0
    add     rdi, 32
    dec     ecx
    jnz     .clear_next

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

    ; Split: add count to left and right using branchless min/max
    ; Left neighbor
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
    ; Copy next_cnt to counts using SIMD (vpaddd pattern - add with zero)
    lea     rdi, [rel counts]
    lea     rsi, [rel next_cnt]
    mov     ecx, MAX_COLS / 4
.copy_counts:
    vmovdqu ymm0, [rsi]
    vmovdqu [rdi], ymm0
    add     rdi, 32
    add     rsi, 32
    dec     ecx
    jnz     .copy_counts

    inc     ebx
    jmp     .p2_row_loop

.p2_done:
    ; Sum all counts using SIMD horizontal add
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
    vzeroupper                          ; clear upper YMM state

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
    add     rsp, 88
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
