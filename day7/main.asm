; Day 7: Tachyon Beam Splitter
; Hand-written x86-64 assembly - register-centric design
; Optimizations: bitmask iteration (tzcnt/blsr), SIMD clearing, row pointer arithmetic
; Requires: BMI1 (tzcnt, blsr), BMI2 (shlx), AVX2 (vpxor, vmovdqu)

global main
extern clock_gettime
extern printf
extern perror
extern ns_since
extern read_file_all

; ============================================================================
; Register allocation (callee-saved, preserved across calls):
;   rbx  = row counter
;   r12d = width (constant after parsing)
;   r13d = height (constant after parsing)
;   r14d = splits (part 1 result, preserved through part 2)
;   r15  = timelines (part 2 result)
;
; Stack frame (minimal):
;   Alignment: ret(8) + pushes(40) + locals(16) = 64 = 0 mod 16
; ============================================================================
%define STK_START_R     0               ; 4 bytes - starting row
%define STK_START_C     4               ; 4 bytes - starting column
%define STK_SIZE        16              ; total frame size (8 bytes padding)

%define CLOCK_MONOTONIC 1
%define BUF_SIZE        1048576
%define MAX_ROWS        256
%define MAX_COLS        256
%define MAX_COLS_SHIFT  8               ; 256 = 1 << 8

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

; Bitmask arrays (256 bits = 4 qwords per array)
align 32
active_mask:   resq 4
next_mask:     resq 4

; Particle counts for Part 2 (per-column counts)
align 32
counts:        resq MAX_COLS
next_cnt:      resq MAX_COLS

section .text

main:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, STK_SIZE

    ; ========================================================================
    ; Read input file
    ; ========================================================================
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
    ; rax = bytes_read (no need to store, compute end ptr immediately)

    ; ========================================================================
    ; Parse grid and find starting position 'S'
    ; Registers during parsing:
    ;   rsi = current file pointer
    ;   rdi = end of file pointer
    ;   r8  = grid row pointer
    ;   ebx = row counter
    ;   ecx = column counter
    ;   r12d = max width (building up)
    ; ========================================================================
    lea     rsi, [rel file_buf]
    lea     rdi, [rsi + rax]            ; end = buf + bytes_read
    lea     r8, [rel grid]

    xor     ebx, ebx                    ; row = 0
    xor     ecx, ecx                    ; col = 0
    xor     r12d, r12d                  ; width = 0
    mov     dword [rsp + STK_START_R], -1
    mov     dword [rsp + STK_START_C], -1

.parse_loop:
    cmp     rsi, rdi
    jge     .parse_done
    movzx   eax, byte [rsi]
    inc     rsi

    cmp     al, 10                      ; newline?
    je      .parse_newline
    cmp     al, 13                      ; carriage return?
    je      .parse_loop

    ; Store character: grid[row_ptr + col] = char
    mov     [r8 + rcx], al

    ; Check for starting position 'S'
    cmp     al, 'S'
    jne     .not_start
    mov     [rsp + STK_START_R], ebx
    mov     [rsp + STK_START_C], ecx

.not_start:
    inc     ecx
    cmp     ecx, r12d
    cmovg   r12d, ecx                   ; width = max(width, col+1)
    jmp     .parse_loop

.parse_newline:
    inc     ebx                         ; row++
    add     r8, MAX_COLS                ; row_ptr += MAX_COLS
    xor     ecx, ecx                    ; col = 0
    jmp     .parse_loop

.parse_done:
    ; Handle last row if not ending with newline
    test    ecx, ecx
    jz      .no_last_row
    cmp     ecx, r12d
    cmovg   r12d, ecx
    inc     ebx
.no_last_row:
    mov     r13d, ebx                   ; r13d = height (in register!)
    ; r12d = width (already set)

    ; ========================================================================
    ; Start timing
    ; ========================================================================
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; ========================================================================
    ; Part 1: Count splits using bitmask simulation
    ; Registers:
    ;   ebx = row
    ;   r8  = grid row pointer
    ;   r9  = active_mask base
    ;   r10 = next_mask base
    ;   r14d = splits (accumulator)
    ;   r12d = width, r13d = height (constants)
    ; ========================================================================

    ; Clear active_mask (4 qwords = 256 bits)
    lea     r9, [rel active_mask]
    xor     eax, eax
    mov     [r9], rax
    mov     [r9 + 8], rax
    mov     [r9 + 16], rax
    mov     [r9 + 24], rax

    ; Set starting position bit using bts
    mov     eax, [rsp + STK_START_C]
    mov     ecx, eax
    shr     ecx, 6                      ; qword index
    and     eax, 63                     ; bit index
    bts     qword [r9 + rcx*8], rax

    xor     r14d, r14d                  ; splits = 0
    mov     ebx, [rsp + STK_START_R]    ; row = start_r

    ; r8 = grid + row * MAX_COLS
    lea     r8, [rel grid]
    mov     ecx, ebx
    shl     ecx, MAX_COLS_SHIFT
    add     r8, rcx

    lea     r10, [rel next_mask]

.p1_row_loop:
    cmp     ebx, r13d                   ; compare with height in register
    jge     .p1_done

    ; Clear next_mask
    xor     eax, eax
    mov     [r10], rax
    mov     [r10 + 8], rax
    mov     [r10 + 16], rax
    mov     [r10 + 24], rax

    ; Process each qword of active_mask
    xor     ecx, ecx                    ; qword_idx = 0

.p1_qword_loop:
    cmp     ecx, 4
    jge     .p1_qword_done

    mov     rax, [r9 + rcx*8]
    test    rax, rax
    jz      .p1_next_qword

    ; Process bits using tzcnt/blsr
    mov     r11, rax                    ; bits to process

    ; Hoist qword base column calculation (qword_idx * 64)
    mov     eax, ecx
    shl     eax, 6                      ; eax = qword_base_col (preserved in esi)
    mov     esi, eax

.p1_bit_loop:
    test    r11, r11
    jz      .p1_next_qword

    tzcnt   rdi, r11                    ; bit position
    blsr    r11, r11                    ; clear lowest bit

    ; Column = qword_base_col + bit_position
    lea     eax, [esi + edi]
    cmp     eax, r12d                   ; compare with width in register
    jge     .p1_bit_loop

    ; Get grid cell
    movzx   edx, byte [r8 + rax]

    cmp     dl, '^'
    jne     .p1_no_split

    ; Split: increment count, add left and right to next_mask
    inc     r14d

    ; Left (col-1) if col > 0
    lea     edx, [eax - 1]
    test    eax, eax
    jz      .p1_try_right
    ; Set bit using bts: next_mask[col/64] |= (1 << col%64)
    mov     edi, edx
    shr     edi, 6                      ; qword index
    and     edx, 63                     ; bit index
    bts     qword [r10 + rdi*8], rdx

.p1_try_right:
    ; Right (col+1) if col+1 < width
    lea     edx, [eax + 1]
    cmp     edx, r12d
    jge     .p1_bit_loop
    mov     edi, edx
    shr     edi, 6
    and     edx, 63
    bts     qword [r10 + rdi*8], rdx
    jmp     .p1_bit_loop

.p1_no_split:
    ; Not a splitter: pass through using bts
    mov     edx, eax
    mov     edi, edx
    shr     edi, 6
    and     edx, 63
    bts     qword [r10 + rdi*8], rdx
    jmp     .p1_bit_loop

.p1_next_qword:
    inc     ecx
    jmp     .p1_qword_loop

.p1_qword_done:
    ; Copy next_mask to active_mask
    mov     rax, [r10]
    mov     [r9], rax
    mov     rax, [r10 + 8]
    mov     [r9 + 8], rax
    mov     rax, [r10 + 16]
    mov     [r9 + 16], rax
    mov     rax, [r10 + 24]
    mov     [r9 + 24], rax

    ; Check if any beams remain
    mov     rax, [r9]
    or      rax, [r9 + 8]
    or      rax, [r9 + 16]
    or      rax, [r9 + 24]
    test    rax, rax
    jz      .p1_done

    inc     ebx
    add     r8, MAX_COLS
    jmp     .p1_row_loop

.p1_done:
    ; r14d = splits (preserved in callee-saved register, no stack save needed)

    ; ========================================================================
    ; Part 2: Count particles (quantum timelines)
    ; Registers:
    ;   ebx = row
    ;   r8  = grid row pointer
    ;   r9  = counts base
    ;   r10 = next_cnt base
    ;   r15 = timelines (accumulator)
    ;   r12d = width, r13d = height, r14d = splits (preserved)
    ; ========================================================================

    ; Clear counts array using SIMD
    lea     r9, [rel counts]
    vpxor   ymm0, ymm0, ymm0
    mov     ecx, MAX_COLS / 4
    mov     rdi, r9
.clear_counts:
    vmovdqu [rdi], ymm0
    add     rdi, 32
    dec     ecx
    jnz     .clear_counts

    ; Set starting count = 1
    mov     eax, [rsp + STK_START_C]
    mov     qword [r9 + rax*8], 1

    mov     ebx, [rsp + STK_START_R]

    ; r8 = grid + row * MAX_COLS
    lea     r8, [rel grid]
    mov     ecx, ebx
    shl     ecx, MAX_COLS_SHIFT
    add     r8, rcx

    lea     r10, [rel next_cnt]

.p2_row_loop:
    cmp     ebx, r13d
    jge     .p2_done

    ; Clear next_cnt using SIMD
    vpxor   ymm0, ymm0, ymm0
    mov     ecx, MAX_COLS / 4
    mov     rdi, r10
.clear_next_cnt:
    vmovdqu [rdi], ymm0
    add     rdi, 32
    dec     ecx
    jnz     .clear_next_cnt

    ; Process columns with particles
    xor     ecx, ecx                    ; col = 0

.p2_col_loop:
    cmp     ecx, r12d                   ; compare with width in register
    jge     .p2_col_done

    mov     r11, [r9 + rcx*8]
    test    r11, r11
    jz      .p2_next_col

    movzx   edx, byte [r8 + rcx]

    cmp     dl, '^'
    jne     .p2_no_split

    ; Split: add count to neighbors
    test    ecx, ecx
    jz      .p2_try_right
    mov     eax, ecx
    dec     eax
    add     [r10 + rax*8], r11

.p2_try_right:
    mov     eax, ecx
    inc     eax
    cmp     eax, r12d
    jge     .p2_next_col
    add     [r10 + rax*8], r11
    jmp     .p2_next_col

.p2_no_split:
    add     [r10 + rcx*8], r11

.p2_next_col:
    inc     ecx
    jmp     .p2_col_loop

.p2_col_done:
    ; Copy next_cnt to counts using SIMD
    mov     ecx, MAX_COLS / 4
    mov     rdi, r9
    mov     rsi, r10
.copy_counts:
    vmovdqu ymm0, [rsi]
    vmovdqu [rdi], ymm0
    add     rdi, 32
    add     rsi, 32
    dec     ecx
    jnz     .copy_counts

    inc     ebx
    add     r8, MAX_COLS
    jmp     .p2_row_loop

.p2_done:
    ; Sum counts into r15
    xor     r15, r15
    xor     ecx, ecx
.sum_loop:
    cmp     ecx, r12d
    jge     .sum_done
    add     r15, [r9 + rcx*8]
    inc     ecx
    jmp     .sum_loop

.sum_done:
    vzeroupper

    ; ========================================================================
    ; End timing and print results
    ; ========================================================================
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts1]
    call    clock_gettime

    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since

    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    ; printf(fmt, splits, timelines, elapsed_ms)
    ; r14d = splits (still in register!), r15 = timelines
    mov     esi, r14d
    mov     rdx, r15
    lea     rdi, [rel fmt_out]
    mov     eax, 1
    call    printf

    xor     eax, eax

.exit:
    add     rsp, STK_SIZE
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

section .note.GNU-stack noalloc noexec
