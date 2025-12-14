; Day 6: Grid Block Evaluation (Hand-Written Assembly)
;
; Part 1: Parse numbers horizontally per block row, apply operator (+/*)
; Part 2: Read digits vertically per column (right-to-left), apply operator
;
; Style notes:
;   - RSP-relative addressing, no frame pointer
;   - Symbolic constants for stack offsets
;   - Row base pointers to eliminate imul from hot loops
;   - SHL for power-of-two multiply (MAX_COLS = 8192 = 2^13)
;   - LEA-based fast multiply for num*10

global main
extern clock_gettime
extern printf
extern ns_since
extern read_file_all

; ─────────────────────────────────────────────────────────────────────────────
; Constants
; ─────────────────────────────────────────────────────────────────────────────
%define CLOCK_MONOTONIC 1
%define BUF_SIZE        1048576
%define MAX_ROWS        16
%define MAX_COLS        8192
%define MAX_COLS_SHIFT  13          ; 8192 = 1 << 13
%define MAX_BLOCKS      1024

; Stack frame layout (RSP-relative)
; Alignment: ret(8) + pushes(40) + locals(64) = 112 = 0 mod 16
%define STK_TS0         0           ; struct timespec (16 bytes)
%define STK_TS1         16          ; struct timespec (16 bytes)
%define STK_ROWS        32          ; uint32_t (4 bytes)
%define STK_WIDTH       36          ; uint32_t (4 bytes)
%define STK_BLKCOUNT    40          ; uint32_t (4 bytes)
%define STK_SIZE        64          ; total frame size

section .data
    input_file:    db "input.txt", 0
    fmt_out:       db "grand_total=%llu quantum_total=%llu elapsed_ms=%.3f", 10, 0
    one_million:   dq 1000000.0

section .bss
    file_buf:      resb BUF_SIZE
    grid:          resb MAX_ROWS * MAX_COLS
    block_starts:  resd MAX_BLOCKS
    block_ends:    resd MAX_BLOCKS

section .text

; ─────────────────────────────────────────────────────────────────────────────
; main
; ─────────────────────────────────────────────────────────────────────────────
main:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, STK_SIZE

    ; ─────────────────────────────────────────────────────────────────────────
    ; Read input file into buffer
    ; ─────────────────────────────────────────────────────────────────────────
    lea     rdi, [rel input_file]
    lea     rsi, [rel file_buf]
    mov     edx, BUF_SIZE
    call    read_file_all
    test    rax, rax
    jle     .error_exit

    mov     r14, rax                    ; r14 = bytes_read
    lea     r12, [rel file_buf]         ; r12 = buf_start
    lea     r13, [r12 + r14]            ; r13 = buf_end

    ; ─────────────────────────────────────────────────────────────────────────
    ; First pass: count rows, find max width
    ; Already optimal - no changes needed
    ; ─────────────────────────────────────────────────────────────────────────
    mov     rsi, r12                    ; ptr = buf_start
    xor     ebx, ebx                    ; rows = 0
    xor     r8d, r8d                    ; max_width = 0
    xor     r9d, r9d                    ; cur_col = 0

.count_loop:
    cmp     rsi, r13
    jge     .count_done
    movzx   eax, byte [rsi]
    inc     rsi
    cmp     al, 10
    je      .count_newline
    cmp     al, 13
    je      .count_loop
    inc     r9d
    jmp     .count_loop

.count_newline:
    cmp     r9d, r8d
    cmovg   r8d, r9d
    inc     ebx
    xor     r9d, r9d
    jmp     .count_loop

.count_done:
    test    r9d, r9d
    jz      .count_save
    cmp     r9d, r8d
    cmovg   r8d, r9d
    inc     ebx

.count_save:
    mov     [rsp + STK_ROWS], ebx
    mov     [rsp + STK_WIDTH], r8d

    ; ─────────────────────────────────────────────────────────────────────────
    ; Fill grid with spaces
    ; Use SHL instead of IMUL for power-of-two multiply
    ; ─────────────────────────────────────────────────────────────────────────
    lea     rdi, [rel grid]
    mov     al, ' '
    mov     ecx, ebx
    shl     ecx, MAX_COLS_SHIFT         ; ecx = rows * 8192
    rep     stosb

    ; ─────────────────────────────────────────────────────────────────────────
    ; Second pass: copy file content to grid
    ; Use row_base pointer instead of computing row * MAX_COLS each char
    ; ─────────────────────────────────────────────────────────────────────────
    mov     rsi, r12                    ; ptr = buf_start
    lea     rdi, [rel grid]             ; row_base = grid (row 0)
    xor     r11d, r11d                  ; col = 0

.copy_loop:
    cmp     rsi, r13
    jge     .copy_done
    movzx   eax, byte [rsi]
    inc     rsi
    cmp     al, 10
    je      .copy_newline
    cmp     al, 13
    je      .copy_loop
    ; grid[row_base + col] = char
    mov     [rdi + r11], al
    inc     r11d
    jmp     .copy_loop

.copy_newline:
    add     rdi, MAX_COLS               ; row_base += MAX_COLS
    xor     r11d, r11d                  ; col = 0
    jmp     .copy_loop

.copy_done:
    ; ─────────────────────────────────────────────────────────────────────────
    ; Start timing
    ; ─────────────────────────────────────────────────────────────────────────
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rsp + STK_TS0]
    call    clock_gettime

    ; ─────────────────────────────────────────────────────────────────────────
    ; Find block boundaries (empty column = all spaces in data rows)
    ;
    ; Use row_ptr incremented per row instead of imul
    ; ─────────────────────────────────────────────────────────────────────────
    mov     r8d, [rsp + STK_ROWS]
    mov     r9d, [rsp + STK_WIDTH]
    lea     r10, [rel grid]             ; grid base
    lea     r11, [rel block_starts]
    lea     r12, [rel block_ends]
    xor     r13d, r13d                  ; block_count = 0
    xor     r14d, r14d                  ; col = 0
    xor     r15d, r15d                  ; in_block = false

    mov     ebx, r8d
    dec     ebx                         ; num_data_rows = rows - 1

.scan_col:
    cmp     r14d, r9d
    jge     .scan_end

    ; check if column r14 is all spaces in rows 0..num_data_rows-1
    ; Use row_ptr instead of computing row * MAX_COLS
    mov     rdi, r10                    ; row_ptr = grid
    xor     ecx, ecx                    ; row = 0
    mov     esi, 1                      ; assume empty

.check_row:
    cmp     ecx, ebx
    jge     .check_done
    movzx   eax, byte [rdi + r14]       ; grid[row_ptr + col]
    cmp     al, ' '
    jne     .not_empty
    add     rdi, MAX_COLS               ; row_ptr += MAX_COLS
    inc     ecx
    jmp     .check_row

.not_empty:
    xor     esi, esi                    ; empty = false

.check_done:
    test    esi, esi
    jnz     .is_empty_col

    ; non-empty column
    test    r15d, r15d
    jnz     .scan_next
    mov     r15d, 1
    mov     [r11 + r13*4], r14d         ; block_starts[count] = col
    jmp     .scan_next

.is_empty_col:
    test    r15d, r15d
    jz      .scan_next
    mov     [r12 + r13*4], r14d         ; block_ends[count] = col
    inc     r13d
    xor     r15d, r15d

.scan_next:
    inc     r14d
    jmp     .scan_col

.scan_end:
    ; close final block if open
    test    r15d, r15d
    jz      .blocks_ready
    mov     [r12 + r13*4], r9d
    inc     r13d

.blocks_ready:
    mov     [rsp + STK_BLKCOUNT], r13d

    ; ─────────────────────────────────────────────────────────────────────────
    ; Process each block
    ;
    ; Keep P1/P2 totals in registers throughout (rbx, r13)
    ; ─────────────────────────────────────────────────────────────────────────
    mov     eax, [rsp + STK_ROWS]
    dec     eax
    mov     r8d, eax                    ; num_data_rows
    lea     r9, [rel grid]              ; grid base
    xor     r12d, r12d                  ; block_idx = 0
    xor     ebx, ebx                    ; P1 total (kept in register!)
    xor     r13d, r13d                  ; P2 total (kept in register!)

.block_loop:
    mov     eax, [rsp + STK_BLKCOUNT]
    cmp     r12d, eax
    jge     .blocks_done

    ; load block boundaries
    lea     rax, [rel block_starts]
    mov     r14d, [rax + r12*4]         ; start col
    lea     rax, [rel block_ends]
    mov     r15d, [rax + r12*4]         ; end col

    ; ─────────────────────────────────────────────────────────────────────────
    ; Find operator in the last row (operator row)
    ; Single imul here is fine (outside hot loops)
    ; ─────────────────────────────────────────────────────────────────────────
    mov     eax, r8d
    shl     eax, MAX_COLS_SHIFT         ; op_row * MAX_COLS
    lea     rdi, [r9 + rax]             ; op_row_ptr = grid + offset
    mov     esi, r14d                   ; col = start
    xor     ecx, ecx                    ; operator: 0=add, 1=mul

.find_op:
    cmp     esi, r15d
    jge     .op_done
    movzx   eax, byte [rdi + rsi]
    cmp     al, '+'
    je      .op_done
    cmp     al, '*'
    jne     .find_op_next
    mov     ecx, 1
    jmp     .op_done
.find_op_next:
    inc     esi
    jmp     .find_op
.op_done:
    ; ecx = operator (0=add, 1=mul)
    ; Save operator for Part 2
    push    rcx

    ; ─────────────────────────────────────────────────────────────────────────
    ; Part 1: Parse numbers horizontally from each data row
    ;
    ; Use row_ptr incremented per row instead of imul
    ; Inner loop only does: load byte, check digit, accumulate
    ; ─────────────────────────────────────────────────────────────────────────
    ; init accumulator: 0 for add, 1 for mul
    mov     r11, 1
    test    ecx, ecx
    jnz     .p1_start
    xor     r11d, r11d
.p1_start:
    mov     rdi, r9                     ; row_ptr = grid (row 0)
    xor     r10d, r10d                  ; row = 0

.p1_row:
    cmp     r10d, r8d
    jge     .p1_block_done

    ; parse number from this row
    xor     eax, eax                    ; num = 0
    xor     edx, edx                    ; found = 0
    mov     esi, r14d                   ; col = start

.p1_col:
    cmp     esi, r15d
    jge     .p1_row_done

    ; grid[row_ptr + col]
    movzx   ecx, byte [rdi + rsi]

    cmp     cl, '0'
    jb      .p1_skip
    cmp     cl, '9'
    ja      .p1_skip

    ; digit found: num = num*10 + digit
    ; LEA-based multiply by 10
    lea     rax, [rax + rax*4]          ; rax *= 5
    add     rax, rax                    ; rax *= 2 (total: *10)
    ; Fix partial-register: movzx before sub
    movzx   ecx, cl
    sub     ecx, '0'
    add     rax, rcx
    mov     edx, 1                      ; found = true

.p1_skip:
    inc     esi
    jmp     .p1_col

.p1_row_done:
    ; apply operator if we found a number
    test    edx, edx
    jz      .p1_next_row
    pop     rcx                         ; get operator
    push    rcx                         ; restore for later
    test    ecx, ecx
    jnz     .p1_mul
    add     r11, rax
    jmp     .p1_next_row
.p1_mul:
    imul    r11, rax

.p1_next_row:
    add     rdi, MAX_COLS               ; row_ptr += MAX_COLS
    inc     r10d
    jmp     .p1_row

.p1_block_done:
    add     rbx, r11                    ; P1 total += block result

    ; ─────────────────────────────────────────────────────────────────────────
    ; Part 2: Parse numbers vertically from each column (right to left)
    ;
    ; Use row_ptr incremented per row instead of imul
    ; ─────────────────────────────────────────────────────────────────────────
    pop     rcx                         ; get operator
    mov     r11, 1
    test    ecx, ecx
    jnz     .p2_start
    xor     r11d, r11d
.p2_start:
    push    rcx                         ; save operator again
    mov     esi, r15d
    dec     esi                         ; col = end - 1

.p2_col:
    cmp     esi, r14d
    jl      .p2_block_done

    ; parse number from this column (top to bottom)
    xor     eax, eax                    ; num = 0
    xor     edx, edx                    ; found = 0
    mov     rdi, r9                     ; row_ptr = grid (row 0)
    xor     r10d, r10d                  ; row = 0

.p2_row:
    cmp     r10d, r8d
    jge     .p2_col_done

    ; grid[row_ptr + col]
    movzx   ecx, byte [rdi + rsi]

    cmp     cl, '0'
    jb      .p2_vskip
    cmp     cl, '9'
    ja      .p2_vskip

    ; digit: num = num*10 + digit
    lea     rax, [rax + rax*4]
    add     rax, rax
    ; Fix partial-register: movzx before sub
    movzx   ecx, cl
    sub     ecx, '0'
    add     rax, rcx
    mov     edx, 1

.p2_vskip:
    add     rdi, MAX_COLS               ; row_ptr += MAX_COLS
    inc     r10d
    jmp     .p2_row

.p2_col_done:
    test    edx, edx
    jz      .p2_next_col
    ; get operator without consuming it
    mov     ecx, [rsp]
    test    ecx, ecx
    jnz     .p2_vmul
    add     r11, rax
    jmp     .p2_next_col
.p2_vmul:
    imul    r11, rax

.p2_next_col:
    dec     esi
    jmp     .p2_col

.p2_block_done:
    add     r13, r11                    ; P2 total += block result
    add     rsp, 8                      ; pop saved operator

    inc     r12d
    jmp     .block_loop

.blocks_done:
    ; ─────────────────────────────────────────────────────────────────────────
    ; End timing, print results
    ; rbx = P1 total, r13 = P2 total
    ; ─────────────────────────────────────────────────────────────────────────
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rsp + STK_TS1]
    call    clock_gettime

    lea     rdi, [rsp + STK_TS0]
    lea     rsi, [rsp + STK_TS1]
    call    ns_since

    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    mov     rsi, rbx                    ; P1 total from register
    mov     rdx, r13                    ; P2 total from register
    lea     rdi, [rel fmt_out]
    mov     eax, 1
    call    printf

    xor     eax, eax
    jmp     .exit

.error_exit:
    mov     eax, 1

.exit:
    add     rsp, STK_SIZE
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

section .note.GNU-stack noalloc noexec
