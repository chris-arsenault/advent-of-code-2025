; Day 4: Grid Neighbor Analysis
;
; Hand-written ASM style with advanced optimizations:
; - No frame pointers - rsp-relative locals with symbolic offsets
; - Bitfield packing: grid and removed use 1 bit per cell (8x memory reduction)
; - Inlined neighbor counting with popcnt (no function call in hot loop)
; - setc pattern: branchless bitmask building (control→data dependency)
; - Full SIMD Part 1: pmovmskb + popcnt for 16-cell batches
; - Packed BFS queue: (row<<8 | col) in 16-bit entries (rows/cols ≤ 256)
; - Hoisted imul: row offsets computed once per row, not per cell
;
; Algorithm (from ALGORITHMS.md):
; - Part 1: Count '@' cells with < 4 neighbors (8-connected)
; - Part 2: BFS removal until no accessible cells remain

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
%define MAX_CELLS (MAX_ROWS * MAX_COLS)
%define BITS_PER_ROW (MAX_COLS / 8)     ; 32 bytes per row in bitfield

section .data
    align 64                            ; Cache-line alignment
input_file:    db "input.txt", 0
fmt_out:       db "accessible=%d removable_total=%d elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

; Threshold vector for SIMD comparison (16 copies of value 3)
    align 16
threshold_vec: times 16 db 3

; Bit expansion masks for SIMD
; For each byte position i (0-15), the mask isolates bit (i % 8)
; Bytes 0-7 test bits 0-7 of the low byte
; Bytes 8-15 test bits 0-7 of the high byte
    align 16
bit_masks:     db 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80
               db 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80

; Shuffle mask to broadcast low byte to bytes 0-7, high byte to bytes 8-15
    align 16
broadcast_shuf: db 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1

; 8-connected neighbor offsets (dr, dc) as dword pairs
neighbors:     dd -1, -1,  -1, 0,  -1, 1,  0, -1,  0, 1,  1, -1,  1, 0,  1, 1

section .bss
    align 64                            ; Cache-line alignment for all arrays
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2

; Bitfield-packed arrays (1 bit per cell)
grid_bits:     resb MAX_CELLS / 8       ; 8KB instead of 64KB
removed_bits:  resb MAX_CELLS / 8       ; 8KB instead of 64KB

; Byte arrays (kept for BFS update simplicity)
counts:        resb MAX_CELLS           ; neighbor counts per cell

; BFS queue - packed (row<<8 | col) entries (16-bit each)
; This halves memory traffic compared to separate dwords
queue:         resw MAX_CELLS           ; 2 bytes per entry

section .text

;------------------------------------------------------------------------------
; main
;
; Register ownership varies by phase (documented inline).
;
; Stack frame layout:
;------------------------------------------------------------------------------
%define ROWS        0
%define COLS        4
%define MAX_COL_TMP 8
%define ACCESSIBLE  12
%define MAIN_FRAME  32              ; 16 bytes used + 16 alignment

main:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, MAIN_FRAME

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
    mov     r14, rax                ; bytes read

    ; Clear bitfield arrays (cache-line efficient)
    lea     rdi, [rel grid_bits]
    xor     eax, eax
    mov     ecx, MAX_CELLS / 8
    rep     stosb

    ; Initialize parsing state
    ; r12 = current pointer, r13 = end pointer
    ; ebx = row, r15d = col
    lea     r12, [rel file_buf]
    lea     r13, [rel file_buf]
    add     r13, r14

    xor     ebx, ebx                ; current row
    xor     r15d, r15d              ; current col
    mov     dword [rsp + MAX_COL_TMP], 0

    lea     r8, [rel grid_bits]     ; cache grid_bits pointer

    ; Precompute row byte offset (row * BITS_PER_ROW)
    ; Hoisted outside inner loop - updated only on newline
    xor     r9d, r9d                ; row_byte_offset = 0

.parse_loop:
    cmp     r12, r13
    jge     .parse_done

    movzx   eax, byte [r12]
    inc     r12

    cmp     al, 10                  ; newline
    je      .newline
    cmp     al, 13                  ; CR
    je      .parse_loop

    ; Check for '@' - set bit in grid_bits
    cmp     al, '@'
    jne     .not_at

    ; Set bit at (row, col) using bts (clearer, one instruction)
    ; byte_offset = row_byte_offset + col / 8 (row_byte_offset precomputed)
    ; bit_position = col & 7
    mov     eax, r15d
    shr     eax, 3                  ; col / 8
    add     eax, r9d                ; + row_byte_offset
    mov     ecx, r15d
    and     ecx, 7                  ; bit position
    bts     dword [r8 + rax], ecx   ; set bit (atomic-style, clear intent)

.not_at:
    inc     r15d
    ; Update max_cols
    cmp     r15d, [rsp + MAX_COL_TMP]
    jle     .parse_loop
    mov     [rsp + MAX_COL_TMP], r15d
    jmp     .parse_loop

.newline:
    inc     ebx
    xor     r15d, r15d
    ; Update row_byte_offset for next row (hoisted imul)
    add     r9d, BITS_PER_ROW
    jmp     .parse_loop

.parse_done:
    test    r15d, r15d
    jz      .no_extra_row
    inc     ebx
.no_extra_row:
    mov     [rsp + ROWS], ebx
    mov     eax, [rsp + MAX_COL_TMP]
    mov     [rsp + COLS], eax

    ; --- Start timing ---
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; --- Compute neighbor counts with INLINED popcnt logic ---
    ; No function call overhead in hot loop
    ;
    ; Register ownership:
    ;   r8  = grid_bits base
    ;   r9  = counts base
    ;   r10d = rows
    ;   r11d = cols
    ;   ebx = current row
    ;   r15d = current col
    ;   r12d = row_byte_offset (precomputed, updated per row)
    ;   r13d = row_counts_offset (precomputed, updated per row)

    lea     r8, [rel grid_bits]
    lea     r9, [rel counts]
    mov     r10d, [rsp + ROWS]
    mov     r11d, [rsp + COLS]

    xor     ebx, ebx                ; row = 0
    xor     r12d, r12d              ; row_byte_offset = 0
    xor     r13d, r13d              ; row_counts_offset = 0

.count_row_loop:
    cmp     ebx, r10d
    jge     .counts_done
    xor     r15d, r15d              ; col = 0

.count_col_loop:
    cmp     r15d, r11d
    jge     .next_count_row

    ; Check if bit is set in grid_bits
    mov     eax, r15d
    shr     eax, 3                  ; col / 8
    add     eax, r12d               ; + row_byte_offset
    mov     ecx, r15d
    and     ecx, 7
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jnc     .skip_count             ; bit not set

    ; --- INLINED neighbor counting with popcnt ---
    ; Build 8-bit bitmask of present neighbors using setc (branchless)
    ;
    ; This replaces 8 conditional branches with 8 data-dependent setc.
    ; Control dependency → data dependency = better pipelining.
    ;
    ; Register usage within this block:
    ;   eax, ecx, edx = scratch for address calculation
    ;   esi = accumulated neighbor bitmask
    ;   edi = direction index

    xor     esi, esi                ; neighbor bitmask accumulator
    lea     r14, [rel neighbors]

    ; Direction 0: (-1, -1)
    mov     eax, ebx
    dec     eax                     ; nr = row - 1
    mov     edx, r15d
    dec     edx                     ; nc = col - 1
    ; Bounds check: unsigned wrap makes negative large
    cmp     eax, r10d
    setb    cl                      ; cl = 1 if nr < rows (in bounds)
    cmp     edx, r11d
    setb    ch                      ; ch = 1 if nc < cols (in bounds)
    and     cl, ch                  ; cl = both in bounds
    jz      .n0_skip
    ; Fetch bit from grid
    imul    edi, eax, BITS_PER_ROW
    mov     eax, edx
    shr     eax, 3
    add     edi, eax
    and     edx, 7
    movzx   eax, byte [r8 + rdi]
    bt      eax, edx
    setc    cl
    or      sil, cl                 ; bit 0
.n0_skip:

    ; Direction 1: (-1, 0)
    mov     eax, ebx
    dec     eax                     ; nr = row - 1
    mov     edx, r15d               ; nc = col
    cmp     eax, r10d
    setb    cl
    cmp     edx, r11d
    setb    ch
    and     cl, ch
    jz      .n1_skip
    imul    edi, eax, BITS_PER_ROW
    mov     eax, edx
    shr     eax, 3
    add     edi, eax
    and     edx, 7
    movzx   eax, byte [r8 + rdi]
    bt      eax, edx
    setc    cl
    shl     cl, 1
    or      sil, cl                 ; bit 1
.n1_skip:

    ; Direction 2: (-1, +1)
    mov     eax, ebx
    dec     eax
    mov     edx, r15d
    inc     edx
    cmp     eax, r10d
    setb    cl
    cmp     edx, r11d
    setb    ch
    and     cl, ch
    jz      .n2_skip
    imul    edi, eax, BITS_PER_ROW
    mov     eax, edx
    shr     eax, 3
    add     edi, eax
    and     edx, 7
    movzx   eax, byte [r8 + rdi]
    bt      eax, edx
    setc    cl
    shl     cl, 2
    or      sil, cl                 ; bit 2
.n2_skip:

    ; Direction 3: (0, -1)
    mov     eax, ebx                ; nr = row
    mov     edx, r15d
    dec     edx                     ; nc = col - 1
    cmp     eax, r10d
    setb    cl
    cmp     edx, r11d
    setb    ch
    and     cl, ch
    jz      .n3_skip
    imul    edi, eax, BITS_PER_ROW
    mov     eax, edx
    shr     eax, 3
    add     edi, eax
    and     edx, 7
    movzx   eax, byte [r8 + rdi]
    bt      eax, edx
    setc    cl
    shl     cl, 3
    or      sil, cl                 ; bit 3
.n3_skip:

    ; Direction 4: (0, +1)
    mov     eax, ebx
    mov     edx, r15d
    inc     edx
    cmp     eax, r10d
    setb    cl
    cmp     edx, r11d
    setb    ch
    and     cl, ch
    jz      .n4_skip
    imul    edi, eax, BITS_PER_ROW
    mov     eax, edx
    shr     eax, 3
    add     edi, eax
    and     edx, 7
    movzx   eax, byte [r8 + rdi]
    bt      eax, edx
    setc    cl
    shl     cl, 4
    or      sil, cl                 ; bit 4
.n4_skip:

    ; Direction 5: (+1, -1)
    mov     eax, ebx
    inc     eax
    mov     edx, r15d
    dec     edx
    cmp     eax, r10d
    setb    cl
    cmp     edx, r11d
    setb    ch
    and     cl, ch
    jz      .n5_skip
    imul    edi, eax, BITS_PER_ROW
    mov     eax, edx
    shr     eax, 3
    add     edi, eax
    and     edx, 7
    movzx   eax, byte [r8 + rdi]
    bt      eax, edx
    setc    cl
    shl     cl, 5
    or      sil, cl                 ; bit 5
.n5_skip:

    ; Direction 6: (+1, 0)
    mov     eax, ebx
    inc     eax
    mov     edx, r15d
    cmp     eax, r10d
    setb    cl
    cmp     edx, r11d
    setb    ch
    and     cl, ch
    jz      .n6_skip
    imul    edi, eax, BITS_PER_ROW
    mov     eax, edx
    shr     eax, 3
    add     edi, eax
    and     edx, 7
    movzx   eax, byte [r8 + rdi]
    bt      eax, edx
    setc    cl
    shl     cl, 6
    or      sil, cl                 ; bit 6
.n6_skip:

    ; Direction 7: (+1, +1)
    mov     eax, ebx
    inc     eax
    mov     edx, r15d
    inc     edx
    cmp     eax, r10d
    setb    cl
    cmp     edx, r11d
    setb    ch
    and     cl, ch
    jz      .n7_skip
    imul    edi, eax, BITS_PER_ROW
    mov     eax, edx
    shr     eax, 3
    add     edi, eax
    and     edx, 7
    movzx   eax, byte [r8 + rdi]
    bt      eax, edx
    setc    cl
    shl     cl, 7
    or      sil, cl                 ; bit 7
.n7_skip:

    ; --- popcnt to count all neighbors at once ---
    popcnt  eax, esi

    ; Store count
    mov     edx, r13d               ; row_counts_offset
    add     edx, r15d               ; + col
    mov     [r9 + rdx], al

.skip_count:
    inc     r15d
    jmp     .count_col_loop

.next_count_row:
    inc     ebx
    add     r12d, BITS_PER_ROW      ; row_byte_offset += BITS_PER_ROW
    add     r13d, MAX_COLS          ; row_counts_offset += MAX_COLS
    jmp     .count_row_loop

.counts_done:
    ; --- Part 1: Full SIMD with pmovmskb + popcnt ---
    ;
    ; For each 16-cell batch:
    ; 1. Load 16 count bytes
    ; 2. pcmpgtb against threshold (3) → mask of cells with count > 3
    ; 3. Load 2 bytes from grid_bits, expand to 16-byte mask
    ; 4. AND grid mask with (counts <= 3) mask
    ; 5. pmovmskb → 16-bit mask in GP register
    ; 6. popcnt to count accessible cells in batch
    ;
    ; This processes 16 cells with ~6-8 instructions, no pextrb loops.

    xor     r12d, r12d              ; accessible count
    lea     r8, [rel grid_bits]
    lea     r9, [rel counts]
    mov     r10d, [rsp + ROWS]
    mov     r11d, [rsp + COLS]

    movdqa  xmm1, [rel threshold_vec]   ; threshold vector (3s)

    xor     ebx, ebx                ; row
    xor     r13d, r13d              ; row_counts_offset
    xor     r14d, r14d              ; row_bits_offset

.p1_row_loop:
    cmp     ebx, r10d
    jge     .p1_done

    xor     r15d, r15d              ; col

.p1_col_loop:
    ; Check if we can do SIMD (16 cells remaining)
    mov     eax, r11d
    sub     eax, r15d               ; remaining cols
    cmp     eax, 16
    jl      .p1_scalar              ; fewer than 16 remaining

    ; --- Full SIMD path ---
    ; Load 16 counts
    mov     eax, r13d
    add     eax, r15d               ; counts offset
    movdqu  xmm0, [r9 + rax]

    ; Compare: xmm2 = (counts > 3) ? 0xFF : 0x00
    movdqa  xmm2, xmm0
    pcmpgtb xmm2, xmm1

    ; Invert: xmm2 = (counts <= 3) ? 0xFF : 0x00
    pcmpeqb xmm3, xmm3              ; all 1s
    pxor    xmm2, xmm3              ; invert

    ; Expand 16 grid bits to 16 bytes in xmm4 (branchless SIMD)
    ; grid_bits for these 16 cells: 2 consecutive bytes starting at
    ; row_bits_offset + col/8
    mov     eax, r15d
    shr     eax, 3                  ; col / 8
    add     eax, r14d               ; + row_bits_offset
    movzx   ecx, word [r8 + rax]    ; load 2 bytes (16 bits)

    ; Branchless bit expansion using pshufb + pand + pcmpeqb:
    ; 1. movd the 16-bit value into xmm4
    ; 2. pshufb to broadcast: byte 0 → positions 0-7, byte 1 → positions 8-15
    ; 3. pand with bit_masks to isolate each bit
    ; 4. pcmpeqb with zero → 0xFF where bit was 0, 0x00 where bit was 1
    ; 5. Invert to get 0xFF where bit was set

    movd    xmm4, ecx               ; load 16 bits into low dword
    pshufb  xmm4, [rel broadcast_shuf]  ; broadcast bytes
    pand    xmm4, [rel bit_masks]   ; isolate each bit position
    ; Now xmm4[i] = (grid_bits >> (i%8)) & 1, for appropriate byte
    ; Non-zero means bit was set

    ; Convert non-zero to 0xFF: compare with zero, invert
    pxor    xmm5, xmm5              ; zero vector
    pcmpeqb xmm4, xmm5              ; xmm4 = 0xFF where was 0, 0x00 where non-zero
    pcmpeqb xmm5, xmm5              ; xmm5 = all 0xFF
    pxor    xmm4, xmm5              ; invert: 0xFF where bit set, 0x00 where not

    ; AND: accessible = grid_present AND (count <= 3)
    pand    xmm4, xmm2

    ; pmovmskb: extract high bit of each byte → 16-bit mask
    pmovmskb eax, xmm4

    ; popcnt: count accessible cells in this batch
    popcnt  eax, eax
    add     r12d, eax

    add     r15d, 16
    jmp     .p1_col_loop

.p1_scalar:
    ; Scalar fallback for remaining cells
    cmp     r15d, r11d
    jge     .p1_next_row

    ; Check grid bit
    mov     eax, r15d
    shr     eax, 3
    add     eax, r14d               ; + row_bits_offset
    mov     ecx, r15d
    and     ecx, 7
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jnc     .p1_scalar_skip

    ; Check count < 4
    mov     eax, r13d
    add     eax, r15d
    movzx   ecx, byte [r9 + rax]
    cmp     ecx, 4
    jge     .p1_scalar_skip
    inc     r12d

.p1_scalar_skip:
    inc     r15d
    jmp     .p1_scalar

.p1_next_row:
    inc     ebx
    add     r13d, MAX_COLS          ; row_counts_offset
    add     r14d, BITS_PER_ROW      ; row_bits_offset
    jmp     .p1_row_loop

.p1_done:
    mov     [rsp + ACCESSIBLE], r12d

    ; --- Part 2: BFS removal with packed queue ---
    ; Queue entries: (row << 8) | col in 16-bit words
    ; This halves memory traffic vs separate dwords

    ; Clear removed_bits
    lea     rdi, [rel removed_bits]
    xor     eax, eax
    mov     ecx, MAX_CELLS / 8
    rep     stosb

    ; Initialize queue with accessible cells
    lea     r8, [rel queue]
    xor     r11d, r11d              ; queue_tail
    lea     r9, [rel grid_bits]
    lea     r10, [rel counts]
    mov     r14d, [rsp + ROWS]
    mov     ecx, [rsp + COLS]
    mov     [rsp + MAX_COL_TMP], ecx

    xor     ebx, ebx                ; row
    xor     r12d, r12d              ; row_bits_offset (hoisted)
    xor     r13d, r13d              ; row_counts_offset (hoisted)

.init_q_row:
    cmp     ebx, r14d
    jge     .init_q_done
    xor     r15d, r15d              ; col

.init_q_col:
    cmp     r15d, [rsp + MAX_COL_TMP]
    jge     .init_q_next_row

    ; Check grid bit (using hoisted row_bits_offset)
    mov     eax, r15d
    shr     eax, 3
    add     eax, r12d               ; + row_bits_offset
    mov     ecx, r15d
    and     ecx, 7
    movzx   edx, byte [r9 + rax]
    bt      edx, ecx
    jnc     .init_q_skip

    ; Check count < 4 (using hoisted row_counts_offset)
    mov     eax, r13d
    add     eax, r15d
    movzx   ecx, byte [r10 + rax]
    cmp     ecx, 4
    jge     .init_q_skip

    ; Add to queue as packed (row << 8) | col
    mov     eax, ebx
    shl     eax, 8
    or      eax, r15d
    mov     [r8 + r11*2], ax        ; 16-bit entry
    inc     r11d

.init_q_skip:
    inc     r15d
    jmp     .init_q_col

.init_q_next_row:
    inc     ebx
    add     r12d, BITS_PER_ROW
    add     r13d, MAX_COLS
    jmp     .init_q_row

.init_q_done:
    xor     r12d, r12d              ; removed_count
    xor     r13d, r13d              ; queue_head

    ; BFS loop
.bfs_loop:
    cmp     r13d, r11d
    jge     .bfs_done

    ; Pop from queue (packed entry)
    lea     r8, [rel queue]
    movzx   eax, word [r8 + r13*2]
    inc     r13d

    ; Unpack: row = ax >> 8, col = ax & 0xFF
    mov     ebx, eax
    shr     ebx, 8                  ; row
    and     eax, 0xFF
    mov     r15d, eax               ; col

    ; Precompute row_bits_offset for this cell (used multiple times)
    imul    r14d, ebx, BITS_PER_ROW

    ; Check if already removed (bit in removed_bits)
    mov     eax, r15d
    shr     eax, 3
    add     eax, r14d               ; byte offset
    mov     ecx, r15d
    and     ecx, 7                  ; bit position

    lea     rdx, [rel removed_bits]
    movzx   esi, byte [rdx + rax]
    bt      esi, ecx
    jc      .bfs_loop               ; already removed

    ; Mark as removed using bts (clearer than shl/or)
    bts     dword [rdx + rax], ecx
    inc     r12d

    ; Update neighbors - inlined for performance
    ; We reuse r14d (row_bits_offset) where possible

    ; Neighbor 0: (row-1, col-1)
    mov     eax, ebx
    dec     eax                     ; nr = row - 1
    mov     esi, r15d
    dec     esi                     ; nc = col - 1
    ; Bounds check (unsigned wrap: negative becomes large)
    cmp     eax, [rsp + ROWS]
    jae     .bfs_n1
    cmp     esi, [rsp + MAX_COL_TMP]
    jae     .bfs_n1
    ; Process this neighbor
    mov     edi, eax                ; save nr
    mov     r10d, esi               ; save nc

    imul    eax, edi, BITS_PER_ROW
    mov     ecx, r10d
    shr     ecx, 3
    add     eax, ecx
    mov     ecx, r10d
    and     ecx, 7

    ; Check grid bit
    lea     r8, [rel grid_bits]
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jnc     .bfs_n1

    ; Check removed bit
    lea     r8, [rel removed_bits]
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jc      .bfs_n1

    ; Decrement count
    imul    eax, edi, MAX_COLS
    add     eax, r10d
    lea     r8, [rel counts]
    movzx   edx, byte [r8 + rax]
    dec     edx
    mov     [r8 + rax], dl

    ; If count < 4, enqueue
    cmp     edx, 4
    jge     .bfs_n1
    mov     eax, edi
    shl     eax, 8
    or      eax, r10d
    lea     r8, [rel queue]
    mov     [r8 + r11*2], ax
    inc     r11d

.bfs_n1:
    ; Neighbor 1: (row-1, col)
    mov     eax, ebx
    dec     eax
    mov     esi, r15d
    cmp     eax, [rsp + ROWS]
    jae     .bfs_n2
    cmp     esi, [rsp + MAX_COL_TMP]
    jae     .bfs_n2
    mov     edi, eax
    mov     r10d, esi

    imul    eax, edi, BITS_PER_ROW
    mov     ecx, r10d
    shr     ecx, 3
    add     eax, ecx
    mov     ecx, r10d
    and     ecx, 7

    lea     r8, [rel grid_bits]
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jnc     .bfs_n2

    lea     r8, [rel removed_bits]
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jc      .bfs_n2

    imul    eax, edi, MAX_COLS
    add     eax, r10d
    lea     r8, [rel counts]
    movzx   edx, byte [r8 + rax]
    dec     edx
    mov     [r8 + rax], dl

    cmp     edx, 4
    jge     .bfs_n2
    mov     eax, edi
    shl     eax, 8
    or      eax, r10d
    lea     r8, [rel queue]
    mov     [r8 + r11*2], ax
    inc     r11d

.bfs_n2:
    ; Neighbor 2: (row-1, col+1)
    mov     eax, ebx
    dec     eax
    mov     esi, r15d
    inc     esi
    cmp     eax, [rsp + ROWS]
    jae     .bfs_n3
    cmp     esi, [rsp + MAX_COL_TMP]
    jae     .bfs_n3
    mov     edi, eax
    mov     r10d, esi

    imul    eax, edi, BITS_PER_ROW
    mov     ecx, r10d
    shr     ecx, 3
    add     eax, ecx
    mov     ecx, r10d
    and     ecx, 7

    lea     r8, [rel grid_bits]
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jnc     .bfs_n3

    lea     r8, [rel removed_bits]
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jc      .bfs_n3

    imul    eax, edi, MAX_COLS
    add     eax, r10d
    lea     r8, [rel counts]
    movzx   edx, byte [r8 + rax]
    dec     edx
    mov     [r8 + rax], dl

    cmp     edx, 4
    jge     .bfs_n3
    mov     eax, edi
    shl     eax, 8
    or      eax, r10d
    lea     r8, [rel queue]
    mov     [r8 + r11*2], ax
    inc     r11d

.bfs_n3:
    ; Neighbor 3: (row, col-1)
    mov     eax, ebx
    mov     esi, r15d
    dec     esi
    cmp     eax, [rsp + ROWS]
    jae     .bfs_n4
    cmp     esi, [rsp + MAX_COL_TMP]
    jae     .bfs_n4
    mov     edi, eax
    mov     r10d, esi

    imul    eax, edi, BITS_PER_ROW
    mov     ecx, r10d
    shr     ecx, 3
    add     eax, ecx
    mov     ecx, r10d
    and     ecx, 7

    lea     r8, [rel grid_bits]
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jnc     .bfs_n4

    lea     r8, [rel removed_bits]
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jc      .bfs_n4

    imul    eax, edi, MAX_COLS
    add     eax, r10d
    lea     r8, [rel counts]
    movzx   edx, byte [r8 + rax]
    dec     edx
    mov     [r8 + rax], dl

    cmp     edx, 4
    jge     .bfs_n4
    mov     eax, edi
    shl     eax, 8
    or      eax, r10d
    lea     r8, [rel queue]
    mov     [r8 + r11*2], ax
    inc     r11d

.bfs_n4:
    ; Neighbor 4: (row, col+1)
    mov     eax, ebx
    mov     esi, r15d
    inc     esi
    cmp     eax, [rsp + ROWS]
    jae     .bfs_n5
    cmp     esi, [rsp + MAX_COL_TMP]
    jae     .bfs_n5
    mov     edi, eax
    mov     r10d, esi

    imul    eax, edi, BITS_PER_ROW
    mov     ecx, r10d
    shr     ecx, 3
    add     eax, ecx
    mov     ecx, r10d
    and     ecx, 7

    lea     r8, [rel grid_bits]
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jnc     .bfs_n5

    lea     r8, [rel removed_bits]
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jc      .bfs_n5

    imul    eax, edi, MAX_COLS
    add     eax, r10d
    lea     r8, [rel counts]
    movzx   edx, byte [r8 + rax]
    dec     edx
    mov     [r8 + rax], dl

    cmp     edx, 4
    jge     .bfs_n5
    mov     eax, edi
    shl     eax, 8
    or      eax, r10d
    lea     r8, [rel queue]
    mov     [r8 + r11*2], ax
    inc     r11d

.bfs_n5:
    ; Neighbor 5: (row+1, col-1)
    mov     eax, ebx
    inc     eax
    mov     esi, r15d
    dec     esi
    cmp     eax, [rsp + ROWS]
    jae     .bfs_n6
    cmp     esi, [rsp + MAX_COL_TMP]
    jae     .bfs_n6
    mov     edi, eax
    mov     r10d, esi

    imul    eax, edi, BITS_PER_ROW
    mov     ecx, r10d
    shr     ecx, 3
    add     eax, ecx
    mov     ecx, r10d
    and     ecx, 7

    lea     r8, [rel grid_bits]
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jnc     .bfs_n6

    lea     r8, [rel removed_bits]
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jc      .bfs_n6

    imul    eax, edi, MAX_COLS
    add     eax, r10d
    lea     r8, [rel counts]
    movzx   edx, byte [r8 + rax]
    dec     edx
    mov     [r8 + rax], dl

    cmp     edx, 4
    jge     .bfs_n6
    mov     eax, edi
    shl     eax, 8
    or      eax, r10d
    lea     r8, [rel queue]
    mov     [r8 + r11*2], ax
    inc     r11d

.bfs_n6:
    ; Neighbor 6: (row+1, col)
    mov     eax, ebx
    inc     eax
    mov     esi, r15d
    cmp     eax, [rsp + ROWS]
    jae     .bfs_n7
    cmp     esi, [rsp + MAX_COL_TMP]
    jae     .bfs_n7
    mov     edi, eax
    mov     r10d, esi

    imul    eax, edi, BITS_PER_ROW
    mov     ecx, r10d
    shr     ecx, 3
    add     eax, ecx
    mov     ecx, r10d
    and     ecx, 7

    lea     r8, [rel grid_bits]
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jnc     .bfs_n7

    lea     r8, [rel removed_bits]
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jc      .bfs_n7

    imul    eax, edi, MAX_COLS
    add     eax, r10d
    lea     r8, [rel counts]
    movzx   edx, byte [r8 + rax]
    dec     edx
    mov     [r8 + rax], dl

    cmp     edx, 4
    jge     .bfs_n7
    mov     eax, edi
    shl     eax, 8
    or      eax, r10d
    lea     r8, [rel queue]
    mov     [r8 + r11*2], ax
    inc     r11d

.bfs_n7:
    ; Neighbor 7: (row+1, col+1)
    mov     eax, ebx
    inc     eax
    mov     esi, r15d
    inc     esi
    cmp     eax, [rsp + ROWS]
    jae     .bfs_next
    cmp     esi, [rsp + MAX_COL_TMP]
    jae     .bfs_next
    mov     edi, eax
    mov     r10d, esi

    imul    eax, edi, BITS_PER_ROW
    mov     ecx, r10d
    shr     ecx, 3
    add     eax, ecx
    mov     ecx, r10d
    and     ecx, 7

    lea     r8, [rel grid_bits]
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jnc     .bfs_next

    lea     r8, [rel removed_bits]
    movzx   edx, byte [r8 + rax]
    bt      edx, ecx
    jc      .bfs_next

    imul    eax, edi, MAX_COLS
    add     eax, r10d
    lea     r8, [rel counts]
    movzx   edx, byte [r8 + rax]
    dec     edx
    mov     [r8 + rax], dl

    cmp     edx, 4
    jge     .bfs_next
    mov     eax, edi
    shl     eax, 8
    or      eax, r10d
    lea     r8, [rel queue]
    mov     [r8 + r11*2], ax
    inc     r11d

.bfs_next:
    jmp     .bfs_loop

.bfs_done:
    ; --- End timing ---
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts1]
    call    clock_gettime

    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since
    cvtsi2sd xmm0, rax
    divsd   xmm0, [rel one_million]

    ; --- Output ---
    lea     rdi, [rel fmt_out]
    mov     esi, [rsp + ACCESSIBLE]
    mov     edx, r12d
    mov     eax, 1
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

section .note.GNU-stack noalloc noexec nowrite progbits
