; Day 12: Polyomino Packing with Bitboard Operations
; Hand-written x86-64 assembly - register-centric design
;
; DESIGN: Caller-allocated scratch frame (library-grade pattern)
; ============================================================
; All temporary/scratch data lives in a stack-allocated frame, not BSS.
; This makes functions reentrant, thread-safe, and composable.
;
; r15 = scratch frame base pointer (preserved across all calls)
; All scratch-using functions receive context through r15.
;
; This is the same pattern used by:
;   - BLAS/LAPACK (workspace pointers)
;   - OpenSSL (context structures)
;   - Game engines (per-frame arenas)

global main
extern printf
extern perror
extern read_file_all
extern sort_u64
extern clock_gettime

%define BUF_SIZE 65536
%define MAX_SHAPES 8
%define MAX_ORIENT 8
%define SEARCH_AREA_LIMIT 64
%define SEARCH_PIECE_LIMIT 10

;------------------------------------------------------------------------------
; Scratch Frame Layout (r15 = base)
;------------------------------------------------------------------------------
; All temporary data that was previously in BSS is now stack-allocated.
; Functions access via [r15 + SCR_xxx]. This enables:
;   - Reentrancy: multiple concurrent solves possible
;   - Thread-safety: each thread has its own stack frame
;   - Cache locality: scratch data is near other stack data
;
%define SCR_DP_POSS        0          ; 64 bytes - DP bitset for parity
%define SCR_DP_NEXT        64         ; 64 bytes - DP temp for shift
%define SCR_TEMP_GRID1     128        ; 16 bytes - orientation generation
%define SCR_TEMP_GRID2     144        ; 16 bytes - orientation generation
%define SCR_TEMP_COUNTS    160        ; 32 bytes - piece counts per shape
%define SCR_TEMP_REMAIN    192        ; 32 bytes - remaining pieces
%define SCR_PIECE_ORDER    224        ; 40 bytes - DFS piece ordering
%define SCR_PLACE_COUNT    264        ; 32 bytes - placement count per shape
%define SCR_PLACEMENTS     296        ; 32768 bytes - placement bitmasks
; Total: 296 + 32768 = 33064 bytes
; Aligned to 16 bytes for SSE compatibility
%define SCRATCH_SIZE       33072

section .data
input_file:    db "input.txt", 0
fmt_out:       db "regions_that_fit=%d elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0

%define CLOCK_MONOTONIC 1

; Lookup tables for 3x3 grid index -> row/col (eliminates div in hot loop)
row_table:     db 0,0,0,1,1,1,2,2,2
col_table:     db 0,1,2,0,1,2,0,1,2

; Bitmask lookup for branchless grid_to_mask
bit_masks:     dd 1,2,4,8,16,32,64,128,256

section .bss
align 16
;------------------------------------------------------------------------------
; Persistent data (not scratch - stays in BSS)
;------------------------------------------------------------------------------
; This data is either:
;   - I/O related (file_buf)
;   - Computed once at startup and read-only thereafter (shape_*, orient_*)
;
file_buf:      resb BUF_SIZE

; Shape data (computed during parsing, read-only during solve)
shape_count:   resd 1
shape_area:    resd MAX_SHAPES
shape_diff:    resd MAX_SHAPES
shape_grid:    resb MAX_SHAPES * 9

; Orientation masks (precomputed, read-only during solve)
orient_masks:  resw MAX_SHAPES * MAX_ORIENT
orient_count:  resd MAX_SHAPES

; NOTE: All scratch/temporary data that was here has been moved
; to the caller-allocated scratch frame (see SCR_* offsets above)

section .text

;------------------------------------------------------------------------------
; rotate90: Rotate 3x3 grid 90 degrees clockwise (no stack)
; Input: rdi = src, rsi = dst
;------------------------------------------------------------------------------
rotate90:
    movzx   eax, byte [rdi]
    mov     [rsi + 2], al
    movzx   eax, byte [rdi + 1]
    mov     [rsi + 5], al
    movzx   eax, byte [rdi + 2]
    mov     [rsi + 8], al
    movzx   eax, byte [rdi + 3]
    mov     [rsi + 1], al
    movzx   eax, byte [rdi + 4]
    mov     [rsi + 4], al
    movzx   eax, byte [rdi + 5]
    mov     [rsi + 7], al
    movzx   eax, byte [rdi + 6]
    mov     [rsi], al
    movzx   eax, byte [rdi + 7]
    mov     [rsi + 3], al
    movzx   eax, byte [rdi + 8]
    mov     [rsi + 6], al
    ret

;------------------------------------------------------------------------------
; flip_h: Horizontal flip of 3x3 grid (no stack)
;------------------------------------------------------------------------------
flip_h:
    movzx   eax, byte [rdi]
    mov     [rsi + 2], al
    movzx   eax, byte [rdi + 1]
    mov     [rsi + 1], al
    movzx   eax, byte [rdi + 2]
    mov     [rsi], al
    movzx   eax, byte [rdi + 3]
    mov     [rsi + 5], al
    movzx   eax, byte [rdi + 4]
    mov     [rsi + 4], al
    movzx   eax, byte [rdi + 5]
    mov     [rsi + 3], al
    movzx   eax, byte [rdi + 6]
    mov     [rsi + 8], al
    movzx   eax, byte [rdi + 7]
    mov     [rsi + 7], al
    movzx   eax, byte [rdi + 8]
    mov     [rsi + 6], al
    ret

;------------------------------------------------------------------------------
; grid_to_mask: Convert 3x3 grid to 9-bit mask (branchless)
; Uses comparison + mask lookup to avoid branches and BTS
;------------------------------------------------------------------------------
grid_to_mask:
    xor     eax, eax
    lea     rcx, [rel bit_masks]
    ; Unrolled branchless: cmp sets ZF, sete gives 0/1, AND with mask
    %assign i 0
    %rep 9
        xor     edx, edx
        cmp     byte [rdi + i], '#'
        sete    dl                          ; dl = 1 if '#', 0 otherwise
        imul    edx, [rcx + i*4]            ; edx = mask if set, 0 if not
        or      eax, edx
    %assign i i+1
    %endrep
    ret

;------------------------------------------------------------------------------
; shl_512: Left shift 512 bits (8 qwords) by r8d bits (r8d < 64)
; Input: rsi = src, rdi = dst, r8d = shift amount
; Clobbers: rax, rdx, rcx
;------------------------------------------------------------------------------
shl_512:
    mov     ecx, 64
    sub     ecx, r8d                     ; rcx = 64 - shift
    ; q7 (highest)
    mov     rax, [rsi + 56]
    shlx    rax, rax, r8
    mov     rdx, [rsi + 48]
    shrx    rdx, rdx, rcx
    or      rax, rdx
    mov     [rdi + 56], rax
    ; q6
    mov     rax, [rsi + 48]
    shlx    rax, rax, r8
    mov     rdx, [rsi + 40]
    shrx    rdx, rdx, rcx
    or      rax, rdx
    mov     [rdi + 48], rax
    ; q5
    mov     rax, [rsi + 40]
    shlx    rax, rax, r8
    mov     rdx, [rsi + 32]
    shrx    rdx, rdx, rcx
    or      rax, rdx
    mov     [rdi + 40], rax
    ; q4
    mov     rax, [rsi + 32]
    shlx    rax, rax, r8
    mov     rdx, [rsi + 24]
    shrx    rdx, rdx, rcx
    or      rax, rdx
    mov     [rdi + 32], rax
    ; q3
    mov     rax, [rsi + 24]
    shlx    rax, rax, r8
    mov     rdx, [rsi + 16]
    shrx    rdx, rdx, rcx
    or      rax, rdx
    mov     [rdi + 24], rax
    ; q2
    mov     rax, [rsi + 16]
    shlx    rax, rax, r8
    mov     rdx, [rsi + 8]
    shrx    rdx, rdx, rcx
    or      rax, rdx
    mov     [rdi + 16], rax
    ; q1
    mov     rax, [rsi + 8]
    shlx    rax, rax, r8
    mov     rdx, [rsi]
    shrx    rdx, rdx, rcx
    or      rax, rdx
    mov     [rdi + 8], rax
    ; q0 (lowest) - no carry from below
    mov     rax, [rsi]
    shlx    rax, rax, r8
    mov     [rdi], rax
    ret

;------------------------------------------------------------------------------
; shr_512: Right shift 512 bits (8 qwords) by r8d bits (r8d < 64)
; Input: rsi = src, rdi = dst, r8d = shift amount
; Clobbers: rax, rdx, rcx
;------------------------------------------------------------------------------
shr_512:
    mov     ecx, 64
    sub     ecx, r8d                     ; rcx = 64 - shift
    ; q0 (lowest)
    mov     rax, [rsi]
    shrx    rax, rax, r8
    mov     rdx, [rsi + 8]
    shlx    rdx, rdx, rcx
    or      rax, rdx
    mov     [rdi], rax
    ; q1
    mov     rax, [rsi + 8]
    shrx    rax, rax, r8
    mov     rdx, [rsi + 16]
    shlx    rdx, rdx, rcx
    or      rax, rdx
    mov     [rdi + 8], rax
    ; q2
    mov     rax, [rsi + 16]
    shrx    rax, rax, r8
    mov     rdx, [rsi + 24]
    shlx    rdx, rdx, rcx
    or      rax, rdx
    mov     [rdi + 16], rax
    ; q3
    mov     rax, [rsi + 24]
    shrx    rax, rax, r8
    mov     rdx, [rsi + 32]
    shlx    rdx, rdx, rcx
    or      rax, rdx
    mov     [rdi + 24], rax
    ; q4
    mov     rax, [rsi + 32]
    shrx    rax, rax, r8
    mov     rdx, [rsi + 40]
    shlx    rdx, rdx, rcx
    or      rax, rdx
    mov     [rdi + 32], rax
    ; q5
    mov     rax, [rsi + 40]
    shrx    rax, rax, r8
    mov     rdx, [rsi + 48]
    shlx    rdx, rdx, rcx
    or      rax, rdx
    mov     [rdi + 40], rax
    ; q6
    mov     rax, [rsi + 48]
    shrx    rax, rax, r8
    mov     rdx, [rsi + 56]
    shlx    rdx, rdx, rcx
    or      rax, rdx
    mov     [rdi + 48], rax
    ; q7 (highest) - no carry from above
    mov     rax, [rsi + 56]
    shrx    rax, rax, r8
    mov     [rdi + 56], rax
    ret

;------------------------------------------------------------------------------
; generate_orientations: Generate unique orientation masks
; Uses scratch frame at r15 for temp grids (reentrant)
; Input: rdi = shape grid, rsi = output masks, r15 = scratch base
; Output: eax = count
;------------------------------------------------------------------------------
generate_orientations:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    rbp
    ; r15 is scratch pointer - preserved by caller

    mov     r12, rdi                     ; shape grid
    mov     r13, rsi                     ; output masks
    xor     r14d, r14d                   ; orientation count

    ; Copy initial grid to scratch temp_grid1
    lea     rdi, [r15 + SCR_TEMP_GRID1]
    mov     rax, [r12]
    mov     [rdi], rax
    movzx   eax, byte [r12 + 8]
    mov     [rdi + 8], al

    xor     ebp, ebp                     ; rotation counter (was r15d)
.rot_loop:
    lea     rdi, [r15 + SCR_TEMP_GRID1]
    call    grid_to_mask
    mov     ebx, eax

    ; Check if duplicate
    xor     ecx, ecx
.check_rot:
    cmp     ecx, r14d
    jge     .add_rot
    movzx   edx, word [r13 + rcx*2]
    cmp     edx, ebx
    je      .skip_rot
    inc     ecx
    jmp     .check_rot

.add_rot:
    mov     [r13 + r14*2], bx
    inc     r14d

.skip_rot:
    ; Generate flipped version
    lea     rdi, [r15 + SCR_TEMP_GRID1]
    lea     rsi, [r15 + SCR_TEMP_GRID2]
    call    flip_h

    lea     rdi, [r15 + SCR_TEMP_GRID2]
    call    grid_to_mask
    mov     ebx, eax

    xor     ecx, ecx
.check_flip:
    cmp     ecx, r14d
    jge     .add_flip
    movzx   edx, word [r13 + rcx*2]
    cmp     edx, ebx
    je      .skip_flip
    inc     ecx
    jmp     .check_flip

.add_flip:
    mov     [r13 + r14*2], bx
    inc     r14d

.skip_flip:
    ; Rotate for next iteration
    lea     rdi, [r15 + SCR_TEMP_GRID1]
    lea     rsi, [r15 + SCR_TEMP_GRID2]
    call    rotate90
    ; Copy back
    lea     rdi, [r15 + SCR_TEMP_GRID1]
    lea     rsi, [r15 + SCR_TEMP_GRID2]
    mov     rax, [rsi]
    mov     [rdi], rax
    movzx   eax, byte [rsi + 8]
    mov     [rdi + 8], al

    inc     ebp
    cmp     ebp, 4
    jb      .rot_loop

    mov     eax, r14d
    pop     rbp
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; generate_placements: Generate placements for one orientation
; Register-only inner loop, minimal stack
; Input: edi=width, esi=height, edx=mask, rcx=output, r8d=count, r9d=capacity
; Output: eax = new count
;------------------------------------------------------------------------------
generate_placements:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    push    rbp

    mov     r10d, edi                    ; width
    mov     r11d, esi                    ; height
    mov     r12d, edx                    ; orient mask
    mov     r13, rcx                     ; output array
    mov     r14d, r8d                    ; current count
    mov     r15d, r9d                    ; capacity

    ; Calculate max y and x offsets
    mov     eax, r11d
    sub     eax, 2                       ; max_y = height - 2 (inclusive, for 3x3)
    mov     ebp, eax
    mov     eax, r10d
    sub     eax, 2                       ; max_x = width - 2
    mov     r9d, eax                     ; r9d = max_x

    ; Preload lookup table addresses (avoids RIP-relative + index issues)
    lea     rax, [rel row_table]
    mov     [rsp - 8], rax               ; use red zone for row_table ptr
    lea     rax, [rel col_table]
    mov     [rsp - 16], rax              ; use red zone for col_table ptr

    ; Iterate y from 0 to max_y
    xor     ebx, ebx                     ; y
.y_loop:
    cmp     ebx, ebp
    jg      .done

    xor     ecx, ecx                     ; x
.x_loop:
    cmp     ecx, r9d
    jg      .next_y

    ; Build placement mask from orient mask
    ; For each bit i in orient_mask, set bit at (y + row[i])*width + (x + col[i])
    ; Uses lookup tables instead of div for row/col calculation
    xor     r8, r8                       ; placement mask
    xor     edi, edi                     ; bit index
.build_mask:
    cmp     edi, 9
    jge     .check_valid

    bt      r12d, edi
    jnc     .next_bit

    ; Calculate position using lookup tables (no div!)
    ; row = row_table[i], col = col_table[i]
    mov     rax, [rsp - 8]               ; row_table ptr
    movzx   eax, byte [rax + rdi]
    mov     rdx, [rsp - 16]              ; col_table ptr
    movzx   edx, byte [rdx + rdi]
    add     eax, ebx                     ; row + y
    imul    eax, r10d                    ; * width
    add     eax, ecx                     ; + x
    add     eax, edx                     ; + col
    bts     r8, rax

.next_bit:
    inc     edi
    jmp     .build_mask

.check_valid:
    ; Skip duplicate check - we'll sort+dedup after all placements generated
    ; Just add if capacity allows
    cmp     r14d, r15d
    jge     .skip_add
    mov     [r13 + r14*8], r8
    inc     r14d
.skip_add:
    inc     ecx
    jmp     .x_loop

.next_y:
    inc     ebx
    jmp     .y_loop

.done:
    mov     eax, r14d
    pop     rbp
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; reachable_diff: Check if parity difference is achievable via bitset DP
; Uses scratch frame at r15 for DP bitsets (reentrant)
; Uses 512-bit bitset (8 qwords) with shift+OR operations - no inner loops
; Input: edi=total_area, esi=shape_count, rdx=counts[], rcx=areas[], r8=diffs[]
;        r15 = scratch base
; Output: eax = 1 if reachable, 0 otherwise
;------------------------------------------------------------------------------
reachable_diff:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    rbp
    ; r15 is scratch pointer - preserved by caller

    mov     r10d, edi                    ; total_area
    mov     r11d, esi                    ; shape_count
    mov     r12, rdx                     ; counts[]
    mov     r13, rcx                     ; areas[]
    mov     r14, r8                      ; diffs[]

    ; Calculate used_area and max_diff
    xor     eax, eax                     ; used_area
    xor     ebx, ebx                     ; max_diff
    xor     ecx, ecx
.calc_loop:
    cmp     ecx, r11d
    jge     .calc_done
    mov     edx, [r12 + rcx*4]
    mov     esi, [r13 + rcx*4]
    imul    esi, edx
    add     eax, esi
    mov     esi, [r14 + rcx*4]
    ; abs(diff)
    mov     edi, esi
    sar     edi, 31
    xor     esi, edi
    sub     esi, edi
    imul    esi, edx
    add     ebx, esi
    inc     ecx
    jmp     .calc_loop

.calc_done:
    mov     r8d, eax                     ; r8d = used_area
    mov     r9d, ebx                     ; r9d = max_diff

    ; Check used_area <= total_area
    cmp     eax, r10d
    jg      .fail

    ; Calculate bounds
    mov     eax, r10d
    inc     eax
    shr     eax, 1                       ; black = (total_area + 1) / 2
    mov     ecx, r10d
    shr     ecx, 1                       ; white = total_area / 2
    mov     edx, r8d                     ; used_area

    ; lower = used_area - 2*white
    mov     esi, edx
    mov     edi, ecx
    shl     edi, 1
    sub     esi, edi                     ; lower in esi

    ; upper = 2*black - used_area
    mov     edi, eax
    shl     edi, 1
    sub     edi, edx                     ; upper in edi

    ; If max_diff == 0, check if 0 is in range
    test    r9d, r9d
    jnz     .do_dp
    cmp     esi, 0
    jg      .fail
    cmp     edi, 0
    jl      .fail
    jmp     .success

.do_dp:
    ; If max_diff > 250, skip DP (bitset only holds 512 bits)
    cmp     r9d, 250
    jg      .success

    ; Save bounds for later (use proper stack, not red zone)
    push    rsi                          ; lower
    push    rdi                          ; upper

    ; ebp = offset = max_diff (center position in bitset)
    mov     ebp, r9d

    ; Clear dp_poss (8 qwords) in scratch
    lea     rdi, [r15 + SCR_DP_POSS]
    xor     eax, eax
    mov     [rdi], rax
    mov     [rdi + 8], rax
    mov     [rdi + 16], rax
    mov     [rdi + 24], rax
    mov     [rdi + 32], rax
    mov     [rdi + 40], rax
    mov     [rdi + 48], rax
    mov     [rdi + 56], rax

    ; Set bit at position offset (center)
    ; bit_index = offset, qword = offset / 64, bit = offset % 64
    mov     eax, ebp
    mov     ecx, ebp
    shr     eax, 6                       ; qword index
    and     ecx, 63                      ; bit index
    mov     rdx, 1
    shlx    rdx, rdx, rcx                ; bit mask
    or      [rdi + rax*8], rdx           ; set bit

    ; DP over shapes using bitset shift+OR
    xor     ebx, ebx                     ; shape index (was r15d)
.dp_shape:
    cmp     ebx, r11d
    jge     .dp_done

    mov     eax, [r14 + rbx*4]
    mov     ecx, eax
    sar     ecx, 31
    xor     eax, ecx
    sub     eax, ecx                     ; abs(diff) = step
    test    eax, eax
    jz      .dp_next_shape

    mov     r8d, eax                     ; r8d = step
    mov     ecx, [r12 + rbx*4]           ; count
    test    ecx, ecx
    jz      .dp_next_shape
    mov     r9d, ecx                     ; r9d = count

.dp_iter:
    ; dp_poss = (dp_poss << step) | (dp_poss >> step)
    ; Using dp_next as temporary for left-shifted version

    ; First: dp_next = dp_poss << step
    lea     rsi, [r15 + SCR_DP_POSS]
    lea     rdi, [r15 + SCR_DP_NEXT]
    call    shl_512

    ; Load all 8 qwords of dp_poss, shift right, OR with dp_next
    lea     rsi, [r15 + SCR_DP_POSS]
    lea     rdi, [r15 + SCR_DP_NEXT]
    mov     ecx, 64
    sub     ecx, r8d                     ; rcx = 64 - step

    ; Process each qword: (dp_poss >> step) | dp_next, store to dp_poss
    ; q0
    mov     rax, [rsi]
    shrx    rax, rax, r8
    mov     rdx, [rsi + 8]
    shlx    rdx, rdx, rcx
    or      rax, rdx
    or      rax, [rdi]
    mov     [rsi], rax
    ; q1
    mov     rax, [rsi + 8]
    shrx    rax, rax, r8
    mov     rdx, [rsi + 16]
    shlx    rdx, rdx, rcx
    or      rax, rdx
    or      rax, [rdi + 8]
    mov     [rsi + 8], rax
    ; q2
    mov     rax, [rsi + 16]
    shrx    rax, rax, r8
    mov     rdx, [rsi + 24]
    shlx    rdx, rdx, rcx
    or      rax, rdx
    or      rax, [rdi + 16]
    mov     [rsi + 16], rax
    ; q3
    mov     rax, [rsi + 24]
    shrx    rax, rax, r8
    mov     rdx, [rsi + 32]
    shlx    rdx, rdx, rcx
    or      rax, rdx
    or      rax, [rdi + 24]
    mov     [rsi + 24], rax
    ; q4
    mov     rax, [rsi + 32]
    shrx    rax, rax, r8
    mov     rdx, [rsi + 40]
    shlx    rdx, rdx, rcx
    or      rax, rdx
    or      rax, [rdi + 32]
    mov     [rsi + 32], rax
    ; q5
    mov     rax, [rsi + 40]
    shrx    rax, rax, r8
    mov     rdx, [rsi + 48]
    shlx    rdx, rdx, rcx
    or      rax, rdx
    or      rax, [rdi + 40]
    mov     [rsi + 40], rax
    ; q6
    mov     rax, [rsi + 48]
    shrx    rax, rax, r8
    mov     rdx, [rsi + 56]
    shlx    rdx, rdx, rcx
    or      rax, rdx
    or      rax, [rdi + 48]
    mov     [rsi + 48], rax
    ; q7 - no carry from above
    mov     rax, [rsi + 56]
    shrx    rax, rax, r8
    or      rax, [rdi + 56]
    mov     [rsi + 56], rax

    dec     r9d
    jnz     .dp_iter

.dp_next_shape:
    inc     ebx
    jmp     .dp_shape

.dp_done:
    ; Retrieve bounds (pop in reverse order)
    pop     rdi                          ; upper
    pop     rsi                          ; lower

    ; lo = max(lower + offset, 0)
    mov     eax, esi
    add     eax, ebp
    test    eax, eax
    jns     .lo_ok
    xor     eax, eax
.lo_ok:
    ; hi = min(upper + offset, 2*offset)
    mov     ecx, edi
    add     ecx, ebp
    mov     edx, ebp
    shl     edx, 1
    cmp     ecx, edx
    jle     .hi_ok
    mov     ecx, edx
.hi_ok:
    ; Check if any bit in range [lo, hi] is set
    lea     rsi, [r15 + SCR_DP_POSS]
.range_loop:
    cmp     eax, ecx
    jg      .fail
    ; Test bit at position eax
    mov     edx, eax
    shr     edx, 6                       ; qword index
    mov     r8d, eax
    and     r8d, 63                      ; bit index
    bt      qword [rsi + rdx*8], r8
    jc      .success
    inc     eax
    jmp     .range_loop

.fail:
    xor     eax, eax
    jmp     .exit

.success:
    mov     eax, 1

.exit:
    pop     rbp
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; dfs_place: Recursive DFS to place pieces
; Uses scratch frame at r15 for piece_order, placements, place_count
; Uses rbp for placement index to eliminate inner-loop pushes
; Input: edi=piece_idx, esi=total, rdx=used_mask, r15=scratch base
; Output: eax = 1 if found, 0 otherwise
;------------------------------------------------------------------------------
dfs_place:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    rbp
    ; r15 is scratch pointer - preserved by caller

    cmp     edi, esi
    jge     .found

    mov     r10d, edi                    ; piece index
    mov     r11d, esi                    ; total pieces
    mov     r13, rdx                     ; used_mask

    ; Get shape from piece_order in scratch
    lea     r12, [r15 + SCR_PIECE_ORDER]
    mov     eax, [r12 + r10*4]           ; shape
    mov     ebx, eax

    ; Get placement count from scratch
    lea     r14, [r15 + SCR_PLACE_COUNT]
    mov     ecx, [r14 + rbx*4]           ; placement count
    test    ecx, ecx
    jz      .not_found

    xor     ebp, ebp                     ; placement index in rbp (callee-saved!)
.try_loop:
    cmp     ebp, ecx
    jge     .not_found

    ; Compute placements address: scratch + SCR_PLACEMENTS + shape*4096 + idx*8
    mov     rax, rbx
    shl     rax, 12                      ; shape * 4096
    lea     rax, [r15 + SCR_PLACEMENTS + rax]

    mov     r8, [rax + rbp*8]            ; pmask
    test    r8, r13
    jnz     .next_place

    ; Recurse with new_used = used_mask | pmask
    ; No pushes needed - rbp preserved by callee, ecx recomputed after
    lea     edi, [r10 + 1]
    mov     esi, r11d
    mov     rdx, r13
    or      rdx, r8
    call    dfs_place

    ; After call: r10, r11, r13, r15, rbx, rbp all preserved (callee-saved)
    ; Recompute ecx = place_count[shape]
    lea     r14, [r15 + SCR_PLACE_COUNT]
    mov     ecx, [r14 + rbx*4]

    test    eax, eax
    jnz     .found

.next_place:
    inc     ebp
    jmp     .try_loop

.not_found:
    xor     eax, eax
    jmp     .exit

.found:
    mov     eax, 1

.exit:
    pop     rbp
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; can_pack_small: Check if shapes can fit in WxH region
; Uses scratch frame at r15 for all temp arrays (reentrant)
; Input: edi=width, esi=height, edx=shape_count, rcx=counts, r8=orient_masks, r9=orient_count
;        r15 = scratch base
; Output: eax = 1 if packable, 0 otherwise
;------------------------------------------------------------------------------
can_pack_small:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    rbp
    sub     rsp, 8                       ; align stack + store orient_count
    ; r15 is scratch pointer - preserved by caller

    ; Save parameters to callee-saved registers
    mov     r10d, edi                    ; width
    mov     r11d, esi                    ; height
    mov     r12d, edx                    ; shape_count
    mov     r13, rcx                     ; counts (input)
    mov     r14, r8                      ; orient_masks
    mov     [rsp], r9                    ; orient_count (on stack, r15 is scratch)

    ; Count total pieces and copy counts to scratch temp_counts
    xor     eax, eax
    xor     ecx, ecx
    lea     rdi, [r15 + SCR_TEMP_COUNTS]
.count_pieces:
    cmp     ecx, r12d
    jge     .pieces_counted
    mov     edx, [r13 + rcx*4]
    mov     [rdi + rcx*4], edx
    add     eax, edx
    inc     ecx
    jmp     .count_pieces

.pieces_counted:
    test    eax, eax
    jz      .success
    mov     ebp, eax                     ; ebp = total_pieces

    ; Clear placement counts in scratch
    lea     rdi, [r15 + SCR_PLACE_COUNT]
    xor     eax, eax
    mov     ecx, MAX_SHAPES
    rep     stosd

    ; Generate placements for each shape
    xor     ebx, ebx                     ; shape index
.gen_shape:
    cmp     ebx, r12d
    jge     .gen_done

    lea     rax, [r15 + SCR_TEMP_COUNTS]
    cmp     dword [rax + rbx*4], 0
    je      .gen_next

    mov     rax, [rsp]                   ; orient_count from stack
    mov     eax, [rax + rbx*4]           ; orient_count[shape]
    mov     r8d, eax                     ; r8d = orient count

    ; placements_ptr = scratch + SCR_PLACEMENTS + shape * 4096
    mov     rax, rbx
    shl     rax, 12
    lea     r9, [r15 + SCR_PLACEMENTS + rax]

    xor     ecx, ecx                     ; placement count
    xor     edx, edx                     ; orient index
.gen_orient:
    cmp     edx, r8d
    jge     .gen_orient_done

    ; Get orientation mask
    mov     rax, rbx
    shl     rax, 4
    add     rax, r14
    movzx   esi, word [rax + rdx*2]      ; orient_masks[shape][orient]

    push    rdx
    push    rcx
    push    r8
    push    r9
    mov     edi, r10d                    ; width
    mov     r8d, ecx                     ; current count
    mov     ecx, esi                     ; mask in ecx temporarily
    mov     esi, r11d                    ; height
    mov     edx, ecx                     ; mask
    mov     rcx, r9                      ; output
    mov     r9d, 512                     ; capacity
    call    generate_placements
    pop     r9
    pop     r8
    pop     rcx
    pop     rdx
    mov     ecx, eax                     ; new count

    inc     edx
    jmp     .gen_orient

.gen_orient_done:
    ; Sort and deduplicate placements for this shape
    ; ecx = count, r9 = placements array for this shape
    test    ecx, ecx
    jz      .save_count                  ; nothing to sort

    ; Save registers across sort call
    push    rbx
    push    r10
    push    r11
    push    r12
    push    r13
    push    r14
    push    rbp
    push    rcx                          ; save count
    push    r9                           ; save array ptr

    ; sort_u64(array, count)
    mov     rdi, r9
    mov     esi, ecx
    call    sort_u64

    ; Deduplicate: compact sorted array removing duplicates
    pop     rdi                          ; array ptr (was r9)
    pop     rcx                          ; count
    cmp     ecx, 1
    jle     .dedup_done                  ; 0 or 1 elements, no dups possible

    ; Compact: read through, write unique values
    ; rdi = array, ecx = count
    ; Use rsi as read index, edx as write index
    mov     eax, 1                       ; write index starts at 1 (first element always kept)
    mov     esi, 1                       ; read index
    mov     r8, [rdi]                    ; last written value
.dedup_loop:
    cmp     esi, ecx
    jge     .dedup_compact_done
    mov     r9, [rdi + rsi*8]            ; current value
    cmp     r9, r8                       ; same as last?
    je      .dedup_skip
    mov     [rdi + rax*8], r9            ; write unique value
    mov     r8, r9                       ; update last
    inc     eax
.dedup_skip:
    inc     esi
    jmp     .dedup_loop

.dedup_compact_done:
    mov     ecx, eax                     ; new count

.dedup_done:
    ; Restore r9 (need it for later checks, but value was popped to rdi)
    mov     r9, rdi

    pop     rbp
    pop     r14
    pop     r13
    pop     r12
    pop     r11
    pop     r10
    pop     rbx

.save_count:
    lea     rax, [r15 + SCR_PLACE_COUNT]
    mov     [rax + rbx*4], ecx

    ; If shape used but no placements, fail
    lea     rax, [r15 + SCR_TEMP_COUNTS]
    cmp     dword [rax + rbx*4], 0
    je      .gen_next
    test    ecx, ecx
    jz      .fail

.gen_next:
    inc     ebx
    jmp     .gen_shape

.gen_done:
    ; Build piece_order (most constrained first)
    ; Copy counts to temp_remain in scratch
    lea     rsi, [r15 + SCR_TEMP_COUNTS]
    lea     rdi, [r15 + SCR_TEMP_REMAIN]
    xor     ecx, ecx
.copy_remain:
    cmp     ecx, r12d
    jge     .copy_done
    mov     eax, [rsi + rcx*4]
    mov     [rdi + rcx*4], eax
    inc     ecx
    jmp     .copy_remain

.copy_done:
    lea     r8, [r15 + SCR_PIECE_ORDER]
    xor     r9d, r9d                     ; piece index
.build_order:
    cmp     r9d, ebp
    jge     .order_done

    ; Find shape with fewest placements
    mov     ebx, -1                      ; best_shape
    mov     ecx, 1000000                 ; best_places
    xor     edx, edx
.find_best:
    cmp     edx, r12d
    jge     .found_best

    lea     rax, [r15 + SCR_TEMP_REMAIN]
    cmp     dword [rax + rdx*4], 0
    je      .find_next

    lea     rax, [r15 + SCR_PLACE_COUNT]
    mov     esi, [rax + rdx*4]
    cmp     esi, ecx
    jge     .find_next
    mov     ecx, esi
    mov     ebx, edx

.find_next:
    inc     edx
    jmp     .find_best

.found_best:
    mov     [r8 + r9*4], ebx
    lea     rax, [r15 + SCR_TEMP_REMAIN]
    dec     dword [rax + rbx*4]
    inc     r9d
    jmp     .build_order

.order_done:
    ; DFS to find solution
    ; dfs_place uses r15 for scratch access
    xor     edi, edi
    mov     esi, ebp
    xor     edx, edx                     ; used_mask = 0
    call    dfs_place

    test    eax, eax
    jz      .fail
    jmp     .success

.fail:
    xor     eax, eax
    jmp     .exit

.success:
    mov     eax, 1

.exit:
    add     rsp, 8
    pop     rbp
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; parse_uint: Parse unsigned integer (no stack)
;------------------------------------------------------------------------------
parse_uint:
.skip:
    cmp     rdi, rsi
    jge     .no_num
    movzx   eax, byte [rdi]
    cmp     al, '0'
    jb      .not_digit
    cmp     al, '9'
    jbe     .found
.not_digit:
    inc     rdi
    jmp     .skip

.found:
    xor     eax, eax
.parse:
    cmp     rdi, rsi
    jge     .done
    movzx   ecx, byte [rdi]
    cmp     cl, '0'
    jb      .done
    cmp     cl, '9'
    ja      .done
    imul    eax, 10
    sub     cl, '0'
    add     eax, ecx
    inc     rdi
    jmp     .parse

.done:
    ret

.no_num:
    xor     eax, eax
    ret

;------------------------------------------------------------------------------
; main: Allocates scratch frame on stack for reentrant functions
; r15 = scratch base pointer (preserved across all calls)
;------------------------------------------------------------------------------
; Stack frame layout (after 6 pushes, rsp is 8-aligned):
;   [rsp]                = file position (8 bytes)
;   [rsp + 8]            = start timespec (16 bytes: tv_sec + tv_nsec)
;   [rsp + 24]           = end timespec (16 bytes)
;   [rsp + 40]           = scratch frame start (SCRATCH_SIZE bytes, 16-aligned)
; Total: 40 + SCRATCH_SIZE, must be 8 mod 16 for call alignment
;------------------------------------------------------------------------------
%define MAIN_FILE_POS      0
%define MAIN_START_TIME    8
%define MAIN_END_TIME      24
%define MAIN_SCRATCH       40
%define MAIN_FRAME_SIZE    (40 + SCRATCH_SIZE)

main:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    push    rbp
    ; 6 pushes = 48 bytes, need frame aligned to 16
    ; MAIN_FRAME_SIZE should be multiple of 16 for alignment
    sub     rsp, MAIN_FRAME_SIZE

    ; Set up scratch pointer (r15 = base of scratch frame)
    lea     r15, [rsp + MAIN_SCRATCH]

    ; Get start time
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rsp + MAIN_START_TIME]
    call    clock_gettime

    ; Read file
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
    mov     r12, rax                     ; file size
    lea     r13, [rel file_buf]          ; start
    lea     r14, [r13 + r12]             ; end
    mov     [rsp + MAIN_FILE_POS], r13   ; current position (use stack slot)
    xor     ebx, ebx                     ; shape_count

    ;------------------------------------------------------------------
    ; Parse shapes section
    ; Uses rsi as current file position (loaded from/stored to stack)
    ;------------------------------------------------------------------
.parse_shapes:
    mov     rsi, [rsp + MAIN_FILE_POS]
    cmp     rsi, r14
    jge     .shapes_done

    movzx   eax, byte [rsi]
    cmp     al, '0'
    jb      .skip_char
    cmp     al, '9'
    ja      .skip_char

    mov     rdi, rsi
.scan_colon:
    cmp     rdi, r14
    jge     .skip_char
    movzx   eax, byte [rdi]
    cmp     al, ':'
    je      .found_header
    cmp     al, 'x'
    je      .shapes_done
    cmp     al, 10
    je      .skip_char
    inc     rdi
    jmp     .scan_colon

.found_header:
    ; Initialize shape grid to '.'
    push    rsi                          ; save file pos
    lea     rdi, [rel shape_grid]
    mov     rax, rbx
    imul    rax, 9
    add     rdi, rax
    mov     ecx, 9
    mov     al, '.'
    rep     stosb
    pop     rsi                          ; restore file pos

    lea     rdi, [rel shape_area]
    mov     dword [rdi + rbx*4], 0
    lea     rdi, [rel shape_diff]
    mov     dword [rdi + rbx*4], 0

.skip_to_newline1:
    cmp     rsi, r14
    jge     .shapes_done
    movzx   eax, byte [rsi]
    inc     rsi
    cmp     al, 10
    jne     .skip_to_newline1

    xor     ebp, ebp                     ; row
.read_grid:
    cmp     ebp, 3
    jge     .shape_done
    cmp     rsi, r14
    jge     .shape_done

    movzx   eax, byte [rsi]
    cmp     al, '0'
    jb      .not_new_section
    cmp     al, '9'
    ja      .not_new_section
    mov     rdi, rsi
.check_new:
    cmp     rdi, r14
    jge     .not_new_section
    movzx   eax, byte [rdi]
    cmp     al, ':'
    je      .shape_done
    cmp     al, 'x'
    je      .shape_done
    cmp     al, 10
    je      .not_new_section
    inc     rdi
    jmp     .check_new

.not_new_section:
    lea     rdi, [rel shape_grid]
    mov     rax, rbx
    imul    rax, 9
    add     rdi, rax
    mov     rax, rbp
    imul    rax, 3
    add     rdi, rax

    xor     ecx, ecx
.read_cols:
    cmp     ecx, 3
    jge     .row_done
    cmp     rsi, r14
    jge     .row_done
    movzx   eax, byte [rsi]
    cmp     al, 10
    je      .row_done
    cmp     al, 13
    je      .skip_cr
    mov     [rdi + rcx], al

    cmp     al, '#'
    jne     .not_hash
    push    rdi
    lea     rdi, [rel shape_area]
    inc     dword [rdi + rbx*4]
    mov     eax, ebp
    add     eax, ecx
    and     eax, 1
    lea     rdi, [rel shape_diff]
    jnz     .odd_parity
    inc     dword [rdi + rbx*4]
    pop     rdi
    jmp     .not_hash
.odd_parity:
    dec     dword [rdi + rbx*4]
    pop     rdi

.not_hash:
    inc     rsi
    inc     ecx
    jmp     .read_cols

.skip_cr:
    inc     rsi
    jmp     .read_cols

.row_done:
.skip_rest_of_line:
    cmp     rsi, r14
    jge     .row_done2
    movzx   eax, byte [rsi]
    inc     rsi
    cmp     al, 10
    jne     .skip_rest_of_line

.row_done2:
    inc     ebp
    jmp     .read_grid

.shape_done:
    mov     [rsp + MAIN_FILE_POS], rsi   ; save file pos
    inc     ebx
    jmp     .parse_shapes

.skip_char:
    inc     rsi
    mov     [rsp + MAIN_FILE_POS], rsi   ; save file pos
    jmp     .parse_shapes

.shapes_done:
    mov     [rsp + MAIN_FILE_POS], rsi   ; save final file pos
    mov     [rel shape_count], ebx
    mov     r8d, ebx                     ; save shape_count in r8d

    ;------------------------------------------------------------------
    ; Generate orientations for each shape
    ;------------------------------------------------------------------
    xor     r12d, r12d
.gen_orients:
    cmp     r12d, r8d
    jge     .orients_done

    push    r8
    lea     rdi, [rel shape_grid]
    mov     rax, r12
    imul    rax, 9
    add     rdi, rax

    lea     rsi, [rel orient_masks]
    mov     rax, r12
    shl     rax, 4
    add     rsi, rax

    call    generate_orientations

    lea     rdi, [rel orient_count]
    mov     [rdi + r12*4], eax
    pop     r8

    inc     r12d
    jmp     .gen_orients

.orients_done:
    ;------------------------------------------------------------------
    ; Parse and process regions
    ; r8d = shape_count, ebp = fits_count, r12d/r13d = width/height
    ; Uses rsi as current file position (loaded from/stored to stack)
    ;------------------------------------------------------------------
    xor     ebp, ebp                     ; fits_count

.parse_regions:
    mov     rsi, [rsp + MAIN_FILE_POS]
    cmp     rsi, r14
    jge     .regions_done

    movzx   eax, byte [rsi]
    cmp     al, ' '
    je      .skip_ws
    cmp     al, 10
    je      .skip_ws
    cmp     al, 13
    je      .skip_ws
    jmp     .check_region

.skip_ws:
    inc     rsi
    mov     [rsp + MAIN_FILE_POS], rsi
    jmp     .parse_regions

.check_region:
    mov     rdi, rsi
.find_x:
    cmp     rdi, r14
    jge     .skip_line
    movzx   eax, byte [rdi]
    cmp     al, 10
    je      .skip_line
    cmp     al, 'x'
    je      .found_region
    inc     rdi
    jmp     .find_x

.skip_line:
    mov     rsi, rdi
    cmp     rsi, r14
    jge     .regions_done
    inc     rsi
    mov     [rsp + MAIN_FILE_POS], rsi
    jmp     .parse_regions

.found_region:
    ; Parse width
    push    r8
    mov     rdi, rsi                     ; rdi = current pos for parse_uint
    mov     rsi, r14                     ; rsi = end
    call    parse_uint
    mov     r12d, eax                    ; width
    mov     rsi, rdi                     ; rsi = updated position
    pop     r8

    inc     rsi                          ; skip 'x'

    ; Parse height
    push    r8
    push    rsi                          ; save current pos
    mov     rdi, rsi                     ; rdi = current pos
    mov     rsi, r14                     ; rsi = end
    call    parse_uint
    mov     r13d, eax                    ; height
    pop     rsi                          ; discard saved (we use rdi)
    mov     rsi, rdi                     ; rsi = updated position
    pop     r8

    ; Calculate area in r9d
    mov     eax, r12d
    imul    eax, r13d
    mov     r9d, eax                     ; area

    ; Find colon
.find_colon:
    cmp     rsi, r14
    jge     .next_region
    movzx   eax, byte [rsi]
    inc     rsi
    cmp     al, ':'
    jne     .find_colon

    ; Parse counts into scratch temp_counts, accumulate total_pieces and used_area
    xor     ecx, ecx                     ; shape index
    xor     r10d, r10d                   ; total_pieces
    xor     r11d, r11d                   ; used_area
    lea     rbx, [r15 + SCR_TEMP_COUNTS] ; use scratch for temp_counts
.parse_counts:
    cmp     ecx, r8d
    jge     .counts_done
    cmp     rsi, r14
    jge     .counts_done

    movzx   eax, byte [rsi]
    cmp     al, 10
    je      .counts_done
    cmp     al, 13
    je      .counts_done

    push    rcx
    push    r8
    push    r10
    push    r11
    push    rsi                          ; save current pos
    mov     rdi, rsi                     ; rdi = current pos for parse_uint
    mov     rsi, r14                     ; rsi = end
    call    parse_uint
    pop     rsi                          ; discard (use rdi)
    mov     rsi, rdi                     ; rsi = updated position
    pop     r11
    pop     r10
    pop     r8
    pop     rcx

    mov     [rbx + rcx*4], eax           ; temp_counts[shape] = count
    add     r10d, eax                    ; total_pieces += count

    push    rax
    lea     rdi, [rel shape_area]
    mov     eax, [rdi + rcx*4]
    pop     rdi                          ; count
    imul    eax, edi
    add     r11d, eax                    ; used_area += count * area

    inc     ecx
    jmp     .parse_counts

.counts_done:
    mov     [rsp + MAIN_FILE_POS], rsi   ; save file pos

    ; Quick area check
    cmp     r11d, r9d
    jg      .next_region

    ; Parity reachability check
    ; reachable_diff uses r15 for scratch
    push    r8
    push    r9
    push    r10
    mov     edi, r9d                     ; total_area
    mov     esi, r8d                     ; shape_count
    lea     rdx, [r15 + SCR_TEMP_COUNTS] ; counts from scratch
    lea     rcx, [rel shape_area]
    lea     r8, [rel shape_diff]
    call    reachable_diff
    pop     r10
    pop     r9
    pop     r8
    test    eax, eax
    jz      .next_region

    ; For small regions, do exact DFS
    cmp     r9d, SEARCH_AREA_LIMIT
    ja      .assume_fits

    cmp     r10d, SEARCH_PIECE_LIMIT
    ja      .assume_fits

    ; Do exact cover check
    ; can_pack_small uses r15 for scratch
    push    r8
    mov     edi, r12d                    ; width
    mov     esi, r13d                    ; height
    mov     edx, r8d                     ; shape_count
    lea     rcx, [r15 + SCR_TEMP_COUNTS] ; counts from scratch
    lea     r8, [rel orient_masks]
    lea     r9, [rel orient_count]
    call    can_pack_small
    pop     r8
    test    eax, eax
    jz      .next_region
    jmp     .fits

.assume_fits:
.fits:
    inc     ebp

.next_region:
.skip_eol:
    mov     rsi, [rsp + MAIN_FILE_POS]
    cmp     rsi, r14
    jge     .regions_done
    movzx   eax, byte [rsi]
    inc     rsi
    mov     [rsp + MAIN_FILE_POS], rsi
    cmp     al, 10
    jne     .skip_eol
    jmp     .parse_regions

.regions_done:
    ;------------------------------------------------------------------
    ; Output results
    ;------------------------------------------------------------------
    ; Save result count
    mov     r12d, ebp

    ; Get end time
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rsp + MAIN_END_TIME]
    call    clock_gettime

    ; Calculate elapsed_ms = (end.tv_sec - start.tv_sec) * 1000.0 +
    ;                        (end.tv_nsec - start.tv_nsec) / 1000000.0
    mov     rax, [rsp + MAIN_END_TIME]        ; end.tv_sec
    sub     rax, [rsp + MAIN_START_TIME]      ; - start.tv_sec
    cvtsi2sd xmm0, rax                        ; convert to double
    mov     rax, 1000
    cvtsi2sd xmm1, rax
    mulsd   xmm0, xmm1                        ; * 1000.0

    mov     rax, [rsp + MAIN_END_TIME + 8]    ; end.tv_nsec
    sub     rax, [rsp + MAIN_START_TIME + 8]  ; - start.tv_nsec
    cvtsi2sd xmm2, rax                        ; convert to double
    mov     rax, 1000000
    cvtsi2sd xmm1, rax
    divsd   xmm2, xmm1                        ; / 1000000.0
    addsd   xmm0, xmm2                        ; total elapsed_ms

    ; printf("regions_that_fit=%d elapsed_ms=%.3f\n", count, elapsed_ms)
    mov     esi, r12d
    lea     rdi, [rel fmt_out]
    mov     eax, 1                            ; 1 xmm register used
    call    printf

    xor     eax, eax

.exit:
    add     rsp, MAIN_FRAME_SIZE
    pop     rbp
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
