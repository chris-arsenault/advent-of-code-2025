; Day 9: Rectangle Areas
; Hand-written x86-64 assembly - register-centric design
; Part 1: Max rectangle area from any two points
; Part 2: Max rectangle inside polygon formed by points
;
; Implemented Optimizations (from ALGORITHMS.md + review feedback):
;   - Cross-product ray intersection: replaces idiv (20-40 cycles) with imul (3-4)
;   - Point-on-edge check for correct boundary handling
;   - Early exit on first failing corner or edge crossing check
;   - Branchless abs using cmovs (GCC pattern): mov/neg/cmovs - no branch, short dep
;   - Predictable branch for j = (i+1) % n wrap (beats cmov for "wrap once" pattern)
;   - Inline ray-cast macro: eliminates 4x function call overhead per rectangle
;   - Register-based crossings counter: avoids memory traffic in hot loop
;   - Hoisted array bases in callee-saved registers (r12, r13, r14)
;   - px/py loaded into registers once per corner (avoids repeated memory access)

global main
extern clock_gettime
extern printf
extern perror
extern ns_since
extern read_file_all
extern parse_uint64
extern skip_non_digits

; ============================================================================
; Register allocation (callee-saved, preserved across calls):
;   rbx  = loop counter / scratch
;   r12d = point_count (constant after parsing)
;   r13  = points_x base (hoisted)
;   r14  = points_y base (hoisted)
;   r15  = best area (part 1 result, then part 2 result)
;
; Stack frame (main):
;   ret(8) + 5 pushes(40) = 48, 48 mod 16 = 0
;   sub rsp, 48: keeps alignment for calls
; ============================================================================
%define STK_FILE_PTR    0               ; 8 bytes - current file pointer
%define STK_FILE_END    8               ; 8 bytes - file end pointer
%define STK_P1_RESULT   16              ; 8 bytes - part 1 result
%define STK_J_SAVE      24              ; 4 bytes - j loop counter save
%define STK_PADDING     28              ; 4 bytes padding
%define STK_COORDS      32              ; 16 bytes - saved coords for rect_in_polygon
%define STK_SIZE        48              ; total frame size

%define CLOCK_MONOTONIC 1
%define BUF_SIZE        1048576
%define MAX_POINTS      512

section .data
input_file:    db "input.txt", 0
fmt_out:       db "max_rect_area=%llu max_green_rect_area=%llu elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2
points_x:      resd MAX_POINTS      ; 32-bit coords (matches C, halves memory)
points_y:      resd MAX_POINTS      ; 32-bit coords (matches C, halves memory)

section .text

; ============================================================================
; INLINE RAY-CAST MACRO
; ============================================================================
; Why inline? Function call overhead dominates Part 2 performance:
;   - Each rect_in_polygon checks 4 corners = 4 function calls
;   - Each call: 6 pushes + 6 pops + call/ret = ~30 cycles overhead
;   - With 512 points → ~130k rectangles → ~15-25ms wasted on call overhead
;
; This macro eliminates that overhead by expanding the ray-cast loop inline.
; Trade-off: Larger code size, but 4x fewer push/pop sequences.
;
; OPTIMIZATION: px/py are loaded into registers ONCE at macro start.
; This avoids repeated memory accesses when %1/%2 are memory operands.
; The C compiler does this automatically; we must do it explicitly.
;
; Usage: RAY_CAST_INLINE px_mem, py_mem, result_reg, unique_label_prefix
;   - px_mem, py_mem: memory operands or registers for point coordinates
;   - result_reg: register to receive result (1=inside, 0=outside)
;   - unique_label_prefix: string to make labels unique per invocation
;
; Requires pre-set registers:
;   - r12 = points_x base pointer
;   - r13 = points_y base pointer
;   - r14d = n (point count)
;
; Register allocation inside macro:
;   - rdi = px (loaded once at start, constant throughout)
;   - rsi = py (loaded once at start, constant throughout)
;   - rbp = crossings counter
;   - rbx = i (loop counter)
;   - ecx = j (next index)
;   - r8, r9 = x1, y1 (edge start)
;   - r10, r11 = x2, y2 (edge end)
;   - rax, rdx = scratch for min/max and cross-product
;   - r15 = scratch for cross-product (y2-y1)
;
; Clobbers: rax, rcx, rdx, rdi, rsi, r8, r9, r10, r11, r15, rbx, rbp
; ============================================================================
%macro RAY_CAST_INLINE 4
    ; %1 = px (memory/reg), %2 = py (memory/reg), %3 = result reg, %4 = label prefix

    ; === CRITICAL OPTIMIZATION: Load px/py into registers ONCE ===
    ; Avoids N memory accesses per corner (where N = polygon edge count)
    mov     rdi, %1                     ; px in rdi (constant for this corner)
    mov     rsi, %2                     ; py in rsi (constant for this corner)

    xor     ebp, ebp                    ; crossings = 0 (register, not stack!)
    xor     ebx, ebx                    ; i = 0

%%loop:
    cmp     ebx, r14d
    jge     %%done

    ; j = (i + 1) % n - predictable branch
    lea     ecx, [ebx + 1]
    cmp     ecx, r14d
    jl      %%j_ok
    xor     ecx, ecx
%%j_ok:

    ; Load edge endpoints: (x1,y1) -> (x2,y2) - 32-bit coords, zero-extend
    mov     r8d, [r12 + rbx*4]          ; x1 (32-bit, zero-extends to r8)
    mov     r9d, [r13 + rbx*4]          ; y1
    mov     r10d, [r12 + rcx*4]         ; x2
    mov     r11d, [r13 + rcx*4]         ; y2

    ; --- Point-on-edge check: vertical edge (x1 == x2) ---
    cmp     r8, r10
    jne     %%check_horiz

    cmp     rdi, r8                     ; px == x1?
    jne     %%ray_test

    ; Check py in [min(y1,y2), max(y1,y2)]
    ; Use rax/rdx as scratch (rdi/rsi hold px/py)
    mov     rax, r9                     ; min candidate = y1
    mov     rdx, r11                    ; max candidate = y2
    cmp     rax, rdx
    jle     %%v_ok
    xchg    rax, rdx                    ; swap so rax=min, rdx=max
%%v_ok:
    cmp     rsi, rax                    ; py >= min?
    jl      %%ray_test
    cmp     rsi, rdx                    ; py <= max?
    jg      %%ray_test
    mov     %3, 1                       ; on edge = inside
    jmp     %%ret

%%check_horiz:
    ; --- Point-on-edge check: horizontal edge (y1 == y2) ---
    cmp     r9, r11
    jne     %%ray_test

    cmp     rsi, r9                     ; py == y1?
    jne     %%ray_test

    ; Check px in [min(x1,x2), max(x1,x2)]
    ; Use rax/rdx as scratch
    mov     rax, r8                     ; min candidate = x1
    mov     rdx, r10                    ; max candidate = x2
    cmp     rax, rdx
    jle     %%h_ok
    xchg    rax, rdx                    ; swap so rax=min, rdx=max
%%h_ok:
    cmp     rdi, rax                    ; px >= min?
    jl      %%ray_test
    cmp     rdi, rdx                    ; px <= max?
    jg      %%ray_test
    mov     %3, 1                       ; on edge = inside
    jmp     %%ret

%%ray_test:
    ; --- Ray casting: does rightward ray cross this edge? ---
    ; Check if edge straddles py (y1 and y2 on opposite sides)
    ; Use eax/ecx for setg flags (rdi/rsi hold px/py)
    xor     eax, eax
    cmp     r9, rsi                     ; y1 > py?
    setg    al
    xor     ecx, ecx
    cmp     r11, rsi                    ; y2 > py?
    setg    cl
    cmp     eax, ecx
    je      %%next                      ; same side = no crossing

    ; Normalize: ensure y2 > y1 (swap if needed for positive divisor)
    test    al, al
    jz      %%no_swap
    xchg    r8, r10
    xchg    r9, r11
%%no_swap:

    ; Cross-product comparison: px < x_intersect
    ; Equivalent to: (px-x1)*(y2-y1) < (x2-x1)*(py-y1)
    ; Use r15 for (y2-y1) since rdi holds px
    mov     rax, rdi                    ; px (from register, not memory!)
    sub     rax, r8                     ; px - x1
    mov     r15, r11
    sub     r15, r9                     ; y2 - y1 (positive)
    imul    rax, r15                    ; LHS = (px-x1)*(y2-y1)

    mov     rcx, r10
    sub     rcx, r8                     ; x2 - x1
    mov     rdx, rsi                    ; py (from register, not memory!)
    sub     rdx, r9                     ; py - y1
    imul    rcx, rdx                    ; RHS = (x2-x1)*(py-y1)

    cmp     rax, rcx
    jge     %%next
    inc     ebp                         ; crossings++ (in register!)

%%next:
    inc     ebx
    jmp     %%loop

%%done:
    ; Result: odd crossings count = inside polygon
    mov     %3, rbp
    and     %3, 1

%%ret:
%endmacro

; ============================================================================
; point_in_polygon - standalone function (kept for reference/testing)
; In production, rect_in_polygon uses the inline macro instead.
; ============================================================================
; point_in_polygon(px, py, n, points_x_base, points_y_base)
; Ray casting algorithm with point-on-edge check
; Input:  rdi=px, rsi=py, edx=n, rcx=points_x, r8=points_y
; Output: eax=1 if inside/on-edge, 0 if outside
; ============================================================================
point_in_polygon:
    push    rbx
    push    rbp
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, 8                      ; stack slot for crossings

    mov     r12, rdi                    ; px
    mov     r13, rsi                    ; py
    mov     r14d, edx                   ; n
    mov     r15, rcx                    ; points_x base
    mov     rbp, r8                     ; points_y base

    mov     dword [rsp], 0              ; crossings = 0
    xor     ebx, ebx                    ; i = 0

.pip_loop:
    cmp     ebx, r14d
    jge     .pip_done

    ; j = (i + 1) % n - predictable branch beats cmov here
    lea     ecx, [ebx + 1]
    cmp     ecx, r14d
    jl      .pip_j_ok
    xor     ecx, ecx
.pip_j_ok:

    ; Load edge endpoints: (x1,y1) -> (x2,y2) - 32-bit coords
    mov     r8d, [r15 + rbx*4]          ; x1
    mov     r9d, [rbp + rbx*4]          ; y1
    mov     r10d, [r15 + rcx*4]         ; x2
    mov     r11d, [rbp + rcx*4]         ; y2

    ; Point-on-edge check: vertical edge (x1 == x2)
    cmp     r8, r10
    jne     .check_horiz

    cmp     r12, r8                     ; px == x1?
    jne     .ray_test

    ; Check py in [min(y1,y2), max(y1,y2)]
    mov     rdi, r9
    mov     rsi, r11
    cmp     rdi, rsi
    jle     .v_ok
    xchg    rdi, rsi
.v_ok:
    cmp     r13, rdi
    jl      .ray_test
    cmp     r13, rsi
    jg      .ray_test
    mov     eax, 1                      ; on edge - return 1
    jmp     .pip_ret

.check_horiz:
    ; Point-on-edge check: horizontal edge (y1 == y2)
    cmp     r9, r11
    jne     .ray_test

    cmp     r13, r9                     ; py == y1?
    jne     .ray_test

    ; Check px in [min(x1,x2), max(x1,x2)]
    mov     rdi, r8
    mov     rsi, r10
    cmp     rdi, rsi
    jle     .h_ok
    xchg    rdi, rsi
.h_ok:
    cmp     r12, rdi
    jl      .ray_test
    cmp     r12, rsi
    jg      .ray_test
    mov     eax, 1                      ; on edge - return 1
    jmp     .pip_ret

.ray_test:
    ; Ray casting: does rightward ray from (px,py) cross this edge?
    ; Condition: (y1 > py) != (y2 > py) AND px < x_intersect
    xor     edi, edi
    cmp     r9, r13                     ; y1 > py?
    setg    dil
    xor     esi, esi
    cmp     r11, r13                    ; y2 > py?
    setg    sil
    cmp     edi, esi
    je      .pip_next                   ; same side of ray - no crossing

    ; Normalize: ensure y2 > y1 so (y2-y1) is positive
    ; If y1 > py (dil=1), then y1 > y2, need to swap
    test    dil, dil
    jz      .no_swap
    xchg    r8, r10
    xchg    r9, r11
.no_swap:

    ; Cross-product comparison replaces expensive idiv (20-40 cycles) with imul (3-4)
    ; px < x_intersect ⟺ (px-x1)*(y2-y1) < (x2-x1)*(py-y1)
    ; y2-y1 guaranteed positive after normalization, and non-zero since y1 != y2
    mov     rax, r12
    sub     rax, r8                     ; px - x1
    mov     rdi, r11
    sub     rdi, r9                     ; y2 - y1 (positive)
    imul    rax, rdi                    ; LHS = (px-x1)*(y2-y1)

    mov     rcx, r10
    sub     rcx, r8                     ; x2 - x1
    mov     rdx, r13
    sub     rdx, r9                     ; py - y1
    imul    rcx, rdx                    ; RHS = (x2-x1)*(py-y1)

    cmp     rax, rcx                    ; LHS < RHS?
    jge     .pip_next
    inc     dword [rsp]                 ; crossings++

.pip_next:
    inc     ebx
    jmp     .pip_loop

.pip_done:
    mov     eax, [rsp]
    and     eax, 1                      ; odd crossings = inside

.pip_ret:
    add     rsp, 8
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbp
    pop     rbx
    ret

; ============================================================================
; rect_in_polygon(xlo, ylo, xhi, yhi, n, points_x, points_y)
; Check 4 corners inside + no edge crosses interior
; Input: rdi=xlo, rsi=ylo, rdx=xhi, rcx=yhi, r8d=n, r9=points_x, [rsp+8]=points_y
; Output: eax=1 if rect inside, 0 otherwise
;
; OPTIMIZATION: Uses inline ray-cast macro instead of function calls.
; This eliminates 4x function call overhead per rectangle:
;   - No push/pop of 6 callee-saved registers per corner
;   - No call/ret overhead
;   - Saves ~15-25ms on typical input
;
; Trade-off: Larger code size from 4 macro expansions, but much faster.
; ============================================================================
; Local stack layout (32 bytes for rect coords)
%define RIP_XLO     0
%define RIP_YLO     8
%define RIP_XHI     16
%define RIP_YHI     24
%define RIP_FRAME   32
%define RIP_ARG7    96    ; 32 + 48(pushes) + 8(ret) + 8(align)

rect_in_polygon:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    push    rbp
    sub     rsp, RIP_FRAME

    ; Set up polygon array pointers and count in callee-saved registers
    ; These remain constant across all 4 corner checks and edge loop
    mov     r12, r9                     ; points_x base
    mov     r13, [rsp+RIP_ARG7]         ; points_y base (7th arg)
    mov     r14d, r8d                   ; n (point count)

    ; Save rectangle coordinates to stack
    ; These are accessed via memory operands in the inline macro
    mov     [rsp+RIP_XLO], rdi
    mov     [rsp+RIP_YLO], rsi
    mov     [rsp+RIP_XHI], rdx
    mov     [rsp+RIP_YHI], rcx

    ; =========================================================================
    ; INLINE CORNER CHECKS - 4 macro expansions replace 4 function calls
    ; Each macro checks if (px, py) is inside the polygon using ray-casting.
    ; Memory operands [rsp+...] are valid for the macro's compare/move ops.
    ; =========================================================================

    ; Corner 1: (xlo, ylo)
    RAY_CAST_INLINE [rsp+RIP_XLO], [rsp+RIP_YLO], rax, corner1
    test    eax, eax
    jz      .rip_false

    ; Corner 2: (xhi, ylo)
    RAY_CAST_INLINE [rsp+RIP_XHI], [rsp+RIP_YLO], rax, corner2
    test    eax, eax
    jz      .rip_false

    ; Corner 3: (xlo, yhi)
    RAY_CAST_INLINE [rsp+RIP_XLO], [rsp+RIP_YHI], rax, corner3
    test    eax, eax
    jz      .rip_false

    ; Corner 4: (xhi, yhi)
    RAY_CAST_INLINE [rsp+RIP_XHI], [rsp+RIP_YHI], rax, corner4
    test    eax, eax
    jz      .rip_false

    ; =========================================================================
    ; EDGE CROSSING CHECKS - ensure no polygon edge crosses rectangle interior
    ; Use r15d as loop counter (rbx was clobbered by inline macros above)
    ; =========================================================================
    xor     r15d, r15d                  ; i = 0

.edge_loop:
    cmp     r15d, r14d
    jge     .rip_true

    ; j = (i + 1) % n - predictable branch
    lea     ecx, [r15d + 1]
    cmp     ecx, r14d
    jl      .rip_j_ok
    xor     ecx, ecx
.rip_j_ok:

    ; Load edge endpoints (note: using r15 as loop index, not rbx) - 32-bit coords
    mov     r8d, [r12 + r15*4]          ; x1
    mov     r9d, [r13 + r15*4]          ; y1
    mov     r10d, [r12 + rcx*4]         ; x2
    mov     r11d, [r13 + rcx*4]         ; y2

    ; Vertical edge crossing check (x1 == x2)
    cmp     r8, r10
    jne     .check_horiz_edge

    ; x strictly inside (xlo, xhi)?
    cmp     r8, [rsp+RIP_XLO]
    jle     .edge_next
    cmp     r8, [rsp+RIP_XHI]
    jge     .edge_next

    ; y range overlaps (ylo, yhi)?
    mov     rax, r9
    mov     rdx, r11
    cmp     rax, rdx
    jle     .v_sorted
    xchg    rax, rdx
.v_sorted:
    cmp     rdx, [rsp+RIP_YLO]          ; max <= ylo?
    jle     .edge_next
    cmp     rax, [rsp+RIP_YHI]          ; min >= yhi?
    jge     .edge_next
    jmp     .rip_false                  ; edge crosses interior

.check_horiz_edge:
    ; Horizontal edge crossing check (y1 == y2)
    cmp     r9, r11
    jne     .edge_next

    ; y strictly inside (ylo, yhi)?
    cmp     r9, [rsp+RIP_YLO]
    jle     .edge_next
    cmp     r9, [rsp+RIP_YHI]
    jge     .edge_next

    ; x range overlaps (xlo, xhi)?
    mov     rax, r8
    mov     rdx, r10
    cmp     rax, rdx
    jle     .h_sorted
    xchg    rax, rdx
.h_sorted:
    cmp     rdx, [rsp+RIP_XLO]          ; max <= xlo?
    jle     .edge_next
    cmp     rax, [rsp+RIP_XHI]          ; min >= xhi?
    jge     .edge_next
    jmp     .rip_false                  ; edge crosses interior

.edge_next:
    inc     r15d
    jmp     .edge_loop

.rip_true:
    mov     eax, 1
    jmp     .rip_done

.rip_false:
    xor     eax, eax

.rip_done:
    add     rsp, RIP_FRAME
    pop     rbp
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

; ============================================================================
; main
; ============================================================================
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
    ; Set up file pointers
    lea     rcx, [rel file_buf]
    mov     [rsp + STK_FILE_PTR], rcx
    lea     r14, [rcx + rax]                ; r14 = file_end (temporary)
    mov     [rsp + STK_FILE_END], r14

    ; Hoist array bases into callee-saved registers for entire main
    lea     r13, [rel points_x]
    lea     r14, [rel points_y]             ; r14 repurposed: file_end -> points_y base

    ; ========================================================================
    ; Parse points
    ; ========================================================================
    xor     r12d, r12d                  ; point_count = 0

.parse_loop:
    mov     rdi, [rsp + STK_FILE_PTR]
    mov     rsi, [rsp + STK_FILE_END]
    cmp     rdi, rsi
    jge     .parse_done

    call    skip_non_digits
    mov     [rsp + STK_FILE_PTR], rax
    cmp     rax, [rsp + STK_FILE_END]
    jge     .parse_done

    ; Parse x (store as 32-bit)
    lea     rdi, [rsp + STK_FILE_PTR]
    mov     rsi, [rsp + STK_FILE_END]
    call    parse_uint64
    mov     [r13 + r12*4], eax          ; 32-bit store

    ; Skip to y
    mov     rdi, [rsp + STK_FILE_PTR]
    mov     rsi, [rsp + STK_FILE_END]
    call    skip_non_digits
    mov     [rsp + STK_FILE_PTR], rax

    ; Parse y (store as 32-bit)
    lea     rdi, [rsp + STK_FILE_PTR]
    mov     rsi, [rsp + STK_FILE_END]
    call    parse_uint64
    mov     [r14 + r12*4], eax          ; 32-bit store

    inc     r12d
    cmp     r12d, MAX_POINTS
    jl      .parse_loop

.parse_done:
    ; r12d = point_count (constant)
    ; r13 = points_x base (constant)
    ; r14 = points_y base (constant)

    ; ========================================================================
    ; Start timing
    ; ========================================================================
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; ========================================================================
    ; Part 1: Find max rectangle area (O(n²) point pairs)
    ; OPTIMIZATION: Hoist x[i], y[i] outside j loop (matches GCC)
    ; ========================================================================
    xor     r15, r15                    ; best = 0
    xor     ebx, ebx                    ; i = 0

.p1_i:
    cmp     ebx, r12d
    jge     .p1_done

    ; HOIST: Load i-th point coords ONCE (32-bit, zero-extends)
    mov     edi, [r13 + rbx*4]          ; x_i - hoisted (32-bit)
    mov     esi, [r14 + rbx*4]          ; y_i - hoisted (32-bit)
    lea     ecx, [ebx + 1]              ; j = i + 1

.p1_j:
    cmp     ecx, r12d
    jge     .p1_next_i

    ; abs(x_i - x_j) - all 32-bit, no sign extension (matches GCC)
    mov     eax, edi                    ; copy x_i
    sub     eax, [r13 + rcx*4]          ; eax = x_i - x_j
    mov     edx, eax                    ; backup
    neg     eax
    cmovs   eax, edx                    ; eax = |x_i - x_j|
    inc     eax                         ; dx + 1
    mov     r10d, eax                   ; save (clears upper r10)

    ; abs(y_i - y_j) - all 32-bit
    mov     eax, esi                    ; copy y_i
    sub     eax, [r14 + rcx*4]          ; eax = y_i - y_j
    mov     edx, eax                    ; backup
    neg     eax
    cmovs   eax, edx                    ; eax = |y_i - y_j|
    inc     eax                         ; dy + 1 (eax zero-extends to rax)

    ; area = (dx+1) * (dy+1) - 64-bit result
    imul    rax, r10                    ; rax = (dy+1) * (dx+1)
    cmp     rax, r15
    cmovg   r15, rax

    inc     ecx
    jmp     .p1_j

.p1_next_i:
    inc     ebx
    jmp     .p1_i

.p1_done:
    mov     [rsp + STK_P1_RESULT], r15  ; save part 1 result

    ; ========================================================================
    ; Part 2: Max rectangle inside polygon
    ; Early exit on first failing corner or edge check
    ; ========================================================================
    xor     r15, r15                    ; r15 repurposed: best1 -> best2 = 0
    xor     ebx, ebx                    ; i = 0

.p2_i:
    cmp     ebx, r12d
    jge     .p2_done

    lea     ecx, [ebx + 1]              ; j = i + 1

.p2_j:
    cmp     ecx, r12d
    jge     .p2_next_i

    ; Load coordinates (32-bit, zero-extend)
    mov     eax, [r13 + rbx*4]          ; x1
    mov     edx, [r13 + rcx*4]          ; x2
    cmp     eax, edx
    je      .p2_next_j

    mov     r8d, [r14 + rbx*4]          ; y1
    mov     r9d, [r14 + rcx*4]          ; y2
    cmp     r8d, r9d
    je      .p2_next_j

    ; Save j and coords for after rect_in_polygon call
    mov     [rsp + STK_J_SAVE], ecx
    mov     [rsp + STK_COORDS], rax     ; x1
    mov     [rsp + STK_COORDS+8], rdx   ; x2

    ; Compute xlo, xhi, ylo, yhi
    mov     rdi, rax                    ; xlo = x1
    mov     rsi, rdx                    ; xhi = x2
    cmp     rdi, rsi
    jle     .x_ok
    xchg    rdi, rsi
.x_ok:
    mov     rdx, rsi                    ; xhi in rdx

    mov     rsi, r8                     ; ylo = y1
    mov     rcx, r9                     ; yhi = y2
    cmp     rsi, rcx
    jle     .y_ok
    xchg    rsi, rcx
.y_ok:
    ; rdi=xlo, rsi=ylo, rdx=xhi, rcx=yhi

    ; Call rect_in_polygon(xlo, ylo, xhi, yhi, n, points_x, points_y)
    mov     r8d, r12d                   ; n
    mov     r9, r13                     ; points_x
    push    r14                         ; points_y on stack
    sub     rsp, 8                      ; align
    call    rect_in_polygon
    add     rsp, 16

    mov     ecx, [rsp + STK_J_SAVE]     ; restore j
    test    eax, eax
    jz      .p2_next_j

    ; Calculate area from saved coords - branchless abs using cmovs
    ; abs(x1-x2) + 1
    mov     rax, [rsp + STK_COORDS]     ; x1
    sub     rax, [rsp + STK_COORDS+8]   ; x1 - x2
    mov     rdx, rax
    neg     rax
    cmovs   rax, rdx
    inc     rax                         ; dx + 1
    mov     r10, rax

    ; abs(y1-y2) + 1 (rcx = j from restore above) - 32-bit arrays
    mov     eax, [r14 + rbx*4]          ; y1 (32-bit)
    sub     eax, [r14 + rcx*4]          ; y1 - y2 (32-bit signed)
    cdqe                                ; sign-extend to rax
    mov     rdx, rax
    neg     rax
    cmovs   rax, rdx
    inc     rax                         ; dy + 1

    imul    rax, r10
    cmp     rax, r15
    cmovg   r15, rax

.p2_next_j:
    inc     ecx
    jmp     .p2_j

.p2_next_i:
    inc     ebx
    jmp     .p2_i

.p2_done:
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

    ; printf(fmt, p1_result, p2_result, elapsed_ms)
    mov     rsi, [rsp + STK_P1_RESULT]
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
