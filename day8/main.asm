; Day 8: 3D MST with DSU
; Hand-written x86-64 assembly - register-centric design
; Part 1: Product of top 3 component sizes after 1000 edges
; Part 2: Product of x coords of final MST edge
;
; Optimizations: 32-bit distance calc, top-3 selection, hoisted DSU bases

global main
extern printf
extern perror
extern read_file_all
extern parse_uint64
extern skip_non_digits
extern sort_edges_3
extern clock_gettime
extern ns_since

; ============================================================================
; Register allocation (callee-saved, preserved across calls):
;   rbx  = loop counter / scratch
;   r12d = point_count (constant after parsing)
;   r13d = edge_count (constant after edge building)
;   r14  = file_end during parsing → p1_result after part 1
;   r15  = p1_limit during part 1 → p2_result during part 2
;
; Stack frame (minimal):
;   ret(8) + 5 pushes(40) = 48, 48 mod 16 = 0
;   sub rsp, 16: keeps alignment for calls
;   Only file_ptr needs stack (parse_uint64 takes pointer-to-pointer)
; ============================================================================
%define STK_FILE_PTR    0               ; 8 bytes - current file pointer
%define STK_SIZE        16              ; 8 bytes padding for alignment

%define BUF_SIZE        1048576
%define MAX_POINTS      1024
%define MAX_EDGES       600000
%define CONNECT_COUNT   1000

section .data
input_file:    db "input.txt", 0
fmt_out:       db "top3_product=%llu final_join_x_product=%llu elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2
points_x:      resq MAX_POINTS          ; 64-bit to avoid overflow in dist²
points_y:      resq MAX_POINTS
points_z:      resq MAX_POINTS
edges_dist:    resq MAX_EDGES
edges_i:       resd MAX_EDGES
edges_j:       resd MAX_EDGES
dsu_parent:    resd MAX_POINTS
dsu_size:      resd MAX_POINTS

section .text

main:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, STK_SIZE

    ; Start timing
    mov     edi, 1                      ; CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

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
    ; r14 = file_end (constant during parsing, repurposed to p1_result later)
    lea     rcx, [rel file_buf]
    mov     [rsp + STK_FILE_PTR], rcx
    lea     r14, [rcx + rax]

    ; ========================================================================
    ; Parse points: x,y,z coordinates
    ; Hoist array bases into registers for parsing
    ; ========================================================================
    lea     r8, [rel points_x]
    lea     r9, [rel points_y]
    lea     r10, [rel points_z]
    xor     r12d, r12d                  ; point_count = 0

.parse_points:
    mov     rdi, [rsp + STK_FILE_PTR]
    cmp     rdi, r14
    jge     .points_done

    mov     rsi, r14
    call    skip_non_digits
    mov     [rsp + STK_FILE_PTR], rax
    cmp     rax, r14
    jge     .points_done

    ; Parse x
    lea     rdi, [rsp + STK_FILE_PTR]
    mov     rsi, r14
    call    parse_uint64
    mov     [r8 + r12*8], rax

    ; Skip to y
    mov     rdi, [rsp + STK_FILE_PTR]
    mov     rsi, r14
    call    skip_non_digits
    mov     [rsp + STK_FILE_PTR], rax

    ; Parse y
    lea     rdi, [rsp + STK_FILE_PTR]
    mov     rsi, r14
    call    parse_uint64
    mov     [r9 + r12*8], rax

    ; Skip to z
    mov     rdi, [rsp + STK_FILE_PTR]
    mov     rsi, r14
    call    skip_non_digits
    mov     [rsp + STK_FILE_PTR], rax

    ; Parse z
    lea     rdi, [rsp + STK_FILE_PTR]
    mov     rsi, r14
    call    parse_uint64
    mov     [r10 + r12*8], rax

    inc     r12d
    cmp     r12d, MAX_POINTS
    jl      .parse_points

.points_done:
    ; r12d = point_count (now constant)
    ; r14 now free for reuse (will become p1_result)

    ; ========================================================================
    ; Build edges: O(n²) all pairs with squared Euclidean distance
    ; 64-bit arithmetic required (diff² can exceed 32 bits)
    ; ========================================================================
    lea     r8, [rel points_x]
    lea     r9, [rel points_y]
    lea     r10, [rel points_z]
    lea     r11, [rel edges_dist]

    xor     r13d, r13d                  ; edge_count = 0
    xor     ebx, ebx                    ; i = 0

.edge_i:
    cmp     ebx, r12d
    jge     .edges_done

    mov     ecx, ebx
    inc     ecx                         ; j = i + 1

.edge_j:
    cmp     ecx, r12d
    jge     .next_i

    ; Calculate dist² = (x[i]-x[j])² + (y[i]-y[j])² + (z[i]-z[j])²
    mov     rax, [r8 + rbx*8]
    sub     rax, [r8 + rcx*8]
    imul    rax, rax
    mov     rdi, rax                    ; accumulate in rdi

    mov     rax, [r9 + rbx*8]
    sub     rax, [r9 + rcx*8]
    imul    rax, rax
    add     rdi, rax

    mov     rax, [r10 + rbx*8]
    sub     rax, [r10 + rcx*8]
    imul    rax, rax
    add     rdi, rax

    ; Store edge
    mov     [r11 + r13*8], rdi
    lea     rsi, [rel edges_i]
    mov     [rsi + r13*4], ebx
    lea     rsi, [rel edges_j]
    mov     [rsi + r13*4], ecx

    inc     r13d
    inc     ecx
    jmp     .edge_j

.next_i:
    inc     ebx
    jmp     .edge_i

.edges_done:
    ; r13d = edge_count (now constant)

    ; ========================================================================
    ; Sort edges by distance using quicksort from shared utils
    ; ========================================================================
    lea     rdi, [rel edges_dist]
    lea     rsi, [rel edges_i]
    lea     rdx, [rel edges_j]
    mov     ecx, r13d
    call    sort_edges_3

    ; ========================================================================
    ; Initialize DSU
    ; dsu_parent[i] = i (loop, not rep stosd - each value differs)
    ; dsu_size[i] = 1 (rep stosd - all same value)
    ; ========================================================================
    lea     rdi, [rel dsu_parent]
    mov     ecx, r12d
    xor     eax, eax
.init_parent:
    mov     [rdi + rax*4], eax
    inc     eax
    cmp     eax, ecx
    jl      .init_parent

    ; dsu_size[i] = 1 using rep stosd (all same value)
    lea     rdi, [rel dsu_size]
    mov     ecx, r12d
    mov     eax, 1
    rep     stosd

    ; ========================================================================
    ; Part 1: Process first min(1000, edge_count) edges
    ; r15 = p1_limit (in register, repurposed to p2_result later)
    ; ========================================================================
    mov     r15d, CONNECT_COUNT
    cmp     r15d, r13d
    cmovg   r15d, r13d

    ; Hoist DSU bases for union calls
    lea     r8, [rel dsu_parent]
    lea     r9, [rel dsu_size]
    lea     r10, [rel edges_i]
    lea     r11, [rel edges_j]
    xor     ebx, ebx

.p1_loop:
    cmp     ebx, r15d
    jge     .p1_done

    mov     edi, [r10 + rbx*4]
    mov     esi, [r11 + rbx*4]
    ; r8 = dsu_parent, r9 = dsu_size (passed implicitly, hoisted)
    call    .dsu_union

    inc     ebx
    jmp     .p1_loop

.p1_done:
    ; ========================================================================
    ; Find top 3 component sizes in single pass (no sorting needed)
    ; Registers: rax=top1, rcx=top2, rdx=top3
    ; ========================================================================
    lea     r8, [rel dsu_parent]
    lea     r9, [rel dsu_size]
    xor     eax, eax                    ; top1 = 0
    xor     ecx, ecx                    ; top2 = 0
    xor     edx, edx                    ; top3 = 0
    xor     ebx, ebx

.find_top3:
    cmp     ebx, r12d
    jge     .top3_done

    ; Check if this is a root (parent[i] == i)
    cmp     [r8 + rbx*4], ebx
    jne     .skip_component

    ; Get size
    mov     esi, [r9 + rbx*4]

    ; Update top 3: if size > top1, shift down
    cmp     esi, eax
    jle     .try_top2
    mov     edx, ecx                    ; top3 = top2
    mov     ecx, eax                    ; top2 = top1
    mov     eax, esi                    ; top1 = size
    jmp     .skip_component

.try_top2:
    cmp     esi, ecx
    jle     .try_top3
    mov     edx, ecx                    ; top3 = top2
    mov     ecx, esi                    ; top2 = size
    jmp     .skip_component

.try_top3:
    cmp     esi, edx
    jle     .skip_component
    mov     edx, esi                    ; top3 = size

.skip_component:
    inc     ebx
    jmp     .find_top3

.top3_done:
    ; r14 = top1 * top2 * top3 (p1_result, repurposing r14)
    imul    eax, ecx
    imul    eax, edx
    mov     r14d, eax

.p1_result:
    ; r14 = part 1 result (preserved through part 2)

    ; ========================================================================
    ; Reinitialize DSU for Part 2
    ; ========================================================================
    lea     rdi, [rel dsu_parent]
    mov     ecx, r12d
    xor     eax, eax
.reinit_parent:
    mov     [rdi + rax*4], eax
    inc     eax
    cmp     eax, ecx
    jl      .reinit_parent

    lea     rdi, [rel dsu_size]
    mov     ecx, r12d
    mov     eax, 1
    rep     stosd

    ; ========================================================================
    ; Part 2: Full Kruskal's MST, track final connecting edge
    ; r15 repurposed: was p1_limit, now p2_result
    ; ========================================================================
    mov     r8d, r12d                   ; components = point_count
    xor     ebx, ebx                    ; edge index
    xor     r15, r15                    ; last_product (r15 = p2_result)

    lea     r9, [rel edges_i]
    lea     r10, [rel edges_j]
    lea     r11, [rel points_x]

.p2_loop:
    cmp     ebx, r13d
    jge     .p2_done
    cmp     r8d, 1
    jle     .p2_done

    mov     edi, [r9 + rbx*4]           ; edge.i
    mov     esi, [r10 + rbx*4]          ; edge.j
    call    .dsu_union

    test    eax, eax
    jz      .p2_next

    ; Merged: reload indices (still in L1 cache) and compute x[i] * x[j]
    mov     eax, [r9 + rbx*4]
    mov     ecx, [r10 + rbx*4]
    mov     rax, [r11 + rax*8]          ; x[i] (64-bit)
    imul    rax, [r11 + rcx*8]          ; x[i] * x[j]
    mov     r15, rax
    dec     r8d

.p2_next:
    inc     ebx
    jmp     .p2_loop

.p2_done:
    ; ========================================================================
    ; End timing
    ; ========================================================================
    mov     edi, 1                      ; CLOCK_MONOTONIC
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

    ; ========================================================================
    ; Print results
    ; ========================================================================
    ; printf(fmt, top3_product, final_join_x_product, elapsed_ms)
    mov     esi, r14d                   ; p1_result (32-bit suffices)
    mov     rdx, r15                    ; p2_result
    lea     rdi, [rel fmt_out]
    mov     eax, 1                      ; 1 XMM register used
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

; ============================================================================
; DSU find with path compression (halving)
; Input:  edi = node, rsi = dsu_parent base
; Output: eax = root
; Clobbers: rcx, rdx
; ============================================================================
.dsu_find:
    mov     eax, edi
.find_loop:
    mov     ecx, [rsi + rax*4]
    cmp     ecx, eax
    je      .find_done
    ; Path compression: parent[x] = parent[parent[x]]
    mov     edx, [rsi + rcx*4]
    mov     [rsi + rax*4], edx
    mov     eax, edx
    jmp     .find_loop
.find_done:
    ret

; ============================================================================
; DSU union by size
; Input:  edi = node a, esi = node b
; Output: eax = 1 if merged, 0 if same component
; Uses:   dsu_parent/dsu_size from globals (hoisted by caller when possible)
; Preserves: r8-r11 (caller may use for array bases)
; ============================================================================
.dsu_union:
    push    rbx
    push    r12
    push    r13
    mov     r12d, edi                   ; save node a
    mov     r13d, esi                   ; save node b

    ; Find root of a
    lea     rsi, [rel dsu_parent]
    mov     edi, r12d
    call    .dsu_find
    mov     ebx, eax                    ; root_a

    ; Find root of b
    lea     rsi, [rel dsu_parent]
    mov     edi, r13d
    call    .dsu_find
    ; eax = root_b

    cmp     ebx, eax
    je      .same_set

    ; Union by size: attach smaller to larger
    lea     rsi, [rel dsu_parent]
    lea     rcx, [rel dsu_size]
    mov     edx, [rcx + rbx*4]          ; size[root_a]
    mov     edi, [rcx + rax*4]          ; size[root_b]

    cmp     edx, edi
    jge     .union_do
    xchg    ebx, eax
    xchg    edx, edi

.union_do:
    mov     [rsi + rax*4], ebx          ; parent[root_b] = root_a
    add     edx, edi
    mov     [rcx + rbx*4], edx          ; size[root_a] += size[root_b]

    mov     eax, 1
    pop     r13
    pop     r12
    pop     rbx
    ret

.same_set:
    xor     eax, eax
    pop     r13
    pop     r12
    pop     rbx
    ret

section .note.GNU-stack noalloc noexec
