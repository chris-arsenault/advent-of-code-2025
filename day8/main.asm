; Day 8: 3D MST with DSU
; Part 1: Product of top 3 component sizes after 1000 edges
; Part 2: Product of x coords of final MST edge

global main
extern clock_gettime
extern printf
extern perror
extern ns_since
extern read_file_all
extern parse_uint64
extern skip_non_digits
extern sort_edges_3

%define CLOCK_MONOTONIC 1
%define BUF_SIZE 1048576
%define MAX_POINTS 1024
%define MAX_EDGES 600000

section .data
input_file:    db "input.txt", 0
fmt_out:       db "top3_product=%llu final_join_x_product=%llu elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2
points_x:      resq MAX_POINTS
points_y:      resq MAX_POINTS
points_z:      resq MAX_POINTS
edges_dist:    resq MAX_EDGES
edges_i:       resd MAX_EDGES
edges_j:       resd MAX_EDGES
dsu_parent:    resd MAX_POINTS
dsu_size:      resd MAX_POINTS
sizes_buf:     resq MAX_POINTS

section .text

main:
    push    rbp
    mov     rbp, rsp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, 104

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
    mov     [rbp-48], rax

    ; Parse points
    lea     r12, [rel file_buf]
    mov     r13, [rbp-48]
    add     r13, r12

    xor     ebx, ebx                    ; point count

.parse_points:
    cmp     r12, r13
    jge     .points_done

    mov     rdi, r12
    mov     rsi, r13
    call    skip_non_digits
    mov     r12, rax
    cmp     r12, r13
    jge     .points_done

    ; Parse x
    lea     rdi, [rbp-56]
    mov     [rbp-56], r12
    mov     rsi, r13
    call    parse_uint64
    mov     r12, [rbp-56]
    lea     rcx, [rel points_x]
    mov     [rcx + rbx*8], rax

    ; Parse y
    mov     rdi, r12
    mov     rsi, r13
    call    skip_non_digits
    mov     r12, rax
    lea     rdi, [rbp-56]
    mov     [rbp-56], r12
    mov     rsi, r13
    call    parse_uint64
    mov     r12, [rbp-56]
    lea     rcx, [rel points_y]
    mov     [rcx + rbx*8], rax

    ; Parse z
    mov     rdi, r12
    mov     rsi, r13
    call    skip_non_digits
    mov     r12, rax
    lea     rdi, [rbp-56]
    mov     [rbp-56], r12
    mov     rsi, r13
    call    parse_uint64
    mov     r12, [rbp-56]
    lea     rcx, [rel points_z]
    mov     [rcx + rbx*8], rax

    inc     ebx
    cmp     ebx, MAX_POINTS
    jl      .parse_points

.points_done:
    mov     [rbp-60], ebx

    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; Build edges
    mov     r14d, [rbp-60]
    xor     r15d, r15d                  ; edge count

    xor     ebx, ebx
.edge_i:
    cmp     ebx, r14d
    jge     .edges_done

    mov     ecx, ebx
    inc     ecx

.edge_j:
    cmp     ecx, r14d
    jge     .next_i

    ; Calculate dist^2
    lea     r8, [rel points_x]
    lea     r9, [rel points_y]
    lea     r10, [rel points_z]

    mov     rax, [r8 + rbx*8]
    sub     rax, [r8 + rcx*8]
    imul    rax, rax

    mov     rdx, [r9 + rbx*8]
    sub     rdx, [r9 + rcx*8]
    imul    rdx, rdx
    add     rax, rdx

    mov     rdx, [r10 + rbx*8]
    sub     rdx, [r10 + rcx*8]
    imul    rdx, rdx
    add     rax, rdx

    ; Store edge
    lea     rdi, [rel edges_dist]
    mov     [rdi + r15*8], rax
    lea     rdi, [rel edges_i]
    mov     [rdi + r15*4], ebx
    lea     rdi, [rel edges_j]
    mov     [rdi + r15*4], ecx

    inc     r15d
    inc     ecx
    jmp     .edge_j

.next_i:
    inc     ebx
    jmp     .edge_i

.edges_done:
    mov     [rbp-64], r15d

    ; Sort edges using quicksort from shared utils
    lea     rdi, [rel edges_dist]
    lea     rsi, [rel edges_i]
    lea     rdx, [rel edges_j]
    mov     ecx, r15d
    call    sort_edges_3
    ; Initialize DSU using rep stosd for fast array initialization
    ; First, initialize dsu_parent[i] = i for all i
    lea     rdi, [rel dsu_parent]
    mov     ecx, [rbp-60]
    xor     eax, eax
.init_parent:
    cmp     eax, ecx
    jge     .init_size
    mov     [rdi + rax*4], eax
    inc     eax
    jmp     .init_parent

.init_size:
    ; Initialize dsu_size[i] = 1 for all i using rep stosd
    lea     rdi, [rel dsu_size]
    mov     ecx, [rbp-60]
    mov     eax, 1
    rep     stosd

.dsu_ready:
    ; Part 1: Process first 1000 edges
    mov     ecx, 1000
    cmp     ecx, [rbp-64]
    cmovg   ecx, [rbp-64]
    mov     [rbp-84], ecx

    xor     ebx, ebx
.p1_loop:
    cmp     ebx, [rbp-84]
    jge     .p1_done

    lea     r8, [rel edges_i]
    lea     r9, [rel edges_j]
    mov     edi, [r8 + rbx*4]
    mov     esi, [r9 + rbx*4]
    call    .dsu_union

    inc     ebx
    jmp     .p1_loop

.p1_done:
    ; Collect component sizes
    xor     r15d, r15d
    mov     r14d, [rbp-60]
    xor     ebx, ebx

.collect_sizes:
    cmp     ebx, r14d
    jge     .sizes_collected

    mov     edi, ebx
    call    .dsu_find
    cmp     eax, ebx
    jne     .skip_size

    lea     rdi, [rel dsu_size]
    mov     eax, [rdi + rbx*4]
    lea     rsi, [rel sizes_buf]
    mov     [rsi + r15*8], rax
    inc     r15d

.skip_size:
    inc     ebx
    jmp     .collect_sizes

.sizes_collected:
    ; Sort sizes descending (bubble sort)
    mov     r14d, r15d
    dec     r14d

.bubble_outer:
    test    r14d, r14d
    jz      .sizes_sorted
    xor     edx, edx
    xor     ebx, ebx

.bubble_inner:
    cmp     ebx, r14d
    jge     .bubble_check
    lea     rsi, [rel sizes_buf]
    mov     rax, [rsi + rbx*8]
    mov     rcx, [rsi + rbx*8 + 8]
    cmp     rax, rcx
    jge     .no_swap
    mov     [rsi + rbx*8], rcx
    mov     [rsi + rbx*8 + 8], rax
    mov     edx, 1
.no_swap:
    inc     ebx
    jmp     .bubble_inner

.bubble_check:
    test    edx, edx
    jnz     .bubble_outer

.sizes_sorted:
    ; Calculate product of top 3
    mov     r14, 1
    lea     rsi, [rel sizes_buf]
    cmp     r15d, 0
    jle     .p1_result
    imul    r14, [rsi]
    cmp     r15d, 1
    jle     .p1_result
    imul    r14, [rsi + 8]
    cmp     r15d, 2
    jle     .p1_result
    imul    r14, [rsi + 16]

.p1_result:
    mov     [rbp-96], r14

    ; Reinitialize DSU for part 2 using rep stosd
    lea     rdi, [rel dsu_parent]
    mov     ecx, [rbp-60]
    xor     eax, eax
.reinit_parent:
    cmp     eax, ecx
    jge     .reinit_size
    mov     [rdi + rax*4], eax
    inc     eax
    jmp     .reinit_parent

.reinit_size:
    lea     rdi, [rel dsu_size]
    mov     ecx, [rbp-60]
    mov     eax, 1
    rep     stosd

.p2_ready:
    mov     r14d, [rbp-60]              ; components
    xor     ebx, ebx                    ; edge idx
    xor     r15, r15                    ; last product

.p2_loop:
    cmp     ebx, [rbp-64]
    jge     .p2_done
    cmp     r14d, 1
    jle     .p2_done

    lea     r8, [rel edges_i]
    lea     r9, [rel edges_j]
    mov     r12d, [r8 + rbx*4]
    mov     r13d, [r9 + rbx*4]

    mov     edi, r12d
    mov     esi, r13d
    call    .dsu_union

    test    eax, eax
    jz      .p2_next

    ; Merged
    lea     rsi, [rel points_x]
    mov     rax, [rsi + r12*8]
    imul    rax, [rsi + r13*8]
    mov     r15, rax
    dec     r14d

.p2_next:
    inc     ebx
    jmp     .p2_loop

.p2_done:
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts1]
    call    clock_gettime

    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since
    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    mov     rsi, [rbp-96]
    mov     rdx, r15
    lea     rdi, [rel fmt_out]
    mov     eax, 1
    call    printf
    xor     eax, eax

.exit:
    add     rsp, 104
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

; DSU find with path compression
.dsu_find:
    lea     rsi, [rel dsu_parent]
    mov     eax, edi
.find_loop:
    mov     ecx, [rsi + rax*4]
    cmp     ecx, eax
    je      .find_done
    mov     edx, [rsi + rcx*4]
    mov     [rsi + rax*4], edx
    mov     eax, edx
    jmp     .find_loop
.find_done:
    ret

; DSU union - returns 1 if merged, 0 if same
.dsu_union:
    push    rbx
    push    r12
    push    r13

    mov     r12d, edi
    mov     r13d, esi

    mov     edi, r12d
    call    .dsu_find
    mov     ebx, eax

    mov     edi, r13d
    call    .dsu_find

    cmp     ebx, eax
    je      .same_set

    lea     rsi, [rel dsu_parent]
    lea     rcx, [rel dsu_size]
    mov     edx, [rcx + rbx*4]
    mov     r8d, [rcx + rax*4]
    cmp     edx, r8d
    jge     .union_do
    xchg    ebx, eax
    xchg    edx, r8d
.union_do:
    mov     [rsi + rax*4], ebx
    add     edx, r8d
    mov     [rcx + rbx*4], edx

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

section .note.GNU-stack noalloc noexec nowrite progbits
