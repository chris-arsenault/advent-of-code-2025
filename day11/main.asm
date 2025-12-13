; Day 11: Graph Path Counting
; Part 1: Count paths from "you" to "out"
; Part 2: a1*a2*a3 + b1*b2*b3 where paths go through svr,dac,fft

global main
extern clock_gettime
extern printf
extern perror
extern ns_since
extern read_file_all

%define CLOCK_MONOTONIC 1
%define BUF_SIZE 1048576
%define MAX_NODES 1024
%define MAX_EDGES 8192

section .data
input_file:    db "input.txt", 0
fmt_out:       db "paths_you_to_out=%llu paths_svr_via_dac_fft=%llu elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0
node_you:      db "you", 0
node_out:      db "out", 0
node_svr:      db "svr", 0
node_dac:      db "dac", 0
node_fft:      db "fft", 0

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2
; Node names (4 bytes each: 3 chars + null)
node_names:    resd MAX_NODES
node_count:    resd 1
; Adjacency list in CSR format
adj_ptr:       resd MAX_NODES + 1
adj_list:      resd MAX_EDGES
edge_count:    resd 1
write_pos:     resd MAX_NODES           ; temp write positions
; Memoization array (-1 = not computed)
memo:          resq MAX_NODES
; DFS stack
dfs_stack:     resd MAX_NODES
dfs_top:       resd 1

section .text

;------------------------------------------------------------------------------
; int find_or_add_node(char* name)
; Returns node ID (0-based)
; Input: rdi = pointer to 3-char name
; Output: eax = node ID
;------------------------------------------------------------------------------
find_or_add_node:
    push    rbx
    push    r12

    ; Load 3 chars as a dword (with padding)
    movzx   r12d, byte [rdi]
    movzx   eax, byte [rdi+1]
    shl     eax, 8
    or      r12d, eax
    movzx   eax, byte [rdi+2]
    shl     eax, 16
    or      r12d, eax                   ; r12d = name as dword

    ; Search existing nodes
    lea     rdi, [rel node_names]
    mov     ecx, [rel node_count]
    xor     ebx, ebx
.search:
    cmp     ebx, ecx
    jge     .add_new
    cmp     [rdi + rbx*4], r12d
    je      .found
    inc     ebx
    jmp     .search

.add_new:
    ; Add new node
    mov     [rdi + rcx*4], r12d
    mov     eax, ecx
    inc     ecx
    mov     [rel node_count], ecx
    pop     r12
    pop     rbx
    ret

.found:
    mov     eax, ebx
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; int find_node(char* name)
; Returns node ID or -1 if not found
;------------------------------------------------------------------------------
find_node:
    ; Load 3 chars as a dword
    movzx   edx, byte [rdi]
    movzx   eax, byte [rdi+1]
    shl     eax, 8
    or      edx, eax
    movzx   eax, byte [rdi+2]
    shl     eax, 16
    or      edx, eax                    ; edx = name as dword

    lea     rdi, [rel node_names]
    mov     ecx, [rel node_count]
    xor     eax, eax
.search:
    cmp     eax, ecx
    jge     .not_found
    cmp     [rdi + rax*4], edx
    je      .done
    inc     eax
    jmp     .search
.not_found:
    mov     eax, -1
.done:
    ret

;------------------------------------------------------------------------------
; uint64 count_paths_iterative(int from, int to)
; Iterative DFS with explicit stack - avoids recursion overhead
; Uses level-synchronous style: processes nodes as dependencies resolve
;------------------------------------------------------------------------------
count_paths:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    push    rbp
    mov     rbp, rsp
    sub     rsp, 32

    mov     [rbp-4], edi                ; from
    mov     [rbp-8], esi                ; to (target)

    ; Clear memo array to -1
    lea     rdi, [rel memo]
    mov     ecx, [rel node_count]
    mov     rax, -1
.clear_memo:
    test    ecx, ecx
    jz      .memo_cleared
    mov     [rdi], rax
    add     rdi, 8
    dec     ecx
    jmp     .clear_memo
.memo_cleared:

    ; Set memo[target] = 1
    mov     eax, [rbp-8]
    lea     rdi, [rel memo]
    mov     qword [rdi + rax*8], 1

    ; Initialize stack with start node
    lea     r12, [rel dfs_stack]
    mov     eax, [rbp-4]
    mov     [r12], eax                  ; push start node
    mov     dword [rel dfs_top], 1      ; stack size = 1

    lea     r13, [rel memo]
    lea     r14, [rel adj_ptr]
    lea     r15, [rel adj_list]

.stack_loop:
    mov     ecx, [rel dfs_top]
    test    ecx, ecx
    jz      .stack_done

    ; Peek top of stack
    dec     ecx
    mov     ebx, [r12 + rcx*4]          ; current node

    ; Check if already computed
    mov     rax, [r13 + rbx*8]
    cmp     rax, -1
    jne     .pop_and_continue           ; already done, pop

    ; Check if all children are computed
    mov     r8d, [r14 + rbx*4]          ; adj start
    mov     r9d, [r14 + rbx*4 + 4]      ; adj end
    xor     r10d, r10d                  ; all_done = true
    mov     r11d, r8d                   ; iterator

.check_children:
    cmp     r11d, r9d
    jge     .children_checked
    mov     eax, [r15 + r11*4]          ; child node
    mov     rax, [r13 + rax*8]          ; memo[child]
    cmp     rax, -1
    jne     .child_ok
    ; Child not computed - push it
    mov     eax, [r15 + r11*4]
    mov     edx, [rel dfs_top]
    mov     [r12 + rdx*4], eax
    inc     dword [rel dfs_top]
    mov     r10d, 1                     ; mark not all done
.child_ok:
    inc     r11d
    jmp     .check_children

.children_checked:
    test    r10d, r10d
    jnz     .stack_loop                 ; some children pushed, loop again

    ; All children computed - sum their values
    xor     rax, rax                    ; sum = 0
    mov     r8d, [r14 + rbx*4]
    mov     r9d, [r14 + rbx*4 + 4]

.sum_children:
    cmp     r8d, r9d
    jge     .sum_done
    mov     ecx, [r15 + r8*4]           ; child node
    add     rax, [r13 + rcx*8]          ; sum += memo[child]
    inc     r8d
    jmp     .sum_children

.sum_done:
    ; Store result and pop
    mov     [r13 + rbx*8], rax

.pop_and_continue:
    dec     dword [rel dfs_top]
    jmp     .stack_loop

.stack_done:
    ; Return memo[from]
    mov     eax, [rbp-4]
    mov     rax, [r13 + rax*8]

    add     rsp, 32
    pop     rbp
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; main
;------------------------------------------------------------------------------
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

    ; Initialize
    mov     dword [rel node_count], 0
    mov     dword [rel edge_count], 0

    ; First pass: collect all nodes
    lea     r12, [rel file_buf]
    mov     r13, [rbp-48]
    add     r13, r12                    ; end ptr

.parse_nodes_loop:
    cmp     r12, r13
    jge     .nodes_done

    ; Skip whitespace
    movzx   eax, byte [r12]
    cmp     al, ' '
    je      .skip_ws_node
    cmp     al, 10
    je      .skip_ws_node
    cmp     al, 13
    je      .skip_ws_node
    cmp     al, ':'
    je      .skip_ws_node
    jmp     .parse_node_name

.skip_ws_node:
    inc     r12
    jmp     .parse_nodes_loop

.parse_node_name:
    ; Check if we have 3 chars
    lea     rax, [r12 + 3]
    cmp     rax, r13
    jg      .nodes_done

    ; Add node
    mov     rdi, r12
    call    find_or_add_node

    add     r12, 3
    jmp     .parse_nodes_loop

.nodes_done:
    ; Initialize adjacency pointers
    mov     ecx, [rel node_count]
    inc     ecx
    lea     rdi, [rel adj_ptr]
    xor     eax, eax
.init_adj:
    test    ecx, ecx
    jz      .adj_init_done
    mov     [rdi], eax
    add     rdi, 4
    dec     ecx
    jmp     .init_adj
.adj_init_done:

    ; Second pass: count edges per node
    lea     r12, [rel file_buf]
    mov     r13, [rbp-48]
    add     r13, r12

    mov     dword [rbp-52], -1          ; current source node

.count_edges_loop:
    cmp     r12, r13
    jge     .count_done

    movzx   eax, byte [r12]
    cmp     al, ' '
    je      .skip_ws_count
    cmp     al, 13
    je      .skip_ws_count
    cmp     al, 10
    je      .newline_count
    cmp     al, ':'
    je      .colon_count
    jmp     .check_node_count

.skip_ws_count:
    inc     r12
    jmp     .count_edges_loop

.newline_count:
    mov     dword [rbp-52], -1
    inc     r12
    jmp     .count_edges_loop

.colon_count:
    inc     r12
    jmp     .count_edges_loop

.check_node_count:
    ; Get node ID
    mov     rdi, r12
    call    find_node
    mov     ebx, eax
    add     r12, 3

    ; If this is first node on line, it's the source
    cmp     dword [rbp-52], -1
    jne     .is_dest_count
    mov     [rbp-52], ebx
    jmp     .count_edges_loop

.is_dest_count:
    ; Increment edge count for source
    mov     eax, [rbp-52]
    lea     rdi, [rel adj_ptr]
    inc     dword [rdi + rax*4 + 4]
    jmp     .count_edges_loop

.count_done:
    ; Convert counts to cumulative offsets
    mov     ecx, [rel node_count]
    inc     ecx                         ; need node_count+1 entries
    lea     rdi, [rel adj_ptr]
    xor     eax, eax
.cumsum:
    test    ecx, ecx
    jz      .cumsum_done
    mov     edx, [rdi]
    add     [rdi], eax
    add     eax, edx
    add     rdi, 4
    dec     ecx
    jmp     .cumsum
.cumsum_done:
    mov     [rel edge_count], eax

    ; Copy adj_ptr to write_pos (only need node_count entries for write positions)
    mov     ecx, [rel node_count]
    lea     rsi, [rel adj_ptr]
    lea     rdi, [rel write_pos]
.copy_ptr:
    test    ecx, ecx
    jz      .copy_done
    mov     eax, [rsi]
    mov     [rdi], eax
    add     rsi, 4
    add     rdi, 4
    dec     ecx
    jmp     .copy_ptr
.copy_done:

    ; Third pass: fill adjacency list
    ; Use write_pos as current position counters
    lea     r12, [rel file_buf]
    mov     r13, [rbp-48]
    add     r13, r12

    mov     dword [rbp-52], -1

.fill_edges_loop:
    cmp     r12, r13
    jge     .fill_done

    movzx   eax, byte [r12]
    cmp     al, ' '
    je      .skip_ws_fill
    cmp     al, 13
    je      .skip_ws_fill
    cmp     al, 10
    je      .newline_fill
    cmp     al, ':'
    je      .colon_fill
    jmp     .check_node_fill

.skip_ws_fill:
    inc     r12
    jmp     .fill_edges_loop

.newline_fill:
    mov     dword [rbp-52], -1
    inc     r12
    jmp     .fill_edges_loop

.colon_fill:
    inc     r12
    jmp     .fill_edges_loop

.check_node_fill:
    mov     rdi, r12
    call    find_node
    mov     ebx, eax
    add     r12, 3

    cmp     dword [rbp-52], -1
    jne     .is_dest_fill
    mov     [rbp-52], ebx
    jmp     .fill_edges_loop

.is_dest_fill:
    ; Add edge: source -> dest
    mov     eax, [rbp-52]               ; source
    lea     rdi, [rel write_pos]
    mov     ecx, [rdi + rax*4]          ; current position
    lea     rsi, [rel adj_list]
    mov     [rsi + rcx*4], ebx          ; store dest
    inc     dword [rdi + rax*4]         ; increment position
    jmp     .fill_edges_loop

.fill_done:
    ; Set adj_ptr[node_count] = edge_count (sentinel for CSR)
    mov     ecx, [rel node_count]
    mov     eax, [rel edge_count]
    lea     rdi, [rel adj_ptr]
    mov     [rdi + rcx*4], eax

    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; Find special node IDs
    lea     rdi, [rel node_you]
    call    find_node
    mov     [rbp-56], eax               ; id_you

    lea     rdi, [rel node_out]
    call    find_node
    mov     [rbp-60], eax               ; id_out

    lea     rdi, [rel node_svr]
    call    find_node
    mov     [rbp-64], eax               ; id_svr

    lea     rdi, [rel node_dac]
    call    find_node
    mov     [rbp-68], eax               ; id_dac

    lea     rdi, [rel node_fft]
    call    find_node
    mov     [rbp-72], eax               ; id_fft

    ; Part 1: count_paths(you, out)
    mov     edi, [rbp-56]
    mov     esi, [rbp-60]
    call    count_paths
    mov     [rbp-80], rax               ; p1

    ; Part 2: a1*a2*a3 + b1*b2*b3
    ; a1 = count_paths(svr, dac)
    mov     edi, [rbp-64]
    mov     esi, [rbp-68]
    call    count_paths
    mov     [rbp-88], rax               ; a1

    ; a2 = count_paths(dac, fft)
    mov     edi, [rbp-68]
    mov     esi, [rbp-72]
    call    count_paths
    mov     [rbp-96], rax               ; a2

    ; a3 = count_paths(fft, out)
    mov     edi, [rbp-72]
    mov     esi, [rbp-60]
    call    count_paths
    mov     r14, rax                    ; a3

    ; Compute a1*a2*a3
    mov     rax, [rbp-88]
    imul    rax, [rbp-96]
    imul    rax, r14
    mov     r15, rax                    ; a1*a2*a3

    ; b1 = count_paths(svr, fft)
    mov     edi, [rbp-64]
    mov     esi, [rbp-72]
    call    count_paths
    mov     [rbp-88], rax               ; b1

    ; b2 = count_paths(fft, dac)
    mov     edi, [rbp-72]
    mov     esi, [rbp-68]
    call    count_paths
    mov     [rbp-96], rax               ; b2

    ; b3 = count_paths(dac, out)
    mov     edi, [rbp-68]
    mov     esi, [rbp-60]
    call    count_paths
    mov     r14, rax                    ; b3

    ; Compute b1*b2*b3 and add to result
    mov     rax, [rbp-88]
    imul    rax, [rbp-96]
    imul    rax, r14
    add     r15, rax                    ; p2 = a1*a2*a3 + b1*b2*b3

    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts1]
    call    clock_gettime

    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since
    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    mov     rsi, [rbp-80]               ; p1
    mov     rdx, r15                    ; p2
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

section .note.GNU-stack noalloc noexec nowrite progbits
