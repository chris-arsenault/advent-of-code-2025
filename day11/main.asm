; Day 11: DAG Path Counting
; Part 1: Count paths from "you" to "out"
; Part 2: a1*a2*a3 + b1*b2*b3 where paths go through svr,dac,fft
;
; Optimized hand-written x86-64 assembly:
; - Generation-based memoization (avoids clearing memo array each call)
; - Hash table for O(1) average node lookup
; - Unaligned 32-bit load for 3-byte name packing
; - Fused successor check + sum loop
; - test/js for -1 checks (fewer µops than cmp imm)

global main
extern printf
extern perror
extern read_file_all
extern clock_gettime
extern ns_since

%define BUF_SIZE 1048576
%define MAX_NODES 1024
%define MAX_EDGES 8192
%define HASH_SIZE 2048              ; power of 2, > 2*MAX_NODES for low collision
%define HASH_MASK (HASH_SIZE - 1)

section .data
    align 8
input_file:    db "input.txt", 0
fmt_out:       db "paths_you_to_out=%llu paths_svr_via_dac_fft=%llu elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
node_you:      db "you", 0
node_out:      db "out", 0
node_svr:      db "svr", 0
node_dac:      db "dac", 0
node_fft:      db "fft", 0
one_million:   dq 1000000.0

section .bss
    align 64
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2
; Node names packed as 3-byte (low 24 bits of dword)
node_names:    resd MAX_NODES
node_count:    resd 1
; Hash table: maps packed_name -> node_id+1 (0 = empty slot)
; Using linear probing for simplicity
hash_table:    resd HASH_SIZE
; CSR adjacency: adj_ptr[i] to adj_ptr[i+1] gives outgoing edge range
adj_ptr:       resd MAX_NODES + 1
adj_list:      resd MAX_EDGES
edge_count:    resd 1
write_pos:     resd MAX_NODES
; Generation-based memoization (avoids clearing array each call)
memo_value:    resq MAX_NODES       ; path count values
memo_gen:      resd MAX_NODES       ; generation when value was computed
current_gen:   resd 1               ; current generation counter
; DFS work stack
dfs_stack:     resd MAX_NODES

section .text

;------------------------------------------------------------------------------
; pack_name: Load 3-char name as masked 24-bit value
; Input:  rdi = pointer to 3-char name
; Output: eax = packed name (low 24 bits)
; Single unaligned load + mask - faster than 3 loads + shifts
;------------------------------------------------------------------------------
%macro PACK_NAME 0
    mov     eax, [rdi]
    and     eax, 0x00FFFFFF
%endmacro

;------------------------------------------------------------------------------
; hash_name: Simple multiplicative hash for 24-bit packed name
; Input:  eax = packed name
; Output: eax = hash value (0 to HASH_SIZE-1)
;------------------------------------------------------------------------------
%macro HASH_NAME 0
    imul    eax, eax, 2654435761    ; Knuth multiplicative hash
    shr     eax, 32 - 11            ; HASH_SIZE = 2048 = 2^11
%endmacro

;------------------------------------------------------------------------------
; find_or_add_node: Look up or insert via hash table
; Input:  rdi = pointer to 3-char name
; Output: eax = node ID (0-based)
; Clobbers: rcx, rdx, r8, r9
;------------------------------------------------------------------------------
find_or_add_node:
    PACK_NAME                       ; eax = packed name
    mov     r8d, eax                ; save packed name in r8d

    HASH_NAME                       ; eax = hash
    lea     r9, [rel hash_table]

    ; Linear probe to find slot
.probe:
    mov     ecx, [r9 + rax*4]       ; hash_table[hash]
    test    ecx, ecx
    jz      .empty_slot             ; slot empty, insert here

    ; Check if this slot has our key
    dec     ecx                     ; stored as id+1, convert to id
    lea     rdx, [rel node_names]
    cmp     [rdx + rcx*4], r8d      ; compare packed names
    je      .found                  ; match, return this id

    ; Collision - linear probe next slot
    inc     eax
    and     eax, HASH_MASK
    jmp     .probe

.empty_slot:
    ; Insert new node
    mov     ecx, [rel node_count]
    lea     rdx, [rel node_names]
    mov     [rdx + rcx*4], r8d      ; store packed name

    ; Update hash table (store id+1)
    lea     edx, [ecx + 1]
    mov     [r9 + rax*4], edx

    ; Increment node count and return old count as id
    mov     eax, ecx
    inc     ecx
    mov     [rel node_count], ecx
    ret

.found:
    mov     eax, ecx                ; return found id
    ret

;------------------------------------------------------------------------------
; find_node: Look up via hash table (no insert)
; Input:  rdi = pointer to 3-char name
; Output: eax = node ID, or -1 if not found
; Clobbers: rcx, rdx, r8, r9
;------------------------------------------------------------------------------
find_node:
    PACK_NAME                       ; eax = packed name
    mov     r8d, eax                ; save packed name

    HASH_NAME                       ; eax = hash
    lea     r9, [rel hash_table]

.probe:
    mov     ecx, [r9 + rax*4]
    test    ecx, ecx
    jz      .not_found              ; empty slot = not in table

    dec     ecx                     ; convert to id
    lea     rdx, [rel node_names]
    cmp     [rdx + rcx*4], r8d
    je      .found

    inc     eax
    and     eax, HASH_MASK
    jmp     .probe

.not_found:
    mov     eax, -1
    ret

.found:
    mov     eax, ecx
    ret

;------------------------------------------------------------------------------
; count_paths: Count paths from 'start' to 'target' in DAG
; Input:  edi = start node ID
;         esi = target node ID
; Output: rax = number of paths
;
; Uses generation-based memoization to avoid clearing memo array.
; Fused successor check + sum when all successors ready.
;
; Register allocation:
;   r10d = start node (callee-saved via stack)
;   ebx  = stack top index
;   r12  = memo_value base
;   r13  = adj_ptr base
;   r14  = adj_list base
;   r15  = dfs_stack base
;   ecx  = current node (in hot loop)
;   r8d  = edge_start, r9d = edge_end
;------------------------------------------------------------------------------
count_paths:
    push    rbx
    push    r10
    push    r12
    push    r13
    push    r14
    push    r15

    mov     r10d, edi               ; start node
    mov     r8d, esi                ; target node (temp)

    ; Increment generation (avoids clearing memo array!)
    lea     rax, [rel current_gen]
    inc     dword [rax]
    mov     r11d, [rax]             ; r11d = this call's generation

    ; Set memo[target] = 1 for this generation
    lea     r12, [rel memo_value]
    lea     rax, [rel memo_gen]
    mov     qword [r12 + r8*8], 1   ; memo_value[target] = 1
    mov     [rax + r8*4], r11d      ; memo_gen[target] = current_gen

    ; Set up register pointers
    lea     r13, [rel adj_ptr]
    lea     r14, [rel adj_list]
    lea     r15, [rel dfs_stack]
    lea     r9, [rel memo_gen]      ; keep memo_gen base in r9 initially

    ; Push start node
    mov     dword [r15], r10d
    mov     ebx, 1                  ; stack_top = 1

.dfs_loop:
    test    ebx, ebx
    jz      .dfs_done

    ; Peek top of stack
    mov     eax, ebx
    dec     eax
    mov     ecx, [r15 + rax*4]      ; current node

    ; Check if already computed this generation
    ; Use test+js pattern: memo_gen[node] == current_gen means valid
    lea     rax, [rel memo_gen]
    cmp     [rax + rcx*4], r11d
    je      .pop_continue           ; already done, pop it

    ; Get adjacency range
    mov     r8d, [r13 + rcx*4]      ; edge_start
    mov     r9d, [r13 + rcx*4 + 4]  ; edge_end

    ; Fused loop: check successors AND accumulate sum
    ; If any successor not ready, push it and set need_more flag
    xor     eax, eax                ; sum = 0 (accumulate optimistically)
    xor     edx, edx                ; need_more = 0
    mov     esi, r8d                ; iterator

.check_and_sum:
    cmp     esi, r9d
    jge     .loop_done

    mov     edi, [r14 + rsi*4]      ; edi = successor node (keep in reg!)

    ; Check if successor computed this generation
    lea     r8, [rel memo_gen]
    cmp     [r8 + rdi*4], r11d
    jne     .need_successor

    ; Successor ready - add to sum
    add     rax, [r12 + rdi*8]
    inc     esi
    jmp     .check_and_sum

.need_successor:
    ; Push this successor (rbx is zero-extended from ebx operations)
    mov     [r15 + rbx*4], edi
    inc     ebx
    mov     edx, 1                  ; need_more = true
    inc     esi
    jmp     .check_and_sum

.loop_done:
    test    edx, edx
    jnz     .dfs_loop               ; some successors pushed, loop again

    ; All successors ready - sum is in rax, store it
    mov     [r12 + rcx*8], rax      ; memo_value[node] = sum
    lea     rax, [rel memo_gen]
    mov     [rax + rcx*4], r11d     ; memo_gen[node] = current_gen

.pop_continue:
    dec     ebx
    jmp     .dfs_loop

.dfs_done:
    ; Return memo_value[start]
    mov     eax, r10d
    mov     rax, [r12 + rax*8]

    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     r10
    pop     rbx
    ret

;------------------------------------------------------------------------------
; main
;------------------------------------------------------------------------------
main:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, 64                 ; [rsp+0]=file_size, [rsp+8-24]=node IDs,
                                    ; [rsp+32]=p1, [rsp+40]=a1*a2*a3

    ; Start timing
    mov     edi, 1
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; Read input file
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
    mov     [rsp], rax
    mov     dword [rel node_count], 0
    mov     dword [rel edge_count], 0
    mov     dword [rel current_gen], 0

    ; Clear hash table
    lea     rdi, [rel hash_table]
    mov     ecx, HASH_SIZE
    xor     eax, eax
.clear_hash:
    mov     [rdi], eax
    add     rdi, 4
    dec     ecx
    jnz     .clear_hash

    ;=== PASS 1: Collect all unique node names ===
    lea     r12, [rel file_buf]
    mov     r13, [rsp]
    add     r13, r12

.parse_nodes:
    cmp     r12, r13
    jge     .nodes_done

    movzx   eax, byte [r12]
    cmp     al, ' '
    je      .skip_ws1
    cmp     al, 10
    je      .skip_ws1
    cmp     al, 13
    je      .skip_ws1
    cmp     al, ':'
    je      .skip_ws1

    lea     rax, [r12 + 3]
    cmp     rax, r13
    jg      .nodes_done

    mov     rdi, r12
    call    find_or_add_node
    add     r12, 3
    jmp     .parse_nodes

.skip_ws1:
    inc     r12
    jmp     .parse_nodes

.nodes_done:
    ; Initialize adj_ptr to zeros
    mov     ecx, [rel node_count]
    inc     ecx
    lea     rdi, [rel adj_ptr]
    xor     eax, eax
.init_adj:
    mov     [rdi], eax
    add     rdi, 4
    dec     ecx
    jnz     .init_adj

    ;=== PASS 2: Count edges per source node ===
    lea     r12, [rel file_buf]
    mov     r13, [rsp]
    add     r13, r12
    mov     r14d, -1                ; current_source = none

.count_edges:
    cmp     r12, r13
    jge     .count_done

    movzx   eax, byte [r12]
    cmp     al, ' '
    je      .skip_ws2
    cmp     al, 13
    je      .skip_ws2
    cmp     al, 10
    je      .newline2
    cmp     al, ':'
    je      .colon2

    mov     rdi, r12
    call    find_node
    mov     ebx, eax
    add     r12, 3

    ; test r14d for -1 using sign bit
    test    r14d, r14d
    js      .set_source

    ; Increment count for source
    lea     rdi, [rel adj_ptr]
    inc     dword [rdi + r14*4 + 4]
    jmp     .count_edges

.set_source:
    mov     r14d, ebx
    jmp     .count_edges

.skip_ws2:
    inc     r12
    jmp     .count_edges

.newline2:
    mov     r14d, -1
    inc     r12
    jmp     .count_edges

.colon2:
    inc     r12
    jmp     .count_edges

.count_done:
    ; Prefix sum
    mov     ecx, [rel node_count]
    inc     ecx
    lea     rdi, [rel adj_ptr]
    xor     eax, eax
.prefix_sum:
    mov     edx, [rdi]
    add     [rdi], eax
    add     eax, edx
    add     rdi, 4
    dec     ecx
    jnz     .prefix_sum
    mov     [rel edge_count], eax

    ; Copy adj_ptr to write_pos
    mov     ecx, [rel node_count]
    lea     rsi, [rel adj_ptr]
    lea     rdi, [rel write_pos]
.copy_pos:
    mov     eax, [rsi]
    mov     [rdi], eax
    add     rsi, 4
    add     rdi, 4
    dec     ecx
    jnz     .copy_pos

    ;=== PASS 3: Fill adjacency list ===
    lea     r12, [rel file_buf]
    mov     r13, [rsp]
    add     r13, r12
    mov     r14d, -1

.fill_edges:
    cmp     r12, r13
    jge     .fill_done

    movzx   eax, byte [r12]
    cmp     al, ' '
    je      .skip_ws3
    cmp     al, 13
    je      .skip_ws3
    cmp     al, 10
    je      .newline3
    cmp     al, ':'
    je      .colon3

    mov     rdi, r12
    call    find_node
    mov     ebx, eax
    add     r12, 3

    test    r14d, r14d
    js      .set_source3

    ; Add edge
    lea     rdi, [rel write_pos]
    mov     ecx, [rdi + r14*4]
    lea     rsi, [rel adj_list]
    mov     [rsi + rcx*4], ebx
    inc     dword [rdi + r14*4]
    jmp     .fill_edges

.set_source3:
    mov     r14d, ebx
    jmp     .fill_edges

.skip_ws3:
    inc     r12
    jmp     .fill_edges

.newline3:
    mov     r14d, -1
    inc     r12
    jmp     .fill_edges

.colon3:
    inc     r12
    jmp     .fill_edges

.fill_done:
    ; Finalize CSR sentinel
    mov     ecx, [rel node_count]
    mov     eax, [rel edge_count]
    lea     rdi, [rel adj_ptr]
    mov     [rdi + rcx*4], eax

    ;=== Find special node IDs ===
    lea     rdi, [rel node_you]
    call    find_node
    mov     [rsp+8], eax

    lea     rdi, [rel node_out]
    call    find_node
    mov     [rsp+12], eax

    lea     rdi, [rel node_svr]
    call    find_node
    mov     [rsp+16], eax

    lea     rdi, [rel node_dac]
    call    find_node
    mov     [rsp+20], eax

    lea     rdi, [rel node_fft]
    call    find_node
    mov     [rsp+24], eax

    ;=== Part 1: count_paths(you, out) ===
    mov     edi, [rsp+8]
    mov     esi, [rsp+12]
    call    count_paths
    mov     [rsp+32], rax

    ;=== Part 2: a1*a2*a3 + b1*b2*b3 ===
    mov     edi, [rsp+16]           ; svr
    mov     esi, [rsp+20]           ; dac
    call    count_paths
    mov     r12, rax                ; a1

    mov     edi, [rsp+20]           ; dac
    mov     esi, [rsp+24]           ; fft
    call    count_paths
    mov     r13, rax                ; a2

    mov     edi, [rsp+24]           ; fft
    mov     esi, [rsp+12]           ; out
    call    count_paths
    mov     r14, rax                ; a3

    imul    r12, r13
    imul    r12, r14
    mov     [rsp+40], r12           ; a1*a2*a3

    mov     edi, [rsp+16]           ; svr
    mov     esi, [rsp+24]           ; fft
    call    count_paths
    mov     r12, rax                ; b1

    mov     edi, [rsp+24]           ; fft
    mov     esi, [rsp+20]           ; dac
    call    count_paths
    mov     r13, rax                ; b2

    mov     edi, [rsp+20]           ; dac
    mov     esi, [rsp+12]           ; out
    call    count_paths
    mov     r14, rax                ; b3

    imul    r12, r13
    imul    r12, r14
    add     r12, [rsp+40]           ; p2 = a1*a2*a3 + b1*b2*b3

    ; End timing
    mov     edi, 1
    lea     rsi, [rel ts1]
    call    clock_gettime

    ; elapsed_ns = ns_since(&ts0, &ts1)
    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since

    ; Convert to ms in xmm0
    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    ;=== Print results ===
    mov     rsi, [rsp+32]
    mov     rdx, r12
    lea     rdi, [rel fmt_out]
    mov     eax, 1                  ; 1 float arg in xmm
    call    printf
    xor     eax, eax

.exit:
    add     rsp, 64
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
