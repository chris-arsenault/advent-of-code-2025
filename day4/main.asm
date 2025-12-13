; Day 4: Grid Neighbor Analysis
; Part 1: Count positions with < 4 neighbors (8-connected)
; Part 2: BFS to remove positions with < 4 neighbors, propagating

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

section .data
input_file:    db "input.txt", 0
fmt_out:       db "accessible=%d removable_total=%d elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

; 8-connected neighbor offsets (dr, dc)
neighbors:     dd -1, -1,  -1, 0,  -1, 1,  0, -1,  0, 1,  1, -1,  1, 0,  1, 1

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2
grid:          resb MAX_CELLS     ; 1 if '@', 0 otherwise
counts:        resb MAX_CELLS     ; neighbor counts
removed:       resb MAX_CELLS     ; 1 if removed
queue:         resd MAX_CELLS * 2 ; BFS queue (row, col pairs)

section .text

;------------------------------------------------------------------------------
; int count_neighbors(int row, int col, int rows, int cols)
; Count 8-connected neighbors that are '@'
; rdi = row, rsi = col, rdx = rows, rcx = cols
;------------------------------------------------------------------------------
count_neighbors:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15

    mov     r12d, edi           ; row
    mov     r13d, esi           ; col
    mov     r14d, edx           ; rows
    mov     r15d, ecx           ; cols

    xor     ebx, ebx            ; count = 0
    lea     r8, [rel neighbors]
    xor     ecx, ecx            ; neighbor index

.neighbor_loop:
    cmp     ecx, 8
    jge     .neighbor_done

    mov     eax, [r8 + rcx*8]       ; dr
    mov     edx, [r8 + rcx*8 + 4]   ; dc

    add     eax, r12d               ; nr = row + dr
    add     edx, r13d               ; nc = col + dc

    ; Bounds check
    cmp     eax, 0
    jl      .next_neighbor
    cmp     eax, r14d
    jge     .next_neighbor
    cmp     edx, 0
    jl      .next_neighbor
    cmp     edx, r15d
    jge     .next_neighbor

    ; Check if grid[nr][nc] is '@'
    imul    r9d, eax, MAX_COLS
    add     r9d, edx
    lea     r10, [rel grid]
    movzx   r11d, byte [r10 + r9]
    add     ebx, r11d               ; count += grid[nr][nc]

.next_neighbor:
    inc     ecx
    jmp     .neighbor_loop

.neighbor_done:
    mov     eax, ebx
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
    sub     rsp, 56

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
    mov     r14, rax                ; bytes read
    mov     dword [rbp-48], 0       ; rows
    mov     dword [rbp-52], 0       ; cols

    ; Clear grid
    lea     rdi, [rel grid]
    xor     eax, eax
    mov     ecx, MAX_CELLS
    rep     stosb

    ; Parse grid
    lea     r12, [rel file_buf]
    lea     r13, [rel file_buf]
    add     r13, r14

    xor     ebx, ebx                ; current row
    xor     r15d, r15d              ; current col
    mov     dword [rbp-56], 0       ; max_cols

.parse_loop:
    cmp     r12, r13
    jge     .parse_done

    movzx   eax, byte [r12]
    inc     r12

    cmp     al, 10                  ; newline
    je      .newline
    cmp     al, 13                  ; CR
    je      .parse_loop

    ; Check for '@'
    cmp     al, '@'
    jne     .not_at
    ; Set grid[row][col] = 1
    imul    eax, ebx, MAX_COLS
    add     eax, r15d
    lea     rcx, [rel grid]
    mov     byte [rcx + rax], 1
.not_at:
    inc     r15d
    ; Update max_cols
    cmp     r15d, [rbp-56]
    jle     .parse_loop
    mov     [rbp-56], r15d
    jmp     .parse_loop

.newline:
    inc     ebx                     ; next row
    xor     r15d, r15d              ; reset col
    jmp     .parse_loop

.parse_done:
    ; Handle last row if no trailing newline
    test    r15d, r15d
    jz      .no_extra_row
    inc     ebx
.no_extra_row:
    mov     [rbp-48], ebx           ; rows
    mov     eax, [rbp-56]
    mov     [rbp-52], eax           ; cols

    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; Compute neighbor counts
    lea     r8, [rel counts]
    mov     r9d, [rbp-48]           ; rows
    mov     r10d, [rbp-52]          ; cols

    xor     ebx, ebx                ; row
.count_row_loop:
    cmp     ebx, r9d
    jge     .counts_done
    xor     r15d, r15d              ; col
.count_col_loop:
    cmp     r15d, r10d
    jge     .next_count_row

    ; Check if cell has '@'
    imul    eax, ebx, MAX_COLS
    add     eax, r15d
    lea     rcx, [rel grid]
    movzx   edx, byte [rcx + rax]
    test    edx, edx
    jz      .skip_count

    ; Count neighbors
    mov     edi, ebx
    mov     esi, r15d
    mov     edx, r9d
    mov     ecx, r10d
    ; Save registers
    mov     [rbp-60], r9d
    mov     [rbp-64], r10d
    call    count_neighbors
    mov     r9d, [rbp-60]
    mov     r10d, [rbp-64]

    ; Store count
    imul    edx, ebx, MAX_COLS
    add     edx, r15d
    lea     rcx, [rel counts]
    mov     [rcx + rdx], al

.skip_count:
    inc     r15d
    jmp     .count_col_loop

.next_count_row:
    inc     ebx
    jmp     .count_row_loop

.counts_done:
    ; Part 1: Count positions with < 4 neighbors
    xor     r12d, r12d              ; accessible count
    lea     r8, [rel grid]
    lea     r9, [rel counts]

    xor     ebx, ebx                ; row
.p1_row_loop:
    cmp     ebx, [rbp-48]
    jge     .p1_done
    xor     r15d, r15d              ; col
.p1_col_loop:
    cmp     r15d, [rbp-52]
    jge     .p1_next_row

    imul    eax, ebx, MAX_COLS
    add     eax, r15d
    movzx   ecx, byte [r8 + rax]    ; grid[r][c]
    test    ecx, ecx
    jz      .p1_skip

    movzx   ecx, byte [r9 + rax]    ; counts[r][c]
    cmp     ecx, 4
    jge     .p1_skip
    inc     r12d

.p1_skip:
    inc     r15d
    jmp     .p1_col_loop

.p1_next_row:
    inc     ebx
    jmp     .p1_row_loop

.p1_done:
    mov     [rbp-68], r12d          ; save accessible

    ; Part 2: BFS removal
    ; Clear removed array
    lea     rdi, [rel removed]
    xor     eax, eax
    mov     ecx, MAX_CELLS
    rep     stosb

    ; Initialize queue with positions having < 4 neighbors
    lea     r8, [rel queue]
    xor     r11d, r11d              ; queue_tail
    lea     r9, [rel grid]
    lea     r10, [rel counts]

    xor     ebx, ebx                ; row
.init_q_row:
    cmp     ebx, [rbp-48]
    jge     .init_q_done
    xor     r15d, r15d              ; col
.init_q_col:
    cmp     r15d, [rbp-52]
    jge     .init_q_next_row

    imul    eax, ebx, MAX_COLS
    add     eax, r15d
    movzx   ecx, byte [r9 + rax]
    test    ecx, ecx
    jz      .init_q_skip

    movzx   ecx, byte [r10 + rax]
    cmp     ecx, 4
    jge     .init_q_skip

    ; Add to queue
    mov     [r8 + r11*8], ebx       ; row
    mov     [r8 + r11*8 + 4], r15d  ; col
    inc     r11d

.init_q_skip:
    inc     r15d
    jmp     .init_q_col

.init_q_next_row:
    inc     ebx
    jmp     .init_q_row

.init_q_done:
    xor     r12d, r12d              ; removed_count
    xor     r13d, r13d              ; queue_head

    ; BFS loop
.bfs_loop:
    cmp     r13d, r11d              ; head >= tail?
    jge     .bfs_done

    ; Pop from queue
    lea     r8, [rel queue]
    mov     ebx, [r8 + r13*8]       ; row
    mov     r15d, [r8 + r13*8 + 4]  ; col
    inc     r13d

    ; Check if already removed
    imul    eax, ebx, MAX_COLS
    add     eax, r15d
    lea     rcx, [rel removed]
    movzx   edx, byte [rcx + rax]
    test    edx, edx
    jnz     .bfs_loop

    ; Mark as removed
    mov     byte [rcx + rax], 1
    inc     r12d

    ; Update neighbors
    lea     r9, [rel neighbors]
    xor     r14d, r14d              ; neighbor index
.update_neighbors:
    cmp     r14d, 8
    jge     .bfs_loop

    mov     eax, [r9 + r14*8]       ; dr
    mov     edx, [r9 + r14*8 + 4]   ; dc
    add     eax, ebx                ; nr
    add     edx, r15d               ; nc

    ; Bounds check
    cmp     eax, 0
    jl      .next_update
    cmp     eax, [rbp-48]
    jge     .next_update
    cmp     edx, 0
    jl      .next_update
    cmp     edx, [rbp-52]
    jge     .next_update

    ; Check if in grid and not removed
    imul    ecx, eax, MAX_COLS
    add     ecx, edx
    lea     r8, [rel grid]
    movzx   esi, byte [r8 + rcx]
    test    esi, esi
    jz      .next_update

    lea     r8, [rel removed]
    movzx   esi, byte [r8 + rcx]
    test    esi, esi
    jnz     .next_update

    ; Decrement neighbor count
    lea     r8, [rel counts]
    movzx   esi, byte [r8 + rcx]
    dec     esi
    mov     [r8 + rcx], sil

    ; If count < 4, add to queue
    cmp     esi, 4
    jge     .next_update

    lea     r8, [rel queue]
    mov     [r8 + r11*8], eax       ; row
    mov     [r8 + r11*8 + 4], edx   ; col
    inc     r11d

.next_update:
    inc     r14d
    jmp     .update_neighbors

.bfs_done:
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts1]
    call    clock_gettime

    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since
    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    mov     esi, [rbp-68]           ; accessible
    mov     edx, r12d               ; removed_count
    lea     rdi, [rel fmt_out]
    call    printf
    xor     eax, eax

.exit:
    add     rsp, 56
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
