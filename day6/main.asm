; Day 6: Grid Block Evaluation
; Part 1: Parse numbers horizontally per block, apply operator
; Part 2: Read digits vertically per column, form numbers, apply operator

global main
extern clock_gettime
extern printf
extern perror
extern ns_since
extern read_file_all

%define CLOCK_MONOTONIC 1
%define BUF_SIZE 1048576
%define MAX_ROWS 16
%define MAX_COLS 8192
%define MAX_BLOCKS 1024

section .data
input_file:    db "input.txt", 0
fmt_out:       db "grand_total=%llu quantum_total=%llu elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2
grid:          resb MAX_ROWS * MAX_COLS
block_starts:  resd MAX_BLOCKS
block_ends:    resd MAX_BLOCKS

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
    mov     [rbp-48], rax               ; bytes read

    ; First pass: count rows and find max width
    lea     r12, [rel file_buf]
    mov     r13, [rbp-48]
    add     r13, r12                    ; end ptr

    xor     ebx, ebx                    ; row count
    xor     r14d, r14d                  ; max width
    xor     r15d, r15d                  ; current col

.count_loop:
    cmp     r12, r13
    jge     .count_done
    movzx   eax, byte [r12]
    inc     r12
    cmp     al, 10
    je      .end_line
    cmp     al, 13
    je      .count_loop
    inc     r15d
    jmp     .count_loop

.end_line:
    cmp     r15d, r14d
    cmovg   r14d, r15d
    inc     ebx
    xor     r15d, r15d
    jmp     .count_loop

.count_done:
    ; Handle last row if no trailing newline
    test    r15d, r15d
    jz      .no_extra
    cmp     r15d, r14d
    cmovg   r14d, r15d
    inc     ebx
.no_extra:
    mov     [rbp-52], ebx               ; rows
    mov     [rbp-56], r14d              ; width

    ; Fill grid with spaces
    lea     rdi, [rel grid]
    mov     al, ' '
    mov     ecx, ebx
    imul    ecx, MAX_COLS
    rep     stosb

    ; Second pass: copy to grid
    lea     r12, [rel file_buf]
    lea     r8, [rel grid]
    xor     ebx, ebx                    ; row
    xor     r15d, r15d                  ; col

.copy_loop:
    cmp     r12, r13
    jge     .copy_done
    movzx   eax, byte [r12]
    inc     r12
    cmp     al, 10
    je      .copy_newline
    cmp     al, 13
    je      .copy_loop
    ; Store char
    mov     edx, ebx
    imul    edx, MAX_COLS
    add     edx, r15d
    mov     [r8 + rdx], al
    inc     r15d
    jmp     .copy_loop

.copy_newline:
    inc     ebx
    xor     r15d, r15d
    jmp     .copy_loop

.copy_done:
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; Find blocks by scanning for empty columns
    mov     r9d, [rbp-52]               ; rows
    mov     r10d, [rbp-56]              ; width
    lea     r8, [rel grid]
    lea     r11, [rel block_starts]
    lea     r14, [rel block_ends]
    xor     r15d, r15d                  ; block count
    xor     ecx, ecx                    ; col
    mov     dword [rbp-60], 0           ; in_block

.scan_cols:
    cmp     ecx, r10d
    jge     .scan_done

    ; Check if column c is all spaces
    xor     edx, edx                    ; row
    mov     esi, 1                      ; assume empty
.check_col:
    cmp     edx, r9d
    jge     .col_checked
    mov     eax, edx
    imul    eax, MAX_COLS
    add     eax, ecx
    movzx   edi, byte [r8 + rax]
    cmp     dil, ' '
    jne     .not_empty
    inc     edx
    jmp     .check_col
.not_empty:
    xor     esi, esi
.col_checked:
    test    esi, esi
    jnz     .is_empty

    ; Non-empty column
    cmp     dword [rbp-60], 0
    jne     .next_col
    mov     dword [rbp-60], 1
    mov     [r11 + r15*4], ecx
    jmp     .next_col

.is_empty:
    cmp     dword [rbp-60], 0
    je      .next_col
    mov     [r14 + r15*4], ecx
    inc     r15d
    mov     dword [rbp-60], 0

.next_col:
    inc     ecx
    jmp     .scan_cols

.scan_done:
    ; Close last block if open
    cmp     dword [rbp-60], 0
    je      .blocks_done
    mov     ecx, r10d
    mov     [r14 + r15*4], ecx
    inc     r15d

.blocks_done:
    mov     [rbp-64], r15d              ; block_count

    ; Part 1: Process blocks
    xor     r12d, r12d                  ; block idx
    mov     qword [rbp-72], 0           ; p1_total

.p1_block:
    cmp     r12d, [rbp-64]
    jge     .p1_done

    lea     rax, [rel block_starts]
    mov     ecx, [rax + r12*4]          ; start
    mov     [rbp-80], ecx
    lea     rax, [rel block_ends]
    mov     edx, [rax + r12*4]          ; end
    mov     [rbp-84], edx

    ; Find operator in last row
    mov     eax, [rbp-52]
    dec     eax                         ; last row
    mov     [rbp-88], eax
    lea     r8, [rel grid]
    mov     ecx, [rbp-80]
    xor     edi, edi                    ; op: 0=add, 1=mul

.find_op1:
    cmp     ecx, [rbp-84]
    jge     .got_op1
    mov     eax, [rbp-88]
    imul    eax, MAX_COLS
    add     eax, ecx
    movzx   edx, byte [r8 + rax]
    cmp     dl, '+'
    je      .got_op1
    cmp     dl, '*'
    jne     .next_op1
    mov     edi, 1
    jmp     .got_op1
.next_op1:
    inc     ecx
    jmp     .find_op1

.got_op1:
    mov     [rbp-92], edi               ; save operator

    ; Init accumulator
    mov     r14, 1
    test    edi, edi
    jnz     .acc1_ok
    xor     r14d, r14d
.acc1_ok:

    ; Process each row except last
    xor     r15d, r15d                  ; row
.p1_row:
    cmp     r15d, [rbp-88]
    jge     .p1_row_done

    ; Parse number from this row, block columns
    xor     r9, r9                      ; num
    xor     r10d, r10d                  ; found
    mov     ecx, [rbp-80]               ; col
    lea     r8, [rel grid]

.p1_parse:
    cmp     ecx, [rbp-84]
    jge     .p1_parsed
    mov     eax, r15d
    imul    eax, MAX_COLS
    add     eax, ecx
    movzx   edx, byte [r8 + rax]
    cmp     dl, '0'
    jb      .p1_skip
    cmp     dl, '9'
    ja      .p1_skip
    sub     dl, '0'
    imul    r9, r9, 10
    movzx   edx, dl
    add     r9, rdx
    mov     r10d, 1
.p1_skip:
    inc     ecx
    jmp     .p1_parse

.p1_parsed:
    test    r10d, r10d
    jz      .p1_next_row
    cmp     dword [rbp-92], 0
    jne     .p1_mul
    add     r14, r9
    jmp     .p1_next_row
.p1_mul:
    imul    r14, r9

.p1_next_row:
    inc     r15d
    jmp     .p1_row

.p1_row_done:
    add     [rbp-72], r14
    inc     r12d
    jmp     .p1_block

.p1_done:
    ; Part 2: Process blocks, columns right to left
    xor     r12d, r12d
    mov     qword [rbp-104], 0          ; p2_total

.p2_block:
    cmp     r12d, [rbp-64]
    jge     .p2_done

    lea     rax, [rel block_starts]
    mov     ecx, [rax + r12*4]
    mov     [rbp-80], ecx
    lea     rax, [rel block_ends]
    mov     edx, [rax + r12*4]
    mov     [rbp-84], edx

    ; Find operator
    mov     eax, [rbp-52]
    dec     eax
    mov     [rbp-88], eax
    lea     r8, [rel grid]
    mov     ecx, [rbp-80]
    xor     edi, edi

.find_op2:
    cmp     ecx, [rbp-84]
    jge     .got_op2
    mov     eax, [rbp-88]
    imul    eax, MAX_COLS
    add     eax, ecx
    movzx   edx, byte [r8 + rax]
    cmp     dl, '+'
    je      .got_op2
    cmp     dl, '*'
    jne     .next_op2
    mov     edi, 1
    jmp     .got_op2
.next_op2:
    inc     ecx
    jmp     .find_op2

.got_op2:
    mov     [rbp-92], edi

    ; Init accumulator
    mov     r14, 1
    test    edi, edi
    jnz     .acc2_ok
    xor     r14d, r14d
.acc2_ok:

    ; Columns right to left
    mov     ecx, [rbp-84]
    dec     ecx                         ; end - 1

.p2_col:
    cmp     ecx, [rbp-80]
    jl      .p2_col_done
    mov     [rbp-96], ecx               ; save col

    ; Collect digits vertically
    xor     r9, r9                      ; num
    xor     r10d, r10d                  ; found
    xor     r15d, r15d                  ; row
    lea     r8, [rel grid]

.p2_vert:
    cmp     r15d, [rbp-88]
    jge     .p2_vert_done
    mov     eax, r15d
    imul    eax, MAX_COLS
    add     eax, [rbp-96]
    movzx   edx, byte [r8 + rax]
    cmp     dl, '0'
    jb      .p2_vnext
    cmp     dl, '9'
    ja      .p2_vnext
    sub     dl, '0'
    imul    r9, r9, 10
    movzx   edx, dl
    add     r9, rdx
    mov     r10d, 1
.p2_vnext:
    inc     r15d
    jmp     .p2_vert

.p2_vert_done:
    test    r10d, r10d
    jz      .p2_next_col
    cmp     dword [rbp-92], 0
    jne     .p2_mul
    add     r14, r9
    jmp     .p2_next_col
.p2_mul:
    imul    r14, r9

.p2_next_col:
    mov     ecx, [rbp-96]
    dec     ecx
    jmp     .p2_col

.p2_col_done:
    add     [rbp-104], r14
    inc     r12d
    jmp     .p2_block

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

    mov     rsi, [rbp-72]
    mov     rdx, [rbp-104]
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
