; Day 10: Button Press Optimization
; Part 1: Light toggle XOR puzzle (brute force)
; Part 2: Counter constraints (simplified greedy)

global main
extern clock_gettime
extern printf
extern perror
extern ns_since
extern read_file_all

%define CLOCK_MONOTONIC 1
%define BUF_SIZE 1048576
%define MAX_BUTTONS 16
%define MAX_COUNTERS 16

section .data
input_file:    db "input.txt", 0
fmt_out:       db "min_lights_presses=%d min_counter_presses=%d elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0
dbg_sol:       db "p2: n=%d sol[0]=%d sol[1]=%d sol[2]=%d sum=%d", 10, 0
dbg_mat:       db "n_btns=%d n_cnt=%d tgt[0]=%d tgt[1]=%d A[0][0]=%d A[0][n]=%d", 10, 0
dbg_btn:       db "found button, r15=%d at char '%c'", 10, 0

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2

section .text

;------------------------------------------------------------------------------
; Parse next integer from buffer
; Input: rdi = ptr to current position, rsi = end ptr
; Output: rax = value, rdi updated to past the number
;------------------------------------------------------------------------------
parse_int:
    push    rbx
    mov     rbx, rdi

    ; Skip non-digits
.skip:
    cmp     rbx, rsi
    jge     .no_num
    movzx   eax, byte [rbx]
    cmp     al, '0'
    jb      .not_digit
    cmp     al, '9'
    jbe     .found
.not_digit:
    inc     rbx
    jmp     .skip

.found:
    xor     eax, eax
.parse:
    cmp     rbx, rsi
    jge     .done
    movzx   ecx, byte [rbx]
    cmp     cl, '0'
    jb      .done
    cmp     cl, '9'
    ja      .done
    imul    eax, 10
    sub     cl, '0'
    add     eax, ecx
    inc     rbx
    jmp     .parse

.done:
    mov     rdi, rbx
    pop     rbx
    ret

.no_num:
    mov     rdi, rbx
    xor     eax, eax
    pop     rbx
    ret

;------------------------------------------------------------------------------
; Parse a single machine and solve Part 1
; Input: rdi = start of line, rsi = end of file
; Output: rax = min presses for lights, rdi = next line
;------------------------------------------------------------------------------
solve_p1_machine:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    push    rbp
    mov     rbp, rsp
    sub     rsp, 128

    mov     r12, rdi                    ; current pos
    mov     r13, rsi                    ; end

    ; Skip to '['
.find_bracket:
    cmp     r12, r13
    jge     .no_solution
    movzx   eax, byte [r12]
    cmp     al, '['
    je      .found_bracket
    inc     r12
    jmp     .find_bracket

.found_bracket:
    inc     r12                         ; skip '['

    ; Parse pattern to get target mask
    xor     r14d, r14d                  ; target_mask
    xor     ecx, ecx                    ; bit position
.parse_pattern:
    cmp     r12, r13
    jge     .no_solution
    movzx   eax, byte [r12]
    cmp     al, ']'
    je      .pattern_done
    cmp     al, '#'
    jne     .not_on
    mov     eax, 1
    mov     edx, ecx
    shl     eax, cl
    or      r14d, eax
.not_on:
    inc     ecx
    inc     r12
    jmp     .parse_pattern

.pattern_done:
    mov     [rbp-8], ecx                ; lights count
    mov     [rbp-16], r14d              ; target mask
    inc     r12                         ; skip ']'

    ; Parse buttons
    xor     r15d, r15d                  ; button count
    lea     rbx, [rbp-80]               ; button_masks array

.parse_buttons:
    ; Find next '(' or end of line
    cmp     r12, r13
    jge     .buttons_done
    movzx   eax, byte [r12]
    cmp     al, 10
    je      .buttons_done
    cmp     al, '{'
    je      .buttons_done
    cmp     al, '('
    je      .found_button
    inc     r12
    jmp     .parse_buttons

.found_button:
    inc     r12                         ; skip '('
    mov     dword [rbp-80 + r15*4], 0   ; Initialize this button's mask to 0

.parse_button_indices:
    cmp     r12, r13
    jge     .button_done
    movzx   eax, byte [r12]
    cmp     al, ')'
    je      .button_done
    cmp     al, ','
    je      .skip_comma
    cmp     al, ' '
    je      .skip_comma
    cmp     al, '0'
    jb      .skip_other
    cmp     al, '9'
    ja      .skip_other

    ; Parse index
    push    r15                         ; save button count
    mov     rdi, r12
    mov     rsi, r13
    call    parse_int
    mov     r12, rdi
    pop     r15

    ; Set bit if index < lights
    cmp     eax, [rbp-8]
    jge     .parse_button_indices
    mov     ecx, eax
    mov     edx, 1
    shl     edx, cl
    or      [rbp-80 + r15*4], edx
    jmp     .parse_button_indices

.skip_comma:
.skip_other:
    inc     r12
    jmp     .parse_button_indices

.button_done:
    cmp     r12, r13
    jge     .buttons_done
    inc     r12                         ; skip ')'
    inc     r15d                        ; button count++
    cmp     r15d, MAX_BUTTONS
    jl      .parse_buttons

.buttons_done:
    ; Skip to end of line
.skip_to_eol:
    cmp     r12, r13
    jge     .solve_p1
    movzx   eax, byte [r12]
    inc     r12
    cmp     al, 10
    jne     .skip_to_eol

.solve_p1:
    ; Enumerate all 2^n button subsets
    mov     [rbp-84], r15d              ; save button count
    mov     ecx, r15d
    mov     eax, 1
    shl     eax, cl                     ; 2^n
    mov     [rbp-88], eax               ; total subsets

    mov     dword [rbp-92], 999         ; best = large value (infinity)
    xor     ebx, ebx                    ; mask = 0

.enum_subsets:
    cmp     ebx, [rbp-88]
    jge     .p1_done

    ; Compute XOR state for this subset
    xor     r8d, r8d                    ; state = 0
    xor     ecx, ecx                    ; i = 0
.apply_buttons:
    cmp     ecx, [rbp-84]
    jge     .check_match

    mov     eax, ebx
    shr     eax, cl
    and     eax, 1
    jz      .next_button

    xor     r8d, [rbp-80 + rcx*4]       ; state ^= button_masks[i]

.next_button:
    inc     ecx
    jmp     .apply_buttons

.check_match:
    cmp     r8d, [rbp-16]               ; state == target_mask?
    jne     .next_subset

    ; Count bits in mask (popcount)
    mov     eax, ebx
    xor     ecx, ecx
.popcount:
    test    eax, eax
    jz      .popcount_done
    mov     edx, eax
    and     edx, 1
    add     ecx, edx
    shr     eax, 1
    jmp     .popcount

.popcount_done:
    cmp     ecx, [rbp-92]
    jge     .next_subset
    mov     [rbp-92], ecx               ; best = presses

.next_subset:
    inc     ebx
    jmp     .enum_subsets

.p1_done:
    mov     eax, [rbp-92]
    cmp     eax, 999
    jne     .have_solution
    xor     eax, eax                    ; no solution found
.have_solution:
    mov     rdi, r12                    ; return updated position

    add     rsp, 128
    pop     rbp
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

.no_solution:
    mov     rdi, r13
    xor     eax, eax
    add     rsp, 128
    pop     rbp
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; Parse a single machine and solve Part 2 using floating-point Gaussian elim
; Input: rdi = start of line, rsi = end of file
; Output: rax = min presses for counters, rdi = next line
; Stack layout:
;   rbp-64:  targets[16] (64 bytes, int)
;   rbp-128: solution[16] (64 bytes, int)
;   rbp-132: n_counters (4 bytes)
;   rbp-136: n_buttons (4 bytes)
;   rbp-144: temp storage (8 bytes)
;   rbp-400: button_lists[16][16] (256 bytes)
;   rbp-464: button_lens[16] (64 bytes)
;   rbp-2640: matrix[16][17] as doubles (2176 bytes)
;------------------------------------------------------------------------------
solve_p2_machine:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    push    rbp
    mov     rbp, rsp
    sub     rsp, 2656                       ; Enough for all variables

    mov     r12, rdi
    mov     r13, rsi

    ; Skip to '['
.p2_find_bracket:
    cmp     r12, r13
    jge     .p2_no_solution
    movzx   eax, byte [r12]
    cmp     al, '['
    je      .p2_found_bracket
    inc     r12
    jmp     .p2_find_bracket

.p2_found_bracket:
    inc     r12
    ; Skip pattern - count lights
    xor     ecx, ecx
.p2_skip_pattern:
    cmp     r12, r13
    jge     .p2_no_solution
    movzx   eax, byte [r12]
    cmp     al, ']'
    je      .p2_pattern_done
    inc     ecx
    inc     r12
    jmp     .p2_skip_pattern

.p2_pattern_done:
    mov     [rbp-132], ecx              ; n_counters
    inc     r12

    ; Parse buttons and store their indices
    xor     r15d, r15d                  ; button count
    lea     rbx, [rbp-400]              ; button_lists array
    lea     r14, [rbp-464]              ; button_lens array

.p2_parse_buttons:
    cmp     r12, r13
    jge     .p2_buttons_done
    movzx   eax, byte [r12]
    cmp     al, 10
    je      .p2_buttons_done
    cmp     al, '{'
    je      .p2_buttons_done
    cmp     al, '('
    je      .p2_found_button
    inc     r12
    jmp     .p2_parse_buttons

.p2_found_button:
    inc     r12
    ; Store starting offset for this button's list
    mov     rax, r15
    shl     rax, 4                      ; * 16
    add     rax, rbx
    mov     [rbp-144], rax
    xor     ecx, ecx

.p2_parse_indices:
    cmp     r12, r13
    jge     .p2_button_done
    movzx   eax, byte [r12]
    cmp     al, ')'
    je      .p2_button_done
    cmp     al, ','
    je      .p2_skip_idx_comma
    cmp     al, ' '
    je      .p2_skip_idx_comma
    cmp     al, '0'
    jb      .p2_skip_idx_other
    cmp     al, '9'
    ja      .p2_skip_idx_other

    push    rcx
    push    r15
    mov     rdi, r12
    mov     rsi, r13
    call    parse_int
    mov     r12, rdi
    pop     r15
    pop     rcx

    mov     rdi, [rbp-144]
    mov     [rdi + rcx], al
    inc     ecx
    jmp     .p2_parse_indices

.p2_skip_idx_comma:
.p2_skip_idx_other:
    inc     r12
    jmp     .p2_parse_indices

.p2_button_done:
    mov     [r14 + r15*4], ecx
    cmp     r12, r13
    jge     .p2_buttons_done
    inc     r12
    inc     r15d
    cmp     r15d, 16
    jl      .p2_parse_buttons

.p2_buttons_done:
    mov     [rbp-136], r15d             ; n_buttons

    ; Skip to '{' and parse targets
.p2_find_targets:
    cmp     r12, r13
    jge     .p2_no_solution
    movzx   eax, byte [r12]
    cmp     al, '{'
    je      .p2_found_targets
    cmp     al, 10
    je      .p2_no_solution
    inc     r12
    jmp     .p2_find_targets

.p2_found_targets:
    inc     r12
    xor     r14d, r14d
    lea     rbx, [rbp-64]

.p2_parse_targets:
    cmp     r12, r13
    jge     .p2_targets_done
    movzx   eax, byte [r12]
    cmp     al, '}'
    je      .p2_targets_done
    cmp     al, 10
    je      .p2_targets_done
    cmp     al, ','
    je      .p2_skip_target_comma
    cmp     al, '0'
    jb      .p2_skip_target_char
    cmp     al, '9'
    ja      .p2_skip_target_char

    push    r14
    mov     rdi, r12
    mov     rsi, r13
    call    parse_int
    mov     r12, rdi
    pop     r14

    mov     [rbx + r14*4], eax
    inc     r14d
    cmp     r14d, 16
    jl      .p2_parse_targets
    jmp     .p2_targets_done

.p2_skip_target_comma:
.p2_skip_target_char:
    inc     r12
    jmp     .p2_parse_targets

.p2_targets_done:
    ; Build and solve using floating-point Gaussian elimination
    ; Matrix is 16 rows x 17 cols of doubles at rbp-2640
    ; Matrix layout: row r, col c at offset (r*17 + c)*8

    mov     ecx, [rbp-132]              ; n = n_counters
    mov     [rbp-140], ecx

    ; Zero the matrix (doubles)
    lea     rdi, [rbp-2640]
    mov     ecx, 17*16                  ; 17 cols * 16 rows
    xorpd   xmm0, xmm0
.p2_zero_matrix:
    movsd   [rdi], xmm0
    add     rdi, 8
    dec     ecx
    jnz     .p2_zero_matrix

    ; Fill matrix: for each button b, set A[c][b] = 1.0 for each counter c
    xor     r8d, r8d                    ; button index
.p2_fill_matrix:
    cmp     r8d, [rbp-136]
    jge     .p2_fill_b

    mov     r9, r8
    shl     r9, 4
    lea     r10, [rbp-400]
    add     r10, r9
    mov     ecx, [rbp-464 + r8*4]

    xor     edx, edx
.p2_fill_row:
    cmp     edx, ecx
    jge     .p2_next_fill_button
    movzx   eax, byte [r10 + rdx]       ; counter index c
    ; A[c][b] at offset (c*17 + b)*8
    imul    r9d, eax, 17
    add     r9d, r8d
    lea     rdi, [rbp-2640]
    mov     rax, 0x3FF0000000000000     ; 1.0 as double
    mov     [rdi + r9*8], rax
    inc     edx
    jmp     .p2_fill_row

.p2_next_fill_button:
    inc     r8d
    jmp     .p2_fill_matrix

.p2_fill_b:
    ; Fill augmented column with targets (as doubles)
    lea     rdi, [rbp-2640]
    lea     rsi, [rbp-64]
    mov     ecx, [rbp-140]              ; n
    xor     edx, edx
.p2_fill_augment:
    cmp     edx, ecx
    jge     .p2_gauss_start
    cvtsi2sd xmm0, dword [rsi + rdx*4]  ; target as double
    mov     r8d, edx
    imul    r8d, 17
    add     r8d, ecx                    ; row*17 + n
    movsd   [rdi + r8*8], xmm0
    inc     edx
    jmp     .p2_fill_augment

.p2_gauss_start:
    ; Gaussian elimination with partial pivoting (floating-point)
    mov     ecx, [rbp-140]              ; n
    xor     r8d, r8d                    ; col (pivot)

.p2_gauss_col:
    cmp     r8d, ecx
    jge     .p2_back_sub

    ; Find pivot (largest abs value in column)
    mov     r9d, r8d
    xorpd   xmm1, xmm1                  ; best = 0
    mov     r11d, -1                    ; best_row = -1

.p2_find_pivot:
    cmp     r9d, ecx
    jge     .p2_check_pivot

    mov     eax, r9d
    imul    eax, 17
    add     eax, r8d
    lea     rdi, [rbp-2640]
    movsd   xmm0, [rdi + rax*8]
    ; abs(xmm0)
    movsd   xmm2, xmm0
    xorpd   xmm3, xmm3
    subsd   xmm3, xmm2                  ; -val
    maxsd   xmm2, xmm3                  ; abs(val)

    ucomisd xmm2, xmm1
    jbe     .p2_next_pivot_check
    movsd   xmm1, xmm2
    mov     r11d, r9d

.p2_next_pivot_check:
    inc     r9d
    jmp     .p2_find_pivot

.p2_check_pivot:
    ; Check if we found a pivot
    cmp     r11d, -1
    je      .p2_next_col

    ; Swap rows if needed
    cmp     r8d, r11d
    je      .p2_do_eliminate

    ; Swap rows r8 and r11
    mov     r10d, [rbp-140]
    inc     r10d                        ; n+1 columns
    xor     r9d, r9d
.p2_swap_col:
    cmp     r9d, r10d
    jge     .p2_do_eliminate

    mov     eax, r8d
    imul    eax, 17
    add     eax, r9d
    mov     edx, r11d
    imul    edx, 17
    add     edx, r9d

    lea     rdi, [rbp-2640]
    movsd   xmm0, [rdi + rax*8]
    movsd   xmm1, [rdi + rdx*8]
    movsd   [rdi + rax*8], xmm1
    movsd   [rdi + rdx*8], xmm0

    inc     r9d
    jmp     .p2_swap_col

.p2_do_eliminate:
    ; Normalize pivot row: divide by pivot
    mov     eax, r8d
    imul    eax, 17
    add     eax, r8d
    lea     rdi, [rbp-2640]
    movsd   xmm1, [rdi + rax*8]         ; pivot

    mov     r10d, [rbp-140]
    inc     r10d                        ; n+1 cols
    xor     r9d, r9d
.p2_norm_row:
    cmp     r9d, r10d
    jge     .p2_elim_rows

    mov     eax, r8d
    imul    eax, 17
    add     eax, r9d
    movsd   xmm0, [rdi + rax*8]
    divsd   xmm0, xmm1
    movsd   [rdi + rax*8], xmm0

    inc     r9d
    jmp     .p2_norm_row

.p2_elim_rows:
    ; Eliminate in all other rows
    xor     r9d, r9d                    ; row
.p2_elim_row:
    mov     ecx, [rbp-140]
    cmp     r9d, ecx
    jge     .p2_next_col

    cmp     r9d, r8d
    je      .p2_next_elim_row           ; skip pivot row

    mov     eax, r9d
    imul    eax, 17
    add     eax, r8d
    lea     rdi, [rbp-2640]
    movsd   xmm1, [rdi + rax*8]         ; factor

    ; Row r9 -= factor * Row r8
    mov     r10d, [rbp-140]
    inc     r10d
    xor     r11d, r11d
.p2_elim_col:
    cmp     r11d, r10d
    jge     .p2_next_elim_row

    ; A[r8][col]
    mov     eax, r8d
    imul    eax, 17
    add     eax, r11d
    movsd   xmm0, [rdi + rax*8]
    mulsd   xmm0, xmm1                  ; factor * A[r8][col]

    ; A[r9][col] -= factor * A[r8][col]
    mov     eax, r9d
    imul    eax, 17
    add     eax, r11d
    movsd   xmm2, [rdi + rax*8]
    subsd   xmm2, xmm0
    movsd   [rdi + rax*8], xmm2

    inc     r11d
    jmp     .p2_elim_col

.p2_next_elim_row:
    inc     r9d
    jmp     .p2_elim_row

.p2_next_col:
    inc     r8d
    mov     ecx, [rbp-140]
    jmp     .p2_gauss_col

.p2_back_sub:
    ; After RREF, solution is in augmented column
    ; For row i, if A[i][i] == 1, then x[i] = A[i][n]
    lea     rdi, [rbp-128]              ; solution array (int)
    lea     rsi, [rbp-2640]             ; matrix (double)
    mov     ecx, [rbp-140]              ; n

    ; Zero solution first
    xor     eax, eax
    xor     edx, edx
.p2_zero_sol:
    cmp     edx, ecx
    jge     .p2_read_sol
    mov     [rdi + rdx*4], eax
    inc     edx
    jmp     .p2_zero_sol

.p2_read_sol:
    xor     r8d, r8d                    ; row
.p2_read_row:
    mov     ecx, [rbp-140]
    cmp     r8d, ecx
    jge     .p2_calc_total

    ; Get A[row][row]
    mov     eax, r8d
    imul    eax, 17
    add     eax, r8d
    movsd   xmm0, [rsi + rax*8]

    ; Check if ~= 1.0
    mov     rax, 0x3FEFAE147AE147AE     ; 0.99
    movq    xmm1, rax
    ucomisd xmm0, xmm1
    jb      .p2_next_read_row           ; skip if < 0.99

    ; x[row] = round(A[row][n])
    mov     eax, r8d
    imul    eax, 17
    add     eax, ecx                    ; row*17 + n
    movsd   xmm0, [rsi + rax*8]

    ; Round to nearest int: add 0.5 and truncate (for positive)
    ; For negative: subtract 0.5
    xorpd   xmm2, xmm2
    ucomisd xmm0, xmm2
    jb      .p2_round_neg

    mov     rax, 0x3FE0000000000000     ; 0.5
    movq    xmm1, rax
    addsd   xmm0, xmm1
    cvttsd2si eax, xmm0
    jmp     .p2_store_sol

.p2_round_neg:
    mov     rax, 0x3FE0000000000000     ; 0.5
    movq    xmm1, rax
    subsd   xmm0, xmm1
    cvttsd2si eax, xmm0

.p2_store_sol:
    mov     [rdi + r8*4], eax

.p2_next_read_row:
    inc     r8d
    jmp     .p2_read_row

.p2_calc_total:
    ; Sum solution values
    xor     eax, eax
    xor     ecx, ecx
    lea     rdi, [rbp-128]
    mov     r10d, [rbp-136]             ; n_buttons
    mov     r11d, [rbp-140]             ; n
    cmp     r10d, r11d
    jle     .p2_sum_solution
    mov     r10d, r11d
.p2_sum_solution:
    cmp     ecx, r10d
    jge     .p2_skip_eol
    add     eax, [rdi + rcx*4]
    inc     ecx
    jmp     .p2_sum_solution

.p2_skip_eol:
    mov     [rbp-24], eax
.p2_skip_eol_loop:
    cmp     r12, r13
    jge     .p2_return
    movzx   edx, byte [r12]
    inc     r12
    cmp     dl, 10
    jne     .p2_skip_eol_loop

.p2_return:
    mov     eax, [rbp-24]
    mov     rdi, r12

    add     rsp, 2656
    pop     rbp
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

.p2_no_solution:
    mov     rdi, r13
    xor     eax, eax
    add     rsp, 2656
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
    mov     [rbp-48], rax               ; file size

    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; Process machines for Part 1
    lea     r12, [rel file_buf]
    mov     r13, [rbp-48]
    add     r13, r12                    ; end
    xor     r14d, r14d                  ; p1 total
    xor     r15d, r15d                  ; p2 total

.process_machines:
    cmp     r12, r13
    jge     .done_machines

    ; Skip whitespace
    movzx   eax, byte [r12]
    cmp     al, 10
    je      .skip_ws
    cmp     al, 13
    je      .skip_ws
    cmp     al, ' '
    je      .skip_ws
    jmp     .process_one

.skip_ws:
    inc     r12
    jmp     .process_machines

.process_one:
    ; Save position for Part 2
    mov     [rbp-56], r12

    ; Part 1
    mov     rdi, r12
    mov     rsi, r13
    call    solve_p1_machine
    add     r14d, eax
    mov     r12, rdi

    ; Part 2 - reparse from saved position
    mov     rdi, [rbp-56]
    mov     rsi, r13
    call    solve_p2_machine
    add     r15d, eax

    jmp     .process_machines

.done_machines:
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts1]
    call    clock_gettime

    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since
    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    mov     esi, r14d                   ; p1
    mov     edx, r15d                   ; p2
    lea     rdi, [rel fmt_out]
    mov     eax, 1
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
