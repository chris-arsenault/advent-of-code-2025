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
; Stack layout (non-overlapping):
;   rbp-64:  targets[16] (64 bytes, int)
;   rbp-128: solution[16] (64 bytes, int)
;   rbp-132: n_counters (4 bytes)
;   rbp-136: n_buttons (4 bytes)
;   rbp-144: temp storage (8 bytes)
;   rbp-152: temp for debug (8 bytes)
;   rbp-216: pivot_cols[16] (64 bytes, int)
;   rbp-472: button_lists[16][16] (256 bytes)
;   rbp-536: button_lens[16] (64 bytes)
;   rbp-2712: matrix[16][17] as doubles (2176 bytes)
;------------------------------------------------------------------------------
solve_p2_machine:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    push    rbp
    mov     rbp, rsp
    sub     rsp, 2880                       ; Enough for all variables

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
    lea     rbx, [rbp-472]              ; button_lists array
    lea     r14, [rbp-536]              ; button_lens array

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
    ; Matrix is 16 rows x 17 cols of doubles at rbp-2712
    ; Matrix layout: row r, col c at offset (r*17 + c)*8

    mov     ecx, [rbp-132]              ; n = n_counters
    mov     [rbp-140], ecx

    ; Zero the matrix (doubles)
    lea     rdi, [rbp-2712]
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
    lea     r10, [rbp-472]
    add     r10, r9
    mov     ecx, [rbp-536 + r8*4]

    xor     edx, edx
.p2_fill_row:
    cmp     edx, ecx
    jge     .p2_next_fill_button
    movzx   eax, byte [r10 + rdx]       ; counter index c
    ; A[c][b] at offset (c*17 + b)*8
    imul    r9d, eax, 17
    add     r9d, r8d
    lea     rdi, [rbp-2712]
    mov     rax, 0x3FF0000000000000     ; 1.0 as double
    mov     [rdi + r9*8], rax
    inc     edx
    jmp     .p2_fill_row

.p2_next_fill_button:
    inc     r8d
    jmp     .p2_fill_matrix

.p2_fill_b:
    ; Fill augmented column with targets (as doubles)
    ; Augmented column is at position n_buttons (after all button columns)
    lea     rdi, [rbp-2712]
    lea     rsi, [rbp-64]
    mov     ecx, [rbp-140]              ; n_counters (number of rows)
    mov     r9d, [rbp-136]              ; n_buttons (augmented column index)
    xor     edx, edx
.p2_fill_augment:
    cmp     edx, ecx
    jge     .p2_gauss_start
    cvtsi2sd xmm0, dword [rsi + rdx*4]  ; target as double
    mov     r8d, edx
    imul    r8d, 17
    add     r8d, r9d                    ; row*17 + n_buttons
    movsd   [rdi + r8*8], xmm0
    inc     edx
    jmp     .p2_fill_augment

.p2_gauss_start:
    ; Gaussian elimination with partial pivoting (floating-point)
    ; Track pivot columns: pivot_cols[row] stored at rbp-216
    ; Initialize all to -1
    lea     rdi, [rbp-216]
    mov     ecx, 16
    mov     eax, -1
.p2_init_pivots:
    mov     [rdi + rcx*4 - 4], eax
    dec     ecx
    jnz     .p2_init_pivots

    mov     r14d, [rbp-136]             ; n_buttons
    xor     r8d, r8d                    ; current row
    xor     r9d, r9d                    ; current column

.p2_gauss_col:
    ; For each column, find pivot and eliminate
    mov     ecx, [rbp-140]              ; n_counters
    cmp     r8d, ecx
    jge     .p2_back_sub
    cmp     r9d, r14d
    jge     .p2_back_sub

    ; Find pivot (largest abs value in column r9, rows r8 to n-1)
    mov     r10d, r8d                   ; search row
    xorpd   xmm1, xmm1                  ; best = 0
    mov     r11d, -1                    ; best_row = -1

.p2_find_pivot:
    mov     ecx, [rbp-140]
    cmp     r10d, ecx
    jge     .p2_check_pivot

    mov     eax, r10d
    imul    eax, 17
    add     eax, r9d
    lea     rdi, [rbp-2712]
    movsd   xmm0, [rdi + rax*8]
    ; abs(xmm0)
    movsd   xmm2, xmm0
    xorpd   xmm3, xmm3
    subsd   xmm3, xmm2
    maxsd   xmm2, xmm3

    ucomisd xmm2, xmm1
    jbe     .p2_next_pivot_check
    movsd   xmm1, xmm2
    mov     r11d, r10d

.p2_next_pivot_check:
    inc     r10d
    jmp     .p2_find_pivot

.p2_check_pivot:
    ; Check if pivot is too small (skip this column)
    mov     rax, 0x3EB0C6F7A0B5ED8D     ; 1e-6
    movq    xmm2, rax
    ucomisd xmm1, xmm2
    jbe     .p2_skip_col                ; no pivot in this column

    ; Swap rows r8 and r11 if needed
    cmp     r8d, r11d
    je      .p2_do_eliminate

    mov     r10d, r14d
    inc     r10d                        ; n_buttons+1 columns
    xor     eax, eax
.p2_swap_col:
    cmp     eax, r10d
    jge     .p2_do_eliminate

    push    rax
    mov     edx, r8d
    imul    edx, 17
    add     edx, eax
    mov     ecx, r11d
    imul    ecx, 17
    add     ecx, eax

    lea     rdi, [rbp-2712]
    movsd   xmm0, [rdi + rdx*8]
    movsd   xmm1, [rdi + rcx*8]
    movsd   [rdi + rdx*8], xmm1
    movsd   [rdi + rcx*8], xmm0

    pop     rax
    inc     eax
    jmp     .p2_swap_col

.p2_do_eliminate:
    ; Store pivot column for this row
    lea     rdi, [rbp-216]
    mov     [rdi + r8*4], r9d

    ; Normalize pivot row: divide by pivot
    mov     eax, r8d
    imul    eax, 17
    add     eax, r9d
    lea     rdi, [rbp-2712]
    movsd   xmm1, [rdi + rax*8]         ; pivot

    mov     r10d, r14d
    inc     r10d                        ; n_buttons+1 cols
    xor     eax, eax
.p2_norm_row:
    cmp     eax, r10d
    jge     .p2_elim_rows

    push    rax
    mov     edx, r8d
    imul    edx, 17
    add     edx, eax
    movsd   xmm0, [rdi + rdx*8]
    divsd   xmm0, xmm1
    movsd   [rdi + rdx*8], xmm0
    pop     rax

    inc     eax
    jmp     .p2_norm_row

.p2_elim_rows:
    ; Eliminate in all other rows (above and below)
    xor     r10d, r10d                  ; row
.p2_elim_row:
    mov     ecx, [rbp-140]
    cmp     r10d, ecx
    jge     .p2_next_col

    cmp     r10d, r8d
    je      .p2_next_elim_row           ; skip pivot row

    mov     eax, r10d
    imul    eax, 17
    add     eax, r9d
    lea     rdi, [rbp-2712]
    movsd   xmm1, [rdi + rax*8]         ; factor

    ; Row r10 -= factor * Row r8
    mov     ecx, r14d
    inc     ecx                         ; n_buttons+1
    xor     r11d, r11d
.p2_elim_col:
    cmp     r11d, ecx
    jge     .p2_next_elim_row

    ; A[r8][col]
    mov     eax, r8d
    imul    eax, 17
    add     eax, r11d
    movsd   xmm0, [rdi + rax*8]
    mulsd   xmm0, xmm1

    ; A[r10][col] -= factor * A[r8][col]
    mov     eax, r10d
    imul    eax, 17
    add     eax, r11d
    movsd   xmm2, [rdi + rax*8]
    subsd   xmm2, xmm0
    movsd   [rdi + rax*8], xmm2

    inc     r11d
    jmp     .p2_elim_col

.p2_next_elim_row:
    inc     r10d
    jmp     .p2_elim_row

.p2_next_col:
    ; After processing a pivot, move to next row AND next column
    inc     r8d                         ; next row
    inc     r9d                         ; next column
    jmp     .p2_gauss_col

.p2_skip_col:
    ; No pivot in this column, try next column (SAME row)
    inc     r9d
    jmp     .p2_gauss_col

.p2_back_sub:
    ; Find free columns (columns without pivots)
    ; Store at rbp-2720: n_free, rbp-2736: free_cols[4]
    lea     rdi, [rbp-2736]
    xor     ecx, ecx                    ; n_free = 0
    xor     edx, edx                    ; column index
    mov     r10d, [rbp-136]             ; n_buttons
.p2_find_free:
    cmp     edx, r10d
    jge     .p2_free_done
    ; Check if column edx has a pivot
    xor     eax, eax                    ; row
    mov     r9d, [rbp-140]              ; n_counters
.p2_check_pivot_col:
    cmp     eax, r9d
    jge     .p2_is_free                 ; no pivot found
    lea     r8, [rbp-216]
    cmp     [r8 + rax*4], edx
    je      .p2_not_free
    inc     eax
    jmp     .p2_check_pivot_col
.p2_is_free:
    cmp     ecx, 4                      ; max 4 free vars
    jge     .p2_not_free
    mov     [rdi + rcx*4], edx
    inc     ecx
.p2_not_free:
    inc     edx
    jmp     .p2_find_free

.p2_free_done:
    mov     [rbp-2720], ecx             ; n_free

    ; Initialize best_sum to large value
    mov     dword [rbp-2740], 999999

    ; Initialize free variable values to 0
    xor     eax, eax
    mov     [rbp-2756], eax             ; free_val[0]
    mov     [rbp-2752], eax             ; free_val[1]
    mov     [rbp-2748], eax             ; free_val[2]
    mov     [rbp-2744], eax             ; free_val[3]

.p2_enum_loop:
    ; Compute solution for current free variable values
    ; x[free_col[i]] = free_val[i]
    ; x[pivot_col[row]] = A[row][n] - sum(A[row][free_col[j]] * free_val[j])
    lea     rdi, [rbp-128]              ; solution array
    lea     rsi, [rbp-2712]             ; matrix
    mov     r10d, [rbp-136]             ; n_buttons

    ; Zero solution
    xor     eax, eax
    xor     edx, edx
.p2_zero_sol2:
    cmp     edx, r10d
    jge     .p2_set_free
    mov     [rdi + rdx*4], eax
    inc     edx
    jmp     .p2_zero_sol2

.p2_set_free:
    ; Set free variable values
    mov     ecx, [rbp-2720]             ; n_free
    test    ecx, ecx
    jz      .p2_compute_deps
    lea     r8, [rbp-2736]              ; free_cols
    lea     r9, [rbp-2756]              ; free_vals
    xor     edx, edx
.p2_set_free_loop:
    cmp     edx, ecx
    jge     .p2_compute_deps
    mov     eax, [r8 + rdx*4]           ; free_col[i]
    mov     r11d, [r9 + rdx*4]          ; free_val[i]
    mov     [rdi + rax*4], r11d
    inc     edx
    jmp     .p2_set_free_loop

.p2_compute_deps:
    ; For each row with pivot, compute dependent variable
    xor     r8d, r8d                    ; row
.p2_comp_row:
    mov     ecx, [rbp-140]              ; n_counters
    cmp     r8d, ecx
    jge     .p2_check_valid

    ; Get pivot column for this row
    lea     r10, [rbp-216]
    mov     r9d, [r10 + r8*4]
    cmp     r9d, -1
    je      .p2_next_comp_row

    ; x[pivot_col] = A[row][n_buttons] - sum(A[row][free_col[j]] * free_val[j])
    mov     eax, r8d
    imul    eax, 17
    add     eax, [rbp-136]              ; row*17 + n_buttons
    movsd   xmm0, [rsi + rax*8]         ; start with augmented value

    ; Subtract contributions from free variables
    mov     ecx, [rbp-2720]             ; n_free
    test    ecx, ecx
    jz      .p2_round_dep
    lea     r10, [rbp-2736]             ; free_cols
    lea     r11, [rbp-2756]             ; free_vals
    xor     edx, edx
.p2_sub_free:
    cmp     edx, ecx
    jge     .p2_round_dep
    mov     eax, [r10 + rdx*4]          ; free_col[j]
    push    rdx
    mov     edx, r8d
    imul    edx, 17
    add     edx, eax                    ; row*17 + free_col[j]
    movsd   xmm1, [rsi + rdx*8]         ; A[row][free_col[j]]
    pop     rdx
    cvtsi2sd xmm2, dword [r11 + rdx*4]  ; free_val[j]
    mulsd   xmm1, xmm2
    subsd   xmm0, xmm1
    inc     edx
    jmp     .p2_sub_free

.p2_round_dep:
    ; Round to nearest int
    xorpd   xmm2, xmm2
    ucomisd xmm0, xmm2
    jb      .p2_round_neg2
    mov     rax, 0x3FE0000000000000     ; 0.5
    movq    xmm1, rax
    addsd   xmm0, xmm1
    cvttsd2si eax, xmm0
    jmp     .p2_store_dep

.p2_round_neg2:
    mov     rax, 0x3FE0000000000000     ; 0.5
    movq    xmm1, rax
    subsd   xmm0, xmm1
    cvttsd2si eax, xmm0

.p2_store_dep:
    mov     [rdi + r9*4], eax

.p2_next_comp_row:
    inc     r8d
    jmp     .p2_comp_row

.p2_check_valid:
    ; Check if all values are non-negative and compute sum
    xor     eax, eax                    ; sum
    xor     ecx, ecx
    mov     r10d, [rbp-136]             ; n_buttons
.p2_valid_loop:
    cmp     ecx, r10d
    jge     .p2_update_best
    mov     edx, [rdi + rcx*4]
    cmp     edx, 0
    jl      .p2_next_enum               ; invalid, skip
    add     eax, edx
    inc     ecx
    jmp     .p2_valid_loop

.p2_update_best:
    cmp     eax, [rbp-2740]
    jge     .p2_next_enum
    mov     [rbp-2740], eax             ; new best sum
    ; Copy solution to best_sol at rbp-2820
    lea     r8, [rbp-2820]
    xor     ecx, ecx
.p2_copy_best:
    cmp     ecx, r10d
    jge     .p2_next_enum
    mov     edx, [rdi + rcx*4]
    mov     [r8 + rcx*4], edx
    inc     ecx
    jmp     .p2_copy_best

.p2_next_enum:
    ; Increment free variable values (like counting in base MAX_FREE_VAL)
    ; free_val[0]++, if >= MAX, reset and carry
    mov     ecx, [rbp-2720]             ; n_free
    test    ecx, ecx
    jz      .p2_enum_done               ; no free vars, done after one iteration

    lea     r8, [rbp-2756]              ; free_vals
    xor     edx, edx                    ; index
.p2_inc_free:
    cmp     edx, ecx
    jge     .p2_enum_done               ; all overflowed, done
    mov     eax, [r8 + rdx*4]
    inc     eax
    cmp     eax, 200                    ; MAX_FREE_VAL
    jl      .p2_store_inc
    ; Overflow, reset and carry
    mov     dword [r8 + rdx*4], 0
    inc     edx
    jmp     .p2_inc_free

.p2_store_inc:
    mov     [r8 + rdx*4], eax
    jmp     .p2_enum_loop

.p2_enum_done:
    ; Use best solution if found
    cmp     dword [rbp-2740], 999999
    je      .p2_no_solution

    ; Copy best_sol to solution
    lea     rdi, [rbp-128]
    lea     r8, [rbp-2820]
    mov     r10d, [rbp-136]
    xor     ecx, ecx
.p2_copy_final:
    cmp     ecx, r10d
    jge     .p2_calc_total
    mov     edx, [r8 + rcx*4]
    mov     [rdi + rcx*4], edx
    inc     ecx
    jmp     .p2_copy_final

.p2_calc_total:
    ; Sum all button press values (validate non-negative)
    xor     eax, eax
    xor     ecx, ecx
    lea     rdi, [rbp-128]
    mov     r10d, [rbp-136]             ; n_buttons
.p2_sum_solution:
    cmp     ecx, r10d
    jge     .p2_skip_eol
    mov     edx, [rdi + rcx*4]
    cmp     edx, 0
    jl      .p2_no_solution             ; negative = invalid
    add     eax, edx
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

    add     rsp, 2880
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
    add     rsp, 2880
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
