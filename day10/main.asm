; Day 10: Factory - Light Toggle & Counter Puzzle
; Hand-written x86-64 assembly - register-centric design
;
; DESIGN: Caller-allocated scratch frame (library-grade pattern)
; ============================================================
; All temporary/scratch data lives in a stack-allocated frame.
; r15 = scratch frame base pointer (preserved across all calls)
;
; Part 1: GF(2) Gaussian elimination using 64-bit bitmask rows
;   - tzcnt for first-set-bit, popcnt for counting
;   - pdep for extracting free variable combinations
;   - xor for GF(2) row operations
;
; Part 2: Floating-point Gaussian elimination with integer DFS
;   - Partial pivoting for numerical stability
;   - Back-substitution with free variable enumeration

global main
extern clock_gettime
extern printf
extern perror
extern ns_since
extern read_file_all

%define CLOCK_MONOTONIC 1
%define BUF_SIZE 65536
%define MAX_N 16
%define MAX_COLS 17

;------------------------------------------------------------------------------
; Scratch Frame Layout (r15 = base)
;------------------------------------------------------------------------------
; Part 1 uses:
%define SCR_P1_MATRIX      0         ; 16 qwords = 128 bytes (one per light row)
%define SCR_P1_PIVOT       128       ; 16 bytes (pivot column for each row)
; Part 2 uses (starts at 144, overlaps P1 when not in use):
; NOTE: Part 1 and Part 2 scratch regions are never live concurrently.
%define SCR_P2_AUG         144       ; 16*17*8 = 2176 bytes (augmented matrix)
%define SCR_P2_TARGETS     2320      ; 16*4 = 64 bytes
%define SCR_P2_PIVOT       2384      ; 16 bytes
%define SCR_P2_FREE_COLS   2400      ; 16 bytes
%define SCR_P2_FREE_VALS   2416      ; 16*8 = 128 bytes
%define SCR_P2_SOLUTION    2544      ; 16*8 = 128 bytes
%define SCR_P2_COEF        2672      ; 16*16*8 = 2048 bytes
%define SCR_P2_RHS         4720      ; 16*8 = 128 bytes
; Part 1 temporary storage (semantic names for clarity)
%define SCR_P1_NBUTTONS    4848      ; n_buttons during parsing/elimination
%define SCR_P1_COMBOS      4852      ; 2^free_count during enumeration
%define SCR_P1_RESULT      4848      ; result (overlaps NBUTTONS after reuse)
; Main loop temporaries
%define SCR_LINE_SAVE      4880      ; save line position between p1/p2 calls
%define SCRATCH_SIZE       4912      ; Total, aligned to 16

section .data
input_file:     db "input.txt", 0
fmt_out:        db "min_lights_presses=%d min_counter_presses=%d elapsed_ms=%.3f", 10, 0
err_open:       db "open", 0
one_million:    dq 1000000.0
fp_half:        dq 0.5
fp_eps:         dq 1.0e-9
fp_neg_eps:     dq -1.0e-9
fp_one:         dq 1.0
align 16
fp_abs_mask:    dq 0x7FFFFFFFFFFFFFFF, 0x7FFFFFFFFFFFFFFF  ; mask to clear sign bit

section .bss
file_buf:       resb BUF_SIZE
ts0:            resq 2
ts1:            resq 2

section .text

;------------------------------------------------------------------------------
; parse_int: Parse unsigned integer (leaf function, no stack)
; Input:  rdi = ptr to current position, rsi = end ptr
; Output: eax = value, rdi updated
;------------------------------------------------------------------------------
parse_int:
.skip:
    cmp     rdi, rsi
    jge     .zero
    movzx   eax, byte [rdi]
    sub     al, '0'
    cmp     al, 10
    jb      .parse_start
    inc     rdi
    jmp     .skip
.parse_start:
    xor     eax, eax
.parse:
    cmp     rdi, rsi
    jge     .done
    movzx   ecx, byte [rdi]
    sub     cl, '0'
    cmp     cl, 10
    jae     .done
    lea     eax, [rax + rax*4]
    lea     eax, [rcx + rax*2]
    inc     rdi
    jmp     .parse
.done:
    ret
.zero:
    xor     eax, eax
    ret

;------------------------------------------------------------------------------
; solve_p1_machine: Part 1 - GF(2) Gaussian elimination
; Input:  rdi = line start, rsi = buffer end, r15 = scratch frame
; Output: eax = min presses, rdi = next line
;
; Registers:
;   r12 = current position
;   r13 = buffer end
;   r14d = target bitmask (16 bits)
;   ebp = n_lights
;   r8d = n_buttons / row counter
;   r9d = column counter
;   r10d = rank
;   r11d = free_count
;   ebx = free_mask
;------------------------------------------------------------------------------
solve_p1_machine:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    rbp

    mov     r12, rdi
    mov     r13, rsi

    ; Find '[' and parse target pattern
.find_bracket:
    cmp     r12, r13
    jge     .no_solution
    cmp     byte [r12], '['
    je      .found_bracket
    inc     r12
    jmp     .find_bracket

.found_bracket:
    inc     r12
    xor     r14d, r14d              ; target = 0
    xor     ecx, ecx                ; light index
.parse_pattern:
    cmp     r12, r13
    jge     .no_solution
    movzx   eax, byte [r12]
    cmp     al, ']'
    je      .pattern_done
    cmp     al, '#'
    jne     .not_on
    bts     r14d, ecx
.not_on:
    inc     ecx
    inc     r12
    jmp     .parse_pattern

.pattern_done:
    mov     ebp, ecx                ; n_lights
    inc     r12

    ; Zero matrix in scratch frame
    lea     rdi, [r15 + SCR_P1_MATRIX]
    xor     eax, eax
    %rep 16
        mov     qword [rdi], rax
        add     rdi, 8
    %endrep

    ; Parse buttons into matrix
    xor     r8d, r8d                ; button count
.parse_buttons:
    cmp     r12, r13
    jge     .buttons_done
    movzx   eax, byte [r12]
    cmp     al, 10
    je      .buttons_done
    cmp     al, '{'
    je      .buttons_done
    cmp     al, '('
    jne     .skip_to_button
    inc     r12
    jmp     .parse_indices
.skip_to_button:
    inc     r12
    jmp     .parse_buttons

.parse_indices:
    cmp     r12, r13
    jge     .button_done
    movzx   eax, byte [r12]
    cmp     al, ')'
    je      .button_done
    sub     al, '0'
    cmp     al, 10
    jae     .skip_idx
    ; Parse the light index
    mov     rdi, r12
    mov     rsi, r13
    call    parse_int
    mov     r12, rdi
    ; Set matrix[light][button]
    cmp     eax, ebp
    jge     .parse_indices
    lea     rdi, [r15 + SCR_P1_MATRIX]
    bts     qword [rdi + rax*8], r8
    jmp     .parse_indices
.skip_idx:
    inc     r12
    jmp     .parse_indices

.button_done:
    cmp     r12, r13
    jge     .buttons_done
    inc     r12
    inc     r8d
    cmp     r8d, MAX_N
    jl      .parse_buttons

.buttons_done:
    ; r8d = n_buttons, ebp = n_lights, r14d = target
    mov     [r15 + SCR_P1_NBUTTONS], r8d

    ; Initialize pivot_cols to 0xFF
    lea     rdi, [r15 + SCR_P1_PIVOT]
    mov     eax, 0xFFFFFFFF
    mov     [rdi], eax
    mov     [rdi+4], eax
    mov     [rdi+8], eax
    mov     [rdi+12], eax

    ; GF(2) Gaussian elimination to RREF
    xor     r9d, r9d                ; row = 0
    xor     r10d, r10d              ; col = 0
    lea     rsi, [r15 + SCR_P1_MATRIX]

.rref_loop:
    cmp     r9d, ebp
    jge     .rref_done
    cmp     r10d, r8d
    jge     .rref_done

    ; Find pivot in column r10, starting at row r9
    mov     ecx, r9d
.find_pivot:
    cmp     ecx, ebp
    jge     .no_pivot
    mov     rax, [rsi + rcx*8]
    bt      rax, r10
    jc      .found_pivot
    inc     ecx
    jmp     .find_pivot

.no_pivot:
    inc     r10d
    jmp     .rref_loop

.found_pivot:
    ; Swap rows if needed (ecx != r9d)
    cmp     ecx, r9d
    je      .no_swap
    mov     rax, [rsi + r9*8]
    mov     rdx, [rsi + rcx*8]
    mov     [rsi + r9*8], rdx
    mov     [rsi + rcx*8], rax
    ; Swap target bits (using full-width registers to avoid partial-reg stalls)
    mov     eax, r14d
    bt      eax, r9d
    sbb     r8d, r8d                ; r8d = -1 if bit at r9d was set, else 0
    bt      eax, ecx
    sbb     r11d, r11d              ; r11d = -1 if bit at ecx was set, else 0
    btr     r14d, r9d
    btr     r14d, ecx
    test    r11d, r11d
    jz      .no_set1
    bts     r14d, r9d
.no_set1:
    test    r8d, r8d
    jz      .no_swap
    bts     r14d, ecx
.no_swap:
    ; Record pivot
    lea     rdi, [r15 + SCR_P1_PIVOT]
    mov     [rdi + r9], r10b

    ; Eliminate all other rows with bit r10 set
    mov     rbx, [rsi + r9*8]       ; pivot row
    xor     ecx, ecx
.elim_loop:
    cmp     ecx, ebp
    jge     .elim_done
    cmp     ecx, r9d
    je      .elim_next
    mov     rax, [rsi + rcx*8]
    bt      rax, r10
    jnc     .elim_next
    xor     rax, rbx
    mov     [rsi + rcx*8], rax
    ; XOR target bit if pivot's target bit is set
    bt      r14d, r9d
    jnc     .elim_next
    btc     r14d, ecx
.elim_next:
    inc     ecx
    jmp     .elim_loop

.elim_done:
    inc     r9d
    inc     r10d
    jmp     .rref_loop

.rref_done:
    mov     r11d, r9d               ; rank = r9

    ; Check consistency (rows >= rank with target bit set but all-zero row)
    mov     ecx, r11d
.check_cons:
    cmp     ecx, ebp
    jge     .cons_ok
    mov     rax, [rsi + rcx*8]
    test    rax, rax
    jnz     .cons_next
    bt      r14d, ecx
    jc      .no_solution
.cons_next:
    inc     ecx
    jmp     .check_cons

.cons_ok:
    ; Find free columns (non-pivot) -> build free_mask
    mov     r8d, [r15 + SCR_P1_NBUTTONS]
    lea     rdi, [r15 + SCR_P1_PIVOT]
    xor     ebx, ebx                ; free_mask
    xor     r9d, r9d                ; free_count
    xor     ecx, ecx                ; col
.find_free:
    cmp     ecx, r8d
    jge     .free_done
    xor     edx, edx                ; row
.check_pivot:
    cmp     edx, r11d
    jge     .is_free
    cmp     [rdi + rdx], cl
    je      .not_free
    inc     edx
    jmp     .check_pivot
.is_free:
    bts     ebx, ecx
    inc     r9d
.not_free:
    inc     ecx
    jmp     .find_free

.free_done:
    ; Enumerate 2^free_count solutions (limit to 20)
    mov     r10d, 9999              ; best = infinity
    cmp     r9d, 20
    jg      .greedy_solve

    mov     ecx, r9d
    mov     eax, 1
    shl     eax, cl                 ; combos = 2^free_count
    mov     [r15 + SCR_P1_COMBOS], eax
    xor     ecx, ecx                ; mask

.enum_loop:
    cmp     ecx, [r15 + SCR_P1_COMBOS]
    jge     .enum_done

    ; Build partial solution from free vars using pdep
    ; Requires BMI2 (pdep). Fallback required for older CPUs (pre-Haswell/Excavator).
    pdep    r8d, ecx, ebx           ; r8d = solution bits at free positions

    ; Back-substitute for pivot variables
    push    rcx
    mov     ecx, r11d               ; rank
    dec     ecx                     ; start from rank-1
.back_sub:
    test    ecx, ecx
    js      .count_sol
    lea     rdi, [r15 + SCR_P1_PIVOT]
    movzx   edx, byte [rdi + rcx]
    cmp     edx, 0xFF
    je      .next_back
    ; val = target[row] XOR parity(matrix[row] & solution & ~pivot_col)
    lea     rsi, [r15 + SCR_P1_MATRIX]
    mov     rax, [rsi + rcx*8]
    btr     rax, rdx
    and     rax, r8
    popcnt  rax, rax
    and     eax, 1                  ; eax = parity (0 or 1)
    bt      r14d, ecx
    sbb     r11d, r11d              ; r11d = -1 if target bit set, else 0
    and     r11d, 1                 ; r11d = 1 if target bit set, else 0
    xor     eax, r11d               ; eax = parity XOR target
    test    eax, eax
    jz      .next_back
    bts     r8d, edx
.next_back:
    dec     ecx
    jmp     .back_sub

.count_sol:
    pop     rcx
    popcnt  eax, r8d
    cmp     eax, r10d
    cmovl   r10d, eax
    inc     ecx
    jmp     .enum_loop

.greedy_solve:
    xor     r8d, r8d                ; solution = 0
    mov     ecx, r11d
    dec     ecx
.greedy_back:
    test    ecx, ecx
    js      .greedy_count
    lea     rdi, [r15 + SCR_P1_PIVOT]
    movzx   edx, byte [rdi + rcx]
    cmp     edx, 0xFF
    je      .greedy_next
    lea     rsi, [r15 + SCR_P1_MATRIX]
    mov     rax, [rsi + rcx*8]
    btr     rax, rdx
    and     rax, r8
    popcnt  rax, rax
    and     eax, 1                  ; eax = parity (0 or 1)
    bt      r14d, ecx
    sbb     r11d, r11d              ; r11d = -1 if target bit set, else 0
    and     r11d, 1                 ; r11d = 1 if target bit set, else 0
    xor     eax, r11d               ; eax = parity XOR target
    test    eax, eax
    jz      .greedy_next
    bts     r8d, edx
.greedy_next:
    dec     ecx
    jmp     .greedy_back

.greedy_count:
    popcnt  r10d, r8d

.enum_done:
    mov     eax, r10d
    cmp     eax, 9999
    jne     .skip_eol
    xor     eax, eax

.skip_eol:
    mov     [r15 + SCR_P1_RESULT], eax
.skip_eol_loop:
    cmp     r12, r13
    jge     .return
    movzx   edx, byte [r12]
    inc     r12
    cmp     dl, 10
    jne     .skip_eol_loop

.return:
    mov     eax, [r15 + SCR_P1_RESULT]
    mov     rdi, r12
    pop     rbp
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

.no_solution:
    cmp     r12, r13
    jge     .no_sol_ret
.skip_eol2:
    movzx   eax, byte [r12]
    inc     r12
    cmp     al, 10
    jne     .skip_eol2
.no_sol_ret:
    xor     eax, eax
    mov     rdi, r12
    pop     rbp
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; solve_p2_machine: Part 2 - Floating-point Gaussian + integer DFS
; Input:  rdi = line start, rsi = buffer end, r15 = scratch frame
; Output: eax = min presses, rdi = next line
;------------------------------------------------------------------------------
solve_p2_machine:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    rbp
    sub     rsp, 48                 ; small local frame for temporaries

    mov     r12, rdi
    mov     r13, rsi

    ; Find '[' and count lights (skip pattern content)
.find_bracket:
    cmp     r12, r13
    jge     .p2_no_solution
    cmp     byte [r12], '['
    je      .found_bracket
    inc     r12
    jmp     .find_bracket

.found_bracket:
    inc     r12
    xor     ebp, ebp                ; n_counters
.skip_pattern:
    cmp     r12, r13
    jge     .p2_no_solution
    movzx   eax, byte [r12]
    cmp     al, ']'
    je      .pattern_done
    inc     ebp
    inc     r12
    jmp     .skip_pattern

.pattern_done:
    inc     r12
    mov     [rsp], ebp              ; save n_counters

    ; Zero augmented matrix
    lea     rdi, [r15 + SCR_P2_AUG]
    xor     eax, eax
    mov     ecx, MAX_N * MAX_COLS
.zero_aug:
    mov     qword [rdi], rax
    add     rdi, 8
    dec     ecx
    jnz     .zero_aug

    ; Parse buttons - store in temp then fill matrix
    xor     r14d, r14d              ; n_buttons = 0
    sub     rsp, 32                 ; temp indices

.parse_buttons:
    cmp     r12, r13
    jge     .buttons_done
    movzx   eax, byte [r12]
    cmp     al, 10
    je      .buttons_done
    cmp     al, '{'
    je      .buttons_done
    cmp     al, '('
    jne     .skip_btn
    inc     r12
    xor     ecx, ecx                ; index count
.parse_idx:
    cmp     r12, r13
    jge     .btn_done
    movzx   eax, byte [r12]
    cmp     al, ')'
    je      .btn_done
    sub     al, '0'
    cmp     al, 10
    jae     .skip_idx2
    mov     rdi, r12
    mov     rsi, r13
    push    rcx
    call    parse_int
    pop     rcx
    mov     r12, rdi
    cmp     ecx, 16
    jge     .parse_idx
    mov     [rsp + rcx], al
    inc     ecx
    jmp     .parse_idx
.skip_idx2:
    inc     r12
    jmp     .parse_idx

.btn_done:
    ; Fill column r14 from temp indices
    xor     edx, edx
.fill_col:
    cmp     edx, ecx
    jge     .next_btn
    movzx   eax, byte [rsp + rdx]
    cmp     eax, [rsp + 32]         ; n_counters
    jge     .fill_next
    imul    edi, eax, MAX_COLS
    add     edi, r14d
    lea     rsi, [r15 + SCR_P2_AUG]
    mov     rax, [rel fp_one]
    mov     [rsi + rdi*8], rax
.fill_next:
    inc     edx
    jmp     .fill_col

.next_btn:
    cmp     r12, r13
    jge     .buttons_done
    inc     r12
    inc     r14d
    cmp     r14d, MAX_N
    jl      .parse_buttons
    jmp     .buttons_done

.skip_btn:
    inc     r12
    jmp     .parse_buttons

.buttons_done:
    add     rsp, 32
    mov     [rsp + 4], r14d         ; save n_buttons

    ; Find '{' and parse targets
.find_targets:
    cmp     r12, r13
    jge     .p2_no_solution
    movzx   eax, byte [r12]
    cmp     al, '{'
    je      .found_targets
    cmp     al, 10
    je      .p2_no_solution
    inc     r12
    jmp     .find_targets

.found_targets:
    inc     r12
    lea     rbx, [r15 + SCR_P2_TARGETS]
    xor     r8d, r8d                ; target count
.parse_targets:
    cmp     r12, r13
    jge     .targets_done
    movzx   eax, byte [r12]
    cmp     al, '}'
    je      .targets_done
    cmp     al, 10
    je      .targets_done
    sub     al, '0'
    cmp     al, 10
    jae     .skip_tgt
    mov     rdi, r12
    mov     rsi, r13
    call    parse_int
    mov     r12, rdi
    cmp     r8d, MAX_N
    jge     .parse_targets
    mov     [rbx + r8*4], eax
    inc     r8d
    jmp     .parse_targets
.skip_tgt:
    inc     r12
    jmp     .parse_targets

.targets_done:
    ; Use min(pattern_lights, target_count) as n_counters
    mov     ebp, [rsp]
    cmp     r8d, ebp
    cmovl   ebp, r8d
    mov     [rsp], ebp

    ; Fill augmented column with targets
    mov     r14d, [rsp + 4]         ; n_buttons
    lea     rdi, [r15 + SCR_P2_AUG]
    lea     rsi, [r15 + SCR_P2_TARGETS]
    xor     ecx, ecx
.fill_aug:
    cmp     ecx, ebp
    jge     .gauss_start
    cvtsi2sd xmm0, dword [rsi + rcx*4]
    imul    edx, ecx, MAX_COLS
    add     edx, r14d
    movsd   [rdi + rdx*8], xmm0
    inc     ecx
    jmp     .fill_aug

.gauss_start:
    ; Initialize pivot_cols to 0xFF
    lea     rdi, [r15 + SCR_P2_PIVOT]
    mov     eax, 0xFFFFFFFF
    mov     [rdi], eax
    mov     [rdi+4], eax
    mov     [rdi+8], eax
    mov     [rdi+12], eax

    ; Gaussian elimination with partial pivoting
    xor     r8d, r8d                ; row
    xor     r9d, r9d                ; col
    lea     rdi, [r15 + SCR_P2_AUG]

.gauss_col:
    cmp     r8d, ebp
    jge     .gauss_done
    cmp     r9d, r14d
    jge     .gauss_done

    ; Find pivot (max abs in column r9, rows >= r8)
    mov     r10d, r8d
    xorpd   xmm1, xmm1
    mov     r11d, -1
.find_piv:
    cmp     r10d, ebp
    jge     .check_piv
    imul    eax, r10d, MAX_COLS
    add     eax, r9d
    movsd   xmm0, [rdi + rax*8]
    movsd   xmm2, xmm0
    andpd   xmm2, [rel fp_abs_mask]  ; xmm2 = |a_ij| (clear sign bit)
    ucomisd xmm2, xmm1
    jbe     .next_piv
    movsd   xmm1, xmm2
    mov     r11d, r10d
.next_piv:
    inc     r10d
    jmp     .find_piv

.check_piv:
    movsd   xmm2, [rel fp_eps]
    ucomisd xmm1, xmm2
    jbe     .skip_col

    ; Swap rows r8 and r11
    cmp     r8d, r11d
    je      .do_elim
    mov     ecx, r14d
    inc     ecx
    xor     eax, eax
.swap_loop:
    cmp     eax, ecx
    jge     .do_elim
    imul    edx, r8d, MAX_COLS
    add     edx, eax
    imul    esi, r11d, MAX_COLS
    add     esi, eax
    movsd   xmm0, [rdi + rdx*8]
    movsd   xmm1, [rdi + rsi*8]
    movsd   [rdi + rdx*8], xmm1
    movsd   [rdi + rsi*8], xmm0
    inc     eax
    jmp     .swap_loop

.do_elim:
    ; Record pivot
    lea     rsi, [r15 + SCR_P2_PIVOT]
    mov     [rsi + r8], r9b

    ; Normalize row
    imul    eax, r8d, MAX_COLS
    add     eax, r9d
    movsd   xmm1, [rdi + rax*8]
    mov     ecx, r14d
    inc     ecx
    xor     eax, eax
.norm_loop:
    cmp     eax, ecx
    jge     .elim_rows
    imul    edx, r8d, MAX_COLS
    add     edx, eax
    movsd   xmm0, [rdi + rdx*8]
    divsd   xmm0, xmm1
    movsd   [rdi + rdx*8], xmm0
    inc     eax
    jmp     .norm_loop

.elim_rows:
    xor     r10d, r10d
.elim_row:
    cmp     r10d, ebp
    jge     .next_col
    cmp     r10d, r8d
    je      .next_erow
    imul    eax, r10d, MAX_COLS
    add     eax, r9d
    movsd   xmm1, [rdi + rax*8]
    mov     ecx, r14d
    inc     ecx
    xor     r11d, r11d
.elim_col:
    cmp     r11d, ecx
    jge     .next_erow
    imul    eax, r8d, MAX_COLS
    add     eax, r11d
    movsd   xmm0, [rdi + rax*8]
    mulsd   xmm0, xmm1
    imul    eax, r10d, MAX_COLS
    add     eax, r11d
    movsd   xmm2, [rdi + rax*8]
    subsd   xmm2, xmm0
    movsd   [rdi + rax*8], xmm2
    inc     r11d
    jmp     .elim_col
.next_erow:
    inc     r10d
    jmp     .elim_row

.next_col:
    inc     r8d
    inc     r9d
    jmp     .gauss_col

.skip_col:
    inc     r9d
    jmp     .gauss_col

.gauss_done:
    mov     [rsp + 8], r8d          ; rank

    ; Find free columns (max 4)
    lea     rsi, [r15 + SCR_P2_PIVOT]
    lea     rbx, [r15 + SCR_P2_FREE_COLS]
    xor     ecx, ecx                ; free_count
    xor     edx, edx
.find_free:
    cmp     edx, r14d
    jge     .free_done
    xor     eax, eax
.check_piv2:
    cmp     eax, r8d
    jge     .is_free2
    cmp     [rsi + rax], dl
    je      .not_free2
    inc     eax
    jmp     .check_piv2
.is_free2:
    cmp     ecx, 4
    jge     .not_free2
    mov     [rbx + rcx], dl
    inc     ecx
.not_free2:
    inc     edx
    jmp     .find_free

.free_done:
    mov     [rsp + 12], ecx         ; free_count
    mov     dword [rsp + 16], 0x7FFFFFFF  ; best_sum

    ; Prepare g_coef and g_rhs
    mov     r10d, r8d               ; rank
    lea     rdi, [r15 + SCR_P2_AUG]
    lea     r8, [r15 + SCR_P2_COEF]
    lea     r9, [r15 + SCR_P2_RHS]
    xor     eax, eax
.prep_coef:
    cmp     eax, r10d
    jge     .dfs_init
    imul    edx, eax, MAX_COLS
    add     edx, r14d
    movsd   xmm0, [rdi + rdx*8]
    movsd   [r9 + rax*8], xmm0
    xor     r11d, r11d
.prep_inner:
    cmp     r11d, ecx
    jge     .prep_next
    movzx   edx, byte [rbx + r11]
    imul    esi, eax, MAX_COLS
    add     esi, edx
    movsd   xmm0, [rdi + rsi*8]
    imul    esi, eax, MAX_N
    add     esi, r11d
    movsd   [r8 + rsi*8], xmm0
    inc     r11d
    jmp     .prep_inner
.prep_next:
    inc     eax
    jmp     .prep_coef

.dfs_init:
    ; Zero free_vals
    lea     rdi, [r15 + SCR_P2_FREE_VALS]
    xor     eax, eax
    mov     ecx, MAX_N
.zero_free:
    mov     qword [rdi + rcx*8 - 8], rax
    dec     ecx
    jnz     .zero_free

.dfs_loop:
    ; Sum free vals
    lea     r8, [r15 + SCR_P2_FREE_VALS]
    mov     ecx, [rsp + 12]
    xor     eax, eax
    xor     edx, edx
.sum_free:
    cmp     edx, ecx
    jge     .check_prune
    add     rax, [r8 + rdx*8]
    inc     edx
    jmp     .sum_free

.check_prune:
    cmp     eax, [rsp + 16]
    jge     .next_dfs

    ; Evaluate solution
    mov     [rsp + 24], rax         ; current_sum
    mov     r10d, [rsp + 8]         ; rank
    lea     r11, [r15 + SCR_P2_COEF]
    lea     rdi, [r15 + SCR_P2_SOLUTION]

    ; Zero solution
    xor     eax, eax
    xor     edx, edx
.zero_sol:
    cmp     edx, r14d
    jge     .set_free
    mov     qword [rdi + rdx*8], rax
    inc     edx
    jmp     .zero_sol

.set_free:
    lea     rbx, [r15 + SCR_P2_FREE_COLS]
    mov     ecx, [rsp + 12]
    lea     r8, [r15 + SCR_P2_FREE_VALS]
    xor     edx, edx
.set_free_loop:
    cmp     edx, ecx
    jge     .compute_deps
    movzx   eax, byte [rbx + rdx]
    mov     rsi, [r8 + rdx*8]
    mov     [rdi + rax*8], rsi
    inc     edx
    jmp     .set_free_loop

.compute_deps:
    lea     r9, [r15 + SCR_P2_RHS]
    xor     r8d, r8d
.comp_row:
    cmp     r8d, r10d
    jge     .check_valid
    lea     rsi, [r15 + SCR_P2_PIVOT]
    movzx   ebx, byte [rsi + r8]
    cmp     ebx, 0xFF
    je      .next_comp

    movsd   xmm0, [r9 + r8*8]
    mov     ecx, [rsp + 12]
    xor     edx, edx
.sub_free:
    cmp     edx, ecx
    jge     .round_dep
    imul    eax, r8d, MAX_N
    add     eax, edx
    movsd   xmm1, [r11 + rax*8]
    lea     rsi, [r15 + SCR_P2_FREE_VALS]
    cvtsi2sd xmm2, qword [rsi + rdx*8]
    mulsd   xmm1, xmm2
    subsd   xmm0, xmm1
    inc     edx
    jmp     .sub_free

.round_dep:
    ; Check v >= -eps (reject if too negative)
    movsd   xmm2, [rel fp_neg_eps]
    ucomisd xmm0, xmm2
    jb      .next_dfs

    ; Round v to nearest integer: iv = round(v)
    movsd   xmm3, xmm0              ; save original v in xmm3
    movsd   xmm1, [rel fp_half]
    addsd   xmm0, xmm1
    cvttsd2si rax, xmm0             ; rax = floor(v + 0.5) = round(v)

    ; Check |v - iv| <= eps (reject if not close to integer)
    cvtsi2sd xmm1, rax              ; xmm1 = (double)iv
    subsd   xmm3, xmm1              ; xmm3 = v - iv
    andpd   xmm3, [rel fp_abs_mask] ; xmm3 = |v - iv| (clear sign bit)
    movsd   xmm2, [rel fp_eps]
    ucomisd xmm3, xmm2
    ja      .next_dfs               ; reject if |v - iv| > eps

    mov     [rdi + rbx*8], rax

.next_comp:
    inc     r8d
    jmp     .comp_row

.check_valid:
    xor     eax, eax
    xor     ecx, ecx
.valid_loop:
    cmp     ecx, r14d
    jge     .update_best
    mov     rdx, [rdi + rcx*8]
    test    rdx, rdx
    js      .next_dfs
    add     rax, rdx
    inc     ecx
    jmp     .valid_loop

.update_best:
    cmp     eax, [rsp + 16]
    jge     .next_dfs
    mov     [rsp + 16], eax

.next_dfs:
    mov     ecx, [rsp + 12]
    test    ecx, ecx
    jz      .dfs_done
    lea     r8, [r15 + SCR_P2_FREE_VALS]
    xor     edx, edx

.inc_free:
    cmp     edx, ecx
    jge     .dfs_done
    ; Cap free variable growth to avoid runaway DFS (heuristic cutoff)
    mov     r10d, [rsp + 16]
    cmp     r10d, 500
    jle     .maxv_ok
    mov     r10d, 500
.maxv_ok:
    mov     rax, [r8 + rdx*8]
    inc     rax
    cmp     eax, r10d
    jle     .store_inc
    mov     qword [r8 + rdx*8], 0
    inc     edx
    jmp     .inc_free

.store_inc:
    mov     [r8 + rdx*8], rax
    jmp     .dfs_loop

.dfs_done:
    mov     eax, [rsp + 16]
    cmp     eax, 0x7FFFFFFF
    jne     .skip_eol3
    xor     eax, eax

.skip_eol3:
    mov     [rsp + 20], eax
.skip_eol_loop2:
    cmp     r12, r13
    jge     .p2_return
    movzx   edx, byte [r12]
    inc     r12
    cmp     dl, 10
    jne     .skip_eol_loop2

.p2_return:
    mov     eax, [rsp + 20]
    mov     rdi, r12
    add     rsp, 48
    pop     rbp
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

.p2_no_solution:
    mov     rdi, r13
    xor     eax, eax
    add     rsp, 48
    pop     rbp
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; main: Entry point - allocates scratch frame, processes all machines
;------------------------------------------------------------------------------
main:
    push    rbp
    mov     rbp, rsp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    ; Stack after 6 pushes (48 bytes): rsp is 16-byte aligned (return addr + pushes = 56, odd)
    ; SCRATCH_SIZE (4912) + 24 = 4936, which keeps 16-byte alignment for ABI compliance
    ; The +24 pads to maintain alignment after the 5 callee-saved pushes + rbp
    sub     rsp, SCRATCH_SIZE + 24

    ; r15 = scratch frame base (all functions use this for temporary storage)
    lea     r15, [rsp]

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
    mov     rbx, rax

    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    lea     r12, [rel file_buf]
    lea     r13, [r12 + rbx]
    xor     r14d, r14d              ; part1 total
    xor     ebx, ebx                ; part2 total (reuse rbx)

.process_loop:
    cmp     r12, r13
    jge     .done
    movzx   eax, byte [r12]
    cmp     al, ' '
    je      .skip_ws
    cmp     al, 10
    je      .skip_ws
    cmp     al, 13
    je      .skip_ws
    jmp     .process

.skip_ws:
    inc     r12
    jmp     .process_loop

.process:
    mov     [r15 + SCR_LINE_SAVE], r12  ; save line position for part2

    mov     rdi, r12
    mov     rsi, r13
    call    solve_p1_machine
    add     r14d, eax
    mov     r12, rdi

    mov     rdi, [r15 + SCR_LINE_SAVE]
    mov     rsi, r13
    call    solve_p2_machine
    add     ebx, eax

    jmp     .process_loop

.done:
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts1]
    call    clock_gettime

    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since
    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    mov     esi, r14d
    mov     edx, ebx
    lea     rdi, [rel fmt_out]
    mov     eax, 1
    call    printf
    xor     eax, eax

.exit:
    add     rsp, SCRATCH_SIZE + 24
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
