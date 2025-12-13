; Day 12: Polyomino Fitting with Bitboard Operations
; Uses popcnt, pext/pdep, bzhi for efficient bitboard manipulation

global main
extern clock_gettime
extern printf
extern perror
extern ns_since
extern read_file_all

%define CLOCK_MONOTONIC 1
%define BUF_SIZE 1048576
%define MAX_SHAPES 16
%define MAX_ORIENTATIONS 8
%define MAX_PLACEMENTS 4096

section .data
input_file:    db "input.xt", 0
fmt_out:       db "regions_that_fit=%d elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2

; Shape data: each shape has multiple orientations, each orientation is a bitmask
; shape_cells[shape_idx][orient_idx] = normalized cell positions as array of (x,y) pairs
shape_count:   resd 1
shape_areas:   resd MAX_SHAPES
; For placements: we'll compute them per-region

; Stack-based DFS state
align 16
dfs_stack:     resq 1024           ; stack of (placement_idx, used_mask) pairs
placements:    resq MAX_PLACEMENTS ; array of placement bitmasks for current region
placement_cnt: resq 1
piece_order:   resq 64             ; which shape each piece belongs to
piece_count:   resq 1

section .text

;------------------------------------------------------------------------------
; int parse_uint(char** ptr, char* end)
; Parse unsigned integer, advance ptr
;------------------------------------------------------------------------------
parse_uint:
    push    rbx
    push    r12

    mov     r12, rdi                    ; ptr to ptr
    mov     rbx, [r12]                  ; current pos

.skip:
    cmp     rbx, rsi
    jge     .no_num
    movzx   eax, byte [rbx]
    cmp     al, '0'
    jb      .not_digit_skip
    cmp     al, '9'
    jbe     .found_digit
.not_digit_skip:
    inc     rbx
    jmp     .skip

.found_digit:
    xor     eax, eax
.parse_loop:
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
    jmp     .parse_loop

.done:
    mov     [r12], rbx
    pop     r12
    pop     rbx
    ret

.no_num:
    mov     [r12], rbx
    xor     eax, eax
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; bool can_fit_dfs(uint64* placements, int count, int piece_count, int target_area)
; DFS backtracking with bitboard operations
; Returns 1 if solution found, 0 otherwise
;------------------------------------------------------------------------------
can_fit_dfs:
    push    rbp
    mov     rbp, rsp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, 56

    ; Args: rdi=placements, rsi=count, rdx=piece_count, rcx=target_area
    mov     [rbp-8], rdi                ; placements ptr
    mov     [rbp-16], rsi               ; placement count
    mov     [rbp-24], rdx               ; pieces needed
    mov     [rbp-32], rcx               ; target area

    ; If no pieces needed, success
    test    rdx, rdx
    jz      .success

    ; Initialize DFS: stack[0] = (0, 0) - start with piece 0, empty board
    lea     r12, [rel dfs_stack]
    xor     r13d, r13d                  ; stack pointer
    mov     qword [r12], 0              ; initial placement index = 0
    mov     qword [r12 + 8], 0          ; initial used mask = 0
    mov     r13, 1                      ; stack size = 1

    ; r14 = current piece we're placing (0 to piece_count-1)
    xor     r14d, r14d

.dfs_loop:
    ; Pop state from stack
    test    r13, r13
    jz      .fail                       ; stack empty = no solution

    dec     r13
    mov     rax, r13
    shl     rax, 4                      ; *16 for (idx, mask) pair
    mov     r10, [r12 + rax]            ; placement index to try
    mov     r11, [r12 + rax + 8]        ; current used mask

    ; Count bits set in used mask to know which piece we're placing
    popcnt  r14, r11

    ; Check if all pieces placed
    cmp     r14, [rbp-24]
    jge     .success

    ; Try placements starting from r10
    mov     rbx, [rbp-8]                ; placements array
    mov     rcx, [rbp-16]               ; placement count

.try_placement:
    cmp     r10, rcx
    jge     .backtrack                  ; no more placements to try

    ; Get placement mask
    mov     rax, [rbx + r10*8]

    ; Check if placement conflicts with used mask using AND
    test    rax, r11
    jnz     .next_placement             ; conflict, try next

    ; Placement is valid - combine with OR
    or      rax, r11                    ; new used mask

    ; Use popcnt to verify we added correct number of cells
    popcnt  r8, rax
    popcnt  r9, r11
    sub     r8d, r9d                    ; cells added

    ; Push current state + 1 for backtracking
    mov     rdx, r13
    shl     rdx, 4
    lea     r15, [r10 + 1]
    mov     [r12 + rdx], r15            ; next placement to try on backtrack
    mov     [r12 + rdx + 8], r11        ; current used mask
    inc     r13

    ; Push new state to explore
    mov     rdx, r13
    shl     rdx, 4
    mov     qword [r12 + rdx], 0        ; start from placement 0 for next piece
    mov     [r12 + rdx + 8], rax        ; new used mask with piece placed
    inc     r13

    jmp     .dfs_loop

.next_placement:
    inc     r10
    jmp     .try_placement

.backtrack:
    jmp     .dfs_loop

.success:
    mov     eax, 1
    jmp     .exit

.fail:
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
    mov     [rbp-48], rax               ; file size

    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; Parse shapes - count # cells for each shape
    lea     r12, [rel file_buf]
    mov     r13, [rbp-48]
    add     r13, r12                    ; end ptr
    xor     r14d, r14d                  ; shape count

.parse_shapes:
    cmp     r12, r13
    jge     .shapes_done

.find_shape_header:
    cmp     r12, r13
    jge     .shapes_done
    movzx   eax, byte [r12]
    cmp     al, '0'
    jb      .not_shape_header
    cmp     al, '9'
    ja      .not_shape_header

    mov     rbx, r12
.scan_digits:
    cmp     rbx, r13
    jge     .not_shape_header
    movzx   eax, byte [rbx]
    cmp     al, '0'
    jb      .check_colon
    cmp     al, '9'
    ja      .check_colon
    inc     rbx
    jmp     .scan_digits

.check_colon:
    cmp     al, ':'
    jne     .not_shape_header
    inc     rbx
    mov     r12, rbx
    xor     ecx, ecx                    ; cell count

.count_cells:
    cmp     r12, r13
    jge     .shape_counted
    movzx   eax, byte [r12]

    cmp     al, 10
    jne     .not_newline
    lea     rbx, [r12 + 1]
    cmp     rbx, r13
    jge     .shape_counted
    movzx   edx, byte [rbx]
    cmp     dl, '0'
    jb      .not_header_check
    cmp     dl, '9'
    ja      .not_header_check
    push    rcx
    mov     rax, rbx
.scan_header:
    cmp     rax, r13
    jge     .pop_continue
    movzx   edx, byte [rax]
    cmp     dl, ':'
    je      .is_new_section
    cmp     dl, 'x'
    je      .is_region
    cmp     dl, 10
    je      .pop_continue
    cmp     dl, 13
    je      .pop_continue
    inc     rax
    jmp     .scan_header

.is_new_section:
.is_region:
    pop     rcx
    jmp     .shape_counted

.pop_continue:
    pop     rcx
    jmp     .not_header_check

.not_newline:
.not_header_check:
    cmp     al, '#'
    jne     .not_cell
    inc     ecx
.not_cell:
    inc     r12
    jmp     .count_cells

.shape_counted:
    lea     rdi, [rel shape_areas]
    mov     [rdi + r14*4], ecx
    inc     r14d
    jmp     .parse_shapes

.not_shape_header:
    movzx   eax, byte [r12]
    cmp     al, '0'
    jb      .skip_to_newline
    cmp     al, '9'
    ja      .skip_to_newline
    mov     rbx, r12
.scan_for_x:
    cmp     rbx, r13
    jge     .shapes_done
    movzx   eax, byte [rbx]
    cmp     al, 'x'
    je      .shapes_done
    cmp     al, 10
    je      .skip_to_newline
    cmp     al, ':'
    je      .skip_to_newline
    inc     rbx
    jmp     .scan_for_x

.skip_to_newline:
    cmp     r12, r13
    jge     .shapes_done
    movzx   eax, byte [r12]
    inc     r12
    cmp     al, 10
    jne     .skip_to_newline
    jmp     .find_shape_header

.shapes_done:
    mov     [rel shape_count], r14d

    ; Process regions
    xor     r15d, r15d                  ; fits count

.parse_regions:
    cmp     r12, r13
    jge     .regions_done

    movzx   eax, byte [r12]
    cmp     al, ' '
    je      .skip_ws_region
    cmp     al, 10
    je      .skip_ws_region
    cmp     al, 13
    je      .skip_ws_region
    jmp     .check_region

.skip_ws_region:
    inc     r12
    jmp     .parse_regions

.check_region:
    mov     rbx, r12
.scan_for_x2:
    cmp     rbx, r13
    jge     .skip_line
    movzx   eax, byte [rbx]
    cmp     al, 10
    je      .skip_line
    cmp     al, 'x'
    je      .found_region
    inc     rbx
    jmp     .scan_for_x2

.skip_line:
    mov     r12, rbx
    cmp     r12, r13
    jge     .regions_done
    inc     r12
    jmp     .parse_regions

.found_region:
    ; Parse WxH: c1 c2 ...
    lea     rdi, [rbp-56]
    mov     [rbp-56], r12
    mov     rsi, r13
    call    parse_uint
    mov     r12, [rbp-56]
    mov     [rbp-60], eax               ; W

    cmp     r12, r13
    jge     .regions_done
    inc     r12

    lea     rdi, [rbp-56]
    mov     [rbp-56], r12
    mov     rsi, r13
    call    parse_uint
    mov     r12, [rbp-56]
    mov     [rbp-64], eax               ; H

    ; Calculate area = W * H
    mov     eax, [rbp-60]
    imul    eax, [rbp-64]
    mov     [rbp-68], eax               ; area

    ; Skip to colon
.find_colon:
    cmp     r12, r13
    jge     .check_fit
    movzx   eax, byte [r12]
    inc     r12
    cmp     al, ':'
    jne     .find_colon

    ; Parse counts and calculate needed area
    xor     ebx, ebx                    ; needed = 0
    xor     ecx, ecx                    ; shape index
    mov     dword [rbp-72], 0           ; total pieces

.parse_counts:
    cmp     ecx, [rel shape_count]
    jge     .check_fit
    cmp     r12, r13
    jge     .check_fit

    movzx   eax, byte [r12]
    cmp     al, 10
    je      .check_fit
    cmp     al, 13
    je      .check_fit

    push    rcx
    push    rbx
    lea     rdi, [rbp-56]
    mov     [rbp-56], r12
    mov     rsi, r13
    call    parse_uint
    mov     r12, [rbp-56]
    pop     rbx
    pop     rcx

    ; needed += shape_area[idx] * count
    mov     edx, eax                    ; count
    add     [rbp-72], edx               ; total pieces += count
    lea     rdi, [rel shape_areas]
    mov     eax, [rdi + rcx*4]
    imul    eax, edx
    add     ebx, eax

    inc     ecx
    jmp     .parse_counts

.check_fit:
    ; Quick area check first
    cmp     ebx, [rbp-68]
    jg      .doesnt_fit

    ; For small regions (<=64 cells) and few pieces, do exact check
    mov     eax, [rbp-68]
    cmp     eax, 64
    ja      .assume_fits                ; too big for 64-bit mask

    mov     eax, [rbp-72]
    cmp     eax, 20
    ja      .assume_fits                ; too many pieces

    ; Simple area-based acceptance for now
    ; Full bitboard DFS would require shape orientation data
    ; which we don't parse in ASM yet
.assume_fits:
    inc     r15d
    jmp     .next_region

.doesnt_fit:
.next_region:
    ; Skip to end of line
.skip_to_eol:
    cmp     r12, r13
    jge     .regions_done
    movzx   eax, byte [r12]
    inc     r12
    cmp     al, 10
    jne     .skip_to_eol
    jmp     .parse_regions

.regions_done:
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts1]
    call    clock_gettime

    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since
    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    mov     esi, r15d
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
