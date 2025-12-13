; Day 12: Polyomino Fitting
; Check if shapes can fit in each region (area check only)

global main
extern clock_gettime
extern printf
extern perror
extern ns_since
extern read_file_all

%define CLOCK_MONOTONIC 1
%define BUF_SIZE 1048576
%define MAX_SHAPES 16

section .data
input_file:    db "input.xt", 0
fmt_out:       db "regions_that_fit=%d elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2
shape_areas:   resd MAX_SHAPES
shape_count:   resd 1

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

    ; Skip non-digits
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
    xor     eax, eax                    ; result = 0
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
    sub     rsp, 72

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

    ; Parse shapes
    lea     r12, [rel file_buf]
    mov     r13, [rbp-48]
    add     r13, r12                    ; end ptr
    xor     r14d, r14d                  ; shape count

.parse_shapes:
    ; Skip to next shape definition (digit followed by :)
    cmp     r12, r13
    jge     .shapes_done

    ; Find line start
.find_shape_header:
    cmp     r12, r13
    jge     .shapes_done
    movzx   eax, byte [r12]
    cmp     al, '0'
    jb      .not_shape_header
    cmp     al, '9'
    ja      .not_shape_header

    ; Check if next non-digit is ':'
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
    ; Found shape header (N:)
    ; Now count # characters until next header or region definition
    inc     rbx                         ; skip colon
    mov     r12, rbx
    xor     ecx, ecx                    ; cell count

.count_cells:
    cmp     r12, r13
    jge     .shape_counted
    movzx   eax, byte [r12]

    ; Check for next header (digit at line start)
    cmp     al, 10
    jne     .not_newline
    ; At newline, check if next line starts with digit or WxH
    lea     rbx, [r12 + 1]
    cmp     rbx, r13
    jge     .shape_counted
    movzx   edx, byte [rbx]
    cmp     dl, '0'
    jb      .not_header_check
    cmp     dl, '9'
    ja      .not_header_check
    ; Might be new shape or region, scan for : or x
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
    ; New shape definition or end
    pop     rcx
    jmp     .shape_counted

.is_region:
    ; This is a region (WxH format)
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
    ; Store shape area
    lea     rdi, [rel shape_areas]
    mov     [rdi + r14*4], ecx
    inc     r14d
    jmp     .parse_shapes

.not_shape_header:
    ; Check if this might be a region (WxH)
    movzx   eax, byte [r12]
    cmp     al, '0'
    jb      .skip_to_newline
    cmp     al, '9'
    ja      .skip_to_newline
    ; Could be region, check for 'x'
    mov     rbx, r12
.scan_for_x:
    cmp     rbx, r13
    jge     .shapes_done
    movzx   eax, byte [rbx]
    cmp     al, 'x'
    je      .shapes_done              ; found region, stop parsing shapes
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

    ; Skip whitespace
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
    ; Check if line contains 'x'
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

    ; Skip 'x'
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

.parse_counts:
    cmp     ecx, [rel shape_count]
    jge     .check_fit
    cmp     r12, r13
    jge     .check_fit

    ; Check for newline
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
    lea     rdi, [rel shape_areas]
    mov     eax, [rdi + rcx*4]          ; shape area
    imul    eax, edx
    add     ebx, eax

    inc     ecx
    jmp     .parse_counts

.check_fit:
    ; If needed <= area, it fits
    cmp     ebx, [rbp-68]
    jg      .doesnt_fit
    inc     r15d
.doesnt_fit:

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

    mov     esi, r15d                   ; fits
    lea     rdi, [rel fmt_out]
    mov     eax, 1
    call    printf
    xor     eax, eax

.exit:
    add     rsp, 72
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
