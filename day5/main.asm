; Day 5: ID Range Coverage
; Part 1: Count IDs that fall within valid ranges
; Part 2: Total span of all merged ranges

global main
extern clock_gettime
extern printf
extern perror
extern ns_since
extern read_file_all
extern parse_uint64
extern skip_non_digits

%define CLOCK_MONOTONIC 1
%define BUF_SIZE 1048576
%define MAX_RANGES 256
%define MAX_IDS 1024

section .data
input_file:    db "input.txt", 0
fmt_out:       db "available_fresh=%d total_fresh_ids=%llu elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2
ranges:        resq MAX_RANGES * 2    ; pairs of (start, end)
merged:        resq MAX_RANGES * 2    ; merged ranges
ids:           resq MAX_IDS

section .text

;------------------------------------------------------------------------------
; void sort_ranges(uint64_t *ranges, int count)
; Simple insertion sort by start value
;------------------------------------------------------------------------------
sort_ranges:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15

    mov     r12, rdi            ; ranges array
    mov     r13d, esi           ; count

    cmp     r13d, 2
    jl      .sort_done

    mov     r14d, 1             ; i = 1
.outer_loop:
    cmp     r14d, r13d
    jge     .sort_done

    ; key = ranges[i]
    mov     rax, r14
    shl     rax, 4              ; i * 16
    mov     rcx, [r12 + rax]    ; key_start
    mov     rdx, [r12 + rax + 8]; key_end

    mov     r15d, r14d
    dec     r15d                ; j = i - 1

.inner_loop:
    cmp     r15d, 0
    jl      .insert
    ; Compare ranges[j].start > key_start
    mov     rax, r15
    shl     rax, 4              ; j * 16
    mov     rbx, [r12 + rax]    ; ranges[j].start
    cmp     rbx, rcx
    jle     .insert

    ; Shift ranges[j] to ranges[j+1]
    mov     r8, [r12 + rax]
    mov     r9, [r12 + rax + 8]
    mov     [r12 + rax + 16], r8
    mov     [r12 + rax + 24], r9

    dec     r15d
    jmp     .inner_loop

.insert:
    ; ranges[j+1] = key
    mov     eax, r15d
    inc     eax
    shl     rax, 4              ; (j+1) * 16
    mov     [r12 + rax], rcx
    mov     [r12 + rax + 8], rdx

    inc     r14d
    jmp     .outer_loop

.sort_done:
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; int merge_ranges(uint64_t *ranges, int count, uint64_t *merged)
; Returns number of merged ranges
;------------------------------------------------------------------------------
merge_ranges_fn:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15

    mov     r12, rdi            ; ranges
    mov     r13d, esi           ; count
    mov     r14, rdx            ; merged output

    test    r13d, r13d
    jz      .merge_empty

    ; First range goes into merged
    mov     rax, [r12]
    mov     [r14], rax
    mov     rax, [r12 + 8]
    mov     [r14 + 8], rax
    mov     r15d, 1             ; merged_count = 1

    mov     ebx, 1              ; i = 1
.merge_loop:
    cmp     ebx, r13d
    jge     .merge_done

    ; Current range
    mov     rax, rbx
    shl     rax, 4              ; i * 16
    mov     r8, [r12 + rax]     ; a = ranges[i].start
    mov     r9, [r12 + rax + 8] ; b = ranges[i].end

    ; Last merged range
    mov     eax, r15d
    dec     eax
    shl     rax, 4              ; (merged_count-1) * 16
    mov     r10, [r14 + rax + 8]; last.end

    ; Check if overlaps: a <= last.end + 1
    mov     rcx, r10
    inc     rcx                 ; last.end + 1
    cmp     r8, rcx
    ja      .new_range

    ; Merge: extend last range's end if needed
    cmp     r9, r10
    jle     .next_merge
    mov     [r14 + rax + 8], r9 ; last.end = max(last.end, b)
    jmp     .next_merge

.new_range:
    ; Add new range
    mov     rax, r15
    shl     rax, 4              ; merged_count * 16
    mov     [r14 + rax], r8
    mov     [r14 + rax + 8], r9
    inc     r15d

.next_merge:
    inc     ebx
    jmp     .merge_loop

.merge_done:
    mov     eax, r15d
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

.merge_empty:
    xor     eax, eax
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; bool in_any(uint64_t *merged, int count, uint64_t x)
; Binary search to check if x is in any range
;------------------------------------------------------------------------------
in_any:
    push    rbx
    push    r12
    push    r13
    push    r14

    mov     r12, rdi            ; merged
    mov     r13d, esi           ; count
    mov     r14, rdx            ; x

    test    r13d, r13d
    jz      .not_found

    xor     eax, eax            ; lo = 0
    mov     ecx, r13d           ; hi = count

.bsearch_loop:
    cmp     eax, ecx
    jge     .not_found

    ; mid = (lo + hi) / 2
    lea     ebx, [eax + ecx]
    shr     ebx, 1

    ; Get merged[mid]
    mov     rdx, rbx
    shl     rdx, 4              ; mid * 16
    mov     r8, [r12 + rdx]     ; start
    mov     r9, [r12 + rdx + 8] ; end

    ; if x < start: hi = mid
    cmp     r14, r8
    jb      .go_left
    ; if x > end: lo = mid + 1
    cmp     r14, r9
    ja      .go_right
    ; Found: start <= x <= end
    mov     eax, 1
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

.go_left:
    mov     ecx, ebx            ; hi = mid
    jmp     .bsearch_loop

.go_right:
    lea     eax, [ebx + 1]      ; lo = mid + 1
    jmp     .bsearch_loop

.not_found:
    xor     eax, eax
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
    mov     r14, rax                ; bytes read

    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; Parse ranges until blank line
    lea     r12, [rel file_buf]
    lea     r13, [rel file_buf]
    add     r13, r14

    lea     r15, [rel ranges]
    xor     ebx, ebx                ; range_count

.parse_ranges:
    cmp     r12, r13
    jge     .ranges_done

    ; Skip whitespace
    mov     rdi, r12
    mov     rsi, r13
    call    skip_non_digits
    mov     r12, rax

    cmp     r12, r13
    jge     .ranges_done

    ; Check for blank line (two consecutive newlines)
    movzx   eax, byte [r12]
    cmp     al, 10
    je      .check_blank
    cmp     al, 13
    je      .skip_cr

    ; Parse start number
    lea     rdi, [rbp-80]
    mov     [rbp-80], r12
    mov     rsi, r13
    call    parse_uint64
    ; Store start
    mov     rcx, rbx
    shl     rcx, 4
    mov     [r15 + rcx], rax
    mov     r12, [rbp-80]

    ; Skip '-'
    cmp     r12, r13
    jge     .ranges_done
    movzx   eax, byte [r12]
    cmp     al, '-'
    jne     .skip_to_eol
    inc     r12

    ; Parse end number
    lea     rdi, [rbp-80]
    mov     [rbp-80], r12
    mov     rsi, r13
    call    parse_uint64
    ; Store end
    mov     rcx, rbx
    shl     rcx, 4
    mov     [r15 + rcx + 8], rax
    mov     r12, [rbp-80]

    inc     ebx                     ; range_count++
    cmp     ebx, MAX_RANGES
    jge     .ranges_done

.skip_to_eol:
    cmp     r12, r13
    jge     .ranges_done
    movzx   eax, byte [r12]
    cmp     al, 10
    je      .next_range_line
    inc     r12
    jmp     .skip_to_eol

.next_range_line:
    inc     r12                     ; skip newline
    cmp     r12, r13
    jge     .ranges_done
    movzx   eax, byte [r12]
    cmp     al, 10                  ; check for blank line
    je      .ranges_done
    cmp     al, 13
    je      .ranges_done
    jmp     .parse_ranges

.skip_cr:
    inc     r12
    jmp     .parse_ranges

.check_blank:
    ; Might be blank line - check next char
    inc     r12
    cmp     r12, r13
    jge     .ranges_done
    movzx   eax, byte [r12]
    cmp     al, 10
    je      .ranges_done
    cmp     al, 13
    je      .ranges_done
    ; Not blank, continue parsing
    jmp     .parse_ranges

.ranges_done:
    mov     [rbp-48], ebx           ; save range_count

    ; Skip blank line(s)
.skip_blank:
    cmp     r12, r13
    jge     .ids_done
    movzx   eax, byte [r12]
    cmp     al, 10
    je      .skip_blank_char
    cmp     al, 13
    je      .skip_blank_char
    cmp     al, ' '
    je      .skip_blank_char
    jmp     .parse_ids
.skip_blank_char:
    inc     r12
    jmp     .skip_blank

    ; Parse IDs
.parse_ids:
    lea     r15, [rel ids]
    xor     ebx, ebx                ; id_count

.id_loop:
    cmp     r12, r13
    jge     .ids_done

    ; Skip whitespace
    mov     rdi, r12
    mov     rsi, r13
    call    skip_non_digits
    mov     r12, rax

    cmp     r12, r13
    jge     .ids_done

    ; Check if digit
    movzx   eax, byte [r12]
    cmp     al, '0'
    jb      .ids_done
    cmp     al, '9'
    ja      .ids_done

    ; Parse ID
    lea     rdi, [rbp-80]
    mov     [rbp-80], r12
    mov     rsi, r13
    call    parse_uint64
    mov     rcx, rbx
    shl     rcx, 3              ; i * 8
    mov     [r15 + rcx], rax
    mov     r12, [rbp-80]

    inc     ebx
    cmp     ebx, MAX_IDS
    jge     .ids_done
    jmp     .id_loop

.ids_done:
    mov     [rbp-52], ebx           ; save id_count

    ; Sort ranges
    lea     rdi, [rel ranges]
    mov     esi, [rbp-48]
    call    sort_ranges

    ; Merge ranges
    lea     rdi, [rel ranges]
    mov     esi, [rbp-48]
    lea     rdx, [rel merged]
    call    merge_ranges_fn
    mov     [rbp-56], eax           ; merged_count

    ; Part 1: Count IDs in ranges
    xor     r14d, r14d              ; fresh_count
    lea     r12, [rel ids]
    mov     ebx, [rbp-52]           ; id_count
    xor     ecx, ecx                ; i

.count_loop:
    cmp     ecx, ebx
    jge     .count_done

    mov     [rbp-60], ecx           ; save i
    lea     rdi, [rel merged]
    mov     esi, [rbp-56]
    mov     rax, rcx
    shl     rax, 3              ; i * 8
    mov     rdx, [r12 + rax]
    call    in_any
    mov     ecx, [rbp-60]

    test    eax, eax
    jz      .count_next
    inc     r14d

.count_next:
    inc     ecx
    jmp     .count_loop

.count_done:
    mov     [rbp-64], r14d          ; save fresh_count

    ; Part 2: Sum range sizes
    xor     r15, r15                ; total = 0
    lea     r12, [rel merged]
    mov     ebx, [rbp-56]           ; merged_count
    xor     ecx, ecx

.sum_loop:
    cmp     ecx, ebx
    jge     .sum_done

    mov     rax, rcx
    shl     rax, 4              ; i * 16
    mov     r8, [r12 + rax + 8] ; end
    sub     r8, [r12 + rax]     ; end - start
    inc     r8                  ; + 1
    add     r15, r8

    inc     ecx
    jmp     .sum_loop

.sum_done:
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts1]
    call    clock_gettime

    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since
    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    mov     esi, [rbp-64]           ; fresh_count
    mov     rdx, r15                ; total
    lea     rdi, [rel fmt_out]
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
