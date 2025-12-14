; Day 5: Fresh Ingredient IDs (Interval Coverage)
;
; Hand-written ASM style:
; - No frame pointers - rsp-relative locals with symbolic offsets
; - Register-centric: counts in callee-saved registers (ebx, r15d) survive calls
; - Two-pointer merge for Part 1: O(ids + intervals) vs O(ids × log intervals)
; - Range cached in registers, reloaded only on range advance
; - cmov for branchless max in interval merging
; - Cache-aligned data structures (64-byte for arrays)
;
; Algorithm:
; - Parse ranges (start-end pairs) until line without dash
; - Parse IDs after transition
; - Sort ranges by start, merge overlapping
; - Part 1: Sort IDs, two-pointer walk to count IDs in ranges
; - Part 2: Sum (end - start + 1) for all merged ranges

global main
extern clock_gettime
extern printf
extern perror
extern ns_since
extern read_file_all

%define CLOCK_MONOTONIC 1
%define BUF_SIZE 1048576
%define MAX_RANGES 512
%define MAX_IDS 2048

section .data
input_file:    db "input.txt", 0
fmt_out:       db "available_fresh=%d total_fresh_ids=%llu elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

section .bss
    align 64
file_buf:      resb BUF_SIZE
    align 64
ranges:        resq MAX_RANGES * 2      ; pairs of (start, end), 16 bytes each
    align 64
ids:           resq MAX_IDS
    align 16
ts0:           resq 2
ts1:           resq 2

section .text

;------------------------------------------------------------------------------
; uint64_t parse_number(char **ptr, char *end)
; Parse decimal number, advance *ptr past digits
;
; Input:  rdi = pointer to char* (updated on return)
;         rsi = end pointer
; Output: rax = parsed number
;------------------------------------------------------------------------------
parse_number:
    mov     r8, rdi                 ; save ptr location
    mov     rdi, [rdi]              ; current position
    xor     eax, eax                ; result = 0

.digit_loop:
    cmp     rdi, rsi
    jge     .done
    movzx   ecx, byte [rdi]
    sub     ecx, '0'
    cmp     ecx, 9
    ja      .done
    ; result = result * 10 + digit
    lea     rax, [rax + rax*4]
    lea     rax, [rcx + rax*2]
    inc     rdi
    jmp     .digit_loop

.done:
    mov     [r8], rdi               ; update caller's pointer
    ret

;------------------------------------------------------------------------------
; void sort_u64(uint64_t *arr, int count)
; Insertion sort for uint64 array
;
; Insertion sort chosen because MAX_IDS is small (2048) and input data
; is typically nearly sorted. Worst-case O(n²) is acceptable for this bound.
;
; Register ownership:
;   r12 = array base (constant)
;   r13d = count (constant)
;   r14d = outer index i
;   rbx = key value
;   ecx = inner index j
;------------------------------------------------------------------------------
sort_u64:
    push    rbx
    push    r12
    push    r13
    push    r14

    mov     r12, rdi
    mov     r13d, esi

    cmp     r13d, 2
    jl      .sort_done

    mov     r14d, 1                 ; i = 1
.outer:
    cmp     r14d, r13d
    jge     .sort_done

    ; key = arr[i]
    mov     rbx, [r12 + r14*8]
    mov     ecx, r14d
    dec     ecx                     ; j = i - 1

.inner:
    test    ecx, ecx
    js      .place
    mov     rax, [r12 + rcx*8]      ; arr[j]
    cmp     rax, rbx
    jbe     .place                  ; arr[j] <= key, done shifting
    ; Shift arr[j] to arr[j+1]
    mov     [r12 + rcx*8 + 8], rax
    dec     ecx
    jmp     .inner

.place:
    ; arr[j+1] = key
    inc     ecx
    mov     [r12 + rcx*8], rbx
    inc     r14d
    jmp     .outer

.sort_done:
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; void sort_ranges(uint64_t *ranges, int count)
; Insertion sort by start value (pairs of 16 bytes each)
;
; Register ownership:
;   r12 = array base (constant)
;   r13d = count (constant)
;   r14d = outer index i
;   rbx = key.start, r15 = key.end
;   ecx = inner index j
;------------------------------------------------------------------------------
sort_ranges:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15

    mov     r12, rdi
    mov     r13d, esi

    cmp     r13d, 2
    jl      .sr_done

    mov     r14d, 1                 ; i = 1
.sr_outer:
    cmp     r14d, r13d
    jge     .sr_done

    ; key = ranges[i]
    mov     eax, r14d
    shl     eax, 4                  ; i * 16
    mov     rbx, [r12 + rax]        ; key.start
    mov     r15, [r12 + rax + 8]    ; key.end

    mov     ecx, r14d
    dec     ecx                     ; j = i - 1

.sr_inner:
    test    ecx, ecx
    js      .sr_place
    mov     eax, ecx
    shl     eax, 4                  ; j * 16
    mov     rdx, [r12 + rax]        ; ranges[j].start
    cmp     rdx, rbx
    jbe     .sr_place               ; ranges[j].start <= key.start

    ; Shift ranges[j] to ranges[j+1]
    mov     r8, [r12 + rax]
    mov     r9, [r12 + rax + 8]
    mov     [r12 + rax + 16], r8
    mov     [r12 + rax + 24], r9

    dec     ecx
    jmp     .sr_inner

.sr_place:
    ; ranges[j+1] = key
    inc     ecx
    shl     ecx, 4
    mov     [r12 + rcx], rbx
    mov     [r12 + rcx + 8], r15

    inc     r14d
    jmp     .sr_outer

.sr_done:
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; int merge_ranges_inplace(uint64_t *ranges, int count)
; Merge overlapping ranges in-place, return new count
; Uses cmov for branchless max
;
; Register ownership:
;   r12 = array base (constant)
;   r13d = input count (constant)
;   r14d = write index w
;   ebx = read index i
;   rcx = current.start, rdx = current.end
;   r8 = last.end (at write position)
;------------------------------------------------------------------------------
merge_ranges_inplace:
    push    rbx
    push    r12
    push    r13
    push    r14

    mov     r12, rdi
    mov     r13d, esi

    test    r13d, r13d
    jz      .mr_empty

    xor     r14d, r14d              ; w = 0 (first range stays at index 0)
    mov     ebx, 1                  ; i = 1

.mr_loop:
    cmp     ebx, r13d
    jge     .mr_done

    ; Current range
    mov     eax, ebx
    shl     eax, 4
    mov     rcx, [r12 + rax]        ; current.start
    mov     rdx, [r12 + rax + 8]    ; current.end

    ; Last merged range's end
    mov     eax, r14d
    shl     eax, 4
    mov     r8, [r12 + rax + 8]     ; last.end

    ; Check overlap: current.start <= last.end + 1
    lea     r9, [r8 + 1]
    cmp     rcx, r9
    ja      .mr_new_range

    ; Merge: last.end = max(last.end, current.end)
    ; Branchless with cmov
    cmp     rdx, r8
    cmova   r8, rdx                 ; r8 = max(last.end, current.end)
    mov     [r12 + rax + 8], r8
    jmp     .mr_next

.mr_new_range:
    ; Start new range
    inc     r14d
    mov     eax, r14d
    shl     eax, 4
    mov     [r12 + rax], rcx
    mov     [r12 + rax + 8], rdx

.mr_next:
    inc     ebx
    jmp     .mr_loop

.mr_done:
    lea     eax, [r14d + 1]         ; return w + 1
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

.mr_empty:
    xor     eax, eax
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; int count_ids_in_ranges_twoptr(uint64_t *ids, int id_count,
;                                 uint64_t *ranges, int range_count)
; Two-pointer walk: O(ids + ranges) after sorting
; Assumes both arrays are sorted
;
; Register ownership:
;   r12 = ids array (constant)
;   r13d = id_count (constant)
;   r14 = ranges array (constant)
;   r15d = range_count (constant)
;   ebx = id index
;   ecx = range index
;   eax = count (result)
;   r9 = current range.start (cached, reloaded on range advance)
;   r10 = current range.end (cached, reloaded on range advance)
;------------------------------------------------------------------------------
count_ids_twoptr:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15

    mov     r12, rdi                ; ids
    mov     r13d, esi               ; id_count
    mov     r14, rdx                ; ranges
    mov     r15d, ecx               ; range_count

    xor     eax, eax                ; count = 0
    xor     ebx, ebx                ; id_idx = 0
    xor     ecx, ecx                ; range_idx = 0

    ; Early exit if no IDs or no ranges
    test    r13d, r13d
    jz      .tp_done
    test    r15d, r15d
    jz      .tp_done

    ; Load first range into registers (cached)
    mov     r9, [r14]               ; range.start
    mov     r10, [r14 + 8]          ; range.end

.tp_loop:
    ; Check ID bound only (range bound checked on advance)
    cmp     ebx, r13d
    jge     .tp_done

    ; Get current ID
    mov     r8, [r12 + rbx*8]

    ; If ID < range.start: ID not covered, advance ID
    cmp     r8, r9
    jb      .tp_id_before

    ; If ID > range.end: advance range
    cmp     r8, r10
    ja      .tp_id_after

    ; range.start <= ID <= range.end: ID is covered
    inc     eax                     ; count++
    inc     ebx                     ; advance ID
    jmp     .tp_loop

.tp_id_before:
    inc     ebx                     ; ID not in any range, skip it
    jmp     .tp_loop

.tp_id_after:
    inc     ecx                     ; advance to next range
    cmp     ecx, r15d
    jge     .tp_done                ; no more ranges
    ; Reload new range into cached registers
    mov     edx, ecx
    shl     edx, 4
    mov     r9, [r14 + rdx]         ; range.start
    mov     r10, [r14 + rdx + 8]    ; range.end
    jmp     .tp_loop

.tp_done:
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; main
;
; Register-centric: counts kept in callee-saved registers across function calls
;   ebx = range_count (survives all calls)
;   r15d = id_count (survives all calls)
;
; Stack frame (only for values that must survive after register reuse):
;------------------------------------------------------------------------------
%define MAIN_MERGED_COUNT   0
%define MAIN_FRESH_COUNT    4
%define MAIN_FRAME          16      ; 8 bytes used + alignment

main:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, MAIN_FRAME

    ; --- Read file ---
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
    ; Register allocation during range parsing:
    ;   r12 = current parse pointer (hot)
    ;   r13 = end pointer (constant)
    ;   r14 = ranges array
    ;   ebx = range_count (kept in callee-saved register)
    lea     r12, [rel file_buf]
    lea     r13, [rel file_buf]
    add     r13, rax
    lea     r14, [rel ranges]
    xor     ebx, ebx                ; range_count = 0

    ; --- Start timing ---
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; === Parse ranges (until line without '-') ===
    ; Blank line or ID-only line triggers transition to ID parsing
.parse_ranges:
    cmp     r12, r13
    jge     .ranges_done

    ; Skip whitespace/CR/LF to find start of next content
.skip_ws_r:
    cmp     r12, r13
    jge     .ranges_done
    movzx   eax, byte [r12]
    cmp     al, ' '
    je      .skip_ws_r_next
    cmp     al, 13                  ; CR
    je      .skip_ws_r_next
    cmp     al, 10                  ; LF
    je      .skip_ws_r_next
    jmp     .parse_range_start

.skip_ws_r_next:
    inc     r12
    jmp     .skip_ws_r

.parse_range_start:
    ; Must be digit to start a range
    movzx   eax, byte [r12]
    sub     al, '0'
    cmp     al, 9
    ja      .skip_range_line        ; not a digit, skip line

    ; Parse first number inline
    xor     eax, eax
.start_loop:
    cmp     r12, r13
    jge     .ranges_done
    movzx   ecx, byte [r12]
    sub     ecx, '0'
    cmp     ecx, 9
    ja      .start_done
    lea     rax, [rax + rax*4]
    lea     rax, [rcx + rax*2]
    inc     r12
    jmp     .start_loop

.start_done:
    ; Check for '-' - if not found, this is an ID, not a range
    cmp     r12, r13
    jge     .ranges_done
    movzx   ecx, byte [r12]
    cmp     cl, '-'
    jne     .first_id_found         ; No dash = first ID line, transition to ID parsing

    ; Store start value
    mov     edx, ebx
    shl     edx, 4
    mov     [r14 + rdx], rax
    inc     r12                     ; skip '-'

    ; Parse end number inline
    xor     eax, eax
.end_loop:
    cmp     r12, r13
    jge     .end_done
    movzx   ecx, byte [r12]
    sub     ecx, '0'
    cmp     ecx, 9
    ja      .end_done
    lea     rax, [rax + rax*4]
    lea     rax, [rcx + rax*2]
    inc     r12
    jmp     .end_loop

.end_done:
    ; Store end
    mov     edx, ebx
    shl     edx, 4
    mov     [r14 + rdx + 8], rax

    inc     ebx                     ; range_count++
    cmp     ebx, MAX_RANGES
    jge     .ranges_done

.skip_range_line:
    ; Skip to end of line
    cmp     r12, r13
    jge     .ranges_done
    movzx   eax, byte [r12]
    cmp     al, 10
    je      .next_range
    inc     r12
    jmp     .skip_range_line

.next_range:
    inc     r12                     ; skip LF
    jmp     .parse_ranges

.first_id_found:
    ; We parsed a number that has no dash - it's the first ID
    ; rax contains the first ID value, ebx has range_count (survives calls)
    lea     r14, [rel ids]
    mov     [r14], rax              ; store first ID
    mov     r15d, 1                 ; id_count = 1
    jmp     .id_loop                ; continue with remaining IDs

.ranges_done:
    ; ebx holds range_count (callee-saved, survives function calls)

    ; Skip blank line(s)
.skip_blanks:
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
    jmp     .skip_blanks

    ; === Parse IDs ===
    ; r15d = id_count (callee-saved, survives function calls)
.parse_ids:
    lea     r14, [rel ids]
    xor     r15d, r15d              ; id_count = 0

.id_loop:
    cmp     r12, r13
    jge     .ids_done

    ; Skip non-digits
.skip_nondigit:
    cmp     r12, r13
    jge     .ids_done
    movzx   eax, byte [r12]
    sub     al, '0'
    cmp     al, 9
    jbe     .parse_id
    inc     r12
    jmp     .skip_nondigit

.parse_id:
    ; Parse ID inline
    xor     eax, eax
.id_digit_loop:
    cmp     r12, r13
    jge     .id_digit_done
    movzx   ecx, byte [r12]
    sub     ecx, '0'
    cmp     ecx, 9
    ja      .id_digit_done
    lea     rax, [rax + rax*4]
    lea     rax, [rcx + rax*2]
    inc     r12
    jmp     .id_digit_loop

.id_digit_done:
    ; Store ID
    mov     [r14 + r15*8], rax
    inc     r15d
    cmp     r15d, MAX_IDS
    jge     .ids_done
    jmp     .id_loop

.ids_done:
    ; ebx = range_count, r15d = id_count (both callee-saved)

    ; === Sort ranges by start ===
    lea     rdi, [rel ranges]
    mov     esi, ebx                ; range_count from callee-saved register
    call    sort_ranges

    ; === Merge overlapping ranges ===
    lea     rdi, [rel ranges]
    mov     esi, ebx                ; range_count from callee-saved register
    call    merge_ranges_inplace
    mov     [rsp + MAIN_MERGED_COUNT], eax

    ; === Sort IDs for two-pointer algorithm ===
    lea     rdi, [rel ids]
    mov     esi, r15d               ; id_count from callee-saved register
    call    sort_u64

    ; === Part 1: Count IDs in ranges (two-pointer) ===
    lea     rdi, [rel ids]
    mov     esi, r15d               ; id_count from callee-saved register
    lea     rdx, [rel ranges]
    mov     ecx, [rsp + MAIN_MERGED_COUNT]
    call    count_ids_twoptr
    mov     [rsp + MAIN_FRESH_COUNT], eax

    ; === Part 2: Sum of range sizes ===
    ; ebx and r15 no longer needed, reuse for sum loop
    xor     r15, r15                ; total = 0
    lea     r14, [rel ranges]
    mov     ebx, [rsp + MAIN_MERGED_COUNT]
    xor     ecx, ecx                ; i = 0

.sum_loop:
    cmp     ecx, ebx
    jge     .sum_done

    mov     eax, ecx
    shl     eax, 4                  ; i * 16
    mov     r8, [r14 + rax + 8]     ; end
    sub     r8, [r14 + rax]         ; end - start
    inc     r8                      ; + 1 (inclusive)
    add     r15, r8

    inc     ecx
    jmp     .sum_loop

.sum_done:
    ; --- End timing ---
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts1]
    call    clock_gettime

    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since
    cvtsi2sd xmm0, rax
    divsd   xmm0, [rel one_million]

    ; --- Output ---
    lea     rdi, [rel fmt_out]
    mov     esi, [rsp + MAIN_FRESH_COUNT]
    mov     rdx, r15                ; total_fresh_ids
    mov     eax, 1                  ; 1 XMM register used
    call    printf

    xor     eax, eax

.exit:
    add     rsp, MAIN_FRAME
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

section .note.GNU-stack noalloc noexec
