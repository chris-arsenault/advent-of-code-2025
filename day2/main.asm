; Day 2 x86-64 assembly - Precomputation Strategy
; Generates all repeated-pattern numbers, sorts, uses binary search for range queries.
; SysV ABI, x86_64, Linux/WSL2.

global main
extern clock_gettime
extern printf
extern perror

; Shared utilities from shared/utils.asm
extern ns_since
extern read_file_all
extern parse_uint64
extern skip_non_digits
extern pow10
extern lower_bound_u64
extern upper_bound_u64
extern sort_u64

%define CLOCK_MONOTONIC 1
%define BUF_SIZE 1048576
%define MAX_PATTERNS 200000      ; max number of patterns we might generate
%define MAX_RANGES 1000          ; max number of input ranges

section .data
input_file:    db "input.txt", 0
fmt_out:       db "repeated-halves-sum=%llu repeated-pattern-sum=%llu elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2

; Arrays for precomputed patterns
even_arr:      resq MAX_PATTERNS          ; even-half numbers
even_prefix:   resq MAX_PATTERNS + 1      ; prefix sums
periodic_arr:  resq MAX_PATTERNS          ; periodic numbers
periodic_prefix: resq MAX_PATTERNS + 1    ; prefix sums

; Input ranges
ranges:        resq MAX_RANGES * 2        ; pairs of (lo, hi)
range_count:   resq 1
max_value:     resq 1

; Counters
even_count:    resq 1
periodic_count: resq 1

section .text

;------------------------------------------------------------------------------
; void generate_even_half(uint64_t max_n)
; Generates all numbers where first half equals second half (even digit count).
; Stores in even_arr, updates even_count.
;------------------------------------------------------------------------------
generate_even_half:
    push    rbp
    mov     rbp, rsp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15

    mov     r15, rdi                ; max_n
    xor     r14d, r14d              ; count = 0
    lea     r13, [rel even_arr]     ; output array

    ; Find max_len (number of digits in max_n)
    mov     rdi, r15
    call    get_digit_count
    mov     r12d, eax               ; max_len

    ; For half_len from 1 to max_len/2
    mov     ebx, 1                  ; half_len = 1
.half_len_loop:
    mov     eax, r12d
    shr     eax, 1                  ; max_len / 2
    cmp     ebx, eax
    jg      .gen_even_done

    ; start = 10^(half_len-1), end = 10^half_len
    lea     edi, [ebx - 1]
    call    pow10
    mov     r8, rax                 ; start

    mov     edi, ebx
    call    pow10
    mov     r9, rax                 ; end (also multiplier)

    ; For t from start to end-1
    mov     r10, r8                 ; t = start
.t_loop:
    cmp     r10, r9
    jge     .next_half_len

    ; n = t * 10^half_len + t = t * (end + 1) ... no wait
    ; n = t * 10^half_len + t
    mov     rax, r10
    imul    rax, r9                 ; t * 10^half_len
    add     rax, r10                ; + t
    mov     r11, rax                ; n

    ; if n > max_n, we're done with this half_len
    cmp     r11, r15
    ja      .next_half_len

    ; Store n
    mov     [r13 + r14*8], r11
    inc     r14

    inc     r10
    jmp     .t_loop

.next_half_len:
    inc     ebx
    jmp     .half_len_loop

.gen_even_done:
    mov     [rel even_count], r14

    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

;------------------------------------------------------------------------------
; void generate_periodic(uint64_t max_n)
; Generates all numbers that are a base pattern repeated k>=2 times.
; Stores in periodic_arr, updates periodic_count.
; Uses simple deduplication by sorting and removing duplicates.
; Register allocation:
;   r15 = max_n
;   r14 = output count
;   r13 = output array pointer
;   r12 = max_len (digit count of max_n)
;   rbx = base_len
;   [rbp-56] = start (10^(base_len-1))
;   [rbp-64] = end (10^base_len)
;   [rbp-72] = base (current base value)
;   [rbp-80] = reps (current repetition count)
;------------------------------------------------------------------------------
generate_periodic:
    push    rbp
    mov     rbp, rsp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, 48                 ; local space

    mov     r15, rdi                ; max_n
    xor     r14d, r14d              ; count = 0
    lea     r13, [rel periodic_arr] ; output array

    ; Find max_len
    mov     rdi, r15
    call    get_digit_count
    mov     r12d, eax               ; max_len

    ; For base_len from 1 to ceil(max_len/2)
    mov     ebx, 1                  ; base_len = 1
.base_len_loop:
    mov     eax, r12d
    inc     eax
    shr     eax, 1                  ; ceil(max_len / 2)
    cmp     ebx, eax
    jg      .gen_periodic_done

    ; start = 10^(base_len-1), end = 10^base_len
    lea     edi, [ebx - 1]
    call    pow10
    mov     [rbp - 56], rax         ; start

    mov     edi, ebx
    call    pow10
    mov     [rbp - 64], rax         ; end

    ; For base from start to end-1
    mov     rax, [rbp - 56]
    mov     [rbp - 72], rax         ; base = start
.base_loop:
    mov     rax, [rbp - 72]
    cmp     rax, [rbp - 64]
    jge     .next_base_len

    ; For reps from 2 to max_len/base_len
    mov     dword [rbp - 80], 2     ; reps = 2
.reps_loop:
    mov     eax, r12d
    xor     edx, edx
    div     ebx                     ; eax = max_len / base_len
    cmp     [rbp - 80], eax
    jg      .next_base

    ; Build number: base repeated reps times
    mov     rdi, [rbp - 72]         ; base
    mov     esi, ebx                ; base_len
    mov     edx, [rbp - 80]         ; reps
    call    build_repeated_number
    mov     r11, rax                ; n

    ; if n > max_n, done with this base
    cmp     r11, r15
    ja      .next_base

    ; Store n (may have duplicates, we'll sort and dedup later)
    mov     [r13 + r14*8], r11
    inc     r14

    inc     dword [rbp - 80]        ; reps++
    jmp     .reps_loop

.next_base:
    inc     qword [rbp - 72]        ; base++
    jmp     .base_loop

.next_base_len:
    inc     ebx
    jmp     .base_len_loop

.gen_periodic_done:
    mov     [rel periodic_count], r14

    add     rsp, 48
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

;------------------------------------------------------------------------------
; uint64_t build_repeated_number(uint64_t base, uint32_t base_len, uint32_t reps)
; Returns base repeated reps times as a single number.
; Input:  rdi = base value
;         esi = number of digits in base
;         edx = number of repetitions
; Output: rax = repeated number
;------------------------------------------------------------------------------
build_repeated_number:
    push    rbx
    push    r12

    mov     rbx, rdi                ; base
    mov     r12d, edx               ; reps

    ; multiplier = 10^base_len
    mov     edi, esi
    call    pow10
    mov     rcx, rax                ; multiplier

    ; result = 0
    xor     eax, eax
    mov     edx, r12d               ; remaining reps
.build_loop:
    test    edx, edx
    jz      .build_done
    imul    rax, rcx                ; result *= multiplier
    add     rax, rbx                ; result += base
    dec     edx
    jmp     .build_loop

.build_done:
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; uint32_t get_digit_count(uint64_t n)
; Returns number of decimal digits.
;------------------------------------------------------------------------------
get_digit_count:
    mov     rax, rdi
    xor     ecx, ecx
    test    rax, rax
    jnz     .gdc_loop
    mov     eax, 1
    ret
.gdc_loop:
    test    rax, rax
    jz      .gdc_done
    inc     ecx
    xor     edx, edx
    mov     r8, 10
    div     r8
    jmp     .gdc_loop
.gdc_done:
    mov     eax, ecx
    ret

;------------------------------------------------------------------------------
; void remove_duplicates(uint64_t *arr, size_t *count)
; Removes duplicates from sorted array in-place.
; Input:  rdi = array pointer
;         rsi = pointer to count (updated in place)
;------------------------------------------------------------------------------
remove_duplicates:
    push    rbx
    push    r12

    mov     r12, rsi                ; pointer to count
    mov     rcx, [r12]              ; n = *count
    test    rcx, rcx
    jz      .dedup_done
    cmp     rcx, 1
    je      .dedup_done

    ; write_idx = 1, read_idx = 1
    mov     rax, 1                  ; write_idx
    mov     rbx, 1                  ; read_idx
    mov     r8, [rdi]               ; prev = arr[0]

.dedup_loop:
    cmp     rbx, rcx
    jge     .dedup_finish
    mov     r9, [rdi + rbx*8]       ; curr = arr[read_idx]
    cmp     r9, r8
    je      .dedup_skip             ; duplicate, skip
    ; Not duplicate, write it
    mov     [rdi + rax*8], r9
    mov     r8, r9                  ; prev = curr
    inc     rax                     ; write_idx++
.dedup_skip:
    inc     rbx                     ; read_idx++
    jmp     .dedup_loop

.dedup_finish:
    mov     [r12], rax              ; *count = write_idx

.dedup_done:
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; void compute_prefix_sums(uint64_t *arr, size_t n, uint64_t *prefix)
; Computes prefix sums: prefix[i] = sum of arr[0..i-1]
; prefix[0] = 0, prefix[n] = total sum
;------------------------------------------------------------------------------
compute_prefix_sums:
    mov     qword [rdx], 0          ; prefix[0] = 0
    xor     ecx, ecx                ; i = 0
    xor     r8, r8                  ; running sum = 0
.prefix_loop:
    cmp     rcx, rsi
    jge     .prefix_done
    add     r8, [rdi + rcx*8]       ; sum += arr[i]
    mov     [rdx + rcx*8 + 8], r8   ; prefix[i+1] = sum
    inc     rcx
    jmp     .prefix_loop
.prefix_done:
    ret

;------------------------------------------------------------------------------
; uint64_t range_sum(uint64_t *arr, size_t n, uint64_t *prefix, uint64_t lo, uint64_t hi)
; Returns sum of elements in arr where lo <= element <= hi.
; Uses binary search on sorted array with prefix sums.
;------------------------------------------------------------------------------
range_sum:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15

    mov     rbx, rdi                ; arr
    mov     r12, rsi                ; n
    mov     r13, rdx                ; prefix
    mov     r14, rcx                ; lo
    mov     r15, r8                 ; hi (save before calls clobber r8)

    ; i = lower_bound(arr, n, lo)
    mov     rdi, rbx
    mov     rsi, r12
    mov     rdx, r14
    call    lower_bound_u64
    mov     r14, rax                ; i (reuse r14, don't need lo anymore)

    ; j = upper_bound(arr, n, hi)
    mov     rdi, rbx
    mov     rsi, r12
    mov     rdx, r15                ; hi
    call    upper_bound_u64
    ; rax = j

    ; sum = prefix[j] - prefix[i]
    mov     rdx, [r13 + rax*8]      ; prefix[j]
    sub     rdx, [r13 + r14*8]      ; - prefix[i]
    mov     rax, rdx

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
    sub     rsp, 24                 ; align + locals

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
    mov     r15, rax                ; file size

    ; Parse ranges, find max value
    lea     r12, [rel file_buf]     ; ptr
    lea     r13, [rel file_buf]
    add     r13, r15                ; end
    lea     r14, [rel ranges]       ; ranges array
    xor     ebx, ebx                ; range_count = 0
    mov     qword [rel max_value], 0

.parse_loop:
    ; Skip to first number
    mov     rdi, r12
    mov     rsi, r13
    call    skip_non_digits
    mov     r12, rax
    cmp     r12, r13
    jge     .parse_done

    ; Parse first number (lo)
    lea     rdi, [rbp - 64]
    mov     [rbp - 64], r12
    mov     rsi, r13
    call    parse_uint64
    mov     r8, rax                 ; lo
    mov     r12, [rbp - 64]

    ; Skip to second number
    mov     rdi, r12
    mov     rsi, r13
    call    skip_non_digits
    mov     r12, rax
    cmp     r12, r13
    jge     .parse_done

    ; Parse second number (hi)
    lea     rdi, [rbp - 64]
    mov     [rbp - 64], r12
    mov     rsi, r13
    call    parse_uint64
    mov     r9, rax                 ; hi
    mov     r12, [rbp - 64]

    ; Store range
    mov     [r14 + rbx*8], r8       ; ranges[count*2] = lo
    mov     [r14 + rbx*8 + 8], r9   ; ranges[count*2 + 1] = hi
    add     rbx, 2

    ; Update max_value
    cmp     r9, [rel max_value]
    jbe     .parse_loop
    mov     [rel max_value], r9
    jmp     .parse_loop

.parse_done:
    shr     rbx, 1                  ; range_count = rbx / 2
    mov     [rel range_count], rbx

    ; Start timing
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; Generate even-half numbers
    mov     rdi, [rel max_value]
    call    generate_even_half

    ; Generate periodic numbers
    mov     rdi, [rel max_value]
    call    generate_periodic

    ; even_arr is generated in sorted order (by half_len, then by t within each)
    ; Skip sorting, but still remove duplicates for safety
    lea     rdi, [rel even_arr]
    lea     rsi, [rel even_count]
    call    remove_duplicates

    ; Compute prefix sums for even
    lea     rdi, [rel even_arr]
    mov     rsi, [rel even_count]
    lea     rdx, [rel even_prefix]
    call    compute_prefix_sums

    ; Sort periodic array (already generated above)
    lea     rdi, [rel periodic_arr]
    mov     rsi, [rel periodic_count]
    call    sort_u64

    ; Remove duplicates from periodic
    lea     rdi, [rel periodic_arr]
    lea     rsi, [rel periodic_count]
    call    remove_duplicates

    ; Compute prefix sums for periodic
    lea     rdi, [rel periodic_arr]
    mov     rsi, [rel periodic_count]
    lea     rdx, [rel periodic_prefix]
    call    compute_prefix_sums

    ; Process ranges, compute sums
    xor     r12d, r12d              ; part1 sum
    xor     r13d, r13d              ; part2 sum
    xor     ebx, ebx                ; range index

.range_loop:
    cmp     rbx, [rel range_count]
    jge     .ranges_done

    ; Get lo, hi for this range
    lea     rax, [rel ranges]
    mov     rcx, rbx
    shl     rcx, 4                  ; rcx = rbx * 16
    mov     r14, [rax + rcx]        ; lo
    mov     r15, [rax + rcx + 8]    ; hi

    ; Part 1: sum even-half numbers in range
    lea     rdi, [rel even_arr]
    mov     rsi, [rel even_count]
    lea     rdx, [rel even_prefix]
    mov     rcx, r14                ; lo
    mov     r8, r15                 ; hi
    call    range_sum
    add     r12, rax

    ; Part 2: sum periodic numbers in range
    lea     rdi, [rel periodic_arr]
    mov     rsi, [rel periodic_count]
    lea     rdx, [rel periodic_prefix]
    mov     rcx, r14                ; lo
    mov     r8, r15                 ; hi
    call    range_sum
    add     r13, rax

    inc     rbx
    jmp     .range_loop

.ranges_done:
    ; End timing
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts1]
    call    clock_gettime

    ; Calculate elapsed ms
    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since
    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    ; Print results
    lea     rdi, [rel fmt_out]
    mov     rsi, r12                ; part1
    mov     rdx, r13                ; part2
    call    printf

    xor     eax, eax

.exit:
    add     rsp, 24
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

section .note.GNU-stack noalloc nobits progbits align=1
