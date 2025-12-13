; Day 2 x86-64 assembly - Precomputation Strategy
;
; Hand-written ASM style:
; - No frame pointers - rsp-relative locals with symbolic offsets
; - Register-centric hot paths, stack only where call pressure forces it
; - Comparison-based digit counting (no division in utility code)
; - Clear register ownership: documented and maintained through functions
;
; Algorithm (from ALGORITHMS.md):
; - Pattern generation: Generate all even-half and periodic patterns up to max
; - Hybrid quicksort: sort_u64 from shared utils (median-of-three + insertion)
; - Binary search: lower_bound_u64, upper_bound_u64 from shared utils
; - Prefix sums: O(1) range sum queries after O(n) preprocessing

global main
extern clock_gettime
extern printf
extern perror

; Shared utilities
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
%define MAX_PATTERNS 200000
%define MAX_RANGES 1000

section .data
input_file:    db "input.txt", 0
fmt_out:       db "repeated-halves-sum=%llu repeated-pattern-sum=%llu elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

; Comparison thresholds for digit counting (10^1 through 10^19)
align 8
digit_thresholds:
    dq 10
    dq 100
    dq 1000
    dq 10000
    dq 100000
    dq 1000000
    dq 10000000
    dq 100000000
    dq 1000000000
    dq 10000000000
    dq 100000000000
    dq 1000000000000
    dq 10000000000000
    dq 100000000000000
    dq 1000000000000000
    dq 10000000000000000
    dq 100000000000000000
    dq 1000000000000000000
    dq 10000000000000000000

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2

even_arr:      resq MAX_PATTERNS
even_prefix:   resq MAX_PATTERNS + 1
periodic_arr:  resq MAX_PATTERNS
periodic_prefix: resq MAX_PATTERNS + 1

ranges:        resq MAX_RANGES * 2
range_count:   resq 1
max_value:     resq 1

even_count:    resq 1
periodic_count: resq 1

section .text

;------------------------------------------------------------------------------
; uint32_t get_digit_count(uint64_t n)
; Returns decimal digit count using comparison thresholds (no division).
; Input:  rdi = n
; Output: eax = digit count (1-20)
;------------------------------------------------------------------------------
get_digit_count:
    test    rdi, rdi
    jnz     .nonzero
    mov     eax, 1
    ret
.nonzero:
    lea     rax, [rel digit_thresholds]
    mov     ecx, 1                  ; digit count starts at 1
.check:
    cmp     rdi, [rax]
    jb      .done                   ; n < threshold[i] means i+1 digits
    add     rax, 8
    inc     ecx
    cmp     ecx, 20
    jb      .check
.done:
    mov     eax, ecx
    ret

;------------------------------------------------------------------------------
; void generate_even_half(uint64_t max_n)
; Generates numbers where first half equals second half (even digit count).
;
; Register ownership (maintained throughout):
;   r15 = max_n (constant)
;   r14 = output count (accumulator)
;   r13 = output array base (constant)
;   r12 = max_len (constant after init)
;   rbx = half_len (outer loop)
;   r8  = start (inner loop bound)
;   r9  = multiplier (inner loop constant)
;   r10 = t (inner loop variable)
;------------------------------------------------------------------------------
generate_even_half:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15

    mov     r15, rdi
    xor     r14d, r14d
    lea     r13, [rel even_arr]

    mov     rdi, r15
    call    get_digit_count
    mov     r12d, eax

    mov     ebx, 1

.half_loop:
    mov     eax, r12d
    shr     eax, 1
    cmp     ebx, eax
    jg      .done

    ; Compute start and multiplier for this half_len
    lea     edi, [ebx - 1]
    call    pow10
    mov     r8, rax                 ; start = 10^(half_len-1)

    mov     edi, ebx
    call    pow10
    mov     r9, rax                 ; multiplier = 10^half_len

    mov     r10, r8                 ; t = start

    ; Inner loop: fully register-resident
.t_loop:
    cmp     r10, r9
    jge     .next_half

    ; n = t * multiplier + t
    mov     rax, r10
    imul    rax, r9
    add     rax, r10

    cmp     rax, r15
    ja      .next_half

    mov     [r13 + r14*8], rax
    inc     r14
    inc     r10
    jmp     .t_loop

.next_half:
    inc     ebx
    jmp     .half_loop

.done:
    mov     [rel even_count], r14
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; uint64_t build_repeated(uint64_t base, uint32_t base_len, uint32_t reps)
; Builds base repeated reps times. Inlined multiplier computation.
;
; Register ownership:
;   rbx = base (constant)
;   r12 = reps countdown
;   rcx = multiplier (constant after pow10 call)
;   rax = accumulator
;------------------------------------------------------------------------------
build_repeated:
    push    rbx
    push    r12
    push    r13                     ; alignment

    mov     rbx, rdi
    mov     r12d, edx

    mov     edi, esi
    call    pow10
    mov     rcx, rax

    xor     eax, eax
.loop:
    test    r12d, r12d
    jz      .done
    imul    rax, rcx
    add     rax, rbx
    dec     r12d
    jmp     .loop

.done:
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; void generate_periodic(uint64_t max_n)
; Generates numbers that are a base pattern repeated k>=2 times.
;
; Register ownership:
;   r15 = max_n (constant)
;   r14 = output count (accumulator)
;   r13 = output array base (constant)
;   r12 = max_len (constant after init)
;   rbx = base_len (outer loop)
;
; Stack frame (symbolic offsets for call-surviving state):
;------------------------------------------------------------------------------
%define PERIODIC_START   0         ; 10^(base_len-1)
%define PERIODIC_END     8         ; 10^base_len
%define PERIODIC_BASE    16        ; current base value
%define PERIODIC_REPS    24        ; current repetition count
%define PERIODIC_FRAME   32        ; total frame size (must be 8-mod-16 for alignment)

generate_periodic:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, PERIODIC_FRAME + 8 ; +8 for 16-byte alignment

    mov     r15, rdi
    xor     r14d, r14d
    lea     r13, [rel periodic_arr]

    mov     rdi, r15
    call    get_digit_count
    mov     r12d, eax

    mov     ebx, 1

.base_len_loop:
    ; Check: base_len <= ceil(max_len/2)
    mov     eax, r12d
    inc     eax
    shr     eax, 1
    cmp     ebx, eax
    jg      .done

    ; Compute and store range bounds
    lea     edi, [ebx - 1]
    call    pow10
    mov     [rsp + PERIODIC_START], rax

    mov     edi, ebx
    call    pow10
    mov     [rsp + PERIODIC_END], rax

    ; Initialize base = start
    mov     rax, [rsp + PERIODIC_START]
    mov     [rsp + PERIODIC_BASE], rax

.base_loop:
    mov     rax, [rsp + PERIODIC_BASE]
    cmp     rax, [rsp + PERIODIC_END]
    jge     .next_base_len

    ; Compute max_reps = max_len / base_len
    mov     eax, r12d
    xor     edx, edx
    div     ebx
    mov     r8d, eax                ; r8d = max_reps (survives inner loop)

    mov     dword [rsp + PERIODIC_REPS], 2

.reps_loop:
    cmp     [rsp + PERIODIC_REPS], r8d
    jg      .next_base

    mov     rdi, [rsp + PERIODIC_BASE]
    mov     esi, ebx
    mov     edx, [rsp + PERIODIC_REPS]
    call    build_repeated

    cmp     rax, r15
    ja      .next_base

    mov     [r13 + r14*8], rax
    inc     r14

    inc     dword [rsp + PERIODIC_REPS]
    jmp     .reps_loop

.next_base:
    inc     qword [rsp + PERIODIC_BASE]
    jmp     .base_loop

.next_base_len:
    inc     ebx
    jmp     .base_len_loop

.done:
    mov     [rel periodic_count], r14
    add     rsp, PERIODIC_FRAME + 8
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; void remove_duplicates(uint64_t *arr, size_t *count)
; In-place dedup of sorted array. Pure register implementation.
;
; Register ownership:
;   rdi = array base (constant)
;   r12 = count pointer
;   rcx = n
;   rax = write index
;   rbx = read index
;   r8  = prev value
;   r9  = curr value
;------------------------------------------------------------------------------
remove_duplicates:
    push    rbx
    push    r12

    mov     r12, rsi
    mov     rcx, [r12]
    cmp     rcx, 1
    jbe     .done

    mov     rax, 1
    mov     rbx, 1
    mov     r8, [rdi]

.loop:
    cmp     rbx, rcx
    jge     .finish
    mov     r9, [rdi + rbx*8]
    cmp     r9, r8
    je      .skip
    mov     [rdi + rax*8], r9
    mov     r8, r9
    inc     rax
.skip:
    inc     rbx
    jmp     .loop

.finish:
    mov     [r12], rax
.done:
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; void compute_prefix_sums(uint64_t *arr, size_t n, uint64_t *prefix)
; Pure register implementation, no stack frame needed.
;
; Register ownership:
;   rdi = array (constant)
;   rsi = n (constant)
;   rdx = prefix array (constant)
;   rcx = index
;   r8  = running sum
;------------------------------------------------------------------------------
compute_prefix_sums:
    mov     qword [rdx], 0
    xor     ecx, ecx
    xor     r8d, r8d
.loop:
    cmp     rcx, rsi
    jge     .done
    add     r8, [rdi + rcx*8]
    mov     [rdx + rcx*8 + 8], r8
    inc     rcx
    jmp     .loop
.done:
    ret

;------------------------------------------------------------------------------
; uint64_t range_sum(uint64_t *arr, size_t n, uint64_t *prefix, uint64_t lo, uint64_t hi)
; Binary search + prefix sum lookup.
;
; Register ownership:
;   rbx = arr (constant)
;   r12 = n (constant)
;   r13 = prefix (constant)
;   r14 = lower bound index
;   r15 = hi (then upper bound index)
;------------------------------------------------------------------------------
range_sum:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15

    mov     rbx, rdi
    mov     r12, rsi
    mov     r13, rdx
    mov     r14, rcx                ; lo
    mov     r15, r8                 ; hi

    mov     rdi, rbx
    mov     rsi, r12
    mov     rdx, r14
    call    lower_bound_u64
    mov     r14, rax                ; i = lower_bound result

    mov     rdi, rbx
    mov     rsi, r12
    mov     rdx, r15
    call    upper_bound_u64
    ; rax = j

    mov     rdx, [r13 + rax*8]
    sub     rdx, [r13 + r14*8]
    mov     rax, rdx

    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; main
;
; Register ownership:
;   r15 = file size (parsing), then hi in range loop
;   r14 = ranges array (parsing), then lo in range loop
;   r13 = end pointer (parsing), then part2 sum
;   r12 = current pointer (parsing), then part1 sum
;   rbx = range count accumulator, then loop index
;
; Stack frame:
;------------------------------------------------------------------------------
%define MAIN_PTR    0              ; parse_uint64 pointer storage
%define MAIN_FRAME  16             ; 8 used + 8 alignment

main:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, MAIN_FRAME

    ; --- File read ---
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
    mov     r15, rax

    ; --- Parse ranges ---
    lea     r12, [rel file_buf]
    lea     r13, [rel file_buf]
    add     r13, r15
    lea     r14, [rel ranges]
    xor     ebx, ebx
    mov     qword [rel max_value], 0

.parse_loop:
    mov     rdi, r12
    mov     rsi, r13
    call    skip_non_digits
    mov     r12, rax
    cmp     r12, r13
    jge     .parse_done

    ; Parse lo
    mov     [rsp + MAIN_PTR], r12
    lea     rdi, [rsp + MAIN_PTR]
    mov     rsi, r13
    call    parse_uint64
    mov     r8, rax
    mov     r12, [rsp + MAIN_PTR]

    mov     rdi, r12
    mov     rsi, r13
    call    skip_non_digits
    mov     r12, rax
    cmp     r12, r13
    jge     .parse_done

    ; Parse hi
    mov     [rsp + MAIN_PTR], r12
    lea     rdi, [rsp + MAIN_PTR]
    mov     rsi, r13
    call    parse_uint64
    mov     r9, rax
    mov     r12, [rsp + MAIN_PTR]

    ; Store range
    mov     [r14 + rbx*8], r8
    mov     [r14 + rbx*8 + 8], r9
    add     rbx, 2

    ; Track max
    cmp     r9, [rel max_value]
    jbe     .parse_loop
    mov     [rel max_value], r9
    jmp     .parse_loop

.parse_done:
    shr     rbx, 1
    mov     [rel range_count], rbx

    ; --- Timing start ---
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; --- Generate patterns ---
    mov     rdi, [rel max_value]
    call    generate_even_half

    mov     rdi, [rel max_value]
    call    generate_periodic

    ; --- Process even array ---
    lea     rdi, [rel even_arr]
    lea     rsi, [rel even_count]
    call    remove_duplicates

    lea     rdi, [rel even_arr]
    mov     rsi, [rel even_count]
    lea     rdx, [rel even_prefix]
    call    compute_prefix_sums

    ; --- Process periodic array ---
    lea     rdi, [rel periodic_arr]
    mov     rsi, [rel periodic_count]
    call    sort_u64

    lea     rdi, [rel periodic_arr]
    lea     rsi, [rel periodic_count]
    call    remove_duplicates

    lea     rdi, [rel periodic_arr]
    mov     rsi, [rel periodic_count]
    lea     rdx, [rel periodic_prefix]
    call    compute_prefix_sums

    ; --- Sum ranges ---
    ; Register reassignment for range loop:
    ;   r12 = part1 sum
    ;   r13 = part2 sum
    ;   rbx = range index
    ;   r14 = lo (per iteration)
    ;   r15 = hi (per iteration)
    xor     r12d, r12d
    xor     r13d, r13d
    xor     ebx, ebx

.range_loop:
    cmp     rbx, [rel range_count]
    jge     .ranges_done

    ; Load lo, hi for this range
    lea     rax, [rel ranges]
    mov     rcx, rbx
    shl     rcx, 4
    mov     r14, [rax + rcx]
    mov     r15, [rax + rcx + 8]

    ; Part 1: even-half sum
    lea     rdi, [rel even_arr]
    mov     rsi, [rel even_count]
    lea     rdx, [rel even_prefix]
    mov     rcx, r14
    mov     r8, r15
    call    range_sum
    add     r12, rax

    ; Part 2: periodic sum
    lea     rdi, [rel periodic_arr]
    mov     rsi, [rel periodic_count]
    lea     rdx, [rel periodic_prefix]
    mov     rcx, r14
    mov     r8, r15
    call    range_sum
    add     r13, rax

    inc     rbx
    jmp     .range_loop

.ranges_done:
    ; --- Timing end ---
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
    mov     rsi, r12
    mov     rdx, r13
    mov     eax, 1
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

section .note.GNU-stack noalloc noexec nobits
