; Shared helper functions (pure syscall based)

global ns_since
global read_file_all
global parse_uint64
global skip_non_digits
global uint64_digit_count
global uint64_to_digits
global pow10
global lower_bound_u64
global upper_bound_u64
global sort_u64
global sort_edges_3

section .text

;------------------------------------------------------------------------------
; uint64_t parse_uint64(const char **pptr, const char *end)
; Parses an unsigned 64-bit integer from string.
; Input:  rdi = pointer to pointer to current position (updated on return)
;         rsi = end pointer
; Output: rax = parsed value (0 if no digits found)
; Updates *pptr to point after the last digit parsed.
;------------------------------------------------------------------------------
parse_uint64:
    push    rbx
    mov     rbx, [rdi]          ; ptr = *pptr
    xor     eax, eax            ; result = 0
    xor     ecx, ecx            ; found_digit = 0

.parse_loop:
    cmp     rbx, rsi
    jge     .done
    movzx   edx, byte [rbx]
    cmp     dl, '0'
    jb      .done
    cmp     dl, '9'
    ja      .done
    ; Found a digit
    mov     ecx, 1              ; found_digit = true
    sub     dl, '0'
    imul    rax, rax, 10
    movzx   edx, dl
    add     rax, rdx
    inc     rbx
    jmp     .parse_loop

.done:
    mov     [rdi], rbx          ; *pptr = updated position
    pop     rbx
    ret

;------------------------------------------------------------------------------
; const char* skip_non_digits(const char *ptr, const char *end)
; Skips whitespace and common separators (space, comma, dash, newline, CR).
; Input:  rdi = current pointer
;         rsi = end pointer
; Output: rax = pointer to first digit or end
;------------------------------------------------------------------------------
skip_non_digits:
    mov     rax, rdi
.skip_loop:
    cmp     rax, rsi
    jge     .skip_done
    movzx   ecx, byte [rax]
    cmp     cl, ' '
    je      .skip_next
    cmp     cl, ','
    je      .skip_next
    cmp     cl, '-'
    je      .skip_next
    cmp     cl, 10              ; newline
    je      .skip_next
    cmp     cl, 13              ; carriage return
    je      .skip_next
    cmp     cl, 9               ; tab
    je      .skip_next
    ; Not a separator, check if digit
    cmp     cl, '0'
    jb      .skip_next          ; skip other non-digit chars
    cmp     cl, '9'
    ja      .skip_next
    ; Found a digit
    jmp     .skip_done
.skip_next:
    inc     rax
    jmp     .skip_loop
.skip_done:
    ret

;------------------------------------------------------------------------------
; uint64_t uint64_digit_count(uint64_t n)
; Returns the number of decimal digits in n.
; Input:  rdi = number
; Output: rax = digit count (1 for n=0)
;------------------------------------------------------------------------------
uint64_digit_count:
    mov     rax, rdi
    xor     ecx, ecx            ; count = 0
    test    rax, rax
    jnz     .count_loop
    mov     eax, 1              ; n=0 has 1 digit
    ret
.count_loop:
    test    rax, rax
    jz      .count_done
    inc     ecx
    xor     edx, edx
    mov     r8, 10
    div     r8                  ; rax = rax / 10
    jmp     .count_loop
.count_done:
    mov     eax, ecx
    ret

;------------------------------------------------------------------------------
; uint64_t uint64_to_digits(uint64_t n, uint8_t *buf)
; Extracts decimal digits of n into buf (most significant first).
; Input:  rdi = number
;         rsi = buffer pointer (must have space for 20 bytes)
; Output: rax = digit count
;         buf filled with digits as bytes 0-9 (not ASCII)
;------------------------------------------------------------------------------
uint64_to_digits:
    push    rbx
    push    r12
    push    r13
    mov     r12, rsi            ; save buf
    mov     rax, rdi            ; n
    xor     r13d, r13d          ; count = 0

    ; Handle n=0 special case
    test    rax, rax
    jnz     .extract_loop
    mov     byte [r12], 0
    mov     eax, 1
    pop     r13
    pop     r12
    pop     rbx
    ret

.extract_loop:
    ; Extract digits in reverse order onto stack area
    test    rax, rax
    jz      .reverse
    xor     edx, edx
    mov     rcx, 10
    div     rcx                 ; rax = n/10, rdx = n%10
    mov     [r12 + r13], dl     ; store digit (reversed)
    inc     r13
    jmp     .extract_loop

.reverse:
    ; Reverse the digits in place
    xor     ecx, ecx            ; i = 0
    mov     rdx, r13
    dec     rdx                 ; j = count - 1
.reverse_loop:
    cmp     rcx, rdx
    jge     .reverse_done
    mov     al, [r12 + rcx]
    mov     bl, [r12 + rdx]
    mov     [r12 + rcx], bl
    mov     [r12 + rdx], al
    inc     rcx
    dec     rdx
    jmp     .reverse_loop
.reverse_done:
    mov     rax, r13            ; return count
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; uint64_t pow10(uint32_t exp)
; Returns 10^exp.
; Input:  rdi = exponent (0-19 valid for 64-bit result)
; Output: rax = 10^exp
;------------------------------------------------------------------------------
pow10:
    mov     eax, 1
    test    edi, edi
    jz      .pow_done
    mov     ecx, edi
.pow_loop:
    imul    rax, rax, 10
    dec     ecx
    jnz     .pow_loop
.pow_done:
    ret

; long long ns_since(const struct timespec *start, const struct timespec *end)
; Returns nanoseconds between start and end.
ns_since:
    mov     rax, [rsi]        ; end.tv_sec
    sub     rax, [rdi]        ; end.tv_sec - start.tv_sec
    imul    rax, rax, 1000000000
    mov     rdx, [rsi + 8]    ; end.tv_nsec
    sub     rdx, [rdi + 8]    ; end.tv_nsec - start.tv_nsec
    add     rax, rdx
    ret

%define SYS_OPEN 2
%define SYS_READ 0
%define SYS_CLOSE 3

; ssize_t read_file_all(const char *path, void *buf, size_t bufsize)
; Returns bytes read or -1 on error.
; Note: syscall clobbers rcx and r11, so we use r14 for total.
read_file_all:
    push    rbp
    mov     rbp, rsp
    push    r12
    push    r13
    push    r14
    push    r15                 ; keep stack 16-byte aligned

    mov     r12, rsi            ; buf
    mov     r13, rdx            ; bufsize
    mov     r15, rdi            ; save path (rdi clobbered by syscall args)

    ; open(path, O_RDONLY, 0)
    mov     eax, SYS_OPEN
    ; rdi already contains path
    xor     esi, esi            ; O_RDONLY
    xor     edx, edx            ; mode = 0
    syscall
    cmp     rax, 0
    jl      .err
    mov     r10, rax            ; fd

    xor     r14d, r14d          ; total = 0 (use r14, not r11!)

.read_loop:
    cmp     r14, r13
    jge     .finish_ok

    mov     eax, SYS_READ
    mov     rdi, r10            ; fd
    lea     rsi, [r12 + r14]    ; buf + total
    mov     rdx, r13
    sub     rdx, r14            ; bufsize - total
    syscall

    test    rax, rax
    jl      .read_error
    je      .finish_ok

    add     r14, rax            ; accumulate total (r14 survives syscall)
    jmp     .read_loop

.finish_ok:
    mov     eax, SYS_CLOSE
    mov     rdi, r10
    syscall
    mov     rax, r14            ; return total bytes read
    jmp     .ret

.read_error:
    mov     eax, SYS_CLOSE
    mov     rdi, r10
    syscall
    mov     rax, -1
    jmp     .ret

.err:
    mov     rax, -1

.ret:
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbp
    ret

;------------------------------------------------------------------------------
; size_t lower_bound_u64(const uint64_t *arr, size_t n, uint64_t value)
; Binary search for first element >= value in sorted array.
; Input:  rdi = array pointer
;         rsi = array size (number of elements)
;         rdx = value to search for
; Output: rax = index of first element >= value, or n if none
;------------------------------------------------------------------------------
lower_bound_u64:
    xor     eax, eax            ; lo = 0
    mov     rcx, rsi            ; hi = n
.lb_loop:
    cmp     rax, rcx
    jge     .lb_done
    ; mid = (lo + hi) / 2
    lea     r8, [rax + rcx]
    shr     r8, 1               ; mid
    ; if arr[mid] < value: lo = mid + 1
    mov     r9, [rdi + r8*8]
    cmp     r9, rdx
    jae     .lb_not_less
    lea     rax, [r8 + 1]       ; lo = mid + 1
    jmp     .lb_loop
.lb_not_less:
    mov     rcx, r8             ; hi = mid
    jmp     .lb_loop
.lb_done:
    ret

;------------------------------------------------------------------------------
; size_t upper_bound_u64(const uint64_t *arr, size_t n, uint64_t value)
; Binary search for first element > value in sorted array.
; Input:  rdi = array pointer
;         rsi = array size (number of elements)
;         rdx = value to search for
; Output: rax = index of first element > value, or n if none
;------------------------------------------------------------------------------
upper_bound_u64:
    xor     eax, eax            ; lo = 0
    mov     rcx, rsi            ; hi = n
.ub_loop:
    cmp     rax, rcx
    jge     .ub_done
    ; mid = (lo + hi) / 2
    lea     r8, [rax + rcx]
    shr     r8, 1               ; mid
    ; if arr[mid] <= value: lo = mid + 1
    mov     r9, [rdi + r8*8]
    cmp     r9, rdx
    ja      .ub_greater
    lea     rax, [r8 + 1]       ; lo = mid + 1
    jmp     .ub_loop
.ub_greater:
    mov     rcx, r8             ; hi = mid
    jmp     .ub_loop
.ub_done:
    ret

;------------------------------------------------------------------------------
; void sort_u64(uint64_t *arr, size_t n)
; In-place quicksort for uint64 array with median-of-three pivot selection.
; Falls back to insertion sort for small subarrays.
; Input:  rdi = array pointer
;         rsi = array size (number of elements)
; Uses stack for recursion simulation.
;------------------------------------------------------------------------------
sort_u64:
    push    rbp
    mov     rbp, rsp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, 8              ; align stack

    ; Use stack to simulate recursion: push (lo, hi) pairs
    ; Initial: lo=0, hi=n-1
    test    rsi, rsi
    jz      .sort_done
    cmp     rsi, 1
    je      .sort_done

    mov     r12, rdi            ; arr base pointer
    ; Push initial range (0, n-1)
    xor     eax, eax            ; lo = 0
    lea     rcx, [rsi - 1]      ; hi = n - 1
    push    rcx                 ; push hi
    push    rax                 ; push lo

.sort_stack_loop:
    ; Check if stack is empty (back to original rsp+8 after alignment)
    lea     rax, [rbp - 48]     ; original rsp after pushes
    cmp     rsp, rax
    jge     .sort_done

    ; Pop lo, hi
    pop     r13                 ; lo
    pop     r14                 ; hi

    ; If lo >= hi, skip this range
    cmp     r13, r14
    jge     .sort_stack_loop

    ; For small ranges, use insertion sort
    mov     rax, r14
    sub     rax, r13
    cmp     rax, 16
    jle     .insertion_sort

    ; Median-of-three pivot selection: median of arr[lo], arr[mid], arr[hi]
    lea     rax, [r13 + r14]
    shr     rax, 1              ; mid = (lo + hi) / 2

    mov     r8, [r12 + r13*8]   ; a = arr[lo]
    mov     r9, [r12 + rax*8]   ; b = arr[mid]
    mov     r10, [r12 + r14*8]  ; c = arr[hi]

    ; Find median and swap it to arr[hi]
    ; if a <= b <= c or c <= b <= a: median = b (mid)
    ; if b <= a <= c or c <= a <= b: median = a (lo)
    ; if a <= c <= b or b <= c <= a: median = c (hi) - already there

    cmp     r8, r9
    jg      .med_a_gt_b
    ; a <= b
    cmp     r9, r10
    jle     .med_use_mid        ; a <= b <= c, median = b
    cmp     r8, r10
    jg      .med_use_hi         ; c < a <= b, median = a? No wait...
    jmp     .med_use_hi         ; a <= c < b, median = c (already at hi)

.med_a_gt_b:
    ; a > b
    cmp     r8, r10
    jle     .med_use_lo         ; b < a <= c, median = a
    cmp     r9, r10
    jg      .med_use_hi         ; c < b < a, median = c
    jmp     .med_use_mid        ; b <= c < a, median = c? No, b

.med_use_mid:
    ; Swap arr[mid] with arr[hi]
    mov     [r12 + rax*8], r10
    mov     [r12 + r14*8], r9
    jmp     .do_partition

.med_use_lo:
    ; Swap arr[lo] with arr[hi]
    mov     [r12 + r13*8], r10
    mov     [r12 + r14*8], r8
    jmp     .do_partition

.med_use_hi:
    ; Pivot already at hi, nothing to do

.do_partition:
    ; Partition: pivot = arr[hi]
    mov     r15, [r12 + r14*8]  ; pivot = arr[hi]
    mov     rbx, r13            ; i = lo
    mov     rcx, r13            ; j = lo

.partition_loop:
    cmp     rcx, r14
    jge     .partition_done
    ; if arr[j] < pivot: swap arr[i] and arr[j], i++
    mov     rax, [r12 + rcx*8]
    cmp     rax, r15
    jge     .partition_next
    ; swap arr[i] and arr[j]
    mov     rdx, [r12 + rbx*8]
    mov     [r12 + rbx*8], rax
    mov     [r12 + rcx*8], rdx
    inc     rbx                 ; i++
.partition_next:
    inc     rcx                 ; j++
    jmp     .partition_loop

.partition_done:
    ; swap arr[i] and arr[hi] (put pivot in place)
    mov     rax, [r12 + rbx*8]
    mov     rdx, [r12 + r14*8]
    mov     [r12 + rbx*8], rdx
    mov     [r12 + r14*8], rax
    ; pivot is now at index rbx

    ; Push right partition (i+1, hi) if non-empty
    lea     rax, [rbx + 1]
    cmp     rax, r14
    jg      .skip_right
    push    r14                 ; hi
    push    rax                 ; i+1
.skip_right:

    ; Push left partition (lo, i-1) if non-empty
    lea     rax, [rbx - 1]
    cmp     r13, rax
    jg      .skip_left
    push    rax                 ; i-1
    push    r13                 ; lo
.skip_left:

    jmp     .sort_stack_loop

.insertion_sort:
    ; Insertion sort for small range [lo, hi]
    lea     rcx, [r13 + 1]      ; i = lo + 1
.ins_outer:
    cmp     rcx, r14
    jg      .sort_stack_loop    ; done with this range

    mov     rax, [r12 + rcx*8]  ; key = arr[i]
    mov     rbx, rcx
    dec     rbx                 ; j = i - 1

.ins_inner:
    cmp     rbx, r13
    jl      .ins_place          ; j < lo
    mov     rdx, [r12 + rbx*8]  ; arr[j]
    cmp     rdx, rax
    jle     .ins_place          ; arr[j] <= key
    ; arr[j+1] = arr[j]
    mov     [r12 + rbx*8 + 8], rdx
    dec     rbx
    jmp     .ins_inner

.ins_place:
    ; arr[j+1] = key
    mov     [r12 + rbx*8 + 8], rax
    inc     rcx
    jmp     .ins_outer

.sort_done:
    add     rsp, 8
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

;------------------------------------------------------------------------------
; void sort_edges_3(uint64_t *dist, uint32_t *arr_i, uint32_t *arr_j, size_t n)
; In-place quicksort for 3 parallel arrays (dist as key, arr_i and arr_j as satellites).
; Sorts by dist ascending, keeping arr_i and arr_j in sync.
; Input:  rdi = dist array pointer (uint64_t)
;         rsi = arr_i array pointer (uint32_t)
;         rdx = arr_j array pointer (uint32_t)
;         rcx = array size (number of elements)
;------------------------------------------------------------------------------
sort_edges_3:
    push    rbp
    mov     rbp, rsp
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, 40             ; local storage + alignment

    ; Store array pointers
    mov     [rbp-48], rdi       ; dist
    mov     [rbp-56], rsi       ; arr_i
    mov     [rbp-64], rdx       ; arr_j
    mov     [rbp-72], rcx       ; n

    test    rcx, rcx
    jz      .se3_done
    cmp     rcx, 1
    je      .se3_done

    ; Push initial range (0, n-1)
    xor     eax, eax            ; lo = 0
    lea     r8, [rcx - 1]       ; hi = n - 1
    push    r8                  ; push hi
    push    rax                 ; push lo

.se3_stack_loop:
    ; Check if stack is empty
    lea     rax, [rbp - 80]     ; original rsp after pushes
    cmp     rsp, rax
    jge     .se3_done

    pop     r13                 ; lo
    pop     r14                 ; hi

    cmp     r13, r14
    jge     .se3_stack_loop

    ; For small ranges, use insertion sort
    mov     rax, r14
    sub     rax, r13
    cmp     rax, 16
    jle     .se3_insertion

    ; Load array pointers
    mov     r12, [rbp-48]       ; dist
    mov     r8, [rbp-56]        ; arr_i
    mov     r9, [rbp-64]        ; arr_j

    ; Median-of-three pivot: median of dist[lo], dist[mid], dist[hi]
    lea     rax, [r13 + r14]
    shr     rax, 1              ; mid

    mov     r10, [r12 + r13*8]  ; a = dist[lo]
    mov     r11, [r12 + rax*8]  ; b = dist[mid]
    mov     r15, [r12 + r14*8]  ; c = dist[hi]

    ; Find median and swap to hi position
    cmp     r10, r11
    jg      .se3_a_gt_b
    cmp     r11, r15
    jle     .se3_use_mid
    jmp     .se3_use_hi
.se3_a_gt_b:
    cmp     r10, r15
    jle     .se3_use_lo
    cmp     r11, r15
    jg      .se3_use_hi
    jmp     .se3_use_mid

.se3_use_mid:
    ; Swap [mid] with [hi] for all 3 arrays
    mov     [r12 + rax*8], r15
    mov     [r12 + r14*8], r11
    ; Swap arr_i
    mov     r10d, [r8 + rax*4]
    mov     r11d, [r8 + r14*4]
    mov     [r8 + rax*4], r11d
    mov     [r8 + r14*4], r10d
    ; Swap arr_j
    mov     r10d, [r9 + rax*4]
    mov     r11d, [r9 + r14*4]
    mov     [r9 + rax*4], r11d
    mov     [r9 + r14*4], r10d
    jmp     .se3_partition

.se3_use_lo:
    ; Swap [lo] with [hi]
    mov     r11, [r12 + r14*8]
    mov     [r12 + r13*8], r11
    mov     [r12 + r14*8], r10
    ; Swap arr_i
    mov     r10d, [r8 + r13*4]
    mov     r11d, [r8 + r14*4]
    mov     [r8 + r13*4], r11d
    mov     [r8 + r14*4], r10d
    ; Swap arr_j
    mov     r10d, [r9 + r13*4]
    mov     r11d, [r9 + r14*4]
    mov     [r9 + r13*4], r11d
    mov     [r9 + r14*4], r10d
    jmp     .se3_partition

.se3_use_hi:
    ; Pivot already at hi

.se3_partition:
    mov     r15, [r12 + r14*8]  ; pivot = dist[hi]
    mov     rbx, r13            ; i = lo
    mov     rcx, r13            ; j = lo

.se3_part_loop:
    cmp     rcx, r14
    jge     .se3_part_done
    mov     rax, [r12 + rcx*8]
    cmp     rax, r15
    jge     .se3_part_next
    ; Swap [i] and [j] for all 3 arrays
    mov     rdx, [r12 + rbx*8]
    mov     [r12 + rbx*8], rax
    mov     [r12 + rcx*8], rdx
    mov     r10d, [r8 + rbx*4]
    mov     r11d, [r8 + rcx*4]
    mov     [r8 + rbx*4], r11d
    mov     [r8 + rcx*4], r10d
    mov     r10d, [r9 + rbx*4]
    mov     r11d, [r9 + rcx*4]
    mov     [r9 + rbx*4], r11d
    mov     [r9 + rcx*4], r10d
    inc     rbx
.se3_part_next:
    inc     rcx
    jmp     .se3_part_loop

.se3_part_done:
    ; Swap [i] and [hi] to put pivot in place
    mov     rax, [r12 + rbx*8]
    mov     rdx, [r12 + r14*8]
    mov     [r12 + rbx*8], rdx
    mov     [r12 + r14*8], rax
    mov     r10d, [r8 + rbx*4]
    mov     r11d, [r8 + r14*4]
    mov     [r8 + rbx*4], r11d
    mov     [r8 + r14*4], r10d
    mov     r10d, [r9 + rbx*4]
    mov     r11d, [r9 + r14*4]
    mov     [r9 + rbx*4], r11d
    mov     [r9 + r14*4], r10d

    ; Push right partition (i+1, hi)
    lea     rax, [rbx + 1]
    cmp     rax, r14
    jg      .se3_skip_right
    push    r14
    push    rax
.se3_skip_right:
    ; Push left partition (lo, i-1)
    lea     rax, [rbx - 1]
    cmp     r13, rax
    jg      .se3_skip_left
    push    rax
    push    r13
.se3_skip_left:
    jmp     .se3_stack_loop

.se3_insertion:
    ; Insertion sort for small range
    mov     r12, [rbp-48]       ; dist
    mov     r8, [rbp-56]        ; arr_i
    mov     r9, [rbp-64]        ; arr_j
    lea     rcx, [r13 + 1]      ; i = lo + 1

.se3_ins_outer:
    cmp     rcx, r14
    jg      .se3_stack_loop

    mov     rax, [r12 + rcx*8]  ; key_dist
    mov     r10d, [r8 + rcx*4]  ; key_i
    mov     r11d, [r9 + rcx*4]  ; key_j
    mov     rbx, rcx
    dec     rbx                 ; j = i - 1

.se3_ins_inner:
    cmp     rbx, r13
    jl      .se3_ins_place
    mov     rdx, [r12 + rbx*8]
    cmp     rdx, rax
    jle     .se3_ins_place
    ; Shift [j] to [j+1]
    mov     [r12 + rbx*8 + 8], rdx
    mov     edx, [r8 + rbx*4]
    mov     [r8 + rbx*4 + 4], edx
    mov     edx, [r9 + rbx*4]
    mov     [r9 + rbx*4 + 4], edx
    dec     rbx
    jmp     .se3_ins_inner

.se3_ins_place:
    mov     [r12 + rbx*8 + 8], rax
    mov     [r8 + rbx*4 + 4], r10d
    mov     [r9 + rbx*4 + 4], r11d
    inc     rcx
    jmp     .se3_ins_outer

.se3_done:
    add     rsp, 40
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

section .note.GNU-stack noalloc noexec nowrite progbits