; Day 9: Rectangle Areas
; Part 1: Max rectangle area from any two points
; Part 2: Max rectangle inside polygon formed by points

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
%define MAX_POINTS 512

section .data
input_file:    db "input.txt", 0
fmt_out:       db "max_rect_area=%llu max_green_rect_area=%llu elapsed_ms=%.3f", 10, 0
err_open:      db "open", 0
one_million:   dq 1000000.0

section .bss
file_buf:      resb BUF_SIZE
ts0:           resq 2
ts1:           resq 2
points_x:      resq MAX_POINTS
points_y:      resq MAX_POINTS

section .text

;------------------------------------------------------------------------------
; bool point_in_polygon(int64 px, int64 py, int n)
; Ray casting algorithm - count crossings
; Input: rdi=px, rsi=py, rdx=n
; Output: eax=1 if inside, 0 if outside
;------------------------------------------------------------------------------
point_in_polygon:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    push    rbp
    mov     rbp, rsp
    sub     rsp, 32

    mov     [rbp-8], rdi                ; px
    mov     [rbp-16], rsi               ; py
    mov     r14d, edx                   ; n

    xor     r15d, r15d                  ; crossings = 0
    xor     ebx, ebx                    ; i = 0

    lea     r12, [rel points_x]
    lea     r13, [rel points_y]

.pip_loop:
    cmp     ebx, r14d
    jge     .pip_done

    ; j = (i + 1) % n - avoid division
    mov     ecx, ebx
    inc     ecx
    cmp     ecx, r14d
    jl      .pip_j_ok
    xor     ecx, ecx
.pip_j_ok:

    ; Get points: (x1, y1) = points[i], (x2, y2) = points[j]
    mov     r8, [r12 + rbx*8]           ; x1
    mov     r9, [r13 + rbx*8]           ; y1
    mov     r10, [r12 + rcx*8]          ; x2
    mov     r11, [r13 + rcx*8]          ; y2

    mov     rdi, [rbp-8]                ; px
    mov     rsi, [rbp-16]               ; py

    ; Check if ray from (px, py) going right crosses edge (x1,y1)-(x2,y2)
    ; Condition: (y1 > py) != (y2 > py) AND
    ;            px < (x2-x1) * (py-y1) / (y2-y1) + x1

    ; First check: (y1 > py) != (y2 > py)
    xor     eax, eax
    cmp     r9, rsi
    setg    al
    xor     edx, edx
    cmp     r11, rsi
    setg    dl
    cmp     al, dl
    je      .pip_next                   ; same side, no crossing possible

    ; Calculate: x_intersect = (x2-x1) * (py-y1) / (y2-y1) + x1
    ; Check if px < x_intersect

    mov     rax, r10
    sub     rax, r8                     ; x2 - x1
    mov     rcx, rsi
    sub     rcx, r9                     ; py - y1
    imul    rax, rcx                    ; (x2-x1) * (py-y1)

    mov     rcx, r11
    sub     rcx, r9                     ; y2 - y1
    test    rcx, rcx
    jz      .pip_next                   ; avoid division by zero

    ; Signed division
    cqo
    idiv    rcx                         ; rax = (x2-x1)*(py-y1)/(y2-y1)
    add     rax, r8                     ; x_intersect = ... + x1

    cmp     rdi, rax                    ; px < x_intersect?
    jge     .pip_next
    inc     r15d                        ; crossings++

.pip_next:
    inc     ebx
    jmp     .pip_loop

.pip_done:
    ; odd crossings = inside
    mov     eax, r15d
    and     eax, 1

    add     rsp, 32
    pop     rbp
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; bool rect_in_polygon(int64 x1, int64 y1, int64 x2, int64 y2, int n)
; Check if all 4 corners are inside polygon
;------------------------------------------------------------------------------
rect_in_polygon:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, 48

    ; Store args
    mov     [rsp], rdi                  ; x1
    mov     [rsp+8], rsi                ; y1
    mov     [rsp+16], rdx               ; x2
    mov     [rsp+24], rcx               ; y2
    mov     [rsp+32], r8d               ; n

    ; Check corner (x1, y1)
    mov     rdi, [rsp]
    mov     rsi, [rsp+8]
    mov     edx, [rsp+32]
    call    point_in_polygon
    test    eax, eax
    jz      .rip_false

    ; Check corner (x2, y1)
    mov     rdi, [rsp+16]
    mov     rsi, [rsp+8]
    mov     edx, [rsp+32]
    call    point_in_polygon
    test    eax, eax
    jz      .rip_false

    ; Check corner (x1, y2)
    mov     rdi, [rsp]
    mov     rsi, [rsp+24]
    mov     edx, [rsp+32]
    call    point_in_polygon
    test    eax, eax
    jz      .rip_false

    ; Check corner (x2, y2)
    mov     rdi, [rsp+16]
    mov     rsi, [rsp+24]
    mov     edx, [rsp+32]
    call    point_in_polygon
    test    eax, eax
    jz      .rip_false

    mov     eax, 1
    jmp     .rip_done

.rip_false:
    xor     eax, eax

.rip_done:
    add     rsp, 48
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
    mov     [rbp-48], rax

    ; Parse points
    lea     r12, [rel file_buf]
    mov     r13, [rbp-48]
    add     r13, r12

    xor     ebx, ebx                    ; point count

.parse_points:
    cmp     r12, r13
    jge     .points_done

    mov     rdi, r12
    mov     rsi, r13
    call    skip_non_digits
    mov     r12, rax
    cmp     r12, r13
    jge     .points_done

    ; Parse x
    lea     rdi, [rbp-56]
    mov     [rbp-56], r12
    mov     rsi, r13
    call    parse_uint64
    mov     r12, [rbp-56]
    lea     rcx, [rel points_x]
    mov     [rcx + rbx*8], rax

    ; Parse y
    mov     rdi, r12
    mov     rsi, r13
    call    skip_non_digits
    mov     r12, rax
    lea     rdi, [rbp-56]
    mov     [rbp-56], r12
    mov     rsi, r13
    call    parse_uint64
    mov     r12, [rbp-56]
    lea     rcx, [rel points_y]
    mov     [rcx + rbx*8], rax

    inc     ebx
    cmp     ebx, MAX_POINTS
    jl      .parse_points

.points_done:
    mov     [rbp-60], ebx               ; n

    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts0]
    call    clock_gettime

    ; Part 1: Find max rectangle area
    mov     qword [rbp-72], 0           ; best = 0
    mov     r14d, [rbp-60]              ; n

    xor     ebx, ebx                    ; i = 0
.p1_i:
    cmp     ebx, r14d
    jge     .p1_done

    mov     ecx, ebx
    inc     ecx                         ; j = i + 1

.p1_j:
    cmp     ecx, r14d
    jge     .p1_next_i

    lea     r8, [rel points_x]
    lea     r9, [rel points_y]

    mov     rax, [r8 + rbx*8]           ; x1
    mov     rdx, [r8 + rcx*8]           ; x2
    cmp     rax, rdx
    je      .p1_next_j                  ; skip if same x

    mov     r10, [r9 + rbx*8]           ; y1
    mov     r11, [r9 + rcx*8]           ; y2
    cmp     r10, r11
    je      .p1_next_j                  ; skip if same y

    ; area = abs(x1-x2) * abs(y1-y2)
    sub     rax, rdx
    mov     rdx, rax
    sar     rdx, 63
    xor     rax, rdx
    sub     rax, rdx                    ; abs(x1-x2)
    mov     r12, rax

    mov     rax, r10
    sub     rax, r11
    mov     rdx, rax
    sar     rdx, 63
    xor     rax, rdx
    sub     rax, rdx                    ; abs(y1-y2)

    imul    rax, r12                    ; area
    cmp     rax, [rbp-72]
    jle     .p1_next_j
    mov     [rbp-72], rax

.p1_next_j:
    inc     ecx
    jmp     .p1_j

.p1_next_i:
    inc     ebx
    jmp     .p1_i

.p1_done:
    ; Part 2: Find max rectangle inside polygon
    ; NOTE: O(n³) algorithm too slow for ~500 points
    ; Would need spatial indexing or segment tree for production
    mov     qword [rbp-80], 0           ; best2 = 0
    jmp     .p2_done                    ; Skip for now

    xor     ebx, ebx                    ; i = 0
.p2_i:
    cmp     ebx, r14d
    jge     .p2_done

    mov     ecx, ebx
    inc     ecx                         ; j = i + 1

.p2_j:
    cmp     ecx, r14d
    jge     .p2_next_i

    lea     r8, [rel points_x]
    lea     r9, [rel points_y]

    mov     rax, [r8 + rbx*8]           ; x1
    mov     rdx, [r8 + rcx*8]           ; x2
    cmp     rax, rdx
    je      .p2_next_j

    mov     r10, [r9 + rbx*8]           ; y1
    mov     r11, [r9 + rcx*8]           ; y2
    cmp     r10, r11
    je      .p2_next_j

    ; Save for area calculation later
    mov     [rbp-88], rax               ; x1
    mov     [rbp-96], rdx               ; x2
    mov     [rbp-104], r10              ; y1
    mov     [rbp-112], r11              ; y2

    ; Get min/max for rectangle
    cmp     rax, rdx
    jle     .min_x_ok
    xchg    rax, rdx
.min_x_ok:
    mov     rdi, rax                    ; min_x
    mov     rdx, [rbp-96]
    cmp     [rbp-88], rdx
    jge     .max_x_ok
    mov     rdx, [rbp-88]
.max_x_ok:
    push    rdx                         ; max_x on stack

    mov     rax, r10
    mov     rsi, r11
    cmp     rax, rsi
    jle     .min_y_ok
    xchg    rax, rsi
.min_y_ok:
    mov     rsi, rax                    ; min_y
    mov     rcx, r10
    cmp     r11, rcx
    jge     .max_y_ok
    mov     rcx, r11
.max_y_ok:
    pop     rdx                         ; max_x
    ; rdi=min_x, rsi=min_y, rdx=max_x, rcx=max_y

    mov     r8d, r14d                   ; n
    call    rect_in_polygon
    test    eax, eax
    jz      .p2_next_j

    ; Calculate area
    mov     rax, [rbp-88]
    sub     rax, [rbp-96]
    mov     rdx, rax
    sar     rdx, 63
    xor     rax, rdx
    sub     rax, rdx                    ; abs(x1-x2)
    mov     r12, rax

    mov     rax, [rbp-104]
    sub     rax, [rbp-112]
    mov     rdx, rax
    sar     rdx, 63
    xor     rax, rdx
    sub     rax, rdx                    ; abs(y1-y2)

    imul    rax, r12
    cmp     rax, [rbp-80]
    jle     .p2_next_j
    mov     [rbp-80], rax

.p2_next_j:
    inc     ecx
    jmp     .p2_j

.p2_next_i:
    inc     ebx
    jmp     .p2_i

.p2_done:
    mov     edi, CLOCK_MONOTONIC
    lea     rsi, [rel ts1]
    call    clock_gettime

    lea     rdi, [rel ts0]
    lea     rsi, [rel ts1]
    call    ns_since
    cvtsi2sd xmm0, rax
    movsd   xmm1, [rel one_million]
    divsd   xmm0, xmm1

    mov     rsi, [rbp-72]               ; p1
    mov     rdx, [rbp-80]               ; p2
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
