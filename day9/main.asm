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

    ; Check if point is on this edge
    cmp     r8, r10
    jne     .check_horiz_edge
    ; Vertical edge: x1 == x2
    cmp     rdi, r8                     ; px == x1?
    jne     .pip_ray_test
    ; Check if py in range [min(y1,y2), max(y1,y2)]
    mov     rax, r9
    mov     rdx, r11
    cmp     rax, rdx
    jle     .v_minmax_ok
    xchg    rax, rdx
.v_minmax_ok:
    cmp     rsi, rax                    ; py < min?
    jl      .pip_ray_test
    cmp     rsi, rdx                    ; py > max?
    jg      .pip_ray_test
    jmp     .pip_on_edge                ; point is on this edge

.check_horiz_edge:
    cmp     r9, r11
    jne     .pip_ray_test
    ; Horizontal edge: y1 == y2
    cmp     rsi, r9                     ; py == y1?
    jne     .pip_ray_test
    ; Check if px in range [min(x1,x2), max(x1,x2)]
    mov     rax, r8
    mov     rdx, r10
    cmp     rax, rdx
    jle     .h_minmax_ok
    xchg    rax, rdx
.h_minmax_ok:
    cmp     rdi, rax                    ; px < min?
    jl      .pip_ray_test
    cmp     rdi, rdx                    ; px > max?
    jg      .pip_ray_test
    jmp     .pip_on_edge                ; point is on this edge

.pip_ray_test:
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

.pip_on_edge:
    ; Point is on edge, return 1
    mov     eax, 1
    jmp     .pip_ret

.pip_done:
    ; odd crossings = inside
    mov     eax, r15d
    and     eax, 1

.pip_ret:
    add     rsp, 32
    pop     rbp
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    ret

;------------------------------------------------------------------------------
; bool rect_in_polygon(int64 xlo, int64 ylo, int64 xhi, int64 yhi, int n)
; Check if all 4 corners are inside polygon AND no edge crosses interior
;------------------------------------------------------------------------------
rect_in_polygon:
    push    rbx
    push    r12
    push    r13
    push    r14
    push    r15
    sub     rsp, 56

    ; Store args (xlo, ylo, xhi, yhi, n)
    mov     [rsp], rdi                  ; xlo
    mov     [rsp+8], rsi                ; ylo
    mov     [rsp+16], rdx               ; xhi
    mov     [rsp+24], rcx               ; yhi
    mov     [rsp+32], r8d               ; n

    ; Check corner (xlo, ylo)
    mov     rdi, [rsp]
    mov     rsi, [rsp+8]
    mov     edx, [rsp+32]
    call    point_in_polygon
    test    eax, eax
    jz      .rip_false

    ; Check corner (xhi, ylo)
    mov     rdi, [rsp+16]
    mov     rsi, [rsp+8]
    mov     edx, [rsp+32]
    call    point_in_polygon
    test    eax, eax
    jz      .rip_false

    ; Check corner (xlo, yhi)
    mov     rdi, [rsp]
    mov     rsi, [rsp+24]
    mov     edx, [rsp+32]
    call    point_in_polygon
    test    eax, eax
    jz      .rip_false

    ; Check corner (xhi, yhi)
    mov     rdi, [rsp+16]
    mov     rsi, [rsp+24]
    mov     edx, [rsp+32]
    call    point_in_polygon
    test    eax, eax
    jz      .rip_false

    ; Check no polygon edge crosses the rectangle interior
    lea     r12, [rel points_x]
    lea     r13, [rel points_y]
    mov     r14d, [rsp+32]              ; n
    xor     ebx, ebx                    ; i = 0

.edge_check_loop:
    cmp     ebx, r14d
    jge     .rip_true

    ; j = (i + 1) % n
    mov     ecx, ebx
    inc     ecx
    cmp     ecx, r14d
    jl      .edge_j_ok
    xor     ecx, ecx
.edge_j_ok:
    ; Get edge endpoints
    mov     r8, [r12 + rbx*8]           ; x1
    mov     r9, [r13 + rbx*8]           ; y1
    mov     r10, [r12 + rcx*8]          ; x2
    mov     r11, [r13 + rcx*8]          ; y2

    ; Check if vertical edge crosses interior
    cmp     r8, r10
    jne     .check_horiz
    ; Vertical edge: x1 == x2
    mov     rax, [rsp]                  ; xlo
    cmp     r8, rax
    jle     .edge_next
    mov     rax, [rsp+16]               ; xhi
    cmp     r8, rax
    jge     .edge_next
    ; x is strictly inside, check y overlap
    mov     rax, r9
    cmp     rax, r11
    jle     .v_ya_ok
    xchg    rax, r11
.v_ya_ok:                               ; rax = min(y1,y2), r11 = max(y1,y2)
    mov     rcx, [rsp+24]               ; yhi
    cmp     r11, [rsp+8]                ; ylo
    jle     .edge_next
    cmp     rax, rcx
    jge     .edge_next
    jmp     .rip_false                  ; Edge crosses interior

.check_horiz:
    cmp     r9, r11
    jne     .edge_next
    ; Horizontal edge: y1 == y2
    mov     rax, [rsp+8]                ; ylo
    cmp     r9, rax
    jle     .edge_next
    mov     rax, [rsp+24]               ; yhi
    cmp     r9, rax
    jge     .edge_next
    ; y is strictly inside, check x overlap
    mov     rax, r8
    cmp     rax, r10
    jle     .h_xa_ok
    xchg    rax, r10
.h_xa_ok:                               ; rax = min(x1,x2), r10 = max(x1,x2)
    mov     rcx, [rsp+16]               ; xhi
    cmp     r10, [rsp]                  ; xlo
    jle     .edge_next
    cmp     rax, rcx
    jge     .edge_next
    jmp     .rip_false                  ; Edge crosses interior

.edge_next:
    inc     ebx
    jmp     .edge_check_loop

.rip_true:
    mov     eax, 1
    jmp     .rip_done

.rip_false:
    xor     eax, eax

.rip_done:
    add     rsp, 56
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
    sub     rsp, 88

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

    ; area = (abs(x1-x2) + 1) * (abs(y1-y2) + 1)
    sub     rax, rdx
    mov     rdx, rax
    sar     rdx, 63
    xor     rax, rdx
    sub     rax, rdx                    ; abs(x1-x2)
    inc     rax                         ; + 1
    mov     r12, rax

    mov     rax, r10
    sub     rax, r11
    mov     rdx, rax
    sar     rdx, 63
    xor     rax, rdx
    sub     rax, rdx                    ; abs(y1-y2)
    inc     rax                         ; + 1

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
    mov     qword [rbp-80], 0           ; best2 = 0

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

    ; Save original coordinates for area calculation
    mov     [rbp-88], rax               ; x1
    mov     [rbp-96], rdx               ; x2
    mov     [rbp-104], r10              ; y1
    mov     [rbp-112], r11              ; y2
    mov     [rbp-120], ecx              ; save j loop counter

    ; Compute min/max for xlo, xhi, ylo, yhi
    ; xlo = min(x1, x2), xhi = max(x1, x2)
    mov     rdi, rax                    ; xlo = x1
    mov     r12, rdx                    ; xhi = x2
    cmp     rdi, r12
    jle     .x_sorted
    xchg    rdi, r12                    ; swap if x1 > x2
.x_sorted:
    ; ylo = min(y1, y2), yhi = max(y1, y2)
    mov     rsi, r10                    ; ylo = y1
    mov     rcx, r11                    ; yhi = y2
    cmp     rsi, rcx
    jle     .y_sorted
    xchg    rsi, rcx                    ; swap if y1 > y2
.y_sorted:
    ; rdi=xlo, rsi=ylo, r12=xhi, rcx=yhi
    mov     rdx, r12                    ; rdx = xhi
    mov     r8d, r14d                   ; n
    call    rect_in_polygon
    mov     ecx, [rbp-120]              ; restore j loop counter
    test    eax, eax
    jz      .p2_next_j

    ; Calculate area = (abs(x1-x2) + 1) * (abs(y1-y2) + 1)
    mov     rax, [rbp-88]
    sub     rax, [rbp-96]
    mov     rdx, rax
    sar     rdx, 63
    xor     rax, rdx
    sub     rax, rdx                    ; abs(x1-x2)
    inc     rax                         ; + 1
    mov     r12, rax

    mov     rax, [rbp-104]
    sub     rax, [rbp-112]
    mov     rdx, rax
    sar     rdx, 63
    xor     rax, rdx
    sub     rax, rdx                    ; abs(y1-y2)
    inc     rax                         ; + 1

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
    add     rsp, 88
    pop     r15
    pop     r14
    pop     r13
    pop     r12
    pop     rbx
    pop     rbp
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
