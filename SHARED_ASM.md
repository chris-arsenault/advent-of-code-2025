# Shared Assembly Utilities Reference

> **Living Document**: This file documents the shared x86-64 assembly utilities in `shared/utils.asm`. It is intended to be updated as new shared code is implemented or extracted from day-to-day assembly solutions.

---

## Overview

The shared utilities provide common functionality for Advent of Code solutions written in x86-64 assembly. All functions use the System V AMD64 ABI calling convention (Linux).

**Location:** `shared/utils.asm`

**Linking:** Include in your build with:
```bash
nasm -f elf64 ../shared/utils.asm -o utils.o
nasm -f elf64 main.asm -o main.o
ld -o main main.o utils.o
```

---

## Calling Convention Quick Reference

| Register | Purpose |
|----------|---------|
| `rdi` | 1st argument |
| `rsi` | 2nd argument |
| `rdx` | 3rd argument |
| `rcx` | 4th argument |
| `r8` | 5th argument |
| `r9` | 6th argument |
| `rax` | Return value |
| `rbx, rbp, r12-r15` | Callee-saved (preserved) |
| `rcx, r11` | Clobbered by `syscall` |

---

## Functions

### `parse_uint64`

Parses an unsigned 64-bit integer from a string.

```asm
; uint64_t parse_uint64(const char **pptr, const char *end)
```

| Parameter | Register | Description |
|-----------|----------|-------------|
| `pptr` | `rdi` | Pointer to pointer to current position |
| `end` | `rsi` | End of buffer pointer |
| **Return** | `rax` | Parsed value (0 if no digits) |

**Behavior:**
- Reads consecutive decimal digits starting at `*pptr`
- Updates `*pptr` to point after the last digit
- Stops at first non-digit character or when reaching `end`
- Returns 0 if no digits found (does not distinguish from parsing "0")

**Example:**
```asm
    lea     rdi, [ptr_var]      ; address of pointer variable
    mov     rsi, buf_end
    call    parse_uint64
    ; rax = parsed number
    ; ptr_var now points past the digits
```

---

### `skip_non_digits`

Advances a pointer past whitespace and common separators to the next digit.

```asm
; const char* skip_non_digits(const char *ptr, const char *end)
```

| Parameter | Register | Description |
|-----------|----------|-------------|
| `ptr` | `rdi` | Current position |
| `end` | `rsi` | End of buffer |
| **Return** | `rax` | Pointer to first digit or `end` |

**Skipped Characters:**
- Space (` `)
- Comma (`,`)
- Dash (`-`)
- Newline (`\n`, ASCII 10)
- Carriage return (`\r`, ASCII 13)
- Tab (`\t`, ASCII 9)
- Any other non-digit character

**Example:**
```asm
    mov     rdi, current_ptr
    mov     rsi, buf_end
    call    skip_non_digits
    mov     current_ptr, rax    ; now points to digit or end
```

---

### `uint64_digit_count`

Returns the number of decimal digits in a 64-bit unsigned integer.

```asm
; uint64_t uint64_digit_count(uint64_t n)
```

| Parameter | Register | Description |
|-----------|----------|-------------|
| `n` | `rdi` | Number to count digits of |
| **Return** | `rax` | Digit count (1-20) |

**Notes:**
- Returns 1 for n=0
- Valid range: 0 to 18,446,744,073,709,551,615 (20 digits max)

---

### `uint64_to_digits`

Extracts decimal digits of a number into a buffer (most significant first).

```asm
; uint64_t uint64_to_digits(uint64_t n, uint8_t *buf)
```

| Parameter | Register | Description |
|-----------|----------|-------------|
| `n` | `rdi` | Number to convert |
| `buf` | `rsi` | Output buffer (min 20 bytes) |
| **Return** | `rax` | Digit count |

**Output Format:**
- Digits stored as raw byte values 0-9 (NOT ASCII '0'-'9')
- Most significant digit at `buf[0]`
- To convert to ASCII, add `'0'` to each byte

**Example:**
```asm
    mov     rdi, 12345
    lea     rsi, [digit_buf]
    call    uint64_to_digits
    ; rax = 5
    ; digit_buf = [1, 2, 3, 4, 5, ?, ?, ...]
```

---

### `pow10`

Computes 10 raised to a power.

```asm
; uint64_t pow10(uint32_t exp)
```

| Parameter | Register | Description |
|-----------|----------|-------------|
| `exp` | `edi` | Exponent (0-19 valid) |
| **Return** | `rax` | 10^exp |

**Valid Range:**
- exp=0 returns 1
- exp=19 returns 10,000,000,000,000,000,000
- exp >= 20 overflows 64-bit

---

### `ns_since`

Calculates elapsed nanoseconds between two `timespec` structures.

```asm
; int64_t ns_since(const struct timespec *start, const struct timespec *end)
```

| Parameter | Register | Description |
|-----------|----------|-------------|
| `start` | `rdi` | Start time pointer |
| `end` | `rsi` | End time pointer |
| **Return** | `rax` | Nanoseconds elapsed |

**timespec Layout:**
```
offset 0:  tv_sec  (8 bytes, int64_t)
offset 8:  tv_nsec (8 bytes, int64_t)
```

**Example:**
```asm
section .bss
    t0: resb 16
    t1: resb 16

section .text
    ; Get start time
    mov     eax, 228            ; SYS_clock_gettime
    xor     edi, edi            ; CLOCK_REALTIME (or 1 for CLOCK_MONOTONIC)
    lea     rsi, [t0]
    syscall

    ; ... do work ...

    ; Get end time
    mov     eax, 228
    xor     edi, edi
    lea     rsi, [t1]
    syscall

    ; Calculate elapsed
    lea     rdi, [t0]
    lea     rsi, [t1]
    call    ns_since
    ; rax = nanoseconds elapsed
```

---

### `read_file_all`

Reads an entire file into a buffer using raw syscalls.

```asm
; ssize_t read_file_all(const char *path, void *buf, size_t bufsize)
```

| Parameter | Register | Description |
|-----------|----------|-------------|
| `path` | `rdi` | Null-terminated file path |
| `buf` | `rsi` | Destination buffer |
| `bufsize` | `rdx` | Maximum bytes to read |
| **Return** | `rax` | Bytes read, or -1 on error |

**Behavior:**
- Opens file read-only
- Reads in a loop until EOF or buffer full
- Closes file handle before returning
- Returns -1 if open fails or read error occurs

**Example:**
```asm
section .data
    filename: db "input.txt", 0

section .bss
    buffer: resb 65536

section .text
    lea     rdi, [filename]
    lea     rsi, [buffer]
    mov     rdx, 65536
    call    read_file_all
    cmp     rax, 0
    jl      .error
    ; rax = bytes read, buffer contains file contents
```

---

## Syscall Numbers Reference

Defined in utils.asm for internal use:

| Constant | Value | Description |
|----------|-------|-------------|
| `SYS_READ` | 0 | Read from file descriptor |
| `SYS_OPEN` | 2 | Open file |
| `SYS_CLOSE` | 3 | Close file descriptor |

Additional commonly used syscalls:

| Number | Name | Description |
|--------|------|-------------|
| 1 | `SYS_WRITE` | Write to file descriptor |
| 60 | `SYS_EXIT` | Terminate process |
| 228 | `SYS_CLOCK_GETTIME` | Get clock time |

---

## Future Additions

Candidates for extraction from day solutions:

- [ ] `print_uint64` - Print number to stdout
- [ ] `print_string` - Print null-terminated string
- [ ] `memset` / `memcpy` - Memory operations
- [ ] `atoi_signed` - Parse signed integers
- [ ] `skip_line` - Advance to next line
- [ ] `count_lines` - Count newlines in buffer
- [ ] `hash_string` - Simple string hashing
- [ ] `sort_uint64` - In-place quicksort/radix sort
- [ ] `binary_search` - Binary search in sorted array
- [ ] `gcd` / `lcm` - Math utilities

---

## Performance Notes

### Register Preservation
- Functions preserve `rbx`, `rbp`, `r12-r15` per ABI
- `syscall` instruction clobbers `rcx` and `r11` - use `r12-r15` for values that must survive syscalls

### Optimization Opportunities
- `uint64_digit_count`: Could use BSR + lookup table for O(1)
- `pow10`: Could use lookup table instead of loop
- `parse_uint64`: Could use SIMD for parallel digit conversion

### SIMD Considerations
When extending for SIMD (SSE2/AVX2):
- Align buffers to 16/32 bytes for vector loads
- Use `pmovmskb` for parallel character classification
- Consider `pshufb` for parallel digit conversion
