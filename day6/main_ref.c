#include <ctype.h>
#include <stdio.h>
#include <string.h>

#define INPUT_FILE "input.txt"
#define MAX_ROWS 10
#define MAX_COLS 4000
#define MAX_PROBLEMS 1500

typedef struct {
    int start; /* inclusive column */
    int end;   /* exclusive column */
    char op;   /* '+' or '*' */
} Problem;

int main(void) {
    FILE *fp = fopen(INPUT_FILE, "r");
    if (!fp) {
        perror(INPUT_FILE);
        return 1;
    }

    static char grid[MAX_ROWS][MAX_COLS];
    int rows = 0;
    int width = 0;

    char line[MAX_COLS + 8];
    while (fgets(line, sizeof(line), fp)) {
        if (rows >= MAX_ROWS) {
            fprintf(stderr, "Too many rows (max %d)\n", MAX_ROWS);
            fclose(fp);
            return 1;
        }

        int len = (int)strcspn(line, "\r\n");
        if (len > MAX_COLS) {
            fprintf(stderr, "Line too long (max %d)\n", MAX_COLS);
            fclose(fp);
            return 1;
        }

        /* Pad to spaces so every row has MAX_COLS characters. */
        memset(grid[rows], ' ', MAX_COLS);
        memcpy(grid[rows], line, (size_t)len);

        if (len > width) {
            width = len;
        }
        rows++;
    }
    fclose(fp);

    if (rows < 2) {
        fprintf(stderr, "Need at least one digit row and one operator row\n");
        return 1;
    }

    int digit_rows = rows - 1;
    int operator_row = rows - 1;

    /* Identify separator columns (all spaces). */
    static int sep[MAX_COLS];
    for (int c = 0; c < width; ++c) {
        int all_space = 1;
        for (int r = 0; r < rows; ++r) {
            if (grid[r][c] != ' ') {
                all_space = 0;
                break;
            }
        }
        sep[c] = all_space;
    }

    /* Collect problems as contiguous non-separator column ranges. */
    static Problem problems[MAX_PROBLEMS];
    int problem_count = 0;
    int c = 0;
    while (c < width) {
        if (sep[c]) {
            c++;
            continue;
        }
        int start = c;
        while (c < width && !sep[c]) {
            c++;
        }
        int end = c;

        char op = '+';
        for (int col = start; col < end; ++col) {
            char ch = grid[operator_row][col];
            if (ch == '+' || ch == '*') {
                op = ch;
                break;
            }
        }

        if (problem_count >= MAX_PROBLEMS) {
            fprintf(stderr, "Too many problems (max %d)\n", MAX_PROBLEMS);
            return 1;
        }
        problems[problem_count++] = (Problem){start, end, op};
    }

    unsigned long long total = 0;

    for (int p = 0; p < problem_count; ++p) {
        Problem cur = problems[p];
        unsigned long long value = (cur.op == '*') ? 1ULL : 0ULL;

        for (int col = cur.start; col < cur.end; ++col) {
            unsigned long long num = 0;
            for (int row = 0; row < digit_rows; ++row) {
                char ch = grid[row][col];
                if (isdigit((unsigned char)ch)) {
                    num = num * 10ULL + (unsigned long long)(ch - '0');
                }
            }

            if (cur.op == '*') {
                value *= num;
            } else {
                value += num;
            }
        }

        total += value;
    }

    printf("Part 2: %llu\n", total);
    return 0;
}
