#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdbool.h>
#include <ctype.h>

#define MAX_LINES 100
#define MAX_WIDTH 65536

static inline long long ns_since(const struct timespec *start, const struct timespec *end) {
    return (long long)(end->tv_sec - start->tv_sec) * 1000000000LL + (end->tv_nsec - start->tv_nsec);
}

int main(void) {
    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("fopen");
        return EXIT_FAILURE;
    }

    // Read entire file as character grid
    char grid[MAX_LINES][MAX_WIDTH];
    int height = 0;
    int width = 0;

    while (fgets(grid[height], sizeof(grid[height]), fp)) {
        int len = strlen(grid[height]);
        // Remove trailing newline
        while (len > 0 && (grid[height][len-1] == '\n' || grid[height][len-1] == '\r')) {
            grid[height][len-1] = '\0';
            len--;
        }
        if (len > width) width = len;
        height++;
    }
    fclose(fp);

    // Pad all lines to same width with spaces
    for (int h = 0; h < height; h++) {
        int len = strlen(grid[h]);
        for (int c = len; c < width; c++) {
            grid[h][c] = ' ';
        }
        grid[h][width] = '\0';
    }

    // Last row contains operators
    int op_row = height - 1;
    int num_rows = height - 1;

    unsigned long long part1 = 0;
    unsigned long long part2 = 0;

    // Find problem boundaries (columns that are all spaces in number rows)
    // Process each problem
    int col = 0;
    while (col < width) {
        // Skip separator columns (all spaces)
        while (col < width) {
            bool all_space = true;
            for (int r = 0; r < num_rows; r++) {
                if (grid[r][col] != ' ') {
                    all_space = false;
                    break;
                }
            }
            if (!all_space) break;
            col++;
        }

        if (col >= width) break;

        // Find end of this problem (next all-space column or end)
        int prob_start = col;
        while (col < width) {
            bool all_space = true;
            for (int r = 0; r < num_rows; r++) {
                if (grid[r][col] != ' ') {
                    all_space = false;
                    break;
                }
            }
            if (all_space) break;
            col++;
        }
        int prob_end = col;

        // Find operator for this problem
        char op = ' ';
        for (int c = prob_start; c < prob_end; c++) {
            if (grid[op_row][c] == '+' || grid[op_row][c] == '*') {
                op = grid[op_row][c];
                break;
            }
        }

        if (op == ' ') continue;  // No valid operator found

        // Part 1: Parse numbers horizontally from each row
        unsigned long long p1_result = (op == '+') ? 0 : 1;
        for (int r = 0; r < num_rows; r++) {
            // Extract number from this row within problem bounds
            unsigned long long num = 0;
            bool has_digit = false;
            for (int c = prob_start; c < prob_end; c++) {
                if (isdigit(grid[r][c])) {
                    num = num * 10 + (grid[r][c] - '0');
                    has_digit = true;
                }
            }
            if (has_digit) {
                if (op == '+') {
                    p1_result += num;
                } else {
                    p1_result *= num;
                }
            }
        }
        part1 += p1_result;

        // Part 2: Parse numbers vertically from each column (right to left)
        unsigned long long p2_result = (op == '+') ? 0 : 1;
        for (int c = prob_end - 1; c >= prob_start; c--) {
            // Extract number from this column (top to bottom)
            unsigned long long num = 0;
            bool has_digit = false;
            for (int r = 0; r < num_rows; r++) {
                if (isdigit(grid[r][c])) {
                    num = num * 10 + (grid[r][c] - '0');
                    has_digit = true;
                }
            }
            if (has_digit) {
                if (op == '+') {
                    p2_result += num;
                } else {
                    p2_result *= num;
                }
            }
        }
        part2 += p2_result;
    }

    clock_gettime(CLOCK_MONOTONIC, &t1);
    double elapsed_ms = ns_since(&t0, &t1) / 1e6;

    printf("grand_total=%llu quantum_total=%llu elapsed_ms=%.3f\n",
           part1, part2, elapsed_ms);

    return EXIT_SUCCESS;
}
