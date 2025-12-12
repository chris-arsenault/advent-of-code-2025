#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define LINE_BUF 4096
#define GRID_SIZE 2048

static int start_row = 0;
static int start_col = 0;

static inline int parta(char grid[GRID_SIZE][GRID_SIZE], int height, int width ) {
    int active[GRID_SIZE];
    int next[GRID_SIZE];
    int seen[GRID_SIZE];
    for (int c = 0; c < width; c++) {
        active[c] = 0;
    }
    active[start_col] = 1;

    int splits = 0;

    for (int row = start_row; row < height; row++) {
        int queue[GRID_SIZE];
        int head = 0;
        int tail = 0;

        for (int c = 0; c < width; c++) {
            next[c] = 0;
            seen[c] = 0;
            if (active[c]) {
                queue[tail++] = c;
            }
        }

        while (head < tail) {
            int col = queue[head++];
            if (seen[col]) continue;
            seen[col] = 1;

            char cell = grid[row][col];
            if (cell == '^') {
                splits++;
                if (col > 0) queue[tail++] = col - 1;
                if (col + 1 < width) queue[tail++] = col + 1;
            } else {
                next[col] = 1;
            }
        }

        int has_beam = 0;
        for (int c = 0; c < width; c++) {
            active[c] = next[c];
            if (next[c]) has_beam = 1;
        }
        if (!has_beam) break;
    }

    return splits;
}


static inline unsigned long long  partb(char grid[GRID_SIZE][GRID_SIZE], int height, int width ) {
    unsigned long long active[GRID_SIZE];
    unsigned long long next[GRID_SIZE];
    unsigned long long pending[GRID_SIZE];
    char in_queue[GRID_SIZE];
    int queue[GRID_SIZE * 8];

    for (int c = 0; c < width; c++) {
        active[c] = 0;
    }
    active[start_col] = 1;

    for (int row = start_row; row < height; row++) {
        int head = 0;
        int tail = 0;
        for (int c = 0; c < width; c++) {
            next[c] = 0;
            pending[c] = 0;
            in_queue[c] = 0;
            if (active[c]) {
                pending[c] = active[c];
                queue[tail++] = c;
                in_queue[c] = 1;
            }
        }

        while (head < tail) {
            int col = queue[head++];
            in_queue[col] = 0;
            unsigned long long count = pending[col];
            if (count == 0) continue;
            pending[col] = 0;

            char cell = grid[row][col];
            if (cell == '^') {
                if (col > 0) {
                    pending[col - 1] += count;
                    if (!in_queue[col - 1]) {
                        queue[tail++] = col - 1;
                        in_queue[col - 1] = 1;
                    }
                }
                if (col + 1 < width) {
                    pending[col + 1] += count;
                    if (!in_queue[col + 1]) {
                        queue[tail++] = col + 1;
                        in_queue[col + 1] = 1;
                    }
                }
            } else {
                next[col] += count;
            }
        }

        int has_beam = 0;
        for (int c = 0; c < width; c++) {
            active[c] = next[c];
            if (next[c]) has_beam = 1;
        }
        if (!has_beam) break;
    }

    unsigned long long timelines = 0;
    for (int c = 0; c < width; c++) {
        timelines += active[c];
    }
    return timelines;

}

static inline long long ns_since(const struct timespec *start, const struct timespec *end) {
    return (long long)(end->tv_sec - start->tv_sec) * 1000000000LL + (end->tv_nsec - start->tv_nsec);
}

int main(void) {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("fopen");
        return EXIT_FAILURE;
    }

    char buf[LINE_BUF];
    char grid[GRID_SIZE][GRID_SIZE];
    int width = -1;
    int height = 0;

    while (fgets(buf, sizeof(buf), fp)) {
        int len = strcspn(buf, "\r\n");
        buf[len] = '\0';
        if (width < 0) width = len;
        strcpy(grid[height], buf);

        for (int c = 0; c < len; c++) {
            if (buf[c] == 'S') {
                start_row = height;
                start_col = c;
            }
        }
        height++;
    }

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    int splits = parta(grid, height, width);
    unsigned long long timelines = partb(grid, height, width);
    clock_gettime(CLOCK_MONOTONIC, &t1);

    printf("splits=%d timelines=%llu elapsed_ms=%.3f\n", splits, timelines, ns_since(&t0, &t1) / 1e6);

    if (ferror(fp)) {
        perror("read error");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fclose(fp);
    return EXIT_SUCCESS;
}
