#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MAX_SIZE 4096

char grid[MAX_SIZE][MAX_SIZE];
int neighbors[MAX_SIZE][MAX_SIZE];
int rows = 0, cols = 0;

// BFS queue
int q_row[MAX_SIZE * MAX_SIZE];
int q_col[MAX_SIZE * MAX_SIZE];
int q_head = 0, q_tail = 0;
char in_queue[MAX_SIZE][MAX_SIZE];

// 8 directions
const int dr[] = {-1, -1, -1, 0, 0, 1, 1, 1};
const int dc[] = {-1, 0, 1, -1, 1, -1, 0, 1};

void enqueue(int r, int c) {
    if (!in_queue[r][c]) {
        in_queue[r][c] = 1;
        q_row[q_tail] = r;
        q_col[q_tail] = c;
        q_tail++;
    }
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

    char buf[MAX_SIZE + 2];
    while (fgets(buf, sizeof(buf), fp)) {
        size_t len = strcspn(buf, "\r\n");
        if (len == 0) continue;
        if (cols == 0) {
            cols = (int)len;
        }
        memcpy(grid[rows], buf, len);
        rows++;
    }
    fclose(fp);

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    // Precompute neighbor counts for all rolls
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] != '@') continue;
            int count = 0;
            for (int d = 0; d < 8; d++) {
                int nr = r + dr[d], nc = c + dc[d];
                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols)
                    if (grid[nr][nc] == '@') count++;
            }
            neighbors[r][c] = count;
        }
    }

    // Part 1: Count initially accessible rolls, enqueue them for Part 2
    int part1 = 0;
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] == '@' && neighbors[r][c] < 4) {
                part1++;
                enqueue(r, c);
            }
        }
    }

    // Part 2: BFS removal - process queue, update neighbors, enqueue newly accessible
    int part2 = 0;
    while (q_head < q_tail) {
        int r = q_row[q_head], c = q_col[q_head];
        q_head++;

        if (grid[r][c] != '@') continue;  // Already removed

        // Remove this roll
        grid[r][c] = '.';
        part2++;

        // Update neighbors and enqueue newly accessible rolls
        for (int d = 0; d < 8; d++) {
            int nr = r + dr[d], nc = c + dc[d];
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                if (grid[nr][nc] == '@') {
                    neighbors[nr][nc]--;
                    if (neighbors[nr][nc] < 4) {
                        enqueue(nr, nc);
                    }
                }
            }
        }
    }

    clock_gettime(CLOCK_MONOTONIC, &t1);

    printf("accessible=%d removable_total=%d elapsed_ms=%.3f\n",
           part1, part2, ns_since(&t0, &t1) / 1e6);

    return EXIT_SUCCESS;
}
