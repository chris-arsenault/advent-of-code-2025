#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MAX_SIZE 4096

char grid[MAX_SIZE][MAX_SIZE];
int neighbors[MAX_SIZE][MAX_SIZE];
int rows = 0, cols = 0;

// Simple queue
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
        if ((int)len > MAX_SIZE) {
            fprintf(stderr, "Line too long (%zu)\n", len);
            fclose(fp);
            return EXIT_FAILURE;
        }
        if (cols == 0) {
            cols = (int)len;
        } else if ((int)len != cols) {
            fprintf(stderr, "Width mismatch on line %d\n", rows);
            fclose(fp);
            return EXIT_FAILURE;
        }
        if (rows >= MAX_SIZE) {
            fprintf(stderr, "Grid too tall (%d)\n", rows);
            fclose(fp);
            return EXIT_FAILURE;
        }
        memcpy(grid[rows], buf, len);
        rows++;
    }

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

    // Part 1: Find initially accessible, enqueue them
    int part1 = 0;
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] == '@' && neighbors[r][c] < 4) {
                part1++;
                enqueue(r, c);
            }
        }
    }
    printf("Part 1: %d\n", part1);

    // Part 2: BFS-style removal
    int part2 = 0;
    while (q_head < q_tail) {
        int r = q_row[q_head], c = q_col[q_head];
        q_head++;

        if (grid[r][c] != '@') continue;  // Already removed

        // Remove this roll
        grid[r][c] = '.';
        part2++;

        // Update neighbors and enqueue newly accessible ones
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

    printf("Part 2: %d\n", part2);
    printf("Elapsed: %.3f ms\n", ns_since(&t0, &t1) / 1e6);

    if (ferror(fp)) {
        perror("read error");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fclose(fp);
    return EXIT_SUCCESS;
}
