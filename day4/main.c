#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define LINE_BUF 4096
#define GRID_SIZE 2048

static inline int parta(char grid[GRID_SIZE][GRID_SIZE], int height, int width ) {
    int moveable_rolls = 0;

    for (int i1 = 0; i1 < height; i1++) {
        for (int j2=0; j2 < width; j2++) {
            if (grid[i1][j2] != '@') continue;

            int roll = 0;
            for (int a1=-1; a1 <= 1; a1++) {
                for (int b2=-1; b2 <= 1; b2++) {
                    if (a1 == 0 && b2 == 0) continue;
                    if (i1 + a1 < 0 || i1 + a1 >= height || j2 + b2 < 0 || j2 + b2 >= width) continue;
                    if (grid[i1+a1][j2+b2] == '@') {
                        roll++;
                    }
                }
            }
        //    printf("%d rolls at %d, %d\n", roll, i1, j2);
            if (roll < 4) {
                moveable_rolls++;
            }
        }
    }

    // printf("Best: %s, %d\n", buf, best);
    return moveable_rolls;
}


static inline int partb(char grid[GRID_SIZE][GRID_SIZE], int height, int width ) {
    int moveable_rolls = 0;

    for (int i1 = 0; i1 < height; i1++) {
        for (int j2=0; j2 < width; j2++) {
            if (grid[i1][j2] != '@') continue;

            int roll = 0;
            for (int a1=-1; a1 <= 1; a1++) {
                for (int b2=-1; b2 <= 1; b2++) {
                    if (a1 == 0 && b2 == 0) continue;
                    if (i1 + a1 < 0 || i1 + a1 >= height || j2 + b2 < 0 || j2 + b2 >= width) continue;
                    if (grid[i1+a1][j2+b2] == '@') {
                        roll++;
                    }
                }
            }
            //printf("%d rolls at %d, %d\n", roll, i1, j2);
            if (roll < 4) {
                grid[i1][j2] = 'x';
                moveable_rolls++;
            }
        }
    }

    // printf("Best: %s, %d\n", buf, best);
    return moveable_rolls;
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
    int num_rolls  = 0;
    int cul_num_rolls  = 0;
    char grid[GRID_SIZE][GRID_SIZE];
    int width = -1;
    int height = 0;

    while (fgets(buf, sizeof(buf), fp)) {
        int new_width = strlen(buf);
        if (new_width != width) {
           // printf("WIDTH MISMATCH ln: %d\n", height);
            width = strlen(buf);
        }
        strcpy(grid[height++], buf);
    }

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    num_rolls = parta(grid, height, width);
    int rolls_to_remove = partb(grid, height, width);
    cul_num_rolls = rolls_to_remove;

    int cycles = 0;
    while(rolls_to_remove > 0) {
      //  printf("%d:  %d removed\n", cycles, rolls_to_remove);
        rolls_to_remove = partb(grid, height, width);
        cul_num_rolls += rolls_to_remove;
        cycles++;
    }

    clock_gettime(CLOCK_MONOTONIC, &t1);

    printf("accessible=%d removable_total=%d elapsed_ms=%.3f\n", num_rolls, cul_num_rolls, ns_since(&t0, &t1) / 1e6);

    if (ferror(fp)) {
        perror("read error");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fclose(fp);
    return EXIT_SUCCESS;
}
