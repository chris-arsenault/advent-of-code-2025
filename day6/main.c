#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdbool.h>

#define LINE_BUF 4096
#define MAX_HEIGHT 10

static inline unsigned long long parta(int nums[LINE_BUF][MAX_HEIGHT], char ops[LINE_BUF], int height, int problem ) {
    unsigned long long accum = 0;
    if (ops[problem] == '*') accum = 1;

    for (int h = 0; h < height; h++) {
        if (ops[problem] == '+') {
            //printf("add %d for %llu\n", nums[problem][h], accum);
            accum += nums[problem][h];
        } else if (ops[problem] == '*') {
            //printf("mult %d for %llu\n", nums[problem][h], accum);
            accum *= nums[problem][h];
        } else {
            printf("Unrecognized op: %c\n", ops[problem]);
        }
    }

    return accum;
}

static inline int partb(int nums[LINE_BUF][MAX_HEIGHT] ) {

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
    bool ending = false;
    int width = -1;
    int height = 0;
    int nums[LINE_BUF][MAX_HEIGHT];
    char ops[LINE_BUF];
    const char *sep = " \t";

    while (fgets(buf, sizeof(buf), fp)) {
        width = 0;
        char *tok = strtok(buf, sep);
        while(tok != NULL) {
            if (tok[0] == '+' || tok[0] == '*') {
                ops[width] = tok[0];
                ending = true;
            } else {
                nums[width][height] = atoi(tok);
            }
            width++;
            tok = strtok(NULL, sep);
        }
        if (ending) break;
        height++;
    }
    printf("loaded %d problems with %d digits\n", width, height);

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    unsigned long long a_accum = 0;
    for (int problem = 0; problem < width; problem++) {
        a_accum += parta(nums, ops, height, problem);
        //printf("ACCU=%llu at problem %d\n", a_accum, problem);
       // break;
    }


    clock_gettime(CLOCK_MONOTONIC, &t1);

    printf("grand_total=%llu quantum_total=%llu elapsed_ms=%.3f\n", a_accum, a_accum, ns_since(&t0, &t1) / 1e6);

    if (ferror(fp)) {
        perror("read error");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fclose(fp);
    return EXIT_SUCCESS;
}
