#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define LINE_BUF 4096

static inline long long ns_since(const struct timespec *start, const struct timespec *end) {
    return (long long)(end->tv_sec - start->tv_sec) * 1000000000LL + (end->tv_nsec - start->tv_nsec);
}

static inline int parta(char * buf) {
    int best = -1;

    const size_t len = strlen(buf);
    for (int i1 = 0; i1 < len; i1++) {
        for (int j2=i1+1; j2 < len; j2++) {
            char test[4];
            test[0] = buf[i1];
            test[1] = buf[j2];
            test[3] = 0;
            int numeric = atoi(test);
            if (numeric > best) {
                best = numeric;
            }
        }
    }

   // printf("Best: %s, %d\n", buf, best);
    return best;
}

static inline unsigned long long partb(char * buf, int num_jolts) {
    buf[strcspn(buf, "\r\n")] = 0;
    int iters = 0;
 //   printf("Find: %d in %s\n", num_jolts, buf);

    const size_t len = strlen(buf);
    char max[len + 1];
    memset(max, 0, sizeof(max)); // get rid of that garbo

    int to_drop = len - num_jolts;
    int top = -1;

    for (int i1 = 0; i1 < len; i1++) {
      //  printf("starloop i=%d d=%c stack=%d to_drop=%d, jolts=%s\n", i1, buf[i1], top+1, to_drop, max);

        //space left in stack, new character is higher, need to filter more
        while (top >= 0 && max[top] < buf[i1] && to_drop > 0) {
      //      printf("droppin i=%d d=%c stack=%d to_drop=%d, jolts=%s\n", i1, buf[i1], top+1, to_drop, max);
            top--;
            to_drop--;
            iters++;
        }
        iters++;

     //   printf("set i=%d d=%c stack=%d to_drop=%d, jolts=%s\n", i1, buf[i1], top+1, to_drop, max);
        max[++top] = buf[i1];
    }

    max[num_jolts] = 0;
    //printf("Best: %s, %s, %d, %d\n", buf, max, iters, len);

    return strtoull(max, NULL, 10);
}

int main(void) {
    FILE *fp = fopen("input_eric.txt", "r");
    if (!fp) {
        perror("fopen");
        return EXIT_FAILURE;
    }

    char buf[LINE_BUF];
    long two_jolts = 0;
    unsigned long long twelve_jolts = 0;

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    while (fgets(buf, sizeof(buf), fp)) {
        two_jolts += parta(buf);
        twelve_jolts += partb(buf, 12);
    }

    clock_gettime(CLOCK_MONOTONIC, &t1);

    printf("max_2_digit_sum=%ld max_12_digit_sum=%llu elapsed_ms=%.3f\n", two_jolts, twelve_jolts, ns_since(&t0, &t1) / 1e6);

    if (ferror(fp)) {
        perror("read error");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fclose(fp);
    return EXIT_SUCCESS;
}
