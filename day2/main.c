#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>
#include <time.h>

#define LINE_BUF 4096

static inline long long ns_since(const struct timespec *start, const struct timespec *end) {
    return (long long)(end->tv_sec - start->tv_sec) * 1000000000LL + (end->tv_nsec - start->tv_nsec);
}


// i/10^(len(i)/2) == i % 10^(len(i)/2)
static inline bool part1(unsigned long long test_i) {
    const int num_length = (int)floor(log10((long double)test_i)) + 1;

    if (num_length % 2 == 1) return false;;

    const long h = pow(10, num_length/2);
    if (test_i / h == test_i % h) {
        return true;
    }

    return false;
}

static inline bool part2(unsigned long long test_i) {
    char buf[LINE_BUF];
    sprintf(buf, "%llu", test_i);
    const size_t str_len = strlen(buf);
    for (int substr_len = 1; substr_len < str_len; substr_len++) {
        if (str_len % substr_len != 0) continue;
        const int substr_total_repeates = str_len / substr_len;

        bool ok = true;
        for (int substr_index = 0; substr_index < substr_len; substr_index++) {
            for (int which_substring = 1; which_substring < substr_total_repeates; which_substring++) {
                char first_substr_char = buf[substr_index];
                char test_char = buf[which_substring*substr_len + substr_index];
                //printf("%s, substr_len %d, substr_index %d, which_substring %d, test_index, %d, f %c, t %c\n", buf, substr_len, substr_index, which_substring, which_substring*substr_len + substr_index, first_substr_char, test_char);
                if (first_substr_char != test_char) {
                    ok = false;
                    break;
                }
            }
        }
        if (ok) {return true;}
    }

    return false;
}

int main(void) {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("fopen");
        return EXIT_FAILURE;
    }

    char buf[LINE_BUF];
    const char *sep = ",-";

    unsigned long long part_1_total = 0;
    unsigned long long part_2_total = 0;

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    while (fgets(buf, sizeof(buf), fp)) {

        char *first = strtok(buf, sep);
        char *last = strtok(NULL, sep);

        while (last) {
            size_t first_l = strlen(first);
            size_t last_l = strlen(last);
           // printf("Searching: %s %s\n", first, last);

            const unsigned long long  first_i = strtoull(first, NULL, 10);
            const unsigned long long  last_i = strtoull(last, NULL, 10);
            for (unsigned long long test_i = first_i; test_i <= last_i; test_i++) {
                if (part1(test_i)) {
                    part_1_total += test_i;
                    //printf("Part1: %llu, %llu\n", test_i, part_1_total);
                }

                if (part2(test_i)) {
                    part_2_total += test_i;
           //         printf("Part2: %llu, %llu\n", test_i, part_2_total);
                }
            }
            //exit(0);

            first = strtok(NULL, sep);
            last = strtok(NULL, sep);
        }

    clock_gettime(CLOCK_MONOTONIC, &t1);
    double elapsed_ms = ns_since(&t0, &t1) / 1e6;

    printf("Silly Elf: %llu, %llu elapsed_ms=%.3f\n", part_1_total, part_2_total, elapsed_ms);
    }

    if (ferror(fp)) {
        perror("read error");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fclose(fp);
    return EXIT_SUCCESS;
}
