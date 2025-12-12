#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>

typedef struct {
    long long lo;
    long long hi;
} Interval;

static inline long long ns_since(const struct timespec *start, const struct timespec *end) {
    return (long long)(end->tv_sec - start->tv_sec) * 1000000000LL + (end->tv_nsec - start->tv_nsec);
}

static int cmp_interval(const void *a, const void *b) {
    const Interval *ia = (const Interval *)a;
    const Interval *ib = (const Interval *)b;
    if (ia->lo < ib->lo) return -1;
    if (ia->lo > ib->lo) return 1;
    if (ia->hi < ib->hi) return -1;
    if (ia->hi > ib->hi) return 1;
    return 0;
}

static size_t merge_intervals(Interval *intervals, size_t n) {
    if (n == 0) return 0;
    qsort(intervals, n, sizeof(Interval), cmp_interval);
    size_t w = 0;
    for (size_t i = 1; i < n; i++) {
        if (intervals[i].lo <= intervals[w].hi + 1) {
            if (intervals[i].hi > intervals[w].hi) {
                intervals[w].hi = intervals[i].hi;
            }
        } else {
            intervals[++w] = intervals[i];
        }
    }
    return w + 1;
}

static bool contains(const Interval *intervals, size_t n, long long value) {
    size_t lo = 0, hi = n;
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        if (value < intervals[mid].lo) {
            hi = mid;
        } else if (value > intervals[mid].hi) {
            lo = mid + 1;
        } else {
            return true;
        }
    }
    return false;
}

int main(int argc, char **argv) {
    const char *path = (argc > 1) ? argv[1] : "input.txt";
    FILE *fp = fopen(path, "r");
    if (!fp) {
        perror("fopen");
        return EXIT_FAILURE;
    }

    Interval *ranges = NULL;
    size_t ranges_len = 0, ranges_cap = 0;
    long long *ids = NULL;
    size_t ids_len = 0, ids_cap = 0;

    char line[256];
    bool reading_ids = false;

    while (fgets(line, sizeof(line), fp)) {
        size_t len = strcspn(line, "\r\n");
        line[len] = '\0';
        if (len == 0) {
            reading_ids = true;
            continue;
        }

        if (!reading_ids) {
            long long a = 0, b = 0;
            if (sscanf(line, "%lld-%lld", &a, &b) != 2) {
                fprintf(stderr, "Invalid range line: %s\n", line);
                fclose(fp);
                free(ranges);
                free(ids);
                return EXIT_FAILURE;
            }
            if (a > b) {
                long long tmp = a; a = b; b = tmp;
            }
            if (ranges_len == ranges_cap) {
                ranges_cap = ranges_cap ? ranges_cap * 2 : 64;
                ranges = realloc(ranges, ranges_cap * sizeof(Interval));
                if (!ranges) {
                    perror("realloc");
                    fclose(fp);
                    free(ids);
                    return EXIT_FAILURE;
                }
            }
            ranges[ranges_len++] = (Interval){a, b};
        } else {
            long long id = 0;
            if (sscanf(line, "%lld", &id) != 1) {
                fprintf(stderr, "Invalid id line: %s\n", line);
                fclose(fp);
                free(ranges);
                free(ids);
                return EXIT_FAILURE;
            }
            if (ids_len == ids_cap) {
                ids_cap = ids_cap ? ids_cap * 2 : 128;
                ids = realloc(ids, ids_cap * sizeof(long long));
                if (!ids) {
                    perror("realloc");
                    fclose(fp);
                    free(ranges);
                    return EXIT_FAILURE;
                }
            }
            ids[ids_len++] = id;
        }
    }

    if (ferror(fp)) {
        perror("read error");
        fclose(fp);
        free(ranges);
        free(ids);
        return EXIT_FAILURE;
    }
    fclose(fp);

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    size_t merged_len = merge_intervals(ranges, ranges_len);

    long long fresh_count = 0;
    for (size_t i = 0; i < ids_len; i++) {
        if (contains(ranges, merged_len, ids[i])) {
            fresh_count++;
        }
    }

    // Part 2: total fresh IDs covered by merged ranges
    long long fresh_total = 0;
    for (size_t i = 0; i < merged_len; i++) {
        fresh_total += (ranges[i].hi - ranges[i].lo + 1);
    }

    clock_gettime(CLOCK_MONOTONIC, &t1);

    printf("available_fresh=%lld total_fresh_ids=%lld elapsed_ms=%.3f\n", fresh_count, fresh_total, ns_since(&t0, &t1) / 1e6);

    free(ranges);
    free(ids);
    return EXIT_SUCCESS;
}
