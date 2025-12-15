#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define LINE_BUF 4096

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

    char buf[LINE_BUF];
	int at = 50;
	int zero_count = 0;
	int crossings = 0;

    while (fgets(buf, sizeof(buf), fp)) {
		int sign = (buf[0]-80 > 0) - (buf[0]-80 < 0);
		int mag = atoi(buf + 1);

 		int first = at;

    	if (sign == +1) {
        	first = 100 - at;
    	}
    	if (first == 0) first = 100;

    	int hits = 0;
    	if (mag >= first) {
        	hits = 1 + (mag - first) / 100;
    	}
		crossings += hits;

		int sum = at +  sign * mag;
		at = (sum % 100 + 100) % 100;
		if (at == 0) {
			zero_count++;
		}
    }

    clock_gettime(CLOCK_MONOTONIC, &t1);
    double elapsed_ms = ns_since(&t0, &t1) / 1e6;

    printf("zero_landings=%d crossings=%d final_pos=%d elapsed_ms=%.3f\n", zero_count, crossings, at, elapsed_ms);

    if (ferror(fp)) {
        perror("read error");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fclose(fp);
    return EXIT_SUCCESS;
}
