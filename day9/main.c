#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define LINE_BUF 256
#define MAX_POINTS 200000

static inline int point_on_edge(int px, int py, int x1, int y1, int x2, int y2) {
    if (x1 == x2) {
        if (px != x1) return 0;
        if (py < (y1 < y2 ? y1 : y2) || py > (y1 > y2 ? y1 : y2)) return 0;
        return 1;
    } else if (y1 == y2) {
        if (py != y1) return 0;
        if (px < (x1 < x2 ? x1 : x2) || px > (x1 > x2 ? x1 : x2)) return 0;
        return 1;
    }
    return 0;
}

static inline int point_inside(int px, int py, int *xs, int *ys, int count) {
    int inside = 0;
    for (int i = 0, j = count - 1; i < count; j = i++) {
        int x1 = xs[j], y1 = ys[j];
        int x2 = xs[i], y2 = ys[i];
        if (point_on_edge(px, py, x1, y1, x2, y2)) return 1;
        int cond = ((y1 > py) != (y2 > py));
        if (cond) {
            long long x_intersect;
            if (y2 != y1) {
                x_intersect = (long long)(x2 - x1) * (py - y1) / (y2 - y1) + x1;
            } else {
                x_intersect = x1;
            }
            if (px < x_intersect) inside = !inside;
        }
    }
    return inside;
}

static inline int edge_crosses_interior(int xlo, int xhi, int ylo, int yhi, int x1, int y1, int x2, int y2) {
    if (x1 == x2) {
        if (x1 <= xlo || x1 >= xhi) return 0;
        int ya = y1 < y2 ? y1 : y2;
        int yb = y1 > y2 ? y1 : y2;
        int overlap_lo = ya;
        int overlap_hi = yb;
        if (overlap_hi <= ylo || overlap_lo >= yhi) return 0;
        if (overlap_lo < yhi && overlap_hi > ylo) return 1;
    } else if (y1 == y2) {
        if (y1 <= ylo || y1 >= yhi) return 0;
        int xa = x1 < x2 ? x1 : x2;
        int xb = x1 > x2 ? x1 : x2;
        int overlap_lo = xa;
        int overlap_hi = xb;
        if (overlap_hi <= xlo || overlap_lo >= xhi) return 0;
        if (overlap_lo < xhi && overlap_hi > xlo) return 1;
    }
    return 0;
}

static inline int rect_inside_polygon(int xlo, int xhi, int ylo, int yhi, int *xs, int *ys, int count) {
    if (!point_inside(xlo, ylo, xs, ys, count)) return 0;
    if (!point_inside(xlo, yhi, xs, ys, count)) return 0;
    if (!point_inside(xhi, ylo, xs, ys, count)) return 0;
    if (!point_inside(xhi, yhi, xs, ys, count)) return 0;

    for (int i = 0, j = count - 1; i < count; j = i++) {
        if (edge_crosses_interior(xlo, xhi, ylo, yhi, xs[j], ys[j], xs[i], ys[i])) {
            return 0;
        }
    }
    return 1;
}

static inline unsigned long long parta(int *xs, int *ys, int count) {
    unsigned long long best = 0;
    for (int i = 0; i < count; i++) {
        for (int j = i + 1; j < count; j++) {
            long long dx = xs[i] - xs[j];
            if (dx < 0) dx = -dx;
            long long dy = ys[i] - ys[j];
            if (dy < 0) dy = -dy;
            unsigned long long area = (unsigned long long)(dx + 1) * (unsigned long long)(dy + 1);
            if (area > best) best = area;
        }
    }
    return best;
}

static inline unsigned long long partb(int *xs, int *ys, int count) {
    unsigned long long best = 0;
    for (int i = 0; i < count; i++) {
        for (int j = i + 1; j < count; j++) {
            if (xs[i] == xs[j] || ys[i] == ys[j]) continue;
            int xlo = xs[i] < xs[j] ? xs[i] : xs[j];
            int xhi = xs[i] > xs[j] ? xs[i] : xs[j];
            int ylo = ys[i] < ys[j] ? ys[i] : ys[j];
            int yhi = ys[i] > ys[j] ? ys[i] : ys[j];

            if (!rect_inside_polygon(xlo, xhi, ylo, yhi, xs, ys, count)) continue;

            unsigned long long area = (unsigned long long)(xhi - xlo + 1) * (unsigned long long)(yhi - ylo + 1);
            if (area > best) best = area;
        }
    }
    return best;
}

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

    int xs[MAX_POINTS];
    int ys[MAX_POINTS];
    int count = 0;
    char buf[LINE_BUF];

    while (fgets(buf, sizeof(buf), fp)) {
        int x, y;
        if (sscanf(buf, "%d,%d", &x, &y) == 2) {
            xs[count] = x;
            ys[count] = y;
            count++;
        }
    }

    unsigned long long result_a = parta(xs, ys, count);
    unsigned long long result_b = partb(xs, ys, count);

    clock_gettime(CLOCK_MONOTONIC, &t1);
    double elapsed_ms = ns_since(&t0, &t1) / 1e6;

    printf("max_rect_area=%llu max_green_rect_area=%llu elapsed_ms=%.3f\n", result_a, result_b, elapsed_ms);

    if (ferror(fp)) {
        perror("read error");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fclose(fp);
    return EXIT_SUCCESS;
}
