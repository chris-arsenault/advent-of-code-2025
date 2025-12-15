#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define LINE_BUF 256
#define MAX_POINTS 6000
#define CONNECT_COUNT 1000

static inline int find_root(int *parent, int x) {
    while (parent[x] != x) {
        parent[x] = parent[parent[x]];
        x = parent[x];
    }
    return x;
}

static inline void unite(int *parent, int *size, int a, int b) {
    int ra = find_root(parent, a);
    int rb = find_root(parent, b);
    if (ra == rb) return;
    if (size[ra] < size[rb]) {
        int tmp = ra; ra = rb; rb = tmp;
    }
    parent[rb] = ra;
    size[ra] += size[rb];
}

static inline void swap_edges(long long *dist, int *a, int *b, int i, int j) {
    long long td = dist[i]; dist[i] = dist[j]; dist[j] = td;
    int ta = a[i]; a[i] = a[j]; a[j] = ta;
    int tb = b[i]; b[i] = b[j]; b[j] = tb;
}

static void sort_edges(long long *dist, int *a, int *b, int lo, int hi) {
    if (lo >= hi) return;
    long long pivot = dist[(lo + hi) / 2];
    int i = lo;
    int j = hi;
    while (i <= j) {
        while (dist[i] < pivot) i++;
        while (dist[j] > pivot) j--;
        if (i <= j) {
            swap_edges(dist, a, b, i, j);
            i++;
            j--;
        }
    }
    if (lo < j) sort_edges(dist, a, b, lo, j);
    if (i < hi) sort_edges(dist, a, b, i, hi);
}

static inline int build_edges(int *xs, int *ys, int *zs, int count, long long *dist, int *ea, int *eb) {
    int idx = 0;
    for (int i = 0; i < count; i++) {
        for (int j = i + 1; j < count; j++) {
            long long dx = xs[i] - xs[j];
            long long dy = ys[i] - ys[j];
            long long dz = zs[i] - zs[j];
            long long d2 = dx * dx + dy * dy + dz * dz;
            dist[idx] = d2;
            ea[idx] = i;
            eb[idx] = j;
            idx++;
        }
    }
    return idx;
}

// Product of sizes of the three largest circuits after the first CONNECT_COUNT connections.
static inline unsigned long long parta(int count, int *ea, int *eb, int edge_count) {
    int parent[MAX_POINTS];
    int size[MAX_POINTS];
    for (int i = 0; i < count; i++) {
        parent[i] = i;
        size[i] = 1;
    }

    int limit = edge_count < CONNECT_COUNT ? edge_count : CONNECT_COUNT;
    for (int i = 0; i < limit; i++) {
        unite(parent, size, ea[i], eb[i]);
    }

    unsigned long long top1 = 0, top2 = 0, top3 = 0;
    for (int i = 0; i < count; i++) {
        if (parent[i] == i) {
            unsigned long long s = (unsigned long long)size[i];
            if (s > top1) {
                top3 = top2;
                top2 = top1;
                top1 = s;
            } else if (s > top2) {
                top3 = top2;
                top2 = s;
            } else if (s > top3) {
                top3 = s;
            }
        }
    }

    return top1 * top2 * top3;
}

// Product of X coordinates of the edge that finally connects all circuits.
static inline unsigned long long partb(int *xs, int count, int *ea, int *eb, int edge_count) {
    int parent[MAX_POINTS];
    int size[MAX_POINTS];
    for (int i = 0; i < count; i++) {
        parent[i] = i;
        size[i] = 1;
    }

    int components = count;
    unsigned long long last_prod = 0;
    for (int i = 0; i < edge_count; i++) {
        int a = ea[i];
        int b = eb[i];
        int ra = find_root(parent, a);
        int rb = find_root(parent, b);
        if (ra != rb) {
            unite(parent, size, ra, rb);
            components--;
            last_prod = (unsigned long long)xs[a] * (unsigned long long)xs[b];
            if (components == 1) break;
        }
    }
    return last_prod;
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
    int zs[MAX_POINTS];
    int count = 0;
    char buf[LINE_BUF];

    while (fgets(buf, sizeof(buf), fp)) {
        int x, y, z;
        if (sscanf(buf, "%d,%d,%d", &x, &y, &z) == 3) {
            xs[count] = x;
            ys[count] = y;
            zs[count] = z;
            count++;
        }
    }

    int edge_count = count * (count - 1) / 2;
    long long *dist = malloc(sizeof(long long) * edge_count);
    int *ea = malloc(sizeof(int) * edge_count);
    int *eb = malloc(sizeof(int) * edge_count);

    int built = build_edges(xs, ys, zs, count, dist, ea, eb);
    sort_edges(dist, ea, eb, 0, built - 1);

    unsigned long long result_a = parta(count, ea, eb, built);
    unsigned long long result_b = partb(xs, count, ea, eb, built);

    clock_gettime(CLOCK_MONOTONIC, &t1);
    double elapsed_ms = ns_since(&t0, &t1) / 1e6;

    printf("top3_product=%llu final_join_x_product=%llu elapsed_ms=%.3f\n", result_a, result_b, elapsed_ms);

    if (ferror(fp)) {
        perror("read error");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fclose(fp);
    return EXIT_SUCCESS;
}
