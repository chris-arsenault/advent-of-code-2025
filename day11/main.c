#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define LINE_BUF 256
#define MAX_NODES 1024
#define MAX_NAME 8
#define MAX_EDGES 8192

static inline long long ns_since(const struct timespec *start, const struct timespec *end) {
    return (long long)(end->tv_sec - start->tv_sec) * 1000000000LL + (end->tv_nsec - start->tv_nsec);
}

static inline int get_id(char names[MAX_NODES][MAX_NAME], int *count, const char *name) {
    for (int i = 0; i < *count; i++) {
        if (strcmp(names[i], name) == 0) return i;
    }
    int id = *count;
    strcpy(names[id], name);
    (*count)++;
    return id;
}

static unsigned long long dfs_paths(int node, int target, int head[MAX_NODES], int to[MAX_EDGES], int next[MAX_EDGES], unsigned long long memo[MAX_NODES], char done[MAX_NODES]) {
    if (done[node]) return memo[node];
    if (node == target) {
        done[node] = 1;
        memo[node] = 1;
        return 1;
    }
    unsigned long long total = 0;
    for (int e = head[node]; e != -1; e = next[e]) {
        total += dfs_paths(to[e], target, head, to, next, memo, done);
    }
    memo[node] = total;
    done[node] = 1;
    return total;
}

static inline unsigned long long count_paths(int start, int target, int head[MAX_NODES], int to[MAX_EDGES], int next[MAX_EDGES], int node_count) {
    unsigned long long memo[MAX_NODES];
    char done[MAX_NODES];
    for (int i = 0; i < node_count; i++) {
        memo[i] = 0;
        done[i] = 0;
    }
    return dfs_paths(start, target, head, to, next, memo, done);
}

static inline unsigned long long parta(int start, int target, int head[MAX_NODES], int to[MAX_EDGES], int next[MAX_EDGES], int node_count) {
    return count_paths(start, target, head, to, next, node_count);
}

static inline unsigned long long partb(int svr, int dac, int fft, int out, int head[MAX_NODES], int to[MAX_EDGES], int next[MAX_EDGES], int node_count) {
    if (svr < 0 || dac < 0 || fft < 0 || out < 0) return 0;
    unsigned long long a1 = count_paths(svr, dac, head, to, next, node_count);
    unsigned long long a2 = count_paths(dac, fft, head, to, next, node_count);
    unsigned long long a3 = count_paths(fft, out, head, to, next, node_count);
    unsigned long long b1 = count_paths(svr, fft, head, to, next, node_count);
    unsigned long long b2 = count_paths(fft, dac, head, to, next, node_count);
    unsigned long long b3 = count_paths(dac, out, head, to, next, node_count);
    return a1 * a2 * a3 + b1 * b2 * b3;
}

int main(void) {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("fopen");
        return EXIT_FAILURE;
    }

    char buf[LINE_BUF];
    char names[MAX_NODES][MAX_NAME];
    int node_count = 0;
    int head[MAX_NODES];
    int to[MAX_EDGES];
    int next[MAX_EDGES];
    int edge_count = 0;

    for (int i = 0; i < MAX_NODES; i++) head[i] = -1;

    while (fgets(buf, sizeof(buf), fp)) {
        int len = strcspn(buf, "\r\n");
        buf[len] = '\0';
        if (buf[0] == '\0') continue;

        char *tok = strtok(buf, " :\t");
        if (!tok) continue;
        int from = get_id(names, &node_count, tok);

        tok = strtok(NULL, " :\t");
        while (tok) {
            int dest = get_id(names, &node_count, tok);
            to[edge_count] = dest;
            next[edge_count] = head[from];
            head[from] = edge_count;
            edge_count++;
            tok = strtok(NULL, " :\t");
        }
    }

    int start = get_id(names, &node_count, "you");
    int target = get_id(names, &node_count, "out");
    int svr = get_id(names, &node_count, "svr");
    int dac = get_id(names, &node_count, "dac");
    int fft = get_id(names, &node_count, "fft");

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    unsigned long long paths = parta(start, target, head, to, next, node_count);
    unsigned long long paths_b = partb(svr, dac, fft, target, head, to, next, node_count);
    clock_gettime(CLOCK_MONOTONIC, &t1);

    printf("Paths to out: %llu\n", paths);
    printf("Paths svr->out via dac+fft: %llu elapsed_ms=%.3f\n", paths_b, ns_since(&t0, &t1) / 1e6);

    if (ferror(fp)) {
        perror("read error");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fclose(fp);
    return EXIT_SUCCESS;
}
