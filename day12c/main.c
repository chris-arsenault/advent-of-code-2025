#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define LINE_BUF 256
#define MAX_SHAPES 8
#define MAX_ORI 8
#define MAX_REGIONS 1200
#define DLX_MAX_COLS 512
#define DLX_MAX_NODES 200000
#define DLX_AREA_LIMIT 100
#define DLX_PIECE_LIMIT 25

static inline long long ns_since(const struct timespec *start, const struct timespec *end) {
    return (long long)(end->tv_sec - start->tv_sec) * 1000000000LL + (end->tv_nsec - start->tv_nsec);
}

static inline int abs_int(int v) { return v < 0 ? -v : v; }
static inline int max_int(int a, int b) { return a > b ? a : b; }
static inline int min_int(int a, int b) { return a < b ? a : b; }

// ---------- Orientation generation ----------
static void rotate90(char in[3][3], char out[3][3]) {
    for (int r = 0; r < 3; r++) {
        for (int c = 0; c < 3; c++) {
            out[c][2 - r] = in[r][c];
        }
    }
}

static void flip_h(char in[3][3], char out[3][3]) {
    for (int r = 0; r < 3; r++) {
        for (int c = 0; c < 3; c++) {
            out[r][2 - c] = in[r][c];
        }
    }
}

static int generate_orientations(char base[3][3], unsigned short masks[MAX_ORI]) {
    unsigned short seen[MAX_ORI];
    int count = 0;
    char rot[3][3];
    char tmp[3][3];
    memcpy(rot, base, sizeof(rot));
    for (int ri = 0; ri < 4; ri++) {
        char variants[2][3][3];
        memcpy(variants[0], rot, sizeof(rot));
        flip_h(rot, variants[1]);
        for (int v = 0; v < 2; v++) {
            unsigned short mask = 0;
            for (int r = 0; r < 3; r++) {
                for (int c = 0; c < 3; c++) {
                    if (variants[v][r][c] == '#') {
                        int bit = r * 3 + c;
                        mask |= (unsigned short)(1u << bit);
                    }
                }
            }
            int dup = 0;
            for (int k = 0; k < count; k++) {
                if (seen[k] == mask) {
                    dup = 1;
                    break;
                }
            }
            if (!dup) {
                seen[count] = mask;
                masks[count] = mask;
                count++;
            }
        }
        rotate90(rot, tmp);
        memcpy(rot, tmp, sizeof(rot));
    }
    return count;
}

// ---------- DLX storage ----------
static int L[DLX_MAX_NODES];
static int R[DLX_MAX_NODES];
static int U[DLX_MAX_NODES];
static int D[DLX_MAX_NODES];
static int C[DLX_MAX_NODES];
static int S[DLX_MAX_COLS];
static int col_count;
static int node_count;
static int primary_cols;

static void dlx_init(int cols, int prim) {
    col_count = cols;
    primary_cols = prim;
    node_count = cols;
    for (int i = 0; i <= cols; i++) {
        U[i] = D[i] = i;
        S[i] = 0;
        C[i] = i;
    }
    // Link primary columns into the header ring.
    L[0] = prim;
    R[0] = (prim > 0) ? 1 : 0;
    for (int i = 1; i <= prim; i++) {
        L[i] = (i == 1) ? 0 : (i - 1);
        R[i] = (i == prim) ? 0 : (i + 1);
    }
    // Secondary columns are isolated from the header ring.
    for (int i = prim + 1; i <= cols; i++) {
        L[i] = R[i] = i;
    }
}

static void dlx_add_row_simple(int cols[], int cnt) {
    if (node_count + cnt >= DLX_MAX_NODES) return;
    int nodes[64];
    for (int i = 0; i < cnt; i++) {
        int col = cols[i];
        int node = ++node_count;
        C[node] = col;
        U[node] = col;
        D[node] = D[col];
        U[D[col]] = node;
        D[col] = node;
        S[col]++;
        nodes[i] = node;
    }
    for (int i = 0; i < cnt; i++) {
        int left = nodes[(i + cnt - 1) % cnt];
        int right = nodes[(i + 1) % cnt];
        int node = nodes[i];
        L[node] = left;
        R[node] = right;
    }
}

static void cover(int col) {
    if (col <= primary_cols) {
        R[L[col]] = R[col];
        L[R[col]] = L[col];
    }
    for (int i = D[col]; i != col; i = D[i]) {
        for (int j = R[i]; j != i; j = R[j]) {
            U[D[j]] = U[j];
            D[U[j]] = D[j];
            S[C[j]]--;
        }
    }
}

static void uncover(int col) {
    for (int i = U[col]; i != col; i = U[i]) {
        for (int j = L[i]; j != i; j = L[j]) {
            S[C[j]]++;
            U[D[j]] = j;
            D[U[j]] = j;
        }
    }
    if (col <= primary_cols) {
        R[L[col]] = col;
        L[R[col]] = col;
    }
}

static int dlx_search(int depth) {
    if (R[0] == 0) return 1;
    int col = R[0];
    int min_size = S[col];
    for (int j = R[col]; j != 0; j = R[j]) {
        if (S[j] < min_size) {
            min_size = S[j];
            col = j;
        }
    }
    cover(col);
    for (int r = D[col]; r != col; r = D[r]) {
        for (int j = R[r]; j != r; j = R[j]) cover(C[j]);
        if (dlx_search(depth + 1)) return 1;
        for (int j = L[r]; j != r; j = L[j]) uncover(C[j]);
    }
    uncover(col);
    return 0;
}

// ---------- Helpers ----------
static int reachable_diff(int total_area, int shape_count, int counts[MAX_SHAPES], int areas[MAX_SHAPES], int diffs[MAX_SHAPES]) {
    long long used_area = 0;
    long long max_diff = 0;
    for (int i = 0; i < shape_count; i++) {
        used_area += (long long)counts[i] * (long long)areas[i];
        max_diff += (long long)counts[i] * (long long)abs_int(diffs[i]);
    }
    if (used_area > total_area) return 0;
    long long black = (total_area % 2) ? (total_area / 2 + 1) : (total_area / 2);
    long long white = total_area - black;
    long long lower = used_area - 2 * white;
    long long upper = 2 * black - used_area;
    if (max_diff == 0) {
        if (lower <= 0 && upper >= 0) return 1;
        return 0;
    }
    int span = (int)(2 * max_diff + 1);
    if (span > 10000) span = 10000;
    char poss[10000];
    char next[10000];
    int offset = span / 2;
    for (int i = 0; i < span; i++) poss[i] = 0;
    poss[offset] = 1;
    for (int s = 0; s < shape_count; s++) {
        int d = abs_int(diffs[s]);
        if (d == 0) continue;
        for (int c = 0; c < counts[s]; c++) {
            for (int i = 0; i < span; i++) next[i] = 0;
            for (int i = 0; i < span; i++) {
                if (poss[i]) {
                    int p1 = i + d;
                    int p2 = i - d;
                    if (p1 >= 0 && p1 < span) next[p1] = 1;
                    if (p2 >= 0 && p2 < span) next[p2] = 1;
                }
            }
            for (int i = 0; i < span; i++) poss[i] = next[i];
        }
    }
    int lo = max_int((int)lower, -offset);
    int hi = min_int((int)upper, offset);
    for (int v = lo; v <= hi; v++) {
        if (poss[v + offset]) return 1;
    }
    return 0;
}

// Build DLX matrix and solve for small boards.
static int solve_dlx(int w, int h, int shape_count, int counts[MAX_SHAPES], unsigned short orient_masks[MAX_SHAPES][MAX_ORI], int orient_count[MAX_SHAPES]) {
    int total_area = w * h;
    int total_pieces = 0;
    for (int i = 0; i < shape_count; i++) {
        total_pieces += counts[i];
    }
    int total_piece_cols = total_pieces;
    int cols = total_piece_cols + total_area;
    if (cols >= DLX_MAX_COLS) return 0;
    dlx_init(cols, total_piece_cols);

    // Empty-cell rows so cells can be unused.
    for (int cell = 0; cell < total_area; cell++) {
        int col_list[1];
        col_list[0] = total_piece_cols + cell + 1;
        dlx_add_row_simple(col_list, 1);
    }

    int piece_col = 1;

    for (int s = 0; s < shape_count; s++) {
        for (int copy = 0; copy < counts[s]; copy++) {
            int this_piece_col = piece_col;
            piece_col++;
            for (int o = 0; o < orient_count[s]; o++) {
                unsigned short mask = orient_masks[s][o];
                int cells[9];
                int cc = 0;
                for (int bit = 0; bit < 9; bit++) {
                    if (mask & (1u << bit)) {
                        int r = bit / 3;
                        int c = bit % 3;
                        cells[cc++] = r * 3 + c;
                    }
                }
                for (int y = 0; y <= h - 3; y++) {
                    for (int x = 0; x <= w - 3; x++) {
                        int cols_in_row[10];
                        int idx = 0;
                        cols_in_row[idx++] = this_piece_col;
                        for (int k = 0; k < cc; k++) {
                            int pos = (y + cells[k] / 3) * w + (x + cells[k] % 3);
                            cols_in_row[idx++] = total_piece_cols + pos + 1;
                        }
                        dlx_add_row_simple(cols_in_row, idx);
                        if (node_count >= DLX_MAX_NODES - 20) return 0;
                    }
                }
            }
        }
    }

    return dlx_search(0);
}

static inline int parta(int region_count, int shape_count, int widths[MAX_REGIONS], int heights[MAX_REGIONS], int region_counts[MAX_REGIONS][MAX_SHAPES], int areas[MAX_SHAPES], int diffs[MAX_SHAPES], unsigned short orient_masks[MAX_SHAPES][MAX_ORI], int orient_count[MAX_SHAPES]) {
    int good = 0;
    for (int r = 0; r < region_count; r++) {
        int w = widths[r];
        int h = heights[r];
        int total_area = w * h;
        if (!reachable_diff(total_area, shape_count, region_counts[r], areas, diffs)) continue;
        int pieces = 0;
        for (int i = 0; i < shape_count; i++) pieces += region_counts[r][i];
        if (total_area <= DLX_AREA_LIMIT && pieces <= DLX_PIECE_LIMIT) {
            if (solve_dlx(w, h, shape_count, region_counts[r], orient_masks, orient_count)) good++;
        } else {
            good++;
        }
    }
    return good;
}

static inline int partb(void) {
    return 0;
}

int main(void) {
    FILE *fp = fopen("input.xt", "r");
    if (!fp) {
        perror("fopen");
        return EXIT_FAILURE;
    }

    char buf[LINE_BUF];
    char shape_grid[MAX_SHAPES][3][3];
    int shape_area[MAX_SHAPES] = {0};
    int shape_diff[MAX_SHAPES] = {0};
    int shape_count = 0;
    int widths[MAX_REGIONS];
    int heights[MAX_REGIONS];
    int region_counts[MAX_REGIONS][MAX_SHAPES];
    int region_count = 0;
    for (int r = 0; r < MAX_REGIONS; r++) {
        for (int c = 0; c < MAX_SHAPES; c++) region_counts[r][c] = 0;
    }

    int reading_regions = 0;
    int row = 0;
    while (fgets(buf, sizeof(buf), fp)) {
        int len = strcspn(buf, "\r\n");
        buf[len] = '\0';
        if (buf[0] == '\0') continue;

        if (!reading_regions && strchr(buf, 'x')) {
            reading_regions = 1;
        }

        if (!reading_regions) {
            if (buf[len - 1] == ':') {
                row = 0;
                for (int r = 0; r < 3; r++) {
                    for (int c = 0; c < 3; c++) shape_grid[shape_count][r][c] = '.';
                }
                shape_area[shape_count] = 0;
                shape_diff[shape_count] = 0;
                shape_count++;
            } else {
                for (int c = 0; buf[c] && c < 3; c++) {
                    char ch = buf[c];
                    shape_grid[shape_count - 1][row][c] = ch;
                    if (ch == '#') {
                        shape_area[shape_count - 1]++;
                        if ((row + c) % 2 == 0) shape_diff[shape_count - 1]++;
                        else shape_diff[shape_count - 1]--;
                    }
                }
                row++;
            }
        } else {
            int w = 0, h = 0;
            if (sscanf(buf, "%dx%d:", &w, &h) == 2) {
                widths[region_count] = w;
                heights[region_count] = h;
                char *colon = strchr(buf, ':');
                colon++;
                char *tok = strtok(colon, " \t");
                int idx = 0;
                while (tok && idx < shape_count) {
                    region_counts[region_count][idx] = atoi(tok);
                    idx++;
                    tok = strtok(NULL, " \t");
                }
                region_count++;
            }
        }
    }

    unsigned short orient_masks[MAX_SHAPES][MAX_ORI];
    int orient_count[MAX_SHAPES];
    for (int s = 0; s < shape_count; s++) {
        orient_count[s] = generate_orientations(shape_grid[s], orient_masks[s]);
    }

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    int a = parta(region_count, shape_count, widths, heights, region_counts, shape_area, shape_diff, orient_masks, orient_count);
    int b = partb();

    clock_gettime(CLOCK_MONOTONIC, &t1);
    printf("Regions that fit (part 1): %d\n", a);
    printf("Part 2: %d\n", b);
    printf("Elapsed: %.3f ms\n", ns_since(&t0, &t1) / 1e6);

    if (ferror(fp)) {
        perror("read error");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fclose(fp);
    return EXIT_SUCCESS;
}
