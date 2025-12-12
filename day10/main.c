#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define LINE_BUF 4096
#define MAX_LINES 600
#define MAX_LIGHTS 1024
#define MAX_BUTTONS 256
#define BIT_WORDS ((MAX_BUTTONS + 63) / 64)
#define ENUM_LIMIT 20
#define EPS 1e-9

static double g_rhs[MAX_LIGHTS];
static double g_coef[MAX_LIGHTS][MAX_BUTTONS];
static int g_rank = 0;
static int g_free_count = 0;
static unsigned long long g_free_vals[MAX_BUTTONS];
static unsigned long long g_best = ~0ULL;

static inline long long ns_since(const struct timespec *start, const struct timespec *end) {
    return (long long)(end->tv_sec - start->tv_sec) * 1000000000LL + (end->tv_nsec - start->tv_nsec);
}

static inline double my_fabs(double v) {
    return (v < 0.0) ? -v : v;
}

static inline long long my_llround(double v) {
    return (long long)((v >= 0.0) ? (v + 0.5) : (v - 0.5));
}

static inline int bit_get(unsigned long long row[BIT_WORDS], int col) {
    return (row[col / 64] >> (col % 64)) & 1ULL;
}

static inline void bit_toggle(unsigned long long row[BIT_WORDS], int col) {
    row[col / 64] ^= 1ULL << (col % 64);
}

static inline void row_clear(unsigned long long row[BIT_WORDS]) {
    for (int i = 0; i < BIT_WORDS; i++) row[i] = 0;
}

static inline void row_xor(unsigned long long dst[BIT_WORDS], unsigned long long src[BIT_WORDS]) {
    for (int i = 0; i < BIT_WORDS; i++) dst[i] ^= src[i];
}

static inline int row_zero(unsigned long long row[BIT_WORDS]) {
    for (int i = 0; i < BIT_WORDS; i++) {
        if (row[i]) return 0;
    }
    return 1;
}

// Gaussian elimination to reduced row echelon form.
static inline int rref(int lights, int buttons, unsigned long long matrix[MAX_LIGHTS][BIT_WORDS], char target[MAX_LIGHTS], int pivot_cols[MAX_LIGHTS]) {
    for (int i = 0; i < lights; i++) pivot_cols[i] = -1;
    int row = 0;
    for (int col = 0; col < buttons && row < lights; col++) {
        int pivot = -1;
        for (int r = row; r < lights; r++) {
            if (bit_get(matrix[r], col)) {
                pivot = r;
                break;
            }
        }
        if (pivot == -1) continue;
        if (pivot != row) {
            unsigned long long tmp[BIT_WORDS];
            memcpy(tmp, matrix[row], sizeof(tmp));
            memcpy(matrix[row], matrix[pivot], sizeof(tmp));
            memcpy(matrix[pivot], tmp, sizeof(tmp));
            char tt = target[row];
            target[row] = target[pivot];
            target[pivot] = tt;
        }
        pivot_cols[row] = col;
        for (int r = row + 1; r < lights; r++) {
            if (bit_get(matrix[r], col)) {
                row_xor(matrix[r], matrix[row]);
                target[r] ^= target[row];
            }
        }
        row++;
    }
    int rank = row;
    for (int i = rank - 1; i >= 0; i--) {
        int col = pivot_cols[i];
        if (col < 0) continue;
        for (int r = 0; r < i; r++) {
            if (bit_get(matrix[r], col)) {
                row_xor(matrix[r], matrix[i]);
                target[r] ^= target[i];
            }
        }
    }
    for (int r = rank; r < lights; r++) {
        if (!row_zero(matrix[r]) && target[r]) {
            return -1; // inconsistent
        }
    }
    return rank;
}

static inline unsigned long long solve_min_presses(int buttons, unsigned long long matrix[MAX_LIGHTS][BIT_WORDS], char target[MAX_LIGHTS], int rank, int pivot_cols[MAX_LIGHTS]) {
    int free_idx[MAX_BUTTONS];
    int free_count = 0;
    for (int col = 0; col < buttons; col++) {
        int is_pivot = 0;
        for (int r = 0; r < rank; r++) {
            if (pivot_cols[r] == col) {
                is_pivot = 1;
                break;
            }
        }
        if (!is_pivot) {
            free_idx[free_count++] = col;
        }
    }

    unsigned long long best = (unsigned long long)buttons + 1;
    unsigned char sol[MAX_BUTTONS];

    if (free_count > ENUM_LIMIT) {
        for (int i = 0; i < buttons; i++) sol[i] = 0;
        for (int i = rank - 1; i >= 0; i--) {
            int col = pivot_cols[i];
            int val = target[i];
            for (int w = 0; w < BIT_WORDS; w++) {
                unsigned long long bits = matrix[i][w];
                while (bits) {
                    int b = __builtin_ctzll(bits);
                    int c = w * 64 + b;
                    if (c != col && c < buttons) val ^= sol[c];
                    bits &= bits - 1;
                }
            }
            sol[col] = (unsigned char)val;
        }
        unsigned long long weight = 0;
        for (int i = 0; i < buttons; i++) weight += sol[i];
        return weight;
    }

    unsigned int combos = 1u << free_count;
    for (unsigned int mask = 0; mask < combos; mask++) {
        unsigned long long weight = 0;
        for (int i = 0; i < buttons; i++) sol[i] = 0;
        for (int k = 0; k < free_count; k++) {
            if (mask & (1u << k)) {
                sol[free_idx[k]] = 1;
                weight++;
            }
        }
        for (int i = rank - 1; i >= 0; i--) {
            int col = pivot_cols[i];
            int val = target[i];
            for (int w = 0; w < BIT_WORDS; w++) {
                unsigned long long bits = matrix[i][w];
                while (bits) {
                    int b = __builtin_ctzll(bits);
                    int c = w * 64 + b;
                    if (c != col && c < buttons) val ^= sol[c];
                    bits &= bits - 1;
                }
            }
            sol[col] = (unsigned char)val;
            weight += val;
            if (weight >= best) break;
        }
        if (weight < best) best = weight;
    }
    return best;
}

static void quick_search(int fidx, unsigned long long current_sum, unsigned long long cap) {
    if (fidx == g_free_count) {
        unsigned long long total_press = current_sum;
        for (int r = 0; r < g_rank; r++) {
            double v = g_rhs[r];
            for (int f = 0; f < g_free_count; f++) v -= g_coef[r][f] * (double)g_free_vals[f];
            if (v < -EPS) return;
            long long iv = my_llround(v);
            if (my_fabs(v - (double)iv) > EPS) return;
            total_press += (unsigned long long)iv;
        }
        if (total_press < g_best) g_best = total_press;
        return;
    }
    for (unsigned long long v = 0; v <= cap; v++) {
        if (current_sum + v >= g_best) break;
        g_free_vals[fidx] = v;
        quick_search(fidx + 1, current_sum + v, cap);
    }
}

static void dfs_full(int fidx, unsigned long long current_sum) {
    if (current_sum >= g_best) return;
    if (fidx == g_free_count) {
        unsigned long long total_press = current_sum;
        for (int r = 0; r < g_rank; r++) {
            double v = g_rhs[r];
            for (int f = 0; f < g_free_count; f++) v -= g_coef[r][f] * (double)g_free_vals[f];
            if (v < -EPS) return;
            long long iv = my_llround(v);
            if (my_fabs(v - (double)iv) > EPS) return;
            total_press += (unsigned long long)iv;
            if (total_press >= g_best) return;
        }
        if (total_press < g_best) g_best = total_press;
        return;
    }
    unsigned long long maxv = (g_best > current_sum) ? (g_best - current_sum) : 0;
    for (unsigned long long v = 0; v <= maxv; v++) {
        g_free_vals[fidx] = v;
        dfs_full(fidx + 1, current_sum + v);
    }
}

static inline unsigned long long parta(char lines[MAX_LINES][LINE_BUF], int line_count) {
    unsigned long long total = 0;
    for (int idx = 0; idx < line_count; idx++) {
        char *line = lines[idx];
        char *lb = strchr(line, '[');
        char *rb = strchr(line, ']');
        if (!lb || !rb || rb <= lb + 1) continue;

        int lights = rb - lb - 1;
        if (lights > MAX_LIGHTS) continue;

        char target[MAX_LIGHTS];
        for (int i = 0; i < lights; i++) {
            target[i] = (line[lb - line + 1 + i] == '#') ? 1 : 0;
        }

        unsigned long long matrix[MAX_LIGHTS][BIT_WORDS];
        for (int r = 0; r < lights; r++) row_clear(matrix[r]);

        int buttons = 0;
        char *p = rb;
        while ((p = strchr(p, '(')) != NULL) {
            char *close = strchr(p, ')');
            if (!close) break;
            if (buttons >= MAX_BUTTONS) break;
            int cur = buttons++;
            char tmp[LINE_BUF];
            int len = close - p - 1;
            if (len >= LINE_BUF) len = LINE_BUF - 1;
            memcpy(tmp, p + 1, len);
            tmp[len] = '\0';
            char *tok = strtok(tmp, ",");
            while (tok) {
                while (*tok == ' ') tok++;
                if (*tok) {
                    int pos = atoi(tok);
                    if (pos >= 0 && pos < lights) {
                        bit_toggle(matrix[pos], cur);
                    }
                }
                tok = strtok(NULL, ",");
            }
            p = close + 1;
        }

        int pivot_cols[MAX_LIGHTS];
        int rank = rref(lights, buttons, matrix, target, pivot_cols);
        if (rank == -1) continue;
        unsigned long long presses = solve_min_presses(buttons, matrix, target, rank, pivot_cols);
        total += presses;
    }
    return total;
}

static inline unsigned long long partb(char lines[MAX_LINES][LINE_BUF], int line_count) {
    unsigned long long total = 0;

    for (int idx = 0; idx < line_count; idx++) {
        char *line = lines[idx];

        char *lb = strchr(line, '{');
        char *rb = strchr(line, '}');
        if (!lb || !rb || rb <= lb + 1) continue;

        int targets[MAX_LIGHTS];
        int counters = 0;
        char tmp[LINE_BUF];
        int len = rb - lb - 1;
        if (len >= LINE_BUF) len = LINE_BUF - 1;
        memcpy(tmp, lb + 1, len);
        tmp[len] = '\0';

        char *tok = strtok(tmp, ",");
        while (tok && counters < MAX_LIGHTS) {
            while (*tok == ' ') tok++;
            targets[counters++] = atoi(tok);
            tok = strtok(NULL, ",");
        }
        if (counters == 0) continue;

        int buttons = 0;
        double matrix[counters][MAX_BUTTONS];
        for (int r = 0; r < counters; r++) {
            for (int c = 0; c < MAX_BUTTONS; c++) matrix[r][c] = 0.0;
        }

        char *p = line;
        while ((p = strchr(p, '(')) != NULL) {
            char *close = strchr(p, ')');
            if (!close) break;
            if (buttons >= MAX_BUTTONS) break;
            int cur = buttons++;
            char btmp[LINE_BUF];
            int blen = close - p - 1;
            if (blen >= LINE_BUF) blen = LINE_BUF - 1;
            memcpy(btmp, p + 1, blen);
            btmp[blen] = '\0';
            char *btok = strtok(btmp, ",");
            while (btok) {
                while (*btok == ' ') btok++;
                if (*btok) {
                    int pos = atoi(btok);
                    if (pos >= 0 && pos < counters) {
                        matrix[pos][cur] = 1.0;
                    }
                }
                btok = strtok(NULL, ",");
            }
            p = close + 1;
        }

        if (buttons == 0) continue;

        // Build augmented matrix and reduce to RREF to find free variables.
        double aug[counters][MAX_BUTTONS + 1];
        for (int r = 0; r < counters; r++) {
            for (int c = 0; c < buttons; c++) aug[r][c] = matrix[r][c];
            aug[r][buttons] = targets[r];
        }

        int pivot_cols[MAX_LIGHTS];
        for (int i = 0; i < counters; i++) pivot_cols[i] = -1;
        int row = 0;
        for (int col = 0; col < buttons && row < counters; col++) {
            int pivot = -1;
            double best = 0.0;
            for (int r = row; r < counters; r++) {
                double val = my_fabs(aug[r][col]);
                if (val > EPS && val > best) {
                    best = val;
                    pivot = r;
                }
            }
            if (pivot == -1) continue;
            if (pivot != row) {
                double swap_row[MAX_BUTTONS + 1];
                memcpy(swap_row, aug[row], sizeof(double) * (buttons + 1));
                memcpy(aug[row], aug[pivot], sizeof(double) * (buttons + 1));
                memcpy(aug[pivot], swap_row, sizeof(double) * (buttons + 1));
            }
            double piv = aug[row][col];
            for (int c = col; c <= buttons; c++) aug[row][c] /= piv;
            for (int r = 0; r < counters; r++) {
                if (r == row) continue;
                double factor = aug[r][col];
                if (factor > -EPS && factor < EPS) continue;
                for (int c = col; c <= buttons; c++) {
                    aug[r][c] -= factor * aug[row][c];
                }
            }
            pivot_cols[row] = col;
            row++;
        }

        int rank = row;
        int inconsistent = 0;
        for (int r = rank; r < counters; r++) {
            double rhs = aug[r][buttons];
            double maxv = 0.0;
            for (int c = 0; c < buttons; c++) {
                double v = my_fabs(aug[r][c]);
                if (v > maxv) maxv = v;
            }
            if (maxv < EPS && my_fabs(rhs) > EPS) {
                inconsistent = 1;
                break;
            }
        }
        if (inconsistent) continue;

        int free_cols[MAX_BUTTONS];
        int free_count = 0;
        int used[MAX_BUTTONS];
        for (int c = 0; c < buttons; c++) used[c] = 0;
        for (int r = 0; r < rank; r++) {
            if (pivot_cols[r] >= 0) used[pivot_cols[r]] = 1;
        }
        for (int c = 0; c < buttons; c++) {
            if (!used[c]) free_cols[free_count++] = c;
        }

        for (int r = 0; r < rank; r++) {
            g_rhs[r] = aug[r][buttons];
            for (int f = 0; f < free_count; f++) {
                g_coef[r][f] = aug[r][free_cols[f]];
            }
        }

        g_rank = rank;
        g_free_count = free_count;
        g_best = ~0ULL;
        unsigned long long init_cap = 400;

        quick_search(0, 0, init_cap);
        if (g_best == ~0ULL) g_best = (unsigned long long)-1;
        dfs_full(0, 0);
        if (g_best != ~0ULL && g_best != (unsigned long long)-1) {
            total += g_best;
        }
    }

    return total;
}

int main(void) {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("fopen");
        return EXIT_FAILURE;
    }

    char lines[MAX_LINES][LINE_BUF];
    int line_count = 0;

    while (line_count < MAX_LINES && fgets(lines[line_count], sizeof(lines[line_count]), fp)) {
        size_t len = strlen(lines[line_count]);
        while (len > 0 && (lines[line_count][len - 1] == '\n' || lines[line_count][len - 1] == '\r')) {
            lines[line_count][len - 1] = '\0';
            len--;
        }
        line_count++;
    }

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    unsigned long long result_a = parta(lines, line_count);
    unsigned long long result_b = partb(lines, line_count);
    clock_gettime(CLOCK_MONOTONIC, &t1);

    printf("Fewest presses (part 1): %llu\n", result_a);
    printf("Part 2: %llu elapsed_ms=%.3f\n", result_b, ns_since(&t0, &t1) / 1e6);

    if (ferror(fp)) {
        perror("read error");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fclose(fp);
    return EXIT_SUCCESS;
}
