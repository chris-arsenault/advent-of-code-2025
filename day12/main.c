#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define LINE_BUF 256
#define MAX_SHAPES 8
#define MAX_REGIONS 1200
#define SEARCH_AREA_LIMIT 64
#define SEARCH_PIECE_LIMIT 10

static inline int max_int(int a, int b) { return a > b ? a : b; }
static inline int min_int(int a, int b) { return a < b ? a : b; }

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

static int generate_orientations(char base[3][3], unsigned short masks[8]) {
    unsigned short seen[8];
    int count = 0;
    char rot[3][3];
    char tmp[3][3];
    memcpy(rot, base, sizeof(rot));
    for (int rot_idx = 0; rot_idx < 4; rot_idx++) {
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

static int generate_placements(int w, int h, unsigned short mask, unsigned long long *out, int cap) {
    int count = 0;
    int cells[9];
    int cell_count = 0;
    for (int bit = 0; bit < 9; bit++) {
        if (mask & (1u << bit)) {
            int r = bit / 3;
            int c = bit % 3;
            cells[cell_count++] = r * 3 + c;
        }
    }
    for (int y = 0; y <= h - 3; y++) {
        for (int x = 0; x <= w - 3; x++) {
            unsigned long long placed = 0;
            for (int idx = 0; idx < cell_count; idx++) {
                int bit = cells[idx];
                int r = bit / 3;
                int c = bit % 3;
                int pos = (y + r) * w + (x + c);
                placed |= 1ULL << pos;
            }
            int dup = 0;
            for (int k = 0; k < count; k++) {
                if (out[k] == placed) {
                    dup = 1;
                    break;
                }
            }
            if (!dup && count < cap) {
                out[count++] = placed;
            }
        }
    }
    return count;
}

static int dfs_place(int idx, int total, int piece_order[SEARCH_PIECE_LIMIT], unsigned long long placements[MAX_SHAPES][512], int placement_count[MAX_SHAPES], unsigned long long used_mask) {
    if (idx == total) return 1;
    int shape = piece_order[idx];
    for (int i = 0; i < placement_count[shape]; i++) {
        unsigned long long pmask = placements[shape][i];
        if ((pmask & used_mask) == 0) {
            if (dfs_place(idx + 1, total, piece_order, placements, placement_count, used_mask | pmask)) return 1;
        }
    }
    return 0;
}

static int can_pack_small(int w, int h, int shape_count, int counts[MAX_SHAPES], unsigned short orient_masks[MAX_SHAPES][8], int orient_count[MAX_SHAPES]) {
    int total_pieces = 0;
    for (int i = 0; i < shape_count; i++) total_pieces += counts[i];
    if (total_pieces == 0) return 1;

    unsigned long long placements[MAX_SHAPES][512];
    int placement_count[MAX_SHAPES];
    for (int s = 0; s < shape_count; s++) {
        placement_count[s] = 0;
        for (int m = 0; m < orient_count[s]; m++) {
            placement_count[s] += generate_placements(w, h, orient_masks[s][m], placements[s] + placement_count[s], 512 - placement_count[s]);
        }
        if (counts[s] > 0 && placement_count[s] == 0) return 0;
    }

    int piece_order[SEARCH_PIECE_LIMIT];
    int remaining[MAX_SHAPES];
    for (int i = 0; i < shape_count; i++) remaining[i] = counts[i];
    for (int idx = 0; idx < total_pieces; idx++) {
        int best_shape = -1;
        int best_places = 1000000;
        for (int s = 0; s < shape_count; s++) {
            if (remaining[s] == 0) continue;
            if (placement_count[s] < best_places) {
                best_places = placement_count[s];
                best_shape = s;
            }
        }
        piece_order[idx] = best_shape;
        remaining[best_shape]--;
    }

    return dfs_place(0, total_pieces, piece_order, placements, placement_count, 0);
}

static int reachable_diff(int total_area, int shape_count, int counts[MAX_SHAPES], int areas[MAX_SHAPES], int diffs[MAX_SHAPES]) {
    long long used_area = 0;
    long long max_diff = 0;
    for (int i = 0; i < shape_count; i++) {
        used_area += (long long)counts[i] * (long long)areas[i];
        max_diff += (long long)counts[i] * (long long)(diffs[i] < 0 ? -diffs[i] : diffs[i]);
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

    int span = (int)(max_diff * 2 + 1);
    char poss[span];
    char next[span];
    for (int i = 0; i < span; i++) poss[i] = 0;
    int offset = (int)max_diff;
    poss[offset] = 1;
    for (int s = 0; s < shape_count; s++) {
        int d = diffs[s];
        if (d < 0) d = -d;
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

    int lo = max_int((int)(lower), -offset);
    int hi = min_int((int)(upper), offset);
    for (int val = lo; val <= hi; val++) {
        if (poss[val + offset]) return 1;
    }
    return 0;
}

static inline int parta(int region_count, int shape_count, int widths[MAX_REGIONS], int heights[MAX_REGIONS], int region_counts[MAX_REGIONS][MAX_SHAPES], int areas[MAX_SHAPES], int diffs[MAX_SHAPES], unsigned short orient_masks[MAX_SHAPES][8], int orient_count[MAX_SHAPES]) {
    int good = 0;
    for (int r = 0; r < region_count; r++) {
        int w = widths[r];
        int h = heights[r];
        int total_area = w * h;
        if (!reachable_diff(total_area, shape_count, region_counts[r], areas, diffs)) {
            continue;
        }
        int pieces = 0;
        for (int i = 0; i < shape_count; i++) pieces += region_counts[r][i];
        if (total_area <= SEARCH_AREA_LIMIT && pieces <= SEARCH_PIECE_LIMIT) {
            if (can_pack_small(w, h, shape_count, region_counts[r], orient_masks, orient_count)) {
                good++;
            }
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
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);

    FILE *fp = fopen("input.txt", "r");
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
                shape_area[shape_count] = 0;
                shape_diff[shape_count] = 0;
                for (int r = 0; r < 3; r++) {
                    for (int c = 0; c < 3; c++) shape_grid[shape_count][r][c] = '.';
                }
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

    unsigned short orient_masks[MAX_SHAPES][8];
    int orient_count[MAX_SHAPES];
    for (int s = 0; s < shape_count; s++) {
        orient_count[s] = generate_orientations(shape_grid[s], orient_masks[s]);
    }

    int good = parta(region_count, shape_count, widths, heights, region_counts, shape_area, shape_diff, orient_masks, orient_count);
    int b_ans = partb();
    (void)b_ans;

    clock_gettime(CLOCK_MONOTONIC, &end);
    double elapsed_ms = (end.tv_sec - start.tv_sec) * 1000.0 +
                        (end.tv_nsec - start.tv_nsec) / 1000000.0;

    printf("regions_that_fit=%d elapsed_ms=%.3f\n", good, elapsed_ms);

    if (ferror(fp)) {
        perror("read error");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fclose(fp);
    return EXIT_SUCCESS;
}
