#include <stdio.h>

extern int node_count;
extern int node_names[];
extern int adj_ptr[];
extern int adj_list[];
extern int edge_count;

void debug_print() {
    printf("node_count=%d edge_count=%d\n", node_count, edge_count);
    for (int i = 0; i < node_count && i < 10; i++) {
        int name = node_names[i];
        printf("Node %d: %c%c%c adj[%d:%d]\n", i,
            name & 0xff, (name >> 8) & 0xff, (name >> 16) & 0xff,
            adj_ptr[i], adj_ptr[i+1]);
    }
}
