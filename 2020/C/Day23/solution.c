#include <stdio.h>
#include <stdlib.h>

#define INITIAL_INPUT_LEN 9
#define MAX_ID            1000000
#define ROUNDS            10000000


typedef struct _node {
    int val;
    struct _node *next;
    struct _node *prev;
} Node;


Node *create_node(int v, Node *prev) {
    Node *n = (Node *)malloc(sizeof(struct _node));
    n->val = v;
    n->prev = prev;
    n->next = NULL;

    return n;
}


void play_game(Node *head, Node *all_nodes[], int rounds) {
    Node *curr = head;

    for(int i = 0; i < rounds; i++) {
        // Pick up the next three nodes
        Node *tmp1 = curr->next;
        Node *tmp3 = tmp1->next->next;
        Node *end = tmp3->next;
        
        curr->next = end;
        end->prev = curr;

        int tmp_vals[3] = {
            tmp1->val,
            tmp1->next->val,
            tmp1->next->next->val,
        };

        // Pick up the destination
        int dest_id = curr->val - 1;
        if(dest_id == 0) dest_id = MAX_ID;

        while(tmp_vals[0] == dest_id ||
              tmp_vals[1] == dest_id ||
              tmp_vals[2] == dest_id) {
            dest_id -= 1;
            if(dest_id == 0) dest_id = MAX_ID;
        }

        // Adjust the next and prev pointers, so that we have
        // the following configuration at the end:
        // dest_node -> tmp1 -> ... -> tmp3 -> (old dest_node next)
        Node *dest_node = all_nodes[dest_id-1];
        tmp3->next = dest_node->next;
        tmp1->prev = dest_node;
        tmp3->next->prev = tmp3;
        dest_node->next = tmp1;

        curr = all_nodes[curr->val-1]->next;
    }

    long int a = all_nodes[0]->next->val;
    long int b = all_nodes[0]->next->next->val;

    printf("%ld * %ld = %ld\n", a, b, a*b);
}

int main(int argc, char **argv) {
    int initial_input[] = { 2, 4, 7, 8, 1, 9, 3, 5, 6 };   

    Node *all_nodes[MAX_ID];
    Node *head = NULL;
    Node *tail = NULL;
    Node *prev = NULL;

    for(int i = 0; i < MAX_ID; i++) {
        int val;

        if(i < INITIAL_INPUT_LEN) val = initial_input[i];
        else val = i+1;

        Node *node = create_node(val, prev);
        all_nodes[val-1] = node;

        if(prev) prev->next = node;

        prev = node;
        tail = node;

        if(!head) head = node;
    }

    head->prev = tail;
    tail->next = head;

    play_game(head, all_nodes, ROUNDS);

    return 0;
}
