#ifndef GAL_H
#define GAL_H

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

/*
============================
 ****  Node methods  ****
 ============================
 */

struct node {
    int size;
    struct list_node * head;
};


struct node * make_node();
int node_set_int(struct node * l, int E);
int node_get_int(struct node * l);
int node_set(struct node *l, void *data);
int node_add_head_int(struct node * n, int data);
int node_remove_head_int(struct node * l);
void * node_remove_head(struct node *l);
int node_add_tail_int(struct node * l, int data);
int node_add_head(struct node *l, void *data);
int node_add_tail(struct node *l, void *data);
void * node_get(struct node *n);
int size_node(struct node *n);
void free_node(struct node *m);

/*
============================
 ****  List methods  ****
 ============================
 */

/*
 * A node in a linked list.
 */
struct list_node {
    void * data;
    //union data_type d;
    struct list_node * next;
};

/*
 * A linked list.
 * 'head' points to the first node in the list.
 *
 * This will be called generically as in
 * List<int> data;
 * List<map> data;
 */
struct list {
    int size;
    struct list_node * head;
};


/*
 * Initializes an empty list.
 */
struct list * make_list();

/*
 * Adds to the front of the list.
 * Returns a 1 if successful and 0 otherwise.
 */
int add_head(struct list *l, void *data);

/*
 * Adds to the end of the list.
 * Returns a 1 if successful and 0 otherwise.
 */
int add_tail(struct list *l, void *data);

/*
 * Returns data from the head of the list and removes it.
 */
void * remove_head(struct list *l);

/*
 * Returns data from the tail of a list and removes it.
 */
void * remove_tail(struct list *l);

/*
 * Returns the data of element at index i of the list.
 */
void * list_get(struct list *l, int i);

/*
 * Frees allocated memory for a list.
 */
void free_list(struct list *l);


/*
 * Sets the element at index i to data.
 * Will return 1 if successful (valid i)
 * and 0 otherwise.
 */
int set(struct list *l, int i, void *data);

/*
 * Returns the data of element at index i of the list.
 */
void * list_get(struct list *l, int i);

/*
 * Returns the size of a list.
 */
int size(struct list *l);

/*
 * Prints out list of elements in a list.
 */
void printl(struct list *l);

/*
 * Prints out list of elements in a list.
 */
void printil(struct list *l);

void sortedInsert(struct list_node**, struct list_node*);
void listSort2(struct list_node**);
#endif
