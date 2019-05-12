#ifndef GAL_H
#define GAL_H

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>


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

#endif