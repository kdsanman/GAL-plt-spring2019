/*
 *  A function illustrating how to link C code to code generated from LLVM 
 */

#include "GAL.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h> 

/*
 * Font information: one byte per row, 8 rows per character
 * In order, space, 0-9, A-Z
 */
static const char font[] = {
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x1c, 0x3e, 0x61, 0x41, 0x43, 0x3e, 0x1c, 0x00,
  0x00, 0x40, 0x42, 0x7f, 0x7f, 0x40, 0x40, 0x00,
  0x62, 0x73, 0x79, 0x59, 0x5d, 0x4f, 0x46, 0x00,
  0x20, 0x61, 0x49, 0x4d, 0x4f, 0x7b, 0x31, 0x00,
  0x18, 0x1c, 0x16, 0x13, 0x7f, 0x7f, 0x10, 0x00,
  0x27, 0x67, 0x45, 0x45, 0x45, 0x7d, 0x38, 0x00,
  0x3c, 0x7e, 0x4b, 0x49, 0x49, 0x79, 0x30, 0x00,
  0x03, 0x03, 0x71, 0x79, 0x0d, 0x07, 0x03, 0x00,
  0x36, 0x4f, 0x4d, 0x59, 0x59, 0x76, 0x30, 0x00,
  0x06, 0x4f, 0x49, 0x49, 0x69, 0x3f, 0x1e, 0x00,
  0x7c, 0x7e, 0x13, 0x11, 0x13, 0x7e, 0x7c, 0x00,
  0x7f, 0x7f, 0x49, 0x49, 0x49, 0x7f, 0x36, 0x00,
  0x1c, 0x3e, 0x63, 0x41, 0x41, 0x63, 0x22, 0x00,
  0x7f, 0x7f, 0x41, 0x41, 0x63, 0x3e, 0x1c, 0x00,
  0x00, 0x7f, 0x7f, 0x49, 0x49, 0x49, 0x41, 0x00,
  0x7f, 0x7f, 0x09, 0x09, 0x09, 0x09, 0x01, 0x00,
  0x1c, 0x3e, 0x63, 0x41, 0x49, 0x79, 0x79, 0x00,
  0x7f, 0x7f, 0x08, 0x08, 0x08, 0x7f, 0x7f, 0x00,
  0x00, 0x41, 0x41, 0x7f, 0x7f, 0x41, 0x41, 0x00,
  0x20, 0x60, 0x40, 0x40, 0x40, 0x7f, 0x3f, 0x00,
  0x7f, 0x7f, 0x18, 0x3c, 0x76, 0x63, 0x41, 0x00,
  0x00, 0x7f, 0x7f, 0x40, 0x40, 0x40, 0x40, 0x00,
  0x7f, 0x7f, 0x0e, 0x1c, 0x0e, 0x7f, 0x7f, 0x00,
  0x7f, 0x7f, 0x0e, 0x1c, 0x38, 0x7f, 0x7f, 0x00,
  0x3e, 0x7f, 0x41, 0x41, 0x41, 0x7f, 0x3e, 0x00,
  0x7f, 0x7f, 0x11, 0x11, 0x11, 0x1f, 0x0e, 0x00,
  0x3e, 0x7f, 0x41, 0x51, 0x71, 0x3f, 0x5e, 0x00,
  0x7f, 0x7f, 0x11, 0x31, 0x79, 0x6f, 0x4e, 0x00,
  0x26, 0x6f, 0x49, 0x49, 0x4b, 0x7a, 0x30, 0x00,
  0x00, 0x01, 0x01, 0x7f, 0x7f, 0x01, 0x01, 0x00,
  0x3f, 0x7f, 0x40, 0x40, 0x40, 0x7f, 0x3f, 0x00,
  0x0f, 0x1f, 0x38, 0x70, 0x38, 0x1f, 0x0f, 0x00,
  0x1f, 0x7f, 0x38, 0x1c, 0x38, 0x7f, 0x1f, 0x00,
  0x63, 0x77, 0x3e, 0x1c, 0x3e, 0x77, 0x63, 0x00,
  0x00, 0x03, 0x0f, 0x78, 0x78, 0x0f, 0x03, 0x00,
  0x61, 0x71, 0x79, 0x5d, 0x4f, 0x47, 0x43, 0x00
};

void printbig(int c)
{
  int index = 0;
  int col, data;
  if (c >= '0' && c <= '9') index = 8 + (c - '0') * 8;
  else if (c >= 'A' && c <= 'Z') index = 88 + (c - 'A') * 8;
  do {
    data = font[index++];
    for (col = 0 ; col < 8 ; data <<= 1, col++) {
      char d = data & 0x80 ? 'X' : ' ';
      putchar(d); putchar(d);
    }
    putchar('\n');
  } while (index & 0x7); 
}

/* String Functions */

char *string_concat(char *s1, char *s2) 
{  
    char *new = (char *) malloc(strlen(s1) + strlen(s2) + 1);
    strcpy(new, s1);
    strcat(new, s2);
    return new;
}

int str_size(char *a)
{
   return strlen(a);
}

char *string_copy(char *to, char *from)
{
  return strcpy(to,from);
}

char *tokenize(char *s1, char *delim) 
{
  return strtok(s1, delim);
}

char *find_char_string(char *source, int ch)
{
  return strchr(source, ch);
}

char *find_in_string(char *source, char *str_find)
{
  return strstr(source, str_find);
}


/*
 * NODE METHODS
 */

/*
 * Initializes an empty node.
 */
struct node * make_node() {

  struct node *n;
  n = malloc(sizeof(struct node));
  if (n == NULL)
    return NULL;

  n->size = 0;
  n->head = 0;
  return n;
}

int node_set_int(struct node * l, int E)
{
    int * d = malloc(sizeof(int));
    *d = E;
    node_set(l, (void *) d);
    return 0;
}

int node_get_int(struct node * l)
{
    void * answer = node_get(l, 0);
    return *(int *) answer;
}

int get_node(struct node * n){
  return node_get_int(n);
}

/*
 * Sets the element at index i to data.
 * Will return 1 if successful (valid i)
 * and 0 otherwise.
 */
int node_set(struct node *l, void *data) {

  if (l->head == NULL)
    return 0;

  struct list_node *current = l->head;

  current->data = data;
  return 1;
}

/* For use of primitive int casted to void * for generic linked list*/
int node_add_head_int(struct node * n, int data)
{
    printf("Adding data of type int to head of Node.");
    int * d = malloc(sizeof(int));
    *d = data;
    return node_add_head(n, d);
}


int node_remove_head_int(struct node * l)
{
    void * answer = node_remove_head(l);
    return *(int *) answer;
}

/*
 * Returns data from the head of the list and removes it.
 */
void * node_remove_head(struct node *l) {

  if (l->head == NULL)
    return NULL;

  struct list_node *oldHead = l->head;
  l->head = oldHead->next;
  void *data = oldHead->data;
  free(oldHead);
  l->size -= 1;
  return data;
}

int node_add_tail_int(struct node * l, int data)
{
    int * d = malloc(sizeof(int));
    *d = data;
    return node_add_tail(l, d);
}


/*
 * Adds to the front of the list.
 * Returns a 1 if successful and 0 otherwise.
 */
int node_add_head(struct node *l, void *data) {

  struct list_node *node = (struct list_node *)malloc(sizeof(struct list_node));
  if (node == NULL)
    return 0;

  node->data = data;
  node->next = l->head;
  l->head = node;
  ++l->size;
  return 1;
}

/*
 * Adds to the end of the list.
 * Returns a 1 if successful and 0 otherwise.
 */
int node_add_tail(struct node *n, void *data) {

  struct list_node *node = (struct list_node *)malloc(sizeof(struct list_node));
  if (node == NULL) {
    return 0;
  }
  node->data = data;
  node->next = NULL;

  /* if the list is empty, this node is the head */
  if (n->head == NULL) {
      ++n->size;
      n->head = node;
      return 1;
  }
  struct list_node *current = n->head;
  while (current->next != NULL) {
    current = current->next;
  }

  /* current is now the last node in the list */
  current->next = node;
  ++n->size;
  return 1;
}


/*
 * Returns the data of a node.
 */
void * node_get(struct node *n, int i) {

  if (n->head == NULL)
    return NULL;

  struct list_node *current = n->head;
  int j = 0;
  while (j != i) {
    current = current->next;
                ++j;
  }
  return current->data;
}

/*
 * Returns the size of a node.
 */
int size_node(struct node *n) {

  return n->size;
}

/*
 * Frees allocated memory for a node.
 */
void free_node(struct node *m) {
  free(m);
}

/*
 * LIST METHODS
 */

/*
 * Initializes an empty list.
 */
struct list * make_list() {

  struct list *l;
  l = malloc(sizeof(struct list));
  if (l == NULL)
    return NULL;

  l->size = 0;
  l->head = 0;
  return l;
}



/*
 * Returns the size of a list.
 */
int size(struct list *l) {

  return l->size;
}


int list_len(struct list *l){
  return size(l);
}

/*
 * Returns the element at index i of the list.
 */
void * list_get(struct list *l, int i) {

  if (l->head == NULL || i >= l->size || i < 0)
    return NULL;

  struct list_node *current = l->head;
  int j = 0;
  while (j != i) {
    current = current->next;
                ++j;
  }

  return current->data;
}

/*
 * Sets the element at index i to data.
 * Will return 1 if successful (valid i)
 * and 0 otherwise.
 */
int set(struct list *l, int i, void *data) {

  if (l->head == NULL || i >= l->size || i < 0)
    return 0;

  struct list_node *current = l->head;
  int j = 0;
  while (j != i) {
    current = current->next;
        ++j;
  }

  current->data = data;
  return 1;
}




/*
 * List set an integer specifically
 */
int list_set(struct list *l, int i, int data) {

  if (l->head == NULL || i >= l->size || i < 0)
    return 0;

  struct list_node *current = l->head;
  int j = 0;
  while (j != i) {
    current = current->next;
        ++j;
  }

  current->data = (void *) &data;
  return 1;
}

/*
 * Adds to the front of the list.
 * Returns a 1 if successful and 0 otherwise.
 */
int add_head(struct list *l, void *data) {

  struct list_node *node = (struct list_node *)malloc(sizeof(struct list_node));
  if (node == NULL)
    return 0;

  node->data = data;
  node->next = l->head;
  l->head = node;
  ++l->size;
  return 1;
}

/*
 * Adds to the end of the list.
 * Returns a 1 if successful and 0 otherwise.
 */
int add_tail(struct list *l, void *data) {

  struct list_node *node = (struct list_node *)malloc(sizeof(struct list_node));
  if (node == NULL) {
    return 0;
  }
  node->data = data;
  node->next = NULL;

  /* if the list is empty, this node is the head */
  if (l->head == NULL) {
      ++l->size;
      l->head = node;
      return 1;
  }
  struct list_node *current = l->head;
  while (current->next != NULL) {
    current = current->next;
  }

  /* current is now the last node in the list */
  current->next = node;
  ++l->size;
  return 1;
}

/*
 * Returns data from the head of the list and removes it.
 */
void * remove_head(struct list *l) {

  if (l->head == NULL)
    return NULL;

  struct list_node *oldHead = l->head;
  l->head = oldHead->next;
  void *data = oldHead->data;
  free(oldHead);
  l->size -= 1;
  return data;
}

/*
 * Returns data from the tail of a list and removes it.
 */
void * remove_tail(struct list *l) {

  if (l->head == NULL)
    return NULL;

  if (l->head->next == NULL) {
    return remove_head(l);
  }

  struct list_node *slow = l->head;
  struct list_node *fast = l->head->next;

  while (fast->next != NULL) {
    slow = fast;
    fast = fast->next;
  }

  /* slow is now the second to last node in the list */
  void *data = fast->data;
  slow->next = NULL;
  l->size -= 1;
  free(fast);
  return data;
}

/*
 * Prints out list of elements in a String list.
 * Used for testing.
 */
void printl(struct list *l) {

  printf("[");
  struct list_node *current = l->head;
  while (current != NULL)
        {
            if(current -> next == NULL)
            {

              printf("%s", current -> data);
            }
            else
            {
                printf("%s,", current->data);
            }
            current = current->next;
  }

  printf("]\n");
}

/*
 * Prints out list of elements in a list.
 * Used for testing.
 */
void printil(struct list *l) {

  printf("[");
  struct list_node *current = l->head;
  while (current != NULL)
        {
            if(current -> next == NULL)
            {
                printf("%d", *(int *) current -> data);
            }
            else
            {
                printf("%d,", *(int *)current->data);
            }
            current = current->next;
  }

  printf("]\n");
}

/*
 * Build a new list that is concatenating two lists.
 */
struct list * concat(struct list * a, struct list * b) {
  struct list *  new_list = make_list();
  struct list_node * current = a -> head;
  while (current) {
    add_tail(new_list, current -> data);
    current = current -> next;
  }
  current = b -> head;
  while(current) {
    add_tail(new_list, current -> data);
    current = current -> next;
  }
  return new_list;

}

/* For use of primitive int casted to void * for generic linked list*/
int add_head_int (struct list * l, int data)
{
    int * d = malloc(sizeof(int));
    *d = data;
    return add_head(l, d);
}

int add_tail_int (struct list * l, int data)
{
    int * d = malloc(sizeof(int));
    *d = data;
    return add_tail(l, d);
}

int list_get_int(struct list * l, int index)
{
    void * answer = list_get(l, index);
    return *(int *) answer;
}

int list_set_int(struct list * l, int index, int E)
{
    int answer = list_get_int(l, index);
    int * d = malloc(sizeof(int));
    *d = E;
    set(l, index, (void *) d);
    return answer;
}

int remove_head_int(struct list * l)
{
    void * answer = remove_head(l);
    return *(int *) answer;
}

int remove_tail_int(struct list * l)
{
    void * answer = remove_tail(l);
    return *(int *) answer;
}



void listSort(struct list *head)
{
	struct list_node* temp = head->head;
	//printf("%d\n", *(int *) temp->data);
	while (temp)
	{
		//printf("%d\n", *(int *)temp->data);
		struct list_node* min = temp;
		struct list_node* r = temp->next;

		while(r)
		{
			if (*(int *)min->data > *(int *)r->data)
				min = r;
			r = r->next;
		}

		int x = *(int *)temp->data;
		*(int *)temp->data =*(int *) min->data;
		*(int *)min->data = x;
		temp = temp->next;
	}
}






#ifdef BUILD_TEST
int main()
{
  char s[] = "HELLO WORLD09AZ";
  char *c;
  for ( c = s ; *c ; c++) printbig(*c);
}
#endif
