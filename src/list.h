#pragma once

// This is an intrusive list & as such the *lnode*
// header must be used as the first field of any struct.

typedef struct lnode_ {
	struct lnode_ *prev, *next;
} lnode;

typedef struct {
	lnode *front, *back;
	volatile unsigned cnt;
} list;

#define list_init(l) { (l)->cnt = 0; (l)->front = (l)->back = NULL; }
#define list_count(l) (l)->cnt
#define list_front(l) (void*)(l)->front
#define list_back(l) (void*)(l)->back
#define list_prev(n) (void*)((lnode*)(n))->prev
#define list_next(n) (void*)((lnode*)(n))->next

void list_push_front(list *l, void *n);
void list_push_back(list *l, void *n);
void *list_remove(list *l, void *n);
void *list_pop_front(list *l);
void *list_pop_back(list *l);
