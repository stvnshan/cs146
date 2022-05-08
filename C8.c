#include<stdio.h>

struct Node {

    int data;

    struct Node *next;

};

struct Node *reverse(struct Node *lst){
    struct Node *pre = NULL;    
    struct Node *cur = lst;
    struct Node *next = NULL;
    while(cur != NULL){
        next = cur->next;
        cur->next = pre;
        pre = cur;
       cur = next;
    }
    lst = pre;
    return lst;
}




