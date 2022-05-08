#include <stdlib.h>
#include <stdio.h>



struct Node {

    int bigit;

    struct Node *next;

};


void print_num(struct Node *nlst);
struct Node *add(struct Node *n1lst, struct Node *n2lst);
struct Node *mult(struct Node *n1lst, struct Node *n2lst);
struct Node *cons_bigit(int bgt, struct Node *nxt);
void free_num(struct Node *blst);
struct Node *copy_num(struct Node *nlst);








// links the nodes
struct Node *cons_bigit(int bgt, struct Node *nxt){
    struct Node *result = malloc(sizeof(struct Node));
    (*result).bigit = bgt;
    (*result).next = nxt;
    return result;
}



// frees the memory 
void free_num(struct Node *blst){
    struct Node *cur = blst;
    while(cur){
        struct Node *temp = cur;
        cur = cur->next;
        free(temp);
    }
}


struct Node *copy_num(struct Node *nlst);

//print number in the normal form
void print_num(struct Node *nlst){
    int zero = 0;
    if(nlst == NULL){
        printf("%d",zero);
    }else if(nlst->next != NULL){
        print_num(nlst->next);
        
        if((nlst->bigit)<1000){
            printf("%d",zero);
        }
        if((nlst->bigit)<100){
            printf("%d",zero);
        }
        if((nlst->bigit)<10){
            printf("%d",zero);
        }
        printf("%d",(nlst->bigit));
    }else{
        printf("%d", (nlst->bigit));
    }
    
 

}


//add the two linked node list and produces the new linked node list
struct Node *add(struct Node *n1lst, struct Node *n2lst){
    int x = 0;
    struct Node *output = NULL;
    while((n1lst != NULL) ||  (n2lst != NULL) || (x != 0)){
        struct Node *new;
        if(n1lst == NULL && n2lst == NULL){
            new =  cons_bigit( x%10000 , NULL) ;
            x=x/10000;
        }else if(n1lst == NULL){
            new = cons_bigit ((x+(n2lst->bigit))%10000 , NULL);
            x= (x+(n2lst->bigit))/10000;
            n2lst = n2lst->next;
        }else if(n2lst == NULL){
            new = cons_bigit( (x+(n1lst->bigit))%10000 , NULL);
            x = (x+(n1lst->bigit))/10000;
            n1lst = n1lst->next;
        }else{     
            new =  cons_bigit((x + (n2lst->bigit) + (n1lst->bigit))%10000 , NULL);        
            x =  (x + (n2lst->bigit) + (n1lst->bigit))/10000;
            n1lst = n1lst->next;
            n2lst = n2lst->next;   
        }

        struct Node *last = output; 

        if(last == NULL){
            output = new;
            
        }else{
            while(last->next != NULL){
                last = last->next;  
            }
            last->next = new; 
        }
    }
    return output;
}



// multiplies a number that's smaller than 10000 to a linked list and produces a new linked list

struct Node *multHelp(int coe, struct Node *n1lst ){
    int x = 0; 
    struct Node *output = NULL;

    while(n1lst != NULL || x != 0){

        struct Node *new;

        if(n1lst == NULL){
            new = cons_bigit(x%10000, NULL);
            x=x/10000;
        

        }else{
            new = cons_bigit( (x + ( coe * (n1lst->bigit)))%10000 , NULL);
         
            x= (x + (coe * (n1lst->bigit))) /10000;
        
            
            

            n1lst = n1lst->next;
      
        }

        struct Node *last = output; 

        if(last == NULL){
            output = new;
        }else{
            while(last->next != NULL){
                last = last->next;
            }
            last->next = new;

        }
        

    }


    return output;
}




// multiplies two linked two list and produces a new linked list
struct Node *mult(struct Node *n1lst, struct Node *n2lst){
    
    
    int x = 0;
    struct Node *output = NULL;
    while(n2lst != NULL){
        
        struct Node *special = (multHelp(n2lst->bigit , n1lst));

        if(special!=NULL){
            int i = 0;
            while(i<x){
                special = cons_bigit (0,special);
                i++;
            }
        x++;
        }
        

        
        struct Node *temp = output;

        output =  add( output, special);
        free_num(special);
        free_num(temp);
        
        
        n2lst = n2lst->next;
        

    }
    
    
    return output;

}

// int main(void) {
//     struct Node *n = cons_bigit(9999, cons_bigit(999, cons_bigit(99, cons_bigit(9, cons_bigit(1, NULL)))));
//     struct Node *m = cons_bigit(9999, cons_bigit(999, cons_bigit(99, cons_bigit(9, cons_bigit(1, NULL)))));
//     struct Node *k = NULL;
//     struct Node *h = mult(m, n);
//     struct Node *j = cons_bigit(9999, cons_bigit(1, NULL));
//     struct Node *l = cons_bigit(9999, cons_bigit(1, NULL));
//     struct Node *a = mult(l, j);
//     struct Node *b = mult(m, k);
//     struct Node *c = mult(k, m);
//     struct Node *d = NULL;
//     struct Node *e = mult(n, d);
//     struct Node *f = mult(e, m);
//     print_num(n);
//     printf("\n");
//     print_num(n);
//     printf("\n");
//     print_num(h);
//     printf("\n");
//     print_num(a);
//     printf("\n");
//     print_num(c);
//     printf("\n");
//     print_num(e);
//     printf("\n");
//     print_num(f);
//     free_num(a);
//     free_num(j);
//     free_num(l);
//     free_num(n);
//     free_num(m);
//     free_num(k);
//     free_num(h);
//     return 0;
// }

int main(void){
        struct Node *one = cons_bigit(1, NULL);
     struct Node *nine = cons_bigit(9999, cons_bigit(9999, NULL));
     struct Node *nine8 = cons_bigit(9999, cons_bigit(9989, NULL));
     struct Node *onek = cons_bigit(0, cons_bigit(1, NULL));
     struct Node *series = cons_bigit(2345, cons_bigit(1, NULL));
     struct Node *big = cons_bigit(7890, (cons_bigit(3456, cons_bigit(12, NULL))));
     print_num(NULL); printf("\n");
     printf("1: "); print_num(one); printf("\n");
     printf("9: "); print_num(nine); printf("\n");
     printf("98: "); print_num(nine8); printf("\n");
     printf("10k: "); print_num(onek); printf("\n");
     printf("line: "); print_num(series); printf("\n");
     printf("big: "); print_num(big); printf("\n");
    printf("9+98: ");print_num(add(nine, nine8));printf("\n");
     printf("1+9: "); print_num(add(one, nine)); printf("\n");
     printf("1+98: "); print_num(add(one, nine8)); printf("\n");
     printf("1+0: "); print_num(add(one, NULL)); printf("\n");
     printf("0+1: "); print_num(add(NULL, one)); printf("\n");
     printf("0+0: "); print_num(add(NULL, NULL)); printf("\n");
     printf("10kxline: "); print_num(mult(onek, series)); printf("\n");
     printf("NxN: "); print_num(mult(NULL, NULL)); printf("\n");
     printf("1xN: "); print_num(mult(one, NULL)); printf("\n");
     printf("10kxN: "); print_num(mult(onek, NULL)); printf("\n");
     printf("linexN: "); print_num(mult(series, NULL)); printf("\n");
     printf("bigxN: "); print_num(mult(big, NULL)); printf("\n");
     print_num(mult(nine, nine8)); printf("\n");

     return 0;
     
}