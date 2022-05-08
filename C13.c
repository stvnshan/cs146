#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct anode{
    char *key;
    int value;
    struct anode *next;
};

struct hash {

    struct anode **table;

};

// void add(struct anode *mynode, char *copy, int wordnum)){
//     struct anode newn = malloc(sizeof(struct anode));

// }

//i index count = 1
int same (struct anode *mynode, char *cary , int count){
    int i =0;
    char *a = mynode->key;
    int count2 = 0;
    while(*(a+count2) != '\0'){
        count2++;
    }
    if(count2 != count){
        return 0;
    }
    while(i<=count){
        if(a[i]==cary[i]){
            i++;
        }else{
            return 0;
        }
    }
    return 1;
}





int main(){
    char cary[80];
    char inp;
    int count=0;
    int indx;
    int wordnum=0;
    int f=0;

    struct hash h1;
    h1.table= malloc(2*26*sizeof(struct anode));
    int app = 0;
    while(app < 2*26) h1.table[app++] = NULL;
    app = 0;
    while(app<80) cary[app++] = '\0';
    
    while(scanf("%c",&inp) != -1){
        //printf("%c\n", inp);
        if(inp== ' '||inp == '\n'){
            if(count!=0){
                if('a'<=cary[0] && cary[0]<='z'){
                    indx= cary[0] - 'a';                    
                }else{
                    indx = cary[0] - 'A' + 26;
                }
                
                struct anode *mynode = (h1.table)[indx];
                int value;
                while(mynode){
                    int gg = same(mynode,cary,count);
                    if(gg){
                        value = mynode->value;
                        break;
                    }
                    mynode = mynode -> next;
                }

                
                if(mynode==NULL){
                    
                    char *copy = malloc((strlen(cary)+1)*sizeof(char));
                    strcpy(copy, cary);
                    // char copy[count+1];

                    
                    // int i=0;
                    // while(i<=count){
                    //     printf("%c",cary[i]);
                    //     copy[i]=cary[i];
                    //     i++;
                    // }

                    struct anode *newn = malloc(sizeof(struct anode));
                    newn->value = wordnum;
                    // newn->key = &copy[0];
                    newn->key = copy;
                    newn->next = h1.table[indx];
                    h1.table[indx] = newn;
                    
                    
                    wordnum++;
                    printf("%s", copy);
                }else{
                    printf("%d",value);
                }
                

                count=0;
                int k = 0;
                

                while (k< 80) cary[k++] = '\0';
                // for(int i=0;i<80;i++) cary[i] = '\0';
                printf("%c",inp);
            }else{
                printf("%c",inp);
            }



        }else{
            cary[count] = inp;
            ++count;
        }
    } 


    if(count!=0){
        // int i=0;
        // while(i<=count){
        //     printf("%c",cary[i]);
        //     count--;
        // }
        int value;
        if('a'<=cary[0] && cary[0]<='z'){
            value = cary[0] - 'a';
        }else{
            value = cary[0] -'A' + 26;
        }
        struct anode *aanode = h1.table[value];
        while(aanode){
            if(same(aanode,cary,count)){
                printf("%d",aanode->value);
                f=1;
            }
            aanode=aanode->next;
        }
        if(f == 0){
            printf("%s",cary);
        }
        f=0;
    }



    int k =0;
    while(k< 2*26){
        struct anode *pnode = h1.table[k];
        while(pnode){
            struct anode *temp = pnode;
            pnode = pnode->next;
            //pnode = (*pnode).next;
            free(temp->key);
            free(temp);
        }
        k++;    
    }
    free(h1.table);
}