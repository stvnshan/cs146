#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include<ctype.h>

struct anode{
    char *key;
    int value;
    struct anode *next;
};

struct hash{
    struct anode **table;
};

int main(){
    char cary[80];
    struct hash h1;
    h1.table = malloc(10*(sizeof(struct anode)));
    int count = 0;
    int wordnum = 0;
    char inp;
    int v = 0;
    int fla = 0;
    
    int app = 0;
    while(app < 10) h1.table[app++] = NULL;
    app = 0;
    while(app<80) cary[app++] = '\0';
    
    while(scanf("%c",&inp) != -1){
        if(inp == ' ' || inp == '\n'){
            if(count != 0){
                if(fla != 0){
                    int indx = v % 10;
                    struct anode *mynode = h1.table[indx];
                    while(mynode){
                        if(mynode->value==v){
                            printf("%s",mynode->key);
                        }
                        mynode=mynode->next;
                    }
                }else{
                    char *k = malloc((strlen(cary)+1)*(sizeof(char)));
                        py(k,cary);
                    
                    int indx= wordnum%10;
                    // printf("%d",indx);
                    struct anode *nn=malloc(sizeof(struct anode));
                    nn->key = k;
                    nn->value = wordnum;
                    nn->next = h1.table[indx];
                    h1.table[indx]=nn;
                    printf("%s",k);
                    int i=0;
                    while(i<80){
                        cary[i]='\0';
                        i++;
                    }

                    wordnum++;
                }
                fla = 0;
                v=0;
                count=0;
                printf("%c",inp);
            }else{
                printf("%c",inp);
            }

        }else{
            if(isdigit(inp)){
                fla =1;
                v=v*10+(inp-'0');

                
            }else{
                // printf("%c",inp);
                cary[count] = inp;
            }
            count++;
        }
    }

        if(fla==0){
            printf("%s",cary);
        }else{
            int indx = v % 10;
            struct anode *mynode = h1.table[indx];
            while(mynode){
                if(mynode->value==v){
                    printf("%s",mynode->key);
                }
                mynode=mynode->next;
            }

        }
    

    int i=0;
    while(i<10){
        struct anode *h=h1.table[i];
        while(h){
            struct anode *temp;
            temp = h;
            h=h->next;
            free(temp->key);
            free(temp);
        }
        i++;

    }
    free(h1.table);

}
























