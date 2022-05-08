#include<stdio.h>
#include"array.h"


void moveloc2(int loc1,int loc2){
    for(loc2-1;loc1<loc2;--loc2){
        put(loc2,get(loc2-1));
    } 
}  
void reloc(int loc1, int loc2){
    for(loc1-1;loc1-1<loc2-1;++loc1){
        put(loc1-1,get(loc1));
    }
}

void readInput(int loc1, int loc2, int loc3){
    char a;
    int num;
    int content;
    while(scanf("%c",&a) != EOF){
        
        if (a == 'u'){
            scanf("%d",&num);
            scanf("%d",&content);
            if (num == 0){
                if(loc1 == loc2){
                    put(loc1, content);
                    ++loc1;
                    ++loc2;
                }else{
                    moveloc2(loc1,loc2);
                    ++loc2;
                    put(loc1, content);
                    ++loc1;
                }
            }else if(num == 1){
                put(loc2,content);
                ++loc2;
            }else{
                put(loc3,content);
                --loc3;
            }
        }
        if (a == 'o'){
            scanf("%d",&num);
            if(num==0){
                printf("%d\n",get(loc1-1));
                reloc(loc1,loc2);
                --loc1;
                --loc2;

            }else if(num==1){
                printf("%d\n",get(loc2-1));
                --loc2;
            }else{
                printf("%d\n",get(loc3+1));
                ++loc3;
            }


        }
    }
    
}

int main(){
    readInput(0,0,20);
}
