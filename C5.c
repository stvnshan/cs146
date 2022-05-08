#include<stdio.h>
#include"C5.h"

void regular(int m){
    printf("%d\n",1);
    int i=2;
    for(i; i<= m; i++){
        int j=i;
        while(j%2==0 || j%3==0 || j%5==0){
            if(j%2==0){
                j=j/2;
            }
            if(j%3==0){
                j=j/3;
            }
            if(j%5==0){
                j=j/5;
            }

        }
        
        if(j==1){
            printf("%d\n",i);
        }
    }
}
    
