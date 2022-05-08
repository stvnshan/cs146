#include<stdio.h>
#include"C4.h"


int isqrt (int n){
    int i=1;
    if(n==0){
        return 0;
    }
    while(i<=n/i){
        i++;
    }
    return i-1;
}

void sumsqr(int m){
    int max= isqrt(m);
    int i=0;
    while(i<= max){
        int j=0;
        while(j <= i){
            if(i*i+j*j==m){
                printf("%d %d\n", i , j);
            }
            j++;
        } 
        i++;
    }
}

