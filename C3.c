#include<stdio.h>
#include"C3.h"

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


