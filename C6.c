#include<stdio.h>
#include"C6.h"


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

void prime_factors(int n){
    int i=2;
    while(i<=isqrt(n)){
        if(n%i==0){
            printf("%d\n",i);
            n=n/i;
            i--;
        }
        i++;
    }
    printf("%d\n",n);

}






