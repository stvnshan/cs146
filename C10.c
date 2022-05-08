#include <stdio.h>
#include "C10.h"

 

void printNums(int m, int n){
    int i=0;
    int j=0;
    int k=2;
    int temp;
    goto loop;

    printN:
        printf("%d\n",m);
        if(m==2147483647) goto end;
        ++m;
        goto loop;

    palin:
        if(i>1000000000) goto palinB;
        if(i == 0) goto palinN;
        j=j*10 + i%10;
        i=i/10;
        goto palin;

    // palinB:
    //     j=j*10+i%10;
    //     i=i/10;
    //     j=j*10+i%10;
    //     i=i/10; 
    //     j=j*10+i%10;
    //     i=i/10; 
    //     j=j*10+i%10;
    //     i=i/10; 
    //     j=j*10+i%10;
    //     i=i/10;
    //     if(j == m/100000) goto palinBB;
    //     if(m==n) goto end;
    //     ++m;
    //     goto loop;

    // palinBB:
    //     k=3;
    //     i=m;
    //     goto squP;


    palinN:
        if(j == m) goto palinNN;
        if(m==n) goto end;
        ++m;
        goto loop;

    palinNN:
        k=3;
        i=m;
        goto squP;


    squP:
        if(i%2 == 0) goto idk;
        goto squ;

    idk:
        i=i/2;
        if(i%2==0) goto idkk;
        if(m==2147483647) goto end;
        goto squ;
    idkk:
        ++m;
        if(m==2147483647) goto end;
        if(i%2==0) goto loop;
        goto squ;


    squ:
        if(k<= i/k)    goto squF;
        goto printN;
    squF:
        if( i%k == 0) goto squC;
        k=k+2;
        goto squ;
        
    squC:
        i = i/k;
        if(i% k == 0) goto add;
        k=k+2;
        goto squ;

    add:
        if(m==n) goto end;
        ++m;
        if(m==2147483647) goto end;
        goto loop;
    
    llop:
        i=m;
        j=0;
        goto palin;
    
    loop:
        if(m==2147483647) goto end;
        if(m<=n) goto llop;

    end:
        m++;
        
}