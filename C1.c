#include <stdio.h>
#include <ctype.h>



int getIntHelper(int acc){
    int c= getchar();
    
	return (isdigit(c))? getIntHelper(10*acc + c - '0') : (ungetc(c,stdin) , acc);

}

int skipws(){
    int c=getchar();
    if(c==EOF){
        return -1;
    }
    if(isspace(c)){
        return skipws();
    }
        
	ungetc(c,stdin);
    return 1;
    
}


int getInt(){
    int c = skipws();
    if(c== -1){
        return -1;
    }
    return getIntHelper(0);
}


int getX(int acc){
    int c = getInt();
    if(c != -1){
        return getX(acc+c);
    }
    return acc;
}
 


int main(void){
    int x = getX(0);
    printf("%d\n",x);
    return 0;
}
