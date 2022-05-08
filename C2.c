#include <stdio.h>
#include <ctype.h>



int getIntHelper(int acc){
    int c= getchar();
    
    if (isdigit(c)) {
        return getIntHelper(16*acc + c - '0');
    }
    if(c=='a'||c=='A'){
        return getIntHelper (16*acc + 10 );
    }
    if(c=='b'||c=='B'){
        return getIntHelper(16*acc + 11 );
    }
    if(c=='c'||c=='C'){
        return getIntHelper(16*acc + 12);
    }
    if(c=='d'||c=='D'){
        return getIntHelper(16*acc + 13);
    }
    if(c=='e'||c=='E'){
        return getIntHelper(16*acc + 14 );
    }
    if(c=='f'||c=='F'){
        return getIntHelper(16*acc + 15 );
    }
    return (ungetc(c,stdin) , acc);

	
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
    printf("%x\n",x);
    return 0;
}
