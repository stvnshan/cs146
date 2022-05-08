#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct anode {

  int key;

  char *value;

  struct anode *next;

};

 

struct hash {

    int size;

    struct anode **table;

};



struct hash make_table(int s){
    struct hash h1;
    h1.size = s;
    
    h1.table = malloc(s*sizeof(struct anode*));
    int i=0;
    while(i<s){
      h1.table[i] = NULL;
      ++i;
    }
    return h1;
}

char *search(struct hash T, int k){
    int index = k % T.size;
    struct anode *aanode = (T.table)[index];
    while(aanode){
      if(aanode->key == k){
        return aanode->value;
      }else{
        aanode = aanode->next;
      }
    }
    return NULL;
  
}



void  add(struct hash T, int k, char *v){
    char *nv = malloc(strlen(v)+1);
    char *temp = nv;
    
   
    nv = strcpy(nv, v);
    nv[strlen(v)] = '\0';
   
    int index = k % T.size;
    struct anode *newnode = malloc (sizeof(struct anode));
    newnode->key = k;
    newnode->value = nv;
    newnode->next =  (T.table)[index];
    (T.table)[index] = newnode;
}






void free_table(struct hash T){
  int i = T.size - 1;
  while(i>=0){
    struct anode *head = (T.table)[i];
    while(head){
      struct anode *temp;
      temp = head;
      head = head->next;
      free(temp->value);
      temp->value = NULL;
      free(temp);
      
    }
    --i;
  }
  free(T.table);
}




void delete(struct hash T, int k){
  int index = k % T.size;
  struct anode *head = (T.table)[index];
  struct anode *prev = head;
  
  while(head){
    
    if(head->key == k){
      if(head == (T.table)[index]){

        (T.table)[index] = head->next;
        struct anode *temp = head;
        
        head = head->next;
        prev = head;

        temp->next = NULL;
        free(temp->value);
        temp->value = NULL;
        free(temp);
        
      }else{
        struct anode *temp=head;
        prev->next = head -> next;
        head=head->next;
        temp->next = NULL;
        free(temp->value);
        temp->value = NULL;
        free(temp);
        
      }
    }else{
      prev = head;
      head = head->next;
    } 
  }
}

// int main() {
//     struct hash table = make_table(5);
//     char *a = "abcd";
//     add(table, 1, a);
//     a = "abc";
//     add(table, 2, a);
//     a = "a";
//     add(table, 6, a);
//     printf("%s\n", search(table, 1));
//     printf("%s\n", search(table, 2));
//     printf("%s\n", search(table, 6));
//     delete(table, 1);
//     printf("%d\n", search(table, 1));
//     printf("%s\n", search(table, 2));
//     printf("%s\n", search(table, 6));
// }

// int main(){
//     struct hash table = make_table(3);
//     add(table, 5, "world");
//     add(table, 1, "rickie");
//     add(table, 2, "lucas");
//     add(table, 3, "peter");
//     add(table, 4, "carl");
    
//     int i = 0;
  
//     while (i < 3){
//         struct anode *temp = table.table[i];
//         printf("%d: \n", i);
//         while(temp){
//             printf("key: %d ", temp->key);
//             int j=0;
//             printf("%s", temp->value);
//             // while( (temp->value)[j] != '\0'){
//             //     printf("%c", (temp->value)[j]);
//             //     j++;
//             // }
            
//             printf("\n");
//             temp = temp->next;
//         }
//         ++i;
//     }
//     delete(table, 1);
//     add(table, 2, "hell");
    

//     i=1;
//     struct anode *temp = table.table[i];
//         printf("%d: \n", i);
//         while(temp){
//             printf("key: %d ", temp->key);
//             int j=0;
//             while( (temp->value)[j] != '\0'){
//                 printf("%c", (temp->value)[j]);
//                 j++;
//             }
//             printf("\n");
//             temp = temp->next;
//         }


//     i=0;
//     while (i < 3){
//         struct anode *temp = table.table[i];
//         printf("%d: \n", i);
//         while(temp){
//             printf("key: %d ", temp->key);
//             int j=0;
//             while( j < strlen(temp->value)){
//                 printf("%c", (temp->value)[j]);
//                 j++;
//             }
//             printf("\n");
//             temp = temp->next;
//         }
//         i++;
//     }
    
//     delete(table, 2);
    
//     //printf("%d\n", 1);
//     i = 0;
//     while ( i < 3){
//         struct anode *temp = table.table[i];
//         printf("%d: \n", i);
//         while(temp){
//             printf("key: %d ", temp->key);
//             int j=0;
//             while( j < strlen(temp->value)){
//                 printf("%c", (temp->value)[j]);
//                 j++;
//             }
//             printf("\n");
//             temp = temp->next;
//         }
//         i++;
//     }

//     char* c = search(table, 5);
//     printf("%d\n", c==NULL);

//     char* a = search(table, 4);
//     int j=0;
//     while(j < strlen(a)){
//         printf("%c", a[j]);
//         j++;
//     }
    
// }

// void printword (char *word){
//     printf(">>");
//     if (word != NULL){
//         int len = strlen(word);
//         for(int i = 0; i < len; i++) {
//             printf("%c", word[i]);
//         }
//     }
//     printf("<<\n");
// }

// int main() {
//     printf("creating Hash Table of size 100.\n");

//     struct hash HT;
//     HT = make_table(98);
//     printf("Create table succeeded.\n");
//     //printf("seaching table for key 39...\n");
//     //	printword(search(HT, 39));

//     printf("adding values | key   :\n");
//     printf("Test1 | 1\n");
//     add(HT,1, "Test1");
//     printf("Test2 | 2\n");
//     add(HT, 2, "Test2");
//     printf("Test12 | 12\n");
//     add(HT,12, "Test12");
//     printf("Test23 | 23\n");
//     add(HT,23, "Test23");
//     printf("Test55 | 55\n");
//     add(HT,55, "Test55");
//     printf("Test80 | 80\n");
//     add(HT,80, "Test80");
//     printf("Test99 | 99\n");
//     add(HT,99, "Test99");
//     printf("Test100 | 100\n");
//     add(HT,100, "Test100");
//     printf("Test101 | 101\n");
//     add(HT,101, "Test101");
//     printf("Test196 | 196\n");
//     add(HT,196, "Test196");
//     printf("Test293 | 293\n");
//     add(HT,293, "Test293");
//     printf("Test20003 | 20003\n");
//     add(HT,20003, "Test20003");
//     printf("Test293 | 293\n");
//     add(HT,293, "Test293");

//     printf("searching for said values...\n");
//     printword(search(HT, 1));
//     printword(search(HT, 2));
//     printword(search(HT, 12));
//     printword(search(HT, 23));
//     printword(search(HT, 55));
//     printword(search(HT, 80));
//     printword(search(HT, 99));
//     printword(search(HT, 100));
//     printword(search(HT, 101));
//     printword(search(HT, 196));
//     printword(search(HT, 293));
//     printword(search(HT, 20003));

//     printf("adding TestTest8.52 to key already used, 293\n");
//     add(HT,293, "TestTest8.52");
//     printf("searching for it...\n");
//     printword(search(HT, 293));

//     printf("deleting key 293\n");
//     delete(HT,293);
//     printf("searching for it...\n");
//     printword(search(HT, 293));

//     free_table(HT);
//     printf("Done. Freeing Hash Table...\n");

//     printf("Finished. Exiting...\n");

//     return 0;
// }

