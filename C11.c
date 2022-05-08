#include<stdio.h>

int lowsearch(int A[], int key, int n){
    // n>0, A[0]<= key <=A[n-1]
    int low = 0;
    int high = n;
    // n>0, A[0]<= key <=A[n-1], low = 0, high = n
    int mid;
    // A[low] <= key <= A[high-1]
    while (low < high) {
        // low < high,  A[low]<= key <=A[high-1] 
        mid = (low + high) / 2;
        // mid = (low + high) / 2,  A[low] <= key <= A[high-1]
        // low < mid < high,  A[low] <= key <= A[high-1] 
        if (key <= A[mid]) {
            // key <= A[mid] ,low < mid < high,  A[low] <= key <=A[high-1]
            // key <= A[mid] ,low <= mid-1 < high-1,  A[low] <= key <=A[high-1]
            // low <= mid-1 < high-1,  A[low] <= key <=A[mid-1]
            high = mid ;
            // low <= high-1,  A[low] <= key <=A[high-1]
            // low < high, A[low] <= key <= A[high-1]
        } else{
            // key > A[mid] ,low < mid < high, A[mid] < key <= A[high-1]
            // key >= A[mid+1], low < mid+1 <= high, A[mid] < key <= A[high-1]
            // low < mid+1 <= high, A[mid+1] <= key <= A[high-1]
            low = mid + 1;
            //low <= high, A[low] <= key <= A[high-1]

        }
        //A[low] <= key <=A[high-1] 
    }

    // low=high, A[low]= A[high], A[low] <= key <= A[high-1]
    // A[high-1]=A[low-1]< A[low] <= key <=A[high-1]<= A[high]
    // A[high] >= key , A[high-1] < key
    return high;
}



