#include <stdio.h>
#include <stdlib.h>


/* For sorting integer array */
int compare(const void* a, const void* b){
    return(*(int*)a - *(int*)b);
}

int main(int argc, char argv[]){

    /* Open file */
    FILE *fptr;

    fptr = fopen("input.tsv", "r");

    /* Where integer column data will be written */
    int left[1000];
    int right[1000];
    int count = 0;

    /* Write file data to arrays */
    if(fptr != NULL){

        while(fscanf(fptr, "%d\t%d", &left[count], &right[count]) == 2){
            count++;
        }
        
        
    }else{

        printf("Not able to open the file!");

    }

    /* Sort arrays */
    qsort(left, 1000, sizeof(int), compare);

    qsort(right, 1000, sizeof(int), compare);


    /* Calculate difference */
    int difference = 0;
    for(int i = 0; i < 1000; i++){

        difference += abs(left[i] - right[i]);

    }

    printf("Difference is: %d\n", difference);


    /* Calculate similarity score */
    int counts[1000];
    int starting_index = 0;
    int j;
    int similarity = 0;


    for(int i = 0; i < 1000; i++){

        counts[i] = 0;

        /* Don't need to loop through everything since arrays sorted */
        for(j = starting_index; j < 1000 && right[j] <= left[i]; j++){

            if(left[i] == right[j]){
                counts[i]++;
            }

        }

        starting_index = j;
        similarity += left[i]*counts[i];

    }


    printf("Similarity score is: %d\n", similarity);


    fclose(fptr);


    return 0;


}