#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LEN 256


/* Determine if array passes safety checks */
int determine_safeness(int *D, int n){

    int monotonic = 0;
    int stable = 0;


    bool decreasing;

    if(D[0] < 0){
        decreasing = true;
    }else if(D[0] > 0){
        decreasing = false;
    }else{
        return 0; // Not safe
    }

    for(int i = 1; i < n; i++){

        if((D[i] > 0 && decreasing) || (D[i] < 0 && !decreasing) || (D[i] == 0) || (abs(D[i]) > 3)){

            return 0; // Not safe

        }

    }

    return 1; // Safe

}

/* Determine differences between adjacent elements, from which all else is derivable */
void calc_differences(int **D, const int *a, int n){

    for(int i = 0; i < (n-1); i++){
        (*D)[i] = a[i + 1] - a[i];
    }
    
}

/* For generating array from line */
void parse_numbers(int **a, const char *L){

    int index = 0;

    char num_as_str[5];

    char *token = strtok(L, " ");

    while(token != NULL){
        (*a)[index] = atoi(token);
        token = strtok(NULL, " ");
        index++;
    }

}

/* For determining how many numbers are in the string */
int count_spaces(const char *L){

    int counter;

    for(; *L != '\0'; L++){

        if(*L == ' '){
            counter++;
        }
    }

    return counter;

}

/* Solution implementation */
int main(int argc, char argv[]){

    /* Open file */
    FILE *fptr;

    fptr = fopen("input.tsv", "r");

    char line[MAX_LEN];

    int safe_counter = 0;


    /* Parse line by line */
    if(fptr != NULL){

        while (fgets(line, MAX_LEN, fptr)){

            // Figure out how many numbers are in the line
            int n_nums = count_spaces(line) + 1;

            // If only one number, it's definitely safe
            if(n_nums == 1){
                safe_counter++;
            }

            // Turn line of characters into array of ints
            int *a = malloc(n_nums * sizeof(int));
            parse_numbers(&a, line);

            // Determine difference between each element
            int *diffs = malloc((n_nums - 1) * sizeof(int));
            calc_differences(&diffs, a, n_nums);

            // Determine if it is safe
            safe_counter += determine_safeness(diffs, n_nums - 1);

            free(a);


        }

    }else{

        fprintf(stderr, "Unable to open file!\n");

    }

    printf("Number of safe entires is %d\n", safe_counter);


    return 0;


}