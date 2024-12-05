#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX_BUFFER 256
#define LINE_LENGTH 140
#define ROW_COUNT 140 

/* search_letter needs to return */

int count_xmas(char ws[][MAX_BUFFER], int start_row, int start_col){

    int letter_count = 0;
    int xmas_count = 0;


    for(;;){

        search_letter(letter_count, start_row, start_col);

    }


    return xmas_count;   

}

int main(void){

    /* Open file */
    FILE *file;

    file = fopen("input.txt", "r");

    /* Store word search in 2D array */
    char line[LINE_LENGTH];
    char wordsearch[LINE_LENGTH - 1][ROW_COUNT];

    if(file == NULL){
        printf("Couldn't open file!");
        return 1;
    }

    int linecount = 0;
    while((fgets(wordsearch[linecount], MAX_BUFFER, file) != NULL) & linecount < ROW_COUNT){

        linecount++;

    }

    /* Perform word search */
    int xmas_cnt = 0;
    for(int i = 0; i < LINE_LENGTH; i++){
        for(int j = 0; j < ROW_COUNT; j++){

            if(wordsearch[i, j] == 'X'){

                xmas_cnt += count_xmas(wordsearch, i, j);

            }

        }
    }


    return 0;

}