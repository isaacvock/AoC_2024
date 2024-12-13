#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX_BUFFER 256
#define LINE_LENGTH 140
#define ROW_COUNT 140 

const char *WORD = "XMAS";


int count_xmas(char ws[][LINE_LENGTH], int start_row, int start_col){

    printf("start_row is: %d; start_col is %d\n", start_row, start_col);

    int xmas_count = 0;

    int row_min = start_row - 1;
    if(row_min < 0){
        row_min = 0;
    }

    int row_max = start_row + 1;
    if(row_max >= ROW_COUNT){
        row_max = ROW_COUNT-1;
    }

    int col_min = start_col - 1;
    if(col_min < 0){
        col_min = 0;
    }

    int col_max = start_col + 1;
    if(col_max >= LINE_LENGTH){
        col_max = LINE_LENGTH-1;
    }

    int cstep;
    int row_bound;
    int col_bound;
    char query[5];
    int row_i;
    int col_i;

    printf("Initialized everything\n");

    for(int rdiff = -1; rdiff<=1; rdiff++){

        printf("rdiff is %d\n", rdiff);

        if(rdiff == 0){
            cstep = 2;
        }else{
            cstep = 1;
        }

        for(int cdiff = -1; cdiff<=1; cdiff += cstep){

            printf("cdiff is %d\n", cdiff);

            row_bound = start_row + rdiff*3;
            col_bound = start_col + cdiff*3;

            if(row_bound >= 0 && row_bound < ROW_COUNT && col_bound >= 0 && col_bound < LINE_LENGTH){

                for(int n = 0; n<4; n++){

                    row_i = start_row + rdiff*n;
                    col_i = start_col + cdiff*n;
                    printf("row_i is: %d; col_i is: %d\n", row_i, col_i);

                    query[n] = ws[row_i][col_i];

                }
                query[4] = '\0';
                
                if(strcmp(WORD, query) == 0){
                    xmas_count++;
                }

            }


        }

    }


    return xmas_count;   

}

int main(void){

    /* Open file */
    FILE *file;

    file = fopen("input.txt", "r");

    /* Store word search in 2D array */
    char wordsearch[ROW_COUNT][LINE_LENGTH];

    if(file == NULL){
        printf("Couldn't open file!");
        return 1;
    }

    int linecount = 0;
    while((fgets(wordsearch[linecount], MAX_BUFFER, file) != NULL) & linecount < ROW_COUNT){

        linecount++;

    }

    printf("Stored the file contents\n");

    /* Perform word search */
    int xmas_cnt = 0;
    for(int i = 0; i < LINE_LENGTH; i++){
        for(int j = 0; j < ROW_COUNT; j++){

            printf("i = %d, j = %d\n", i , j);

            if(wordsearch[i][j] == 'X'){

                xmas_cnt += count_xmas(wordsearch, i, j);

            }

        }
    }

    printf("Number of XMASes is %d\n", xmas_cnt);


    return 0;

}