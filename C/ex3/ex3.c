#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


int oneVsAll(int M, int N, double X[M][N], double y[M], int num_labels, double lambda, double all_theta[num_labels][N]);
int predictOneVsAll(int M, int N, double all_theta[10][N], double X[M][N], double g[M]);

int main()
{
int M = 5000, N = 400;
int num_labels = 10;      
FILE *fp;
char line[4012];  //400*10 + 1*10 + 2 /// 2='\n'
int i, j, k, ri;	
static double X[5000][400] ;  //array is large then have to use "static"
static double y[5000] ;
double sel[100][N];
int index[100];
double lambda = 0.1;	
double all_theta[num_labels][N];

if((fp = fopen ("ex3data1.txt", "r")) == NULL){
		printf("open wrong \n");
	}else{
	       i = 0;
	       while(fgets(line, sizeof line, fp) != NULL){
//		   printf("line[%d]=  %s \n", i, line); 
		   
		   X[i][0] = (double)atof(strtok (line, " "));
		   	
		   for(j=1; j < N; j++)	   	
    	         X[i][j] = (double)atof(strtok (NULL, " "));
		  
		   y[i] = (double)atoi(strtok (NULL, ","));			
		   
//		   printf("%.5f, %.5f ,%.0f\n", X[i][0], X[i][1],  y[i]);
//               getchar();

		   i++;	   
	       }
   fclose(fp);
}
	 

// Randomly select 100 data points to display
index[0] = 0;
for(i=0; i < 100; i++){
     ri = rand()%M;     
     for(j=0; j < i; j++){
     	  if(index[j] != ri){
	  	 index[i] = ri;
		 
	  	for(k=0; k < N; k++)
     	         sel[i][k] = X[ri][k];	     
	  }
     }
}
//displayData(sel);


//// ============ Part 2: Vectorize Logistic Regression ============
//  In this part of the exercise, you will reuse your logistic regression
//  code from the last exercise. You task here is to make sure that your
//  regularized logistic regression implementation is vectorized. After
//  that, you will implement one-vs-all classification for the handwritten
//  digit dataset.
//

//printf("\nTraining One-vs-All Logistic Regression...\n")

      oneVsAll(M, N, X, y, num_labels, lambda, all_theta);


//================ Part 3: Predict for One-Vs-All ================

	int ta =0;
	double g[M];
	
      predictOneVsAll(M, N, all_theta, X, g);

	for(i=0; i < M; i++){
//		   printf("%d  %lf  %lf\n", i, g[i], y[i]);
	   if(fabs(y[i] - g[i]) <= 0.1)
	   	ta++;
	}
	printf("Train Accuracy: %d   %d  %f%\n", ta, M,  (double)ta/(double)M * 100);

 exit(0);

}
