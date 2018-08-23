#include <stdio.h>
#include <stdlib.h>
#include <string.h>

double costFunction(int M, int N, double theta[N], double X[M][2], double y[M], double grad[N]);

int main( )
{
	int   M = 100, N = 3;
	int  i;
	double X[M][2];
	double  y[M];
	double grad[3], ini_theta[3];
	double JCost;
   
	FILE *fp;
	char line[50];		
	
	if((fp = fopen ("ex2data1.txt", "r")) == NULL){
		printf("open wrong \n");
	}else{
	       i = 0;
	       while(fgets(line, sizeof line, fp) != NULL){
//		   printf("line[%d]=  %s \n", i, line); 
		   
		   X[i][0] = (double)atof(strtok (line, ","));		   	
    	         X[i][1] = (double)atof(strtok (NULL, ","));	
		      y[i] = (double)atoi(strtok (NULL, ","));			
		   
//		   printf("%.14f, %.14f ,%.0f\n", X[i][0], X[i][1],  y[i]);
//               getchar();

		   i++;	   
	       }
	    fclose(fp);
	 }
	
	for(i=0; i < N; i++)
	   ini_theta[i] = 0;  
      JCost = costFunction(M, N, ini_theta, X, y, grad);

	printf("  \n ");
	printf("cost Function = %lf ", JCost);
	printf("  \n");
      printf("Gradient at initial theta (zeros)= %lf, %lf, %lf \n", grad[0],grad[1],grad[2]);
	printf(" \n");
	
	exit(0);
	
}
