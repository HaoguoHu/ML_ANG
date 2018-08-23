#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

double costFunctionReg(int M, int N, double theta[N], double X[M][N], double y[M], double grad[N], double lambda);
int mapFeature(int M, int N1, double X[M][N1], int degree, int N, double out[M][N]);
int predict(int M, int N, double theta[N], double X[M][N], double p[M]);
//int newtonGradien(int M, int N, double theta[N], double X[M][N], double y[M], double lambda, int iterations);
int fmincg(double (*CostFunctionReg)(int M, int N, double theta[N], double X[M][N], double y[M], double grad[N],
double lambda), int M, int N, double X[M][N], double y[M], double theta[N], double lambda, int maxCostCalls);

int main( )
{
	int   M=118, N1=2, ND=6,  N = 28;
	int  i, j, iterations = 400;
	double X[M][N1], XM[M][N];
	double  y[M], g[M];
	double grad[N], theta[N], gradd[N];
	double JCost, lambda=0.1; 

   
	FILE *fp;
	char line[50];		
	
	if((fp = fopen ("ex2data2.txt", "r")) == NULL){
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
	
	mapFeature(M, N1, X, ND, N, XM); //mapping X[M][N1] to XM[M][N];
//	for(i=0; i < N; i++)
//		printf(" %lf    %lf  %lf\n", XM[i][0], XM[i][1], XM[i][27]);
	
	for(j=0; j < N; j++){
	   theta[j] = 0;  
	}
      JCost = costFunctionReg(M, N, theta, XM, y, grad, lambda);

	printf("  \n ");
	printf("cost Function = %lf ", JCost);
	printf("  \n");
	for(j=0; j < N; j++)
        printf("Gradient at initial theta (zeros)= %lf \n", grad[j]);



	fmincg(costFunctionReg, M, N, XM, y, theta, lambda, iterations);	
	
	predict(M, N, theta, XM, g);
	
	int ta =0;
	for(i=0; i < M; i++){
//		   printf("%d  %lf  %lf\n", i, g[i], y[i]);
	   if(fabs(y[i] - g[i]) <= 0.49)
	   	ta++;
	}
	printf("Train Accuracy: %d  %d  %f%\n", ta, M, (double)ta/(double)M * 100);

	
	
	exit(0);
	
}
