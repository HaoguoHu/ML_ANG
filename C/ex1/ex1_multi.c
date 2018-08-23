#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int LMS(double X[],double Y[],int M, double theta[]);
int transpose(int M, int N, double X[M][N], double XT[M][N]);
int multiply( int M, int N, int L, double X[M][N], double Y[N][L], double XML[M][L]);
void inverse(int dim, double mat[dim][dim], double matt[dim][dim] );
int featureNormalize(int M, double X[M][2], double X_norm[M][2], double mu[2], double sigma[2]);
int batchGradientDescentMulti(int M, int N, double alpha, int iterations, double X_norm[M][2], double y[M], double theta[N]);

	
int main( )
{
	int  i,iter,nfile=20;
	int M = 47, N=3;
	int iterations = 50;
	
	double  X[M][2], X_norm[M][2];
	double  Y[M];
	double   mu[2], sigma[2];
	double  theta[3];

	double  alpha = 0.15;
	double  JCost = 0;
      double  temp0, temp1;
	double  predict;	
	
	FILE *fp;
	char line[40];		
	
	if((fp = fopen ("ex1data2.txt", "r")) == NULL){
		printf("open wrong \n");
	}else{
	       i = 0;
	       while(fgets(line, sizeof line, fp) != NULL){
//		   printf("line[%d]=  %s \n", i, line); 
		   
		   X[i][0] = atof(strtok (line,","));
		   X[i][1] = atof(strtok (NULL, ","));			   	
    	            Y[i] = atof(strtok (NULL, ","));			
		   
//		   printf("X[%d][0]= %lf, X[%d][1]= %lf, Y[%d]= %lf \n", i, X[i][0], X[i][1], i,  Y[i]);
//              getchar();

		   i++;	   
	       }
	    fclose(fp);
	 }
	
	
	featureNormalize(M, X, X_norm, mu, sigma);
	
	printf("mu = %f, %f \n ", mu[0], mu[1]);
	printf("sigma = %f, %f \n \n ", sigma[0], sigma[1]);
//	for(i=0; i< M; i++)
//		printf("%f, %f,  \n", X_norm[i][0],  X_norm[i][1]);
		
	
	printf("Now, lets try Batch Gradient Descent \n");
	batchGradientDescentMulti(M, N, alpha, iterations, X_norm, Y, theta);	    
	printf("Batch Gradient Descent with iterations 50 \n ");
	printf(" %f, %f, %f \n \n", 0, theta[0],  1, theta[1],  2, theta[2]);	
	
	
	printf("Now, lets try the closed form--matrix method: \n");	
	double  XMN[M][N];
	double  XNN[N][N], Xi[N][N];
	double  XNM[N][M], XT[N][M];
	double  YY[M][1];
	double  thetaN[N][1];
	
       for(i=0; i<M; i++){ 
	   XMN[i][0] = 1;
	   XMN[i][1] = X[i][0];
	   XMN[i][2] = X[i][1];
	    YY[i][0] = Y[i];
	 }
	 
	 transpose(M, N, XMN, XT);
	 multiply(N, M, N, XT,  XMN, XNN);
	 inverse(N, XNN, Xi);
	 multiply(N, N, M, Xi, XT, XNM);
	 multiply(N, M, 1, XNM, YY, thetaN);
	 printf("%f, %f, %f \n\n" , thetaN[0][0], thetaN[1][0], thetaN[2][0]);
		
	
      exit(0);
}	
	
