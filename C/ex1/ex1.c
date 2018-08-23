#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int N=2;

int LMS(double X[],double Y[],int M, double theta[]);
int transpose(int M, int N, double X[M][N], double XT[M][N]);
int multiply( int M, int N, int L, double X[M][N], double Y[N][L], double XML[M][L]);
void inverse(int dim, double mat[dim][dim], double matt[dim][dim] );

double costFunction(int M, double X[M],double Y[M], double theta[N]);
int batchGradientDescent(int M, int interations, double alpha,  double X[], double Y[], double theta[] );
int stochasiticGradientDescent(int M, double alpha,  double X[], double Y[], double theta[]);

int main( )
{
	int  i,iter,nfile=20;
	int M = 97;
	int iterations = 1500;
	
	double  X[M], Y[M];
	double  theta[2];

	double  alpha = 0.01;
	double  JCost = 0;
      double  temp0, temp1;
	double  predict1=0, predict2=0;	
	
	FILE *fp;
	char line[20];		
	
	if((fp = fopen ("ex1data1.txt", "r")) == NULL){
		printf("open wrong \n");
	}else{
	       i = 0;
	       while(fgets(line, sizeof line, fp) != NULL){
//		   printf("line[%d]=  %s \n", i, line); 
		   
		   X[i] = atof(strtok (line,","));		   	
    	         Y[i] = atof(strtok (NULL, ","));			
		   
//		   printf("X[%d]= %lf, Y[%d]= %lf \n", i, X[i], i,  Y[i]);
//             getchar();

		   i++;	   
	       }
	    fclose(fp);
	 }
	
	
	JCost = costFunction(M, X, Y, theta) ;    	
	printf("Cost Function(ans) = %f \n\n", JCost);
		
	
	
      printf("Now, lets try Batch Gradient Descent \n");	
      batchGradientDescent(M, iterations, alpha, X, Y, theta);
	printf("Batch Gradient Descent with iterations 1500 \n");
	printf("theta[0]= %f, theta[1]= %f  \n\n" , theta[0], theta[1]);
	

	
	
	predict1 = 10000 * theta[0] + 35000 * theta[1];
	printf("For population = 35,000, we predict a profit of  %f \n", predict1);
	
	predict2 = 10000 * theta[0] + 70000 * theta[1];
	printf("For population = 70,000, we predict a profit of %f \n\n ", predict2);
	
	printf("Now, lets try stochasitic Gradient Descent \n");
	stochasiticGradientDescent(M, alpha, X, Y, theta);	
	printf("Stochastic Gradient Descent solutions are: \n");
	printf("theta[0]= %f, theta[1]= %f \n\n" , theta[0], theta[1]);
	
	
	
	printf("Now, lets try the closed form \n ");	
		 if(LMS(X, Y, M, theta) != 0) printf("LMS wrong!");	
	printf("The closed form solutions are: \n ");
	printf("theta[0]= %f, theta[1]= %f \n\n" , theta[0], theta[1]);

	
      exit(0);
}	
	
