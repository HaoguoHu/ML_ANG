#include <stdio.h>
#include <math.h>
double sigmoid(double z);

double costFunctionReg(int M, int N, double theta[N], double X[M][N], double y[M], double grad[N], double lambda, double gradd[N]);
int newtonGradien(int M, int N, double theta[N], double X[M][N], double y[M], double lambda,  int iterations)
{
	int  j, iter;
	double JCost ;
	double grad[N], gradd[N];
	
				
	for( iter=0; iter < iterations ; iter++){		  
		     
            JCost = costFunctionReg(M, N, theta, X, y, grad, lambda, gradd);
		
	      for(j=0; j < N; j++){
	          theta[j] = theta[j] - grad[j] / gradd[j] ;	
//		    theta[j] = theta[j] - JCost / grad[j] ;		
	     }
		    		     	   
	}
	
	
	
	return 0;
}
