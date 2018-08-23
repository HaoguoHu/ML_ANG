#include <stdio.h>
#include <math.h>
double sigmoid(double z);

double costFunctionReg(int M, int N, double theta[N], double X[M][N], double y[M], double grad[N], double lambda)
{
	int i, j ;
	double   g, JCost,  TX=0, theta2;
	
	for(j=0; j < N; j++){
	        grad[j] = 0;
//		 gradd[j] = 0;
	}
	JCost = 0;
	theta2 = 0;
	
	
	for(i=0; i < M; i++){
	       TX = 0;
		for(j=0; j < N; j++)
			TX = TX + theta[j]*X[i][j];
			
		g = sigmoid(TX);		
		
		JCost = JCost - y[i]*log(g) - (1.- y[i])* log(1.- g); 
		 
		for(j=0; j < N; j++){
		   grad[j] =  grad[j] + X[i][j]*(g - y[i]);		   
//		  gradd[j] = gradd[j] + g * (1. - g) * X[i][j]* X[i][j];
		   
		}	
	}
	
	
	for(j=1; j < N; j++)
		theta2= theta2 + pow(theta[j], 2);
		
	JCost = JCost/ M + lambda/2/ M * theta2 ;
	
	
		grad[0] =  grad[0]/ M;
//	     gradd[0] = gradd[0]/ M;
	for(j=1; j < N; j++){
	       grad[j] = grad[j]/ M + lambda/ M * theta[j];
//		 gradd[j] = gradd[j]/ M + lambda/ M  ;	
	}
	
	
	
	return JCost;
}
