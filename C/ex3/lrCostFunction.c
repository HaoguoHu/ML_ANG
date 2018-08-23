#include <stdio.h>
#include <math.h>

double sigmoid(double z);
//static double X[M][N] ;  //array is large then have to use "static"
//static double y[M] ;

double lrCostFunction(int M, int N, double theta[N], double X[M][N], double y[M], double grad[N], double lambda)
{
	int i, j ;
	double   g, JCost=0,  TX, theta2;

	theta2 = 0;
	JCost =0;
	
	for(j=0; j < N; j++)
	        grad[j] = 0;
	
	for(i=0; i < M; i++){
	       TX = 1;
		for(j=0; j < N; j++)
			TX += theta[j]*X[i][j];
			
		g = sigmoid(TX);		
		
		JCost += - y[i]*log(g) - (1.- y[i])* log(1.- g); 
		 
		for(j=0; j < N; j++)
		   grad[j] += X[i][j]*(g - y[i]);		   		   
	}
	
	
	for(j=0; j < N; j++)
		theta2  += pow(theta[j], 2);
		
	JCost = JCost/ M + lambda/2/ M * theta2 ;
	
		
		grad[0] =  grad[0]/ M;
	for(j=0; j < N; j++)
	       grad[j] = grad[j]/ M + lambda/ M * theta[j];	

	
	return JCost;
}
