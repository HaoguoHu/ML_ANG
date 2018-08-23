#include <stdio.h>

double costFunction(int M, double X[],double Y[], double theta[])
 {  
	int i;
	
	theta[0] = 0;
	theta[1] = 0;
	double JCost = 0;
		
	for(i=0; i < M; i++){
		JCost = JCost + (theta[0] + theta[1]* X[i] - Y[i])*(theta[0] + theta[1]* X[i] - Y[i]);
	}
	JCost = JCost/2/M;
	
	return JCost;
}
