#include <stdio.h>

double linearRegCostFunction(int M,int N,double X[M][N],double y[M],double theta[N], double lambda, double grad[N])
{
	double Jcost;
	int i, j;
	double h[M];
	
	Jcost = 0.f;
	for(i=0;i<M;i++){
			h[i] = 0.f;
		for(j=0;j<N;j++){
			h[i] +=  X[i][j]*theta[j];
		}
//		printf("h[%d]= %lf  %lf\n", i+1, h[i], y[i]);
		
		Jcost = Jcost + (h[i] - y[i])*(h[i] - y[i])/M/2.;	
		
//		printf("Jcost[%d]= %lf \n", i+1, Jcost);	
	}		

	for(j=1;j<N;j++){
		Jcost = Jcost + theta[j]*theta[j]*lambda/M/2.;
	}
	
	
	
	
	for(j=0;j<N;j++)
		grad[j] = 0;
					
	for(i=0;i<M;i++){
		grad[0] = grad[0] + (h[i]-y[i])*X[i][0]/M ;
	}
			
	for(j=1;j<N;j++){
		for(i=0;i<M;i++){
			grad[j] = grad[j] + (h[i]-y[i])*X[i][j]/M + lambda/M*theta[j];
		}
	}	
	
	
	return Jcost;
}
