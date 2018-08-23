#include <stdio.h>
#include <math.h>

int featureNormalize(int M, double X[M][2], double X_norm[M][2], double mu[2], double sigma[2])
{	
	int i, j;	
	double sumx, sumx2;	
	
	for(j=0; j< 2; j++){
	   sumx =0;
	   sumx2=0;
	   for (i = 0; i < M; i++){
            sumx  = sumx  + X[i][j];
		sumx2 = sumx2 + X[i][j] * X[i][j];
	   }		
	    mu[j] = sumx/M;
		   
	    sigma[j] = sqrt((sumx2 - sumx*sumx /M)/(M-1));
	    
	    for (i = 0; i < M; i++)
	         X_norm[i][j] = (X[i][j] - mu[j]) / sigma[j];
	}
	
	return 0;	
}
