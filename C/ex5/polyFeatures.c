#include <math.h>
#include <stdio.h>

int polyFeatures(int M, double X[M], int N, double Xp[M][N])
{
	int i, j;
	
	for(i=0;i<M;i++){
	
		for(j=0;j<N; j++){
			 Xp[i][j] = pow(X[i], j+1);
		}
	}
	
	
	return 0;
}
