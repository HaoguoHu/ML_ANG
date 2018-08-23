#include <math.h>

int mapFeature(int M, int N1, double X[M][N1], int degree, int N, double out[M][N])
{	
	int  i, j, k, L=0;
	
	for(i=1; i <= degree; i++){
		for(j=0; j <= i; j++){
			L++;
		      for(k=0; k < M; k++){
			  out[k][L] = pow(X[k][0], (i-j)) * pow(X[k][1], j);
			}
			 
		}
	}
	
			for(k=0; k < M; k++){
			  out[k][0] = 1;
			}
	return 0;
}
	
