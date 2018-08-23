#include <math.h>
#include <stdio.h>

double meanN(int M, int N, double X[M][N], double mu[N]);
void std(int M, int N, double X[M][N], double std[N]);
void bsxfunM(int M, int N, double X[M][N], double y[M]);
void bsxfunRD(int M, int N, double X[M][N], double y[M]);

int featureNormalize(int M, int N, double X[M][N], double mu[N], double sigma[N])
{
	int i, j;
	
	meanN(M, N, X, mu);

	bsxfunM(M, N, X, mu);			
	std(M, N, X, sigma);		
			
	bsxfunRD(M, N, X, sigma);
	
	
	return 0;
}
