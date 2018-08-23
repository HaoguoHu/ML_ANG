#include <math.h>
#include<stdio.h>
double mean(int M, double X[M])
{
	int i;
	double sum = 0;
	
	for(i=0; i<M; i++){ sum += X[i]; }	
	return sum/M; 
}

double meanN(int M, int N, double X[M][N], double mu[N])
{
	int i, j;

	for(j=0; j< N; j++){
		mu[j] = 0;
		for(i=0; i<M; i++){ mu[j] += X[i][j]; }
		mu[j] /= (double)M;
	}	
}


void std(int M, int N, double X[M][N], double sigma[N])
{
	int i, j;
	double sum ;
	double y[N];
	
	meanN(M, N, X, y);
	
	for (j = 0; j < N; j++){
			sigma[j] = 0;
		for(i=0; i< M; i++){ 
			sigma[j] += pow((X[i][j] - y[j]), 2);
		}
		sigma[j] = sqrt(sigma[j]/(M-1));		
	}
}

void bsxfunM(int M, int N, double X[M][N], double y[M])
{
	int i, j;
	
	for(i=0; i<M; i++){
		for (j=0; j<N; j++) X[i][j] = X[i][j] - y[j];
	}	
}



void bsxfunP(int M, int N, double X[M][N], double y[M])
{
	int i, j;
	
	for(i=0; i<M; i++){
		for (j=0; j<N; j++) X[i][j] = X[i][j] + y[j];
	}
}

void bsxfunRD(int M, int N, double X[M][N], double y[M])
{
	int i, j;
	
	for(i=0; i<M; i++){
		for (j=0; j<N; j++) X[i][j] = X[i][j] / y[j];
	}
}


void bsxfunLD(int M, int N, double X[M][N], double y[M])
{
	int i, j;
	
	for(i=0; i<M; i++){
		for (j=0; j<N; j++) X[i][j] = y[j] / X[i][j];
	}
}


void zeros(int M, int N, double X[M][N], double X1[M][N+1])
{
	int i, j;
	
	for(i=0;i<M;i++){
		X1[i][0] = 0;
		for(j=1;j<N+1;j++){ X1[i][j] = X[i][j-1]; }
	}
}


void ones(int M, int N, double X[M][N], double X1[M][N+1])
{
	int i, j;
	for(i=0;i<M;i++){
		X1[i][0] = 1;
		for(j=1;j<N+1;j++){ X1[i][j] = X[i][j-1]; }
	}
}

void eql(int M, int N, double X1[M][N], double X2[M][N])
{
	int i, j;
	for(i=0; i<M; i++){
		for (j=0; j<N; j++) X2[i][j] = X1[i][j];
	}

}


double min(int M, double X[M])
{
	double min = 1.e30;
	int i;
	
	for(i=0; i<M; i++){
		if (X[i] < min) min = X[i]; 
	}
	
	return min;
}


void minM(int M, int N,  double X[M][N])
{
	double min[N];
	int i, j;
	
	for(j=0; j<N; j++){
		min[j] = 1.e30;
		for(i=0; i<M; i++){
			if (X[i][j] < min[j]) min[j] = X[i][j]; 
		}
	}
}




double max(int M, double X[M])
{
	double max = -1.e30;
	int i;
	
	for(i=0; i<M; i++){
		if (X[i] > max) max = X[i]; 
	}
	
	return max;
}

void maxM(int M, int N,  double X[M][N])
{
	double max[N];
	int i, j;
	
	for(j=0; j<N; j++){
		max[j] = -1.e30;
		for(i=0; i<M; i++){
			if (X[i][j] > max[j]) max[j] = X[i][j]; 
		}
	}
}
