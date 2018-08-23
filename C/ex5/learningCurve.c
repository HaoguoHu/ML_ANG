#include <stdio.h>

double linearRegCostFunction(int M,int N,double X[M][N], double y[M], 
	double theta[N], double lambda, double grad[N]);
int fmincg(double (*linearRegCostFunction)(int M,int N,double X[M][N], double y[M],double theta[N], double lambda, double grad[N]),
	int M,int N, double nn_params[N],double X[M][N],double y[M],double lambda, int maxCostCalls);
int trainLinearReg(int M, int N, double X[M][N],double y[M], double lambda, double theta[N]);

int learningCurve(int M,int N,int MV, double X[M][N], double y[M],
	double Xval[MV][N], double yval[MV], double lambda, 
	double error_train[M], double error_val[M])
{
	int i, j, k, nt;
	double  grad[N], theta[N];
	
	for(i=1; i< M+1; i++){   //Not start from 0;
		double Xi[i][N], yi[i];
		
		for(j=0;j<i;j++){
			yi[j] = y[j];
			for(k=0;k<N;k++) Xi[j][k] = X[j][k];				
		}
			
		trainLinearReg(i, N, Xi, yi, lambda, theta);	
		
		error_train[i-1] =   linearRegCostFunction( i, N, Xi, yi, theta, lambda, grad);
		  error_val[i-1] =   linearRegCostFunction(MV, N, Xval, yval, theta, lambda, grad);

	}

	return 0;
}
