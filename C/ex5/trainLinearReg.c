double linearRegCostFunction(int M,int N,double X[M][N], double y[M], double theta[N], double lambda, double grad[N]);
int fmincg(double (*linearRegCostFunction)(int M,int N,double X[M][N], double y[M],double theta[N], double lambda, double grad[N]),
	int M,int N, double nn_params[N],double X[M][N],double y[M],double lambda, int maxCostCalls);

int trainLinearReg(int M, int N, double X[M][N],double y[M], double lambda, double theta[N])
{
	int i;
	
	for(i=0;i<N;i++){ theta[i] = 0;}
		
	fmincg(linearRegCostFunction, M, N, theta, X, y,lambda, 200);
	
	return 0;
}
