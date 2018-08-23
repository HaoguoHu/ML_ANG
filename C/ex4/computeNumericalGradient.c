
double nnCostFunction(int M,int N,int M1,int N1,int M2,int N2, double nn_params[M1*N1+M2*N2],
 double X[M][N], double y[M], double lambda, double grad[M1*N1+M2*N2]);

void computeNumericalGradient(int M,int N,int M1,int N1,int M2,int N2,double nn_params[M1*N1+M2*N2], 
double X[M][N], double y[M], double lambda, double numgrad[M1*N1+M2*N2])
{
	double e = 1e-4;
	double loss1, loss2;
	int MN = M1*N1 + M2*N2;	
	double grad[MN];
		
	int i, j, k, nn, p;

	for(p=0; p< MN; p++){ 
		nn_params[p] -= e; 
     	loss1 =  nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, grad);  
		nn_params[p] += 2.*e;
    	loss2 =  nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, grad);  
		nn_params[p] -= e;	
		
		numgrad[p] = (loss2 - loss1) / (2.*e);	
	}		
   
}
