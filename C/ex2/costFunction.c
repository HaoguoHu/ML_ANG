#include <stdio.h>
#include <math.h>
double sigmoid(double z);

double costFunction(int M, int N, double theta[N], double X[M][2], double y[M], double grad[N])
{
	int i ;
	double   g, JCost = 0;
		
	
	for(i=0; i < M; i++){
		g = sigmoid( theta[0]+ theta[1]*X[i][0] + theta[2]*X[i][1]);
		JCost = JCost - y[i]*log(g) - (1- y[i])* log(1- g);
		
		grad[0] = grad[0] - 1.00000*(g - y[i]);
		grad[1] = grad[1] - X[i][0]*(g - y[i]);
		grad[2] = grad[2] - X[i][1]*(g - y[i]);
	}
	
	JCost = JCost/M;
	
	for(i=0; i < N; i++)
	       grad[i] = grad[i]/M;
	
	printf("%lf, %lf \n", grad[0], JCost);
//		getchar();
	return JCost;
}
