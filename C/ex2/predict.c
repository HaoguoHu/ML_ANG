
double sigmoid(double z);

int predict(int M, int N, double theta[N], double X[M][N], double g[M])
{
	double TX ;
	int i, j;
	
	for(i=0; i < M; i++){
           TX = 0;
	   for(j=0; j < N; j++)
	       TX = TX + theta[j]*X[i][j];
	   g[i] = sigmoid(TX);
	}
	   			
	return 0 ;		
}
